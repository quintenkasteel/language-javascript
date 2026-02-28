{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Usage analysis implementation for JavaScript AST tree shaking.
--
-- This module implements comprehensive usage analysis that identifies
-- how identifiers are used throughout JavaScript code. The analysis
-- handles lexical scoping, module imports/exports, and side effect
-- detection to enable safe dead code elimination.
--
-- @since 0.8.0.0
module Language.JavaScript.Process.TreeShake.Analysis
  ( -- * Main Analysis Functions
    analyzeUsage,
    analyzeUsageWithOptions,
    buildUsageMap,
    
    -- * Scope Analysis
    analyzeLexicalScopes,
    buildScopeStack,
    findDeclarations,
    
    -- * Reference Analysis  
    findReferences,
    analyzeIdentifierUsage,
    trackCallExpressions,
    
    -- * Module Analysis
    analyzeModuleSystem,
    extractImportInfo,
    extractExportInfo,
    buildDependencyGraph,
    
    -- * Side Effect Analysis
    analyzeSideEffects,
    hasSideEffects,
    isCallWithSideEffects,
    isPureFunctionCall,
    
    -- * Utility Functions
    extractIdentifierName,
    isTopLevelDeclaration,
    isExportedDeclaration,
    calculateEstimatedReduction,
    hasIdentifierUsage,
    isExportedIdentifier,
  )
where

import Control.Lens ((&), (.~), (%~), (^.), (?~))
import Control.Monad.State.Strict (State, gets, modify, execState)
import Data.Foldable (traverse_, for_)
import qualified Data.Map.Strict as Map
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import Language.JavaScript.Process.TreeShake.Types hiding (hasSideEffects)
import qualified Language.JavaScript.Process.TreeShake.Types as Types

-- | Analysis state during AST traversal.
data AnalysisState = AnalysisState
  { _analysisUsageMap :: !UsageMap
  , _analysisScopeStack :: !ScopeStack
  , _analysisModuleDeps :: ![ModuleDependency]
  , _analysisOptions :: !TreeShakeOptions
  , _currentScopeLevel :: !Int
  , _analysisHasEval :: !Bool
  , _analysisEvalCount :: !Int
  , _analysisDynamicAccess :: !(Set.Set Text.Text)
  , _analysisSideEffectCount :: !Int
  } deriving (Eq, Show)

type AnalysisM = State AnalysisState

-- | Analyze usage patterns in JavaScript AST with default options.
analyzeUsage :: JSAST -> UsageAnalysis
analyzeUsage = analyzeUsageWithOptions defaultTreeShakeOptions

-- | Analyze usage patterns with specific configuration options.
analyzeUsageWithOptions :: TreeShakeOptions -> JSAST -> UsageAnalysis
analyzeUsageWithOptions opts ast = 
  let initialState = AnalysisState
        { _analysisUsageMap = Map.empty
        , _analysisScopeStack = [createGlobalScope]
        , _analysisModuleDeps = []
        , _analysisOptions = opts
        , _currentScopeLevel = 0
        , _analysisHasEval = False
        , _analysisEvalCount = 0
        , _analysisDynamicAccess = Set.empty
        , _analysisSideEffectCount = 0
        }
      
      finalState = execState (analyzeAST ast) initialState
      finalUsageMap = _analysisUsageMap finalState
      finalModuleDeps = _analysisModuleDeps finalState
      
  in UsageAnalysis
    { _usageMap = finalUsageMap
    , _moduleDependencies = finalModuleDeps
    , _totalIdentifiers = Map.size finalUsageMap
    , _unusedCount = countUnused finalUsageMap
    , _sideEffectCount = countSideEffects finalUsageMap
    , _estimatedReduction = calculateEstimatedReduction finalUsageMap
    , _hasEvalCall = _analysisHasEval finalState
    , _evalCallCount = _analysisEvalCount finalState
    , _dynamicAccessObjects = _analysisDynamicAccess finalState
    }

-- | Build usage map from AST analysis.
buildUsageMap :: TreeShakeOptions -> JSAST -> UsageMap
buildUsageMap opts ast = _usageMap $ analyzeUsageWithOptions opts ast

-- | Main AST analysis dispatcher.
analyzeAST :: JSAST -> AnalysisM ()
analyzeAST (JSAstProgram statements _) = traverse_ analyzeStatement statements
analyzeAST (JSAstModule moduleItems _) = traverse_ analyzeModuleItem moduleItems  
analyzeAST (JSAstStatement stmt _) = analyzeStatement stmt
analyzeAST (JSAstExpression expr _) = analyzeExpression expr
analyzeAST (JSAstLiteral expr _) = analyzeExpression expr

-- | Analyze individual statements with proper scope handling.
analyzeStatement :: JSStatement -> AnalysisM ()
analyzeStatement stmt = case stmt of
  JSVariable _ varList _ -> do
    -- First pass: declare variables in current scope
    traverse_ declareFromExpression (fromCommaList varList)
    -- Second pass: analyze initializers only
    traverse_ analyzeVariableInitializer (fromCommaList varList)
    
  JSLet _ varList _ -> do
    traverse_ declareFromExpression (fromCommaList varList)
    traverse_ analyzeVariableInitializer (fromCommaList varList)
    
  JSConstant _ varList _ -> do
    traverse_ declareFromExpression (fromCommaList varList)
    traverse_ analyzeVariableInitializer (fromCommaList varList)
    
  JSFunction _ ident _ params _ body _ -> do
    -- Declare function in current scope
    declareIdentifier ident
    -- Create function scope and analyze body
    withFunctionScope $ do
      traverse_ declareFromExpression (fromCommaList params)
      analyzeBlock body
      
  JSAsyncFunction _ _ ident _ params _ body _ -> do
    declareIdentifier ident
    withFunctionScope $ do
      traverse_ declareFromExpression (fromCommaList params)
      analyzeBlock body
      
  JSGenerator _ _ ident _ params _ body _ -> do
    declareIdentifier ident
    withFunctionScope $ do
      traverse_ declareFromExpression (fromCommaList params)
      analyzeBlock body
      
  JSIf _ _ condition _ thenStmt -> do
    analyzeExpression condition
    analyzeStatement thenStmt
    
  JSIfElse _ _ condition _ thenStmt _ elseStmt -> do
    analyzeExpression condition
    analyzeStatement thenStmt
    analyzeStatement elseStmt
    
  JSFor _ _ init _ condition _ increment _ body -> do
    traverse_ analyzeExpression (fromCommaList init)
    traverse_ analyzeExpression (fromCommaList condition)
    traverse_ analyzeExpression (fromCommaList increment)
    analyzeStatement body
    
  JSForIn _ _ var _ obj _ body -> do
    analyzeExpression var
    analyzeExpression obj
    analyzeStatement body
    
  JSForOf _ _ var _ obj _ body -> do
    analyzeExpression var
    analyzeExpression obj
    analyzeStatement body
    
  JSWhile _ _ condition _ body -> do
    analyzeExpression condition
    analyzeStatement body
    
  JSDoWhile _ body _ _ condition _ _ -> do
    analyzeStatement body
    analyzeExpression condition
    
  JSReturn _ maybeExpr _ ->
    maybe (pure ()) analyzeExpression maybeExpr
    
  JSThrow _ expr _ ->
    analyzeExpression expr
    
  JSTry _ body catches finally -> do
    analyzeBlock body
    traverse_ analyzeTryCatch catches
    analyzeTryFinally finally
    
  JSSwitch _ _ expr _ _ cases _ _ -> do
    analyzeExpression expr
    traverse_ analyzeSwitchCase cases
    
  JSWith _ _ expr _ body _ -> do
    analyzeExpression expr
    analyzeStatement body
    
  JSLabelled ident _ stmt -> do
    declareIdentifier ident
    analyzeStatement stmt
    
  JSStatementBlock _ stmts _ _ ->
    withBlockScope $ traverse_ analyzeStatement stmts
    
  JSExpressionStatement expr _ ->
    analyzeExpression expr
    
  JSAssignStatement lhs _ rhs _ -> do
    analyzeExpression lhs
    analyzeExpression rhs
    
  JSMethodCall expr _ args _ _ -> do
    analyzeExpression expr
    -- Special handling for dynamic code execution (eval, Function constructor)
    if isDynamicCodeCall expr
      then do
        markHasEvalCall
        markPotentialEvalIdentifiers args
      else traverse_ analyzeExpression (fromCommaList args)
    markSideEffect
    
  JSClass _ ident heritage _ elements _ _ -> do
    declareIdentifier ident
    analyzeClassHeritage heritage
    traverse_ analyzeClassElement elements
    
  JSEmptyStatement _ -> pure ()
  
  -- Control flow statements
  JSBreak _ ident _ -> do
    case ident of
      JSIdentName _ name -> markIdentifierUsed (Text.decodeUtf8 name)
      JSIdentNone -> pure ()
      
  JSContinue _ ident _ -> do
    case ident of
      JSIdentName _ name -> markIdentifierUsed (Text.decodeUtf8 name)
      JSIdentNone -> pure ()
  
  -- For loop variants with variable declarations
  JSForVar _ _ _ varList _ condition _ increment _ body -> do
    traverse_ declareFromExpression (fromCommaList varList)
    traverse_ analyzeVariableInitializer (fromCommaList varList)
    traverse_ analyzeExpression (fromCommaList condition)
    traverse_ analyzeExpression (fromCommaList increment)
    analyzeStatement body
    
  JSForVarIn _ _ _ var _ obj _ body -> do
    declareFromExpression var
    analyzeExpression obj
    analyzeStatement body
    
  JSForVarOf _ _ _ var _ obj _ body -> do
    declareFromExpression var
    analyzeExpression obj
    analyzeStatement body
    
  JSForLet _ _ _ varList _ condition _ increment _ body ->
    withBlockScope $ do
      traverse_ declareFromExpression (fromCommaList varList)
      traverse_ analyzeVariableInitializer (fromCommaList varList)
      traverse_ analyzeExpression (fromCommaList condition)
      traverse_ analyzeExpression (fromCommaList increment)
      analyzeStatement body
      
  JSForLetIn _ _ _ var _ obj _ body ->
    withBlockScope $ do
      declareFromExpression var
      analyzeExpression obj
      analyzeStatement body
      
  JSForLetOf _ _ _ var _ obj _ body ->
    withBlockScope $ do
      declareFromExpression var
      analyzeExpression obj
      analyzeStatement body
      
  JSForConst _ _ _ varList _ condition _ increment _ body ->
    withBlockScope $ do
      traverse_ declareFromExpression (fromCommaList varList)
      traverse_ analyzeVariableInitializer (fromCommaList varList)
      traverse_ analyzeExpression (fromCommaList condition)
      traverse_ analyzeExpression (fromCommaList increment)
      analyzeStatement body
      
  JSForConstIn _ _ _ var _ obj _ body ->
    withBlockScope $ do
      declareFromExpression var
      analyzeExpression obj
      analyzeStatement body
      
  JSForConstOf _ _ _ var _ obj _ body ->
    withBlockScope $ do
      declareFromExpression var
      analyzeExpression obj
      analyzeStatement body
  
  -- Handle other statement types
  _ -> pure ()

-- | Analyze expressions with proper reference tracking.
analyzeExpression :: JSExpression -> AnalysisM ()
analyzeExpression expr = case expr of
  JSIdentifier _ name -> 
    markIdentifierUsed (Text.decodeUtf8 name)
    
  JSVarInitExpression _var initializer ->
    -- Don't analyze the variable name - it's a declaration, not a usage
    -- Only analyze the initializer for references
    analyzeVarInitializer initializer
    
  JSAssignExpression lhs _ rhs -> do
    analyzeExpression lhs
    analyzeExpression rhs
    markSideEffect
    
  JSCallExpression target _ args _ -> do
    analyzeExpression target
    -- Special handling for dynamic code execution (eval, Function constructor)
    if isDynamicCodeCall target
      then do
        markHasEvalCall
        markPotentialEvalIdentifiers args
      else traverse_ analyzeExpression (fromCommaList args)
    markSideEffect
    
  JSCallExpressionDot target _ _prop -> do
    analyzeExpression target
    -- Don't analyze prop - it's a property name, not a variable reference
    markSideEffect
    
  JSCallExpressionSquare target _ prop _ -> do
    analyzeExpression target
    analyzeExpression prop
    markSideEffect
    
  JSMemberDot target _ prop -> do
    analyzeExpression target
    -- Track property access for tree shaking (mark property as used)
    markPropertyUsed target prop
    
  JSMemberSquare target _ prop _ -> do
    analyzeExpression target
    analyzeExpression prop
    -- Check if this is dynamic property access (property is a variable or expression)
    case prop of
      JSIdentifier _ _ -> markObjectWithDynamicAccess target
      JSCallExpression {} -> markObjectWithDynamicAccess target  -- obj[func()]
      JSExpressionBinary {} -> markObjectWithDynamicAccess target  -- obj[x + y]
      JSVarInitExpression {} -> markObjectWithDynamicAccess target  -- obj[x = y]
      _ -> pure ()
    
  JSOptionalMemberDot target _ _prop ->
    analyzeExpression target
    -- Don't analyze prop - it's a property name, not a variable reference
    
  JSOptionalMemberSquare target _ prop _ -> do
    analyzeExpression target
    analyzeExpression prop
    
  JSOptionalCallExpression target _ args _ -> do
    analyzeExpression target
    traverse_ analyzeExpression (fromCommaList args)
    markSideEffect
    
  JSNewExpression _ target -> do
    analyzeExpression target
    -- Special handling for dynamic code execution (eval, Function constructor)
    when (isDynamicCodeCall target) markHasEvalCall
    markSideEffect
    
  JSMemberNew _ target _ args _ -> do
    analyzeExpression target
    -- Special handling for dynamic code execution (eval, Function constructor)
    if isDynamicCodeCall target
      then do
        markHasEvalCall
        markPotentialEvalIdentifiers args
      else traverse_ analyzeExpression (fromCommaList args)
    markSideEffect
    
  JSUnaryExpression op operand -> do
    analyzeUnaryOp op
    analyzeExpression operand
    when (isUnaryOpSideEffect op) markSideEffect
    
  JSExpressionPostfix operand op -> do
    analyzeExpression operand
    when (isPostfixSideEffect op) markSideEffect
    
  JSExpressionBinary lhs _ rhs -> do
    analyzeExpression lhs
    analyzeExpression rhs
    
  JSExpressionTernary condition _ trueExpr _ falseExpr -> do
    analyzeExpression condition
    analyzeExpression trueExpr
    analyzeExpression falseExpr
    
  JSCommaExpression left _ right -> do
    analyzeExpression left
    analyzeExpression right
    
  JSArrayLiteral _ elements _ ->
    traverse_ analyzeArrayElement elements
    
  JSObjectLiteral _ props _ ->
    analyzeObjectPropertyList props
    
  JSFunctionExpression _ ident _ params _ body -> do
    declareIdentifier ident
    withFunctionScope $ do
      traverse_ declareFromExpression (fromCommaList params)
      analyzeBlock body
      
  JSArrowExpression params _ body ->
    withFunctionScope $ do
      analyzeArrowParams params
      analyzeConciseBody body
      
  JSYieldExpression _ maybeExpr -> do
    maybe (pure ()) analyzeExpression maybeExpr
    markSideEffect
    
  JSYieldFromExpression _ _ expr -> do
    analyzeExpression expr
    markSideEffect
    
  JSAwaitExpression _ expr -> do
    analyzeExpression expr
    markSideEffect
    
  JSSpreadExpression _ expr ->
    analyzeExpression expr
    
  JSTemplateLiteral maybeTag _ _ parts -> do
    maybe (pure ()) analyzeExpression maybeTag
    traverse_ analyzeTemplatePart parts
    
  JSClassExpression _ ident heritage _ elements _ -> do
    declareIdentifier ident
    analyzeClassHeritage heritage
    traverse_ analyzeClassElement elements
    
  JSExpressionParen _ expr _ ->
    analyzeExpression expr
    
  -- Literals and simple expressions
  JSDecimal {} -> pure ()
  JSLiteral {} -> pure ()
  JSHexInteger {} -> pure ()
  JSBinaryInteger {} -> pure ()
  JSOctal {} -> pure ()
  JSBigIntLiteral {} -> pure ()
  JSStringLiteral {} -> pure ()
  JSRegEx {} -> pure ()
  JSImportMeta {} -> pure ()
  JSImportCall _ _ expr _ -> analyzeExpression expr

  -- Additional expression patterns
  JSAsyncFunctionExpression _ _ ident _ params _ body -> do
    declareIdentifier ident
    withFunctionScope $ do
      traverse_ declareFromExpression (fromCommaList params)
      analyzeBlock body
      
  JSGeneratorExpression _ _ ident _ params _ body -> do
    declareIdentifier ident
    withFunctionScope $ do
      traverse_ declareFromExpression (fromCommaList params)
      analyzeBlock body
      
  JSMemberExpression target _ args _ -> do
    analyzeExpression target
    traverse_ analyzeExpression (fromCommaList args)
    markSideEffect

-- | Analyze module items (imports/exports).
analyzeModuleItem :: JSModuleItem -> AnalysisM ()
analyzeModuleItem item = case item of
  JSModuleImportDeclaration _ importDecl ->
    analyzeImportDeclaration importDecl
    
  JSModuleExportDeclaration _ exportDecl ->
    analyzeExportDeclaration exportDecl
    
  JSModuleStatementListItem stmt ->
    analyzeStatement stmt

-- | Analyze import declarations and track dependencies.
analyzeImportDeclaration :: JSImportDeclaration -> AnalysisM ()
analyzeImportDeclaration importDecl = do
  let importInfo = extractImportInfo importDecl
  
  -- Add imported identifiers to usage map
  Set.foldr' (\name acc -> acc >> declareImportedIdentifier name) (pure ()) 
    (_importedNames importInfo)
  
  -- Handle default import
  for_ (_importDefault importInfo) declareImportedIdentifier
    
  -- Handle namespace import
  for_ (_importNamespace importInfo) declareImportedIdentifier

-- | Analyze export declarations and mark exports.
analyzeExportDeclaration :: JSExportDeclaration -> AnalysisM ()
analyzeExportDeclaration exportDecl = do
  let exportInfos = extractExportInfo exportDecl

  -- Mark all exported identifiers
  traverse_ (markIdentifierExported . _exportedName) exportInfos

  -- Analyze exported statements
  case exportDecl of
    JSExport stmt _ -> analyzeStatement stmt
    JSExportDefault _ stmt _ -> analyzeStatement stmt
    _ -> pure ()

-- Helper Functions

-- | Create global scope information.
createGlobalScope :: ScopeInfo
createGlobalScope = ScopeInfo
  { _scopeType = GlobalScope
  , _scopeBindings = Set.empty
  , _scopeLevel = 0
  , _parentScope = Nothing
  }

-- | Execute analysis within a function scope.
withFunctionScope :: AnalysisM a -> AnalysisM a
withFunctionScope action = do
  enterScope FunctionScope
  result <- action
  exitScope
  pure result

-- | Execute analysis within a block scope.
withBlockScope :: AnalysisM a -> AnalysisM a  
withBlockScope action = do
  enterScope BlockScope
  result <- action
  exitScope
  pure result

-- | Enter a new scope.
enterScope :: ScopeType -> AnalysisM ()
enterScope scopeType = do
  currentLevel <- gets _currentScopeLevel
  currentStack <- gets _analysisScopeStack
  
  let newScope = ScopeInfo
        { _scopeType = scopeType
        , _scopeBindings = Set.empty
        , _scopeLevel = currentLevel + 1
        , _parentScope = case currentStack of
            (parent:_) -> Just parent
            [] -> Nothing
        }
  
  modify $ \s -> s 
    { _analysisScopeStack = newScope : _analysisScopeStack s
    , _currentScopeLevel = currentLevel + 1
    }

-- | Exit current scope.
exitScope :: AnalysisM ()
exitScope = do
  currentStack <- gets _analysisScopeStack
  currentLevel <- gets _currentScopeLevel
  
  case currentStack of
    (_:rest) -> modify $ \s -> s 
      { _analysisScopeStack = rest
      , _currentScopeLevel = max 0 (currentLevel - 1)
      }
    [] -> pure ()  -- No scope to exit

-- | Declare identifier in current scope.
declareIdentifier :: JSIdent -> AnalysisM ()
declareIdentifier (JSIdentName annot name) = do
  scopeLevel <- gets _currentScopeLevel
  usageMap <- gets _analysisUsageMap

  let identifier = Text.decodeUtf8 name
  let currentInfo = Map.findWithDefault defaultUsageInfo identifier usageMap
  let updatedInfo = currentInfo
        & scopeDepth .~ scopeLevel
        & declarationLocation ?~ annotPosition annot

  modify $ \s -> s { _analysisUsageMap = Map.insert identifier updatedInfo usageMap }

declareIdentifier JSIdentNone = pure ()

-- | Extract position from annotation, defaulting to origin for missing annotations.
annotPosition :: JSAnnot -> TokenPosn
annotPosition (JSAnnot pos _) = pos
annotPosition JSAnnotSpace = TokenPn 0 0 0
annotPosition JSNoAnnot = TokenPn 0 0 0

-- | Declare identifier from expression, handling destructuring patterns.
-- Supports array destructuring @[a, b]@, object destructuring @{a, b: c}@,
-- spread elements @...rest@, and default values @x = defaultVal@.
declareFromExpression :: JSExpression -> AnalysisM ()
declareFromExpression (JSIdentifier _ name) =
  declareIdentifier (JSIdentName JSNoAnnot name)
declareFromExpression (JSVarInitExpression var _) =
  declareFromExpression var
declareFromExpression (JSArrayLiteral _ elements _) =
  traverse_ declareFromArrayElement elements
declareFromExpression (JSObjectLiteral _ properties _) =
  declareFromObjectProperties properties
declareFromExpression (JSSpreadExpression _ expr) =
  declareFromExpression expr
declareFromExpression (JSAssignExpression lhs _ _rhs) =
  declareFromExpression lhs
declareFromExpression _ = pure ()

-- | Declare identifiers from array destructuring elements.
declareFromArrayElement :: JSArrayElement -> AnalysisM ()
declareFromArrayElement (JSArrayElement expr) = declareFromExpression expr
declareFromArrayElement (JSArrayComma _) = pure ()

-- | Declare identifiers from object destructuring properties.
declareFromObjectProperties :: JSObjectPropertyList -> AnalysisM ()
declareFromObjectProperties (JSCTLComma props _) = declareFromObjectPropertyList props
declareFromObjectProperties (JSCTLNone props) = declareFromObjectPropertyList props

-- | Walk the comma list of object properties for destructuring declarations.
declareFromObjectPropertyList :: JSCommaList JSObjectProperty -> AnalysisM ()
declareFromObjectPropertyList = traverse_ declareFromObjectProperty . fromCommaList

-- | Declare identifier from a single object destructuring property.
declareFromObjectProperty :: JSObjectProperty -> AnalysisM ()
declareFromObjectProperty (JSPropertyNameandValue _ _ exprs) =
  traverse_ declareFromExpression exprs
declareFromObjectProperty (JSPropertyIdentRef _ name) =
  declareIdentifier (JSIdentName JSNoAnnot name)
declareFromObjectProperty (JSObjectSpread _ expr) =
  declareFromExpression expr
declareFromObjectProperty (JSObjectMethod _) = pure ()

-- | Analyze variable initializer (right-hand side of var x = expr).
analyzeVariableInitializer :: JSExpression -> AnalysisM ()
analyzeVariableInitializer (JSVarInitExpression _ (JSVarInit _ initExpr)) = 
  analyzeExpression initExpr
analyzeVariableInitializer (JSIdentifier _ _) = 
  pure ()  -- Just declaration, no initializer
analyzeVariableInitializer expr = 
  analyzeExpression expr  -- Handle other expression types

-- | Declare imported identifier.
declareImportedIdentifier :: Text.Text -> AnalysisM ()
declareImportedIdentifier identifier = do
  usageMap <- gets _analysisUsageMap
  
  let currentInfo = Map.findWithDefault defaultUsageInfo identifier usageMap
  let updatedInfo = currentInfo
        & scopeDepth .~ 0  -- Module scope
        & declarationLocation ?~ TokenPn 0 0 0
  
  modify $ \s -> s { _analysisUsageMap = Map.insert identifier updatedInfo usageMap }

-- | Mark identifier as used.
markIdentifierUsed :: Text.Text -> AnalysisM ()
markIdentifierUsed identifier = do
  usageMap <- gets _analysisUsageMap

  let currentInfo = Map.findWithDefault defaultUsageInfo identifier usageMap
  let updatedInfo = currentInfo
        & isUsed .~ True
        & directReferences %~ (+1)

  modify $ \s -> s { _analysisUsageMap = Map.insert identifier updatedInfo usageMap }

-- | Mark property as used via member access (obj.prop).
markPropertyUsed :: JSExpression -> JSExpression -> AnalysisM ()
markPropertyUsed _target prop =
  -- Extract property name and mark it as used
  for_ (extractIdentifierName prop) markIdentifierUsed

-- | Mark object as having dynamic property access and mark all its properties as used.
markObjectWithDynamicAccess :: JSExpression -> AnalysisM ()
markObjectWithDynamicAccess expr = case expr of
  JSIdentifier _ name -> do
    let objectName = Text.decodeUtf8 name
    modify $ \s -> s { _analysisDynamicAccess = Set.insert objectName (_analysisDynamicAccess s) }
    -- Mark all properties of this object as potentially used by marking the object as having side effects
    markIdentifierWithSideEffects objectName
  _ -> pure ()  -- Only track simple object identifiers for now

-- | Mark identifier as having side effects.
markIdentifierWithSideEffects :: Text.Text -> AnalysisM ()
markIdentifierWithSideEffects identifier = do
  usageMap <- gets _analysisUsageMap

  let currentInfo = Map.findWithDefault defaultUsageInfo identifier usageMap
  let updatedInfo = currentInfo
        & isUsed .~ True
        & Types.hasSideEffects .~ True

  modify $ \s -> s { _analysisUsageMap = Map.insert identifier updatedInfo usageMap }

-- | Mark identifier as exported.
markIdentifierExported :: Text.Text -> AnalysisM ()
markIdentifierExported identifier = do
  usageMap <- gets _analysisUsageMap
  
  let currentInfo = Map.findWithDefault defaultUsageInfo identifier usageMap
  let updatedInfo = currentInfo
        & isExported .~ True
        & isUsed .~ True  -- Exported identifiers are considered used
  
  modify $ \s -> s { _analysisUsageMap = Map.insert identifier updatedInfo usageMap }

-- | Mark that a side effect occurred in the current analysis context.
-- Increments the global side effect counter for use in elimination decisions.
markSideEffect :: AnalysisM ()
markSideEffect = modify $ \s -> s { _analysisSideEffectCount = _analysisSideEffectCount s + 1 }

-- | Check if unary operator has side effects.
isUnaryOpSideEffect :: JSUnaryOp -> Bool
isUnaryOpSideEffect (JSUnaryOpDelete _) = True
isUnaryOpSideEffect (JSUnaryOpIncr _) = True
isUnaryOpSideEffect (JSUnaryOpDecr _) = True
isUnaryOpSideEffect _ = False

-- | Check if postfix operator has side effects.
isPostfixSideEffect :: JSUnaryOp -> Bool
isPostfixSideEffect (JSUnaryOpIncr _) = True
isPostfixSideEffect (JSUnaryOpDecr _) = True
isPostfixSideEffect _ = False

-- | Analyze unary operator.
analyzeUnaryOp :: JSUnaryOp -> AnalysisM ()
analyzeUnaryOp _ = pure ()  -- Operators themselves don't introduce identifiers

-- | Analyze array elements.
analyzeArrayElement :: JSArrayElement -> AnalysisM ()
analyzeArrayElement (JSArrayElement expr) = analyzeExpression expr
analyzeArrayElement (JSArrayComma _) = pure ()

-- | Analyze template part
analyzeTemplatePart :: JSTemplatePart -> AnalysisM ()
analyzeTemplatePart (JSTemplatePart expr _ _) = analyzeExpression expr

-- | Analyze object property list.
analyzeObjectPropertyList :: JSCommaTrailingList JSObjectProperty -> AnalysisM ()
analyzeObjectPropertyList (JSCTLComma props _) = analyzeObjectProperties props
analyzeObjectPropertyList (JSCTLNone props) = analyzeObjectProperties props

-- | Analyze object properties.
analyzeObjectProperties :: JSCommaList JSObjectProperty -> AnalysisM ()
analyzeObjectProperties props = traverse_ analyzeObjectProperty (fromCommaList props)

-- | Analyze individual object property.
analyzeObjectProperty :: JSObjectProperty -> AnalysisM ()
analyzeObjectProperty prop = case prop of
  JSPropertyNameandValue name _ exprs -> do
    analyzePropertyName name
    traverse_ analyzeExpression exprs
    
  JSPropertyIdentRef _ name ->
    -- ES6 shorthand {prop} is equivalent to {prop: prop}, so mark the identifier as used
    markIdentifierUsed (Text.decodeUtf8 name)
  
  JSObjectMethod method -> analyzeMethodDefinition method
  
  JSObjectSpread _ expr -> analyzeExpression expr

-- | Analyze property name.
analyzePropertyName :: JSPropertyName -> AnalysisM ()
analyzePropertyName name = case name of
  JSPropertyIdent _ _ -> pure ()
  JSPropertyString _ _ -> pure ()
  JSPropertyNumber _ _ -> pure ()
  JSPropertyComputed _ expr _ -> analyzeExpression expr

-- | Analyze method definition.
analyzeMethodDefinition :: JSMethodDefinition -> AnalysisM ()
analyzeMethodDefinition method = case method of
  JSMethodDefinition name _ params _ body -> do
    analyzePropertyName name
    withFunctionScope $ do
      traverse_ declareFromExpression (fromCommaList params)
      analyzeBlock body
      
  JSGeneratorMethodDefinition _ name _ params _ body -> do
    analyzePropertyName name
    withFunctionScope $ do
      traverse_ declareFromExpression (fromCommaList params)
      analyzeBlock body

  JSAsyncMethodDefinition _ name _ params _ body -> do
    analyzePropertyName name
    withFunctionScope $ do
      traverse_ declareFromExpression (fromCommaList params)
      analyzeBlock body

  JSPropertyAccessor _ name _ params _ body -> do
    analyzePropertyName name
    withFunctionScope $ do
      traverse_ declareFromExpression (fromCommaList params)
      analyzeBlock body

-- | Analyze block statements.
analyzeBlock :: JSBlock -> AnalysisM ()
analyzeBlock (JSBlock _ stmts _) = traverse_ analyzeStatement stmts

-- | Analyze variable initializer.
analyzeVarInitializer :: JSVarInitializer -> AnalysisM ()
analyzeVarInitializer (JSVarInit _ expr) = analyzeExpression expr
analyzeVarInitializer JSVarInitNone = pure ()

-- | Analyze arrow function parameters.
analyzeArrowParams :: JSArrowParameterList -> AnalysisM ()
analyzeArrowParams (JSUnparenthesizedArrowParameter ident) = 
  declareIdentifier ident
analyzeArrowParams (JSParenthesizedArrowParameterList _ params _) =
  traverse_ declareFromExpression (fromCommaList params)

-- | Analyze concise body.
analyzeConciseBody :: JSConciseBody -> AnalysisM ()
analyzeConciseBody (JSConciseFunctionBody body) = analyzeBlock body
analyzeConciseBody (JSConciseExpressionBody expr) = analyzeExpression expr

-- | Analyze try-catch clauses.
analyzeTryCatch :: JSTryCatch -> AnalysisM ()
analyzeTryCatch (JSCatch _ _ param _ body) = do
  analyzeExpression param
  analyzeBlock body
analyzeTryCatch (JSCatchIf _ _ param _ condition _ body) = do
  analyzeExpression param
  analyzeExpression condition
  analyzeBlock body

-- | Analyze try-finally clause.
analyzeTryFinally :: JSTryFinally -> AnalysisM ()
analyzeTryFinally (JSFinally _ body) = analyzeBlock body
analyzeTryFinally JSNoFinally = pure ()

-- | Analyze switch case.
analyzeSwitchCase :: JSSwitchParts -> AnalysisM ()
analyzeSwitchCase (JSCase _ expr _ stmts) = do
  analyzeExpression expr
  traverse_ analyzeStatement stmts
analyzeSwitchCase (JSDefault _ _ stmts) =
  traverse_ analyzeStatement stmts

-- | Analyze class heritage.
analyzeClassHeritage :: JSClassHeritage -> AnalysisM ()
analyzeClassHeritage (JSExtends _ expr) = analyzeExpression expr
analyzeClassHeritage JSExtendsNone = pure ()

-- | Analyze class element.
analyzeClassElement :: JSClassElement -> AnalysisM ()
analyzeClassElement element = case element of
  JSClassInstanceMethod method -> analyzeMethodDefinition method
  JSClassStaticMethod _ method -> analyzeMethodDefinition method
  JSClassSemi _ -> pure ()
  JSPrivateField _ _ _ maybeInit _ -> 
    maybe (pure ()) analyzeExpression maybeInit
  JSPrivateMethod _ _ _ params _ body -> 
    withFunctionScope $ do
      traverse_ declareFromExpression (fromCommaList params)
      analyzeBlock body
  JSPrivateAccessor _ _ _ _ params _ body ->
    withFunctionScope $ do
      traverse_ declareFromExpression (fromCommaList params)
      analyzeBlock body


-- Implementation of remaining exported functions

analyzeLexicalScopes :: JSAST -> ScopeStack
analyzeLexicalScopes _ast = [createGlobalScope]

buildScopeStack :: ScopeStack
buildScopeStack = [createGlobalScope]

findDeclarations :: JSAST -> AnalysisM ()
findDeclarations = analyzeAST

findReferences :: JSAST -> AnalysisM ()  
findReferences = analyzeAST

analyzeIdentifierUsage :: Text.Text -> TokenPosn -> AnalysisM ()
analyzeIdentifierUsage identifier _pos = do
  currentMap <- gets _analysisUsageMap
  let currentUsage = Map.findWithDefault Types.defaultUsageInfo identifier currentMap
      updatedUsage = currentUsage
        & Types.isUsed .~ True
        & Types.directReferences %~ (+1)
  modify $ \s -> s { _analysisUsageMap = Map.insert identifier updatedUsage currentMap }

trackCallExpressions :: JSExpression -> AnalysisM ()
trackCallExpressions = analyzeExpression

analyzeModuleSystem :: JSAST -> AnalysisM ()
analyzeModuleSystem = analyzeAST

extractImportInfo :: JSImportDeclaration -> ImportInfo
extractImportInfo (JSImportDeclaration clause (JSFromClause _ _ moduleName) _ _) =
  ImportInfo
    { _importModule = Text.decodeUtf8 moduleName
    , _importedNames = extractImportNames clause
    , _importDefault = extractDefaultImport clause
    , _importNamespace = extractNamespaceImport clause
    , _importLocation = TokenPn 0 0 0
    , _isImportTypeOnly = False
    }
extractImportInfo (JSImportDeclarationBare _ moduleName _ _) =
  ImportInfo
    { _importModule = Text.decodeUtf8 moduleName
    , _importedNames = Set.empty
    , _importDefault = Nothing
    , _importNamespace = Nothing
    , _importLocation = TokenPn 0 0 0
    , _isImportTypeOnly = False
    }

extractExportInfo :: JSExportDeclaration -> [ExportInfo]
extractExportInfo (JSExport stmt _) = extractExportInfoFromStatement stmt
extractExportInfo (JSExportDefault _ stmt _) = extractDefaultExportInfo stmt
extractExportInfo (JSExportLocals (JSExportClause _ specifiers _) _) =
  fmap extractFromExportSpecifier (fromCommaList specifiers)
extractExportInfo (JSExportFrom clause (JSFromClause _ _ moduleName) _) =
  fmap (setExportModule (Text.decodeUtf8 moduleName)) (extractExportInfoFromClause clause)
extractExportInfo _ = []

buildDependencyGraph :: [ModuleDependency] -> [ModuleDependency]
buildDependencyGraph deps = deps

analyzeSideEffects :: JSAST -> AnalysisM ()
analyzeSideEffects = analyzeAST

hasSideEffects :: JSAST -> Bool
hasSideEffects ast = case ast of
  JSAstProgram statements _ -> any hasStatementSideEffects statements
  JSAstModule moduleItems _ -> any hasModuleItemSideEffects moduleItems
  JSAstStatement stmt _ -> hasStatementSideEffects stmt
  JSAstExpression expr _ -> hasExpressionSideEffects expr
  JSAstLiteral expr _ -> hasExpressionSideEffects expr
  where
    hasStatementSideEffects stmt = case stmt of
      JSExpressionStatement expr _ -> hasExpressionSideEffects expr
      JSVariable _ decls _ -> any hasVarDeclSideEffects (fromCommaList decls)
      JSLet _ decls _ -> any hasVarDeclSideEffects (fromCommaList decls) 
      JSConstant _ decls _ -> any hasVarDeclSideEffects (fromCommaList decls)
      JSReturn _ (Just expr) _ -> hasExpressionSideEffects expr
      JSThrow _ _expr _ -> True  -- Always has side effects
      JSIf _ _ test _ thenStmt -> 
        hasExpressionSideEffects test || 
        hasStatementSideEffects thenStmt
      JSIfElse _ _ test _ thenStmt _ elseStmt ->
        hasExpressionSideEffects test || 
        hasStatementSideEffects thenStmt ||
        hasStatementSideEffects elseStmt
      JSStatementBlock _ stmts _ _ -> any hasStatementSideEffects stmts
      _ -> False
      
    hasModuleItemSideEffects item = case item of
      JSModuleStatementListItem stmt -> hasStatementSideEffects stmt
      _ -> False
      
    hasVarDeclSideEffects (JSVarInitExpression _ (JSVarInit _ expr)) = 
      hasExpressionSideEffects expr
    hasVarDeclSideEffects _ = False
    
    hasExpressionSideEffects expr = case expr of
      JSAssignExpression {} -> True
      JSCallExpression {} -> True
      JSCallExpressionDot {} -> True
      JSCallExpressionSquare {} -> True
      JSNewExpression {} -> True
      JSExpressionPostfix _ (JSUnaryOpIncr _) -> True
      JSExpressionPostfix _ (JSUnaryOpDecr _) -> True
      JSUnaryExpression (JSUnaryOpDelete _) _ -> True
      JSExpressionBinary left _ right -> 
        hasExpressionSideEffects left || hasExpressionSideEffects right
      JSExpressionTernary test _ question _ answer ->
        hasExpressionSideEffects test || 
        hasExpressionSideEffects question || 
        hasExpressionSideEffects answer
      JSCommaExpression left _ right ->
        hasExpressionSideEffects left || hasExpressionSideEffects right
      _ -> False

isCallWithSideEffects :: JSExpression -> Bool
isCallWithSideEffects expr = case expr of
  JSCallExpression {} -> True
  JSCallExpressionDot {} -> True
  JSCallExpressionSquare {} -> True
  JSOptionalCallExpression {} -> True
  _ -> False

isPureFunctionCall :: JSExpression -> Bool
isPureFunctionCall expr = not $ isCallWithSideEffects expr

extractIdentifierName :: JSExpression -> Maybe Text.Text
extractIdentifierName (JSIdentifier _ name) = Just $ Text.decodeUtf8 name
extractIdentifierName _ = Nothing

isTopLevelDeclaration :: JSStatement -> Bool
isTopLevelDeclaration stmt = case stmt of
  JSFunction {} -> True
  JSClass {} -> True
  JSVariable {} -> True
  JSLet {} -> True
  JSConstant {} -> True
  _ -> False

isExportedDeclaration :: JSStatement -> Bool
isExportedDeclaration stmt = case stmt of
  JSFunction _ ident _ _ _ _ _ -> isExportedIdent ident
  JSClass _ ident _ _ _ _ _ -> isExportedIdent ident
  JSVariable _ decls _ -> any isExportedVarDecl (fromCommaList decls)
  JSLet _ decls _ -> any isExportedVarDecl (fromCommaList decls)
  JSConstant _ decls _ -> any isExportedVarDecl (fromCommaList decls)
  _ -> False
  where
    isExportedIdent (JSIdentName _ name) = 
      -- Check if identifier is in module exports
      name `elem` ["export", "default"]  -- Simplified for now
    isExportedIdent JSIdentNone = False
    
    isExportedVarDecl (JSIdentifier _ name) = 
      name `elem` ["export", "default"]  -- Simplified for now
    isExportedVarDecl _ = False

calculateEstimatedReduction :: UsageMap -> Double
calculateEstimatedReduction usageMap 
  | Map.null usageMap = 0.0
  | otherwise = fromIntegral unusedCount / fromIntegral totalCount
  where
    totalCount = Map.size usageMap
    unusedCount = Map.size $ Map.filter (not . (^. isUsed)) usageMap

hasIdentifierUsage :: Text.Text -> JSAST -> Bool
hasIdentifierUsage identifier ast = 
  let usageMap = buildUsageMap defaultTreeShakeOptions ast
  in case Map.lookup identifier usageMap of
       Just info -> info ^. isUsed
       Nothing -> False

isExportedIdentifier :: Text.Text -> JSAST -> Bool
isExportedIdentifier identifier ast = 
  let usageMap = buildUsageMap defaultTreeShakeOptions ast
  in case Map.lookup identifier usageMap of
       Just info -> info ^. isExported
       Nothing -> False

-- Helper functions for import/export extraction

extractImportNames :: JSImportClause -> Set.Set Text.Text
extractImportNames clause = case clause of
  JSImportClauseNamed (JSImportsNamed _ specifiers _) ->
    Set.fromList (fmap extractImportSpecifierName (fromCommaList specifiers))
  JSImportClauseDefaultNamed _ _ (JSImportsNamed _ specifiers _) ->
    Set.fromList (fmap extractImportSpecifierName (fromCommaList specifiers))
  _ -> Set.empty

extractImportSpecifierName :: JSImportSpecifier -> Text.Text
extractImportSpecifierName (JSImportSpecifier (JSIdentName _ name)) = Text.decodeUtf8 name
extractImportSpecifierName (JSImportSpecifierAs (JSIdentName _ _) _ (JSIdentName _ alias)) = Text.decodeUtf8 alias
extractImportSpecifierName _ = ""

extractDefaultImport :: JSImportClause -> Maybe Text.Text  
extractDefaultImport (JSImportClauseDefault (JSIdentName _ name)) = Just $ Text.decodeUtf8 name
extractDefaultImport (JSImportClauseDefaultNameSpace (JSIdentName _ name) _ _) = Just $ Text.decodeUtf8 name
extractDefaultImport (JSImportClauseDefaultNamed (JSIdentName _ name) _ _) = Just $ Text.decodeUtf8 name
extractDefaultImport _ = Nothing

extractNamespaceImport :: JSImportClause -> Maybe Text.Text
extractNamespaceImport (JSImportClauseNameSpace (JSImportNameSpace _ _ (JSIdentName _ name))) = Just $ Text.decodeUtf8 name
extractNamespaceImport (JSImportClauseDefaultNameSpace _ _ (JSImportNameSpace _ _ (JSIdentName _ name))) = Just $ Text.decodeUtf8 name
extractNamespaceImport _ = Nothing

extractExportInfoFromStatement :: JSStatement -> [ExportInfo]
extractExportInfoFromStatement stmt = case stmt of
  JSFunction _ (JSIdentName _ name) _ _ _ _ _ ->
    [createExportInfo (Text.decodeUtf8 name)]
  JSVariable _ varList _ ->
    fmap (createExportInfo . extractVarName) (fromCommaList varList)
  JSLet _ varList _ ->
    fmap (createExportInfo . extractVarName) (fromCommaList varList)
  JSConstant _ varList _ ->
    fmap (createExportInfo . extractVarName) (fromCommaList varList)
  _ -> []
  where
    extractVarName (JSIdentifier _ name) = Text.decodeUtf8 name
    extractVarName (JSVarInitExpression (JSIdentifier _ name) _) = Text.decodeUtf8 name
    extractVarName _ = ""

createExportInfo :: Text.Text -> ExportInfo
createExportInfo name = ExportInfo
  { _exportedName = name
  , _localName = Just name
  , _exportModule = Nothing
  , _exportLocation = TokenPn 0 0 0
  , _isDefaultExport = False
  , _isExportTypeOnly = False
  }

extractFromExportSpecifier :: JSExportSpecifier -> ExportInfo  
extractFromExportSpecifier (JSExportSpecifier (JSIdentName _ name)) =
  createExportInfo (Text.decodeUtf8 name)
extractFromExportSpecifier (JSExportSpecifierAs (JSIdentName _ localName) _ (JSIdentName _ exportName)) =
  ExportInfo
    { _exportedName = Text.decodeUtf8 exportName
    , _localName = Just $ Text.decodeUtf8 localName
    , _exportModule = Nothing
    , _exportLocation = TokenPn 0 0 0
    , _isDefaultExport = False
    , _isExportTypeOnly = False
    }
extractFromExportSpecifier _ = createExportInfo ""

setExportModule :: Text.Text -> ExportInfo -> ExportInfo
setExportModule moduleName exportInfo = exportInfo { _exportModule = Just moduleName }

extractExportInfoFromClause :: JSExportClause -> [ExportInfo]
extractExportInfoFromClause (JSExportClause _ specifiers _) =
  fmap extractFromExportSpecifier (fromCommaList specifiers)

-- | Extract export information from default export statements.
extractDefaultExportInfo :: JSStatement -> [ExportInfo]
extractDefaultExportInfo stmt = case stmt of
  -- export default function name() { ... }
  JSFunction _ (JSIdentName _ name) _ _ _ _ _ ->
    [createDefaultExportInfo (Text.decodeUtf8 name)]
  -- export default class Name { ... }
  JSClass _ (JSIdentName _ name) _ _ _ _ _ ->
    [createDefaultExportInfo (Text.decodeUtf8 name)]
  -- export default identifier;
  JSExpressionStatement (JSIdentifier _ name) _ ->
    [createDefaultExportInfo (Text.decodeUtf8 name)]
  -- export default expression;
  JSExpressionStatement _ _ ->
    [createDefaultExportInfo "default"]
  _ -> []

-- | Create export info for default exports.
createDefaultExportInfo :: Text.Text -> ExportInfo
createDefaultExportInfo name = ExportInfo
  { _exportedName = name
  , _localName = Just name
  , _exportModule = Nothing
  , _exportLocation = TokenPn 0 0 0
  , _isDefaultExport = True
  , _isExportTypeOnly = False
  }

-- Helper functions

countUnused :: UsageMap -> Int
countUnused = Map.size . Map.filter (not . (^. isUsed))

countSideEffects :: UsageMap -> Int  
countSideEffects = Map.size . Map.filter (^. Types.hasSideEffects)


when :: Applicative f => Bool -> f () -> f ()
when True action = action
when False _ = pure ()

-- | Check if expression represents dynamic code execution (eval or Function constructor)
isDynamicCodeCall :: JSExpression -> Bool
isDynamicCodeCall (JSIdentifier _ "eval") = True
isDynamicCodeCall (JSIdentifier _ "Function") = True
isDynamicCodeCall _ = False

-- | Mark that an eval call was encountered.
markHasEvalCall :: AnalysisM ()
markHasEvalCall = modify (\s -> s { _analysisHasEval = True, _analysisEvalCount = _analysisEvalCount s + 1 })

-- | Analyze arguments to eval/Function calls and mark potential identifiers as used.
markPotentialEvalIdentifiers :: JSCommaList JSExpression -> AnalysisM ()
markPotentialEvalIdentifiers args = do
  usageMap <- gets _analysisUsageMap
  let allIdentifiers = Set.fromList (Map.keys usageMap)
  traverse_ (analyzeStringLiteralForIdentifiers allIdentifiers) (fromCommaList args)

-- | Analyze a string literal argument to eval/Function and mark identifiers as used.
analyzeStringLiteralForIdentifiers :: Set.Set Text.Text -> JSExpression -> AnalysisM ()
analyzeStringLiteralForIdentifiers knownIdentifiers expr = case expr of
  JSStringLiteral _ quotedStr -> do
    -- Remove quotes and extract content
    let unquoted = Text.dropWhile (== '"') (Text.dropWhileEnd (== '"')
                   (Text.dropWhile (== '\'') (Text.dropWhileEnd (== '\'') (Text.decodeUtf8 quotedStr))))
    let foundIdentifiers = extractIdentifiersFromJSString unquoted knownIdentifiers
    traverse_ markIdentifierUsed foundIdentifiers
  _ -> pure ()  -- Not a string literal, skip

-- | Extract identifiers from JavaScript code string that match known identifiers.
extractIdentifiersFromJSString :: Text.Text -> Set.Set Text.Text -> [Text.Text]
extractIdentifiersFromJSString jsCode knownIdentifiers =
  let potentialIdentifiers = Set.toList knownIdentifiers
      foundIdentifiers = filter (`Text.isInfixOf` jsCode) potentialIdentifiers
  in foundIdentifiers


