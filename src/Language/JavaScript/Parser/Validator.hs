{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive AST validation for JavaScript syntax trees.
--
-- This module provides validation functions that ensure JavaScript ASTs
-- represent syntactically and semantically valid programs. The validator 
-- detects structural issues that the parser cannot catch, including:
--
--   * Invalid control flow (break/continue/return/yield/await in wrong contexts)
--   * Invalid assignment targets and destructuring patterns
--   * Strict mode violations and reserved word usage
--   * Function parameter duplicates and invalid patterns
--   * Class inheritance and method definition violations
--   * Module import/export semantic errors
--   * ES6+ feature constraint violations
--
-- ==== Examples
--
-- >>> validate validProgram
-- Right (ValidAST validProgram)
--
-- >>> validate invalidProgram  
-- Left [BreakOutsideLoop (TokenPn 0 1 1), InvalidAssignmentTarget ...]
--
-- @since 0.7.1.0
module Language.JavaScript.Parser.Validator
  ( ValidationError(..)
  , ValidationContext(..)
  , ValidAST(..)
  , ValidationResult
  , StrictMode(..)
  , validate
  , validateWithStrictMode
  , validateStatement
  , validateExpression
  , validateModuleItem
  , validateAssignmentTarget
  , errorToString
  , errorToStringWithContext
  , errorsToString
  , getErrorPosition
  , formatElmStyleError
  ) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (group, isSuffixOf, nub, sort)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

import Language.JavaScript.Parser.AST
  ( JSAST(..)
  , JSStatement(..)
  , JSExpression(..)
  , JSModuleItem(..)
  , JSBlock(..)
  , JSIdent(..)
  , JSVarInitializer(..)
  , JSObjectProperty(..)
  , JSMethodDefinition(..)
  , JSPropertyName(..)
  , JSAccessor(..)
  , JSArrayElement(..)
  , JSCommaList(..)
  , JSCommaTrailingList(..)
  , JSArrowParameterList(..)
  , JSConciseBody(..)
  , JSTryCatch(..)
  , JSTryFinally(..)
  , JSSwitchParts(..)
  , JSImportDeclaration(..)
  , JSImportClause(..)
  , JSFromClause(..)
  , JSImportNameSpace(..)
  , JSImportsNamed(..)
  , JSImportSpecifier(..)
  , JSImportAttributes(..)
  , JSImportAttribute(..)
  , JSExportDeclaration(..)
  , JSExportClause(..)
  , JSExportSpecifier(..)
  , JSTemplatePart(..)
  , JSClassHeritage(..)
  , JSClassElement(..)
  , JSAnnot(..)
  , JSBinOp(..)
  , JSUnaryOp(..)
  , JSAssignOp(..)
  , JSSemi(..)
  )
import Language.JavaScript.Parser.SrcLocation (TokenPosn(..))

-- | Strongly typed validation errors with comprehensive JavaScript coverage.
data ValidationError
  -- Control Flow Errors  
  = BreakOutsideLoop !TokenPosn
  | BreakOutsideSwitch !TokenPosn
  | ContinueOutsideLoop !TokenPosn
  | ReturnOutsideFunction !TokenPosn
  | YieldOutsideGenerator !TokenPosn
  | YieldInParameterDefault !TokenPosn
  | AwaitOutsideAsync !TokenPosn
  | AwaitInParameterDefault !TokenPosn
  
  -- Assignment and Binding Errors
  | InvalidAssignmentTarget !JSExpression !TokenPosn
  | InvalidDestructuringTarget !JSExpression !TokenPosn
  | DuplicateParameter !Text !TokenPosn
  | DuplicateBinding !Text !TokenPosn
  | ConstWithoutInitializer !Text !TokenPosn
  | InvalidLHSInForIn !JSExpression !TokenPosn
  | InvalidLHSInForOf !JSExpression !TokenPosn
  
  -- Function and Class Errors
  | DuplicateMethodName !Text !TokenPosn
  | MultipleConstructors !TokenPosn
  | ConstructorWithGenerator !TokenPosn
  | ConstructorWithAsyncGenerator !TokenPosn
  | StaticConstructor !TokenPosn
  | GetterWithParameters !TokenPosn
  | SetterWithoutParameter !TokenPosn
  | SetterWithMultipleParameters !TokenPosn
  
  -- Strict Mode Violations
  | StrictModeViolation !StrictModeError !TokenPosn
  | InvalidOctalInStrict !Text !TokenPosn
  | DuplicatePropertyInStrict !Text !TokenPosn
  | WithStatementInStrict !TokenPosn
  | DeleteOfUnqualifiedInStrict !TokenPosn
  
  -- ES6+ Feature Errors
  | InvalidSuperUsage !TokenPosn
  | SuperOutsideClass !TokenPosn
  | SuperPropertyOutsideMethod !TokenPosn
  | InvalidNewTarget !TokenPosn
  | NewTargetOutsideFunction !TokenPosn
  | ComputedPropertyInPattern !TokenPosn
  | RestElementNotLast !TokenPosn
  | RestParameterDefault !TokenPosn
  
  -- Module Errors
  | ExportOutsideModule !TokenPosn
  | ImportOutsideModule !TokenPosn
  | ImportMetaOutsideModule !TokenPosn
  | DuplicateExport !Text !TokenPosn
  | DuplicateImport !Text !TokenPosn
  | InvalidExportDefault !TokenPosn
  
  -- Literal and Expression Errors
  | InvalidRegexFlags !Text !TokenPosn
  | InvalidRegexPattern !Text !TokenPosn
  | InvalidNumericLiteral !Text !TokenPosn
  | InvalidBigIntLiteral !Text !TokenPosn
  | InvalidEscapeSequence !Text !TokenPosn
  | UnterminatedTemplateLiteral !TokenPosn
  
  -- Private Field Errors
  | PrivateFieldOutsideClass !Text !TokenPosn
  | PrivateMethodOutsideClass !Text !TokenPosn
  | PrivateAccessorOutsideClass !Text !TokenPosn

  -- Malformed Syntax Recovery Errors
  | UnclosedBracket !Text !TokenPosn
  | UnclosedParenthesis !Text !TokenPosn
  | IncompleteExpression !Text !TokenPosn
  | InvalidDestructuringPattern !Text !TokenPosn
  | MalformedTemplateLiteral !Text !TokenPosn
  
  -- Syntax Context Errors
  | LabelNotFound !Text !TokenPosn
  | DuplicateLabel !Text !TokenPosn
  | InvalidLabelTarget !Text !TokenPosn
  | FunctionNameRequired !TokenPosn
  | UnexpectedToken !Text !TokenPosn
  | ReservedWordAsIdentifier !Text !TokenPosn
  | FutureReservedWord !Text !TokenPosn
  | MultipleDefaultCases !TokenPosn
  
  deriving (Eq, Generic, NFData, Show)

-- | Strict mode error subtypes.
data StrictModeError
  = ArgumentsBinding
  | EvalBinding
  | OctalLiteral
  | DuplicateProperty
  | DuplicateParameterStrict
  | DeleteUnqualified
  | WithStatement
  deriving (Eq, Generic, NFData, Show)

-- | Strict mode context tracking.
data StrictMode
  = StrictModeOn
  | StrictModeOff
  | StrictModeInferred -- Inferred from module context or "use strict"
  deriving (Eq, Generic, NFData, Show)

-- | Extended validation context tracking all JavaScript constructs.
data ValidationContext = ValidationContext
  { contextInLoop :: !Bool
  , contextInFunction :: !Bool  
  , contextInClass :: !Bool
  , contextInModule :: !Bool
  , contextInGenerator :: !Bool
  , contextInAsync :: !Bool
  , contextInSwitch :: !Bool
  , contextInMethod :: !Bool
  , contextInConstructor :: !Bool
  , contextInStaticMethod :: !Bool
  , contextStrictMode :: !StrictMode
  , contextLabels :: ![Text]
  , contextBindings :: ![Text]  -- Track all bound names for duplicate detection
  , contextSuperContext :: !Bool -- Track if super is valid
  } deriving (Eq, Generic, NFData, Show)

-- | Validated AST wrapper ensuring structural correctness.
newtype ValidAST = ValidAST JSAST
  deriving (Eq, Generic, NFData, Show)

-- | Validation result type.
type ValidationResult = Either [ValidationError] ValidAST

-- | Convert validation error to human-readable string.
errorToString :: ValidationError -> String
errorToString = errorToStringSimple

-- | Convert validation error to Elm-style formatted string with source context.
errorToStringWithContext :: Text -> ValidationError -> String
errorToStringWithContext sourceCode err = 
  let pos = getErrorPosition err
      (line, col) = (getErrorLine pos, getErrorColumn pos)
      errorMsg = errorToStringSimple err
      contextLines = getSourceContext sourceCode (line, col)
  in formatElmStyleError errorMsg (line, col) contextLines

-- | Get position from validation error.
getErrorPosition :: ValidationError -> TokenPosn
getErrorPosition err = case err of
  -- Control Flow Errors  
  BreakOutsideLoop pos -> pos
  BreakOutsideSwitch pos -> pos
  ContinueOutsideLoop pos -> pos
  ReturnOutsideFunction pos -> pos
  YieldOutsideGenerator pos -> pos
  YieldInParameterDefault pos -> pos
  AwaitOutsideAsync pos -> pos
  AwaitInParameterDefault pos -> pos
  
  -- Assignment and Binding Errors
  InvalidAssignmentTarget _ pos -> pos
  InvalidDestructuringTarget _ pos -> pos
  DuplicateParameter _ pos -> pos
  DuplicateBinding _ pos -> pos
  ConstWithoutInitializer _ pos -> pos
  InvalidLHSInForIn _ pos -> pos
  InvalidLHSInForOf _ pos -> pos
  
  -- Function and Class Errors
  DuplicateMethodName _ pos -> pos
  MultipleConstructors pos -> pos
  ConstructorWithGenerator pos -> pos
  ConstructorWithAsyncGenerator pos -> pos
  StaticConstructor pos -> pos
  GetterWithParameters pos -> pos
  SetterWithoutParameter pos -> pos
  SetterWithMultipleParameters pos -> pos
  
  -- Strict Mode Violations
  StrictModeViolation _ pos -> pos
  InvalidOctalInStrict _ pos -> pos
  DuplicatePropertyInStrict _ pos -> pos
  WithStatementInStrict pos -> pos
  DeleteOfUnqualifiedInStrict pos -> pos
  
  -- ES6+ Feature Errors
  InvalidSuperUsage pos -> pos
  SuperOutsideClass pos -> pos
  SuperPropertyOutsideMethod pos -> pos
  InvalidNewTarget pos -> pos
  NewTargetOutsideFunction pos -> pos
  ComputedPropertyInPattern pos -> pos
  RestElementNotLast pos -> pos
  RestParameterDefault pos -> pos
  
  -- Module Errors
  ExportOutsideModule pos -> pos
  ImportOutsideModule pos -> pos
  ImportMetaOutsideModule pos -> pos
  DuplicateExport _ pos -> pos
  
  -- Literal and Expression Errors
  InvalidRegexFlags _ pos -> pos
  InvalidRegexPattern _ pos -> pos
  InvalidNumericLiteral _ pos -> pos
  InvalidBigIntLiteral _ pos -> pos
  InvalidEscapeSequence _ pos -> pos
  UnterminatedTemplateLiteral pos -> pos
  
  -- Private Field Errors
  PrivateFieldOutsideClass _ pos -> pos
  PrivateMethodOutsideClass _ pos -> pos
  PrivateAccessorOutsideClass _ pos -> pos
  
  -- Malformed Syntax Recovery Errors
  UnclosedBracket _ pos -> pos
  UnclosedParenthesis _ pos -> pos
  IncompleteExpression _ pos -> pos
  InvalidDestructuringPattern _ pos -> pos
  MalformedTemplateLiteral _ pos -> pos
  
  -- Syntax Context Errors
  LabelNotFound _ pos -> pos
  DuplicateLabel _ pos -> pos
  InvalidLabelTarget _ pos -> pos
  FunctionNameRequired pos -> pos
  UnexpectedToken _ pos -> pos
  ReservedWordAsIdentifier _ pos -> pos
  FutureReservedWord _ pos -> pos
  DuplicateImport _ pos -> pos
  InvalidExportDefault pos -> pos
  MultipleDefaultCases pos -> pos

-- | Extract line number from TokenPosn.
getErrorLine :: TokenPosn -> Int
getErrorLine (TokenPn _ line _) = line

-- | Extract column number from TokenPosn.
getErrorColumn :: TokenPosn -> Int  
getErrorColumn (TokenPn _ _ col) = col

-- | Get source code context around error position.
getSourceContext :: Text -> (Int, Int) -> [Text]
getSourceContext sourceCode (line, _col) =
  let sourceLines = Text.lines sourceCode
      lineIdx = line - 1 -- Convert to 0-based indexing
      startIdx = max 0 (lineIdx - 2)
      endIdx = min (length sourceLines - 1) (lineIdx + 2)
      contextLines = take (endIdx - startIdx + 1) (drop startIdx sourceLines)
  in contextLines

-- | Format error in Elm style with source context.
formatElmStyleError :: String -> (Int, Int) -> [Text] -> String
formatElmStyleError errorMsg (line, col) contextLines = 
  unlines $
    [ "-- VALIDATION ERROR ---------------------------------------------------------------"
    , ""
    , errorMsg
    , ""
    , "Error occurred at line " ++ show line ++ ", column " ++ show col ++ ":"
    , ""
    ] ++ 
    formatContextLines contextLines line ++
    [ ""
    , "Hint: Check the JavaScript syntax and make sure it follows language rules."
    , ""
    ]

-- | Format context lines with line numbers and error pointer.
formatContextLines :: [Text] -> Int -> [String]
formatContextLines contextLines errorLine = 
  let startLine = errorLine - length contextLines + 1
  in concatMap (formatContextLine startLine errorLine) (zip [0..] contextLines)

-- | Format single context line.
formatContextLine :: Int -> Int -> (Int, Text) -> [String]
formatContextLine startLine errorLine (idx, lineText) =
  let currentLine = startLine + idx
      lineNumStr = show currentLine
      lineNumPadded = replicate (4 - length lineNumStr) ' ' ++ lineNumStr
      lineContent = lineNumPadded ++ "│ " ++ Text.unpack lineText
  in if currentLine == errorLine
     then [ lineContent
          , replicate 4 ' ' ++ "│ " ++ replicate (length (Text.unpack lineText)) '^'
          ]
     else [lineContent]

-- | Simple error to string conversion (original implementation).
errorToStringSimple :: ValidationError -> String
errorToStringSimple err = case err of
  -- Control Flow Errors
  BreakOutsideLoop pos -> 
    "Break statement must be inside a loop or switch statement " ++ showPos pos
  BreakOutsideSwitch pos -> 
    "Break statement with label must target a labeled statement " ++ showPos pos
  ContinueOutsideLoop pos -> 
    "Continue statement must be inside a loop " ++ showPos pos
  ReturnOutsideFunction pos -> 
    "Return statement must be inside a function " ++ showPos pos
  YieldOutsideGenerator pos -> 
    "Yield expression must be inside a generator function " ++ showPos pos
  YieldInParameterDefault pos -> 
    "Yield expression not allowed in parameter default value " ++ showPos pos
  AwaitOutsideAsync pos -> 
    "Await expression must be inside an async function " ++ showPos pos
  AwaitInParameterDefault pos -> 
    "Await expression not allowed in parameter default value " ++ showPos pos
  
  -- Assignment and Binding Errors
  InvalidAssignmentTarget _expr pos -> 
    "Invalid left-hand side in assignment " ++ showPos pos
  InvalidDestructuringTarget _expr pos -> 
    "Invalid destructuring assignment target " ++ showPos pos
  DuplicateParameter name pos -> 
    "Duplicate parameter name '" ++ Text.unpack name ++ "' " ++ showPos pos
  DuplicateBinding name pos -> 
    "Duplicate binding '" ++ Text.unpack name ++ "' " ++ showPos pos
  ConstWithoutInitializer name pos -> 
    "Missing initializer in const declaration '" ++ Text.unpack name ++ "' " ++ showPos pos
  InvalidLHSInForIn _expr pos -> 
    "Invalid left-hand side in for-in loop " ++ showPos pos
  InvalidLHSInForOf _expr pos -> 
    "Invalid left-hand side in for-of loop " ++ showPos pos
  
  -- Function and Class Errors
  DuplicateMethodName name pos -> 
    "Duplicate method name '" ++ Text.unpack name ++ "' " ++ showPos pos
  MultipleConstructors pos -> 
    "A class may only have one constructor " ++ showPos pos
  ConstructorWithGenerator pos -> 
    "Class constructor may not be a generator " ++ showPos pos
  ConstructorWithAsyncGenerator pos -> 
    "Class constructor may not be an async generator " ++ showPos pos
  StaticConstructor pos -> 
    "Class constructor may not be static " ++ showPos pos
  GetterWithParameters pos -> 
    "Getter must not have parameters " ++ showPos pos
  SetterWithoutParameter pos -> 
    "Setter must have exactly one parameter " ++ showPos pos
  SetterWithMultipleParameters pos -> 
    "Setter must have exactly one parameter " ++ showPos pos
  
  -- Strict Mode Violations
  StrictModeViolation strictErr pos -> 
    "Strict mode violation: " ++ strictModeErrorToString strictErr ++ " " ++ showPos pos
  InvalidOctalInStrict literal pos -> 
    "Octal literals not allowed in strict mode: " ++ Text.unpack literal ++ " " ++ showPos pos
  DuplicatePropertyInStrict name pos -> 
    "Duplicate property '" ++ Text.unpack name ++ "' not allowed in strict mode " ++ showPos pos
  WithStatementInStrict pos -> 
    "With statement not allowed in strict mode " ++ showPos pos
  DeleteOfUnqualifiedInStrict pos -> 
    "Delete of unqualified identifier not allowed in strict mode " ++ showPos pos
  
  -- ES6+ Feature Errors
  InvalidSuperUsage pos -> 
    "Invalid use of 'super' keyword " ++ showPos pos
  SuperOutsideClass pos -> 
    "'super' keyword must be used within a class " ++ showPos pos
  SuperPropertyOutsideMethod pos -> 
    "'super' property access must be within a method " ++ showPos pos
  InvalidNewTarget pos -> 
    "Invalid use of 'new.target' " ++ showPos pos
  NewTargetOutsideFunction pos -> 
    "'new.target' must be used within a function " ++ showPos pos
  ComputedPropertyInPattern pos -> 
    "Computed property names not allowed in patterns " ++ showPos pos
  RestElementNotLast pos -> 
    "Rest element must be last in destructuring pattern " ++ showPos pos
  RestParameterDefault pos -> 
    "Rest parameter may not have a default value " ++ showPos pos
  
  -- Module Errors
  ExportOutsideModule pos -> 
    "Export statement must be at module level " ++ showPos pos
  ImportOutsideModule pos -> 
    "Import statement must be at module level " ++ showPos pos
  ImportMetaOutsideModule pos -> 
    "import.meta can only be used in module context " ++ showPos pos
  DuplicateExport name pos -> 
    "Duplicate export '" ++ Text.unpack name ++ "' " ++ showPos pos
  DuplicateImport name pos -> 
    "Duplicate import '" ++ Text.unpack name ++ "' " ++ showPos pos
  InvalidExportDefault pos -> 
    "Invalid export default declaration " ++ showPos pos
  
  -- Literal and Expression Errors
  InvalidRegexFlags flags pos -> 
    "Invalid regular expression flags: " ++ Text.unpack flags ++ " " ++ showPos pos
  InvalidRegexPattern pattern pos -> 
    "Invalid regular expression pattern: " ++ Text.unpack pattern ++ " " ++ showPos pos
  InvalidNumericLiteral literal pos -> 
    "Invalid numeric literal: " ++ Text.unpack literal ++ " " ++ showPos pos
  InvalidBigIntLiteral literal pos -> 
    "Invalid BigInt literal: " ++ Text.unpack literal ++ " " ++ showPos pos
  InvalidEscapeSequence escSeq pos -> 
    "Invalid escape sequence: " ++ Text.unpack escSeq ++ " " ++ showPos pos
  UnterminatedTemplateLiteral pos -> 
    "Unterminated template literal " ++ showPos pos
  
  -- Private Field Errors
  PrivateFieldOutsideClass fieldName pos -> 
    "Private field '" ++ Text.unpack fieldName ++ "' can only be used within a class " ++ showPos pos
  PrivateMethodOutsideClass methodName pos -> 
    "Private method '" ++ Text.unpack methodName ++ "' can only be used within a class " ++ showPos pos
  PrivateAccessorOutsideClass accessorName pos -> 
    "Private accessor '" ++ Text.unpack accessorName ++ "' can only be used within a class " ++ showPos pos
  
  -- Malformed Syntax Recovery Errors  
  UnclosedBracket bracket pos -> 
    "Unclosed bracket '" ++ Text.unpack bracket ++ "' " ++ showPos pos
  UnclosedParenthesis paren pos -> 
    "Unclosed parenthesis '" ++ Text.unpack paren ++ "' " ++ showPos pos
  IncompleteExpression expr pos -> 
    "Incomplete expression '" ++ Text.unpack expr ++ "' " ++ showPos pos
  InvalidDestructuringPattern pattern pos -> 
    "Invalid destructuring pattern '" ++ Text.unpack pattern ++ "' " ++ showPos pos
  MalformedTemplateLiteral template pos -> 
    "Malformed template literal '" ++ Text.unpack template ++ "' " ++ showPos pos
  
  -- Syntax Context Errors
  LabelNotFound label pos -> 
    "Label '" ++ Text.unpack label ++ "' not found " ++ showPos pos
  DuplicateLabel label pos -> 
    "Duplicate label '" ++ Text.unpack label ++ "' " ++ showPos pos
  InvalidLabelTarget label pos -> 
    "Invalid target for label '" ++ Text.unpack label ++ "' " ++ showPos pos
  FunctionNameRequired pos -> 
    "Function name required in this context " ++ showPos pos
  UnexpectedToken token pos -> 
    "Unexpected token '" ++ Text.unpack token ++ "' " ++ showPos pos
  ReservedWordAsIdentifier word pos -> 
    "'" ++ Text.unpack word ++ "' is a reserved word " ++ showPos pos
  FutureReservedWord word pos -> 
    "'" ++ Text.unpack word ++ "' is a future reserved word " ++ showPos pos
  MultipleDefaultCases pos ->
    "Switch statement cannot have multiple default clauses " ++ showPos pos

-- | Convert list of validation errors to formatted string.
errorsToString :: [ValidationError] -> String
errorsToString errors = 
  "Validation failed with " ++ show (length errors) ++ " error(s):\n" ++
  unlines (map (("  • " ++) . errorToString) errors)

-- | Convert strict mode error to string.
strictModeErrorToString :: StrictModeError -> String
strictModeErrorToString err = case err of
  ArgumentsBinding -> "Cannot bind 'arguments' in strict mode"
  EvalBinding -> "Cannot bind 'eval' in strict mode" 
  OctalLiteral -> "Octal literals not allowed"
  DuplicateProperty -> "Duplicate property not allowed"
  DuplicateParameterStrict -> "Duplicate parameter not allowed"
  DeleteUnqualified -> "Delete of unqualified identifier not allowed"
  WithStatement -> "With statement not allowed"

-- | Show position information.
showPos :: TokenPosn -> String
showPos (TokenPn _addr line col) = "at line " ++ show line ++ ", column " ++ show col

-- | Default validation context (not inside any special construct).
defaultContext :: ValidationContext
defaultContext = ValidationContext
  { contextInLoop = False
  , contextInFunction = False
  , contextInClass = False
  , contextInModule = False
  , contextInGenerator = False
  , contextInAsync = False
  , contextInSwitch = False
  , contextInMethod = False
  , contextInConstructor = False
  , contextInStaticMethod = False
  , contextStrictMode = StrictModeOff
  , contextLabels = []
  , contextBindings = []
  , contextSuperContext = False
  }

-- | Main validation entry point for JavaScript ASTs.
validate :: JSAST -> ValidationResult
validate = validateWithStrictMode StrictModeOff

-- | Main validation with explicit strict mode setting.
validateWithStrictMode :: StrictMode -> JSAST -> ValidationResult
validateWithStrictMode strictMode ast = 
  case validateAST (defaultContext { contextStrictMode = strictMode }) ast of
    [] -> Right (ValidAST ast)
    errors -> Left errors

-- | Validate JavaScript AST structure with comprehensive edge case coverage.
validateAST :: ValidationContext -> JSAST -> [ValidationError]
validateAST ctx ast = case ast of
  JSAstProgram stmts _annot ->
    let strictMode = detectStrictMode stmts
        ctx' = ctx { contextStrictMode = strictMode }
    in validateStatementsWithLabels ctx' stmts ++
       validateProgramLevel stmts

  JSAstModule items _annot ->
    let moduleContext = ctx { contextInModule = True, contextStrictMode = StrictModeOn }
    in concatMap (validateModuleItem moduleContext) items ++
       validateModuleLevel items

  JSAstStatement stmt _annot ->
    validateStatement ctx stmt

  JSAstExpression expr _annot ->
    validateExpression ctx expr

  JSAstLiteral expr _annot ->
    validateExpression ctx expr

-- | Detect strict mode from program statements.
detectStrictMode :: [JSStatement] -> StrictMode
detectStrictMode stmts = 
  case stmts of
    (JSExpressionStatement (JSStringLiteral _annot "use strict") _):_ -> StrictModeOn
    _ -> StrictModeOff

-- | Validate statements sequentially with accumulated label context.
validateStatementsWithLabels :: ValidationContext -> [JSStatement] -> [ValidationError]
validateStatementsWithLabels ctx stmts = 
  validateDuplicateLabelsInStatements stmts ++
  concatMap (validateStatement ctx) stmts

-- | Validate that no duplicate labels exist in the same statement list.
validateDuplicateLabelsInStatements :: [JSStatement] -> [ValidationError]
validateDuplicateLabelsInStatements stmts =
  let labels = concatMap extractLabelFromStatement stmts
      duplicates = findDuplicateLabels labels
  in map (\(labelText, pos) -> DuplicateLabel labelText pos) duplicates
  where
    extractLabelFromStatement stmt = case stmt of
      JSLabelled label _colon _stmt -> case label of
        JSIdentName _annot labelName -> 
          [(Text.pack labelName, extractIdentPos label)]
        JSIdentNone -> []
      _ -> []
    
    findDuplicateLabels :: [(Text, TokenPosn)] -> [(Text, TokenPosn)]
    findDuplicateLabels labelList =
      let labelCounts = Map.fromListWith (++) [(name, [pos]) | (name, pos) <- labelList]
          duplicateEntries = Map.filter ((>1) . length) labelCounts
      in [(name, head positions) | (name, positions) <- Map.toList duplicateEntries]


-- | Validate program-level constraints.
validateProgramLevel :: [JSStatement] -> [ValidationError]
validateProgramLevel stmts = 
  validateNoDuplicateFunctionDeclarations stmts

-- | Validate module-level constraints.
validateModuleLevel :: [JSModuleItem] -> [ValidationError]
validateModuleLevel items =
  validateNoDuplicateExports items ++
  validateNoDuplicateImports items

-- | Validate JavaScript statements with comprehensive edge case coverage.
validateStatement :: ValidationContext -> JSStatement -> [ValidationError]
validateStatement ctx stmt = case stmt of
  JSStatementBlock _annot stmts _rbrace _semi ->
    concatMap (validateStatement ctx) stmts

  JSBreak _annot ident _semi ->
    validateBreakStatement ctx ident

  JSContinue _annot ident _semi ->
    validateContinueStatement ctx ident

  JSLet _annot exprs _semi ->
    concatMap (validateExpression ctx) (fromCommaList exprs) ++
    validateLetDeclarations ctx (fromCommaList exprs)

  JSConstant _annot exprs _semi ->
    concatMap (validateExpression ctx) (fromCommaList exprs) ++
    validateConstDeclarations ctx (fromCommaList exprs)

  JSClass _annot name heritage _lbrace elements _rbrace _semi ->
    let classCtx = ctx { contextInClass = True, contextSuperContext = hasHeritage heritage }
    in validateClassHeritage ctx heritage ++
       concatMap (validateClassElement classCtx) elements ++
       validateClassElements elements

  JSDoWhile _do stmt _while _lparen expr _rparen _semi ->
    let loopCtx = ctx { contextInLoop = True }
    in validateStatement loopCtx stmt ++
       validateExpression ctx expr

  JSFor _for _lparen init _semi1 test _semi2 update _rparen stmt ->
    let loopCtx = ctx { contextInLoop = True }
    in concatMap (validateExpression ctx) (fromCommaList init) ++
       concatMap (validateExpression ctx) (fromCommaList test) ++
       concatMap (validateExpression ctx) (fromCommaList update) ++
       validateStatement loopCtx stmt

  JSForIn _for _lparen lhs _in rhs _rparen stmt ->
    let loopCtx = ctx { contextInLoop = True }
    in validateForInLHS ctx lhs ++
       validateExpression ctx rhs ++
       validateStatement loopCtx stmt

  JSForVar _for _lparen _var decls _semi1 test _semi2 update _rparen stmt ->
    let loopCtx = ctx { contextInLoop = True }
    in concatMap (validateExpression ctx) (fromCommaList decls) ++
       concatMap (validateExpression ctx) (fromCommaList test) ++
       concatMap (validateExpression ctx) (fromCommaList update) ++
       validateStatement loopCtx stmt

  JSForVarIn _for _lparen _var lhs _in rhs _rparen stmt ->
    let loopCtx = ctx { contextInLoop = True }
    in validateForInLHS ctx lhs ++
       validateExpression ctx rhs ++
       validateStatement loopCtx stmt

  JSForLet _for _lparen _let decls _semi1 test _semi2 update _rparen stmt ->
    let loopCtx = ctx { contextInLoop = True }
    in concatMap (validateExpression ctx) (fromCommaList decls) ++
       concatMap (validateExpression ctx) (fromCommaList test) ++
       concatMap (validateExpression ctx) (fromCommaList update) ++
       validateStatement loopCtx stmt

  JSForLetIn _for _lparen _let lhs _in rhs _rparen stmt ->
    let loopCtx = ctx { contextInLoop = True }
    in validateForInLHS ctx lhs ++
       validateExpression ctx rhs ++
       validateStatement loopCtx stmt

  JSForLetOf _for _lparen _let lhs _of rhs _rparen stmt ->
    let loopCtx = ctx { contextInLoop = True }
    in validateForOfLHS ctx lhs ++
       validateExpression ctx rhs ++
       validateStatement loopCtx stmt

  JSForConst _for _lparen _const decls _semi1 test _semi2 update _rparen stmt ->
    let loopCtx = ctx { contextInLoop = True }
    in concatMap (validateExpression ctx) (fromCommaList decls) ++
       concatMap (validateExpression ctx) (fromCommaList test) ++
       concatMap (validateExpression ctx) (fromCommaList update) ++
       validateStatement loopCtx stmt

  JSForConstIn _for _lparen _const lhs _in rhs _rparen stmt ->
    let loopCtx = ctx { contextInLoop = True }
    in validateForInLHS ctx lhs ++
       validateExpression ctx rhs ++
       validateStatement loopCtx stmt

  JSForConstOf _for _lparen _const lhs _of rhs _rparen stmt ->
    let loopCtx = ctx { contextInLoop = True }
    in validateForOfLHS ctx lhs ++
       validateExpression ctx rhs ++
       validateStatement loopCtx stmt

  JSForOf _for _lparen lhs _of rhs _rparen stmt ->
    let loopCtx = ctx { contextInLoop = True }
    in validateForOfLHS ctx lhs ++
       validateExpression ctx rhs ++
       validateStatement loopCtx stmt

  JSForVarOf _for _lparen _var lhs _of rhs _rparen stmt ->
    let loopCtx = ctx { contextInLoop = True }
    in validateForOfLHS ctx lhs ++
       validateExpression ctx rhs ++
       validateStatement loopCtx stmt

  JSAsyncFunction _async _function name _lparen params _rparen block _semi ->
    let funcCtx = ctx { contextInFunction = True, contextInAsync = True }
    in validateFunctionName ctx name ++
       validateFunctionParameters ctx (fromCommaList params) ++
       validateBlock funcCtx block

  JSFunction _function name _lparen params _rparen block _semi ->
    let funcCtx = ctx { contextInFunction = True }
    in validateFunctionName ctx name ++
       validateFunctionParameters ctx (fromCommaList params) ++
       validateBlock funcCtx block

  JSGenerator _function _star name _lparen params _rparen block _semi ->
    let genCtx = ctx { contextInFunction = True, contextInGenerator = True }
    in validateFunctionName ctx name ++
       validateFunctionParameters ctx (fromCommaList params) ++
       validateBlock genCtx block

  JSIf _if _lparen test _rparen consequent ->
    validateExpression ctx test ++
    validateStatement ctx consequent

  JSIfElse _if _lparen test _rparen consequent _else alternate ->
    validateExpression ctx test ++
    validateStatement ctx consequent ++
    validateStatement ctx alternate

  JSLabelled label _colon stmt ->
    validateLabelledStatement ctx label stmt

  JSEmptyStatement _semi -> []

  JSExpressionStatement expr _semi ->
    validateExpression ctx expr

  JSAssignStatement lhs _op rhs _semi ->
    validateExpression ctx lhs ++
    validateExpression ctx rhs ++
    validateAssignmentTarget lhs

  JSMethodCall expr _lparen args _rparen _semi ->
    validateExpression ctx expr ++
    concatMap (validateExpression ctx) (fromCommaList args)

  JSReturn _return maybeExpr _semi ->
    validateReturnStatement ctx maybeExpr

  JSSwitch _switch _lparen discriminant _rparen _lbrace cases _rbrace _semi ->
    let switchCtx = ctx { contextInSwitch = True }
    in validateExpression ctx discriminant ++
       concatMap (validateSwitchCase switchCtx) cases ++
       validateSwitchCases cases

  JSThrow _throw expr _semi ->
    validateExpression ctx expr

  JSTry _try block catches finally' ->
    validateBlock ctx block ++
    concatMap (validateCatchClause ctx) catches ++
    validateFinallyClause ctx finally'

  JSVariable _var decls _semi ->
    concatMap (validateExpression ctx) (fromCommaList decls)

  JSWhile _while _lparen test _rparen stmt ->
    let loopCtx = ctx { contextInLoop = True }
    in validateExpression ctx test ++
       validateStatement loopCtx stmt

  JSWith _with _lparen object _rparen stmt _semi ->
    validateWithStatement ctx object stmt

-- | Validate JavaScript expressions with comprehensive edge case coverage.
validateExpression :: ValidationContext -> JSExpression -> [ValidationError]
validateExpression ctx expr = case expr of
  -- Terminals require validation for strict mode and literal correctness
  JSIdentifier _annot name ->
    validateIdentifier ctx name

  JSDecimal _annot literal ->
    validateNumericLiteral literal

  JSLiteral _annot literal ->
    validateLiteral ctx literal

  JSHexInteger _annot literal ->
    validateHexLiteral literal

  JSBinaryInteger _annot literal ->
    validateBinaryLiteral literal

  JSOctal _annot literal ->
    validateOctalLiteral ctx literal

  JSBigIntLiteral _annot literal ->
    validateBigIntLiteral literal

  JSStringLiteral _annot literal ->
    validateStringLiteral literal

  JSRegEx _annot regex ->
    validateRegexLiteral regex

  -- Complex expressions requiring recursive validation
  JSArrayLiteral _lbracket elements _rbracket ->
    concatMap (validateArrayElement ctx) elements ++
    validateArrayLiteral elements

  JSAssignExpression lhs _op rhs ->
    validateExpression ctx lhs ++
    validateExpression ctx rhs ++
    validateAssignmentTarget lhs

  JSAwaitExpression _await expr ->
    validateAwaitExpression ctx expr

  JSCallExpression callee _lparen args _rparen ->
    validateExpression ctx callee ++
    concatMap (validateExpression ctx) (fromCommaList args) ++
    validateCallExpression ctx callee args

  JSCallExpressionDot obj _dot prop ->
    validateExpression ctx obj ++
    validateExpression ctx prop

  JSCallExpressionSquare obj _lbracket prop _rbracket ->
    validateExpression ctx obj ++
    validateExpression ctx prop

  JSClassExpression _class name heritage _lbrace elements _rbrace ->
    let classCtx = ctx { contextInClass = True, contextSuperContext = hasHeritage heritage }
    in validateClassHeritage ctx heritage ++
       concatMap (validateClassElement classCtx) elements ++
       validateClassElements elements

  JSCommaExpression left _comma right ->
    validateExpression ctx left ++
    validateExpression ctx right

  JSExpressionBinary left _op right ->
    validateExpression ctx left ++
    validateExpression ctx right

  JSExpressionParen _lparen expr _rparen ->
    validateExpression ctx expr

  JSExpressionPostfix expr _op ->
    validateExpression ctx expr ++
    validateAssignmentTarget expr

  JSExpressionTernary test _question consequent _colon alternate ->
    validateExpression ctx test ++
    validateExpression ctx consequent ++
    validateExpression ctx alternate

  JSArrowExpression params _arrow body ->
    let funcCtx = ctx { contextInFunction = True }
    in validateArrowParameters ctx params ++
       validateConciseBody funcCtx body

  JSFunctionExpression _function name _lparen params _rparen block ->
    let funcCtx = ctx { contextInFunction = True }
    in validateOptionalFunctionName ctx name ++
       validateFunctionParameters ctx (fromCommaList params) ++
       validateBlock funcCtx block

  JSGeneratorExpression _function _star name _lparen params _rparen block ->
    let genCtx = ctx { contextInFunction = True, contextInGenerator = True }
    in validateOptionalFunctionName ctx name ++
       validateFunctionParameters ctx (fromCommaList params) ++
       validateBlock genCtx block

  JSAsyncFunctionExpression _async _function name _lparen params _rparen block ->
    let asyncCtx = ctx { contextInFunction = True, contextInAsync = True }
    in validateOptionalFunctionName ctx name ++
       validateFunctionParameters ctx (fromCommaList params) ++
       validateBlock asyncCtx block

  JSMemberDot obj _dot prop ->
    validateExpression ctx obj ++
    validateExpression ctx prop ++
    validateMemberExpression ctx obj prop

  JSMemberExpression obj _lparen args _rparen ->
    validateExpression ctx obj ++
    concatMap (validateExpression ctx) (fromCommaList args)

  JSMemberNew _new constructor _lparen args _rparen ->
    validateExpression ctx constructor ++
    concatMap (validateExpression ctx) (fromCommaList args)

  JSMemberSquare obj _lbracket prop _rbracket ->
    validateExpression ctx obj ++
    validateExpression ctx prop

  JSNewExpression _new constructor ->
    validateExpression ctx constructor

  JSOptionalMemberDot obj _optDot prop ->
    validateExpression ctx obj ++
    validateExpression ctx prop

  JSOptionalMemberSquare obj _optLbracket prop _rbracket ->
    validateExpression ctx obj ++
    validateExpression ctx prop

  JSOptionalCallExpression callee _optLparen args _rparen ->
    validateExpression ctx callee ++
    concatMap (validateExpression ctx) (fromCommaList args)

  JSObjectLiteral _lbrace props _rbrace ->
    validateObjectLiteral ctx props

  JSSpreadExpression _spread expr ->
    validateExpression ctx expr

  JSTemplateLiteral maybeTag _backtick _head parts ->
    maybe [] (validateExpression ctx) maybeTag ++
    concatMap (validateTemplatePart ctx) parts ++
    validateTemplateLiteral maybeTag parts

  JSUnaryExpression op expr ->
    validateExpression ctx expr ++
    validateUnaryExpression ctx op expr

  JSVarInitExpression expr init ->
    validateExpression ctx expr ++
    validateVarInitializer ctx init

  JSYieldExpression _yield maybeExpr ->
    validateYieldExpression ctx maybeExpr

  JSYieldFromExpression _yield _from expr ->
    validateYieldExpression ctx (Just expr)
  JSImportMeta import_annot _dot ->
    [ ImportMetaOutsideModule (extractAnnotationPos import_annot) | not (contextInModule ctx) ]

-- | Validate module items with import/export semantics.
validateModuleItem :: ValidationContext -> JSModuleItem -> [ValidationError]
validateModuleItem ctx item = case item of
  JSModuleImportDeclaration _annot importDecl ->
    if contextInModule ctx
      then validateImportDeclaration ctx importDecl
      else [ImportOutsideModule (extractTokenPosn item)]

  JSModuleExportDeclaration _annot exportDecl ->
    if contextInModule ctx
      then validateExportDeclaration ctx exportDecl
      else [ExportOutsideModule (extractTokenPosn item)]

  JSModuleStatementListItem stmt ->
    validateStatement ctx stmt

-- | Comprehensive assignment target validation.
validateAssignmentTarget :: JSExpression -> [ValidationError]
validateAssignmentTarget expr = case expr of
  JSIdentifier _annot _name -> []
  JSMemberDot _obj _dot _prop -> []
  JSMemberSquare _obj _lbracket _prop _rbracket -> []
  JSOptionalMemberDot _obj _optDot _prop -> []
  JSOptionalMemberSquare _obj _optLbracket _prop _rbracket -> []
  JSArrayLiteral _lbracket _elements _rbracket -> validateDestructuringArray expr
  JSObjectLiteral _lbrace _props _rbrace -> validateDestructuringObject expr
  JSExpressionParen _lparen inner _rparen -> validateAssignmentTarget inner
  _ -> [InvalidAssignmentTarget expr (extractExpressionPos expr)]

-- Helper functions for comprehensive validation

-- | Extract comma-separated list items.
fromCommaList :: JSCommaList a -> [a]
fromCommaList JSLNil = []
fromCommaList (JSLOne x) = [x]
fromCommaList (JSLCons list _comma x) = fromCommaList list ++ [x]

-- | Convert JSCommaTrailingList to regular list.
fromCommaTrailingList :: JSCommaTrailingList a -> [a]
fromCommaTrailingList (JSCTLComma list _comma) = fromCommaList list
fromCommaTrailingList (JSCTLNone list) = fromCommaList list

-- | Check if Maybe value is Just (avoiding Data.Maybe.isJust import issue).

-- | Check if class has heritage (extends clause).
hasHeritage :: JSClassHeritage -> Bool
hasHeritage JSExtendsNone = False
hasHeritage (JSExtends _ _) = True

-- | Validate break statement context.
validateBreakStatement :: ValidationContext -> JSIdent -> [ValidationError]
validateBreakStatement ctx ident = case ident of
  JSIdentNone -> 
    if contextInLoop ctx || contextInSwitch ctx
      then []
      else [BreakOutsideLoop (extractIdentPos ident)]
  JSIdentName _annot label ->
    if Text.pack label `elem` contextLabels ctx
      then []
      else [LabelNotFound (Text.pack label) (extractIdentPos ident)]

-- | Validate continue statement context.
validateContinueStatement :: ValidationContext -> JSIdent -> [ValidationError]
validateContinueStatement ctx ident = case ident of
  JSIdentNone ->
    if contextInLoop ctx
      then []
      else [ContinueOutsideLoop (extractIdentPos ident)]
  JSIdentName _annot label ->
    if Text.pack label `elem` contextLabels ctx
      then []
      else [LabelNotFound (Text.pack label) (extractIdentPos ident)]

-- | Validate return statement context.
validateReturnStatement :: ValidationContext -> Maybe JSExpression -> [ValidationError]
validateReturnStatement ctx maybeExpr =
  if contextInFunction ctx
    then maybe [] (validateExpression ctx) maybeExpr
    else [ReturnOutsideFunction (TokenPn 0 0 0)]

-- | Validate yield expression context.
validateYieldExpression :: ValidationContext -> Maybe JSExpression -> [ValidationError]
validateYieldExpression ctx maybeExpr =
  if contextInGenerator ctx
    then maybe [] (validateExpression ctx) maybeExpr
    else [YieldOutsideGenerator (TokenPn 0 0 0)]

-- | Validate await expression context.
validateAwaitExpression :: ValidationContext -> JSExpression -> [ValidationError]
validateAwaitExpression ctx expr =
  if contextInAsync ctx
    then validateExpression ctx expr
    else [AwaitOutsideAsync (TokenPn 0 0 0)]

-- | Validate const declarations have initializers.
validateConstDeclarations :: ValidationContext -> [JSExpression] -> [ValidationError]
validateConstDeclarations _ctx exprs = concatMap checkConstInit exprs
  where
    checkConstInit (JSVarInitExpression (JSIdentifier _annot name) JSVarInitNone) =
      [ConstWithoutInitializer (Text.pack name) (TokenPn 0 0 0)]
    checkConstInit _ = []

-- | Validate let declarations.
validateLetDeclarations :: ValidationContext -> [JSExpression] -> [ValidationError] 
validateLetDeclarations ctx exprs = validateBindingNames ctx (extractBindingNames exprs)

-- | Validate function parameters for duplicates and strict mode violations.
validateFunctionParameters :: ValidationContext -> [JSExpression] -> [ValidationError]
validateFunctionParameters ctx params = 
  let paramNames = extractParameterNames params
      duplicates = findDuplicates paramNames
      duplicateErrors = map (\name -> DuplicateParameter name (TokenPn 0 0 0)) duplicates
      strictModeErrors = concatMap (validateIdentifier ctx . Text.unpack) paramNames
      defaultValueErrors = concatMap (validateParameterDefault ctx) params
      restParamErrors = validateRestParameters params
  in duplicateErrors ++ strictModeErrors ++ defaultValueErrors ++ restParamErrors

-- | Validate rest parameters constraints.
validateRestParameters :: [JSExpression] -> [ValidationError]
validateRestParameters params = 
  let restParams = [(i, param) | (i, param) <- zip [0..] params, isRestParameter param]
      multipleRestErrors = if length restParams > 1
                           then [RestElementNotLast (TokenPn 0 0 0)] -- Multiple rest parameters
                           else []
      notLastErrors = [RestElementNotLast (extractExpressionPos param) 
                      | (i, param) <- restParams, i /= length params - 1]
  in multipleRestErrors ++ notLastErrors
  where
    isRestParameter :: JSExpression -> Bool
    isRestParameter (JSSpreadExpression _ _) = True
    isRestParameter _ = False

-- | Validate parameter default values for forbidden yield/await expressions.
validateParameterDefault :: ValidationContext -> JSExpression -> [ValidationError]
validateParameterDefault ctx param = case param of
  JSVarInitExpression _ident (JSVarInit _eq defaultExpr) ->
    validateExpressionInParameterDefault ctx defaultExpr
  _ -> []

-- | Validate expression in parameter default context (forbids yield/await).
validateExpressionInParameterDefault :: ValidationContext -> JSExpression -> [ValidationError]
validateExpressionInParameterDefault ctx expr = case expr of
  JSYieldExpression _yield _ ->
    [YieldInParameterDefault (extractExpressionPosition expr)]
  JSAwaitExpression _await _ ->
    [AwaitInParameterDefault (extractExpressionPosition expr)]
  -- Recursively check nested expressions
  JSExpressionBinary left _op right ->
    validateExpressionInParameterDefault ctx left ++
    validateExpressionInParameterDefault ctx right
  JSExpressionTernary cond _q consequent _c alternate ->
    validateExpressionInParameterDefault ctx cond ++
    validateExpressionInParameterDefault ctx consequent ++
    validateExpressionInParameterDefault ctx alternate
  JSCallExpression func _lp args _rp ->
    validateExpressionInParameterDefault ctx func ++
    concatMap (validateExpressionInParameterDefault ctx) (fromCommaList args)
  JSMemberDot obj _dot _prop ->
    validateExpressionInParameterDefault ctx obj
  JSMemberSquare obj _lb index _rb ->
    validateExpressionInParameterDefault ctx obj ++
    validateExpressionInParameterDefault ctx index
  JSExpressionParen _lp innerExpr _rp ->
    validateExpressionInParameterDefault ctx innerExpr
  JSUnaryExpression _op operand ->
    validateExpressionInParameterDefault ctx operand
  JSExpressionPostfix target _op ->
    validateExpressionInParameterDefault ctx target
  -- Base cases - literals, identifiers, etc. are fine
  _ -> []

-- | Extract position from expression for error reporting.
extractExpressionPosition :: JSExpression -> TokenPosn
extractExpressionPosition expr = case expr of
  JSYieldExpression (JSAnnot pos _) _ -> pos
  JSAwaitExpression (JSAnnot pos _) _ -> pos
  JSIdentifier (JSAnnot pos _) _ -> pos
  JSDecimal (JSAnnot pos _) _ -> pos
  JSStringLiteral (JSAnnot pos _) _ -> pos
  _ -> TokenPn 0 0 0  -- Default position if we can't extract

-- | Extract parameter names from function parameters.
extractParameterNames :: [JSExpression] -> [Text]
extractParameterNames = concatMap extractParamName
  where
    extractParamName (JSIdentifier _annot name) = [Text.pack name]
    extractParamName (JSSpreadExpression _spread (JSIdentifier _annot name)) = [Text.pack name]
    extractParamName (JSVarInitExpression (JSIdentifier _annot name) _init) = [Text.pack name]
    extractParamName _ = [] -- Handle destructuring patterns, defaults, etc.

-- | Extract binding names from variable declarations.
extractBindingNames :: [JSExpression] -> [Text]
extractBindingNames = concatMap extractBindingName
  where
    extractBindingName (JSVarInitExpression (JSIdentifier _annot name) _) = [Text.pack name]
    extractBindingName (JSIdentifier _annot name) = [Text.pack name]
    extractBindingName _ = []

-- | Find duplicate names in a list.
findDuplicates :: (Eq a, Ord a) => [a] -> [a]
findDuplicates xs = [x | (x:_:_) <- group (sort xs)]

-- | Validate binding names for duplicates.
validateBindingNames :: ValidationContext -> [Text] -> [ValidationError]
validateBindingNames _ctx names =
  let duplicates = findDuplicates names
  in map (\name -> DuplicateBinding name (TokenPn 0 0 0)) duplicates

-- | Validate unary expressions for strict mode violations.
validateUnaryExpression :: ValidationContext -> JSUnaryOp -> JSExpression -> [ValidationError]
validateUnaryExpression ctx (JSUnaryOpDelete annot) expr
  | contextStrictMode ctx == StrictModeOn = case expr of
      JSIdentifier _ _ -> [DeleteOfUnqualifiedInStrict (extractAnnotationPos annot)]
      _ -> []
validateUnaryExpression _ctx _op _expr = []

-- | Validate identifier in strict mode context.
validateIdentifier :: ValidationContext -> String -> [ValidationError]
validateIdentifier ctx name
  | contextStrictMode ctx == StrictModeOn && name `elem` strictModeReserved =
      [ReservedWordAsIdentifier (Text.pack name) (TokenPn 0 0 0)]
  | name `elem` futureReserved =
      [FutureReservedWord (Text.pack name) (TokenPn 0 0 0)]
  | name == "super" = validateSuperUsage ctx
  | otherwise = []

-- | Validate super keyword usage context.
validateSuperUsage :: ValidationContext -> [ValidationError]
validateSuperUsage ctx
  | not (contextInClass ctx) = [SuperOutsideClass (TokenPn 0 0 0)]
  | not (contextInMethod ctx) && not (contextInConstructor ctx) = [SuperPropertyOutsideMethod (TokenPn 0 0 0)]
  | otherwise = []

-- | Strict mode reserved words.
strictModeReserved :: [String]
strictModeReserved = ["arguments", "eval"]

-- | Future reserved words.
futureReserved :: [String] 
futureReserved = ["await", "enum", "implements", "interface", "package", "private", "protected", "public"]

-- | Validate numeric literals.
validateNumericLiteral :: String -> [ValidationError]
validateNumericLiteral literal
  | all isValidNumChar literal = []
  | otherwise = [InvalidNumericLiteral (Text.pack literal) (TokenPn 0 0 0)]
  where
    isValidNumChar c = isDigit c || c `elem` (".-+eE" :: String)

-- | Validate hex literals.
validateHexLiteral :: String -> [ValidationError]
validateHexLiteral literal
  | "0x" `Text.isPrefixOf` Text.pack literal || "0X" `Text.isPrefixOf` Text.pack literal = []
  | otherwise = [InvalidNumericLiteral (Text.pack literal) (TokenPn 0 0 0)]

-- | Validate binary literals (ES2015).
validateBinaryLiteral :: String -> [ValidationError]
validateBinaryLiteral literal
  | "0b" `Text.isPrefixOf` Text.pack literal || "0B" `Text.isPrefixOf` Text.pack literal = 
      let digits = Text.drop 2 (Text.pack literal)
      in if Text.all (\c -> c == '0' || c == '1') digits
         then []
         else [InvalidNumericLiteral (Text.pack literal) (TokenPn 0 0 0)]
  | otherwise = [InvalidNumericLiteral (Text.pack literal) (TokenPn 0 0 0)]

-- | Validate octal literals in strict mode.
validateOctalLiteral :: ValidationContext -> String -> [ValidationError] 
validateOctalLiteral ctx literal
  | contextStrictMode ctx == StrictModeOn = [InvalidOctalInStrict (Text.pack literal) (TokenPn 0 0 0)]
  | otherwise = []

-- | Validate BigInt literals.
validateBigIntLiteral :: String -> [ValidationError]
validateBigIntLiteral literal
  | "n" `Text.isSuffixOf` Text.pack literal = []
  | otherwise = [InvalidBigIntLiteral (Text.pack literal) (TokenPn 0 0 0)]

-- | Validate string literals.
validateStringLiteral :: String -> [ValidationError]
validateStringLiteral literal = validateStringEscapes literal

-- | Validate escape sequences in string literals.
validateStringEscapes :: String -> [ValidationError]
validateStringEscapes = go
  where
    go [] = []
    go ('\\':rest) = validateEscapeSequence rest
    go (_:rest) = go rest
    
    validateEscapeSequence :: String -> [ValidationError]
    validateEscapeSequence [] = [InvalidEscapeSequence (Text.pack "\\") (TokenPn 0 0 0)]
    validateEscapeSequence (c:rest) = case c of
      '"' -> go rest   -- \"
      '\'' -> go rest  -- \'
      '\\' -> go rest  -- \\
      '/' -> go rest   -- \/
      'b' -> go rest   -- \b (backspace)
      'f' -> go rest   -- \f (form feed)
      'n' -> go rest   -- \n (newline)
      'r' -> go rest   -- \r (carriage return)
      't' -> go rest   -- \t (tab)
      'v' -> go rest   -- \v (vertical tab)
      '0' -> validateNullEscape rest
      'u' -> validateUnicodeEscape rest
      'x' -> validateHexEscape rest
      _ -> if isOctalDigit c
           then validateOctalEscape (c:rest)
           else [InvalidEscapeSequence (Text.pack ['\\', c]) (TokenPn 0 0 0)] ++ go rest
    
    validateNullEscape :: String -> [ValidationError]
    validateNullEscape rest = case rest of
      (d:_) | isDigit d -> [InvalidEscapeSequence (Text.pack "\\0") (TokenPn 0 0 0)] ++ go rest
      _ -> go rest
    
    validateUnicodeEscape :: String -> [ValidationError]
    validateUnicodeEscape rest = case rest of
      ('{':hexRest) -> validateUnicodeCodePoint hexRest
      _ -> case take 4 rest of
        [a,b,c,d] | all isHexDigit [a,b,c,d] -> go (drop 4 rest)
        _ -> [InvalidEscapeSequence (Text.pack "\\u") (TokenPn 0 0 0)] ++ go rest
    
    validateUnicodeCodePoint :: String -> [ValidationError]
    validateUnicodeCodePoint rest = case break (== '}') rest of
      (hexDigits, '}':remaining) 
        | length hexDigits >= 1 && length hexDigits <= 6 && all isHexDigit hexDigits ->
            let codePoint = read ("0x" ++ hexDigits) :: Int
            in if codePoint <= 0x10FFFF
               then go remaining
               else [InvalidEscapeSequence (Text.pack ("\\u{" ++ hexDigits ++ "}")) (TokenPn 0 0 0)] ++ go remaining
        | otherwise -> [InvalidEscapeSequence (Text.pack ("\\u{" ++ hexDigits ++ "}")) (TokenPn 0 0 0)] ++ go remaining
      _ -> [InvalidEscapeSequence (Text.pack "\\u{") (TokenPn 0 0 0)] ++ go rest
    
    validateHexEscape :: String -> [ValidationError]
    validateHexEscape rest = case take 2 rest of
      [a,b] | all isHexDigit [a,b] -> go (drop 2 rest)
      _ -> [InvalidEscapeSequence (Text.pack "\\x") (TokenPn 0 0 0)] ++ go rest
    
    validateOctalEscape :: String -> [ValidationError]
    validateOctalEscape rest = 
      let octalChars = takeWhile isOctalDigit rest
          remaining = drop (length octalChars) rest
      in if length octalChars <= 3
         then go remaining
         else [InvalidEscapeSequence (Text.pack ("\\" ++ octalChars)) (TokenPn 0 0 0)] ++ go remaining
    
    isHexDigit :: Char -> Bool
    isHexDigit c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    
    isOctalDigit :: Char -> Bool
    isOctalDigit c = c >= '0' && c <= '7'

-- | Validate regex literals.
validateRegexLiteral :: String -> [ValidationError]
validateRegexLiteral regex = 
  case parseRegexLiteral regex of
    Left err -> [err]
    Right (pattern, flags) -> validateRegexPattern pattern ++ validateRegexFlags flags

-- | Parse regex literal into pattern and flags.
parseRegexLiteral :: String -> Either ValidationError (String, String)
parseRegexLiteral regex = case regex of
  ('/':rest) -> parseRegexParts rest
  _ -> Left (InvalidRegexPattern (Text.pack regex) (TokenPn 0 0 0))
  where
    parseRegexParts :: String -> Either ValidationError (String, String)
    parseRegexParts = go ""
      where
        go acc [] = Left (InvalidRegexPattern (Text.pack regex) (TokenPn 0 0 0))
        go acc ('\\':c:rest) = go (acc ++ ['\\', c]) rest
        go acc ('/':flags) = Right (acc, flags)
        go acc (c:rest) = go (acc ++ [c]) rest

-- | Validate regex pattern.
validateRegexPattern :: String -> [ValidationError]
validateRegexPattern = validateRegexSyntax
  where
    validateRegexSyntax :: String -> [ValidationError]
    validateRegexSyntax = go 0 []
      where
        go :: Int -> [Char] -> String -> [ValidationError]
        go _ _ [] = []
        go depth stack ('\\':c:rest) = go depth stack rest -- Skip escaped chars
        go depth stack ('[':rest) = go depth ('[':stack) rest
        go depth (s:stack') (']':rest) | s == '[' = go depth stack' rest
        go depth stack ('(':rest) = go (depth + 1) ('(':stack) rest
        go depth (s:stack') (')':rest) | s == '(' && depth > 0 = go (depth - 1) stack' rest
        go depth stack (')':rest) | depth == 0 = 
          [InvalidRegexPattern (Text.pack "Unmatched closing parenthesis") (TokenPn 0 0 0)] ++ go depth stack rest
        go depth stack ('|':rest) = go depth stack rest
        go depth stack ('*':rest) = go depth stack rest
        go depth stack ('+':rest) = go depth stack rest
        go depth stack ('?':rest) = go depth stack rest
        go depth stack ('^':rest) = go depth stack rest
        go depth stack ('$':rest) = go depth stack rest
        go depth stack ('.':rest) = go depth stack rest
        go depth stack ('{':rest) = validateQuantifier go depth stack rest
        go depth stack (_:rest) = go depth stack rest
        
        validateQuantifier :: (Int -> [Char] -> String -> [ValidationError]) -> Int -> [Char] -> String -> [ValidationError]
        validateQuantifier goFn depth stack rest = 
          let (quantifier, remaining) = span (\c -> c /= '}') rest
          in case remaining of
            ('}':rest') -> 
              if isValidQuantifier quantifier
                then goFn depth stack rest'
                else [InvalidRegexPattern (Text.pack ("Invalid quantifier: {" ++ quantifier ++ "}")) (TokenPn 0 0 0)] ++ goFn depth stack rest'
            _ -> [InvalidRegexPattern (Text.pack "Unterminated quantifier") (TokenPn 0 0 0)] ++ goFn depth stack rest
        
        isValidQuantifier :: String -> Bool
        isValidQuantifier [] = False
        isValidQuantifier s = case span isDigit s of
          (n1, "") -> not (null n1)
          (n1, ",") -> not (null n1)
          (n1, ',':n2) -> not (null n1) && (null n2 || all isDigit n2)
          _ -> False

-- | Validate regex flags.
validateRegexFlags :: String -> [ValidationError]
validateRegexFlags flags = 
  let validFlags = "gimsuyx" :: String
      invalidFlags = filter (`notElem` validFlags) flags
      duplicateFlags = findDuplicateFlags flags
  in map (\f -> InvalidRegexFlags (Text.pack [f]) (TokenPn 0 0 0)) invalidFlags ++
     map (\f -> InvalidRegexFlags (Text.pack ("Duplicate flag: " ++ [f])) (TokenPn 0 0 0)) duplicateFlags

-- | Find duplicate flags in regex.
findDuplicateFlags :: String -> [Char]
findDuplicateFlags flags = 
  let flagCounts = [(c, length (filter (== c) flags)) | c <- nub flags]
  in [c | (c, count) <- flagCounts, count > 1]

-- | Validate general literals.
validateLiteral :: ValidationContext -> String -> [ValidationError]
validateLiteral ctx literal = 
  -- Detect literal type and validate accordingly
  if "n" `isSuffixOf` literal
  then validateBigIntLiteral literal
  else if isNumericLiteral literal
  then validateNumericLiteral literal
  else validateStringLiteral literal
  where
    isNumericLiteral :: String -> Bool
    isNumericLiteral s = case s of
      [] -> False
      (c:_) -> isDigit c || c == '.'

-- Validation functions remain focused on semantic validation of parsed ASTs

validateBlock :: ValidationContext -> JSBlock -> [ValidationError]
validateBlock ctx (JSBlock _lbrace stmts _rbrace) = concatMap (validateStatement ctx) stmts

validateArrayElement :: ValidationContext -> JSArrayElement -> [ValidationError]
validateArrayElement ctx element = case element of
  JSArrayElement expr -> validateExpression ctx expr
  JSArrayComma _comma -> []

validateArrayLiteral :: [JSArrayElement] -> [ValidationError]
validateArrayLiteral elements = concatMap validateArrayElement' elements
  where
    validateArrayElement' :: JSArrayElement -> [ValidationError]
    validateArrayElement' element = case element of
      JSArrayElement expr -> [] -- Expression validation handled elsewhere
      JSArrayComma _ -> [] -- Comma elements are valid (sparse arrays)

validateCallExpression :: ValidationContext -> JSExpression -> JSCommaList JSExpression -> [ValidationError]
validateCallExpression ctx callee args = 
  let argList = fromCommaList args
  in validateExpression ctx callee ++ concatMap (validateExpression ctx) argList

validateMemberExpression :: ValidationContext -> JSExpression -> JSExpression -> [ValidationError]
validateMemberExpression ctx obj prop = 
  validateExpression ctx obj ++ validateExpression ctx prop ++ 
  validatePrivateFieldAccess ctx prop ++ validateNewTargetAccess ctx obj prop
  where
    validatePrivateFieldAccess :: ValidationContext -> JSExpression -> [ValidationError]
    validatePrivateFieldAccess context propExpr = case propExpr of
      JSIdentifier _annot name | isPrivateIdentifier name ->
        if contextInClass context
        then []
        else [PrivateFieldOutsideClass (Text.pack name) (extractExpressionPos propExpr)]
      _ -> []
    
    validateNewTargetAccess :: ValidationContext -> JSExpression -> JSExpression -> [ValidationError]
    validateNewTargetAccess context objExpr propExpr = 
      case (objExpr, propExpr) of
        (JSIdentifier _ "new", JSIdentifier _ "target") ->
          if contextInFunction context
          then []
          else [NewTargetOutsideFunction (extractExpressionPos propExpr)]
        _ -> []
    
    isPrivateIdentifier :: String -> Bool
    isPrivateIdentifier ('#':_) = True
    isPrivateIdentifier _ = False

validateObjectLiteral :: ValidationContext -> JSCommaTrailingList JSObjectProperty -> [ValidationError]
validateObjectLiteral ctx props = 
  let propList = fromCommaTrailingList props
      propNames = map extractPropertyName propList
      duplicates = findDuplicates propNames
      propErrors = concatMap (validateObjectProperty ctx) propList
      duplicateErrors = if contextStrictMode ctx == StrictModeOn
                          then map (\name -> DuplicatePropertyInStrict name (TokenPn 0 0 0)) duplicates
                          else []
  in propErrors ++ duplicateErrors
  where
    extractPropertyName :: JSObjectProperty -> Text
    extractPropertyName prop = case prop of
      JSPropertyNameandValue propName _ _ -> getPropertyNameText propName
      JSPropertyIdentRef _ ident -> getIdentText ident
      JSObjectMethod method -> getMethodNameText method
      JSObjectSpread _ _ -> Text.empty -- Spread properties don't have names
    
    getPropertyNameText :: JSPropertyName -> Text
    getPropertyNameText propName = case propName of
      JSPropertyIdent _ name -> Text.pack name
      JSPropertyString _ str -> Text.pack str
      JSPropertyNumber _ num -> Text.pack num
      JSPropertyComputed _ _ _ -> Text.pack "[computed]"
    
    getIdentText :: String -> Text
    getIdentText = Text.pack
    
    getMethodNameText :: JSMethodDefinition -> Text
    getMethodNameText method = case method of
      JSMethodDefinition propName _ _ _ _ -> getPropertyNameText propName
      JSGeneratorMethodDefinition _ propName _ _ _ _ -> getPropertyNameText propName
      JSPropertyAccessor _ propName _ _ _ _ -> getPropertyNameText propName

-- | Validate individual object property.
validateObjectProperty :: ValidationContext -> JSObjectProperty -> [ValidationError]
validateObjectProperty ctx prop = case prop of
  JSPropertyNameandValue propName _ values -> 
    validatePropertyName ctx propName ++ concatMap (validateExpression ctx) values
  JSPropertyIdentRef _ _ident -> []
  JSObjectMethod method -> validateMethodDefinition ctx method
  JSObjectSpread _ expr -> validateExpression ctx expr

-- | Validate property name.
validatePropertyName :: ValidationContext -> JSPropertyName -> [ValidationError]
validatePropertyName ctx propName = case propName of
  JSPropertyIdent _annot name -> validateIdentifier ctx name
  JSPropertyString _annot _str -> []
  JSPropertyNumber _annot _num -> []
  JSPropertyComputed _lbracket expr _rbracket -> validateExpression ctx expr

validateTemplatePart :: ValidationContext -> JSTemplatePart -> [ValidationError]
validateTemplatePart ctx (JSTemplatePart expr _rbrace _suffix) = validateExpression ctx expr

validateTemplateLiteral :: Maybe JSExpression -> [JSTemplatePart] -> [ValidationError]
validateTemplateLiteral maybeTag parts = 
  let tagErrors = case maybeTag of
        Nothing -> []
        Just tag -> [] -- Tag validation handled elsewhere
      partErrors = concatMap validateTemplatePart' parts
  in tagErrors ++ partErrors
  where
    validateTemplatePart' :: JSTemplatePart -> [ValidationError]
    validateTemplatePart' (JSTemplatePart _expr _ _) = [] -- Template parts are validated during expression validation

validateVarInitializer :: ValidationContext -> JSVarInitializer -> [ValidationError]
validateVarInitializer ctx init = case init of
  JSVarInit _eq expr -> validateExpression ctx expr
  JSVarInitNone -> []

validateArrowParameters :: ValidationContext -> JSArrowParameterList -> [ValidationError]
validateArrowParameters ctx params = case params of
  JSUnparenthesizedArrowParameter (JSIdentName _annot name) -> 
    validateIdentifier ctx name
  JSUnparenthesizedArrowParameter JSIdentNone -> []
  JSParenthesizedArrowParameterList _lparen exprs _rparen ->
    concatMap (validateExpression ctx) (fromCommaList exprs)

validateConciseBody :: ValidationContext -> JSConciseBody -> [ValidationError]
validateConciseBody ctx body = case body of
  JSConciseFunctionBody block -> validateBlock ctx block
  JSConciseExpressionBody expr -> validateExpression ctx expr

validateFunctionName :: ValidationContext -> JSIdent -> [ValidationError]
validateFunctionName ctx name = case name of
  JSIdentNone -> [FunctionNameRequired (TokenPn 0 0 0)]
  JSIdentName _annot nameStr -> validateIdentifier ctx nameStr

validateOptionalFunctionName :: ValidationContext -> JSIdent -> [ValidationError]
validateOptionalFunctionName ctx name = case name of
  JSIdentNone -> []
  JSIdentName _annot nameStr -> validateIdentifier ctx nameStr

validateClassHeritage :: ValidationContext -> JSClassHeritage -> [ValidationError]
validateClassHeritage ctx heritage = case heritage of
  JSExtends _extends expr -> validateExpression ctx expr
  JSExtendsNone -> []

validateClassElement :: ValidationContext -> JSClassElement -> [ValidationError]
validateClassElement ctx element = case element of
  JSClassInstanceMethod method -> validateMethodDefinition ctx method
  JSClassStaticMethod _static method -> validateMethodDefinition ctx method
  JSClassSemi _semi -> []
  JSPrivateField _annot _name _eq init _semi -> 
    maybe [] (validateExpression ctx) init
  JSPrivateMethod _annot _name _lp _params _rp _block -> [] -- Private method validation handled elsewhere  
  JSPrivateAccessor _accessor _annot _name _lp _params _rp _block -> [] -- Private accessor validation handled elsewhere

validateClassElements :: [JSClassElement] -> [ValidationError]
validateClassElements elements = 
  let methodNames = extractMethodNames elements
      duplicateNames = findDuplicates methodNames
      constructorCount = countConstructors elements
      staticConstructors = findStaticConstructors elements
      constructorErrors = if constructorCount > 1 
                         then [MultipleConstructors (TokenPn 0 0 0)]
                         else []
      staticErrors = map (\_ -> StaticConstructor (TokenPn 0 0 0)) staticConstructors
      duplicateErrors = map (\name -> DuplicateMethodName name (TokenPn 0 0 0)) duplicateNames
  in constructorErrors ++ staticErrors ++ duplicateErrors
  where
    extractMethodNames :: [JSClassElement] -> [Text]
    extractMethodNames = concatMap extractMethodName
      where
        extractMethodName :: JSClassElement -> [Text]
        extractMethodName element = case element of
          JSClassInstanceMethod method -> [getMethodName method]
          JSClassStaticMethod _ method -> [getMethodName method]
          JSClassSemi _ -> []
          JSPrivateField _ name _ _ _ -> [Text.pack ("#" <> name)]
          JSPrivateMethod _ name _ _ _ _ -> [Text.pack ("#" <> name)]
          JSPrivateAccessor _ _ name _ _ _ _ -> [Text.pack ("#" <> name)]
        
        getMethodName :: JSMethodDefinition -> Text
        getMethodName method = case method of
          JSMethodDefinition propName _ _ _ _ -> getPropertyNameFromMethod propName
          JSGeneratorMethodDefinition _ propName _ _ _ _ -> getPropertyNameFromMethod propName
          JSPropertyAccessor _ propName _ _ _ _ -> getPropertyNameFromMethod propName
        
        getPropertyNameFromMethod :: JSPropertyName -> Text
        getPropertyNameFromMethod propName = case propName of
          JSPropertyIdent _ name -> Text.pack name
          JSPropertyString _ str -> Text.pack str
          JSPropertyNumber _ num -> Text.pack num
          JSPropertyComputed _ _ _ -> Text.pack "[computed]"
    
    countConstructors :: [JSClassElement] -> Int
    countConstructors = length . filter isConstructor
      where
        isConstructor :: JSClassElement -> Bool
        isConstructor element = case element of
          JSClassInstanceMethod method -> isConstructorMethod method
          _ -> False
        
        isConstructorMethod :: JSMethodDefinition -> Bool
        isConstructorMethod method = case method of
          JSMethodDefinition propName _ _ _ _ -> isConstructorName propName
          _ -> False
        
        isConstructorName :: JSPropertyName -> Bool
        isConstructorName propName = case propName of
          JSPropertyIdent _ "constructor" -> True
          _ -> False
    
    findStaticConstructors :: [JSClassElement] -> [JSClassElement]
    findStaticConstructors = filter isStaticConstructor
      where
        isStaticConstructor :: JSClassElement -> Bool
        isStaticConstructor element = case element of
          JSClassStaticMethod _ method -> isConstructorMethod method
          _ -> False
          where
            isConstructorMethod :: JSMethodDefinition -> Bool
            isConstructorMethod method = case method of
              JSMethodDefinition propName _ _ _ _ -> isConstructorName propName
              _ -> False
            
            isConstructorName :: JSPropertyName -> Bool
            isConstructorName propName = case propName of
              JSPropertyIdent _ "constructor" -> True
              _ -> False

validateMethodDefinition :: ValidationContext -> JSMethodDefinition -> [ValidationError]
validateMethodDefinition ctx method = case method of
  JSMethodDefinition propName _lparen params _rparen body ->
    let isConstructor = isConstructorProperty propName
        methodCtx = ctx { 
          contextInFunction = True, 
          contextInMethod = True,
          contextInConstructor = isConstructor 
        }
    in validatePropertyName ctx propName ++
       validateFunctionParameters ctx (fromCommaList params) ++
       validateBlock methodCtx body ++
       validateMethodConstraints method
  
  JSGeneratorMethodDefinition _star propName _lparen params _rparen body ->
    let isConstructor = isConstructorProperty propName
        genCtx = ctx { 
          contextInFunction = True, 
          contextInGenerator = True,
          contextInMethod = True,
          contextInConstructor = isConstructor
        }
    in validatePropertyName ctx propName ++
       validateFunctionParameters ctx (fromCommaList params) ++
       validateBlock genCtx body ++
       validateGeneratorMethodConstraints method
  
  JSPropertyAccessor accessor propName _lparen params _rparen body ->
    let accessorCtx = ctx { contextInFunction = True }
    in validatePropertyName ctx propName ++
       validateAccessorParameters accessor params ++
       validateBlock accessorCtx body
  where
    validateMethodConstraints :: JSMethodDefinition -> [ValidationError]
    validateMethodConstraints _ = [] -- No additional method constraints for basic methods
    
    validateGeneratorMethodConstraints :: JSMethodDefinition -> [ValidationError]
    validateGeneratorMethodConstraints method = case method of
      JSGeneratorMethodDefinition _ propName _ _ _ _ ->
        if isConstructorProperty propName
        then [ConstructorWithGenerator (extractPropertyPosition propName)]
        else []
      _ -> []
    
    validateAccessorParameters :: JSAccessor -> JSCommaList JSExpression -> [ValidationError]
    validateAccessorParameters accessor params = 
      let paramList = fromCommaList params
          paramCount = length paramList
      in case accessor of
        JSAccessorGet _ -> 
          if paramCount == 0
          then []
          else [GetterWithParameters (TokenPn 0 0 0)]
        JSAccessorSet _ ->
          if paramCount == 1
          then []
          else if paramCount == 0
               then [SetterWithoutParameter (TokenPn 0 0 0)]
               else [SetterWithMultipleParameters (TokenPn 0 0 0)]
    
    isConstructorProperty :: JSPropertyName -> Bool
    isConstructorProperty propName = case propName of
      JSPropertyIdent _ "constructor" -> True
      _ -> False
    
    extractPropertyPosition :: JSPropertyName -> TokenPosn
    extractPropertyPosition propName = case propName of
      JSPropertyIdent annot _ -> extractAnnotationPos annot
      JSPropertyString annot _ -> extractAnnotationPos annot
      JSPropertyNumber annot _ -> extractAnnotationPos annot
      JSPropertyComputed lbracket _ _ -> extractAnnotationPos lbracket

validateLabelledStatement :: ValidationContext -> JSIdent -> JSStatement -> [ValidationError]
validateLabelledStatement ctx label stmt = 
  let labelErrors = case label of
        JSIdentNone -> []
        JSIdentName _annot labelName -> 
          let labelText = Text.pack labelName
          in if labelText `elem` contextLabels ctx
             then [DuplicateLabel labelText (extractIdentPos label)]
             else []
      stmtCtx = case label of
        JSIdentName _annot labelName -> 
          ctx { contextLabels = Text.pack labelName : contextLabels ctx }
        JSIdentNone -> ctx
  in labelErrors ++ validateStatement stmtCtx stmt

validateSwitchCase :: ValidationContext -> JSSwitchParts -> [ValidationError]
validateSwitchCase ctx switchPart = case switchPart of
  JSCase _case expr _colon stmts ->
    validateExpression ctx expr ++ concatMap (validateStatement ctx) stmts
  JSDefault _default _colon stmts ->
    concatMap (validateStatement ctx) stmts

validateSwitchCases :: [JSSwitchParts] -> [ValidationError]
validateSwitchCases cases = 
  let defaultCount = length (filter isDefaultCase cases)
  in if defaultCount > 1
     then [MultipleDefaultCases (TokenPn 0 0 0)]
     else []
  where
    isDefaultCase :: JSSwitchParts -> Bool
    isDefaultCase switchPart = case switchPart of
      JSDefault _ _ _ -> True
      _ -> False

validateCatchClause :: ValidationContext -> JSTryCatch -> [ValidationError]
validateCatchClause ctx catchClause = case catchClause of
  JSCatch _catch _lparen param _rparen block ->
    validateExpression ctx param ++ validateBlock ctx block
  JSCatchIf _catch _lparen param _if test _rparen block ->
    validateExpression ctx param ++ validateExpression ctx test ++ validateBlock ctx block

validateFinallyClause :: ValidationContext -> JSTryFinally -> [ValidationError]
validateFinallyClause ctx finally' = case finally' of
  JSFinally _finally block -> validateBlock ctx block
  JSNoFinally -> []

validateWithStatement :: ValidationContext -> JSExpression -> JSStatement -> [ValidationError]
validateWithStatement ctx object stmt =
  (if contextStrictMode ctx == StrictModeOn 
     then [WithStatementInStrict (TokenPn 0 0 0)]
     else []) ++
  validateExpression ctx object ++
  validateStatement ctx stmt

validateForInLHS :: ValidationContext -> JSExpression -> [ValidationError]
validateForInLHS _ctx lhs = validateForIteratorLHS lhs InvalidLHSInForIn

validateForOfLHS :: ValidationContext -> JSExpression -> [ValidationError]
validateForOfLHS _ctx lhs = validateForIteratorLHS lhs InvalidLHSInForOf

validateForIteratorLHS :: JSExpression -> (JSExpression -> TokenPosn -> ValidationError) -> [ValidationError]
validateForIteratorLHS lhs errorConstructor = case lhs of
  JSIdentifier _annot _name -> []
  JSMemberDot _obj _dot _prop -> []
  JSMemberSquare _obj _lbracket _prop _rbracket -> []
  JSArrayLiteral _lbracket _elements _rbracket -> []
  JSObjectLiteral _lbrace _props _rbrace -> []
  _ -> [errorConstructor lhs (extractExpressionPos lhs)]

validateDestructuringArray :: JSExpression -> [ValidationError]
validateDestructuringArray expr = case expr of
  JSArrayLiteral _ elements _ -> concatMap validateDestructuringElement elements
  _ -> [InvalidDestructuringTarget expr (extractExpressionPos expr)]
  where
    validateDestructuringElement :: JSArrayElement -> [ValidationError]
    validateDestructuringElement element = case element of
      JSArrayElement e -> validateDestructuringPattern e
      JSArrayComma _ -> []
    
    validateDestructuringPattern :: JSExpression -> [ValidationError]
    validateDestructuringPattern pattern = case pattern of
      JSIdentifier _ _ -> []
      JSArrayLiteral _ _ _ -> validateDestructuringArray pattern
      JSObjectLiteral _ _ _ -> validateDestructuringObject pattern
      JSSpreadExpression _ target -> validateDestructuringPattern target
      _ -> [InvalidDestructuringTarget pattern (extractExpressionPos pattern)]

validateDestructuringObject :: JSExpression -> [ValidationError] 
validateDestructuringObject expr = case expr of
  JSObjectLiteral _ props _ -> 
    let propList = fromCommaTrailingList props
    in concatMap validateDestructuringProperty propList
  _ -> [InvalidDestructuringTarget expr (extractExpressionPos expr)]
  where
    validateDestructuringProperty :: JSObjectProperty -> [ValidationError]
    validateDestructuringProperty prop = case prop of
      JSPropertyNameandValue _ _ values -> concatMap validateDestructuringPattern values
      JSPropertyIdentRef _ _ -> []
      JSObjectMethod _ -> [InvalidDestructuringTarget (JSLiteral (JSAnnot (TokenPn 0 0 0) []) "method") (TokenPn 0 0 0)]
      JSObjectSpread _ expr -> validateDestructuringPattern expr
    
    validateDestructuringPattern :: JSExpression -> [ValidationError]
    validateDestructuringPattern pattern = case pattern of
      JSIdentifier _ _ -> []
      JSArrayLiteral _ _ _ -> validateDestructuringArray pattern
      JSObjectLiteral _ _ _ -> validateDestructuringObject pattern
      _ -> [InvalidDestructuringTarget pattern (extractExpressionPos pattern)]

validateImportDeclaration :: ValidationContext -> JSImportDeclaration -> [ValidationError]
validateImportDeclaration ctx importDecl = case importDecl of
  JSImportDeclaration clause _ attrs _ -> validateImportClause ctx clause ++ maybe [] (validateImportAttributes ctx) attrs
  JSImportDeclarationBare _ _ attrs _ -> maybe [] (validateImportAttributes ctx) attrs
  where
    validateImportClause :: ValidationContext -> JSImportClause -> [ValidationError]
    validateImportClause _ _ = [] -- Import clause validation handled by import name extraction

validateImportAttributes :: ValidationContext -> JSImportAttributes -> [ValidationError]
validateImportAttributes _ctx (JSImportAttributes _ attrs _) = 
    concatMap validateImportAttribute (fromCommaList attrs)
  where
    validateImportAttribute :: JSImportAttribute -> [ValidationError]
    validateImportAttribute (JSImportAttribute _key _ _value) = []

validateExportDeclaration :: ValidationContext -> JSExportDeclaration -> [ValidationError]
validateExportDeclaration ctx exportDecl = case exportDecl of
  JSExport statement _ -> validateStatement ctx statement
  JSExportFrom _ _ _ -> []
  JSExportLocals _ _ -> []
  JSExportAllFrom _ _ _ -> []
  JSExportAllAsFrom _ _ _ _ _ -> []

validateNoDuplicateFunctionDeclarations :: [JSStatement] -> [ValidationError]
validateNoDuplicateFunctionDeclarations stmts = 
  let functionNames = extractFunctionNames stmts
      duplicates = findDuplicates functionNames
  in map (\name -> DuplicateBinding name (TokenPn 0 0 0)) duplicates
  where
    extractFunctionNames :: [JSStatement] -> [Text]
    extractFunctionNames = concatMap extractFunctionName
      where
        extractFunctionName :: JSStatement -> [Text]
        extractFunctionName stmt = case stmt of
          JSFunction _ name _ _ _ _ _ -> getIdentName name
          JSAsyncFunction _ _ name _ _ _ _ _ -> getIdentName name
          JSGenerator _ _ name _ _ _ _ _ -> getIdentName name
          _ -> []
        
        getIdentName :: JSIdent -> [Text]
        getIdentName ident = case ident of
          JSIdentNone -> []
          JSIdentName _ name -> [Text.pack name]

validateNoDuplicateExports :: [JSModuleItem] -> [ValidationError]
validateNoDuplicateExports items = 
  let exportNames = extractExportNames items
      duplicates = findDuplicates exportNames
  in map (\name -> DuplicateExport name (TokenPn 0 0 0)) duplicates
  where
    extractExportNames :: [JSModuleItem] -> [Text]
    extractExportNames = concatMap extractExportName
      where
        extractExportName :: JSModuleItem -> [Text]
        extractExportName item = case item of
          JSModuleExportDeclaration _ exportDecl -> extractExportDeclNames exportDecl
          _ -> []
        
        extractExportDeclNames :: JSExportDeclaration -> [Text]
        extractExportDeclNames exportDecl = case exportDecl of
          JSExport stmt _ -> extractStatementBindings stmt
          JSExportFrom _ _ _ -> [] -- Re-exports don't bind local names
          JSExportLocals (JSExportClause _ specs _) _ -> extractExportSpecNames specs
          JSExportAllFrom _ _ _ -> [] -- Namespace export
          JSExportAllAsFrom _ _ _ _ _ -> [] -- Namespace export as name
        
        extractExportSpecNames :: JSCommaList JSExportSpecifier -> [Text]
        extractExportSpecNames specs = concatMap extractSpecName (fromCommaList specs)
          where
            extractSpecName spec = case spec of
              JSExportSpecifier (JSIdentName _ name) -> [Text.pack name]
              JSExportSpecifierAs (JSIdentName _ _) _ (JSIdentName _ asName) -> [Text.pack asName]
              _ -> []
        
        extractStatementBindings :: JSStatement -> [Text]
        extractStatementBindings stmt = case stmt of
          JSFunction _ (JSIdentName _ name) _ _ _ _ _ -> [Text.pack name]
          JSVariable _ vars _ -> extractVarBindings vars
          JSClass _ (JSIdentName _ name) _ _ _ _ _ -> [Text.pack name]
          _ -> []
        
        extractVarBindings :: JSCommaList JSExpression -> [Text]
        extractVarBindings vars = concatMap extractVarBinding (fromCommaList vars)
          where
            extractVarBinding expr = case expr of
              JSVarInitExpression (JSIdentifier _ name) _ -> [Text.pack name]
              JSIdentifier _ name -> [Text.pack name]
              _ -> []

validateNoDuplicateImports :: [JSModuleItem] -> [ValidationError]
validateNoDuplicateImports items = 
  let importNames = extractImportNames items
      duplicates = findDuplicates importNames
  in map (\name -> DuplicateImport name (TokenPn 0 0 0)) duplicates
  where
    extractImportNames :: [JSModuleItem] -> [Text]
    extractImportNames = concatMap extractImportName
      where
        extractImportName :: JSModuleItem -> [Text]
        extractImportName item = case item of
          JSModuleImportDeclaration _ importDecl -> extractImportDeclNames importDecl
          _ -> []
        
        extractImportDeclNames :: JSImportDeclaration -> [Text]
        extractImportDeclNames importDecl = case importDecl of
          JSImportDeclaration clause _ _ _ -> extractImportClauseNames clause
          JSImportDeclarationBare _ _ _ _ -> [] -- No bindings for bare imports
        
        extractImportClauseNames :: JSImportClause -> [Text]
        extractImportClauseNames clause = case clause of
          JSImportClauseDefault (JSIdentName _ name) -> [Text.pack name]
          JSImportClauseDefault JSIdentNone -> []
          JSImportClauseNameSpace (JSImportNameSpace _ _ (JSIdentName _ name)) -> [Text.pack name]
          JSImportClauseNameSpace (JSImportNameSpace _ _ JSIdentNone) -> []
          JSImportClauseNamed (JSImportsNamed _ specs _) -> extractImportSpecNames specs
          JSImportClauseDefaultNamed (JSIdentName _ defName) _ (JSImportsNamed _ specs _) -> 
            Text.pack defName : extractImportSpecNames specs
          JSImportClauseDefaultNamed JSIdentNone _ (JSImportsNamed _ specs _) -> extractImportSpecNames specs
          JSImportClauseDefaultNameSpace (JSIdentName _ defName) _ (JSImportNameSpace _ _ (JSIdentName _ nsName)) ->
            [Text.pack defName, Text.pack nsName]
          JSImportClauseDefaultNameSpace JSIdentNone _ (JSImportNameSpace _ _ (JSIdentName _ nsName)) ->
            [Text.pack nsName]
          JSImportClauseDefaultNameSpace (JSIdentName _ defName) _ (JSImportNameSpace _ _ JSIdentNone) ->
            [Text.pack defName]
          JSImportClauseDefaultNameSpace JSIdentNone _ (JSImportNameSpace _ _ JSIdentNone) -> []
        
        extractImportSpecNames :: JSCommaList JSImportSpecifier -> [Text]
        extractImportSpecNames specs = concatMap extractImportSpecName (fromCommaList specs)
          where
            extractImportSpecName spec = case spec of
              JSImportSpecifier (JSIdentName _ name) -> [Text.pack name]
              JSImportSpecifierAs (JSIdentName _ _) _ (JSIdentName _ asName) -> [Text.pack asName]
              _ -> []

-- Position extraction helpers

-- | Extract position from JSAnnot.
extractAnnotationPos :: JSAnnot -> TokenPosn
extractAnnotationPos (JSAnnot pos _) = pos
extractAnnotationPos JSNoAnnot = TokenPn 0 0 0
extractAnnotationPos JSAnnotSpace = TokenPn 0 0 0

-- | Extract position from expression.
extractExpressionPos :: JSExpression -> TokenPosn
extractExpressionPos expr = case expr of
  JSIdentifier annot _ -> extractAnnotationPos annot
  JSDecimal annot _ -> extractAnnotationPos annot
  JSLiteral annot _ -> extractAnnotationPos annot
  JSHexInteger annot _ -> extractAnnotationPos annot
  JSBinaryInteger annot _ -> extractAnnotationPos annot
  JSOctal annot _ -> extractAnnotationPos annot
  JSStringLiteral annot _ -> extractAnnotationPos annot
  JSRegEx annot _ -> extractAnnotationPos annot
  JSBigIntLiteral annot _ -> extractAnnotationPos annot
  JSArrayLiteral annot _ _ -> extractAnnotationPos annot
  JSArrowExpression _ annot _ -> extractAnnotationPos annot
  JSAssignExpression lhs _ _ -> extractExpressionPos lhs
  JSAwaitExpression annot _ -> extractAnnotationPos annot
  JSCallExpression callee _ _ _ -> extractExpressionPos callee
  JSCallExpressionDot callee _ _ -> extractExpressionPos callee
  JSCallExpressionSquare callee _ _ _ -> extractExpressionPos callee
  JSClassExpression annot _ _ _ _ _ -> extractAnnotationPos annot
  JSCommaExpression left _ _ -> extractExpressionPos left
  JSExpressionBinary left _ _ -> extractExpressionPos left
  JSExpressionParen annot _ _ -> extractAnnotationPos annot
  JSExpressionPostfix expr' _ -> extractExpressionPos expr'
  JSExpressionTernary cond _ _ _ _ -> extractExpressionPos cond
  JSFunctionExpression annot _ _ _ _ _ -> extractAnnotationPos annot
  JSGeneratorExpression annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSAsyncFunctionExpression annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSMemberDot obj _ _ -> extractExpressionPos obj
  JSMemberExpression expr' _ _ _ -> extractExpressionPos expr'
  JSMemberNew annot _ _ _ _ -> extractAnnotationPos annot
  JSMemberSquare obj _ _ _ -> extractExpressionPos obj
  JSNewExpression annot _ -> extractAnnotationPos annot
  JSObjectLiteral annot _ _ -> extractAnnotationPos annot
  JSTemplateLiteral _ annot _ _ -> extractAnnotationPos annot
  JSUnaryExpression _ expr' -> extractExpressionPos expr'
  JSVarInitExpression lhs _ -> extractExpressionPos lhs
  JSYieldExpression annot _ -> extractAnnotationPos annot
  JSYieldFromExpression annot _ _ -> extractAnnotationPos annot
  JSImportMeta annot _ -> extractAnnotationPos annot
  JSSpreadExpression annot _ -> extractAnnotationPos annot
  JSOptionalMemberDot obj _ _ -> extractExpressionPos obj
  JSOptionalMemberSquare obj _ _ _ -> extractExpressionPos obj
  JSOptionalCallExpression callee _ _ _ -> extractExpressionPos callee

-- | Extract position from statement.
extractStatementPos :: JSStatement -> TokenPosn
extractStatementPos stmt = case stmt of
  JSStatementBlock annot _ _ _ -> extractAnnotationPos annot
  JSBreak annot _ _ -> extractAnnotationPos annot
  JSClass annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSContinue annot _ _ -> extractAnnotationPos annot
  JSConstant annot _ _ -> extractAnnotationPos annot
  JSDoWhile annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSEmptyStatement annot -> extractAnnotationPos annot
  JSFor annot _ _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForIn annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForVar annot _ _ _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForVarIn annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForLet annot _ _ _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForLetIn annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForLetOf annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForConst annot _ _ _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForConstIn annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForConstOf annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForOf annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSForVarOf annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSAsyncFunction annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSFunction annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSGenerator annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSIf annot _ _ _ _ -> extractAnnotationPos annot
  JSIfElse annot _ _ _ _ _ _ -> extractAnnotationPos annot
  JSLabelled label _ _ -> extractIdentPos label
  JSLet annot _ _ -> extractAnnotationPos annot
  JSExpressionStatement expr _ -> extractExpressionPos expr
  JSAssignStatement lhs _ _ _ -> extractExpressionPos lhs
  JSMethodCall expr _ _ _ _ -> extractExpressionPos expr
  JSReturn annot _ _ -> extractAnnotationPos annot
  JSSwitch annot _ _ _ _ _ _ _ -> extractAnnotationPos annot
  JSThrow annot _ _ -> extractAnnotationPos annot
  JSTry annot _ _ _ -> extractAnnotationPos annot
  JSVariable annot _ _ -> extractAnnotationPos annot
  JSWhile annot _ _ _ _ -> extractAnnotationPos annot
  JSWith annot _ _ _ _ _ -> extractAnnotationPos annot

-- | Extract position from identifier.
extractIdentPos :: JSIdent -> TokenPosn
extractIdentPos (JSIdentName annot _) = extractAnnotationPos annot
extractIdentPos JSIdentNone = TokenPn 0 0 0

-- | Extract position from module item.
extractTokenPosn :: JSModuleItem -> TokenPosn  
extractTokenPosn item = case item of
  JSModuleImportDeclaration annot _ -> extractAnnotationPos annot
  JSModuleExportDeclaration annot _ -> extractAnnotationPos annot
  JSModuleStatementListItem stmt -> extractStatementPos stmt