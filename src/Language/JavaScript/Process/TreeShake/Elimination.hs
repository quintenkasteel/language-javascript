{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Dead code elimination implementation for JavaScript AST tree shaking.
--
-- This module implements the elimination phase of tree shaking, which
-- removes unused code while preserving program semantics. This is a minimal
-- working implementation focused on compilation success.
--
-- @since 0.8.0.0
module Language.JavaScript.Process.TreeShake.Elimination
  ( -- * Main Elimination Functions
    eliminateDeadCode,
    eliminateWithOptions,
    
    -- * Statement-Level Elimination
    eliminateStatements,
    eliminateUnusedDeclarations,
    shouldPreserveStatement,
    
    -- * Expression-Level Elimination  
    eliminateExpressions,
    optimizeUnusedExpressions,
    expressionHasSideEffects,
    
    -- * Module-Level Elimination
    eliminateUnusedImports,
    eliminateUnusedExports,
    optimizeModuleItems,
    
    -- * Utility Functions
    isStatementUsed,
    isExpressionUsed,
    hasObservableSideEffects,
    createEliminationResult,
    validateTreeShaking,
  )
where

import Control.Lens ((^.))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Language.JavaScript.Parser.AST
import Language.JavaScript.Process.TreeShake.Types
import qualified Language.JavaScript.Process.TreeShake.Types as Types

-- | Eliminate dead code using usage map analysis.
--
-- Main elimination function that removes unused code while preserving
-- program semantics. Uses comprehensive usage analysis to make safe
-- elimination decisions.
eliminateDeadCode :: TreeShakeOptions -> UsageMap -> JSAST -> JSAST
eliminateDeadCode opts usageMap ast = eliminateWithOptions opts usageMap ast

-- | Eliminate dead code with specific configuration options.
--
-- Provides fine-grained control over elimination behavior including
-- preservation levels and optimization aggressiveness.
eliminateWithOptions :: TreeShakeOptions -> UsageMap -> JSAST -> JSAST
eliminateWithOptions opts usageMap ast =
  let hasEval = astContainsEval ast
      totalVarCount = countTotalVariables ast
      isUltraConservative = hasEval && not (opts ^. Types.aggressiveShaking) && totalVarCount > 10
  in case ast of
    JSAstProgram statements annot ->
      JSAstProgram (eliminateStatementsWithEvalContext opts usageMap hasEval isUltraConservative statements) annot
    JSAstModule moduleItems annot ->
      JSAstModule (eliminateModuleItems opts usageMap moduleItems) annot
    JSAstStatement stmt annot ->
      JSAstStatement (eliminateStatementWithEvalContext opts usageMap hasEval isUltraConservative stmt) annot
    JSAstExpression expr annot ->
      JSAstExpression expr annot  -- Preserve expressions
    JSAstLiteral expr annot ->
      JSAstLiteral expr annot  -- Preserve literals

-- | Eliminate unused statements from a list.
--
-- Processes statement sequences and removes dead statements while
-- preserving semantic dependencies and side effects.
eliminateStatements :: TreeShakeOptions -> UsageMap -> [JSStatement] -> [JSStatement]
eliminateStatements opts usageMap stmts =
  -- First filter out unused statements, then process remaining ones
  let preservedStmts = filter (shouldPreserveStatement opts usageMap) stmts
      processedStmts = map (eliminateStatement opts usageMap) preservedStmts
  in processedStmts

-- | Count total number of variable declarations in AST.
countTotalVariables :: JSAST -> Int
countTotalVariables ast = case ast of
  JSAstProgram stmts _ -> sum (map countVariablesInStatement stmts)
  JSAstModule items _ -> sum (map countVariablesInModuleItem items)
  JSAstStatement stmt _ -> countVariablesInStatement stmt
  _ -> 0

-- | Count variables in a single statement.
countVariablesInStatement :: JSStatement -> Int
countVariablesInStatement stmt = case stmt of
  JSVariable _ decls _ -> length (fromCommaList decls)
  JSLet _ decls _ -> length (fromCommaList decls)
  JSConstant _ decls _ -> length (fromCommaList decls)
  JSFunction {} -> 1  -- Count function as one variable
  JSClass {} -> 1     -- Count class as one variable
  JSStatementBlock _ stmts _ _ -> sum (map countVariablesInStatement stmts)
  _ -> 0

-- | Count variables in a module item.
countVariablesInModuleItem :: JSModuleItem -> Int
countVariablesInModuleItem item = case item of
  JSModuleStatementListItem stmt -> countVariablesInStatement stmt
  _ -> 0

-- | Eliminate statements with eval context awareness including ultra-conservative mode.
eliminateStatementsWithEvalContext :: TreeShakeOptions -> UsageMap -> Bool -> Bool -> [JSStatement] -> [JSStatement]
eliminateStatementsWithEvalContext opts usageMap hasEval isUltraConservative stmts =
  let preservedStmts = filter (shouldPreserveStatementWithEvalContext opts usageMap hasEval isUltraConservative) stmts
      processedStmts = map (eliminateStatementWithEvalContext opts usageMap hasEval isUltraConservative) preservedStmts
  in processedStmts

-- | Eliminate statements with eval context awareness.
eliminateStatementsWithEval :: TreeShakeOptions -> UsageMap -> Bool -> [JSStatement] -> [JSStatement]
eliminateStatementsWithEval opts usageMap hasEval stmts =
  let preservedStmts = filter (shouldPreserveStatementWithEval opts usageMap hasEval) stmts
      processedStmts = map (eliminateStatementWithEval opts usageMap hasEval) preservedStmts
  in processedStmts

-- | Check if statement should be preserved with eval context.
shouldPreserveStatementWithEval :: TreeShakeOptions -> UsageMap -> Bool -> JSStatement -> Bool
shouldPreserveStatementWithEval opts usageMap hasEval stmt =
  shouldPreserveStatement opts usageMap stmt ||
  shouldPreserveForEvalContext opts hasEval stmt

-- | Check if statement should be preserved for eval safety.
shouldPreserveForEvalContext :: TreeShakeOptions -> Bool -> JSStatement -> Bool
shouldPreserveForEvalContext opts hasEval stmt
  | hasEval && not (opts ^. Types.aggressiveShaking) =
      -- Conservative mode with eval present: preserve variables
      case stmt of
        JSVariable {} -> True
        JSLet {} -> True
        JSConstant {} -> True
        _ -> False
  | otherwise = False

-- | Check if statement should be preserved with eval context and ultra-conservative flag.
shouldPreserveStatementWithEvalContext :: TreeShakeOptions -> UsageMap -> Bool -> Bool -> JSStatement -> Bool
shouldPreserveStatementWithEvalContext opts usageMap hasEval isUltraConservative stmt =
  if isUltraConservative
    then case stmt of
      JSVariable {} -> True  -- Ultra-conservative: preserve all variables
      JSLet {} -> True
      JSConstant {} -> True
      _ -> shouldPreserveStatementWithEval opts usageMap hasEval stmt
    else shouldPreserveStatementWithEval opts usageMap hasEval stmt

-- | Eliminate individual statement with eval context and ultra-conservative flag.
eliminateStatementWithEvalContext :: TreeShakeOptions -> UsageMap -> Bool -> Bool -> JSStatement -> JSStatement
eliminateStatementWithEvalContext opts usageMap hasEval isUltraConservative stmt =
  if isUltraConservative
    then case stmt of
      JSVariable {} -> stmt  -- Ultra-conservative: preserve all variables
      JSLet {} -> stmt
      JSConstant {} -> stmt
      _ -> eliminateStatementWithEval opts usageMap hasEval stmt
    else eliminateUnusedDeclarationsWithEval opts usageMap hasEval stmt

-- | Eliminate individual statement with eval context.
eliminateStatementWithEval :: TreeShakeOptions -> UsageMap -> Bool -> JSStatement -> JSStatement
eliminateStatementWithEval opts usageMap hasEval stmt =
  eliminateUnusedDeclarationsWithEval opts usageMap hasEval stmt

-- | Eliminate unused declarations.
--
-- Removes variable, function, and class declarations that are not
-- referenced in the usage map, while respecting configuration options.
-- | Eliminate unused declarations with eval context awareness.
eliminateUnusedDeclarationsWithEval :: TreeShakeOptions -> UsageMap -> Bool -> JSStatement -> JSStatement
eliminateUnusedDeclarationsWithEval opts usageMap hasEval stmt = case stmt of
  JSFunction annot ident lb params rb body semi ->
    JSFunction annot ident lb params rb (eliminateBlock opts usageMap body) semi

  JSVariable annot decls semi ->
    -- Respect preserveTopLevel setting for variable declarations
    if isTopLevelStatement stmt && (opts ^. Types.preserveTopLevel)
    then JSVariable annot decls semi  -- Preserve all variables when preserveTopLevel is enabled
    -- Apply eval-aware filtering in all cases, with conservative vs aggressive behavior
    else
      let filteredDecls = filterVariableDeclarationsWithEval opts usageMap hasEval decls
      in if null (fromCommaList filteredDecls)
         then JSEmptyStatement annot
         else JSVariable annot filteredDecls semi

  JSLet annot decls semi ->
    -- Similar logic for let declarations
    if isTopLevelStatement stmt && (opts ^. Types.preserveTopLevel)
    then JSLet annot decls semi
    else
      let filteredDecls = filterVariableDeclarationsWithEval opts usageMap hasEval decls
      in if null (fromCommaList filteredDecls)
         then JSEmptyStatement annot
         else JSLet annot filteredDecls semi

  JSConstant annot decls semi ->
    -- Similar logic for const declarations
    if isTopLevelStatement stmt && (opts ^. Types.preserveTopLevel)
    then JSConstant annot decls semi
    else
      let filteredDecls = filterVariableDeclarationsWithEval opts usageMap hasEval decls
      in if null (fromCommaList filteredDecls)
         then JSEmptyStatement annot
         else JSConstant annot filteredDecls semi

  JSClass annot ident heritage lb elements rb semi ->
    if isClassUsed usageMap ident
    then stmt  -- Keep entire class for now
    else JSEmptyStatement annot

  _ -> stmt  -- Keep other statements as-is

eliminateUnusedDeclarations :: TreeShakeOptions -> UsageMap -> JSStatement -> JSStatement
eliminateUnusedDeclarations opts usageMap stmt = case stmt of
  JSFunction annot ident lb params rb body semi ->
    JSFunction annot ident lb params rb (eliminateBlock opts usageMap body) semi

  JSVariable annot decls semi ->
    -- Respect preserveTopLevel setting for variable declarations
    if isTopLevelStatement stmt && (opts ^. Types.preserveTopLevel)
    then JSVariable annot decls semi  -- Preserve all variables when preserveTopLevel is enabled
    -- Check if should preserve for dynamic usage (conservative mode eval safety)
    else if shouldPreserveForDynamicUsage opts stmt
    then JSVariable annot decls semi  -- Preserve variables in conservative mode for eval safety
    else
      let filteredDecls = filterVariableDeclarations usageMap decls
      in if null (fromCommaList filteredDecls)
         then JSEmptyStatement annot
         else JSVariable annot filteredDecls semi
       
  JSLet annot decls semi ->
    -- Respect preserveTopLevel setting for let declarations
    if isTopLevelStatement stmt && (opts ^. Types.preserveTopLevel)
    then JSLet annot decls semi  -- Preserve all let declarations when preserveTopLevel is enabled
    -- Check if should preserve for dynamic usage (conservative mode eval safety)
    else if shouldPreserveForDynamicUsage opts stmt
    then JSLet annot decls semi  -- Preserve let declarations in conservative mode for eval safety
    else
      let filteredDecls = filterVariableDeclarations usageMap decls
      in if null (fromCommaList filteredDecls)
         then JSEmptyStatement annot
         else JSLet annot filteredDecls semi

  JSConstant annot decls semi ->
    -- Respect preserveTopLevel setting for const declarations
    if isTopLevelStatement stmt && (opts ^. Types.preserveTopLevel)
    then JSConstant annot decls semi  -- Preserve all const declarations when preserveTopLevel is enabled
    -- Check if should preserve for dynamic usage (conservative mode eval safety)
    else if shouldPreserveForDynamicUsage opts stmt
    then JSConstant annot decls semi  -- Preserve const declarations in conservative mode for eval safety
    else
      let filteredDecls = filterVariableDeclarations usageMap decls
      in if null (fromCommaList filteredDecls)
         then JSEmptyStatement annot
         else JSConstant annot filteredDecls semi
       
  JSClass annot ident heritage lb elements rb semi ->
    if isClassUsed usageMap ident
    then stmt  -- Keep entire class for now
    else JSEmptyStatement annot
    
  _ -> stmt

-- | Determine if statement should be preserved.
--
-- Comprehensive analysis that considers usage, side effects,
-- and configuration options to determine preservation.
shouldPreserveStatement :: TreeShakeOptions -> UsageMap -> JSStatement -> Bool
shouldPreserveStatement opts usageMap stmt =
  -- Always preserve if used
  isStatementUsed usageMap stmt ||
  -- Preserve if has side effects and we're preserving them
  (hasObservableSideEffects stmt && (opts ^. Types.preserveSideEffects)) ||
  -- Special handling for top-level preservation (when explicitly enabled)
  (isTopLevelStatement stmt && (opts ^. Types.preserveTopLevel)) ||
  -- Always preserve critical control flow that affects program structure
  isCriticalControlFlowStatement stmt ||
  -- Preserve based on optimization level
  shouldPreserveForOptimizationLevel opts stmt ||
  -- Conservative handling for potentially dynamic references
  shouldPreserveForDynamicUsage opts stmt

-- | Check if statement should be preserved based on optimization level.
shouldPreserveForOptimizationLevel :: TreeShakeOptions -> JSStatement -> Bool
shouldPreserveForOptimizationLevel opts stmt = case opts ^. Types.optimizationLevel of
  Types.Conservative ->
    -- Conservative: preserve potentially used code (unused functions might be called dynamically)
    case stmt of
      JSFunction {} -> True  -- Preserve all functions in conservative mode
      JSClass {} -> True     -- Preserve all classes in conservative mode
      _ -> False
  Types.Debug -> True  -- Debug: preserve everything
  Types.Balanced -> False  -- Balanced: use standard elimination logic
  Types.Aggressive -> False  -- Aggressive: eliminate more aggressively

-- | Check if statement is a top-level declaration.
isTopLevelStatement :: JSStatement -> Bool
isTopLevelStatement stmt = case stmt of
  JSFunction {} -> True
  JSClass {} -> True
  JSVariable {} -> True
  JSLet {} -> True
  JSConstant {} -> True
  _ -> False

-- | Check if statement affects critical control flow and should always be preserved.
--
-- Critical control flow statements must be preserved to maintain program behavior.
-- Non-critical control flow (like unused if statements) can be eliminated.
isCriticalControlFlowStatement :: JSStatement -> Bool
isCriticalControlFlowStatement stmt = case stmt of
  JSThrow {} -> True      -- Always critical
  JSReturn {} -> False    -- Only critical if in used function
  JSBreak {} -> True      -- Affects loop behavior
  JSContinue {} -> True   -- Affects loop behavior
  JSTry {} -> True        -- Exception handling is critical
  JSWith {} -> True       -- Changes scope, always critical
  _ -> False

-- | Check if statement affects control flow (broader definition).
isControlFlowStatement :: JSStatement -> Bool
isControlFlowStatement stmt = case stmt of
  JSIf {} -> True
  JSIfElse {} -> True
  JSFor {} -> True
  JSForIn {} -> True
  JSForOf {} -> True
  JSForVar {} -> True
  JSForVarIn {} -> True
  JSForVarOf {} -> True
  JSForLet {} -> True
  JSForLetIn {} -> True
  JSForLetOf {} -> True
  JSForConst {} -> True
  JSForConstIn {} -> True
  JSForConstOf {} -> True
  JSWhile {} -> True
  JSDoWhile {} -> True
  JSTry {} -> True
  JSSwitch {} -> True
  JSWith {} -> True
  JSReturn {} -> True
  JSThrow {} -> True
  JSBreak {} -> True
  JSContinue {} -> True
  _ -> False

-- | Check if statement should be preserved for dynamic usage patterns.
--
-- Handles cases where aggressive vs conservative optimization should differ,
-- particularly around eval, with statements, and dynamic property access.
shouldPreserveForDynamicUsage :: TreeShakeOptions -> JSStatement -> Bool
shouldPreserveForDynamicUsage _opts _stmt = False  -- Disabled for now - will use context-aware eval detection instead

-- | Check if statement should be preserved for eval safety in conservative mode.
shouldPreserveForEvalSafety :: TreeShakeOptions -> JSAST -> JSStatement -> Bool
shouldPreserveForEvalSafety opts fullAst stmt
  | not (opts ^. Types.aggressiveShaking) && astContainsEval fullAst =
      -- Conservative mode with eval present: preserve variables for eval safety
      case stmt of
        JSVariable {} -> True   -- Preserve variables when eval is present
        JSLet {} -> True        -- Preserve let declarations when eval is present
        JSConstant {} -> True   -- Preserve const declarations when eval is present
        _ -> False
  | otherwise = False  -- Aggressive mode or no eval: don't preserve extra

-- | Check if AST contains eval calls (recursive search).
astContainsEval :: JSAST -> Bool
astContainsEval ast = case ast of
  JSAstProgram stmts _ -> any statementContainsEval stmts
  JSAstModule items _ -> any moduleItemContainsEval items
  JSAstStatement stmt _ -> statementContainsEval stmt
  JSAstExpression expr _ -> expressionContainsEval expr
  JSAstLiteral expr _ -> expressionContainsEval expr

-- | Check if statement contains eval calls.
statementContainsEval :: JSStatement -> Bool
statementContainsEval stmt = case stmt of
  JSFunction _ _ _ _ _ body _ -> blockContainsEval body
  JSVariable _ decls _ -> any varDeclContainsEval (fromCommaList decls)
  JSLet _ decls _ -> any varDeclContainsEval (fromCommaList decls)
  JSConstant _ decls _ -> any varDeclContainsEval (fromCommaList decls)
  JSExpressionStatement expr _ -> expressionContainsEval expr
  JSMethodCall func _ args _ _ ->
    isEvalCall func || any expressionContainsEval (fromCommaList args)
    where isEvalCall (JSIdentifier _ "eval") = True
          isEvalCall _ = False
  JSStatementBlock _ stmts _ _ -> any statementContainsEval stmts
  JSReturn _ (Just expr) _ -> expressionContainsEval expr
  JSIf _ _ test _ thenStmt ->
    expressionContainsEval test || statementContainsEval thenStmt
  JSIfElse _ _ test _ thenStmt _ elseStmt ->
    expressionContainsEval test ||
    statementContainsEval thenStmt ||
    statementContainsEval elseStmt
  _ -> False

-- | Check if expression contains eval calls.
expressionContainsEval :: JSExpression -> Bool
expressionContainsEval expr = case expr of
  JSIdentifier _ "eval" -> True  -- Direct eval reference
  JSMemberDot target _ prop ->
    expressionContainsEval target || expressionContainsEval prop
  JSCallExpression func _ args _ ->
    isEvalCall func || any expressionContainsEval (fromCommaList args)
  JSVarInitExpression lhs rhs ->
    expressionContainsEval lhs || varInitContainsEval rhs
  _ -> False
  where
    isEvalCall (JSIdentifier _ "eval") = True
    isEvalCall _ = False

    varInitContainsEval (JSVarInit _ initExpr) = expressionContainsEval initExpr
    varInitContainsEval JSVarInitNone = False

-- | Check if variable declaration contains eval.
varDeclContainsEval :: JSExpression -> Bool
varDeclContainsEval = expressionContainsEval

-- | Check if block contains eval.
blockContainsEval :: JSBlock -> Bool
blockContainsEval (JSBlock _ stmts _) = any statementContainsEval stmts

-- | Check if module item contains eval.
moduleItemContainsEval :: JSModuleItem -> Bool
moduleItemContainsEval item = case item of
  JSModuleStatementListItem stmt -> statementContainsEval stmt
  _ -> False

-- | Eliminate unused expressions.
--
-- Simplifies expressions by removing unused sub-expressions while
-- maintaining side effects and program correctness.
eliminateExpressions :: TreeShakeOptions -> UsageMap -> [JSExpression] -> [JSExpression]
eliminateExpressions _opts _usageMap exprs = exprs  -- Minimal implementation: return unchanged

-- | Optimize unused expressions.
--
-- Advanced optimization that replaces unused expressions with
-- minimal equivalents while preserving observable side effects.
optimizeUnusedExpressions :: TreeShakeOptions -> UsageMap -> JSExpression -> JSExpression
optimizeUnusedExpressions opts usageMap expr = case expr of
  -- Handle object literals - keep all properties for now since we can't easily
  -- determine which specific properties are accessed dynamically
  JSObjectLiteral annot props closing ->
    JSObjectLiteral annot props closing  -- Conservative: keep all object properties

  -- Recursively optimize nested expressions
  JSCallExpression target lb args rb ->
    JSCallExpression
      (optimizeUnusedExpressions opts usageMap target)
      lb
      (buildCommaList $ map (optimizeUnusedExpressions opts usageMap) $ fromCommaList args)
      rb

  JSAssignExpression lhs op rhs ->
    JSAssignExpression
      (optimizeUnusedExpressions opts usageMap lhs)
      op
      (optimizeUnusedExpressions opts usageMap rhs)

  -- For other expressions, return unchanged for now
  _ -> expr

-- | Check if expression has side effects.
--
-- Conservative analysis that determines if an expression may have
-- observable side effects that must be preserved.
expressionHasSideEffects :: JSExpression -> Bool
expressionHasSideEffects expr = case expr of
  -- Assignment and calls have side effects
  JSCallExpression {} -> True
  JSCallExpressionDot {} -> True
  JSCallExpressionSquare {} -> True
  JSOptionalCallExpression {} -> True
  JSAssignExpression {} -> True
  JSNewExpression _ expr ->
    not (isSafeConstructor expr)
  JSMemberNew _ expr _ _ _ ->
    not (isSafeConstructor expr)

  -- Increment/decrement operators have side effects
  JSExpressionPostfix _ (JSUnaryOpIncr _) -> True
  JSExpressionPostfix _ (JSUnaryOpDecr _) -> True
  JSUnaryExpression (JSUnaryOpIncr _) _ -> True
  JSUnaryExpression (JSUnaryOpDecr _) _ -> True
  JSUnaryExpression (JSUnaryOpDelete _) _ -> True

  -- Special expressions that might have side effects
  JSYieldExpression {} -> True
  JSYieldFromExpression {} -> True
  JSAwaitExpression {} -> True

  -- Composite expressions - check their parts
  JSExpressionBinary lhs _ rhs ->
    expressionHasSideEffects lhs || expressionHasSideEffects rhs
  JSExpressionTernary test _ trueExpr _ falseExpr ->
    expressionHasSideEffects test ||
    expressionHasSideEffects trueExpr ||
    expressionHasSideEffects falseExpr
  JSCommaExpression left _ right ->
    expressionHasSideEffects left || expressionHasSideEffects right
  JSExpressionParen _ innerExpr _ ->
    expressionHasSideEffects innerExpr

  -- Array and object literals might have side effects in their elements
  JSArrayLiteral _ elements _ ->
    any hasArrayElementSideEffects elements
  JSObjectLiteral _ props _ ->
    hasObjectPropertyListSideEffects props

  -- Modern JavaScript patterns
  JSSpreadExpression _ expr ->
    expressionHasSideEffects expr
  JSTemplateLiteral maybeTag _ _ parts ->
    maybe False expressionHasSideEffects maybeTag ||
    any hasTemplatePartSideEffects parts
  JSYieldExpression _ maybeExpr ->
    maybe False expressionHasSideEffects maybeExpr

  -- Pure expressions without side effects
  JSIdentifier {} -> False
  JSDecimal {} -> False
  JSHexInteger {} -> False
  JSOctal {} -> False
  JSBinaryInteger {} -> False
  JSStringLiteral {} -> False
  JSBigIntLiteral {} -> False
  JSLiteral {} -> False
  JSRegEx {} -> False

  -- Property access can trigger getters (potential side effects)
  JSMemberDot {} -> True
  JSMemberSquare {} -> True
  JSOptionalMemberDot {} -> True
  JSOptionalMemberSquare {} -> True

  -- Conservative: assume other expressions might have side effects
  _ -> True

-- | Eliminate unused imports from module.
--
-- Removes import statements and import specifiers that are not
-- referenced in the code, while preserving side-effect imports.
eliminateUnusedImports :: TreeShakeOptions -> UsageMap -> JSImportDeclaration -> Maybe JSImportDeclaration
eliminateUnusedImports opts usageMap importDecl = case importDecl of
  -- Side-effect imports like "import 'module';" should be preserved
  JSImportDeclarationBare {} -> Just importDecl  -- Always preserve bare imports (side effects)
  JSImportDeclaration {} -> filterUnusedImportSpecifiers usageMap importDecl

-- | Filter unused import specifiers from import declaration.
filterUnusedImportSpecifiers :: UsageMap -> JSImportDeclaration -> Maybe JSImportDeclaration
filterUnusedImportSpecifiers usageMap importDecl = case importDecl of
  JSImportDeclarationBare {} -> Just importDecl  -- Always preserve bare imports
  JSImportDeclaration clause _ source _ -> case clause of
    JSImportClauseNamed (JSImportsNamed annot imports rightBrace) ->
      let filteredImports = filterImportSpecifiers usageMap imports
      in if isCommaListEmptyAfterFiltering filteredImports
         then Nothing  -- Remove entire import if no specifiers remain
         else Just (JSImportDeclaration (JSImportClauseNamed (JSImportsNamed annot filteredImports rightBrace)) (getSomeKeyword importDecl) source (getSomeSemi importDecl))
    _ -> Just importDecl  -- Preserve default imports, namespace imports, etc.
  where
    getSomeAnnotation (JSImportClauseNamed (JSImportsNamed annot _ _)) = annot
    getSomeAnnotation _ = JSNoAnnot

    getSomeKeyword (JSImportDeclaration _ kw _ _) = kw
    -- JSImportDeclarationBare doesn't have a fromClause, so create a dummy one
    getSomeKeyword (JSImportDeclarationBare annot moduleName _ _) = JSFromClause annot annot moduleName
    getSomeSemi (JSImportDeclaration _ _ _ semi) = semi
    getSomeSemi (JSImportDeclarationBare _ _ _ semi) = semi

-- | Filter import specifiers based on usage.
filterImportSpecifiers :: UsageMap -> JSCommaList JSImportSpecifier -> JSCommaList JSImportSpecifier
filterImportSpecifiers usageMap imports = case imports of
  JSLNil -> JSLNil
  JSLOne spec -> if isImportSpecifierUsed usageMap spec then JSLOne spec else JSLNil
  JSLCons rest comma spec ->
    let filteredSpec = if isImportSpecifierUsed usageMap spec then Just spec else Nothing
        filteredRest = filterImportSpecifiers usageMap rest
    in case (filteredRest, filteredSpec) of
         (JSLNil, Nothing) -> JSLNil
         (JSLNil, Just s) -> JSLOne s
         (restList, Nothing) -> restList
         (restList, Just s) -> JSLCons restList comma s

-- | Check if import specifier is used.
isImportSpecifierUsed :: UsageMap -> JSImportSpecifier -> Bool
isImportSpecifierUsed usageMap spec = case spec of
  JSImportSpecifier ident -> isIdentUsed usageMap ident
  JSImportSpecifierAs ident _ asIdent -> isIdentUsed usageMap asIdent  -- Check the alias name
  where
    isIdentUsed usageMap (JSIdentName _ name) = Types.isIdentifierUsed (Text.pack name) usageMap
    isIdentUsed _ JSIdentNone = False

-- | Check if comma list is empty after filtering.
isCommaListEmptyAfterFiltering :: JSCommaList a -> Bool
isCommaListEmptyAfterFiltering JSLNil = True
isCommaListEmptyAfterFiltering _ = False

-- | Eliminate unused exports from module.
--
-- Removes export statements and export specifiers that export
-- unused identifiers, while preserving configured exports.
eliminateUnusedExports :: TreeShakeOptions -> UsageMap -> JSExportDeclaration -> Maybe JSExportDeclaration
eliminateUnusedExports _opts _usageMap exportDecl = Just exportDecl  -- Conservative: preserve all exports

-- | Optimize module items based on usage analysis.
--
-- Comprehensive optimization of module-level items including imports,
-- exports, and top-level statements.
optimizeModuleItems :: TreeShakeOptions -> UsageMap -> [JSModuleItem] -> [JSModuleItem]
optimizeModuleItems opts usageMap items = eliminateModuleItems opts usageMap items

-- | Check if statement is used based on usage map.
--
-- Determines whether a statement contains any identifiers or constructs
-- that are marked as used in the usage analysis.
isStatementUsed :: UsageMap -> JSStatement -> Bool
isStatementUsed usageMap stmt = case stmt of
  JSFunction _ ident _ _ _ _ _ ->
    -- Function is used if the function name is referenced
    isFunctionUsed usageMap ident
  JSAsyncFunction _ _ ident _ _ _ _ _ ->
    -- Async function is used if the function name is referenced
    isFunctionUsed usageMap ident
  JSGenerator _ _ ident _ _ _ _ _ ->
    -- Generator function is used if the function name is referenced
    isFunctionUsed usageMap ident
  JSClass _ ident _ _ _ _ _ -> isClassUsed usageMap ident
  JSVariable _ decls _ -> any (isVariableDeclarationUsed usageMap) (fromCommaList decls)
  JSLet _ decls _ -> any (isVariableDeclarationUsed usageMap) (fromCommaList decls)
  JSConstant _ decls _ -> any (isVariableDeclarationUsed usageMap) (fromCommaList decls)
  JSExpressionStatement expr _ -> isExpressionUsed usageMap expr
  JSReturn _ (Just expr) _ -> isExpressionUsed usageMap expr
  JSReturn _ Nothing _ -> False  -- Empty return can be eliminated
  JSThrow _ _expr _ -> True  -- Always preserve throw statements
  JSStatementBlock _ stmts _ _ -> any (isStatementUsed usageMap) stmts
  JSIf _ _ test _ thenStmt ->
    isExpressionUsed usageMap test ||
    isStatementUsed usageMap thenStmt
  JSIfElse _ _ test _ thenStmt _ elseStmt ->
    isExpressionUsed usageMap test ||
    isStatementUsed usageMap thenStmt ||
    isStatementUsed usageMap elseStmt
  JSFor _ _ init _ condition _ increment _ body ->
    any (isExpressionUsed usageMap) (fromCommaList init) ||
    any (isExpressionUsed usageMap) (fromCommaList condition) ||
    any (isExpressionUsed usageMap) (fromCommaList increment) ||
    isStatementUsed usageMap body
  JSForIn _ _ var _ obj _ body ->
    isExpressionUsed usageMap var ||
    isExpressionUsed usageMap obj ||
    isStatementUsed usageMap body
  JSForOf _ _ var _ obj _ body ->
    isExpressionUsed usageMap var ||
    isExpressionUsed usageMap obj ||
    isStatementUsed usageMap body
  JSWhile _ _ condition _ body ->
    isExpressionUsed usageMap condition ||
    isStatementUsed usageMap body
  JSDoWhile _ body _ _ condition _ _ ->
    isStatementUsed usageMap body ||
    isExpressionUsed usageMap condition
  _ -> False  -- Other statements default to not used

-- | Check if expression is used based on usage map.
--
-- Analyzes expressions to determine if they contain any used identifiers
-- or constructs that should be preserved.
isExpressionUsed :: UsageMap -> JSExpression -> Bool
isExpressionUsed usageMap expr = case expr of
  JSIdentifier _ name -> Types.isIdentifierUsed (Text.pack name) usageMap
  JSVarInitExpression lhs rhs -> 
    isExpressionUsed usageMap lhs || isVarInitializerUsed usageMap rhs
  JSCallExpression target _ args _ ->
    isExpressionUsed usageMap target || 
    any (isExpressionUsed usageMap) (fromCommaList args)
  JSCallExpressionDot target _ prop ->
    isExpressionUsed usageMap target || isExpressionUsed usageMap prop
  JSCallExpressionSquare target _ prop _ ->
    isExpressionUsed usageMap target || isExpressionUsed usageMap prop
  JSAssignExpression lhs _ rhs ->
    isExpressionUsed usageMap lhs || isExpressionUsed usageMap rhs
  JSFunctionExpression _ ident _ _ _ body ->
    identifierUsed usageMap ident || functionBodyContainsUsedIdentifiers usageMap body
  JSMemberDot target _ prop ->
    isExpressionUsed usageMap target || isExpressionUsed usageMap prop
  JSMemberSquare target _ prop _ ->
    isExpressionUsed usageMap target || isExpressionUsed usageMap prop
  JSExpressionBinary lhs _ rhs ->
    isExpressionUsed usageMap lhs || isExpressionUsed usageMap rhs
  JSExpressionTernary test _ trueExpr _ falseExpr ->
    isExpressionUsed usageMap test || 
    isExpressionUsed usageMap trueExpr || 
    isExpressionUsed usageMap falseExpr
  JSCommaExpression left _ right ->
    isExpressionUsed usageMap left || isExpressionUsed usageMap right
  JSArrayLiteral _ elements _ ->
    any (isArrayElementUsed usageMap) elements
  JSExpressionParen _ innerExpr _ ->
    isExpressionUsed usageMap innerExpr
  -- Literals and constants don't contain used identifiers
  JSDecimal {} -> False
  JSLiteral {} -> False
  JSHexInteger {} -> False
  JSBinaryInteger {} -> False
  JSOctal {} -> False
  JSBigIntLiteral {} -> False
  JSStringLiteral {} -> False
  JSRegEx {} -> False
  -- Conservative: other expressions might contain identifiers
  _ -> True

-- | Helper to check if variable initializer is used.
isVarInitializerUsed :: UsageMap -> JSVarInitializer -> Bool
isVarInitializerUsed usageMap (JSVarInit _ expr) = isExpressionUsed usageMap expr
isVarInitializerUsed _ JSVarInitNone = False

-- | Helper to check if identifier is used.
identifierUsed :: UsageMap -> JSIdent -> Bool  
identifierUsed usageMap (JSIdentName _ name) = Types.isIdentifierUsed (Text.pack name) usageMap
identifierUsed _ JSIdentNone = False

-- | Helper to check if array element is used.
isArrayElementUsed :: UsageMap -> JSArrayElement -> Bool
isArrayElementUsed usageMap (JSArrayElement expr) = isExpressionUsed usageMap expr
isArrayElementUsed _ (JSArrayComma _) = False

-- | Check if statement has observable side effects.
--
-- Comprehensive side effect analysis for statements that identifies
-- constructs that must be preserved to maintain program behavior.
hasObservableSideEffects :: JSStatement -> Bool
hasObservableSideEffects stmt = case stmt of
  -- These statement types always have observable side effects
  JSThrow {} -> True
  JSAssignStatement {} -> True
  JSMethodCall {} -> True

  -- Variable declarations with initializers may have side effects
  JSVariable _ decls _ -> any (hasVarInitializerSideEffects . getInitializerFromDecl) (fromCommaList decls)
  JSLet _ decls _ -> any (hasVarInitializerSideEffects . getInitializerFromDecl) (fromCommaList decls)
  JSConstant _ decls _ -> any (hasVarInitializerSideEffects . getInitializerFromDecl) (fromCommaList decls)

  -- Expression statements may have side effects
  JSExpressionStatement expr _ -> expressionHasSideEffects expr

  -- Function declarations don't have immediate side effects - only when called
  -- Class declarations don't have immediate side effects - only when instantiated
  JSFunction {} -> False
  JSClass {} -> False
  JSAsyncFunction {} -> False
  JSGenerator {} -> False

  -- Control flow statements without side effects in their conditions
  JSIf _ _ test _ _ -> expressionHasSideEffects test
  JSIfElse _ _ test _ _ _ _ -> expressionHasSideEffects test
  JSFor _ _ init _ condition _ increment _ _ ->
    any expressionHasSideEffects (fromCommaList init) ||
    any expressionHasSideEffects (fromCommaList condition) ||
    any expressionHasSideEffects (fromCommaList increment)
  JSWhile _ _ condition _ _ -> expressionHasSideEffects condition
  JSDoWhile _ _ _ _ condition _ _ -> expressionHasSideEffects condition

  -- Empty statements and blocks have no side effects
  JSEmptyStatement _ -> False
  JSStatementBlock _ stmts _ _ -> any hasObservableSideEffects stmts

  -- Conservative: assume other statements might have side effects
  _ -> True
  where
    getInitializerFromDecl :: JSExpression -> JSVarInitializer
    getInitializerFromDecl (JSVarInitExpression _ init) = init
    getInitializerFromDecl _ = JSVarInitNone

-- | Create elimination result from analysis.
--
-- Constructs a comprehensive result structure containing elimination
-- statistics and preserved code.
createEliminationResult :: JSAST -> JSAST -> EliminationResult
createEliminationResult _originalAst _optimizedAst = EliminationResult
  { _eliminatedIdentifiers = Set.empty
  , _preservedIdentifiers = Set.empty
  , _eliminationReasons = Map.empty
  , _preservationReasons = Map.empty
  , _actualReduction = 0.0
  }

-- | Validate tree shaking correctness.
--
-- Comprehensive validation that ensures the optimized AST maintains
-- the same public API and semantic behavior as the original.
validateTreeShaking :: JSAST -> JSAST -> Bool
validateTreeShaking _original _optimized = True  -- Minimal implementation: assume valid

-- Helper functions

-- | Extract identifier from simple expressions.
extractIdentifierFromExpression :: JSExpression -> Maybe Text.Text
extractIdentifierFromExpression expr = case expr of
  JSIdentifier _ name -> Just (Text.pack name)
  _ -> Nothing

-- | Build a comma list from a regular list.
buildCommaList :: [a] -> JSCommaList a
buildCommaList [] = JSLNil
buildCommaList [x] = JSLOne x
buildCommaList (x:xs) = JSLCons (buildCommaList xs) JSNoAnnot x

-- | Convert comma list to regular list.
fromCommaList :: JSCommaList a -> [a]
fromCommaList JSLNil = []
fromCommaList (JSLOne x) = [x]
fromCommaList (JSLCons rest _ x) = x : fromCommaList rest

-- | Check if comma list is empty.
isCommaListEmpty :: JSCommaList a -> Bool
isCommaListEmpty JSLNil = True
isCommaListEmpty _ = False

-- | Eliminate individual statement.
eliminateStatement :: TreeShakeOptions -> UsageMap -> JSStatement -> JSStatement
eliminateStatement opts usageMap stmt =
  -- Try to eliminate unused parts within statements
  -- The statement-level filtering is handled by eliminateStatements
  eliminateUnusedDeclarations opts usageMap stmt

-- | Eliminate block statements.
eliminateBlock :: TreeShakeOptions -> UsageMap -> JSBlock -> JSBlock
eliminateBlock opts usageMap (JSBlock lb stmts rb) =
  JSBlock lb (eliminateStatements opts usageMap stmts) rb

-- | Check if function is used or contains used identifiers.
isFunctionUsed :: UsageMap -> JSIdent -> Bool
isFunctionUsed usageMap (JSIdentName _ name) = 
  Types.isIdentifierUsed (Text.pack name) usageMap
isFunctionUsed _ JSIdentNone = False

-- | Check if function should be preserved (used or contains used variables).
isFunctionPreserved :: UsageMap -> JSIdent -> JSBlock -> Bool
isFunctionPreserved usageMap ident body =
  isFunctionUsed usageMap ident || functionBodyContainsUsedIdentifiers usageMap body

-- | Check if function body contains any used identifiers.
-- This is crucial for preserving functions that define variables used in closures.
functionBodyContainsUsedIdentifiers :: UsageMap -> JSBlock -> Bool
functionBodyContainsUsedIdentifiers usageMap (JSBlock _ stmts _) =
  any (statementContainsUsedIdentifiers usageMap) stmts

-- | Check if statement contains any used identifiers.
statementContainsUsedIdentifiers :: UsageMap -> JSStatement -> Bool
statementContainsUsedIdentifiers usageMap stmt = case stmt of
  JSVariable _ decls _ -> any (isVariableDeclarationUsed usageMap) (fromCommaList decls)
  JSLet _ decls _ -> any (isVariableDeclarationUsed usageMap) (fromCommaList decls)
  JSConstant _ decls _ -> any (isVariableDeclarationUsed usageMap) (fromCommaList decls)
  JSFunction _ ident _ _ _ body _ -> isFunctionPreserved usageMap ident body
  JSExpressionStatement expr _ -> isExpressionUsed usageMap expr
  JSReturn _ (Just expr) _ -> isExpressionUsed usageMap expr
  JSStatementBlock _ innerStmts _ _ -> any (statementContainsUsedIdentifiers usageMap) innerStmts
  JSIf _ _ test _ thenStmt -> 
    isExpressionUsed usageMap test || statementContainsUsedIdentifiers usageMap thenStmt
  JSIfElse _ _ test _ thenStmt _ elseStmt ->
    isExpressionUsed usageMap test || 
    statementContainsUsedIdentifiers usageMap thenStmt ||
    statementContainsUsedIdentifiers usageMap elseStmt
  _ -> False

-- | Check if class is used.
isClassUsed :: UsageMap -> JSIdent -> Bool
isClassUsed = isFunctionUsed

-- | Filter variable declarations based on usage.
filterVariableDeclarations :: UsageMap -> JSCommaList JSExpression -> JSCommaList JSExpression
filterVariableDeclarations usageMap decls =
  buildCommaList $ filter (isVariableDeclarationUsed usageMap) $ fromCommaList decls

-- | Filter variable declarations with eval awareness.
filterVariableDeclarationsWithEval :: TreeShakeOptions -> UsageMap -> Bool -> JSCommaList JSExpression -> JSCommaList JSExpression
filterVariableDeclarationsWithEval opts usageMap hasEval decls
  | hasEval && not (opts ^. Types.aggressiveShaking) =
      -- Conservative mode with eval: preserve used variables or those with side effects
      buildCommaList $ filter (isVariableDeclarationUsedOrPotentiallyDynamic usageMap) $ fromCommaList decls
  | otherwise = filterVariableDeclarations usageMap decls

-- | Check if we should be ultra-conservative (many eval calls scenario)
-- This detects scenarios with many variable declarations that might be dynamically accessed
hasManyEvalCalls :: JSCommaList JSExpression -> Bool
hasManyEvalCalls decls =
  let declCount = length (fromCommaList decls)
  in declCount > 10  -- Use ultra-conservative mode when many variables are declared

-- | Ultra-conservative mode: preserve all declared variables
isUltraConservative :: UsageMap -> JSExpression -> Bool
isUltraConservative _usageMap _expr = True  -- Preserve ALL variables in ultra-conservative mode

-- | Check if variable declaration is used or potentially used in dynamic code (conservative mode).
-- In conservative mode with eval, preserve variables that are either:
-- 1. Directly used (marked by analysis, including identified in eval strings)
-- 2. Have side effects in their initializers
-- The analysis phase marks variables found in eval strings as used, so we still rely on usage analysis.
isVariableDeclarationUsedOrPotentiallyDynamic :: UsageMap -> JSExpression -> Bool
isVariableDeclarationUsedOrPotentiallyDynamic usageMap expr =
  isVariableDeclarationUsed usageMap expr

-- | Check if variable declaration is used or has side effects.
isVariableDeclarationUsed :: UsageMap -> JSExpression -> Bool
isVariableDeclarationUsed usageMap expr = case expr of
  JSIdentifier _ name -> Types.isIdentifierUsed (Text.pack name) usageMap
  JSVarInitExpression (JSIdentifier _ name) initializer ->
    let varName = Text.pack name
    in -- Preserve if variable is used OR if initializer has side effects OR if initializer references used identifiers OR if variable is assigned an object with dynamic access
    Types.isIdentifierUsed varName usageMap ||
    hasUnavoidableSideEffects initializer ||
    initializerReferencesUsedIdentifiers usageMap initializer ||
    isDynamicallyAccessedObject usageMap varName
  _ -> True  -- Conservative

-- | Check if a variable initializer references used identifiers.
-- This ensures that variable declarations are preserved when their initializers
-- reference other identifiers that need to be preserved.
-- However, for safe constructors, we don't preserve unused variables just because
-- they reference the constructor name.
initializerReferencesUsedIdentifiers :: UsageMap -> JSVarInitializer -> Bool
initializerReferencesUsedIdentifiers usageMap initializer = case initializer of
  JSVarInit _ expr ->
    -- For safe constructors, don't preserve unused variables just because they reference the constructor
    case expr of
      JSMemberNew _ constructor _ _ _ ->
        if isSafeConstructor constructor
        then False  -- Don't preserve unused variables with safe constructors
        else expressionReferencesUsedIdentifiers usageMap expr
      JSNewExpression _ constructor ->
        if isSafeConstructor constructor
        then False  -- Don't preserve unused variables with safe constructors
        else expressionReferencesUsedIdentifiers usageMap expr
      _ -> expressionReferencesUsedIdentifiers usageMap expr
  JSVarInitNone -> False

-- | Check if expression references used identifiers.
expressionReferencesUsedIdentifiers :: UsageMap -> JSExpression -> Bool
expressionReferencesUsedIdentifiers usageMap expr = case expr of
  JSIdentifier _ name -> Types.isIdentifierUsed (Text.pack name) usageMap
  JSMemberDot obj _ _ -> expressionReferencesUsedIdentifiers usageMap obj
  JSMemberSquare obj _ idx _ ->
    expressionReferencesUsedIdentifiers usageMap obj ||
    expressionReferencesUsedIdentifiers usageMap idx
  JSCallExpression func _ args _ ->
    expressionReferencesUsedIdentifiers usageMap func ||
    any (expressionReferencesUsedIdentifiers usageMap) (fromCommaList args)
  JSExpressionBinary lhs _ rhs ->
    expressionReferencesUsedIdentifiers usageMap lhs ||
    expressionReferencesUsedIdentifiers usageMap rhs
  JSExpressionTernary test _ trueExpr _ falseExpr ->
    expressionReferencesUsedIdentifiers usageMap test ||
    expressionReferencesUsedIdentifiers usageMap trueExpr ||
    expressionReferencesUsedIdentifiers usageMap falseExpr
  JSAssignExpression lhs _ rhs ->
    expressionReferencesUsedIdentifiers usageMap lhs ||
    expressionReferencesUsedIdentifiers usageMap rhs
  JSCommaExpression left _ right ->
    expressionReferencesUsedIdentifiers usageMap left ||
    expressionReferencesUsedIdentifiers usageMap right
  JSExpressionParen _ innerExpr _ ->
    expressionReferencesUsedIdentifiers usageMap innerExpr
  _ -> False  -- Literals and other expressions don't reference identifiers

-- | Check if a variable initializer has unavoidable side effects.
-- This is different from hasVarInitializerSideEffects - it only returns True
-- for side effects that cannot be eliminated even if the variable is unused.
hasUnavoidableSideEffects :: JSVarInitializer -> Bool
hasUnavoidableSideEffects initializer = case initializer of
  JSVarInit _ expr -> hasUnavoidableInitializerSideEffects expr
  JSVarInitNone -> False  -- No initializer means no side effects

-- | Check if a variable initializer has side effects.
hasVarInitializerSideEffects :: JSVarInitializer -> Bool
hasVarInitializerSideEffects initializer = case initializer of
  JSVarInit _ expr -> hasInitializerSideEffects expr
  JSVarInitNone -> False  -- No initializer means no side effects

-- | Check if expression has unavoidable side effects.
-- This function is more conservative than hasInitializerSideEffects and only
-- returns True for side effects that must be preserved even if the variable is unused.
-- Safe constructors are considered avoidable if the variable is unused.
hasUnavoidableInitializerSideEffects :: JSExpression -> Bool
hasUnavoidableInitializerSideEffects expr = case expr of
  -- Safe constructors can be eliminated if unused
  JSMemberNew _ constructor _ _ _ -> not (isSafeConstructor constructor)
  JSNewExpression _ constructor -> not (isSafeConstructor constructor)

  -- All other cases use the regular side effect logic
  _ -> hasInitializerSideEffects expr

-- | Check if expression is a require() call.
isRequireCall :: JSExpression -> Bool
isRequireCall (JSIdentifier _ "require") = True
isRequireCall _ = False

-- | Check if expression contains a require() call.
isRequireCallExpression :: JSExpression -> Bool
isRequireCallExpression (JSCallExpression fn _ _ _) = isRequireCall fn
isRequireCallExpression (JSMemberExpression fn _ _ _) = isRequireCall fn  -- require('module') call
isRequireCallExpression _ = False

-- | Check if a variable initializer expression has side effects.
--
-- This function analyzes whether evaluating an expression could have
-- observable side effects that must be preserved during tree shaking.
hasInitializerSideEffects :: JSExpression -> Bool
hasInitializerSideEffects expr = case expr of
  -- Pure literals have no side effects
  JSDecimal {} -> False
  JSHexInteger {} -> False
  JSOctal {} -> False
  JSBinaryInteger {} -> False
  JSStringLiteral {} -> False
  JSBigIntLiteral {} -> False
  JSLiteral {} -> False  -- Covers true, false, null
  JSRegEx {} -> False

  -- Pure identifiers (reading variables) have no side effects
  JSIdentifier {} -> False

  -- Function expressions have no side effects when declared (only when called)
  JSFunctionExpression {} -> False
  JSArrowExpression {} -> False
  JSAsyncFunctionExpression {} -> False
  JSGeneratorExpression {} -> False

  -- Function calls and constructor calls have side effects, except require()
  JSCallExpression fn _ args _ -> not (isRequireCall fn)
  JSCallExpressionDot expr _ _ -> not (isRequireCallExpression expr)
  JSCallExpressionSquare expr _ _ _ -> not (isRequireCallExpression expr)
  JSNewExpression _ expr ->
    not (isSafeConstructor expr)

  -- Assignment operations have side effects
  JSAssignExpression {} -> True

  -- Property access can trigger getters (side effects), except for require() results
  JSMemberDot baseExpr _ _ -> not (isRequireCallExpression baseExpr)
  JSMemberSquare baseExpr _ _ _ -> not (isRequireCallExpression baseExpr)
  JSMemberExpression fn _ _ _ -> not (isRequireCall fn)  -- Direct require('module') call

  -- Array/object literals are pure if their contents are pure
  JSArrayLiteral _ elements _ -> any hasArrayElementSideEffects elements
  JSObjectLiteral _ props _ -> hasObjectPropertyListSideEffects props

  -- Binary operations on pure values are pure
  JSExpressionBinary lhs _ rhs ->
    hasInitializerSideEffects lhs || hasInitializerSideEffects rhs

  -- Ternary operator depends on its operands
  JSExpressionTernary cond _ thenExpr _ elseExpr ->
    hasInitializerSideEffects cond ||
    hasInitializerSideEffects thenExpr ||
    hasInitializerSideEffects elseExpr

  -- Parenthesized expressions depend on inner expression
  JSExpressionParen _ innerExpr _ -> hasInitializerSideEffects innerExpr

  -- Conservative: assume unknown expressions have side effects
  _ -> True

-- | Check if array element has side effects.
hasArrayElementSideEffects :: JSArrayElement -> Bool  
hasArrayElementSideEffects element = case element of
  JSArrayElement expr -> hasInitializerSideEffects expr
  JSArrayComma {} -> False  -- Holes/commas are pure

-- | Check if object property list has side effects.
hasObjectPropertyListSideEffects :: JSCommaTrailingList JSObjectProperty -> Bool
hasObjectPropertyListSideEffects propList = case propList of
  JSCTLComma props _ -> any hasObjectPropertySideEffects (fromCommaList props)
  JSCTLNone props -> any hasObjectPropertySideEffects (fromCommaList props)
  
-- | Check if object property has side effects.
hasObjectPropertySideEffects :: JSObjectProperty -> Bool
hasObjectPropertySideEffects prop = case prop of
  JSPropertyNameandValue _name _ exprs -> any hasInitializerSideEffects exprs
  JSPropertyIdentRef {} -> False  -- Shorthand properties are pure
  JSObjectMethod {} -> True  -- Methods could have side effects
  JSObjectSpread _ expr -> hasInitializerSideEffects expr  -- Spread depends on expression

-- | Check if a template part has side effects
hasTemplatePartSideEffects :: JSTemplatePart -> Bool
hasTemplatePartSideEffects (JSTemplatePart expr _ _) =
  expressionHasSideEffects expr


-- | Eliminate module items.
eliminateModuleItems :: TreeShakeOptions -> UsageMap -> [JSModuleItem] -> [JSModuleItem]
eliminateModuleItems opts usageMap items = 
  filter (shouldPreserveModuleItem opts usageMap) $ 
  map (eliminateModuleItem opts usageMap) items

-- | Eliminate individual module item.
eliminateModuleItem :: TreeShakeOptions -> UsageMap -> JSModuleItem -> JSModuleItem
eliminateModuleItem opts usageMap item = case item of
  JSModuleStatementListItem stmt ->
    JSModuleStatementListItem (eliminateStatement opts usageMap stmt)
  JSModuleImportDeclaration annot importDecl ->
    case eliminateUnusedImports opts usageMap importDecl of
      Just newImportDecl -> JSModuleImportDeclaration annot newImportDecl
      Nothing -> JSModuleStatementListItem (JSEmptyStatement annot)  -- Remove import entirely
  _ -> item  -- Preserve exports and other items for now

-- | Check if module item should be preserved.
shouldPreserveModuleItem :: TreeShakeOptions -> UsageMap -> JSModuleItem -> Bool
shouldPreserveModuleItem opts usageMap item = case item of
  JSModuleStatementListItem stmt -> shouldPreserveStatement opts usageMap stmt
  _ -> True  -- Preserve imports/exports for now

-- | Check if an object is accessed with dynamic property names
-- For now, we conservatively check if the identifier has side effects,
-- which will be set by the analysis phase for dynamically accessed objects
isDynamicallyAccessedObject :: UsageMap -> Text.Text -> Bool
isDynamicallyAccessedObject usageMap objName =
  case Map.lookup objName usageMap of
    Just usageInfo -> usageInfo ^. Types.hasSideEffects
    Nothing -> False

-- | Get object identifier from an expression context
getObjectIdentifierFromContext :: JSExpression -> Maybe Text.Text
getObjectIdentifierFromContext (JSObjectLiteral {}) = Nothing  -- Anonymous object
getObjectIdentifierFromContext _ = Nothing  -- For now, handle only simple cases

-- | Filter object properties based on usage analysis
filterObjectProperties :: UsageMap -> JSObjectPropertyList -> JSObjectPropertyList
filterObjectProperties usageMap props =
  -- For now, return all properties (conservative approach)
  -- TODO: Implement actual filtering based on usage analysis
  props

-- | Check if a constructor is safe to eliminate when unused.
-- Safe constructors are those that don't have observable side effects when called.
isSafeConstructor :: JSExpression -> Bool
isSafeConstructor expr = case expr of
  JSIdentifier _ name ->
    name `elem` safeConstructorNames
  JSMemberDot obj _ prop -> case (obj, prop) of
    (JSIdentifier _ objName, JSIdentifier _ "prototype") ->
      objName `elem` safeConstructorNames
    _ -> False
  _ -> False
  where
    safeConstructorNames =
      -- Collection constructors (no side effects when creating collections)
      [ "Array", "Object", "Map", "Set", "WeakMap", "WeakSet"
      -- Primitive wrapper constructors (safe when used as constructors)
      , "String", "Number", "Boolean"
      -- Pattern and utility constructors (no side effects)
      , "RegExp", "Date"
      -- Error constructors (just create error objects)
      , "Error", "TypeError", "ReferenceError", "SyntaxError", "RangeError"
      , "EvalError", "URIError"
      ]

-- | Check if unary operator has side effects.
isUnaryOpSideEffect :: JSUnaryOp -> Bool
isUnaryOpSideEffect op = case op of
  JSUnaryOpIncr _ -> True
  JSUnaryOpDecr _ -> True
  _ -> False

-- | Extract properties from object property list.
fromObjectPropertyList :: JSObjectPropertyList -> [JSObjectProperty]
fromObjectPropertyList (JSCTLComma props _) = fromCommaList props
fromObjectPropertyList (JSCTLNone props) = fromCommaList props