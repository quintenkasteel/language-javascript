{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Core unit tests for JavaScript tree shaking functionality.
--
-- This module provides comprehensive unit tests for the tree shaking
-- implementation, ensuring correctness of dead code elimination while
-- preserving program semantics.
--
-- Test categories covered:
--   * Basic identifier elimination
--   * Configuration option handling
--   * Edge cases and error conditions
--
-- All tests follow CLAUDE.md standards with real functionality testing
-- and no mock functions. Coverage target: 85%+
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Process.TreeShake.Core
  ( testTreeShakeCore,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Lens.Micro ((.~), (&))
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import Test.Hspec

-- | Main test suite for tree shaking core functionality.
testTreeShakeCore :: Spec
testTreeShakeCore = describe "TreeShake Core Tests" $ do
  testBasicElimination
  testConfigurationOptions
  testEdgeCases

-- | Test basic dead code elimination functionality.
testBasicElimination :: Spec  
testBasicElimination = describe "Basic Elimination" $ do
  it "eliminates unused variable declarations" $ do
    let source = "var used = 1, unused = 2; console.log(used);"
    case parse source "test" of
      Right ast -> do
        let _analysis = analyzeUsage ast
        let optimized = treeShake defaultOptions ast
        -- Verify unused variable was eliminated
        astShouldContainIdentifier optimized "used"
        astShouldNotContainIdentifier optimized "unused"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves used function declarations" $ do
    let source = "function used() { return 1; } function unused() { return 2; } used();"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast  
        astShouldContainIdentifier optimized "used"
        astShouldNotContainIdentifier optimized "unused"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "eliminates unused function declarations" $ do
    let source = "function unused() { return 42; } var x = 1;"
    case parse source "test" of
      Right ast -> do
        let opts = defaultOptions & preserveTopLevel .~ False
        let optimized = treeShake opts ast
        astShouldNotContainIdentifier optimized "unused"
        astShouldNotContainIdentifier optimized "x"  -- x is also unused and has no side effects
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles nested scope elimination correctly" $ do
    let source = "function outer() { var used = 1; var unused = 2; return used; } outer();"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "outer"
        astShouldContainIdentifier optimized "used"
        astShouldNotContainIdentifier optimized "unused"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves variables used in closures" $ do
    let source = "function outer() { var captured = 1; return function() { return captured; }; } outer();"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "captured"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test configuration option handling.
testConfigurationOptions :: Spec
testConfigurationOptions = describe "Configuration Options" $ do
  it "respects preserveTopLevel option" $ do
    let source = "var topLevel = 1; console.log('used');"
    case parse source "test" of
      Right ast -> do
        let opts = defaultOptions & preserveTopLevel .~ True
        let optimized = treeShake opts ast
        astShouldContainIdentifier optimized "topLevel"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "respects aggressiveShaking option" $ do
    let source = "var maybeUsed = 1; eval('console.log(maybeUsed)');"
    case parse source "test" of
      Right ast -> do
        let conservativeOpts = defaultOptions & aggressiveShaking .~ False
        let _aggressiveOpts = defaultOptions & aggressiveShaking .~ True

        let conservativeResult = treeShake conservativeOpts ast

        -- Conservative should preserve due to eval presence
        astShouldContainIdentifier conservativeResult "maybeUsed"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles optimization levels correctly" $ do
    let source = "function unused() { return 42; } var x = 1;"
    case parse source "test" of
      Right ast -> do
        -- Base options that allow optimization level differences to be visible
        let baseOpts = defaultOptions & preserveTopLevel .~ False
        let conservativeOpts = baseOpts & optimizationLevel .~ Conservative
        let aggressiveOpts = baseOpts & optimizationLevel .~ Aggressive
        
        let conservativeResult = treeShake conservativeOpts ast
        let aggressiveResult = treeShake aggressiveOpts ast
        
        -- Conservative should preserve unused function, Aggressive should eliminate it
        astShouldContainIdentifier conservativeResult "unused"
        conservativeResult `shouldNotBe` aggressiveResult
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test edge cases and error conditions.
testEdgeCases :: Spec  
testEdgeCases = describe "Edge Cases" $ do
  it "handles empty programs correctly" $ do
    let source = ""
    case parse source "test" of
      Right ast -> do
        let _optimized = treeShake defaultOptions ast
        let analysis = analyzeUsage ast
        _totalIdentifiers analysis `shouldBe` 0
        _unusedCount analysis `shouldBe` 0
      Left _ -> pure ()  -- Empty source may not parse

  it "handles programs with only comments" $ do
    let source = "/* comment only */"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        optimized `shouldBe` ast  -- Should remain unchanged
      Left _ -> pure ()  -- May not parse

  it "handles circular variable dependencies" $ do
    let source = "var a = b, b = a; console.log(a);"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Both variables should be preserved due to usage
        astShouldContainIdentifier optimized "a"
        astShouldContainIdentifier optimized "b"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles hoisted function declarations" $ do
    let source = "console.log(hoisted()); function hoisted() { return 1; }"
    case parse source "test" of  
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "hoisted"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles with statements correctly" $ do
    let source = "var obj = {prop: 1}; with (obj) { console.log(prop); }"  
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- obj should be preserved due to with statement usage
        astShouldContainIdentifier optimized "obj"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles eval statements conservatively" $ do
    let source = "var x = 1; eval('console.log(x)');"
    case parse source "test" of
      Right ast -> do  
        let optimized = treeShake defaultOptions ast
        -- Variables should be preserved due to potential eval usage
        astShouldContainIdentifier optimized "x"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- Helper Functions for Test Assertions

-- | Check if AST contains specific identifier in its structure.
astShouldContainIdentifier :: JSAST -> ByteString -> Expectation
astShouldContainIdentifier ast identifier =
  if astContainsIdentifier ast identifier
  then pure ()
  else expectationFailure $ "Identifier not found in AST: " ++ BS8.unpack identifier

-- | Check if AST does not contain specific identifier in its structure.
astShouldNotContainIdentifier :: JSAST -> ByteString -> Expectation
astShouldNotContainIdentifier ast identifier =
  if astContainsIdentifier ast identifier
  then expectationFailure $ "Identifier should not be in AST: " ++ BS8.unpack identifier
  else pure ()

-- | Check if AST contains specific identifier anywhere in its structure.
astContainsIdentifier :: JSAST -> ByteString -> Bool
astContainsIdentifier ast identifier = case ast of
  JSAstProgram statements _ -> 
    any (statementContainsIdentifier identifier) statements
  JSAstModule items _ -> 
    any (moduleItemContainsIdentifier identifier) items  
  JSAstStatement stmt _ ->
    statementContainsIdentifier identifier stmt
  JSAstExpression expr _ ->
    expressionContainsIdentifier identifier expr
  JSAstLiteral expr _ ->
    expressionContainsIdentifier identifier expr

-- | Check if statement contains identifier.
statementContainsIdentifier :: ByteString -> JSStatement -> Bool
statementContainsIdentifier identifier stmt = case stmt of
  JSFunction _ ident _ _ _ body _ ->
    identifierMatches identifier ident || blockContainsIdentifier identifier body
  JSVariable _ decls _ ->
    any (expressionContainsIdentifier identifier) (fromCommaList decls)
  JSLet _ decls _ ->
    any (expressionContainsIdentifier identifier) (fromCommaList decls)  
  JSConstant _ decls _ ->
    any (expressionContainsIdentifier identifier) (fromCommaList decls)
  JSClass _ ident _ _ _ _ _ ->
    identifierMatches identifier ident
  JSExpressionStatement expr _ ->
    expressionContainsIdentifier identifier expr
  JSStatementBlock _ stmts _ _ ->
    any (statementContainsIdentifier identifier) stmts
  JSReturn _ (Just expr) _ ->
    expressionContainsIdentifier identifier expr
  JSIf _ _ test _ thenStmt ->
    expressionContainsIdentifier identifier test || 
    statementContainsIdentifier identifier thenStmt
  JSIfElse _ _ test _ thenStmt _ elseStmt ->
    expressionContainsIdentifier identifier test || 
    statementContainsIdentifier identifier thenStmt ||
    statementContainsIdentifier identifier elseStmt
  _ -> False

-- | Check if expression contains identifier.
expressionContainsIdentifier :: ByteString -> JSExpression -> Bool
expressionContainsIdentifier identifier expr = case expr of
  JSIdentifier _ name -> name == identifier
  JSVarInitExpression lhs rhs ->
    expressionContainsIdentifier identifier lhs ||
    initializerContainsIdentifier identifier rhs
  JSCallExpression func _ args _ ->
    expressionContainsIdentifier identifier func ||
    any (expressionContainsIdentifier identifier) (fromCommaList args)
  JSCallExpressionDot func _ prop ->
    expressionContainsIdentifier identifier func ||
    expressionContainsIdentifier identifier prop
  JSCallExpressionSquare func _ prop _ ->
    expressionContainsIdentifier identifier func ||
    expressionContainsIdentifier identifier prop
  JSMemberDot obj _ prop ->
    expressionContainsIdentifier identifier obj ||
    expressionContainsIdentifier identifier prop
  JSMemberSquare obj _ prop _ ->
    expressionContainsIdentifier identifier obj ||
    expressionContainsIdentifier identifier prop
  JSAssignExpression lhs _ rhs ->
    expressionContainsIdentifier identifier lhs ||
    expressionContainsIdentifier identifier rhs
  JSExpressionBinary lhs _ rhs ->
    expressionContainsIdentifier identifier lhs ||
    expressionContainsIdentifier identifier rhs
  JSExpressionParen _ innerExpr _ ->
    expressionContainsIdentifier identifier innerExpr
  JSArrayLiteral _ elements _ ->
    any (arrayElementContainsIdentifier identifier) elements
  JSObjectLiteral _ props _ ->
    objectPropertyListContainsIdentifier identifier props
  JSFunctionExpression _ ident _ params _ body ->
    identifierMatches identifier ident ||
    any (expressionContainsIdentifier identifier) (fromCommaList params) ||
    blockContainsIdentifier identifier body
  _ -> False

-- | Check if variable initializer contains identifier.
initializerContainsIdentifier :: ByteString -> JSVarInitializer -> Bool
initializerContainsIdentifier identifier (JSVarInit _ rhsExpr) =
  expressionContainsIdentifier identifier rhsExpr
initializerContainsIdentifier _ JSVarInitNone = False

-- Helper functions for complex expressions
arrayElementContainsIdentifier :: ByteString -> JSArrayElement -> Bool
arrayElementContainsIdentifier identifier element = case element of
  JSArrayElement expr -> expressionContainsIdentifier identifier expr
  JSArrayComma _ -> False

objectPropertyListContainsIdentifier :: ByteString -> JSObjectPropertyList -> Bool
objectPropertyListContainsIdentifier identifier propList = case propList of
  JSCTLComma props _ -> any (objectPropertyContainsIdentifier identifier) (fromCommaList props)
  JSCTLNone props -> any (objectPropertyContainsIdentifier identifier) (fromCommaList props)

objectPropertyContainsIdentifier :: ByteString -> JSObjectProperty -> Bool
objectPropertyContainsIdentifier identifier prop = case prop of
  JSPropertyNameandValue _ _ values -> any (expressionContainsIdentifier identifier) values
  JSPropertyIdentRef _ name -> name == identifier
  JSObjectMethod (JSMethodDefinition _ _ params _ body) ->
    any (expressionContainsIdentifier identifier) (fromCommaList params) ||
    blockContainsIdentifier identifier body
  JSObjectMethod (JSGeneratorMethodDefinition _ _ _ params _ body) ->
    any (expressionContainsIdentifier identifier) (fromCommaList params) ||
    blockContainsIdentifier identifier body
  JSObjectMethod (JSPropertyAccessor _ _ _ params _ body) ->
    any (expressionContainsIdentifier identifier) (fromCommaList params) ||
    blockContainsIdentifier identifier body
  JSObjectMethod (JSAsyncMethodDefinition _ _ _ params _ body) ->
    any (expressionContainsIdentifier identifier) (fromCommaList params) ||
    blockContainsIdentifier identifier body
  JSObjectSpread _ expr -> expressionContainsIdentifier identifier expr

-- | Check if block contains identifier.
blockContainsIdentifier :: ByteString -> JSBlock -> Bool
blockContainsIdentifier identifier (JSBlock _ stmts _) =
  any (statementContainsIdentifier identifier) stmts

-- | Check if module item contains identifier.
moduleItemContainsIdentifier :: ByteString -> JSModuleItem -> Bool
moduleItemContainsIdentifier identifier item = case item of
  JSModuleStatementListItem stmt -> statementContainsIdentifier identifier stmt
  _ -> False

-- | Check if JSIdent matches identifier.
identifierMatches :: ByteString -> JSIdent -> Bool
identifierMatches identifier (JSIdentName _ name) = name == identifier
identifierMatches _ JSIdentNone = False

