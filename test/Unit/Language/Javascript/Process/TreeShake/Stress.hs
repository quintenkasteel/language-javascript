{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Stress and performance tests for JavaScript tree shaking functionality.
--
-- This module provides stress testing and performance validation for the tree
-- shaking implementation, pushing the boundaries with large codebases, complex
-- dependency graphs, and edge cases that test scalability and robustness.
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Process.TreeShake.Stress
  ( testTreeShakeStress,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import Control.Lens ((.~), (&))
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import Test.Hspec

-- | Main test suite for tree shaking stress testing.
testTreeShakeStress :: Spec
testTreeShakeStress = describe "TreeShake Stress Tests" $ do
  testLargeScaleCodebases
  testComplexDependencyGraphs
  testPathologicalCases

-- | Test large-scale codebase simulation.
testLargeScaleCodebases :: Spec
testLargeScaleCodebases = describe "Large-Scale Codebase Simulation" $ do
  it "handles 1000 variable eliminations efficiently" $ do
    let generateLargeCodebase numVars =
          let usedVars = ["var used" ++ show i ++ " = " ++ show i ++ ";" | i <- [1..5]]
              unusedVars = ["var unused" ++ show i ++ " = " ++ show i ++ ";" | i <- [1..numVars]]
              usage = ["console.log(used1 + used2 + used3 + used4 + used5);"]
          in unlines (usedVars ++ unusedVars ++ usage)
    let source = generateLargeCodebase 1000
    case parse source "stress-test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve only the used variables
        astShouldContainIdentifier optimized "used1"
        astShouldContainIdentifier optimized "used5"
        -- Should eliminate unused variables (spot check)
        astShouldNotContainIdentifier optimized "unused1"
        astShouldNotContainIdentifier optimized "unused500"
        astShouldNotContainIdentifier optimized "unused1000"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles 100 function eliminations with dependencies" $ do
    let generateFunctionChain chainLength =
          let functions = ["function func" ++ show i ++ "() { return func" ++ show (i+1) ++ "(); }" | i <- [1..chainLength]]
              lastFunction = "function func" ++ show (chainLength + 1) ++ "() { return 42; }"
              unusedFunctions = ["function unused" ++ show i ++ "() { return 'unused'; }" | i <- [1..50]]
              usage = ["console.log(func1());"]
          in unlines (functions ++ [lastFunction] ++ unusedFunctions ++ usage)
    let source = generateFunctionChain 100
    case parse source "stress-test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve entire chain
        astShouldContainIdentifier optimized "func1"
        astShouldContainIdentifier optimized "func50"
        astShouldContainIdentifier optimized "func101"
        -- Should eliminate unused functions
        astShouldNotContainIdentifier optimized "unused1"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test complex dependency graphs.
testComplexDependencyGraphs :: Spec
testComplexDependencyGraphs = describe "Complex Dependency Graphs" $ do
  it "handles star dependency patterns (one function calls many)" $ do
    let generateStarPattern numDependencies =
          let dependencies = ["function dep" ++ show i ++ "() { return " ++ show i ++ "; }" | i <- [1..numDependencies]]
              central = ["function central() { var sum = 0;"] ++
                       ["  sum += dep" ++ show i ++ "();" | i <- [1..numDependencies]] ++
                       ["  return sum; }"]
              unused = ["function unused" ++ show i ++ "() { return 'unused'; }" | i <- [1..20]]
              usage = ["console.log(central());"]
          in unlines (dependencies ++ central ++ unused ++ usage)
    let source = generateStarPattern 50  -- Central function calls 50 deps
    case parse source "stress-test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve central and all dependencies
        astShouldContainIdentifier optimized "central"
        astShouldContainIdentifier optimized "dep1"
        astShouldContainIdentifier optimized "dep25"
        astShouldContainIdentifier optimized "dep50"
        -- Should eliminate unused functions
        astShouldNotContainIdentifier optimized "unused1"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles binary tree dependency patterns" $ do
    let generateBinaryTree depth =
          let nodes = ["function node" ++ show level ++ "_" ++ show pos ++ "() { " ++
                      (if level == depth
                       then "return " ++ show (level * 10 + pos) ++ ";"
                       else "return node" ++ show (level+1) ++ "_" ++ show (pos*2) ++ "() + " ++
                            "node" ++ show (level+1) ++ "_" ++ show (pos*2+1) ++ "();") ++
                      " }"
                      | level <- [0..depth], pos <- [0..(2^level - 1)]]
              usage = ["console.log(node0_0());"]
          in unlines (nodes ++ usage)
    let source = generateBinaryTree 6  -- 6 levels = 127 nodes
    case parse source "stress-test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve entire reachable tree
        astShouldContainIdentifier optimized "node0_0"
        astShouldContainIdentifier optimized "node6_0"  -- Leaf level
        astShouldContainIdentifier optimized "node6_63"  -- Last leaf
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test pathological cases.
testPathologicalCases :: Spec
testPathologicalCases = describe "Pathological Cases" $ do
  it "handles deeply nested scopes (50 levels)" $ do
    let generateNestedScopes depth =
          let openBraces = replicate depth "{"
              closeBraces = replicate depth "}"
              varDecls = ["var nested" ++ show i ++ " = " ++ show i ++ ";" | i <- [1..depth]]
              usage = ["console.log(nested" ++ show depth ++ ");"]
          in unlines (["function deeplyNested() {"] ++
                     openBraces ++ varDecls ++ usage ++ closeBraces ++
                     ["}"] ++ ["deeplyNested();"])
    let source = generateNestedScopes 50
    case parse source "stress-test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve the function and deeply nested variable
        astShouldContainIdentifier optimized "deeplyNested"
        astShouldContainIdentifier optimized "nested50"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles many eval statements with conservative mode" $ do
    let generateManyEvals numEvals =
          let evals = ["eval('var dynamic" ++ show i ++ " = " ++ show i ++ ";');" | i <- [1..numEvals]]
              vars = ["var static" ++ show i ++ " = " ++ show i ++ ";" | i <- [1..numEvals]]
              usage = ["console.log('done');"]
          in unlines (evals ++ vars ++ usage)
    let source = generateManyEvals 50  -- 50 eval statements + 50 static vars
    case parse source "stress-test" of
      Right ast -> do
        let opts = defaultOptions & aggressiveShaking .~ False  -- Conservative
        let optimized = treeShake opts ast
        -- In conservative mode, should preserve static variables due to eval presence
        astShouldContainIdentifier optimized "static1"
        astShouldContainIdentifier optimized "static25"
        astShouldContainIdentifier optimized "static50"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles enterprise-scale mixed patterns" $ do
    let generateEnterpriseMix =
          let modules = ["var Module" ++ show i ++ " = { init: function() { return " ++
                        (if i < 10 then "Module" ++ show (i+1) ++ ".init()" else "'done'") ++
                        "; } };" | i <- [1..10]]
              classes = ["class Component" ++ show i ++ " { constructor() { this.id = " ++
                        show i ++ "; } }" | i <- [1..20]]
              utilities = ["function util" ++ show i ++ "() { return Math.random(); }" | i <- [1..100]]
              usage = ["var app = Module1.init(); console.log(app);"]
          in unlines (modules ++ classes ++ utilities ++ usage)
    let source = generateEnterpriseMix
    case parse source "stress-test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve module chain
        astShouldContainIdentifier optimized "Module1"
        astShouldContainIdentifier optimized "Module10"
        astShouldContainIdentifier optimized "app"
        -- Should eliminate unused utilities and classes
        astShouldNotContainIdentifier optimized "util1"
        astShouldNotContainIdentifier optimized "Component1"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- Helper functions (simplified versions)

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
  _ -> False  -- Simplified for stress tests

-- | Check if statement contains identifier.
statementContainsIdentifier :: ByteString -> JSStatement -> Bool
statementContainsIdentifier identifier stmt = case stmt of
  JSFunction _ ident _ _ _ body _ ->
    identifierMatches identifier ident || blockContainsIdentifier identifier body
  JSVariable _ decls _ ->
    any (expressionContainsIdentifier identifier) (fromCommaList decls)
  JSClass _ ident _ _ _ _ _ ->
    identifierMatches identifier ident
  JSExpressionStatement expr _ ->
    expressionContainsIdentifier identifier expr
  JSStatementBlock _ stmts _ _ ->
    any (statementContainsIdentifier identifier) stmts
  _ -> False  -- Simplified

-- | Check if statement block contains identifier.
blockContainsIdentifier :: ByteString -> JSBlock -> Bool
blockContainsIdentifier identifier (JSBlock _ stmts _) =
  any (statementContainsIdentifier identifier) stmts

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
  JSMemberDot obj _ _ ->
    expressionContainsIdentifier identifier obj
  JSMemberSquare obj _ idx _ ->
    expressionContainsIdentifier identifier obj ||
    expressionContainsIdentifier identifier idx
  JSAssignExpression lhs _ rhs ->
    expressionContainsIdentifier identifier lhs ||
    expressionContainsIdentifier identifier rhs
  _ -> False  -- Simplified

-- | Check if variable initializer contains identifier.
initializerContainsIdentifier :: ByteString -> JSVarInitializer -> Bool
initializerContainsIdentifier identifier initializer = case initializer of
  JSVarInit _ expr -> expressionContainsIdentifier identifier expr
  JSVarInitNone -> False

-- | Check if JSIdent matches identifier.
identifierMatches :: ByteString -> JSIdent -> Bool
identifierMatches identifier (JSIdentName _ name) = name == identifier
identifierMatches _ JSIdentNone = False

