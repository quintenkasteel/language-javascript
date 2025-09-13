{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Unit tests for usage analysis in JavaScript tree shaking.
--
-- This module provides comprehensive tests for the usage analysis
-- component of tree shaking, ensuring accurate tracking of identifier
-- usage patterns, scope handling, and dependency analysis.
--
-- Test coverage includes:
--   * Identifier reference tracking
--   * Lexical scope analysis  
--   * Module dependency resolution
--   * Side effect detection
--   * Complex usage patterns
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Process.TreeShake.Usage
  ( testUsageAnalysis,
  )
where

import Control.Lens ((^.))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Parser (parse, parseModule)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import Test.Hspec

-- | Main test suite for usage analysis functionality.
testUsageAnalysis :: Spec
testUsageAnalysis = describe "Usage Analysis Tests" $ do
  testIdentifierTracking
  testScopeAnalysis
  testModuleDependencies
  testSideEffectDetection
  testComplexUsagePatterns

-- | Test identifier reference tracking accuracy.
testIdentifierTracking :: Spec
testIdentifierTracking = describe "Identifier Tracking" $ do
  it "tracks simple variable references" $ do
    let source = "var x = 1; console.log(x);"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Variable x should be marked as used
        case Map.lookup "x" usageMap of
          Just info -> do
            info ^. isUsed `shouldBe` True
            info ^. directReferences `shouldBe` 1
          Nothing -> expectationFailure "Variable 'x' not found in usage map"
        
        -- console should be tracked as used
        case Map.lookup "console" usageMap of
          Just info -> info ^. isUsed `shouldBe` True
          Nothing -> expectationFailure "Variable 'console' not found"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "tracks function parameter usage" $ do
    let source = "function test(a, b) { return a; } test(1, 2);"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Parameter 'a' should be used
        case Map.lookup "a" usageMap of
          Just info -> info ^. isUsed `shouldBe` True
          Nothing -> expectationFailure "Parameter 'a' not tracked"
          
        -- Parameter 'b' should be unused
        case Map.lookup "b" usageMap of
          Just info -> info ^. isUsed `shouldBe` False
          Nothing -> pure ()  -- Might not be tracked if unused
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "tracks member access patterns" $ do
    let source = "var obj = {prop: 1}; console.log(obj.prop);"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Object should be used via member access
        case Map.lookup "obj" usageMap of
          Just info -> do
            info ^. isUsed `shouldBe` True
            info ^. directReferences `shouldSatisfy` (> 0)
          Nothing -> expectationFailure "Object 'obj' not tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "tracks function call usage" $ do
    let source = "function helper() { return 1; } var result = helper();"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Function should be marked as used
        case Map.lookup "helper" usageMap of
          Just info -> do
            info ^. isUsed `shouldBe` True
            info ^. directReferences `shouldBe` 1
          Nothing -> expectationFailure "Function 'helper' not tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "tracks destructuring assignment usage" $ do
    let source = "var obj = {a: 1, b: 2}; var {a, b} = obj; console.log(a);"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Variable 'a' should be used
        case Map.lookup "a" usageMap of
          Just info -> info ^. isUsed `shouldBe` True
          Nothing -> expectationFailure "Destructured 'a' not tracked"
          
        -- Variable 'b' should be unused  
        case Map.lookup "b" usageMap of
          Just info -> info ^. isUsed `shouldBe` False
          Nothing -> pure ()  -- May not track unused destructured vars
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test lexical scope analysis correctness.
testScopeAnalysis :: Spec
testScopeAnalysis = describe "Scope Analysis" $ do
  it "handles function scope correctly" $ do
    let source = "var global = 1; function test() { var local = 2; return global + local; }"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Both variables should be tracked with different scope depths
        case (Map.lookup "global" usageMap, Map.lookup "local" usageMap) of
          (Just globalInfo, Just localInfo) -> do
            globalInfo ^. scopeDepth `shouldBe` 0  -- Global scope
            localInfo ^. scopeDepth `shouldBe` 1   -- Function scope
          _ -> expectationFailure "Variables not properly tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles variable shadowing" $ do
    let source = "var x = 1; function test() { var x = 2; return x; } console.log(x);"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Both x variables should be used
        case Map.lookup "x" usageMap of
          Just info -> info ^. isUsed `shouldBe` True
          Nothing -> expectationFailure "Variable 'x' not tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles block scope (let/const)" $ do
    let source = "var outer = 1; { let inner = 2; console.log(inner); } console.log(outer);"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Both variables should be used with appropriate scoping
        case (Map.lookup "outer" usageMap, Map.lookup "inner" usageMap) of
          (Just outerInfo, Just innerInfo) -> do
            outerInfo ^. isUsed `shouldBe` True
            innerInfo ^. isUsed `shouldBe` True
          _ -> expectationFailure "Block scope variables not tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles closure variable capture" $ do
    let source = "function outer() { var captured = 1; return function() { return captured; }; }"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Captured variable should be marked as used
        case Map.lookup "captured" usageMap of
          Just info -> info ^. isUsed `shouldBe` True
          Nothing -> expectationFailure "Captured variable not tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test module dependency resolution.
testModuleDependencies :: Spec  
testModuleDependencies = describe "Module Dependencies" $ do
  it "analyzes named imports correctly" $ do
    let source = "import {used, unused} from 'module'; console.log(used);"
    case parseModule source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let deps = analysis ^. moduleDependencies
        
        -- Should have one dependency
        length deps `shouldBe` 1
        
        -- Check import analysis
        let moduleInfo = head deps
        moduleInfo ^. moduleName `shouldBe` "module"
        "used" `Set.member` (moduleInfo ^. imports . traverse . importedNames) `shouldBe` True
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "analyzes default imports" $ do
    let source = "import React from 'react'; React.createElement('div');"
    case parseModule source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast  
        let usageMap = analysis ^. usageMap
        
        -- Default import should be used
        case Map.lookup "React" usageMap of
          Just info -> info ^. isUsed `shouldBe` True
          Nothing -> expectationFailure "Default import not tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "analyzes namespace imports" $ do
    let source = "import * as Utils from 'utils'; Utils.helper();"
    case parseModule source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Namespace import should be used
        case Map.lookup "Utils" usageMap of
          Just info -> info ^. isUsed `shouldBe` True  
          Nothing -> expectationFailure "Namespace import not tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "analyzes export declarations" $ do
    let source = "var a = 1, b = 2; export {a, b}; console.log(a);"
    case parseModule source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Both exports should be tracked, but usage differs
        case (Map.lookup "a" usageMap, Map.lookup "b" usageMap) of
          (Just aInfo, Just bInfo) -> do  
            aInfo ^. isUsed `shouldBe` True      -- Used internally
            aInfo ^. isExported `shouldBe` True  -- Also exported
            bInfo ^. isExported `shouldBe` True  -- Exported but unused internally
          _ -> expectationFailure "Export variables not tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test side effect detection accuracy.
testSideEffectDetection :: Spec
testSideEffectDetection = describe "Side Effect Detection" $ do
  it "detects function calls with side effects" $ do
    let source = "var unused = console.log('side effect');"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- console.log should be detected as having side effects
        case Map.lookup "console" usageMap of
          Just info -> info ^. hasSideEffects `shouldBe` True
          Nothing -> expectationFailure "console not tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "detects assignment side effects" $ do
    let source = "var obj = {}; var unused = obj.prop = 42;"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let sideEffectCount = analysis ^. sideEffectCount
        
        -- Should detect assignment as side effect
        sideEffectCount `shouldSatisfy` (> 0)
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "detects constructor side effects" $ do
    let source = "var unused = new Date();"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Constructor calls should have side effects
        case Map.lookup "Date" usageMap of
          Just info -> info ^. hasSideEffects `shouldBe` True
          Nothing -> expectationFailure "Date constructor not tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "identifies pure operations" $ do
    let source = "var unused = Math.abs(-5);"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Math.abs should be considered pure (no side effects)
        case Map.lookup "Math" usageMap of
          Just info -> info ^. hasSideEffects `shouldBe` False
          Nothing -> expectationFailure "Math not tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test complex usage patterns and edge cases.
testComplexUsagePatterns :: Spec
testComplexUsagePatterns = describe "Complex Usage Patterns" $ do
  it "handles conditional usage correctly" $ do
    let source = "var maybe = Math.random() > 0.5 ? used : unused; console.log(maybe);"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Both variables should be considered used due to conditional
        case (Map.lookup "used" usageMap, Map.lookup "unused" usageMap) of
          (Just usedInfo, Just unusedInfo) -> do
            usedInfo ^. isUsed `shouldBe` True
            unusedInfo ^. isUsed `shouldBe` True  -- Potentially used
          _ -> expectationFailure "Conditional variables not tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles dynamic property access" $ do
    let source = "var obj = {prop: 1}; var key = 'prop'; console.log(obj[key]);"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- All involved variables should be used
        case (Map.lookup "obj" usageMap, Map.lookup "key" usageMap) of
          (Just objInfo, Just keyInfo) -> do
            objInfo ^. isUsed `shouldBe` True
            keyInfo ^. isUsed `shouldBe` True
          _ -> expectationFailure "Dynamic access variables not tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles try-catch variable scoping" $ do
    let source = "try { var x = 1; } catch (e) { var y = 2; } console.log(x);"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Variable x should be used, e and y should be unused
        case Map.lookup "x" usageMap of
          Just info -> info ^. isUsed `shouldBe` True
          Nothing -> expectationFailure "Try variable not tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles arrow function parameter usage" $ do
    let source = "var fn = (a, b) => a + 1; fn(1, 2);"
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let usageMap = analysis ^. usageMap
        
        -- Parameter a should be used, b should be unused
        case (Map.lookup "a" usageMap, Map.lookup "b" usageMap) of
          (Just aInfo, maybeB) -> do
            aInfo ^. isUsed `shouldBe` True
            case maybeB of
              Just bInfo -> bInfo ^. isUsed `shouldBe` False
              Nothing -> pure ()  -- Unused params may not be tracked
          _ -> expectationFailure "Arrow function params not tracked"
          
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "calculates usage statistics correctly" $ do
    let source = "var a = 1, b = 2, c = 3; console.log(a, b);"  -- c is unused
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        
        -- Should have correct counts
        analysis ^. totalIdentifiers `shouldSatisfy` (>= 3)
        analysis ^. unusedCount `shouldSatisfy` (>= 1)  -- At least 'c' is unused
        analysis ^. estimatedReduction `shouldSatisfy` (> 0.0)
        analysis ^. estimatedReduction `shouldSatisfy` (< 1.0)
        
      Left err -> expectationFailure $ "Parse failed: " ++ err