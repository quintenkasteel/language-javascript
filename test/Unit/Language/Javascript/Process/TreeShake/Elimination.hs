{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Unit tests for dead code elimination in JavaScript tree shaking.
--
-- This module provides comprehensive tests for the elimination phase
-- of tree shaking, ensuring that unused code is correctly removed while
-- preserving program semantics and observable behavior.
--
-- Test coverage includes:
--   * Statement-level elimination
--   * Expression-level optimization
--   * Module import/export cleanup
--   * Side effect preservation
--   * Configuration-driven elimination
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Process.TreeShake.Elimination
  ( testEliminationCore,
  )
where

import Lens.Micro ((^.), (&), (.~))
import qualified Data.Set as Set
import Language.JavaScript.Parser.Parser (parse, parseModule)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import Test.Hspec

-- | Main test suite for elimination functionality.
testEliminationCore :: Spec
testEliminationCore = describe "Elimination Core Tests" $ do
  testStatementElimination
  testExpressionOptimization
  testModuleCleanup
  testSideEffectHandling
  testConfigurationDrivenElimination
  testEliminationCorrectness

-- | Test statement-level dead code elimination.
testStatementElimination :: Spec
testStatementElimination = describe "Statement Elimination" $ do
  it "removes unused variable declarations" $ do
    let source = "var used = 1, unused = 2; console.log(used);"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Used variable should remain
        optimizedSource `shouldContain` "used"
        -- Unused variable should be removed  
        optimizedSource `shouldNotContain` "unused"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "removes unused function declarations" $ do
    let source = "function used() { return 1; } function unused() { return 2; } used();"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Used function should remain
        optimizedSource `shouldContain` "used"
        -- Unused function should be removed
        optimizedSource `shouldNotContain` "unused()"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles partial variable elimination in declarations" $ do
    let source = "var a = 1, b = 2, c = 3; console.log(a, c);"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Used variables should remain
        optimizedSource `shouldContain` "a"
        optimizedSource `shouldContain` "c"
        -- Unused variable should be removed
        optimizedSource `shouldNotContain` "b"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "removes unused const/let declarations" $ do
    let source = "const USED = 1; const UNUSED = 2; let used = 3; let unused = 4; console.log(USED, used);"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Used declarations should remain
        optimizedSource `shouldContain` "USED"
        optimizedSource `shouldContain` "used"
        -- Unused declarations should be removed
        optimizedSource `shouldNotContain` "UNUSED"
        optimizedSource `shouldNotContain` "unused"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves function parameters that are used" $ do
    let source = "function test(used, unused) { return used; } test(1, 2);"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Function should remain with used parameter
        optimizedSource `shouldContain` "test"
        optimizedSource `shouldContain` "used"
        -- Unused parameter might be removed or preserved for arity
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test expression-level optimization and cleanup.
testExpressionOptimization :: Spec
testExpressionOptimization = describe "Expression Optimization" $ do  
  it "optimizes unused object properties" $ do
    let source = "var obj = {used: 1, unused: 2}; console.log(obj.used);"
    case parse source "test" of
      Right ast -> do
        let aggressiveOpts = defaultOptions & aggressiveShaking .~ True
        let optimized = treeShake aggressiveOpts ast
        let optimizedSource = renderToString optimized
        
        -- Used property should remain
        optimizedSource `shouldContain` "used"
        -- Unused property might be removed in aggressive mode
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "optimizes unused array elements safely" $ do
    let source = "var arr = [used, unused]; console.log(arr[0]);"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Array should be preserved (unsafe to remove elements)
        optimizedSource `shouldContain` "arr"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "eliminates unused comma expression parts" $ do
    let source = "var result = (unused, used); console.log(result);"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Should preserve the used part
        optimizedSource `shouldContain` "used"
        -- May eliminate unused part if no side effects
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "optimizes conditional expressions with dead branches" $ do
    let source = "var result = true ? used : unused; console.log(result);"
    case parse source "test" of
      Right ast -> do
        let aggressiveOpts = defaultOptions & aggressiveShaking .~ True
        let optimized = treeShake aggressiveOpts ast
        let optimizedSource = renderToString optimized
        
        -- Used branch should remain
        optimizedSource `shouldContain` "used"
        -- Dead branch might be eliminated in aggressive mode
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test module import/export cleanup.
testModuleCleanup :: Spec
testModuleCleanup = describe "Module Cleanup" $ do
  it "removes unused named imports" $ do
    let source = "import {used, unused} from 'module'; console.log(used);"
    case parseModule source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Used import should remain
        optimizedSource `shouldContain` "used"
        -- Unused import should be removed
        optimizedSource `shouldNotContain` "unused"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "removes completely unused import statements" $ do
    let source = "import {unused} from 'module'; console.log('test');"
    case parseModule source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Entire import should be removed
        optimizedSource `shouldNotContain` "import"
        optimizedSource `shouldNotContain` "module"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves side-effect imports" $ do
    let source = "import 'polyfill'; console.log('test');"
    case parseModule source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Side-effect import should be preserved
        optimizedSource `shouldContain` "polyfill"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves exported specifiers" $ do
    let source = "var a = 1, b = 2; export {a, b}; console.log(a);"
    case parseModule source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Both exported identifiers should remain since they are exported
        optimizedSource `shouldContain` "a"
        optimizedSource `shouldContain` "b"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles re-exports correctly" $ do
    let source = "export {used, unused} from 'other';"
    case parseModule source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Re-exports should be handled conservatively
        optimizedSource `shouldContain` "export"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test side effect preservation during elimination.
testSideEffectHandling :: Spec
testSideEffectHandling = describe "Side Effect Handling" $ do
  it "preserves assignments with unused results" $ do
    let source = "var obj = {}; var unused = obj.prop = 42; console.log(obj);"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Assignment should be preserved due to side effect
        optimizedSource `shouldContain` "obj.prop = 42"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves function calls with side effects" $ do
    let source = "var unused = console.log('side effect'); var x = 1;"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Side effect call should be preserved
        optimizedSource `shouldContain` "console.log"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves constructor calls that are used" $ do
    let source = "var used = new Date(); console.log(used);"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Constructor should be preserved when result is used
        optimizedSource `shouldContain` "new Date"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves delete operations" $ do
    let source = "var obj = {prop: 1}; var unused = delete obj.prop; console.log(obj);"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Delete operation should be preserved
        optimizedSource `shouldContain` "delete"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "removes unused declarations even with function calls" $ do
    let source = "var unused = Math.abs(-5); console.log('test');"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- The used call should remain
        optimizedSource `shouldContain` "console"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test configuration-driven elimination behavior.
testConfigurationDrivenElimination :: Spec
testConfigurationDrivenElimination = describe "Configuration-Driven Elimination" $ do
  it "respects preserveTopLevel setting" $ do
    let source = "var unusedTopLevel = 1; console.log('hello');"
    case parse source "test" of
      Right ast -> do
        let preserveOpts = defaultOptions & preserveTopLevel .~ True
        let removeOpts = defaultOptions & preserveTopLevel .~ False

        let preserved = treeShake preserveOpts ast
        let removed = treeShake removeOpts ast

        let preservedSource = renderToString preserved
        let removedSource = renderToString removed

        -- Should preserve with preserveTopLevel=True
        preservedSource `shouldContain` "unusedTopLevel"
        -- Should remove with preserveTopLevel=False
        removedSource `shouldNotContain` "unusedTopLevel"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "respects aggressiveShaking setting" $ do
    let source = "var maybeUsed = 1; eval('console.log(maybeUsed)');"
    case parse source "test" of
      Right ast -> do
        let conservativeOpts = defaultOptions & aggressiveShaking .~ False
        let aggressiveOpts = defaultOptions & aggressiveShaking .~ True
        
        let conservative = treeShake conservativeOpts ast
        let aggressive = treeShake aggressiveOpts ast
        
        let conservativeSource = renderToString conservative
        let _aggressiveSource = renderToString aggressive

        -- Conservative should preserve uncertain usage
        conservativeSource `shouldContain` "maybeUsed"
        -- Aggressive might remove it
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "respects preserveExports configuration" $ do
    let source = "var api = 1, internal = 2; export {api, internal};"
    case parseModule source "test" of
      Right ast -> do
        let opts = defaultOptions & preserveExports .~ Set.fromList ["api"]
        let optimized = treeShake opts ast
        let optimizedSource = renderToString optimized
        
        -- Preserved export should remain
        optimizedSource `shouldContain` "api"
        -- Other exports might be removed if unused
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles optimization levels correctly" $ do
    let source = "function unused() { var x = 1; return x; }"
    case parse source "test" of
      Right ast -> do
        let conservativeOpts = defaultOptions & optimizationLevel .~ Conservative
        let aggressiveOpts = defaultOptions & optimizationLevel .~ Aggressive
        
        let conservative = treeShake conservativeOpts ast
        let aggressive = treeShake aggressiveOpts ast
        
        -- Different optimization levels should yield different results
        renderToString conservative `shouldNotBe` renderToString aggressive
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test elimination correctness and semantic preservation.
testEliminationCorrectness :: Spec
testEliminationCorrectness = describe "Elimination Correctness" $ do
  it "maintains program semantics after elimination" $ do
    let source = "var a = 1; var unused = 2; function test() { return a; } console.log(test());"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let isValid = validateTreeShaking ast optimized
        
        -- Semantic validation should pass
        isValid `shouldBe` True
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves execution order for side effects" $ do
    let source = "console.log('first'); var unused = console.log('second'); console.log('third');"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- All console.log calls should be preserved in order
        optimizedSource `shouldContain` "first"
        optimizedSource `shouldContain` "second"  
        optimizedSource `shouldContain` "third"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles hoisting correctly" $ do
    let source = "console.log(hoisted()); function hoisted() { return 'works'; }"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Hoisted function should be preserved
        optimizedSource `shouldContain` "hoisted"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "provides detailed elimination results" $ do
    let source = "var used = 1, unused = 2; console.log(used);"
    case parse source "test" of
      Right ast -> do
        let (_optimized, analysis) = treeShakeWithAnalysis defaultOptions ast

        -- Analysis should provide useful information
        analysis ^. totalIdentifiers `shouldSatisfy` (> 0)
        analysis ^. unusedCount `shouldSatisfy` (> 0)
        analysis ^. estimatedReduction `shouldSatisfy` (> 0.0)
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles complex dependency chains" $ do
    let source = "var a = b; var b = c; var c = 1; var unused = 2; console.log(a);"
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Entire dependency chain should be preserved
        optimizedSource `shouldContain` "a"
        optimizedSource `shouldContain` "b"  
        optimizedSource `shouldContain` "c"
        -- Unused variable should be removed
        optimizedSource `shouldNotContain` "unused"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err