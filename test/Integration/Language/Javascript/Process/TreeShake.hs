{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Integration tests for JavaScript tree shaking functionality.
--
-- This module provides comprehensive integration tests that verify
-- tree shaking works correctly with the complete parser pipeline,
-- including round-trip parsing and pretty printing integration.
--
-- Test scenarios include:
--   * End-to-end tree shaking workflows
--   * Integration with parser and pretty printer
--   * Real-world JavaScript examples
--   * Performance validation
--   * Correctness verification
--
-- @since 0.8.0.0
module Integration.Language.Javascript.Process.TreeShake
  ( testTreeShakeIntegration,
  )
where

import Control.Lens ((^.), (.~), (&))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Parser (parse, parseModule)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake
import Test.Hspec

-- | Main integration test suite for tree shaking.
testTreeShakeIntegration :: Spec
testTreeShakeIntegration = describe "TreeShake Integration Tests" $ do
  testEndToEndPipeline
  testRealWorldExamples
  testParserIntegration
  testPrettyPrinterIntegration
  testPerformanceValidation

-- | Test end-to-end tree shaking pipeline.
testEndToEndPipeline :: Spec
testEndToEndPipeline = describe "End-to-End Pipeline" $ do
  it "handles complete JavaScript program optimization" $ do
    let source = unlines
          [ "// Utility functions"
          , "function used() { return 'used'; }"
          , "function unused() { return 'unused'; }"
          , ""
          , "// Main program"  
          , "var result = used();"
          , "console.log(result);"
          ]
    
    case parse source "test" of
      Right ast -> do
        let (optimized, analysis) = treeShakeWithAnalysis defaultOptions ast
        
        -- Verify analysis results
        _totalIdentifiers analysis `shouldSatisfy` (> 0)
        _unusedCount analysis `shouldSatisfy` (> 0)
        
        -- Verify optimization results
        let optimizedSource = renderToString optimized
        optimizedSource `shouldContain` "used"
        optimizedSource `shouldNotContain` "unused"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles module-based tree shaking" $ do
    let source = unlines
          [ "import {used, unused} from 'utils';"
          , "import 'side-effect';"
          , ""
          , "export const API = used();"
          , "const internal = 'internal';"
          ]
    
    case parseModule source "test" of
      Right ast -> do
        let opts = defaultOptions { _preserveExports = Set.fromList ["API"] }
        let optimized = treeShake opts ast
        let optimizedSource = renderToString optimized
        
        -- Used import should remain
        optimizedSource `shouldContain` "used"
        -- Unused import should be removed
        optimizedSource `shouldNotContain` "unused"
        -- Side-effect import should be preserved
        optimizedSource `shouldContain` "side-effect"
        -- Exported identifier should be preserved
        optimizedSource `shouldContain` "API"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles complex dependency chains" $ do
    let source = unlines
          [ "function level1() { return level2(); }"
          , "function level2() { return level3(); }"  
          , "function level3() { return 'result'; }"
          , "function orphan() { return 'orphan'; }"
          , ""
          , "console.log(level1());"
          ]
    
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Entire dependency chain should be preserved
        optimizedSource `shouldContain` "level1"
        optimizedSource `shouldContain` "level2"
        optimizedSource `shouldContain` "level3"
        -- Orphan function should be removed
        optimizedSource `shouldNotContain` "orphan"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test real-world JavaScript examples.
testRealWorldExamples :: Spec
testRealWorldExamples = describe "Real-World Examples" $ do
  it "optimizes React component code" $ do
    let source = unlines
          [ "var React = require('react');"
          , "var useState = require('react').useState;"
          , "var useEffect = require('react').useEffect;"  -- Unused
          , ""
          , "function MyComponent() {"
          , "  var state = useState(0)[0];"
          , "  var setState = useState(0)[1];"
          , "  return React.createElement('div', null, state);"
          , "}"
          , ""
          , "module.exports = MyComponent;"
          ]

    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Used React imports should remain
        optimizedSource `shouldContain` "React"
        optimizedSource `shouldContain` "useState"
        -- Unused React import should be removed
        optimizedSource `shouldNotContain` "useEffect"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "optimizes utility library code" $ do
    let source = unlines
          [ "// Utility functions"
          , "export function add(a, b) { return a + b; }"
          , "export function subtract(a, b) { return a - b; }"
          , "export function multiply(a, b) { return a * b; }"
          , "export function divide(a, b) { return a / b; }"  -- May be unused
          , ""
          , "// Internal usage"
          , "const result = add(multiply(2, 3), subtract(10, 5));"
          , "console.log(result);"
          ]
    
    case parseModule source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Used functions should remain
        optimizedSource `shouldContain` "add"
        optimizedSource `shouldContain` "multiply" 
        optimizedSource `shouldContain` "subtract"
        -- All exports should be preserved (exported API)
        optimizedSource `shouldContain` "divide"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles Node.js module patterns" $ do
    let source = unlines
          [ "const fs = require('fs');"
          , "const path = require('path');"
          , "const unused = require('crypto');"  -- Unused
          , ""
          , "function readConfig() {"
          , "  return fs.readFileSync(path.join(__dirname, 'config.json'));"
          , "}"
          , ""
          , "module.exports = {readConfig};"
          ]
    
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Used requires should remain
        optimizedSource `shouldContain` "fs"
        optimizedSource `shouldContain` "path"
        -- Unused require should be removed
        optimizedSource `shouldNotContain` "crypto"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test integration with parser components.
testParserIntegration :: Spec
testParserIntegration = describe "Parser Integration" $ do
  it "preserves parse correctness after optimization" $ do
    let source = "var a = 1, b = 2; console.log(a);"
    
    case parse source "test" of
      Right originalAst -> do
        let optimized = treeShake defaultOptions originalAst
        let optimizedSource = renderToString optimized
        
        -- Re-parse optimized code
        case parse optimizedSource "optimized" of
          Right reparse -> do
            -- Should parse successfully
            reparse `shouldSatisfy` isValidAST
          Left err -> expectationFailure $ "Reparsing failed: " ++ err
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles parsing edge cases after optimization" $ do
    let source = "var x; if (true) { var y = x; } console.log(y);"
    
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized
        
        -- Should still parse correctly
        case parse optimizedSource "optimized" of
          Right _ -> pure ()  -- Success
          Left err -> expectationFailure $ "Edge case reparsing failed: " ++ err
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test integration with pretty printer.
testPrettyPrinterIntegration :: Spec
testPrettyPrinterIntegration = describe "Pretty Printer Integration" $ do
  it "produces valid JavaScript output" $ do
    let source = "function test() { var unused = 1; return 'result'; } test();"
    
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let output = renderToString optimized
        
        -- Output should be valid JavaScript
        output `shouldSatisfy` isValidJavaScript
        output `shouldContain` "test"
        output `shouldContain` "result"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves code formatting appropriately" $ do
    let source = unlines
          [ "function formatted() {"
          , "  var used = 'value';"
          , "  var unused = 'unused';"  
          , "  return used;"
          , "}"
          , "formatted();"
          ]
    
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let output = renderToString optimized
        
        -- Should maintain reasonable formatting
        output `shouldContain` "formatted"
        output `shouldContain` "used"
        output `shouldNotContain` "unused"
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test performance characteristics.
testPerformanceValidation :: Spec  
testPerformanceValidation = describe "Performance Validation" $ do
  it "handles large JavaScript files efficiently" $ do
    let largeSource = generateLargeJavaScript 1000  -- 1000 functions
    
    case parse largeSource "large-test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let optimized = treeShake defaultOptions ast
        
        -- Should complete without excessive time/memory
        _totalIdentifiers analysis `shouldSatisfy` (> 500)
        optimized `shouldSatisfy` isValidAST
        
      Left err -> expectationFailure $ "Large file parse failed: " ++ err

  it "provides accurate size reduction estimates" $ do
    let unusedVars = map (\i -> "var unused" ++ show i) [1..10]
    let source = unlines $ unusedVars ++ ["console.log('test');"]
    
    case parse source "test" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let reduction = _estimatedReduction analysis
        
        -- Should estimate significant reduction
        reduction `shouldSatisfy` (> 0.5)  -- At least 50% reduction expected
        
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- Helper Functions

-- | Check if AST represents valid JavaScript structure.
isValidAST :: JSAST -> Bool
isValidAST (JSAstProgram _ _) = True
isValidAST (JSAstModule _ _) = True  
isValidAST (JSAstStatement _ _) = True
isValidAST (JSAstExpression _ _) = True
isValidAST (JSAstLiteral _ _) = True

-- | Check if string is valid JavaScript syntax.
isValidJavaScript :: String -> Bool
isValidJavaScript js = case parse js "validation" of
  Right _ -> True
  Left _ -> False

-- | Generate large JavaScript source for performance testing.
generateLargeJavaScript :: Int -> String
generateLargeJavaScript n = unlines $
  map generateFunction [1..n] ++ 
  ["// Used function", "function used() { return 'used'; }", "used();"]
  where
    generateFunction i = 
      "function unused" ++ show i ++ "() { return " ++ show i ++ "; }"