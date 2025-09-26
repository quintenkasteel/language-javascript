import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Process.TreeShake
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

main :: IO ()
main = do
  putStrLn "=== Debug Failing Test ==="
  
  -- Test the exact case from the failing test
  let source1 = "function unused() { return 42; } var x = 1;"
  debugTest "Test 1 - unused function" source1
  
  let source2 = "function outer() { var captured = 1; var notCaptured = 2; return function inner() { return captured; }; }"
  debugTest "Test 2 - closure" source2

debugTest :: String -> String -> IO ()
debugTest testName source = do
  putStrLn $ "\n--- " ++ testName ++ " ---"
  putStrLn $ "Source: " ++ source
  
  case parse source "test" of
    Right ast -> do
      let analysis = analyzeUsage ast
      let usageMap = _usageMap analysis
      
      putStrLn $ "Total identifiers found: " ++ show (Map.size usageMap)
      putStrLn "Usage map contents:"
      mapM_ (\(k, v) -> putStrLn $ "  " ++ Text.unpack k ++ " -> isUsed: " ++ show (_isUsed v) ++ ", refs: " ++ show (_directReferences v)) (Map.toList usageMap)
      
      let optimized = treeShake defaultOptions ast
      putStrLn "✓ Tree shaking completed"
      
    Left err -> putStrLn $ "Parse error: " ++ err