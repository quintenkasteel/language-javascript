import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  putStrLn "=== Used Variable Debug ==="
  
  -- Test case where variable is actually used
  let source = "var x = 1; console.log(x);"
  putStrLn $ "Source: " ++ source
  
  case parse source "test" of
    Right ast -> do
      putStrLn "\n--- Usage Analysis ---"
      let analysis = analyzeUsage ast
      let usageMap = _usageMap analysis
      
      putStrLn $ "Total identifiers: " ++ show (_totalIdentifiers analysis)
      putStrLn $ "Unused count: " ++ show (_unusedCount analysis)
      
      putStrLn "\n--- Usage Map Contents ---"
      Map.foldrWithKey (\k v acc -> do
        putStrLn $ "  " ++ Text.unpack k ++ ": used=" ++ show (_isUsed v) ++ ", refs=" ++ show (_directReferences v)
        acc) (pure ()) usageMap
        
    Left err -> putStrLn $ "Parse error: " ++ err