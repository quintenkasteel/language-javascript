import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Control.Lens ((^.))

main :: IO ()
main = do
  putStrLn "=== Analysis Debug ==="
  
  let source = "function unused() { return 42; } var x = 1;"
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
        putStrLn $ "  " ++ Text.unpack k ++ ": " ++ show (_isUsed v)
        acc) (pure ()) usageMap
        
      putStrLn "\n--- Optimized AST ---"
      let optimized = treeShake defaultOptions ast
      case optimized of
        JSAstProgram statements _ -> do
          putStrLn $ "Optimized statements count: " ++ show (length statements)
          mapM_ (\(i, stmt) -> putStrLn $ "  " ++ show i ++ ": " ++ show (take 50 (show stmt))) (zip [1..] statements)
        _ -> putStrLn "Not a program"
      
    Left err -> putStrLn $ "Parse error: " ++ err