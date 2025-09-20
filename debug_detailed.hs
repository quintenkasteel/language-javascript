import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Process.TreeShake
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Language.JavaScript.Process.TreeShake.Elimination (hasObservableSideEffects, shouldPreserveStatement)

main :: IO ()
main = do
  putStrLn "=== Detailed Debug ==="
  
  let source = "function unused() { return 42; } var x = 1;"
  putStrLn $ "Source: " ++ source
  
  case parse source "test" of
    Right ast -> do
      putStrLn "\n--- AST Structure ---"
      print ast
      
      putStrLn "\n--- Side Effect Analysis ---"
      case ast of
        JSAstProgram statements _ -> do
          let analysis = analyzeUsage ast
          let usageMap = _usageMap analysis 
          mapM_ (\(i, stmt) -> do
            putStrLn $ "Statement " ++ show i ++ ":"
            putStrLn $ "  " ++ take 50 (show stmt) ++ "..."
            putStrLn $ "  Has observable side effects: " ++ show (hasObservableSideEffects stmt)
            putStrLn $ "  Should preserve: " ++ show (shouldPreserveStatement defaultOptions usageMap stmt)
            ) (zip [1..] statements)
        _ -> putStrLn "Not a program AST"
      
    Left err -> putStrLn $ "Parse error: " ++ err