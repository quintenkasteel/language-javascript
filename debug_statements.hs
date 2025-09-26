import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Process.TreeShake
import qualified Data.Text as Text

main :: IO ()
main = do
  putStrLn "=== Debug Statements ==="
  
  let source = "function unused() { return 42; } var x = 1;"
  putStrLn $ "Source: " ++ source
  
  case parse source "test" of
    Right ast -> do
      putStrLn "\n--- Original AST statements ---"
      case ast of
        JSAstProgram statements _ -> do
          mapM_ (\(i, stmt) -> putStrLn $ "  " ++ show i ++ ": " ++ show (statementType stmt)) (zip [1..] statements)
        _ -> putStrLn "Not a program"
      
      let optimized = treeShake defaultOptions ast
      putStrLn "\n--- Optimized AST statements ---"
      case optimized of
        JSAstProgram statements _ -> do
          mapM_ (\(i, stmt) -> putStrLn $ "  " ++ show i ++ ": " ++ show (statementType stmt)) (zip [1..] statements)
        _ -> putStrLn "Not a program"
      
    Left err -> putStrLn $ "Parse error: " ++ err

statementType :: JSStatement -> String
statementType stmt = case stmt of
  JSFunction {} -> "Function"
  JSVariable {} -> "Variable" 
  JSEmptyStatement {} -> "Empty"
  JSExpressionStatement {} -> "Expression"
  _ -> "Other"