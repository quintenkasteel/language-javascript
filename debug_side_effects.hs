import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Process.TreeShake.Elimination
import qualified Data.Text as Text

main :: IO ()
main = do
  putStrLn "=== Side Effects Debug ==="
  
  let source = "function unused() { return 42; } var x = 1;"
  putStrLn $ "Source: " ++ source
  
  case parse source "test" of
    Right (JSAstProgram statements _) -> do
      putStrLn "\n--- Side Effect Analysis ---"
      mapM_ (\(i, stmt) -> do
        let hasSideEffects = hasObservableSideEffects stmt
        putStrLn $ "Statement " ++ show i ++ ": " ++ statementType stmt ++ " -> side effects: " ++ show hasSideEffects
        ) (zip [1..] statements)
        
    _ -> putStrLn "Failed to parse"

statementType :: JSStatement -> String
statementType stmt = case stmt of
  JSFunction {} -> "Function"
  JSVariable {} -> "Variable" 
  JSEmptyStatement {} -> "Empty"
  JSExpressionStatement {} -> "Expression"
  _ -> "Other"