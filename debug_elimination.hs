import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Process.TreeShake
import qualified Data.Text as Text

main :: IO ()
main = do
  putStrLn "=== Elimination Debug ==="
  
  let source = "function unused() { return 42; } var x = 1;"
  putStrLn $ "Source: " ++ source
  
  case parse source "test" of
    Right ast -> do
      let optimized = treeShake defaultOptions ast
      
      case (ast, optimized) of
        (JSAstProgram originalStmts _, JSAstProgram optimizedStmts _) -> do
          putStrLn $ "\nOriginal statements: " ++ show (length originalStmts)
          mapM_ (\(i, stmt) -> putStrLn $ "  " ++ show i ++ ": " ++ show (statementType stmt)) (zip [1..] originalStmts)
          
          putStrLn $ "\nOptimized statements: " ++ show (length optimizedStmts)  
          mapM_ (\(i, stmt) -> putStrLn $ "  " ++ show i ++ ": " ++ show (statementType stmt)) (zip [1..] optimizedStmts)
          
          -- Check if statements were actually eliminated
          if length originalStmts > length optimizedStmts
            then putStrLn "\n✓ Some statements were eliminated"
            else putStrLn "\n✗ No statements were eliminated"
            
        _ -> putStrLn "Not program ASTs"
      
    Left err -> putStrLn $ "Parse error: " ++ err
    
statementType :: JSStatement -> String
statementType stmt = case stmt of
  JSFunction {} -> "Function"
  JSVariable {} -> "Variable" 
  JSEmptyStatement {} -> "Empty"
  JSExpressionStatement {} -> "Expression"
  _ -> "Other"