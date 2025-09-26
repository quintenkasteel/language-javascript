import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Process.TreeShake
import qualified Data.Text as Text

main :: IO ()
main = do
  putStrLn "=== Debug Used Variable ==="
  
  let source = "function unused() { return 42; } var x = 1; console.log(x);"
  putStrLn $ "Source: " ++ source
  
  case parse source "test" of
    Right ast -> do
      putStrLn "\n--- Analysis ---"
      let analysis = analyzeUsage ast
      putStrLn $ "Original identifiers: " ++ show (_totalIdentifiers analysis)
      
      let optimized = treeShake defaultOptions ast
      putStrLn "\n--- Original vs Optimized statements ---"
      case (ast, optimized) of
        (JSAstProgram origStmts _, JSAstProgram optStmts _) -> do
          putStrLn $ "Original: " ++ show (map statementType origStmts)
          putStrLn $ "Optimized: " ++ show (map statementType optStmts)
        _ -> putStrLn "Not programs"
      
      let optimizedAnalysis = analyzeUsage optimized
      putStrLn $ "Optimized identifiers: " ++ show (_totalIdentifiers optimizedAnalysis)
      
    Left err -> putStrLn $ "Parse error: " ++ err

statementType :: JSStatement -> String
statementType stmt = case stmt of
  JSFunction {} -> "Function"
  JSVariable {} -> "Variable" 
  JSEmptyStatement {} -> "Empty"
  JSExpressionStatement {} -> "Expression"
  JSMethodCall {} -> "MethodCall"
  _ -> "Other"