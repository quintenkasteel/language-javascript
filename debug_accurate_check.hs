import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Pretty.Printer

main :: IO ()
main = do
  putStrLn "=== ACCURATE CHECK DEBUG ==="

  let source = "var x = 42; console.log(x);"
  case parse source "test" of
    Right ast -> do
      putStrLn $ "Source: " ++ source

      let optimized = treeShake defaultOptions ast
      let optimizedSource = renderToString optimized
      let astString = show optimized

      putStrLn $ "Pretty printed: " ++ optimizedSource
      putStrLn $ "Variable check with pretty print: " ++
        if "x" `elem` words optimizedSource then "PRESERVED" else "ELIMINATED"

      putStrLn $ "Variable check with AST show: " ++
        if "x" `elem` words astString then "PRESERVED" else "ELIMINATED"

      -- Check if the variable declaration is in the AST
      putStrLn $ "JSVariable check: " ++
        if "JSVariable" `elem` words astString then "PRESERVED" else "ELIMINATED"

    Left err -> putStrLn $ "Parse failed: " ++ err