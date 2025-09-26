import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Pretty.Printer

main :: IO ()
main = do
  putStrLn "=== AST COMPARISON DEBUG ==="

  let source = "var x = 42; console.log(x);"
  case parse source "test" of
    Right ast -> do
      putStrLn $ "Source: " ++ source
      putStrLn ""

      putStrLn "ORIGINAL AST:"
      putStrLn $ show ast
      putStrLn ""

      let optimized = treeShake defaultOptions ast
      putStrLn "OPTIMIZED AST:"
      putStrLn $ show optimized
      putStrLn ""

      putStrLn "PRETTY PRINTED:"
      let optimizedSource = renderToString optimized
      putStrLn optimizedSource

    Left err -> putStrLn $ "Parse failed: " ++ err