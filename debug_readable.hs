import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Pretty.Printer

main :: IO ()
main = do
  let source = "var x = 42; console.log(x);"
  case parse source "test" of
    Right ast -> do
      putStrLn "=== READABLE DEBUG ==="
      putStrLn $ "Original source: " ++ source

      let optimized = treeShake defaultOptions ast
      let optimizedSource = renderToString optimized

      putStrLn $ "Optimized source: " ++ optimizedSource

      -- Check if x is preserved
      if "var x" `elem` words optimizedSource || "x" `elem` words optimizedSource
      then putStrLn "SUCCESS: Variable x is preserved"
      else putStrLn "PROBLEM: Variable x was eliminated"

    Left err -> putStrLn $ "Parse failed: " ++ err