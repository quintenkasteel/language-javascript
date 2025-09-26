import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake

main :: IO ()
main = do
  let source = unlines
        [ "var x = 42;"
        , "console.log(x);"
        ]
  case parse source "test" of
    Right ast -> do
      putStrLn "=== SIMPLE USAGE TEST ==="
      let optimized = treeShake defaultOptions ast

      -- Check if x exists in optimized AST
      let astString = show optimized
      if "\\\"x\\\"" `elem` words astString || "x" `elem` words astString
      then putStrLn "SUCCESS: x preserved (used in console.log)"
      else putStrLn "PROBLEM: x eliminated (should be preserved)"

    Left err -> putStrLn $ "Parse failed: " ++ err