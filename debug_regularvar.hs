import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake

main :: IO ()
main = do
  let source = unlines
        [ "console.log(hoistedFunction());"
        , "var x = regularVar;"
        , "function hoistedFunction() { return 'hoisted'; }"
        , "var regularVar = 42;"
        , "function unusedHoisted() { return 'unused'; }"
        ]
  case parse source "test" of
    Right ast -> do
      putStrLn "=== CHECKING REGULARVAR ELIMINATION ==="
      let optimized = treeShake defaultOptions ast

      -- Check if regularVar exists in optimized AST
      let astString = show optimized
      if "regularVar" `elem` words astString
      then putStrLn "SUCCESS: regularVar found in optimized AST"
      else putStrLn "PROBLEM: regularVar NOT found in optimized AST (this is the bug)"

      -- Check if x exists in optimized AST
      if "\"x\"" `elem` words astString
      then putStrLn "SUCCESS: x found in optimized AST"
      else putStrLn "PROBLEM: x NOT found in optimized AST"

    Left err -> putStrLn $ "Parse failed: " ++ err