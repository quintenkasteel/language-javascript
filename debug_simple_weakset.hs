import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake

main :: IO ()
main = do
  let source = "var unused = new WeakSet();"
  case parse source "test" of
    Right ast -> do
      putStrLn "=== SIMPLE WEAKSET TEST ==="
      putStrLn "Source: var unused = new WeakSet();"

      putStrLn "\n=== WITH DEFAULT OPTIONS ==="
      let optimized1 = treeShake defaultOptions ast
      if show ast == show optimized1
      then putStrLn "DEFAULT: unused variable NOT eliminated (BUG)"
      else putStrLn "DEFAULT: unused variable eliminated (CORRECT)"

      putStrLn "\n=== WITH AGGRESSIVE OPTIONS ==="
      let aggressiveOpts = (configureAggressive . configurePreserveSideEffects False) defaultOptions
      let optimized2 = treeShake aggressiveOpts ast
      if show ast == show optimized2
      then putStrLn "AGGRESSIVE: unused variable NOT eliminated (BUG)"
      else putStrLn "AGGRESSIVE: unused variable eliminated (CORRECT)"
    Left err -> putStrLn $ "Parse failed: " ++ err