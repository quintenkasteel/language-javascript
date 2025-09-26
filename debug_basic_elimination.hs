import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake

main :: IO ()
main = do
  let source = "var unused = 42;"
  case parse source "test" of
    Right ast -> do
      putStrLn "=== BASIC ELIMINATION TEST ==="
      putStrLn "Source: var unused = 42;"

      putStrLn "\n=== WITH DEFAULT OPTIONS ==="
      let optimized = treeShake defaultOptions ast
      if show ast == show optimized
      then putStrLn "DEFAULT: unused variable NOT eliminated (something is wrong with tree shaking)"
      else putStrLn "DEFAULT: unused variable eliminated (tree shaking works)"
    Left err -> putStrLn $ "Parse failed: " ++ err