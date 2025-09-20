import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Pretty.Printer

main :: IO ()
main = do
  putStrLn "=== WEAKSET AST COMPARISON DEBUG ==="

  let source = unlines
        [ "var friends = new WeakSet();"
        , "friends.add('alice');"
        ]
  case parse source "test" of
    Right ast -> do
      putStrLn "ORIGINAL PRETTY PRINTED:"
      putStrLn $ renderToString ast
      putStrLn ""

      let optimized = treeShake defaultOptions ast
      putStrLn "OPTIMIZED PRETTY PRINTED:"
      putStrLn $ renderToString optimized
      putStrLn ""

      putStrLn "COMPARISON:"
      if renderToString ast == renderToString optimized
      then putStrLn "IDENTICAL: Variable preserved correctly"
      else putStrLn "DIFFERENT: Variable was modified/eliminated"

    Left err -> putStrLn $ "Parse failed: " ++ err