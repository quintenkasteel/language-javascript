import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import qualified Data.Text as Text

main :: IO ()
main = do
  let source = unlines
        [ "var friends = new WeakSet();"
        , "var enemies = new WeakSet();"
        , "function addFriend(person) { friends.add(person); }"
        , "var alice = 'Alice';"
        , "addFriend(alice);"
        ]
  case parse source "test" of
    Right ast -> do
      putStrLn "=== TESTING WITH DEFAULT OPTIONS ==="
      let optimized1 = treeShake defaultOptions ast
      if show ast == show optimized1
      then putStrLn "DEFAULT: AST unchanged - elimination not working!"
      else putStrLn "DEFAULT: AST changed - some elimination occurred"

      putStrLn "\n=== TESTING WITH AGGRESSIVE OPTIONS ==="
      let aggressiveOpts = (configureAggressive . configurePreserveSideEffects False) defaultOptions
      let optimized2 = treeShake aggressiveOpts ast
      if show ast == show optimized2
      then putStrLn "AGGRESSIVE: AST unchanged - elimination not working!"
      else putStrLn "AGGRESSIVE: AST changed - some elimination occurred"
    Left err -> putStrLn $ "Parse failed: " ++ err