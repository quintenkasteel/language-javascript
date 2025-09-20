import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Pretty.Printer

main :: IO ()
main = do
  let source = unlines
        [ "var friends = new WeakSet();"
        , "var enemies = new WeakSet();"
        , ""
        , "function Person(name) {"
        , "  this.name = name;"
        , "}"
        , ""
        , "function addFriend(person) {"
        , "  friends.add(person);"
        , "}"
        , ""
        , "function isFriend(person) {"
        , "  return friends.has(person);"
        , "}"
        , ""
        , "var alice = new Person('Alice');"
        , "addFriend(alice);"
        , "console.log(isFriend(alice));"
        ]
  case parse source "test" of
    Right ast -> do
      putStrLn "=== ACTUAL TEST CASE DEBUG ==="

      putStrLn "ORIGINAL PRETTY PRINTED:"
      putStrLn $ renderToString ast

      let optimized = treeShake defaultOptions ast
      putStrLn "OPTIMIZED PRETTY PRINTED:"
      let optimizedSource = renderToString optimized
      putStrLn optimizedSource

      putStrLn "\n=== EXPECTED BEHAVIOR ==="
      putStrLn "- friends: should be PRESERVED (used in addFriend and isFriend)"
      putStrLn "- enemies: should be ELIMINATED (never used)"

      putStrLn "\n=== ACTUAL RESULTS ==="
      if "friends" `elem` words optimizedSource
      then putStrLn "friends: PRESERVED ✓"
      else putStrLn "friends: ELIMINATED ✗"

      if "enemies" `elem` words optimizedSource
      then putStrLn "enemies: PRESERVED ✗"
      else putStrLn "enemies: ELIMINATED ✓"

    Left err -> putStrLn $ "Parse failed: " ++ err