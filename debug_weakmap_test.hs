import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Pretty.Printer

main :: IO ()
main = do
  let source = unlines
        [ "var friends = new WeakMap();"
        , "var enemies = new WeakMap();"
        , ""
        , "function addFriend(person, data) {"
        , "  friends.set(person, data);"
        , "}"
        , ""
        , "function isFriend(person) {"
        , "  return friends.has(person);"
        , "}"
        , ""
        , "var alice = {};"
        , "addFriend(alice, 'friend');"
        , "console.log(isFriend(alice));"
        ]
  case parse source "test" of
    Right ast -> do
      putStrLn "=== WEAKMAP CONSISTENCY TEST ==="

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