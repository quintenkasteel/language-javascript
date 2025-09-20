import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake

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
      putStrLn "=== WEAKSET ELIMINATION TEST ==="
      let optimized = treeShake defaultOptions ast

      -- Check if enemies exists in optimized AST
      let astString = show optimized
      if "enemies" `elem` words astString
      then putStrLn "PROBLEM: enemies found in optimized AST (should be eliminated)"
      else putStrLn "SUCCESS: enemies eliminated from AST"

      -- Check if friends exists in optimized AST
      if "friends" `elem` words astString
      then putStrLn "SUCCESS: friends preserved in AST"
      else putStrLn "PROBLEM: friends eliminated from AST (should be preserved)"

    Left err -> putStrLn $ "Parse failed: " ++ err