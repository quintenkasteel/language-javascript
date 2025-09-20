import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Control.Lens ((^.))

main :: IO ()
main = do
  let source = unlines
        [ "var friends = new WeakSet();"
        , "var enemies = new WeakSet();"
        , ""
        , "function addFriend(person) {"
        , "  friends.add(person);"
        , "}"
        , ""
        , "function isFriend(person) {"
        , "  return friends.has(person);"
        , "}"
        , ""
        , "var alice = {};"
        , "addFriend(alice);"
        , "console.log(isFriend(alice));"
        ]
  case parse source "test" of
    Right ast -> do
      putStrLn "=== USAGE ANALYSIS DEBUG ==="
      let analysis = analyzeUsage ast
      let usage = analysis ^. usageMap

      -- Print each variable's usage
      let printUsageInfo (name, info) = do
            let isUsedVal = info ^. isUsed
            let refsVal = info ^. directReferences
            putStrLn $ Text.unpack name ++ ": isUsed=" ++ show isUsedVal ++
                       ", refs=" ++ show refsVal
      mapM_ printUsageInfo (Map.toList usage)

      putStrLn "\n=== EXPECTED BEHAVIOR ==="
      putStrLn "- friends: should be isUsed=True (used in addFriend and isFriend)"
      putStrLn "- enemies: should be isUsed=False (never used)"
      putStrLn "- addFriend: should be isUsed=True (called)"
      putStrLn "- isFriend: should be isUsed=True (called)"
    Left err -> putStrLn $ "Parse error: " ++ err