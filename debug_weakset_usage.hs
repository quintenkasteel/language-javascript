import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import qualified Language.JavaScript.Process.TreeShake.Types as Types
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
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
      putStrLn "=== WEAKSET USAGE ANALYSIS ==="

      -- Step 1: Usage analysis
      let analysis = analyzeUsage ast
      let usage = analysis ^. usageMap

      putStrLn "USAGE MAP:"
      let printUsageInfo (name, info) = do
            let isUsedVal = info ^. isUsed
            let refsVal = info ^. directReferences
            putStrLn $ Text.unpack name ++ ": isUsed=" ++ show isUsedVal ++
                       ", refs=" ++ show refsVal
      mapM_ printUsageInfo (Map.toList usage)

      putStrLn "\nSPECIFIC CHECKS:"
      putStrLn $ "friends is used: " ++ show (Types.isIdentifierUsed (Text.pack "friends") usage)
      putStrLn $ "enemies is used: " ++ show (Types.isIdentifierUsed (Text.pack "enemies") usage)

    Left err -> putStrLn $ "Parse failed: " ++ err