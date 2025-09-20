import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import qualified Language.JavaScript.Process.TreeShake.Types as Types
import qualified Data.Text as Text
import Control.Lens ((^.))

-- Add debug printing to see what's happening
debugIsVariableDeclarationUsed :: UsageMap -> String -> IO ()
debugIsVariableDeclarationUsed usageMap varName = do
  let textName = Text.pack varName
  let isUsed = Types.isIdentifierUsed textName usageMap
  putStrLn $ varName ++ " is used: " ++ show isUsed

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
        , "var alice = {};"
        , "addFriend(alice);"
        ]
  case parse source "test" of
    Right ast -> do
      putStrLn "=== ELIMINATION LOGIC DEBUG ==="
      let analysis = analyzeUsage ast
      let usage = analysis ^. usageMap

      -- Debug specific variables
      debugIsVariableDeclarationUsed usage "friends"
      debugIsVariableDeclarationUsed usage "enemies"

      -- Now test elimination
      let optimized = treeShake defaultOptions ast
      let astString = show optimized

      putStrLn "\n=== RESULTS ==="
      if "friends" `elem` words astString
      then putStrLn "friends: PRESERVED"
      else putStrLn "friends: ELIMINATED"

      if "enemies" `elem` words astString
      then putStrLn "enemies: PRESERVED"
      else putStrLn "enemies: ELIMINATED"

    Left err -> putStrLn $ "Parse failed: " ++ err