import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake

main :: IO ()
main = do
  let source = unlines
        [ "var friends = new WeakSet();"
        , "friends.add('alice');"
        ]
  case parse source "test" of
    Right ast -> do
      putStrLn "=== FRIENDS ONLY TEST ==="
      putStrLn $ "Source: " ++ unlines ["var friends = new WeakSet();", "friends.add('alice');"]

      let optimized = treeShake defaultOptions ast

      -- Check if friends exists in optimized AST
      let astString = show optimized
      if "friends" `elem` words astString
      then putStrLn "SUCCESS: friends preserved"
      else putStrLn "PROBLEM: friends eliminated"

    Left err -> putStrLn $ "Parse failed: " ++ err