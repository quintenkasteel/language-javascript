import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake

main :: IO ()
main = do
  let source = unlines
        [ "var x = regularVar;"  -- x uses regularVar
        , "var regularVar = 42;" -- should be preserved (used above)
        ]
  case parse source "test" of
    Right ast -> do
      putStrLn "=== SIMPLE VAR TEST ==="
      putStrLn "Source:"
      putStrLn "var x = regularVar;"
      putStrLn "var regularVar = 42;"
      putStrLn ""

      let optimized = treeShake defaultOptions ast
      let astString = show optimized

      if "regularVar" `elem` words astString
      then putStrLn "SUCCESS: regularVar preserved"
      else putStrLn "PROBLEM: regularVar eliminated (bug!)"

      if "\"x\"" `elem` words astString
      then putStrLn "INFO: x preserved"
      else putStrLn "INFO: x eliminated"

    Left err -> putStrLn $ "Parse failed: " ++ err