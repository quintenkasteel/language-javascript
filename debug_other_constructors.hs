import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Pretty.Printer

testConstructor :: String -> IO ()
testConstructor constructorName = do
  let source = unlines
        [ "var used = new " ++ constructorName ++ "();"
        , "var unused = new " ++ constructorName ++ "();"
        , "console.log(used);"
        ]
  case parse source "test" of
    Right ast -> do
      let optimized = treeShake defaultOptions ast
      let optimizedSource = renderToString optimized

      putStrLn $ "=== " ++ constructorName ++ " TEST ==="
      if "used" `elem` words optimizedSource && not ("unused" `elem` words optimizedSource)
      then putStrLn $ constructorName ++ ": ✓ CORRECT (used preserved, unused eliminated)"
      else if "used" `elem` words optimizedSource && "unused" `elem` words optimizedSource
      then putStrLn $ constructorName ++ ": ⚠ TOO CONSERVATIVE (both preserved)"
      else putStrLn $ constructorName ++ ": ✗ ERROR (used not preserved)"

    Left err -> putStrLn $ constructorName ++ " parse failed: " ++ err

main :: IO ()
main = do
  putStrLn "=== CONSTRUCTOR BEHAVIOR ANALYSIS ==="
  putStrLn ""

  -- Test constructors that should be safe for elimination when unused
  testConstructor "Array"
  testConstructor "Object"
  testConstructor "Map"
  testConstructor "Set"
  testConstructor "WeakMap"
  testConstructor "WeakSet"
  testConstructor "RegExp"
  testConstructor "String"
  testConstructor "Number"
  testConstructor "Boolean"
  testConstructor "Date"