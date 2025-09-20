import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Pretty.Printer

main :: IO ()
main = do
  let source = unlines
        [ "var usedDate = new Date();"
        , "var unusedDate = new Date();"
        , "var usedRegex = new RegExp('test');"
        , "var unusedRegex = new RegExp('unused');"
        , "var usedString = new String('hello');"
        , "var unusedString = new String('unused');"
        , "var usedError = new Error('test');"
        , "var unusedError = new Error('unused');"
        , ""
        , "console.log(usedDate.getTime());"
        , "console.log(usedRegex.test('testing'));"
        , "console.log(usedString.valueOf());"
        , "console.log(usedError.message);"
        ]
  case parse source "test" of
    Right ast -> do
      putStrLn "=== ENHANCED CONSTRUCTORS TEST ==="

      putStrLn "ORIGINAL PRETTY PRINTED:"
      putStrLn $ renderToString ast

      let optimized = treeShake defaultOptions ast
      putStrLn "OPTIMIZED PRETTY PRINTED:"
      let optimizedSource = renderToString optimized
      putStrLn optimizedSource

      putStrLn "\n=== ANALYSIS ==="
      let checkConstructor name =
            if ("used" ++ name) `elem` words optimizedSource && not (("unused" ++ name) `elem` words optimizedSource)
            then putStrLn $ name ++ ": ✓ CORRECT (used preserved, unused eliminated)"
            else if ("used" ++ name) `elem` words optimizedSource && ("unused" ++ name) `elem` words optimizedSource
            then putStrLn $ name ++ ": ⚠ TOO CONSERVATIVE (both preserved)"
            else putStrLn $ name ++ ": ✗ ERROR (used not preserved)"

      checkConstructor "Date"
      checkConstructor "Regex"
      checkConstructor "String"
      checkConstructor "Error"

    Left err -> putStrLn $ "Parse failed: " ++ err