import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Control.Lens ((^.))

main :: IO ()
main = do
  let source = unlines
        [ "console.log(hoistedFunction());"
        , "var x = regularVar;"
        , "function hoistedFunction() { return 'hoisted'; }"
        , "var regularVar = 42;"
        , "function unusedHoisted() { return 'unused'; }"
        ]
  case parse source "test" of
    Right ast -> do
      putStrLn "=== HOISTING TEST ANALYSIS ==="
      let analysis = analyzeUsage ast
      let usage = analysis ^. usageMap

      putStrLn $ "Usage map for 'regularVar': " ++ case Map.lookup (Text.pack "regularVar") usage of
        Just info -> "isUsed=" ++ show (info ^. isUsed) ++
                    ", refs=" ++ show (info ^. directReferences) ++
                    ", sideEffects=" ++ show (info ^. hasSideEffects) ++
                    ", exported=" ++ show (info ^. isExported)
        Nothing -> "NOT FOUND"

      putStrLn $ "Usage map for 'x': " ++ case Map.lookup (Text.pack "x") usage of
        Just info -> "isUsed=" ++ show (info ^. isUsed) ++
                    ", refs=" ++ show (info ^. directReferences)
        Nothing -> "NOT FOUND"

      putStrLn "\n=== TREE SHAKING ==="
      let optimized = treeShake defaultOptions ast
      putStrLn "Tree shaking complete. Check if regularVar survived elimination."
    Left err -> putStrLn $ "Parse failed: " ++ err