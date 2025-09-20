import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Control.Lens ((^.))

main :: IO ()
main = do
  let source = unlines
        [ "console.log(hoistedFunction());"  -- Line 1
        , "var x = regularVar;"              -- Line 2 - x uses regularVar
        , "function hoistedFunction() { return 'hoisted'; }"  -- Line 3
        , "var regularVar = 42;"             -- Line 4 - should be preserved (used in line 2)
        , "function unusedHoisted() { return 'unused'; }"    -- Line 5 - should be eliminated
        ]
  case parse source "test" of
    Right ast -> do
      putStrLn "=== DETAILED HOISTING ANALYSIS ==="
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
      putStrLn "- regularVar: should be PRESERVED (used in 'var x = regularVar')"
      putStrLn "- x: should be PRESERVED (test expects it)"
      putStrLn "- hoistedFunction: should be PRESERVED (called)"
      putStrLn "- unusedHoisted: should be ELIMINATED (never called)"

    Left err -> putStrLn $ "Parse failed: " ++ err