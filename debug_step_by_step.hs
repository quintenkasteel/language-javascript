import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import qualified Language.JavaScript.Process.TreeShake.Types as Types
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Control.Lens ((^.))

main :: IO ()
main = do
  let source = "var x = 42; console.log(x);"
  case parse source "test" of
    Right ast -> do
      putStrLn "=== STEP-BY-STEP DEBUG ==="
      putStrLn $ "Original AST: " ++ show ast

      -- Step 1: Usage analysis
      let analysis = analyzeUsage ast
      let usage = analysis ^. usageMap

      putStrLn "\n=== USAGE ANALYSIS ==="
      let printUsageInfo (name, info) = do
            let isUsedVal = info ^. isUsed
            let refsVal = info ^. directReferences
            putStrLn $ Text.unpack name ++ ": isUsed=" ++ show isUsedVal ++
                       ", refs=" ++ show refsVal
      mapM_ printUsageInfo (Map.toList usage)

      -- Step 2: Tree shake
      let optimized = treeShake defaultOptions ast
      putStrLn "\n=== OPTIMIZED AST ==="
      putStrLn $ "Result: " ++ show optimized

    Left err -> putStrLn $ "Parse failed: " ++ err