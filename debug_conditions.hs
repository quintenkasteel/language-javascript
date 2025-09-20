import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Control.Lens ((^.))

main :: IO ()
main = do
  let source = "var unused = new WeakSet();"
  case parse source "test" of
    Right ast -> do
      putStrLn "=== ANALYZING INDIVIDUAL CONDITIONS ==="
      let analysis = analyzeUsage ast
      let usage = analysis ^. usageMap

      putStrLn $ "Usage map for 'unused': " ++ case Map.lookup (Text.pack "unused") usage of
        Just info -> "isUsed=" ++ show (info ^. isUsed) ++
                    ", refs=" ++ show (info ^. directReferences) ++
                    ", sideEffects=" ++ show (info ^. hasSideEffects) ++
                    ", exported=" ++ show (info ^. isExported)
        Nothing -> "NOT FOUND"

      putStrLn $ "Usage map for 'WeakSet': " ++ case Map.lookup (Text.pack "WeakSet") usage of
        Just info -> "isUsed=" ++ show (info ^. isUsed) ++
                    ", refs=" ++ show (info ^. directReferences) ++
                    ", sideEffects=" ++ show (info ^. hasSideEffects) ++
                    ", exported=" ++ show (info ^. isExported)
        Nothing -> "NOT FOUND"

      putStrLn "\nThis should help identify which condition is preventing elimination."
    Left err -> putStrLn $ "Parse failed: " ++ err