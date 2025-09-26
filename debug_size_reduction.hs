#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, lens
-}

import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Control.Lens ((^.))

main :: IO ()
main = do
  putStrLn "=== Size Reduction Test Debug ==="

  -- Recreate the exact test case
  let source = unlines $ replicate 10 "var unused" ++ ["console.log('test');"]
  putStrLn $ "Source (first 100 chars): " ++ take 100 source

  case parse source "test" of
    Right ast -> do
      let analysis = analyzeUsage ast
      let reduction = analysis ^. estimatedReduction
      let usageMapData = analysis ^. usageMap
      let totalIds = analysis ^. totalIdentifiers
      let unusedCnt = analysis ^. unusedCount

      putStrLn $ "Estimated reduction: " ++ show reduction
      putStrLn $ "Total identifiers: " ++ show totalIds
      putStrLn $ "Unused count: " ++ show unusedCnt
      putStrLn $ "Usage map size: " ++ show (Map.size usageMapData)
      putStrLn $ "Usage map (first 3): " ++ show (take 3 $ Map.toList usageMapData)

      -- Expected: reduction should be > 0.5 since we have 10 unused vars

    Left err -> putStrLn $ "Parse error: " ++ err