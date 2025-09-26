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
  putStrLn "=== Size Reduction Test Debug (Fixed) ==="

  -- Test with unique variable names like the fixed test
  let unusedVars = map (\i -> "var unused" ++ show i) [1..10]
  let source = unlines $ unusedVars ++ ["console.log('test');"]
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
      putStrLn $ "Passes test (> 0.5)? " ++ show (reduction > 0.5)

      -- Show first few identifiers
      putStrLn "First few identifiers:"
      mapM_ (putStrLn . ("  " ++) . show) $ take 5 $ Map.toList usageMapData

    Left err -> putStrLn $ "Parse error: " ++ err