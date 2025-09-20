#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, lens
-}

import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import Control.Lens ((^.))
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  putStrLn "=== Usage Map Debug ==="

  let source = "var x = 1; eval('console.log(x)');"
  putStrLn $ "Source: " ++ source

  case parse source "test" of
    Right ast -> do
      putStrLn "\n=== Usage Analysis ==="
      let opts = defaultOptions
      let analysis = analyzeUsageWithOptions opts ast
      let usageMap = analysis ^. usageMap

      putStrLn $ "hasEvalCall: " ++ show (analysis ^. hasEvalCall)
      putStrLn $ "totalIdentifiers: " ++ show (analysis ^. totalIdentifiers)
      putStrLn $ "unusedCount: " ++ show (analysis ^. unusedCount)

      putStrLn $ "\nUsage Map (" ++ show (Map.size usageMap) ++ " identifiers):"
      mapM_ printUsageInfo (Map.toList usageMap)

      -- Check specific identifier
      let xUsage = Map.lookup (Text.pack "x") usageMap
      putStrLn $ "\nSpecific check for 'x':"
      case xUsage of
        Nothing -> putStrLn "  'x' not found in usage map!"
        Just info -> do
          putStrLn $ "  isUsed: " ++ show (_isUsed info)
          putStrLn $ "  directReferences: " ++ show (_directReferences info)
          putStrLn $ "  hasSideEffects: " ++ show (_hasSideEffects info)

    Left err -> putStrLn $ "Parse error: " ++ err

printUsageInfo :: (Text.Text, UsageInfo) -> IO ()
printUsageInfo (name, info) = do
  putStrLn $ "  " ++ Text.unpack name ++ ":"
  putStrLn $ "    isUsed: " ++ show (_isUsed info)
  putStrLn $ "    directReferences: " ++ show (_directReferences info)
  putStrLn $ "    hasSideEffects: " ++ show (_hasSideEffects info)