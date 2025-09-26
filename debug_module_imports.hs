#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, lens
-}

import Language.JavaScript.Parser.Parser (parseModule)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Pretty.Printer (renderToString)
import qualified Data.Text as Text
import qualified Data.Set as Set
import Control.Lens ((^.))

main :: IO ()
main = do
  putStrLn "=== Module Import Elimination Debug ==="

  -- Test the exact failing case
  let source = "import {used, unused} from 'utils';\nimport 'side-effect';\n\nexport const API = used();\nconst internal = 'internal';\n"
  putStrLn $ "Source:\n" ++ source

  case parseModule source "test" of
    Right ast -> do
      putStrLn "Parse succeeded!"
      let opts = defaultOptions { _preserveExports = Set.fromList [Text.pack "API"] }
      let (optimized, analysis) = treeShakeWithAnalysis opts ast
      let optimizedSource = renderToString optimized

      putStrLn $ "\nAnalysis:"
      putStrLn $ "  Total identifiers: " ++ show (_totalIdentifiers analysis)
      putStrLn $ "  Unused count: " ++ show (_unusedCount analysis)
      putStrLn $ "  Side effects: " ++ show (_sideEffectCount analysis)

      putStrLn $ "\nOptimized source:\n" ++ optimizedSource

      putStrLn $ "\nChecks:"
      putStrLn $ "  Contains 'used': " ++ show ("used" `elem` words optimizedSource)
      putStrLn $ "  Contains 'unused': " ++ show ("unused" `elem` words optimizedSource)
      putStrLn $ "  Contains 'side-effect': " ++ show ("side-effect" `elem` words optimizedSource)
      putStrLn $ "  Contains 'API': " ++ show ("API" `elem` words optimizedSource)

    Left err -> putStrLn $ "Parse error: " ++ err