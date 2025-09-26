#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, containers, text, lens
-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((^.), (.~), (&))
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types as Types

main :: IO ()
main = do
  putStrLn "=== Default Options Debug ==="

  let opts = defaultOptions
  putStrLn $ "preserveTopLevel: " ++ show (opts ^. Types.preserveTopLevel)
  putStrLn $ "aggressiveShaking: " ++ show (opts ^. Types.aggressiveShaking)
  putStrLn $ "preserveSideEffects: " ++ show (opts ^. Types.preserveSideEffects)
  putStrLn $ "optimizationLevel: " ++ show (opts ^. Types.optimizationLevel)

  -- Test shouldPreserveForDynamicUsage logic
  putStrLn $ "\nFor conservative mode (aggressiveShaking=False):"
  putStrLn $ "not (aggressiveShaking) = " ++ show (not (opts ^. Types.aggressiveShaking))

  putStrLn $ "\nFor aggressive mode (aggressiveShaking=True):"
  let aggressiveOpts = defaultOptions & Types.aggressiveShaking .~ True
  putStrLn $ "not (aggressiveShaking) = " ++ show (not (aggressiveOpts ^. Types.aggressiveShaking))