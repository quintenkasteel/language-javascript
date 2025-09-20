#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, hspec, containers, text, lens
-}

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Set as Set
import Integration.Language.Javascript.Process.TreeShake
import Test.Hspec

main :: IO ()
main = hspec testTreeShakeIntegration