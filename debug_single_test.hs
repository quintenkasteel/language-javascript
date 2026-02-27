#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript, text, hspec
-}

{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Control.Monad (forM_)
import qualified Language.JavaScript.Parser as Language.JavaScript.Parser

-- Single test case extracted from CoreProperties
singleTest :: Spec
singleTest = describe "Debug Single Test" $ do
  it "preserves complete programs" $ do
    -- Test with valid program examples
    let validProgs = ["var x = 1;", "function f() { return 2; } f();", "if (true) { console.log('ok'); }"]
    forM_ validProgs $ \input -> do
      case Language.JavaScript.Parser.parse input "test" of
        Right _ -> return () -- Successfully parsed
        Left err -> expectationFailure $ "Failed to parse program: " ++ err

main :: IO ()
main = hspec singleTest