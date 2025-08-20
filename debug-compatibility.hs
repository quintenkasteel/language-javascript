#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Test.Language.Javascript.CompatibilityTest as CompatibilityTest
import Test.Hspec
import Test.Hspec.Runner

main :: IO ()
main = do
  putStrLn "Running CompatibilityTest debug..."
  hspec $ describe "Debug CompatibilityTest" $ do
    describe "Module system compatibility" $ do
      it "debug CommonJS files" $ do
        files <- CompatibilityTest.getCommonJSTestFiles
        print ("CommonJS files found:", files)
        files `shouldSatisfy` (not . null)
      
      it "debug ES6 module files" $ do
        files <- CompatibilityTest.getES6ModuleTestFiles  
        print ("ES6 files found:", files)
        files `shouldSatisfy` (not . null)
        
      it "debug AMD files" $ do
        files <- CompatibilityTest.getAMDTestFiles
        print ("AMD files found:", files)
        files `shouldSatisfy` (not . null)
        
    describe "Error handling" $ do
      it "debug error test files" $ do
        files <- CompatibilityTest.getErrorTestFiles
        print ("Error test files found:", files)
        files `shouldSatisfy` (not . null)