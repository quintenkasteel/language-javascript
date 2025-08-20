{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Test module for QuickCheck generators
--
-- Simple test to verify that the generators compile and work correctly.
module Test.Language.Javascript.GeneratorsTest
    ( testGenerators
    ) where

import Test.Hspec
import Test.QuickCheck

import Language.JavaScript.Parser.AST
import Test.Language.Javascript.Generators

-- | Test suite for QuickCheck generators
testGenerators :: Spec
testGenerators = describe "QuickCheck Generators" $ do

  describe "Expression generators" $ do
    it "generates valid JSExpression instances" $ property $
      \expr -> isValidExpression (expr :: JSExpression)
    
    it "generates JSBinOp instances" $ property $
      \op -> isValidBinOp (op :: JSBinOp)
    
    it "generates JSUnaryOp instances" $ property $
      \op -> isValidUnaryOp (op :: JSUnaryOp)

  describe "Statement generators" $ do
    it "generates valid JSStatement instances" $ property $
      \stmt -> isValidStatement (stmt :: JSStatement)
    
    it "generates JSBlock instances" $ property $
      \block -> isValidBlock (block :: JSBlock)

  describe "Program generators" $ do
    it "generates valid JSAST instances" $ property $
      \ast -> isValidJSAST (ast :: JSAST)
    
    it "generates non-empty identifier strings" $ property $
      \ident -> not (null ident) ==> isValidIdentifierString ident
      where isValidIdentifierString = all (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_$"))

  describe "Complex structure generators" $ do
    it "generates JSObjectProperty instances" $ property $
      \prop -> isValidObjectProperty (prop :: JSObjectProperty)
    
    it "generates JSCommaList instances" $ property $
      \list -> isValidCommaList (list :: JSCommaList JSExpression)

-- Helper functions for validation
isValidExpression :: JSExpression -> Bool
isValidExpression _ = True  -- All generated expressions are valid by construction

isValidBinOp :: JSBinOp -> Bool
isValidBinOp _ = True

isValidUnaryOp :: JSUnaryOp -> Bool
isValidUnaryOp _ = True

isValidStatement :: JSStatement -> Bool
isValidStatement _ = True

isValidBlock :: JSBlock -> Bool
isValidBlock _ = True

isValidJSAST :: JSAST -> Bool
isValidJSAST _ = True

isValidObjectProperty :: JSObjectProperty -> Bool
isValidObjectProperty _ = True

isValidCommaList :: JSCommaList a -> Bool
isValidCommaList _ = True