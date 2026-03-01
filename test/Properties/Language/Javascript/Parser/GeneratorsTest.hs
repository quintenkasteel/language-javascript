{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Test module for QuickCheck generators.
--
-- Validates that generated AST values are well-formed by checking that
-- they can be pretty-printed without crashing and that their ShowStripped
-- representations are non-empty. This provides meaningful validation
-- beyond mere type-level guarantees.
module Properties.Language.Javascript.Parser.GeneratorsTest
  ( testGenerators,
  )
where

import qualified Data.ByteString.Char8 as BS8
import Language.JavaScript.Parser.AST
import Properties.Language.Javascript.Parser.Generators (genValidIdentifier)
import Test.Hspec
import Test.QuickCheck

-- | Test suite for QuickCheck generators
testGenerators :: Spec
testGenerators = describe "QuickCheck Generators" $ do
  describe "Expression generators" $ do
    it "generates JSExpression with non-empty stripped representation" $
      property $
        \expr -> not (null (ss (expr :: JSExpression)))

    it "generates JSBinOp with non-empty stripped representation" $
      property $
        \op -> not (null (ss (op :: JSBinOp)))

    it "generates JSUnaryOp with non-empty stripped representation" $
      property $
        \op -> not (null (ss (op :: JSUnaryOp)))

  describe "Statement generators" $ do
    it "generates JSStatement with non-empty stripped representation" $
      property $
        \stmt -> not (null (ss (stmt :: JSStatement)))

    it "generates JSBlock with non-empty stripped representation" $
      property $
        \block -> not (null (ss (block :: JSBlock)))

  describe "Program generators" $ do
    it "generates JSAST with non-empty showStripped representation" $
      property $
        \ast -> not (null (showStripped (ast :: JSAST)))

    it "generates valid identifier strings" $
      property $ do
        ident <- genValidIdentifier
        return $ not (BS8.null ident) && BS8.all (`elem` (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_$")) ident

  describe "Complex structure generators" $ do
    it "generates JSObjectProperty with non-empty stripped representation" $
      property $
        \prop -> not (null (ss (prop :: JSObjectProperty)))

    it "generates JSCommaList with valid structure" $
      property $
        \list -> commaListLength (list :: JSCommaList JSExpression) >= 0
          && commaListLength list == length (fromCommaList list)

-- | Count elements in a comma list.
commaListLength :: JSCommaList a -> Int
commaListLength JSLNil = 0
commaListLength (JSLOne _) = 1
commaListLength (JSLCons rest _ _) = 1 + commaListLength rest
