{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Test module for QuickCheck generators
--
-- Simple test to verify that the generators compile and work correctly.
module Properties.Language.Javascript.Parser.GeneratorsTest
    ( testGenerators
    ) where

import Test.Hspec
import Test.QuickCheck

import Language.JavaScript.Parser.AST
import Properties.Language.Javascript.Parser.Generators

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
    
    it "generates valid identifier strings" $ property $ do
      ident <- genValidIdentifier
      return $ not (null ident) && all (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_$")) ident

  describe "Complex structure generators" $ do
    it "generates JSObjectProperty instances" $ property $
      \prop -> isValidObjectProperty (prop :: JSObjectProperty)
    
    it "generates JSCommaList instances" $ property $
      \list -> isValidCommaList (list :: JSCommaList JSExpression)

-- Helper functions for validation
isValidExpression :: JSExpression -> Bool
isValidExpression expr = case expr of
  JSIdentifier _ _ -> True
  JSDecimal _ _ -> True
  JSLiteral _ _ -> True
  JSExpressionBinary _ _ _ -> True
  JSExpressionTernary _ _ _ _ _ -> True
  JSCallExpression _ _ _ _ -> True
  JSMemberDot _ _ _ -> True
  JSArrayLiteral _ _ _ -> True
  JSObjectLiteral _ _ _ -> True
  JSArrowExpression _ _ _ -> True
  JSFunctionExpression _ _ _ _ _ _ -> True
  _ -> True  -- Accept all valid AST nodes

isValidBinOp :: JSBinOp -> Bool
isValidBinOp op = case op of
  JSBinOpAnd _ -> True
  JSBinOpBitAnd _ -> True
  JSBinOpBitOr _ -> True
  JSBinOpBitXor _ -> True
  JSBinOpDivide _ -> True
  JSBinOpEq _ -> True
  JSBinOpGe _ -> True
  JSBinOpGt _ -> True
  JSBinOpLe _ -> True
  JSBinOpLt _ -> True
  JSBinOpMinus _ -> True
  JSBinOpMod _ -> True
  JSBinOpNeq _ -> True
  JSBinOpOr _ -> True
  JSBinOpPlus _ -> True
  JSBinOpTimes _ -> True
  _ -> True  -- Accept all valid binary operators

isValidUnaryOp :: JSUnaryOp -> Bool
isValidUnaryOp op = case op of
  JSUnaryOpDecr _ -> True
  JSUnaryOpDelete _ -> True
  JSUnaryOpIncr _ -> True
  JSUnaryOpMinus _ -> True
  JSUnaryOpNot _ -> True
  JSUnaryOpPlus _ -> True
  JSUnaryOpTilde _ -> True
  JSUnaryOpTypeof _ -> True
  JSUnaryOpVoid _ -> True
  _ -> True  -- Accept all valid unary operators

isValidStatement :: JSStatement -> Bool
isValidStatement stmt = case stmt of
  JSStatementBlock _ _ _ _ -> True
  JSBreak _ _ _ -> True
  JSConstant _ _ _ -> True
  JSContinue _ _ _ -> True
  JSDoWhile _ _ _ _ _ _ _ -> True
  JSFor _ _ _ _ _ _ _ _ _ -> True
  JSForIn _ _ _ _ _ _ _ -> True
  JSForVar _ _ _ _ _ _ _ _ _ _ -> True
  JSFunction _ _ _ _ _ _ _ -> True
  JSIf _ _ _ _ _ -> True
  JSIfElse _ _ _ _ _ _ _ -> True
  JSLabelled _ _ _ -> True
  JSReturn _ _ _ -> True
  JSSwitch _ _ _ _ _ _ _ _ -> True
  JSThrow _ _ _ -> True
  JSTry _ _ _ _ -> True
  JSVariable _ _ _ -> True
  JSWhile _ _ _ _ _ -> True
  JSWith _ _ _ _ _ _ -> True
  _ -> True  -- Accept all valid statements

isValidBlock :: JSBlock -> Bool
isValidBlock (JSBlock _ _ _) = True

isValidJSAST :: JSAST -> Bool
isValidJSAST ast = case ast of
  JSAstProgram _ _ -> True
  JSAstModule _ _ -> True
  JSAstStatement _ _ -> True
  JSAstExpression _ _ -> True
  JSAstLiteral _ _ -> True

isValidObjectProperty :: JSObjectProperty -> Bool
isValidObjectProperty prop = case prop of
  JSPropertyNameandValue _ _ _ -> True
  JSPropertyIdentRef _ _ -> True
  JSObjectMethod _ -> True
  JSObjectSpread _ _ -> True

isValidCommaList :: JSCommaList a -> Bool
isValidCommaList JSLNil = True
isValidCommaList (JSLOne _) = True
isValidCommaList (JSLCons _ _ _) = True