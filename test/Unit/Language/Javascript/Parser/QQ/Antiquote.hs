{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests for the @jsx@ quasi-quoter with antiquotation.
--
-- Validates that the @jsx@ quasi-quoter correctly handles @${expr}@
-- antiquotation markers, substituting Haskell expressions into the
-- JavaScript AST at the correct positions.
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Parser.QQ.Antiquote (tests) where

import Language.JavaScript.Parser.AST
import Language.JavaScript.QQ (jsx)
import Test.Hspec

-- | Test suite for the @jsx@ antiquotation quasi-quoter.
tests :: Spec
tests = describe "jsx quasi-quoter" $ do
  describe "basic antiquotation" $ do
    it "splices a JSExpression into a variable declaration" $ do
      let expr = JSDecimal JSNoAnnot 99
      let ast = [jsx| var x = ${expr}; |]
      showStripped ast `shouldContain` "JSDecimal '99"

    it "splices into a binary expression" $ do
      let lhs = JSDecimal JSNoAnnot 1
      let ast = [jsx| var r = ${lhs} + 2; |]
      showStripped ast `shouldContain` "JSDecimal '1"

    it "splices into a function call argument" $ do
      let arg = JSStringLiteral JSNoAnnot "'hello'"
      let ast = [jsx| console.log(${arg}); |]
      showStripped ast `shouldContain` "hello"

    it "handles multiple splices" $ do
      let a = JSDecimal JSNoAnnot 10
      let b = JSDecimal JSNoAnnot 20
      let ast = [jsx| var sum = ${a} + ${b}; |]
      showStripped ast `shouldContain` "JSDecimal '10"
      showStripped ast `shouldContain` "JSDecimal '20"

  describe "splice positions" $ do
    it "splices into assignment RHS" $ do
      let val = JSLiteral JSNoAnnot "true"
      let ast = [jsx| var flag = ${val}; |]
      showStripped ast `shouldContain` "JSLiteral 'true'"

    it "splices into array literal element" $ do
      let elem' = JSDecimal JSNoAnnot 42
      let ast = [jsx| var arr = [${elem'}, 2, 3]; |]
      showStripped ast `shouldContain` "JSDecimal '42"
