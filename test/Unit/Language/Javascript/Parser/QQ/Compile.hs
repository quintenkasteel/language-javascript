{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests for the @jsast@ quasi-quoter.
--
-- Validates that the @jsast@ quasi-quoter correctly parses JavaScript
-- at compile time and produces the expected AST structure matching
-- what the parser would produce at runtime.
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Parser.QQ.Compile (tests) where

import Language.JavaScript.Parser.AST
import Language.JavaScript.QQ (jsast)
import qualified Language.JavaScript.Parser.Parser as Parser
import Test.Hspec

-- | Test suite for the @jsast@ compile quasi-quoter.
tests :: Spec
tests = describe "jsast quasi-quoter" $ do
  describe "AST compilation" $ do
    it "produces a JSAST value" $ do
      let ast = [jsast| var x = 42; |]
      showStripped ast `shouldContain` "JSVariable"

    it "matches runtime parse result for variable declaration" $ do
      let qqAst = [jsast| var x = 42; |]
      let rtResult = Parser.parse " var x = 42; " "test"
      case rtResult of
        Right rtAst -> showStripped qqAst `shouldBe` showStripped rtAst
        Left err -> expectationFailure ("Runtime parse failed: " <> err)

    it "matches runtime parse result for function declaration" $ do
      let qqAst = [jsast| function foo() { return 1; } |]
      let rtResult = Parser.parse " function foo() { return 1; } " "test"
      case rtResult of
        Right rtAst -> showStripped qqAst `shouldBe` showStripped rtAst
        Left err -> expectationFailure ("Runtime parse failed: " <> err)

    it "produces correct AST for expressions" $ do
      let ast = [jsast| 1 + 2; |]
      showStripped ast `shouldContain` "JSExpressionBinary"

    it "produces correct AST for if statements" $ do
      let ast = [jsast| if (true) { x(); } |]
      showStripped ast `shouldContain` "JSIf"

    it "produces correct AST for class declarations" $ do
      let ast = [jsast| class Foo { constructor() {} } |]
      showStripped ast `shouldContain` "JSClass"

    it "produces correct AST for arrow functions" $ do
      let ast = [jsast| var f = (x) => x + 1; |]
      showStripped ast `shouldContain` "JSArrowExpression"
