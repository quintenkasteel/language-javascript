{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests for the @js@ quasi-quoter.
--
-- Validates that the @js@ quasi-quoter correctly parses valid JavaScript
-- at compile time and embeds the source as a string literal.
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Parser.QQ.Validate (tests) where

import Language.JavaScript.QQ (js)
import Test.Hspec

-- | Test suite for the @js@ validate quasi-quoter.
tests :: Spec
tests = describe "js quasi-quoter" $ do
  describe "valid JavaScript" $ do
    it "accepts variable declarations" $ do
      let result = [js| var x = 42; |]
      result `shouldContain` "var"
      result `shouldContain` "42"

    it "accepts function declarations" $ do
      let result = [js| function add(a, b) { return a + b; } |]
      result `shouldContain` "function"
      result `shouldContain` "add"

    it "accepts arrow functions" $ do
      let result = [js| const f = (x) => x + 1; |]
      result `shouldContain` "=>"

    it "accepts multiline JavaScript" $ do
      let result =
            [js|
              var x = 1;
              var y = 2;
              var z = x + y;
            |]
      result `shouldContain` "var x"
      result `shouldContain` "var y"
      result `shouldContain` "var z"

    it "accepts class declarations" $ do
      let result = [js| class Foo { constructor() {} } |]
      result `shouldContain` "class"
      result `shouldContain` "Foo"

    it "preserves original source text" $ do
      let input = [js| console.log("hello"); |]
      input `shouldContain` "console"
      input `shouldContain` "hello"

    it "accepts empty statements" $ do
      let result = [js| ; |]
      result `shouldContain` ";"

    it "accepts template literals" $ do
      let result = [js| var x = `hello world`; |]
      result `shouldContain` "hello world"

    it "accepts for loops" $ do
      let result = [js| for (var i = 0; i < 10; i++) { } |]
      result `shouldContain` "for"

    it "accepts try-catch blocks" $ do
      let result = [js| try { x(); } catch(e) { console.log(e); } |]
      result `shouldContain` "try"
      result `shouldContain` "catch"
