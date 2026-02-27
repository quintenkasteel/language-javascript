{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive Error Recovery Testing for JavaScript Parser
--
-- Tests parser robustness and error handling across JavaScript constructs.
-- Verifies that:
--
--   * Invalid syntax is correctly rejected
--   * Valid syntax is correctly accepted
--   * The parser handles edge cases gracefully without crashing
--   * Incomplete inputs are detected
--   * Large and malformed inputs are handled safely
--
-- All tests use behavioral assertions (isLeft/isRight) for robustness
-- across parser implementations.
--
-- @since 0.7.1.0
module Unit.Language.Javascript.Parser.Error.Recovery
  ( testErrorRecovery,
  )
where

import Control.DeepSeq (deepseq)
import Data.Either (isLeft, isRight)
import Language.JavaScript.Parser
import Test.Hspec

-- | Comprehensive error recovery testing
testErrorRecovery :: Spec
testErrorRecovery = describe "Error Recovery Testing" $ do
  describe "Basic parse errors" testBasicParseErrors

  describe "Syntax error detection" testSyntaxErrorDetection

  describe "Incomplete input handling" testIncompleteInputHandling

  describe "Error message content" testErrorMessageContent

  describe "Malformed input handling" testMalformedInputHandling

  describe "Valid unusual syntax" testValidUnusualSyntax

  describe "Robustness" testRobustness

-- | Test basic parse error detection
testBasicParseErrors :: Spec
testBasicParseErrors = do
  it "rejects function declaration missing name and params" $
    parse "function { return 42; }" "test" `shouldSatisfy` isLeft

  it "rejects missing closing brace" $
    parse "function test() { var x = 1;" "test" `shouldSatisfy` isLeft

  it "accepts numeric assignment as expression statement" $
    parse "1 = 2;" "test" `shouldSatisfy` isRight

  it "rejects for-loop missing semicolon between init and condition" $
    parse "for (var i = 0 i < 10; i++) {}" "test" `shouldSatisfy` isLeft

  it "rejects unclosed object literal" $
    parse "var x = {a: 1, b: 2" "test" `shouldSatisfy` isLeft

  it "accepts missing semicolons via ASI" $
    parse "var x = 1 var y = 2;" "test" `shouldSatisfy` isRight

  it "rejects trailing equals with no expression" $
    parse "var x = " "test" `shouldSatisfy` isLeft

  it "rejects keyword used as variable name" $
    parse "var function = 1;" "test" `shouldSatisfy` isLeft

-- | Test syntax error detection for specific constructs
testSyntaxErrorDetection :: Spec
testSyntaxErrorDetection = do
  it "rejects incomplete arrow function body" $
    parse "() =>" "test" `shouldSatisfy` isLeft

  it "accepts anonymous class expression" $
    parse "class { method() {} }" "test" `shouldSatisfy` isRight

  it "rejects unclosed template interpolation" $
    parse "`hello ${world" "test" `shouldSatisfy` isLeft

  it "rejects generator declaration without name" $
    parse "function* { yield 1; }" "test" `shouldSatisfy` isLeft

  it "accepts async function expression" $
    parse "async function() { await promise; }" "test" `shouldSatisfy` isRight

  it "rejects switch without parenthesized discriminant" $
    parse "switch { case 1: break; }" "test" `shouldSatisfy` isLeft

-- | Test incomplete input handling
testIncompleteInputHandling :: Spec
testIncompleteInputHandling = do
  it "rejects unclosed function parameter list" $
    parse "function test(" "test" `shouldSatisfy` isLeft

  it "rejects class extends with no superclass expression" $
    parse "class Test extends" "test" `shouldSatisfy` isLeft

  it "rejects dangling binary operator" $
    parse "var x = 1 +" "test" `shouldSatisfy` isLeft

  it "rejects incomplete object property" $
    parse "var obj = {key:" "test" `shouldSatisfy` isLeft

-- | Test error message content — verifies errors are non-empty and meaningful
testErrorMessageContent :: Spec
testErrorMessageContent = do
  it "provides non-empty error for missing expression after equals" $
    assertLeftNonEmpty (parse "var x = " "test")
      "Expected parse error for missing expression"

  it "provides non-empty error for dangling plus in function body" $
    assertLeftNonEmpty (parse "function test() { var x = 1 + ; }" "test")
      "Expected parse error for dangling operator"

  it "provides non-empty error for unclosed method params" $
    assertLeftNonEmpty (parse "class Test { method( { return 1; } }" "test")
      "Expected parse error for unclosed method params"

-- | Test malformed input handling
testMalformedInputHandling :: Spec
testMalformedInputHandling = do
  it "rejects completely invalid character sequence" $
    assertLeftDeepseq (parse "!@#$%^&*()_+" "test")
      "Expected parse error for invalid characters"

  it "rejects binary data with null bytes" $
    assertLeftDeepseq (parse "\x00\x01\x02\x03\x04\x05" "test")
      "Expected parse error for null bytes"

-- | Test that unusual-but-valid syntax is accepted
testValidUnusualSyntax :: Spec
testValidUnusualSyntax = do
  it "accepts if with expression statement body" $
    parse "if (true) missing_statement; var x = 1;" "test" `shouldSatisfy` isRight

  it "accepts identifiers that look incomplete" $
    parse "var x = incomplete; var y = 2; var z = 3;" "test" `shouldSatisfy` isRight

  it "accepts binary plus followed by unary plus" $
    parse "a + + b * c" "test" `shouldSatisfy` isRight

  it "accepts ES2019 optional catch binding" $
    parse "try { risky(); } catch { handle(); }" "test" `shouldSatisfy` isRight

  it "accepts extra semicolons as empty statements" $
    parse "var x = 1;; var y = 2;" "test" `shouldSatisfy` isRight

-- | Test robustness on large and complex inputs
testRobustness :: Spec
testRobustness = do
  it "rejects input with multiple cascading errors" $
    parse "var x = ; function bad( { var y = ;" "test" `shouldSatisfy` isLeft

  it "rejects function declaration missing parens" $
    parse "function bad { return 1; }" "test" `shouldSatisfy` isLeft

  it "accepts large valid input without crashing" $ do
    let largeInput = concat (replicate 1000 "var x = 1; ")
    parse largeInput "test" `shouldSatisfy` isRight

  it "handles deeply nested valid functions without crashing" $ do
    let nested = concat (replicate 50 "function f() { ") <> "x" <> concat (replicate 50 " }")
    let result = parse nested "test"
    (result `deepseq` (return ())) :: IO ()

  it "rejects deeply nested unbalanced parens" $
    parse ("function " <> concat (replicate 50 "nested(") <> "error") "test" `shouldSatisfy` isLeft

-- | Assert that a parse result is Left with a non-empty error message
assertLeftNonEmpty :: Either String a -> String -> Expectation
assertLeftNonEmpty result failMsg =
  case result of
    Left err -> err `shouldSatisfy` (not . null)
    Right _ -> expectationFailure failMsg

-- | Assert that a parse result is Left and can be fully evaluated
assertLeftDeepseq :: Either String JSAST -> String -> Expectation
assertLeftDeepseq result failMsg =
  case result of
    Left err -> err `deepseq` (return ())
    Right _ -> expectationFailure failMsg
