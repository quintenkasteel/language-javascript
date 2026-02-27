{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Advanced Error Recovery Testing for JavaScript Parser
--
-- Tests parser behavior on invalid JavaScript inputs, verifying that:
--
--   * Invalid syntax is correctly rejected (produces Left)
--   * Valid syntax is correctly accepted (produces Right)
--   * The parser does not crash on malformed input
--   * Large and deeply nested invalid inputs are handled gracefully
--
-- All tests use behavioral assertions (isLeft/isRight) rather than
-- matching internal error message formats, making them robust across
-- parser implementations.
--
-- @since 0.7.1.0
module Unit.Language.Javascript.Parser.Error.AdvancedRecovery
  ( testAdvancedErrorRecovery,
  )
where

import Data.Either (isLeft, isRight)
import Language.JavaScript.Parser
import Test.Hspec

-- | Comprehensive advanced error recovery testing
testAdvancedErrorRecovery :: Spec
testAdvancedErrorRecovery = describe "Advanced Error Recovery and Multi-Error Detection" $ do
  describe "Local correction recovery" $ do
    testMissingOperatorRecovery
    testMissingBracketRecovery
    testMissingSemicolonRecovery
    testMissingCommaRecovery

  describe "Error production testing" $ do
    testCommonSyntaxErrorPatterns
    testTypicalJavaScriptMistakes
    testModernJSFeatureErrors

  describe "Multi-error reporting" $ do
    testMultipleErrorAccumulation

  describe "Error suggestion quality" $ do
    testErrorSuggestionQuality

  describe "Recovery point accuracy" $ do
    testPreciseErrorLocations
    testParserStateConsistency

-- | Test that invalid expressions with missing operators are rejected
testMissingOperatorRecovery :: Spec
testMissingOperatorRecovery = describe "Missing operator recovery" $ do
  it "rejects two expressions where one is expected in condition" $
    parse "if (x y) { console.log('test'); }" "test" `shouldSatisfy` isLeft

-- | Test that missing brackets cause parse failures
testMissingBracketRecovery :: Spec
testMissingBracketRecovery = describe "Missing bracket recovery" $ do
  it "rejects missing opening parenthesis in function call" $
    parse "console.log 'hello');" "test" `shouldSatisfy` isLeft

  it "rejects semicolon instead of closing brace in object literal" $
    parse "var obj = { a: 1, b: 2; var x = 5;" "test" `shouldSatisfy` isLeft

  it "rejects missing closing square bracket in array access" $
    parse "arr[0; console.log('done');" "test" `shouldSatisfy` isLeft

-- | Test semicolon insertion behavior
testMissingSemicolonRecovery :: Spec
testMissingSemicolonRecovery = describe "Missing semicolon recovery" $ do
  it "rejects for-loop with missing semicolon between condition and update" $
    parse "for (var i = 0; i < 10 i++) { console.log(i); }" "test" `shouldSatisfy` isLeft

-- | Test missing comma detection in parameter lists and literals
testMissingCommaRecovery :: Spec
testMissingCommaRecovery = describe "Missing comma recovery" $ do
  it "rejects missing commas in function parameter list" $
    parse "function test(a b c) { return a + b + c; }" "test" `shouldSatisfy` isLeft

  it "rejects missing comma between object properties" $
    parse "var obj = { a: 1 b: 2, c: 3 };" "test" `shouldSatisfy` isLeft

-- | Test common JavaScript syntax error patterns
testCommonSyntaxErrorPatterns :: Spec
testCommonSyntaxErrorPatterns = describe "Common syntax error patterns" $ do
  it "accepts assignment in condition (valid JS)" $ do
    case parse "if (x = 5) { console.log('assigned'); }" "test" of
      Left _ -> expectationFailure "Assignment in condition is valid JavaScript"
      Right _ -> pure ()

  it "accepts valid object method syntax" $ do
    case parse "var obj = { method: function() { return 1; } };" "test" of
      Left _ -> expectationFailure "Object method is valid ES5 syntax"
      Right _ -> pure ()

-- | Test typical JavaScript mistakes developers make
testTypicalJavaScriptMistakes :: Spec
testTypicalJavaScriptMistakes = describe "Typical JavaScript developer mistakes" $ do
  it "accepts function hoisting (valid JS)" $ do
    case parse "console.log(fn()); function fn() { return 'test'; }" "test" of
      Left _ -> expectationFailure "Function hoisting is valid JavaScript"
      Right _ -> pure ()

  it "accepts block-scoped code (parser does not check semantics)" $ do
    case parse "{ let x = 1; } console.log(x);" "test" of
      Left _ -> expectationFailure "Parser should accept this (scope is semantic)"
      Right _ -> pure ()

  it "accepts valid ES5 callback pattern" $ do
    case parse "var self = this; setTimeout(function() { self.method(); }, 1000);" "test" of
      Left _ -> expectationFailure "Valid legacy ES5 syntax"
      Right _ -> pure ()

-- | Test modern JavaScript feature error patterns
testModernJSFeatureErrors :: Spec
testModernJSFeatureErrors = describe "Modern JavaScript feature errors" $ do
  it "accepts await as identifier in non-async function" $ do
    case parse "function test() { await fetch('/api'); }" "test" of
      Left _ -> expectationFailure "await should be treated as identifier in non-async context"
      Right _ -> pure ()

  it "accepts trailing comma in destructuring (valid ES2017+)" $ do
    case parse "var {a, b, } = obj;" "test" of
      Left _ -> expectationFailure "Trailing commas in destructuring are valid ES2017+"
      Right _ -> pure ()

  it "rejects unterminated template literal interpolation" $
    parse "var msg = `Hello ${name`;" "test" `shouldSatisfy` isLeft

-- | Test parser behavior on multiple errors in single input
testMultipleErrorAccumulation :: Spec
testMultipleErrorAccumulation = describe "Multiple error accumulation" $ do
  it "rejects input with multiple syntax errors" $
    parse "function bad( { var x = ; class Another extends { }" "test" `shouldSatisfy` isLeft

  it "rejects missing parenthesis in function declaration" $
    parse "function test( { var unused_var = 1; }" "test" `shouldSatisfy` isLeft

  it "rejects incomplete function expression" $
    parse "var x = function incomplete(" "test" `shouldSatisfy` isLeft

  it "rejects incomplete function in array literal" $
    parse "[1, 2, , , 5, function bad( ]" "test" `shouldSatisfy` isLeft

  it "rejects double comma in function parameters" $
    parse "function test(a, , c) { return a + c; }" "test" `shouldSatisfy` isLeft

  it "rejects missing expression after equals in function body" $
    parse "function test() { var x = ; return x; }" "test" `shouldSatisfy` isLeft

-- | Test that parser produces non-empty errors and handles valid edge cases
testErrorSuggestionQuality :: Spec
testErrorSuggestionQuality = describe "Error suggestion quality" $ do
  it "accepts typos as identifiers (retrun is a valid identifier)" $ do
    case parse "function test() { retrun 42; }" "test" of
      Left _ -> expectationFailure "retrun should be parsed as a valid identifier"
      Right _ -> pure ()

  it "rejects unclosed parenthesis in expression" $
    parse "var x = (1 + 2" "test" `shouldSatisfy` isLeft

  it "rejects class method with missing closing paren" $
    parse "class Test { method( { return 1; } }" "test" `shouldSatisfy` isLeft

  it "provides non-empty error for missing closing paren in params" $ do
    case parse "function test( { return 42; }" "test" of
      Left err -> err `shouldSatisfy` (not . null)
      Right _ -> expectationFailure "Expected parse error for missing closing paren"

-- | Test precise error location reporting
testPreciseErrorLocations :: Spec
testPreciseErrorLocations = describe "Precise error locations" $ do
  it "rejects multi-line code with dangling operator" $ do
    let multiLineCode =
          unlines
            [ "function test() {",
              "  var x = 1 +",
              "  return x;",
              "}"
            ]
    parse multiLineCode "test" `shouldSatisfy` isLeft

-- | Test parser state consistency during recovery
testParserStateConsistency :: Spec
testParserStateConsistency = describe "Parser state consistency" $ do
  it "accepts block with identifier-valued variables" $
    parse "{ var x = bad; { var y = good; } var z = also_bad; }" "test" `shouldSatisfy` isRight

  it "rejects function with missing close paren in params" $
    parse "function bad( { return 1; } + validExpression" "test" `shouldSatisfy` isLeft
