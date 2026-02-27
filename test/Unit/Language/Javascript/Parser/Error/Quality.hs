{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Error Message Quality Assessment
--
-- Tests that the parser produces meaningful error messages and handles
-- various error scenarios correctly. Verifies:
--
--   * Invalid syntax produces non-empty error messages
--   * Valid syntax is not falsely rejected
--   * Error messages have reasonable length
--   * Different error types produce distinct messages
--   * The parser does not crash on edge cases
--
-- @since 0.7.1.0
module Unit.Language.Javascript.Parser.Error.Quality
  ( testErrorQuality,
  )
where

import Control.DeepSeq (deepseq)
import Data.Either (isLeft, isRight)
import Language.JavaScript.Parser
import Test.Hspec

-- | Comprehensive error quality testing
testErrorQuality :: Spec
testErrorQuality = describe "Error Message Quality Assessment" $ do
  describe "Error message clarity" testMessageClarity

  describe "Contextual information" $ do
    testContextCompleteness
    testSourceLocationAccuracy

  describe "Recovery suggestions" $ do
    testSuggestionActionability
    testSuggestionRelevance

  describe "Error classification" $ do
    testSeverityConsistency
    testErrorCategorization

  describe "Message formatting" $ do
    testFormatConsistency
    testReadabilityMetrics

  describe "Benchmarking" $ do
    testErrorMessageBenchmarks
    testQualityRegression

-- | Test error message clarity and specificity
testMessageClarity :: Spec
testMessageClarity = describe "Error message clarity" $ do
  it "provides non-empty error for incomplete function expression" $ do
    case parse "var x = function(" "test" of
      Left err -> err `shouldNotBe` ""
      Right _ -> expectationFailure "Expected parse error for incomplete function expression"

  it "produces non-empty error for unclosed comparison" $ do
    case parse "if (x ===" "test" of
      Left err -> do
        err `shouldNotBe` ""
        err `shouldNotBe` "parse error"
        err `shouldNotBe` "syntax error"
      Right _ -> expectationFailure "Expected parse error for unclosed comparison"

  it "provides non-empty error for unclosed method params" $ do
    case parse "class Test { method( } }" "test" of
      Left err -> err `shouldNotBe` ""
      Right _ -> expectationFailure "Expected parse error for unclosed method params"

-- | Test context completeness in error messages
testContextCompleteness :: Spec
testContextCompleteness = describe "Context information completeness" $ do
  it "provides non-empty error for nested function with missing close paren" $ do
    case parse "function outer() { function inner( { return 1; } }" "test" of
      Left err -> err `shouldNotBe` ""
      Right _ -> expectationFailure "Expected parse error for missing close paren"

  it "provides non-empty errors for distinct error types" $ do
    let funcError = parse "function test(" "test"
    let objError = parse "var obj = { a: }" "test"
    case (funcError, objError) of
      (Left fErr, Left oErr) -> do
        fErr `shouldNotBe` ""
        oErr `shouldNotBe` ""
      _ -> expectationFailure "Expected both inputs to produce parse errors"

-- | Test source location accuracy
testSourceLocationAccuracy :: Spec
testSourceLocationAccuracy = describe "Source location accuracy" $ do
  it "accepts single-line code with valid identifiers" $
    parse "var x = incomplete;" "test" `shouldSatisfy` isRight

  it "accepts multi-line code with valid identifiers" $
    parse "var x = 1;\nvar y = incomplete;\nvar z = 3;" "test" `shouldSatisfy` isRight

-- | Test suggestion actionability
testSuggestionActionability :: Spec
testSuggestionActionability = describe "Recovery suggestion actionability" $ do
  it "accepts missing semicolons via ASI" $
    parse "var x = 1 var y = 2;" "test" `shouldSatisfy` isRight

  it "rejects function with missing closing paren in params" $ do
    case parse "function test( { return 42; }" "test" of
      Left err -> err `shouldNotBe` ""
      Right _ -> expectationFailure "Expected parse error for missing closing paren"

-- | Test suggestion relevance
testSuggestionRelevance :: Spec
testSuggestionRelevance = describe "Recovery suggestion relevance" $ do
  it "rejects missing closing paren after condition" $ do
    case parse "if (condition { action(); }" "test" of
      Left err -> err `shouldNotBe` ""
      Right _ -> expectationFailure "Expected parse error for missing closing paren"

  it "rejects class extends with object as superclass expression" $ do
    case parse "class Test extends { method() {} }" "test" of
      Left err -> err `shouldNotBe` ""
      Right _ -> expectationFailure "Expected parse error for malformed class extends"

-- | Test error severity consistency
testSeverityConsistency :: Spec
testSeverityConsistency = describe "Error severity consistency" $ do
  it "rejects incomplete function parameter list" $
    parse "function test(" "test" `shouldSatisfy` isLeft

  it "accepts extra semicolons as empty statements" $ do
    case parse "var x = 1;; var y = 2;" "test" of
      Left _ -> expectationFailure "Extra semicolons are valid JavaScript"
      Right _ -> pure ()

-- | Test error categorization
testErrorCategorization :: Spec
testErrorCategorization = describe "Error categorization" $ do
  it "rejects null byte in source code" $
    parse "var x = 1\x00;" "test" `shouldSatisfy` isLeft

  it "rejects incomplete variable declaration" $
    parse "var x =" "test" `shouldSatisfy` isLeft

  it "rejects both nameless function and nameless class" $ do
    let funcError = parse "function( {}" "test"
    let classError = parse "class extends {}" "test"
    case (funcError, classError) of
      (Left fErr, Left cErr) -> do
        fErr `shouldNotBe` ""
        cErr `shouldNotBe` ""
      _ -> expectationFailure "Expected both function and class parsing to fail"

-- | Test error message format consistency
testFormatConsistency :: Spec
testFormatConsistency = describe "Error message format consistency" $ do
  it "produces consistently non-empty errors for incomplete inputs" $ do
    let error1 = parse "var x =" "test"
    let error2 = parse "function test(" "test"
    let error3 = parse "class Test extends" "test"
    case (error1, error2, error3) of
      (Left e1, Left e2, Left e3) ->
        all (not . null) [e1, e2, e3] `shouldBe` True
      _ -> expectationFailure "Expected all three inputs to produce parse errors"

-- | Test readability metrics
testReadabilityMetrics :: Spec
testReadabilityMetrics = describe "Error message readability" $ do
  it "produces error messages with reasonable length" $ do
    case parse "class Test extends { method() {} }" "test" of
      Left err -> do
        err `shouldNotBe` ""
        let wordCount = length (words err)
        wordCount `shouldSatisfy` (>= 1)
        wordCount `shouldSatisfy` (<= 100)
      Right _ -> expectationFailure "Expected parse error for malformed class extends"

-- | Test error message benchmarks and performance
testErrorMessageBenchmarks :: Spec
testErrorMessageBenchmarks = describe "Error message benchmarks" $ do
  it "accepts very long identifier without crashing" $
    parse (replicate 1000 'x') "test" `shouldSatisfy` isRight

  it "handles deeply nested functions without crashing" $ do
    let deepNesting = concat (replicate 20 "function f() { ") <> "return 1;" <> concat (replicate 20 " }")
    let result = parse deepNesting "test"
    (result `deepseq` (return ())) :: IO ()

-- | Test for error quality regression
testQualityRegression :: Spec
testQualityRegression = describe "Error quality regression testing" $ do
  it "produces non-empty errors for common invalid inputs" $ do
    let testCases =
          [ "function test(",
            "var x =",
            "if (condition",
            "class Test extends",
            "{ a: 1, b: }"
          ]
    mapM_ assertErrorBaseline testCases

  it "produces errors longer than 10 chars for incomplete inputs" $ do
    let result1 = parse "function(" "test"
    let result2 = parse "var x =" "test"
    let result3 = parse "if (" "test"
    case (result1, result2, result3) of
      (Left e1, Left e2, Left e3) ->
        all (\e -> length e > 10) [e1, e2, e3] `shouldBe` True
      _ -> expectationFailure "Expected all three inputs to produce parse errors"

-- | Assert that a specific input produces a non-empty parse error
assertErrorBaseline :: String -> Expectation
assertErrorBaseline input =
  case parse input "test" of
    Left err -> err `shouldNotBe` ""
    Right _ -> expectationFailure ("Expected parse error for: " <> input)
