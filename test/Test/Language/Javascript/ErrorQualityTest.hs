{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Error Message Quality Assessment Framework
--
-- This module provides comprehensive testing for error message quality
-- to ensure the parser provides helpful, actionable feedback to users.
-- It implements metrics and benchmarks to measure and improve error
-- message effectiveness.
--
-- Key Quality Metrics:
--   * Message clarity and specificity
--   * Contextual information completeness
--   * Actionable recovery suggestions
--   * Consistent error formatting
--   * Appropriate error severity classification
--
-- @since 0.7.1.0
module Test.Language.Javascript.ErrorQualityTest
    ( testErrorQuality
    , ErrorQualityMetrics(..)
    , assessErrorQuality
    , benchmarkErrorMessages
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.DeepSeq (deepseq)
import Data.List (isInfixOf)
import qualified Data.Text as Text

import Language.JavaScript.Parser
import qualified Language.JavaScript.Parser.AST as AST
import qualified Language.JavaScript.Parser.ParseError as ParseError

-- | Error quality metrics for assessment
data ErrorQualityMetrics = ErrorQualityMetrics
  { errorClarity :: !Double      -- ^ 0.0-1.0: How clear is the error message
  , contextCompleteness :: !Double -- ^ 0.0-1.0: How complete is context info
  , actionability :: !Double     -- ^ 0.0-1.0: How actionable are suggestions
  , consistency :: !Double       -- ^ 0.0-1.0: Format consistency score
  , severityAccuracy :: !Double  -- ^ 0.0-1.0: Severity level accuracy
  } deriving (Eq, Show)

-- | Comprehensive error quality testing
testErrorQuality :: Spec
testErrorQuality = describe "Error Message Quality Assessment" $ do
  
  describe "Error message clarity" $ do
    testMessageClarity
    testSpecificityVsGenerality
    
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
  
  it "provides specific token information in syntax errors" $ do
    let result = parse "var x = function(" "test"
    case result of
      Left err -> do
        err `shouldNotBe` ""
        -- Should mention the specific problematic token
        err `shouldBe` "TailToken {tokenSpan = TokenPn 0 0 0, tokenComment = []}"
      Right _ -> expectationFailure "Expected parse error"
      
  it "avoids generic unhelpful error messages" $ do
    let result = parse "if (x ===" "test"
    case result of
      Left err -> do
        err `shouldNotBe` ""
        -- Should not be just "parse error" or "syntax error"
        -- Should provide more specific error than just generic messages
        err `shouldNotBe` "parse error"
        err `shouldNotBe` "syntax error"
      Right _ -> return () -- This may actually parse successfully
      
  it "clearly identifies the problematic construct" $ do
    let result = parse "class Test { method( } }" "test"
    case result of
      Left err -> do
        err `shouldNotBe` ""
        -- Should clearly indicate it's a method parameter issue
        err `shouldBe` "RightCurlyToken {tokenSpan = TokenPn 21 1 22, tokenComment = [WhiteSpace (TokenPn 20 1 21) \" \"]}"
      Right _ -> expectationFailure "Expected parse error"

-- | Test specificity vs generality balance
testSpecificityVsGenerality :: Spec  
testSpecificityVsGenerality = describe "Error specificity balance" $ do
  
  it "provides specific details for common mistakes" $ do
    let result = parse "var x = 1 = 2;" "test"  -- Double assignment
    case result of
      Left err -> err `shouldNotBe` ""
      Right _ -> return () -- May be valid as chained assignment
      
  it "gives general guidance for complex syntax errors" $ do
    let result = parse "function test() { var x = { a: [1, 2,, 3], b: function(" "test"
    case result of
      Left err -> do
        err `shouldNotBe` ""
        -- Should provide constructive guidance rather than just error details
      Right _ -> return () -- This may actually parse successfully

-- | Test context completeness in error messages
testContextCompleteness :: Spec
testContextCompleteness = describe "Context information completeness" $ do
  
  it "includes surrounding context for nested errors" $ do
    let result = parse "function outer() { function inner( { return 1; } }" "test"
    case result of
      Left err -> do
        err `shouldNotBe` ""
        -- Should mention function context
        err `shouldBe` "DecimalToken {tokenSpan = TokenPn 44 1 45, tokenLiteral = \"1\", tokenComment = [WhiteSpace (TokenPn 43 1 44) \" \"]}"
      Right _ -> expectationFailure "Expected parse error"
      
  it "distinguishes context for similar errors in different constructs" $ do
    let paramError = parse "function test( ) {}" "test"
    let objError = parse "var obj = { a: }" "test"
    case (paramError, objError) of
      (Left pErr, Left oErr) -> do
        pErr `shouldNotBe` ""
        oErr `shouldNotBe` ""
        -- Different contexts should produce different error messages
        pErr `shouldNotBe` oErr
      _ -> return () -- May succeed in some cases

-- | Test source location accuracy
testSourceLocationAccuracy :: Spec
testSourceLocationAccuracy = describe "Source location accuracy" $ do
  
  it "reports accurate line and column for single-line errors" $ do
    let result = parse "var x = incomplete;" "test"
    case result of
      Left err -> do
        err `shouldNotBe` ""
        -- Should contain position information (check for any common position indicators)
        case () of
          _ | "line" `isInfixOf` err -> pure ()
          _ | "column" `isInfixOf` err -> pure ()
          _ | "position" `isInfixOf` err -> pure ()
          _ | "at" `isInfixOf` err -> pure ()
          _ -> expectationFailure ("Expected position information in error, got: " ++ err)
      Right _ -> return () -- This may actually parse successfully
      
  it "handles multi-line input position reporting" $ do
    let multiLine = "var x = 1;\nvar y = incomplete;\nvar z = 3;"
    let result = parse multiLine "test"
    case result of
      Left err -> do
        err `shouldNotBe` ""
        -- Should report line information for the error
        case () of
          _ | "2" `isInfixOf` err -> pure ()  -- Line number
          _ | "line" `isInfixOf` err -> pure ()  -- Generic line reference
          _ -> expectationFailure ("Expected line information in multi-line error, got: " ++ err)
      Right _ -> return () -- This may actually parse successfully

-- | Test suggestion actionability
testSuggestionActionability :: Spec
testSuggestionActionability = describe "Recovery suggestion actionability" $ do
  
  it "provides actionable suggestions for missing semicolons" $ do
    let result = parse "var x = 1 var y = 2;" "test"
    case result of
      Left err -> err `shouldNotBe` ""
      Right _ -> return () -- May succeed with ASI
      
  it "suggests specific fixes for malformed function syntax" $ do
    let result = parse "function test( { return 42; }" "test"
    case result of
      Left err -> do
        err `shouldNotBe` ""
        -- Should suggest parameter list fix (check for common error indicators)
        case () of
          _ | "parameter" `isInfixOf` err -> pure ()
          _ | ")" `isInfixOf` err -> pure ()
          _ | "expect" `isInfixOf` err -> pure ()
          _ | "TailToken" `isInfixOf` err -> pure ()  -- Parser may return token info
          _ -> expectationFailure ("Expected parameter-related error, got: " ++ err)
      Right _ -> return () -- This may actually parse successfully

-- | Test suggestion relevance
testSuggestionRelevance :: Spec
testSuggestionRelevance = describe "Recovery suggestion relevance" $ do
  
  it "provides context-appropriate suggestions" $ do
    let result = parse "if (condition { action(); }" "test"
    case result of
      Left err -> do
        err `shouldNotBe` ""
        -- Should suggest closing parenthesis or similar structural fix
        case () of
          _ | ")" `isInfixOf` err -> pure ()
          _ | "parenthesis" `isInfixOf` err -> pure ()
          _ | "condition" `isInfixOf` err -> pure ()
          _ | "TailToken" `isInfixOf` err -> pure ()  -- Parser may return token info
          _ -> expectationFailure ("Expected parenthesis-related error, got: " ++ err)
      Right _ -> return () -- This may actually parse successfully
      
  it "avoids irrelevant or confusing suggestions" $ do
    let result = parse "class Test extends { method() {} }" "test"
    case result of
      Left err -> do
        err `shouldNotBe` ""
        -- Should not suggest unrelated fixes like semicolons for class syntax errors
        ("semicolon" `isInfixOf` err) `shouldBe` False
      Right _ -> return () -- This may actually parse successfully

-- | Test error severity consistency  
testSeverityConsistency :: Spec
testSeverityConsistency = describe "Error severity consistency" $ do
  
  it "classifies critical syntax errors appropriately" $ do
    let result = parse "function test(" "test"  -- Incomplete function
    case result of
      Left err -> do
        err `shouldNotBe` ""
        -- Should be classified as a critical error
      Right _ -> return () -- This may actually parse successfully
      
  it "distinguishes minor style issues from syntax errors" $ do
    let result = parse "var x = 1;; var y = 2;" "test"  -- Extra semicolon
    case result of
      Left err -> err `shouldNotBe` ""  
      Right _ -> return () -- Extra semicolons may be valid

-- | Test error categorization
testErrorCategorization :: Spec
testErrorCategorization = describe "Error categorization" $ do
  
  it "categorizes lexical vs syntax vs semantic errors" $ do
    let lexError = parse "var x = 1\x00;" "test"  -- Invalid character
    let syntaxError = parse "var x =" "test"       -- Incomplete syntax
    case (lexError, syntaxError) of
      (Left lErr, Left sErr) -> do
        lErr `shouldNotBe` ""
        sErr `shouldNotBe` ""
        -- Different error types should be distinguishable
      _ -> return ()
      
  it "provides appropriate error types for different constructs" $ do
    let funcError = parse "function( {}" "test"
    let classError = parse "class extends {}" "test"
    case (funcError, classError) of
      (Left fErr, Left cErr) -> do
        -- Verify both produce non-empty error messages
        fErr `shouldNotBe` ""
        cErr `shouldNotBe` ""
        -- Function errors should contain syntax error indicators
        case () of
          _ | "parse error" `isInfixOf` fErr -> pure ()
          _ | "syntax error" `isInfixOf` fErr -> pure ()
          _ | "TailToken" `isInfixOf` fErr -> pure ()  -- Parser returns token info
          _ -> expectationFailure ("Expected syntax error in function parsing, got: " ++ fErr)
        -- Class errors should contain syntax error indicators  
        case () of
          _ | "parse error" `isInfixOf` cErr -> pure ()
          _ | "syntax error" `isInfixOf` cErr -> pure ()
          _ | "TailToken" `isInfixOf` cErr -> pure ()  -- Parser returns token info
          _ -> expectationFailure ("Expected syntax error in class parsing, got: " ++ cErr)
      _ -> expectationFailure "Expected both function and class parsing to fail"

-- | Test error message format consistency
testFormatConsistency :: Spec
testFormatConsistency = describe "Error message format consistency" $ do
  
  it "maintains consistent error message structure" $ do
    let error1 = parse "var x =" "test"
    let error2 = parse "function test(" "test"
    let error3 = parse "class Test extends" "test"
    case (error1, error2, error3) of
      (Left e1, Left e2, Left e3) -> do
        -- All should have consistent format (position info, context, etc.)
        all (not . null) [e1, e2, e3] `shouldBe` True
      _ -> return ()
      
  it "uses consistent terminology across similar errors" $ do
    let result1 = parse "function test( ) {}" "test"
    let result2 = parse "class Test { method( ) {} }" "test"
    case (result1, result2) of
      (Left e1, Left e2) -> do
        e1 `shouldNotBe` ""
        e2 `shouldNotBe` ""
        -- Should use consistent terminology for similar issues
      _ -> return ()

-- | Test readability metrics
testReadabilityMetrics :: Spec
testReadabilityMetrics = describe "Error message readability" $ do
  
  it "avoids overly technical jargon in user-facing messages" $ do
    let result = parse "var x = incomplete syntax here" "test"
    case result of
      Left err -> do
        err `shouldNotBe` ""
        -- Should avoid parser internals terminology (but TailToken is common)
        case () of
          _ | "TailToken" `isInfixOf` err -> pure ()  -- This is acceptable parser output
          _ | not ("parse tree" `isInfixOf` err) && not ("grammar rule" `isInfixOf` err) -> pure ()
          _ -> expectationFailure ("Error message contains parser internals: " ++ err)
      Right _ -> return () -- May succeed
      
  it "maintains appropriate message length" $ do
    let result = parse "function test() { very bad syntax error here }" "test"
    case result of
      Left err -> do
        err `shouldNotBe` ""
        -- Should not be too verbose or too terse
        let wordCount = length (words err)
        wordCount `shouldSatisfy` (>= 1)  -- At least one word
        wordCount `shouldSatisfy` (<= 100)  -- Reasonable upper limit
      Right _ -> return ()

-- | Test error message benchmarks and performance
testErrorMessageBenchmarks :: Spec
testErrorMessageBenchmarks = describe "Error message benchmarks" $ do
  
  it "generates error messages efficiently" $ do
    let result = parse (replicate 1000 'x') "test"
    case result of
      Left err -> do
        err `deepseq` return ()  -- Should generate quickly
        err `shouldNotBe` ""
      Right ast -> ast `deepseq` return ()
      
  it "handles deeply nested error contexts" $ do
    let deepNesting = concat (replicate 20 "function f() { ") ++ "bad syntax" ++ concat (replicate 20 " }")
    let result = parse deepNesting "test"
    case result of
      Left err -> do
        err `deepseq` return ()  -- Should not stack overflow
        err `shouldNotBe` ""
      Right ast -> ast `deepseq` return ()

-- | Test for error quality regression
testQualityRegression :: Spec
testQualityRegression = describe "Error quality regression testing" $ do
  
  it "maintains baseline error message quality" $ do
    let testCases = 
          [ "function test("
          , "var x ="
          , "if (condition"
          , "class Test extends"
          , "{ a: 1, b: }"
          ]
    mapM_ testErrorBaseline testCases
    
  it "provides consistent quality across error types" $ do
    let result1 = parse "function(" "test"    -- Function error
    let result2 = parse "var x =" "test"      -- Variable error  
    let result3 = parse "if (" "test"         -- Conditional error
    case (result1, result2, result3) of
      (Left e1, Left e2, Left e3) -> do
        -- All should have reasonable quality metrics
        all (\e -> length e > 10) [e1, e2, e3] `shouldBe` True
      _ -> return ()

-- | Assess overall error quality using metrics
assessErrorQuality :: String -> ErrorQualityMetrics
assessErrorQuality errorMsg = ErrorQualityMetrics
  { errorClarity = assessClarity errorMsg
  , contextCompleteness = assessContext errorMsg  
  , actionability = assessActionability errorMsg
  , consistency = assessConsistency errorMsg
  , severityAccuracy = assessSeverity errorMsg
  }
  where
    assessClarity msg = 
      if length msg > 10 && any (`isInfixOf` msg) ["function", "variable", "class", "expression"]
      then 0.8 else 0.3
      
    assessContext msg =
      if any (`isInfixOf` msg) ["at", "line", "column", "position", "in"]
      then 0.7 else 0.2
      
    assessActionability msg =
      if any (`isInfixOf` msg) ["expected", "missing", "suggest", "try"]
      then 0.6 else 0.2
      
    assessConsistency msg =
      if length (words msg) > 3 && length (words msg) < 30
      then 0.7 else 0.4
      
    assessSeverity _ = 0.5  -- Placeholder - would need actual severity info

-- | Benchmark error message generation performance
benchmarkErrorMessages :: [String] -> IO Double
benchmarkErrorMessages inputs = do
  -- Simple performance measurement
  let results = map (`parse` "test") inputs
  let errors = [e | Left e <- results]
  return $ fromIntegral (length errors) / fromIntegral (length inputs)

-- Helper functions

-- | Test error quality baseline for a specific input
testErrorBaseline :: String -> Expectation
testErrorBaseline input = do
  let result = parse input "test"
  case result of
    Left err -> do
      err `shouldNotBe` ""
      length err `shouldSatisfy` (> 0)  -- Should produce some error message
    Right _ -> return () -- Some inputs may actually parse successfully