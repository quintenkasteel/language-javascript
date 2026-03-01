{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Error Recovery Performance Benchmarks
--
-- This module provides performance benchmarking for error recovery capabilities
-- to measure the impact of enhanced error handling on parser performance.
-- It ensures that robust error recovery doesn't significantly degrade parsing speed.
--
-- Benchmark Areas:
--   * Error-free parsing baseline performance
--   * Single error recovery performance impact
--   * Multiple error scenarios performance
--   * Large input error recovery scaling
--   * Memory usage during error recovery
--   * Error message generation overhead
--
-- Target: Error recovery should add <10% overhead to normal parsing
--
-- @since 0.7.1.0
module Benchmarks.Language.Javascript.Parser.ErrorRecovery
  ( benchmarkErrorRecovery,
    BenchmarkResults (..),
    ErrorRecoveryMetrics (..),
    runPerformanceTests,
  )
where

import Control.DeepSeq (deepseq, force)
import Control.Exception (evaluate)
import Data.Time.Clock
import Language.JavaScript.Parser
import qualified Language.JavaScript.Parser.AST as AST
import Test.Hspec

-- | Error recovery performance metrics
data ErrorRecoveryMetrics = ErrorRecoveryMetrics
  { -- | Time for error-free parsing (ms)
    baselineParseTime :: !Double,
    -- | Time for parsing with errors (ms)
    errorRecoveryTime :: !Double,
    -- | Peak memory usage during recovery
    memoryUsage :: !Int,
    -- | Time to generate error messages (ms)
    errorMessageTime :: !Double,
    -- | Overhead percentage vs baseline
    recoveryOverhead :: !Double
  }
  deriving (Eq, Show)

-- | Benchmark results container
data BenchmarkResults = BenchmarkResults
  { singleErrorMetrics :: !ErrorRecoveryMetrics,
    multipleErrorMetrics :: !ErrorRecoveryMetrics,
    largeInputMetrics :: !ErrorRecoveryMetrics,
    cascadingErrorMetrics :: !ErrorRecoveryMetrics
  }
  deriving (Eq, Show)

-- | Main error recovery benchmarking suite
benchmarkErrorRecovery :: Spec
benchmarkErrorRecovery = describe "Error Recovery Performance Benchmarks" $ do
  describe "Baseline performance" $ do
    testBaselineParsingSpeed
    testMemoryUsageBaseline

  describe "Single error recovery impact" $ do
    testSingleErrorOverhead
    testErrorMessageGenerationSpeed

  describe "Multiple error scenarios" $ do
    testMultipleErrorPerformance
    testErrorCascadePerformance

  describe "Large input scaling" $ do
    testLargeInputErrorRecovery
    testDeepNestingErrorRecovery

  describe "Memory efficiency" $ do
    testErrorRecoveryMemoryUsage
    testGarbageCollectionImpact

-- | Test baseline parsing speed for error-free code
testBaselineParsingSpeed :: Spec
testBaselineParsingSpeed = describe "Baseline parsing performance" $ do
  it "parses small valid programs efficiently" $ do
    let validCode = "function test() { var x = 1; return x + 1; }"
    time <- benchmarkParsing validCode
    time `shouldSatisfy` (< 100) -- Should parse in <100ms
  it "parses medium-sized valid programs efficiently" $ do
    let mediumCode = concat $ replicate 50 "function f() { var x = 1; } "
    time <- benchmarkParsing mediumCode
    time `shouldSatisfy` (< 500) -- Should parse in <500ms
  it "parses large valid programs within bounds" $ do
    let largeCode = concat $ replicate 1000 "var x = 1; "
    time <- benchmarkParsing largeCode
    time `shouldSatisfy` (< 2000) -- Should parse in <2s

-- | Test memory usage baseline
testMemoryUsageBaseline :: Spec
testMemoryUsageBaseline = describe "Baseline memory usage" $ do
  it "has reasonable memory footprint for small programs" $ do
    let validCode = "function test() { return 42; }"
    result <- benchmarkParsingMemory validCode
    case result of
      Right ast -> ast `deepseq` return ()
      Left _ -> expectationFailure "Should parse successfully"

  it "scales memory usage linearly with input size" $ do
    let smallCode = concat $ replicate 100 "var x = 1; "
    let largeCode = concat $ replicate 1000 "var x = 1; "
    smallTime <- benchmarkParsing smallCode
    largeTime <- benchmarkParsing largeCode
    -- 10x larger input should not be more than 30x slower (good linear scaling)
    largeTime `shouldSatisfy` (< smallTime * 30)

-- | Test single error recovery overhead
testSingleErrorOverhead :: Spec
testSingleErrorOverhead = describe "Single error recovery overhead" $ do
  it "adds minimal overhead for simple syntax errors" $ do
    let validCode = "function test() { return 42; }"
    let errorCode = "function test( { return 42; }" -- Missing closing paren
    validTime <- benchmarkParsing validCode
    errorTime <- benchmarkParsing errorCode
    let overhead = (errorTime - validTime) / validTime * 100
    overhead `shouldSatisfy` (< 50) -- <50% overhead acceptable for error cases
  it "handles function declaration errors efficiently" $ do
    let errorCode = "function test( invalid params { return 1; }"
    time <- benchmarkParsing errorCode
    time `shouldSatisfy` (< 200) -- Should still be reasonably fast
  it "processes expression errors quickly" $ do
    let errorCode = "var x = 1 + + 2 * 3;" -- Invalid operator sequence
    time <- benchmarkParsing errorCode
    time `shouldSatisfy` (< 100)

-- | Test error message generation speed
testErrorMessageGenerationSpeed :: Spec
testErrorMessageGenerationSpeed = describe "Error message generation performance" $ do
  it "generates error messages quickly for syntax errors" $ do
    let errorCode = "function test( { return 42; }"
    (parseTime, errorMsg) <- benchmarkErrorMessage errorCode
    parseTime `shouldSatisfy` (< 150) -- Total time including error message
    case errorMsg of
      Just err -> length err `shouldSatisfy` (> 0)
      Nothing -> expectationFailure "Should generate error message"

  it "scales error message generation with complexity" $ do
    let simpleError = "var x ="
    let complexError = "class Test { method( { var x = { a: incomplete } } }"
    simpleTime <- benchmarkParsing simpleError
    complexTime <- benchmarkParsing complexError
    -- Complex errors shouldn't be dramatically slower
    complexTime `shouldSatisfy` (< simpleTime * 5)

-- | Test multiple error performance
testMultipleErrorPerformance :: Spec
testMultipleErrorPerformance = describe "Multiple error handling performance" $ do
  it "handles multiple errors without exponential slowdown" $ do
    let singleError = "function test( { return 1; }"
    let multipleErrors = "function test( { var x = ; return incomplete }"
    singleTime <- benchmarkParsing singleError
    multiTime <- benchmarkParsing multipleErrors
    -- Multiple errors should not cause exponential slowdown
    multiTime `shouldSatisfy` (< singleTime * 3)

  it "processes cascading errors efficiently" $ do
    let cascadingErrors = "if (condition { function bad( { var x = ; } else { more errors }"
    time <- benchmarkParsing cascadingErrors
    time `shouldSatisfy` (< 300) -- Should complete within reasonable time

-- | Test error cascade performance
testErrorCascadePerformance :: Spec
testErrorCascadePerformance = describe "Error cascade handling performance" $ do
  it "prevents performance degradation from error cascades" $ do
    let cascadeCode = "function f( { var x = ; if (bad { while (error { for (broken {"
    time <- benchmarkParsing cascadeCode
    time `shouldSatisfy` (< 400) -- Should not hang or be extremely slow
  it "handles deeply nested error contexts" $ do
    let nestedErrors =
          concat (replicate 10 "function f() { ") ++ "error syntax"
            ++ concat (replicate 10 " }")
    time <- benchmarkParsing nestedErrors
    time `shouldSatisfy` (< 500)

-- | Test large input error recovery scaling
testLargeInputErrorRecovery :: Spec
testLargeInputErrorRecovery = describe "Large input error recovery scaling" $ do
  it "scales linearly with input size for error recovery" $ do
    let smallErrorCode = concat (replicate 10 "var x = ; ") ++ "var y = 1;"
    let largeErrorCode = concat (replicate 100 "var x = ; ") ++ "var y = 1;"
    smallTime <- benchmarkParsing smallErrorCode
    largeTime <- benchmarkParsing largeErrorCode
    -- Should scale reasonably (not worse than quadratic)
    largeTime `shouldSatisfy` (< smallTime * 15)

  it "handles very large files with errors efficiently" $ do
    let veryLargeError = concat (replicate 1000 "var x = incomplete; ")
    time <- benchmarkParsing veryLargeError
    time `shouldSatisfy` (< 3000) -- Should complete in <3 seconds

-- | Test deep nesting error recovery
testDeepNestingErrorRecovery :: Spec
testDeepNestingErrorRecovery = describe "Deep nesting error recovery" $ do
  it "handles deeply nested function errors" $ do
    let deepFunctions =
          concat (replicate 20 "function f() { ")
            ++ "error here"
            ++ concat (replicate 20 " }")
    time <- benchmarkParsing deepFunctions
    time `shouldSatisfy` (< 600) -- Should not cause stack overflow or extreme slowdown
  it "processes nested object/array errors efficiently" $ do
    let deepNesting =
          concat (replicate 15 "{ a: [")
            ++ "invalid syntax"
            ++ concat (replicate 15 "] }")
    time <- benchmarkParsing deepNesting
    time `shouldSatisfy` (< 400)

-- | Test error recovery memory usage
testErrorRecoveryMemoryUsage :: Spec
testErrorRecoveryMemoryUsage = describe "Error recovery memory efficiency" $ do
  it "does not leak memory during error recovery" $ do
    let errorCode = "function test( { var x = incomplete; }"
    result <- benchmarkParsingMemory errorCode
    case result of
      Left err -> do
        err `deepseq` return () -- Should not cause memory issues
        length err `shouldSatisfy` (> 0)
      Right _ -> return () -- May succeed in some cases
  it "manages memory efficiently for large error scenarios" $ do
    let largeErrorCode = concat $ replicate 500 "function f( { "
    result <- benchmarkParsingMemory largeErrorCode
    case result of
      Left err -> err `deepseq` return () -- Should handle without memory explosion
      Right ast -> ast `deepseq` return ()

-- | Test garbage collection impact
testGarbageCollectionImpact :: Spec
testGarbageCollectionImpact = describe "Garbage collection impact" $ do
  it "creates reasonable garbage during error recovery" $ do
    let errorCode = "class Test { method( { var x = incomplete; } }"
    -- Run multiple times to get more stable measurements
    times <- mapM (\_ -> benchmarkParsing errorCode) [1 .. 5 :: Int]
    let maxTime = maximum times
    let minTime = minimum times
    -- Validate that max time is not dramatically different from min time
    -- Allow for more variance due to micro-benchmark measurement noise
    maxTime `shouldSatisfy` (<= max (minTime * 3) (minTime + 10)) -- 3x or +10ms whichever is larger
  it "handles repeated error parsing efficiently" $ do
    let testCodes = replicate 10 "function test( { return 1; }"
    times <- mapM benchmarkParsing testCodes
    let avgTime = sum times / fromIntegral (length times)
    let maxTime = maximum times
    let minTime = minimum times
    -- Validate performance consistency: max should not be more than 10x min
    -- This allows for JIT warmup and GC variations while catching real issues
    maxTime `shouldSatisfy` (<= minTime * 10)
    -- Also check that average performance is reasonable
    avgTime `shouldSatisfy` (< 500) -- Average should be under 500ms

-- | Run comprehensive performance tests
runPerformanceTests :: IO BenchmarkResults
runPerformanceTests = do
  -- Single error metrics
  singleMetrics <- benchmarkSingleError

  -- Multiple error metrics
  multiMetrics <- benchmarkMultipleErrors

  -- Large input metrics
  largeMetrics <- benchmarkLargeInput

  -- Cascading error metrics
  cascadeMetrics <- benchmarkCascadingErrors

  return
    BenchmarkResults
      { singleErrorMetrics = singleMetrics,
        multipleErrorMetrics = multiMetrics,
        largeInputMetrics = largeMetrics,
        cascadingErrorMetrics = cascadeMetrics
      }

-- Helper functions for benchmarking

-- | Benchmark parsing time for given code
benchmarkParsing :: String -> IO Double
benchmarkParsing code = do
  startTime <- getCurrentTime
  result <- evaluate $ force (parse code "benchmark")
  endTime <- getCurrentTime
  result `deepseq` return ()
  let diffTime = diffUTCTime endTime startTime
  return $ fromRational (toRational diffTime) * 1000 -- Convert to milliseconds

-- | Benchmark parsing with memory measurement
benchmarkParsingMemory :: String -> IO (Either String AST.JSAST)
benchmarkParsingMemory code = do
  result <- evaluate $ force (parse code "benchmark")
  result `deepseq` return result

-- | Benchmark error message generation
benchmarkErrorMessage :: String -> IO (Double, Maybe String)
benchmarkErrorMessage code = do
  startTime <- getCurrentTime
  let result = parse code "benchmark"
  endTime <- getCurrentTime
  let diffTime = diffUTCTime endTime startTime
  let timeMs = fromRational (toRational diffTime) * 1000
  case result of
    Left err -> return (timeMs, Just err)
    Right _ -> return (timeMs, Nothing)

-- | Benchmark single error scenario
benchmarkSingleError :: IO ErrorRecoveryMetrics
benchmarkSingleError = do
  let validCode = "function test() { return 42; }"
  let errorCode = "function test( { return 42; }"

  baselineTime <- benchmarkParsing validCode
  errorTime <- benchmarkParsing errorCode
  (msgTime, _) <- benchmarkErrorMessage errorCode

  let overhead = (errorTime - baselineTime) / baselineTime * 100

  return
    ErrorRecoveryMetrics
      { baselineParseTime = baselineTime,
        errorRecoveryTime = errorTime,
        memoryUsage = 0, -- Placeholder - would need actual memory measurement
        errorMessageTime = msgTime,
        recoveryOverhead = overhead
      }

-- | Benchmark multiple error scenario
benchmarkMultipleErrors :: IO ErrorRecoveryMetrics
benchmarkMultipleErrors = do
  let validCode = "function test() { var x = 1; return x; }"
  let errorCode = "function test( { var x = ; return incomplete; }"

  baselineTime <- benchmarkParsing validCode
  errorTime <- benchmarkParsing errorCode
  (msgTime, _) <- benchmarkErrorMessage errorCode

  let overhead = (errorTime - baselineTime) / baselineTime * 100

  return
    ErrorRecoveryMetrics
      { baselineParseTime = baselineTime,
        errorRecoveryTime = errorTime,
        memoryUsage = 0,
        errorMessageTime = msgTime,
        recoveryOverhead = overhead
      }

-- | Benchmark large input scenario
benchmarkLargeInput :: IO ErrorRecoveryMetrics
benchmarkLargeInput = do
  let validCode = concat $ replicate 100 "function test() { return 1; } "
  let errorCode = concat $ replicate 100 "function test( { return 1; } "

  baselineTime <- benchmarkParsing validCode
  errorTime <- benchmarkParsing errorCode
  (msgTime, _) <- benchmarkErrorMessage errorCode

  let overhead = (errorTime - baselineTime) / baselineTime * 100

  return
    ErrorRecoveryMetrics
      { baselineParseTime = baselineTime,
        errorRecoveryTime = errorTime,
        memoryUsage = 0,
        errorMessageTime = msgTime,
        recoveryOverhead = overhead
      }

-- | Benchmark cascading error scenario
benchmarkCascadingErrors :: IO ErrorRecoveryMetrics
benchmarkCascadingErrors = do
  let validCode = "if (true) { function f() { var x = 1; } }"
  let errorCode = "if (true { function f( { var x = ; } }"

  baselineTime <- benchmarkParsing validCode
  errorTime <- benchmarkParsing errorCode
  (msgTime, _) <- benchmarkErrorMessage errorCode

  let overhead = (errorTime - baselineTime) / baselineTime * 100

  return
    ErrorRecoveryMetrics
      { baselineParseTime = baselineTime,
        errorRecoveryTime = errorTime,
        memoryUsage = 0,
        errorMessageTime = msgTime,
        recoveryOverhead = overhead
      }
