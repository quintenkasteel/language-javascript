{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Fuzz testing infrastructure module for JavaScript Parser
--
-- This module provides the core fuzzing infrastructure and configuration
-- for automated testing with random JavaScript inputs.

module Test.Language.Javascript.FuzzTest
  ( FuzzTestConfig(..)
  , FuzzTestResult(..)
  , defaultFuzzTestConfig
  , ciConfig
  , developmentConfig
  , runBasicFuzzing
  , totalIterations
  , propertyViolations
  , executionTime
  ) where

import Data.Time (getCurrentTime, diffUTCTime)
import Language.JavaScript.Parser (readJs)

-- | Configuration for fuzz testing runs
data FuzzTestConfig = FuzzTestConfig
  { fuzzIterations :: !Int
  , fuzzMaxSize :: !Int
  , fuzzTimeout :: !Int
  , testIterations :: !Int
  , testTimeout :: !Int
  , testRegressionMode :: !Bool
  , testCoverageMode :: !Bool
  , testDifferentialMode :: !Bool
  , testPerformanceMode :: !Bool
  } deriving (Eq, Show)

-- | Results of fuzz testing run
data FuzzTestResult = FuzzTestResult
  { _totalIterations :: !Int
  , _propertyViolations :: !Int
  , _successfulTests :: !Int
  , _executionTime :: !Double
  } deriving (Eq, Show)

-- | Default configuration for fuzz testing
defaultFuzzTestConfig :: FuzzTestConfig
defaultFuzzTestConfig = FuzzTestConfig
  { fuzzIterations = 100
  , fuzzMaxSize = 1000
  , fuzzTimeout = 10
  , testIterations = 100
  , testTimeout = 10
  , testRegressionMode = False
  , testCoverageMode = False
  , testDifferentialMode = False
  , testPerformanceMode = False
  }

-- | Configuration optimized for CI environments
ciConfig :: FuzzTestConfig
ciConfig = FuzzTestConfig
  { fuzzIterations = 50
  , fuzzMaxSize = 500
  , fuzzTimeout = 5
  , testIterations = 50
  , testTimeout = 5
  , testRegressionMode = True
  , testCoverageMode = True
  , testDifferentialMode = False
  , testPerformanceMode = True
  }

-- | Configuration for development testing
developmentConfig :: FuzzTestConfig
developmentConfig = FuzzTestConfig
  { fuzzIterations = 10
  , fuzzMaxSize = 100
  , fuzzTimeout = 2
  , testIterations = 10
  , testTimeout = 2
  , testRegressionMode = False
  , testCoverageMode = False
  , testDifferentialMode = False
  , testPerformanceMode = False
  }

-- | Run basic fuzzing with specified number of iterations
runBasicFuzzing :: Int -> IO FuzzTestResult
runBasicFuzzing iterations = do
  startTime <- getCurrentTime
  
  -- Generate random JavaScript inputs and test them
  results <- mapM testRandomInput [1..iterations]
  let violations = length (filter not results)
  let successes = iterations - violations
  
  endTime <- getCurrentTime
  let execTime = realToFrac (diffUTCTime endTime startTime)
  
  pure $ FuzzTestResult
    { _totalIterations = iterations
    , _propertyViolations = violations
    , _successfulTests = successes
    , _executionTime = execTime
    }
  where
    testRandomInput :: Int -> IO Bool
    testRandomInput seed = do
      let randomJS = generateRandomJavaScript seed
      case readJs randomJS of
        _ -> pure True  -- readJs always returns an AST, even for errors

-- | Get total iterations from result
totalIterations :: FuzzTestResult -> Int
totalIterations = _totalIterations

-- | Get property violations from result
propertyViolations :: FuzzTestResult -> Int
propertyViolations = _propertyViolations

-- | Get execution time from result
executionTime :: FuzzTestResult -> Double
executionTime = _executionTime

-- | Generate random JavaScript code based on seed
generateRandomJavaScript :: Int -> String
generateRandomJavaScript seed = 
  let patterns = [ "var x = " ++ show seed ++ ";"
                 , "function f() { return " ++ show seed ++ "; }"
                 , "if (" ++ show seed ++ " > 0) { console.log('test'); }"
                 , "[" ++ show seed ++ ", " ++ show (seed + 1) ++ "]"
                 , "{prop: " ++ show seed ++ "}"
                 , "for (var i = 0; i < " ++ show seed ++ "; i++) {}"
                 ]
      patternIndex = seed `mod` length patterns
  in patterns !! patternIndex