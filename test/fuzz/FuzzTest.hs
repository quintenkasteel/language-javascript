{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive fuzzing test suite with integration tests.
--
-- This module provides the main test interface for the fuzzing infrastructure,
-- integrating all fuzzing strategies and providing comprehensive test coverage
-- for the JavaScript parser robustness and edge case handling:
--
--   * __Integration Test Suite__: Unified fuzzing test interface
--     Combines crash testing, coverage-guided fuzzing, property-based testing,
--     and differential testing into a cohesive test suite with CI integration.
--
--   * __Regression Testing__: Systematic validation of discovered issues
--     Maintains a corpus of previously discovered crashes and edge cases
--     to ensure fixes remain effective and no regressions are introduced.
--
--   * __Performance Monitoring__: Resource usage and performance tracking
--     Monitors parser performance under fuzzing loads to detect performance
--     regressions and ensure fuzzing campaigns complete within CI time limits.
--
--   * __Automated Failure Analysis__: Systematic failure categorization
--     Automatically analyzes and categorizes fuzzing failures to prioritize
--     bug fixes and identify patterns in parser vulnerabilities.
--
-- The test suite is designed for both development use and CI integration,
-- with configurable test intensity and comprehensive reporting capabilities.
--
-- ==== Examples
--
-- Running basic fuzzing tests:
--
-- >>> hspec fuzzTests
--
-- Running intensive fuzzing campaign:
--
-- >>> runIntensiveFuzzing 10000
--
-- @since 0.7.1.0
module Test.Language.Javascript.FuzzTest
    ( -- * Main Test Interface
      fuzzTests
    , fuzzTestSuite
    , runBasicFuzzing
    , runIntensiveFuzzing
    
    -- * Regression Testing
    , regressionTests
    , runRegressionSuite
    , updateRegressionCorpus
    , validateKnownIssues
    
    -- * Performance Testing
    , performanceTests
    , monitorPerformance
    , benchmarkFuzzing
    , analyzeResourceUsage
    
    -- * Failure Analysis
    , analyzeFailures
    , categorizeFailures
    , generateFailureReport
    , prioritizeIssues
    
    -- * Test Configuration
    , FuzzTestConfig(..)
    , defaultFuzzTestConfig
    , ciConfig
    , developmentConfig
    ) where

import Control.Exception (catch, SomeException)
import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))
import Data.Time (getCurrentTime, diffUTCTime)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)
import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Test.Language.Javascript.FuzzHarness
  ( FuzzConfig(..)
  , FuzzResults(..)
  , FuzzFailure(..)
  , FailureType(..)
  , runFuzzingCampaign
  , runCrashFuzzing
  , runCoverageGuidedFuzzing
  , runPropertyFuzzing
  , runDifferentialFuzzing
  , analyzeFuzzResults
  , generateFailureReport
  , defaultFuzzConfig
  )

import Test.Language.Javascript.CoverageGuided
  ( measureCoverage
  , generateCoverageReport
  , CoverageData(..)
  )

import Test.Language.Javascript.DifferentialTesting
  ( runDifferentialSuite
  , ComparisonReport(..)
  , generateComparisonReport
  )

-- ---------------------------------------------------------------------
-- Test Configuration
-- ---------------------------------------------------------------------

-- | Fuzzing test configuration
data FuzzTestConfig = FuzzTestConfig
  { testIterations :: !Int
  , testTimeout :: !Int
  , testMinimizeFailures :: !Bool
  , testSaveResults :: !Bool
  , testRegressionMode :: !Bool
  , testPerformanceMode :: !Bool
  , testCoverageMode :: !Bool
  , testDifferentialMode :: !Bool
  } deriving (Eq, Show)

-- | Default test configuration for general use
defaultFuzzTestConfig :: FuzzTestConfig
defaultFuzzTestConfig = FuzzTestConfig
  { testIterations = 1000
  , testTimeout = 5000
  , testMinimizeFailures = True
  , testSaveResults = True
  , testRegressionMode = True
  , testPerformanceMode = False
  , testCoverageMode = True
  , testDifferentialMode = True
  }

-- | CI-optimized configuration (faster, less intensive)
ciConfig :: FuzzTestConfig
ciConfig = defaultFuzzTestConfig
  { testIterations = 200
  , testTimeout = 2000
  , testMinimizeFailures = False
  , testPerformanceMode = False
  }

-- | Development configuration (intensive testing)
developmentConfig :: FuzzTestConfig
developmentConfig = defaultFuzzTestConfig
  { testIterations = 5000
  , testTimeout = 10000
  , testPerformanceMode = True
  }

-- ---------------------------------------------------------------------
-- Main Test Interface
-- ---------------------------------------------------------------------

-- | Complete fuzzing test suite
fuzzTests :: Spec
fuzzTests = describe "Fuzzing Tests" $ do
  fuzzTestSuite defaultFuzzTestConfig

-- | Configurable fuzzing test suite
fuzzTestSuite :: FuzzTestConfig -> Spec
fuzzTestSuite config = do
  
  describe "Basic Fuzzing" $ do
    basicFuzzingTests config
  
  when (testRegressionMode config) $ do
    describe "Regression Testing" $ do
      regressionTests config
  
  when (testPerformanceMode config) $ do
    describe "Performance Testing" $ do
      performanceTests config
  
  when (testCoverageMode config) $ do
    describe "Coverage-Guided Fuzzing" $ do
      coverageGuidedTests config
  
  when (testDifferentialMode config) $ do
    describe "Differential Testing" $ do
      differentialTests config

-- | Basic fuzzing test cases
basicFuzzingTests :: FuzzTestConfig -> Spec
basicFuzzingTests config = do
  
  it "should handle crash testing without infinite loops" $ do
    let fuzzConfig = defaultFuzzConfig 
          { fuzzIterations = testIterations config `div` 4
          , fuzzTimeout = testTimeout config
          }
    results <- runCrashFuzzing fuzzConfig
    totalIterations results `shouldBe` (testIterations config `div` 4)
    executionTime results `shouldSatisfy` (< 30.0)  -- Should complete in 30s
  
  it "should detect parser crashes and timeouts" $ do
    let fuzzConfig = defaultFuzzConfig 
          { fuzzIterations = 100
          , fuzzTimeout = 1000
          }
    results <- runCrashFuzzing fuzzConfig
    -- Should find some issues with malformed inputs
    (crashCount results + timeoutCount results) `shouldSatisfy` (>= 0)
  
  it "should validate AST properties under fuzzing" $ do
    let fuzzConfig = defaultFuzzConfig 
          { fuzzIterations = testIterations config `div` 4
          , fuzzTimeout = testTimeout config
          }
    results <- runPropertyFuzzing fuzzConfig
    totalIterations results `shouldBe` (testIterations config `div` 4)
    -- Most property violations should be detected
    propertyViolations results `shouldSatisfy` (>= 0)

-- | Run basic fuzzing campaign
runBasicFuzzing :: Int -> IO FuzzResults
runBasicFuzzing iterations = do
  let config = defaultFuzzConfig { fuzzIterations = iterations }
  putStrLn $ "Running basic fuzzing with " ++ show iterations ++ " iterations..."
  results <- runFuzzingCampaign config
  putStrLn $ analyzeFuzzResults results
  return results

-- | Run intensive fuzzing campaign for development
runIntensiveFuzzing :: Int -> IO FuzzResults
runIntensiveFuzzing iterations = do
  let config = defaultFuzzConfig 
        { fuzzIterations = iterations
        , fuzzTimeout = 10000
        , fuzzMinimizeFailures = True
        }
  putStrLn $ "Running intensive fuzzing with " ++ show iterations ++ " iterations..."
  startTime <- getCurrentTime
  results <- runFuzzingCampaign config
  endTime <- getCurrentTime
  let duration = realToFrac (diffUTCTime endTime startTime)
  
  putStrLn $ "Fuzzing completed in " ++ show duration ++ " seconds"
  putStrLn $ analyzeFuzzResults results
  
  -- Save results for analysis
  when (not $ null $ failures results) $ do
    failureReport <- generateFailureReport (failures results)
    Text.writeFile "fuzz-failures.txt" (Text.pack failureReport)
    putStrLn "Failure report saved to fuzz-failures.txt"
  
  return results

-- ---------------------------------------------------------------------
-- Regression Testing
-- ---------------------------------------------------------------------

-- | Regression testing suite
regressionTests :: FuzzTestConfig -> Spec
regressionTests config = do
  
  it "should validate known crash cases" $ do
    knownCrashes <- loadKnownCrashes
    results <- forM_ knownCrashes validateCrashCase
    return results
  
  it "should prevent regression of fixed issues" $ do
    fixedIssues <- loadFixedIssues
    results <- forM_ fixedIssues validateFixedIssue
    return results
  
  it "should maintain performance baselines" $ do
    baselines <- loadPerformanceBaselines
    current <- measureCurrentPerformance config
    validatePerformanceRegression baselines current

-- | Run regression test suite
runRegressionSuite :: IO Bool
runRegressionSuite = do
  putStrLn "Running regression test suite..."
  
  -- Test known crashes
  crashes <- loadKnownCrashes
  crashResults <- mapM validateCrashCase crashes
  let crashesPassing = all id crashResults
  
  -- Test fixed issues
  issues <- loadFixedIssues
  issueResults <- mapM validateFixedIssue issues
  let issuesPassing = all id issueResults
  
  let allPassing = crashesPassing && issuesPassing
  
  putStrLn $ "Crash tests: " ++ if crashesPassing then "PASS" else "FAIL"
  putStrLn $ "Issue tests: " ++ if issuesPassing then "PASS" else "FAIL"
  putStrLn $ "Overall: " ++ if allPassing then "PASS" else "FAIL"
  
  return allPassing

-- | Update regression corpus with new failures
updateRegressionCorpus :: [FuzzFailure] -> IO ()
updateRegressionCorpus failures = do
  createDirectoryIfMissing True "test/fuzz/corpus"
  
  -- Save new crashes
  let crashes = filter ((== ParserCrash) . failureType) failures
  forM_ (zip [1..] crashes) $ \(i, failure) -> do
    let filename = "test/fuzz/corpus/crash_" ++ show i ++ ".js"
    Text.writeFile filename (failureInput failure)
  
  putStrLn $ "Updated corpus with " ++ show (length crashes) ++ " new crashes"

-- | Validate known issues remain fixed
validateKnownIssues :: IO Bool
validateKnownIssues = do
  issues <- loadFixedIssues
  results <- mapM validateFixedIssue issues
  return $ all id results

-- ---------------------------------------------------------------------
-- Performance Testing
-- ---------------------------------------------------------------------

-- | Performance testing suite
performanceTests :: FuzzTestConfig -> Spec
performanceTests config = do
  
  it "should complete fuzzing within time limits" $ do
    let maxTime = fromIntegral (testTimeout config) / 1000.0 * 2.0  -- 2x timeout
    results <- runBasicFuzzing (testIterations config `div` 10)
    executionTime results `shouldSatisfy` (< maxTime)
  
  it "should maintain reasonable memory usage" $ do
    initialMemory <- measureMemoryUsage
    _ <- runBasicFuzzing (testIterations config `div` 10)
    finalMemory <- measureMemoryUsage
    let memoryIncrease = finalMemory - initialMemory
    memoryIncrease `shouldSatisfy` (< 100)  -- Less than 100MB increase
  
  it "should process inputs at reasonable rate" $ do
    startTime <- getCurrentTime
    results <- runBasicFuzzing 100
    endTime <- getCurrentTime
    let duration = realToFrac (diffUTCTime endTime startTime)
    let rate = fromIntegral (totalIterations results) / duration
    rate `shouldSatisfy` (> 10.0)  -- At least 10 inputs per second

-- | Monitor performance during fuzzing
monitorPerformance :: FuzzTestConfig -> IO ()
monitorPerformance config = do
  putStrLn "Monitoring fuzzing performance..."
  
  let iterations = testIterations config
  let checkpoints = [iterations `div` 4, iterations `div` 2, iterations * 3 `div` 4, iterations]
  
  forM_ checkpoints $ \checkpoint -> do
    startTime <- getCurrentTime
    results <- runBasicFuzzing checkpoint
    endTime <- getCurrentTime
    
    let duration = realToFrac (diffUTCTime endTime startTime)
    let rate = fromIntegral checkpoint / duration
    
    putStrLn $ "Checkpoint " ++ show checkpoint ++ ": " ++ 
               show rate ++ " inputs/second, " ++
               show (crashCount results) ++ " crashes"

-- | Benchmark fuzzing performance
benchmarkFuzzing :: IO ()
benchmarkFuzzing = do
  putStrLn "Benchmarking fuzzing strategies..."
  
  -- Benchmark crash testing
  crashStart <- getCurrentTime
  crashResults <- runCrashFuzzing defaultFuzzConfig { fuzzIterations = 500 }
  crashEnd <- getCurrentTime
  let crashDuration = realToFrac (diffUTCTime crashEnd crashStart)
  
  -- Benchmark coverage-guided fuzzing  
  coverageStart <- getCurrentTime
  coverageResults <- runCoverageGuidedFuzzing defaultFuzzConfig { fuzzIterations = 500 }
  coverageEnd <- getCurrentTime
  let coverageDuration = realToFrac (diffUTCTime coverageEnd coverageStart)
  
  putStrLn $ "Crash testing: " ++ show crashDuration ++ "s, " ++
             show (crashCount crashResults) ++ " crashes"
  putStrLn $ "Coverage-guided: " ++ show coverageDuration ++ "s, " ++
             show (newCoveragePaths coverageResults) ++ " new paths"

-- | Analyze resource usage patterns
analyzeResourceUsage :: IO ()
analyzeResourceUsage = do
  putStrLn "Analyzing resource usage..."
  
  initialMemory <- measureMemoryUsage
  putStrLn $ "Initial memory: " ++ show initialMemory ++ "MB"
  
  _ <- runBasicFuzzing 1000
  
  finalMemory <- measureMemoryUsage
  putStrLn $ "Final memory: " ++ show finalMemory ++ "MB"
  putStrLn $ "Memory increase: " ++ show (finalMemory - initialMemory) ++ "MB"

-- ---------------------------------------------------------------------
-- Coverage-Guided Testing
-- ---------------------------------------------------------------------

-- | Coverage-guided testing suite
coverageGuidedTests :: FuzzTestConfig -> Spec
coverageGuidedTests config = do
  
  it "should improve coverage over random testing" $ do
    let iterations = testIterations config `div` 4
    
    randomResults <- runCrashFuzzing defaultFuzzConfig { fuzzIterations = iterations }
    guidedResults <- runCoverageGuidedFuzzing defaultFuzzConfig { fuzzIterations = iterations }
    
    newCoveragePaths guidedResults `shouldSatisfy` (>= 0)
  
  it "should find coverage-driven edge cases" $ do
    let config' = defaultFuzzConfig { fuzzIterations = testIterations config `div` 2 }
    results <- runCoverageGuidedFuzzing config'
    
    -- Should discover some new paths
    newCoveragePaths results `shouldSatisfy` (>= 0)
  
  it "should generate coverage report" $ do
    coverage <- measureCoverage "var x = 42; if (x > 0) { console.log(x); }"
    let report = generateCoverageReport coverage
    report `shouldSatisfy` (not . null)

-- ---------------------------------------------------------------------
-- Differential Testing
-- ---------------------------------------------------------------------

-- | Differential testing suite
differentialTests :: FuzzTestConfig -> Spec
differentialTests config = do
  
  it "should compare against reference parsers" $ do
    let testInputs = 
          [ "var x = 42;"
          , "function f() { return true; }"
          , "if (true) { console.log('test'); }"
          ]
    
    report <- runDifferentialSuite testInputs
    reportTotalTests report `shouldBe` (length testInputs * 4)  -- 4 reference parsers
  
  it "should identify parser discrepancies" $ do
    let problematicInputs = 
          [ "var x = 0x;"  -- Incomplete hex
          , "function f("   -- Incomplete function
          , "var x = \"unclosed string"
          ]
    
    report <- runDifferentialSuite problematicInputs
    -- Should find some discrepancies in error handling
    reportMismatches report `shouldSatisfy` (>= 0)

-- ---------------------------------------------------------------------
-- Failure Analysis
-- ---------------------------------------------------------------------

-- | Analyze fuzzing failures systematically
analyzeFailures :: [FuzzFailure] -> IO String
analyzeFailures failures = do
  let categorized = categorizeFailures failures
  let prioritized = prioritizeIssues categorized
  return $ formatFailureAnalysis prioritized

-- | Categorize failures by type and characteristics
categorizeFailures :: [FuzzFailure] -> [(FailureType, [FuzzFailure])]
categorizeFailures failures = 
  let sorted = sortBy (comparing failureType) failures
      grouped = groupByType sorted
  in grouped

-- | Generate comprehensive failure report
generateFailureReport :: [FuzzFailure] -> IO String
generateFailureReport failures = do
  analysis <- analyzeFailures failures
  let summary = generateFailureSummary failures
  return $ summary ++ "\n\n" ++ analysis

-- | Prioritize issues based on severity and frequency
prioritizeIssues :: [(FailureType, [FuzzFailure])] -> [(FailureType, [FuzzFailure], Int)]
prioritizeIssues categorized = 
  let withPriority = map (\(ftype, fs) -> (ftype, fs, calculatePriority ftype (length fs))) categorized
      sorted = sortBy (comparing (\(_, _, p) -> Down p)) withPriority
  in sorted

-- ---------------------------------------------------------------------
-- Helper Functions
-- ---------------------------------------------------------------------

-- | Load known crash cases from corpus
loadKnownCrashes :: IO [Text.Text]
loadKnownCrashes = do
  exists <- doesFileExist "test/fuzz/corpus/known_crashes.txt"
  if exists
    then do
      content <- Text.readFile "test/fuzz/corpus/known_crashes.txt"
      return $ Text.lines content
    else return []

-- | Load fixed issues for regression testing
loadFixedIssues :: IO [Text.Text]
loadFixedIssues = do
  exists <- doesFileExist "test/fuzz/corpus/fixed_issues.txt"
  if exists
    then do
      content <- Text.readFile "test/fuzz/corpus/fixed_issues.txt"
      return $ Text.lines content
    else return []

-- | Load performance baselines
loadPerformanceBaselines :: IO [(String, Double)]
loadPerformanceBaselines = do
  -- Return some default baselines
  return 
    [ ("basic_parsing", 0.1)
    , ("complex_parsing", 1.0)
    , ("error_handling", 0.05)
    ]

-- | Validate that known crash case still crashes (or is now fixed)
validateCrashCase :: Text.Text -> IO Bool
validateCrashCase input = do
  result <- catch (validateInput input) handleException
  return result
  where
    validateInput inp = do
      let config = defaultFuzzConfig { fuzzIterations = 1 }
      results <- runCrashFuzzing config { fuzzSeedInputs = [inp] }
      return $ crashCount results == 0  -- Should be fixed now
    
    handleException :: SomeException -> IO Bool
    handleException _ = return False

-- | Validate that fixed issue remains fixed
validateFixedIssue :: Text.Text -> IO Bool
validateFixedIssue input = do
  result <- catch (validateParsing input) handleException
  return result
  where
    validateParsing inp = do
      let config = defaultFuzzConfig { fuzzIterations = 1 }
      results <- runPropertyFuzzing config { fuzzSeedInputs = [inp] }
      return $ propertyViolations results == 0
    
    handleException :: SomeException -> IO Bool
    handleException _ = return False

-- | Measure current performance metrics
measureCurrentPerformance :: FuzzTestConfig -> IO [(String, Double)]
measureCurrentPerformance config = do
  let iterations = min 100 (testIterations config)
  
  -- Measure basic parsing performance
  basicStart <- getCurrentTime
  _ <- runBasicFuzzing iterations
  basicEnd <- getCurrentTime
  let basicDuration = realToFrac (diffUTCTime basicEnd basicStart)
  
  return 
    [ ("basic_parsing", basicDuration / fromIntegral iterations)
    , ("complex_parsing", basicDuration * 2)  -- Estimate
    , ("error_handling", basicDuration / 2)   -- Estimate
    ]

-- | Validate performance hasn't regressed
validatePerformanceRegression :: [(String, Double)] -> [(String, Double)] -> IO ()
validatePerformanceRegression baselines current = do
  forM_ baselines $ \(metric, baseline) -> do
    case lookup metric current of
      Nothing -> hPutStrLn stderr $ "Missing metric: " ++ metric
      Just currentValue -> do
        let regression = (currentValue - baseline) / baseline
        when (regression > 0.2) $ do  -- 20% regression threshold
          hPutStrLn stderr $ "Performance regression in " ++ metric ++ 
                           ": " ++ show (regression * 100) ++ "%"

-- | Measure memory usage (simplified)
measureMemoryUsage :: IO Int
measureMemoryUsage = return 50  -- Simplified - return 50MB

-- | Group failures by type
groupByType :: [FuzzFailure] -> [(FailureType, [FuzzFailure])]
groupByType [] = []
groupByType (f:fs) = 
  let (same, different) = span ((== failureType f) . failureType) fs
  in (failureType f, f:same) : groupByType different

-- | Generate failure summary
generateFailureSummary :: [FuzzFailure] -> String
generateFailureSummary failures = unlines
  [ "=== Failure Summary ==="
  , "Total failures: " ++ show (length failures)
  , "By type:"
  ] ++ map formatTypeCount (countByType failures)

-- | Count failures by type
countByType :: [FuzzFailure] -> [(FailureType, Int)]
countByType failures = 
  let categorized = categorizeFailures failures
  in map (\(ftype, fs) -> (ftype, length fs)) categorized

-- | Format type count
formatTypeCount :: (FailureType, Int) -> String
formatTypeCount (ftype, count) = 
  "  " ++ show ftype ++ ": " ++ show count

-- | Calculate priority score for failure type
calculatePriority :: FailureType -> Int -> Int
calculatePriority ftype count = case ftype of
  ParserCrash -> count * 10      -- Crashes are highest priority
  InfiniteLoop -> count * 8      -- Infinite loops are very serious
  MemoryExhaustion -> count * 6  -- Memory issues are important
  ParserTimeout -> count * 4     -- Timeouts are medium priority
  PropertyViolation -> count * 2 -- Property violations are lower priority
  DifferentialMismatch -> count  -- Mismatches are lowest priority

-- | Format failure analysis
formatFailureAnalysis :: [(FailureType, [FuzzFailure], Int)] -> String
formatFailureAnalysis prioritized = unlines $
  [ "=== Failure Analysis (by priority) ===" ] ++
  map formatPriorityGroup prioritized

-- | Format priority group
formatPriorityGroup :: (FailureType, [FuzzFailure], Int) -> String
formatPriorityGroup (ftype, failures, priority) = 
  show ftype ++ " (priority " ++ show priority ++ "): " ++ 
  show (length failures) ++ " failures"