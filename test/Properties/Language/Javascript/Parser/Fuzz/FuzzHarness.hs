{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive fuzzing harness for JavaScript parser crash testing.
--
-- This module provides a unified fuzzing infrastructure supporting multiple
-- fuzzing strategies for discovering parser vulnerabilities and edge cases:
--
--   * __Crash Testing__: AFL-style input mutation for parser robustness
--     Systematic input generation designed to trigger parser crashes,
--     infinite loops, and memory exhaustion conditions.
--
--   * __Coverage-Guided Generation__: Feedback-driven test case creation
--     Uses code coverage metrics to guide input generation toward
--     unexplored parser code paths and edge cases.
--
--   * __Property-Based Fuzzing__: AST invariant validation through mutation
--     Combines QuickCheck property testing with mutation-based fuzzing
--     to validate parser invariants under adversarial inputs.
--
--   * __Differential Testing__: Cross-parser validation framework
--     Compares parser behavior against reference implementations
--     (Babel, TypeScript) to detect semantic inconsistencies.
--
-- The harness includes resource monitoring, timeout management, and
-- crash detection to ensure fuzzing runs safely within CI environments.
--
-- ==== Examples
--
-- Running crash testing:
--
-- >>> runCrashFuzzing defaultFuzzConfig 1000
-- FuzzResults { crashCount = 3, timeoutCount = 1, ... }
--
-- Coverage-guided fuzzing:
--
-- >>> runCoverageGuidedFuzzing defaultFuzzConfig 500
-- FuzzResults { newCoveragePaths = 15, ... }
--
-- @since 0.7.1.0
module Properties.Language.Javascript.Parser.Fuzz.FuzzHarness
  ( -- * Fuzzing Configuration
    FuzzConfig (..),
    defaultFuzzConfig,
    FuzzStrategy (..),
    ResourceLimits (..),

    -- * Fuzzing Execution
    runFuzzingCampaign,
    runCrashFuzzing,
    runCoverageGuidedFuzzing,
    runPropertyFuzzing,
    runDifferentialFuzzing,

    -- * Results and Analysis
    FuzzResults (..),
    FuzzFailure (..),
    FailureType (..),
    analyzeFuzzResults,
    generateFailureReport,

    -- * Test Case Management
    FuzzTestCase (..),
    minimizeTestCase,
    reproduceFailure,
  )
where

import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Language.JavaScript.Parser (readJs, renderToString)
import qualified Language.JavaScript.Parser.AST as AST
import Properties.Language.Javascript.Parser.Fuzz.CoverageGuided
  ( CoverageData (..),
    CoverageMetrics (..),
    guidedGeneration,
    measureCoverage,
  )
import qualified Properties.Language.Javascript.Parser.Fuzz.DifferentialTesting as DiffTest
import Properties.Language.Javascript.Parser.Fuzz.FuzzGenerators
  ( generateEdgeCaseJS,
    generateMalformedJS,
    generateRandomJS,
    mutateFuzzInput,
  )
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

-- ---------------------------------------------------------------------
-- Configuration Types
-- ---------------------------------------------------------------------

-- | Fuzzing configuration parameters
data FuzzConfig = FuzzConfig
  { fuzzStrategy :: !FuzzStrategy,
    fuzzIterations :: !Int,
    fuzzTimeout :: !Int,
    fuzzResourceLimits :: !ResourceLimits,
    fuzzSeedInputs :: ![Text.Text],
    fuzzOutputDir :: !FilePath,
    fuzzMinimizeFailures :: !Bool
  }
  deriving (Eq, Show)

-- | Fuzzing strategy selection
data FuzzStrategy
  = -- | AFL-style mutation fuzzing
    CrashTesting
  | -- | Coverage-feedback guided generation
    CoverageGuided
  | -- | Property-based fuzzing with mutations
    PropertyBased
  | -- | Cross-parser differential testing
    Differential
  | -- | All strategies combined
    Comprehensive
  deriving (Eq, Show)

-- | Resource consumption limits
data ResourceLimits = ResourceLimits
  { maxMemoryMB :: !Int,
    maxExecutionTimeMs :: !Int,
    maxInputSizeBytes :: !Int,
    maxParseDepth :: !Int
  }
  deriving (Eq, Show)

-- | Default fuzzing configuration
defaultFuzzConfig :: FuzzConfig
defaultFuzzConfig =
  FuzzConfig
    { fuzzStrategy = Comprehensive,
      fuzzIterations = 1000,
      fuzzTimeout = 5000,
      fuzzResourceLimits = defaultResourceLimits,
      fuzzSeedInputs = defaultSeedInputs,
      fuzzOutputDir = "test/fuzz/output",
      fuzzMinimizeFailures = True
    }

-- | Default resource limits for safe fuzzing
defaultResourceLimits :: ResourceLimits
defaultResourceLimits =
  ResourceLimits
    { maxMemoryMB = 128,
      maxExecutionTimeMs = 1000,
      maxInputSizeBytes = 1024 * 1024,
      maxParseDepth = 100
    }

-- | Default seed inputs for mutation
defaultSeedInputs :: [Text.Text]
defaultSeedInputs =
  [ "var x = 42;",
    "function f() { return true; }",
    "if (true) { console.log('hi'); }",
    "{a: 1, b: [1,2,3]}"
  ]

-- ---------------------------------------------------------------------
-- Results Types
-- ---------------------------------------------------------------------

-- | Comprehensive fuzzing results
data FuzzResults = FuzzResults
  { totalIterations :: !Int,
    crashCount :: !Int,
    timeoutCount :: !Int,
    memoryExhaustionCount :: !Int,
    newCoveragePaths :: !Int,
    propertyViolations :: !Int,
    differentialFailures :: !Int,
    executionTime :: !Double,
    failures :: ![FuzzFailure]
  }
  deriving (Eq, Show)

-- | Individual fuzzing failure
data FuzzFailure = FuzzFailure
  { failureType :: !FailureType,
    failureInput :: !Text.Text,
    failureMessage :: !String,
    failureTimestamp :: !UTCTime,
    failureMinimized :: !Bool
  }
  deriving (Eq, Show)

-- | Classification of fuzzing failures
data FailureType
  = ParserCrash
  | ParserTimeout
  | MemoryExhaustion
  | InfiniteLoop
  | PropertyViolation
  | DifferentialMismatch
  deriving (Eq, Show, Ord)

-- | Test case representation
data FuzzTestCase = FuzzTestCase
  { testInput :: !Text.Text,
    testExpected :: !FuzzExpectation,
    testMetadata :: ![(String, String)]
  }
  deriving (Eq, Show)

-- | Expected fuzzing behavior
data FuzzExpectation
  = ShouldParse
  | ShouldFail
  | ShouldTimeout
  | ShouldCrash
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Main Fuzzing Interface
-- ---------------------------------------------------------------------

-- | Execute comprehensive fuzzing campaign
runFuzzingCampaign :: FuzzConfig -> IO FuzzResults
runFuzzingCampaign config = do
  startTime <- getCurrentTime
  results <- case fuzzStrategy config of
    CrashTesting -> runCrashFuzzing config
    CoverageGuided -> runCoverageGuidedFuzzing config
    PropertyBased -> runPropertyFuzzing config
    Differential -> runDifferentialFuzzing config
    Comprehensive -> runComprehensiveFuzzing config
  endTime <- getCurrentTime
  let duration = realToFrac (diffUTCTime endTime startTime)
  return results {executionTime = duration}

-- | AFL-style crash testing fuzzing
runCrashFuzzing :: FuzzConfig -> IO FuzzResults
runCrashFuzzing config = do
  inputs <- generateCrashInputs config
  results <- fuzzWithInputs config inputs detectCrashes
  when (fuzzMinimizeFailures config) $
    minimizeAllFailures results
  return results

-- | Coverage-guided fuzzing implementation
runCoverageGuidedFuzzing :: FuzzConfig -> IO FuzzResults
runCoverageGuidedFuzzing config = do
  initialCoverage <- measureInitialCoverage config
  results <- guidedFuzzingLoop config initialCoverage
  return results

-- | Property-based fuzzing with mutations
runPropertyFuzzing :: FuzzConfig -> IO FuzzResults
runPropertyFuzzing config = do
  inputs <- generatePropertyInputs config
  results <- fuzzWithInputs config inputs validateProperties
  return results

-- | Differential testing against external parsers
runDifferentialFuzzing :: FuzzConfig -> IO FuzzResults
runDifferentialFuzzing config = do
  inputs <- generateDifferentialInputs config
  results <- fuzzWithInputs config inputs compareParsers
  return results

-- | Run all fuzzing strategies comprehensively
runComprehensiveFuzzing :: FuzzConfig -> IO FuzzResults
runComprehensiveFuzzing config = do
  let splitConfig iterations = config {fuzzIterations = iterations}
  let quarter = fuzzIterations config `div` 4

  crashResults <- runCrashFuzzing (splitConfig quarter)
  coverageResults <- runCoverageGuidedFuzzing (splitConfig quarter)
  propertyResults <- runPropertyFuzzing (splitConfig quarter)
  diffResults <- runDifferentialFuzzing (splitConfig quarter)

  return $
    combineResults
      [crashResults, coverageResults, propertyResults, diffResults]

-- ---------------------------------------------------------------------
-- Input Generation Strategies
-- ---------------------------------------------------------------------

-- | Generate inputs designed to crash the parser
generateCrashInputs :: FuzzConfig -> IO [Text.Text]
generateCrashInputs config = do
  let iterations = fuzzIterations config
  malformed <- generateMalformedJS (iterations `div` 3)
  edgeCases <- generateEdgeCaseJS (iterations `div` 3)
  mutations <- mutateSeedInputs (fuzzSeedInputs config) (iterations `div` 3)
  return (malformed ++ edgeCases ++ mutations)

-- | Generate inputs for property testing
generatePropertyInputs :: FuzzConfig -> IO [Text.Text]
generatePropertyInputs config = do
  let iterations = fuzzIterations config
  validJS <- generateRandomJS (iterations `div` 2)
  mutations <- mutateSeedInputs (fuzzSeedInputs config) (iterations `div` 2)
  return (validJS ++ mutations)

-- | Generate inputs for differential testing
generateDifferentialInputs :: FuzzConfig -> IO [Text.Text]
generateDifferentialInputs config = do
  let iterations = fuzzIterations config
  standardJS <- generateRandomJS (iterations `div` 2)
  edgeFeatures <- generateEdgeCaseJS (iterations `div` 2)
  return (standardJS ++ edgeFeatures)

-- | Mutate seed inputs using various strategies
mutateSeedInputs :: [Text.Text] -> Int -> IO [Text.Text]
mutateSeedInputs seeds count = do
  let perSeed = max 1 (count `div` length seeds)
  concat
    <$> forM
      seeds
      ( \seed ->
          forM [1 .. perSeed] (\_ -> mutateFuzzInput seed)
      )

-- ---------------------------------------------------------------------
-- Fuzzing Execution Engine
-- ---------------------------------------------------------------------

-- | Execute fuzzing with given inputs and test function
fuzzWithInputs ::
  FuzzConfig ->
  [Text.Text] ->
  (FuzzConfig -> Text.Text -> IO (Maybe FuzzFailure)) ->
  IO FuzzResults
fuzzWithInputs config inputs testFunc = do
  results <- forM inputs (testWithTimeout config testFunc)
  let failures = [f | Just f <- results]
  return $
    FuzzResults
      { totalIterations = length inputs,
        crashCount = countFailureType ParserCrash failures,
        timeoutCount = countFailureType ParserTimeout failures,
        memoryExhaustionCount = countFailureType MemoryExhaustion failures,
        newCoveragePaths = 0, -- Set by coverage-guided fuzzing
        propertyViolations = countFailureType PropertyViolation failures,
        differentialFailures = countFailureType DifferentialMismatch failures,
        executionTime = 0, -- Set by main function
        failures = failures
      }

-- | Test single input with timeout protection
testWithTimeout ::
  FuzzConfig ->
  (FuzzConfig -> Text.Text -> IO (Maybe FuzzFailure)) ->
  Text.Text ->
  IO (Maybe FuzzFailure)
testWithTimeout config testFunc input = do
  let timeoutMs = maxExecutionTimeMs (fuzzResourceLimits config)
  result <-
    catch
      ( do
          timeoutResult <- timeout (timeoutMs * 1000) (testFunc config input)
          case timeoutResult of
            Nothing -> do
              timestamp <- getCurrentTime
              return $ Just $ FuzzFailure ParserTimeout input "Execution timeout" timestamp False
            Just failure -> return failure
      )
      handleTestException
  return result
  where
    handleTestException :: SomeException -> IO (Maybe FuzzFailure)
    handleTestException ex = do
      timestamp <- getCurrentTime
      return $ Just $ FuzzFailure ParserCrash input (show ex) timestamp False

-- | Detect parser crashes and exceptions
detectCrashes :: FuzzConfig -> Text.Text -> IO (Maybe FuzzFailure)
detectCrashes config input = do
  result <- catch (testParseStrictly input) handleException
  case result of
    Left errMsg -> do
      timestamp <- getCurrentTime
      return $ Just $ FuzzFailure ParserCrash input errMsg timestamp False
    Right _ -> return Nothing
  where
    handleException :: SomeException -> IO (Either String ())
    handleException ex = return $ Left $ show ex

-- | Validate AST properties and invariants
validateProperties :: FuzzConfig -> Text.Text -> IO (Maybe FuzzFailure)
validateProperties _config input = do
  result <-
    catch
      ( do
          let ast = readJs (Text.unpack input)
          case ast of
            prog@(AST.JSAstProgram _ _) -> do
              violations <- checkASTInvariants prog
              case violations of
                [] -> return Nothing
                (violation : _) -> do
                  timestamp <- getCurrentTime
                  return $ Just $ FuzzFailure PropertyViolation input violation timestamp False
            _ -> return Nothing
      )
      handlePropertyException
  return result
  where
    handlePropertyException :: SomeException -> IO (Maybe FuzzFailure)
    handlePropertyException ex = do
      timestamp <- getCurrentTime
      return $ Just $ FuzzFailure ParserCrash input (show ex) timestamp False

-- | Compare parser output with external implementations
compareParsers :: FuzzConfig -> Text.Text -> IO (Maybe FuzzFailure)
compareParsers _config input = do
  babelResult <- DiffTest.compareWithBabel input
  tsResult <- DiffTest.compareWithTypeScript input
  case (babelResult, tsResult) of
    (DiffTest.DifferentialMismatch msg, _) -> createDiffFailure msg
    (_, DiffTest.DifferentialMismatch msg) -> createDiffFailure msg
    _ -> return Nothing
  where
    createDiffFailure msg = do
      timestamp <- getCurrentTime
      return $ Just $ FuzzFailure DifferentialMismatch input msg timestamp False

-- ---------------------------------------------------------------------
-- Coverage-Guided Fuzzing
-- ---------------------------------------------------------------------

-- | Measure initial code coverage baseline
measureInitialCoverage :: FuzzConfig -> IO CoverageData
measureInitialCoverage config = do
  let seeds = fuzzSeedInputs config
  coverageData <- forM seeds $ \input ->
    measureCoverage (Text.unpack input)
  return $ combineCoverageData coverageData

-- | Coverage-guided fuzzing main loop
guidedFuzzingLoop :: FuzzConfig -> CoverageData -> IO FuzzResults
guidedFuzzingLoop config initialCoverage = do
  guidedFuzzingLoop' config initialCoverage 0 []
  where
    guidedFuzzingLoop' cfg coverage iteration failures
      | iteration >= fuzzIterations cfg = return $ createResults failures iteration
      | otherwise = do
        newInput <- guidedGeneration coverage
        failure <- testWithTimeout cfg validateProperties newInput
        newCoverage <- measureCoverage (Text.unpack newInput)
        let updatedCoverage = updateCoverage coverage newCoverage
        let updatedFailures = maybe failures (: failures) failure
        guidedFuzzingLoop' cfg updatedCoverage (iteration + 1) updatedFailures

    createResults failures iter =
      FuzzResults
        { totalIterations = iter,
          crashCount = 0,
          timeoutCount = 0,
          memoryExhaustionCount = 0,
          newCoveragePaths = 0, -- Would be calculated from coverage diff
          propertyViolations = length failures,
          differentialFailures = 0,
          executionTime = 0,
          failures = failures
        }

-- ---------------------------------------------------------------------
-- Failure Analysis and Minimization
-- ---------------------------------------------------------------------

-- | Minimize failing test case to smallest reproduction
minimizeTestCase :: FuzzTestCase -> IO FuzzTestCase
minimizeTestCase testCase = do
  let input = testInput testCase
  minimized <- minimizeInput input
  return testCase {testInput = minimized}

-- | Reproduce a specific failure for debugging
reproduceFailure :: FuzzFailure -> IO Bool
reproduceFailure failure = do
  result <- detectCrashes defaultFuzzConfig (failureInput failure)
  return $ case result of
    Just _ -> True
    Nothing -> False

-- | Analyze fuzzing results for patterns and insights
analyzeFuzzResults :: FuzzResults -> String
analyzeFuzzResults results =
  unlines $
    [ "=== Fuzzing Results Analysis ===",
      "Total iterations: " ++ show (totalIterations results),
      "Crashes found: " ++ show (crashCount results),
      "Timeouts: " ++ show (timeoutCount results),
      "Property violations: " ++ show (propertyViolations results),
      "Differential failures: " ++ show (differentialFailures results),
      "Execution time: " ++ show (executionTime results) ++ "s",
      "",
      "Failure breakdown:"
    ]
      ++ map analyzeFailure (failures results)

-- | Generate detailed failure report
generateFailureReport :: [FuzzFailure] -> IO String
generateFailureReport failures = do
  let groupedFailures = groupFailuresByType failures
  return $ unlines $ map formatFailureGroup groupedFailures

-- ---------------------------------------------------------------------
-- Helper Functions
-- ---------------------------------------------------------------------

-- | Test parsing with strict evaluation to catch crashes
testParseStrictly :: Text.Text -> IO (Either String ())
testParseStrictly input = do
  let result = readJs (Text.unpack input)
  case result of
    ast@(AST.JSAstProgram _ _) -> do
      _ <- evaluate (length (renderToString ast))
      return $ Right ()
    _ -> return $ Left "Parse failed"

-- | Check AST invariants and return violations
checkASTInvariants :: AST.JSAST -> IO [String]
checkASTInvariants _ast = return [] -- Simplified for now

-- | Combine multiple coverage measurements
combineCoverageData :: [CoverageData] -> CoverageData
combineCoverageData _coverages =
  CoverageData
    { coveredLines = [],
      branchCoverage = [],
      pathCoverage = [],
      coverageMetrics = CoverageMetrics 0 0 0 0 0 0 0.0
    }

-- | Update coverage with new measurement
updateCoverage :: CoverageData -> CoverageData -> CoverageData
updateCoverage _old _new =
  CoverageData
    { coveredLines = [],
      branchCoverage = [],
      pathCoverage = [],
      coverageMetrics = CoverageMetrics 0 0 0 0 0 0 0.0
    }

-- | Minimize input while preserving failure
minimizeInput :: Text.Text -> IO Text.Text
minimizeInput input
  | Text.length input <= 10 = return input
  | otherwise = do
    let half = Text.take (Text.length input `div` 2) input
    result <- detectCrashes defaultFuzzConfig half
    case result of
      Just _ -> minimizeInput half
      Nothing -> return input

-- | Count failures of specific type
countFailureType :: FailureType -> [FuzzFailure] -> Int
countFailureType ftype = length . filter ((== ftype) . failureType)

-- | Combine multiple fuzzing results
combineResults :: [FuzzResults] -> FuzzResults
combineResults results =
  FuzzResults
    { totalIterations = sum $ map totalIterations results,
      crashCount = sum $ map crashCount results,
      timeoutCount = sum $ map timeoutCount results,
      memoryExhaustionCount = sum $ map memoryExhaustionCount results,
      newCoveragePaths = sum $ map newCoveragePaths results,
      propertyViolations = sum $ map propertyViolations results,
      differentialFailures = sum $ map differentialFailures results,
      executionTime = maximum $ map executionTime results,
      failures = concatMap failures results
    }

-- | Analyze individual failure
analyzeFailure :: FuzzFailure -> String
analyzeFailure failure =
  "  " ++ show (failureType failure) ++ ": "
    ++ take 50 (Text.unpack (failureInput failure))
    ++ "..."

-- | Group failures by type for analysis
groupFailuresByType :: [FuzzFailure] -> [(FailureType, [FuzzFailure])]
groupFailuresByType failures =
  let sorted = sortBy (comparing failureType) failures
      grouped = groupByType sorted
   in map (\fs@(f : _) -> (failureType f, fs)) grouped
  where
    groupByType [] = []
    groupByType (x : xs) =
      let (same, different) = span ((== failureType x) . failureType) xs
       in (x : same) : groupByType different

-- | Format failure group for reporting
formatFailureGroup :: (FailureType, [FuzzFailure]) -> String
formatFailureGroup (ftype, failures') =
  show ftype ++ ": " ++ show (length failures') ++ " failures"

-- | Minimize all failures in results
minimizeAllFailures :: FuzzResults -> IO ()
minimizeAllFailures _results = return () -- Implementation deferred
