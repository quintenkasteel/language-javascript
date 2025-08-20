{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Main coverage-driven test generation application.
--
-- This executable orchestrates the entire coverage-driven test generation
-- process, from HPC report analysis through ML-driven test synthesis to
-- integration with existing test infrastructure.
--
-- ==== Usage
--
-- @
-- coverage-gen --hpc dist/hpc/tix/testsuite/testsuite.tix \
--              --corpus corpus/real-world/ \
--              --target 0.95 \
--              --output test/Generated/
-- @
--
-- @since 1.0.0
module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import Control.Monad (unless, when)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List

import Coverage.Analysis
  ( parseHpcReport
  , identifyCoverageGaps
  , prioritizeGaps
  , CoverageGap(..)
  )
import Coverage.Generation
  ( createMLGenerator
  , generateTestCases
  , GenerationConfig(..)
  , GenerationStrategy(..)
  , MLConfig(..)
  , MLModelType(..)
  , NetworkConfig(..)
  , ActivationType(..)
  , OptimizerType(..)
  , TestCase(..)
  )
import Coverage.Optimization
  ( createCoverageOptimizer
  , optimizeTestSuite
  , measureCoverageGain
  , OptimizationConfig(..)
  , TestSuite(..)
  , CoverageMetrics(..)
  )
import Coverage.Corpus
  ( loadCorpus
  , extractPatterns
  , generateFromCorpus
  , CodePattern(..)
  )
import Coverage.Integration
  ( createTestIntegrator
  , runGeneratedTests
  , measureIntegratedCoverage
  , IntegrationConfig(..)
  )

-- | Main application entry point.
main :: IO ()
main = do
  args <- getArgs
  config <- parseCommandLine args
  
  putStrLn "Starting coverage-driven test generation..."
  result <- runCoverageGeneration config
  
  case result of
    Right coverage -> do
      putStrLn ("Final coverage: " ++ show coverage)
      if coverage >= _configTargetCoverage config
        then do
          putStrLn "Target coverage achieved!"
          exitSuccess
        else do
          putStrLn "Target coverage not reached, but progress made."
          exitSuccess
    Left err -> do
      putStrLn ("Error: " ++ Text.unpack err)
      exitFailure

-- | Application configuration.
data AppConfig = AppConfig
  { _configHpcFile :: !FilePath
  , _configCorpusDir :: !(Maybe FilePath)
  , _configTargetCoverage :: !Double
  , _configOutputDir :: !FilePath
  , _configStrategy :: !GenerationStrategy
  , _configVerbose :: !Bool
  , _configParallel :: !Bool
  , _configMaxTests :: !Int
  } deriving (Eq, Show)

-- | Parse command line arguments.
parseCommandLine :: [String] -> IO AppConfig
parseCommandLine args = 
  case parseArgs args defaultConfig of
    Right config -> pure config
    Left err -> do
      putStrLn err
      printUsage
      exitFailure
  where
    defaultConfig = AppConfig
      { _configHpcFile = "dist/hpc/tix/testsuite/testsuite.tix"
      , _configCorpusDir = Nothing
      , _configTargetCoverage = 0.95
      , _configOutputDir = "test/Generated/"
      , _configStrategy = MachineLearning defaultMLConfig
      , _configVerbose = False
      , _configParallel = True
      , _configMaxTests = 100
      }
    
    defaultMLConfig = MLConfig
      { _mlModelType = NeuralNetwork defaultNetConfig
      , _mlTrainingSize = 1000
      , _mlFeatureSet = ["input_length", "complexity", "nesting_depth"]
      , _mlOptimizer = Adam
      }
    
    defaultNetConfig = NetworkConfig
      { _networkLayers = [10, 20, 10, 1]
      , _networkActivation = ReLU
      , _networkDropout = 0.2
      }

-- | Parse command line arguments recursively.
parseArgs :: [String] -> AppConfig -> Either String AppConfig
parseArgs [] config = Right config
parseArgs ("--hpc":file:rest) config = 
  parseArgs rest (config { _configHpcFile = file })
parseArgs ("--corpus":dir:rest) config = 
  parseArgs rest (config { _configCorpusDir = Just dir })
parseArgs ("--target":target:rest) config = 
  case reads target of
    [(val, "")] -> parseArgs rest (config { _configTargetCoverage = val })
    _ -> Left ("Invalid target coverage: " ++ target)
parseArgs ("--output":dir:rest) config = 
  parseArgs rest (config { _configOutputDir = dir })
parseArgs ("--max-tests":count:rest) config = 
  case reads count of
    [(val, "")] -> parseArgs rest (config { _configMaxTests = val })
    _ -> Left ("Invalid max tests: " ++ count)
parseArgs ("--verbose":rest) config = 
  parseArgs rest (config { _configVerbose = True })
parseArgs ("--sequential":rest) config = 
  parseArgs rest (config { _configParallel = False })
parseArgs ("--help":_) _ = Left "help"
parseArgs (arg:_) _ = Left ("Unknown argument: " ++ arg)

-- | Print usage information.
printUsage :: IO ()
printUsage = putStrLn $ unlines
  [ "Usage: coverage-gen [OPTIONS]"
  , ""
  , "Options:"
  , "  --hpc FILE        HPC coverage file (.tix) [default: dist/hpc/tix/testsuite/testsuite.tix]"
  , "  --corpus DIR      Real-world JavaScript corpus directory"
  , "  --target PERCENT  Target coverage (0.0-1.0) [default: 0.95]"
  , "  --output DIR      Output directory for generated tests [default: test/Generated/]"
  , "  --max-tests NUM   Maximum number of tests to generate [default: 100]"
  , "  --verbose         Enable verbose output"
  , "  --sequential      Disable parallel execution"
  , "  --help            Show this help message"
  , ""
  , "Examples:"
  , "  coverage-gen --hpc my-coverage.tix --target 0.90"
  , "  coverage-gen --corpus corpus/ --output test/Auto/ --verbose"
  ]

-- | Run the complete coverage generation process.
runCoverageGeneration :: AppConfig -> IO (Either Text Double)
runCoverageGeneration config = do
  when (_configVerbose config) $
    putStrLn "Analyzing HPC coverage report..."
  
  -- Parse HPC report
  hpcResult <- parseHpcReport (_configHpcFile config)
  case hpcResult of
    Left err -> pure (Left err)
    Right hpcReport -> do
      
      -- Identify coverage gaps
      let gaps = identifyCoverageGaps hpcReport
      let prioritizedGaps = prioritizeGaps gaps
      
      when (_configVerbose config) $
        putStrLn ("Found " ++ show (length gaps) ++ " coverage gaps")
      
      -- Load corpus if specified
      corpusPatterns <- case _configCorpusDir config of
        Nothing -> pure mempty
        Just corpusDir -> do
          when (_configVerbose config) $
            putStrLn ("Loading corpus from " ++ corpusDir)
          corpusResult <- loadCorpus corpusDir
          case corpusResult of
            Left err -> do
              putStrLn ("Warning: Could not load corpus: " ++ Text.unpack err)
              pure mempty
            Right corpus -> extractPatterns corpus
      
      -- Generate test cases
      when (_configVerbose config) $
        putStrLn "Generating test cases..."
      
      testCases <- generateTestsWithStrategy config prioritizedGaps corpusPatterns
      
      when (_configVerbose config) $
        putStrLn ("Generated " ++ show (length testCases) ++ " test cases")
      
      -- Optimize test suite
      when (_configVerbose config) $
        putStrLn "Optimizing test suite..."
      
      optimizedSuite <- optimizeGeneratedTests config testCases
      
      -- Integrate with existing tests
      when (_configVerbose config) $
        putStrLn "Integrating with existing test infrastructure..."
      
      finalCoverage <- integrateAndMeasure config optimizedSuite
      
      pure (Right finalCoverage)

-- | Generate tests using the configured strategy.
generateTestsWithStrategy :: AppConfig -> [CoverageGap] -> Map Text [CodePattern] -> IO [TestCase]
generateTestsWithStrategy config gaps corpusPatterns = do
  let genConfig = GenerationConfig
        { _configStrategy = _configStrategy config
        , _configMaxTests = _configMaxTests config
        , _configTargetCoverage = _configTargetCoverage config
        , _configMutationRate = 0.1
        }
  
  generator <- createMLGenerator genConfig
  
  -- Generate from ML model
  mlTests <- generateTestCases generator gaps
  
  -- Generate from corpus if available
  corpusTests <- if null corpusPatterns
    then pure []
    else generateFromCorpus corpusPatterns gaps
  
  -- Combine and deduplicate
  let allTests = mlTests ++ corpusTests
  pure (take (_configMaxTests config) allTests)

-- | Optimize generated test suite.
optimizeGeneratedTests :: AppConfig -> [TestCase] -> IO TestSuite
optimizeGeneratedTests config testCases = do
  let optConfig = OptimizationConfig
        { _configPopulationSize = 50
        , _configGenerations = 10
        , _configEliteSize = 5
        , _configTournamentSize = 3
        , _configCrossoverRate = 0.8
        , _configMutationRate = 0.2
        , _configConvergenceThreshold = 0.01
        }
  
  optimizer <- createCoverageOptimizer optConfig
  
  -- Create initial test suite
  let initialSuite = TestSuite testCases initialMetrics (length testCases) 0.0
  let initialMetrics = CoverageMetrics 0.0 0.0 0.0 0.0
  
  -- Optimize
  optimizeTestSuite optimizer initialSuite

-- | Integrate tests and measure final coverage.
integrateAndMeasure :: AppConfig -> TestSuite -> IO Double
integrateAndMeasure config testSuite = do
  let integConfig = IntegrationConfig
        { _configTestCommand = "cabal test"
        , _configCoverageCommand = "cabal test --enable-coverage"
        , _configTestDirectory = _configOutputDir config
        , _configCoverageDirectory = "dist/hpc/"
        , _configTimeout = 300  -- 5 minutes
        , _configParallel = _configParallel config
        , _configVerbose = _configVerbose config
        }
  
  integrator <- createTestIntegrator integConfig
  
  -- Run tests and measure coverage
  (_, result) <- runGeneratedTests integrator (_suiteTests testSuite)
  measureIntegratedCoverage result

