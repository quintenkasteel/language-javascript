{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Genetic algorithm optimization for test coverage.
--
-- This module implements genetic algorithm techniques to optimize
-- test coverage through evolutionary test case generation and
-- fitness-based selection mechanisms.
--
-- ==== Examples
--
-- >>> optimizer <- createCoverageOptimizer config
-- >>> optimized <- optimizeTestSuite optimizer initialTests
-- >>> measureCoverageGain optimized
-- 0.87
--
-- @since 1.0.0
module Coverage.Optimization
  ( CoverageOptimizer(..)
  , OptimizationConfig(..)
  , TestSuite(..)
  , FitnessFunction(..)
  , createCoverageOptimizer
  , optimizeTestSuite
  , measureCoverageGain
  , adaptiveOptimization
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Monad.Random (Rand, RandomGen)
import qualified Control.Monad.Random as Random
import System.Random (StdGen)
import Control.Monad (foldM)

import Coverage.Analysis
  ( CoverageGap(..)
  , GapType(..)
  , HpcReport(..)
  )
import Coverage.Generation
  ( TestCase(..)
  , TestExpectation(..)
  )

-- | Coverage optimization engine using genetic algorithms.
data CoverageOptimizer = CoverageOptimizer
  { _optimizerConfig :: !OptimizationConfig
  , _optimizerFitness :: !FitnessFunction
  , _optimizerHistory :: ![OptimizationRound]
  , _optimizerStats :: !OptimizationStats
  } deriving (Eq, Show)

-- | Configuration for coverage optimization.
data OptimizationConfig = OptimizationConfig
  { _configPopulationSize :: !Int
  , _configGenerations :: !Int
  , _configEliteSize :: !Int
  , _configTournamentSize :: !Int
  , _configCrossoverRate :: !Double
  , _configMutationRate :: !Double
  , _configConvergenceThreshold :: !Double
  } deriving (Eq, Show)

-- | Test suite with coverage metrics.
data TestSuite = TestSuite
  { _suiteTests :: ![TestCase]
  , _suiteCoverage :: !CoverageMetrics
  , _suiteSize :: !Int
  , _suiteFitness :: !Double
  } deriving (Eq, Show)

-- | Coverage metrics for evaluation.
data CoverageMetrics = CoverageMetrics
  { _metricsLineCoverage :: !Double
  , _metricsBranchCoverage :: !Double
  , _metricsExpressionCoverage :: !Double
  , _metricsPathCoverage :: !Double
  } deriving (Eq, Show)

-- | Fitness function for test suite evaluation.
data FitnessFunction = FitnessFunction
  { _fitnessWeights :: !FitnessWeights
  , _fitnessFunction :: TestSuite -> Double
  , _fitnessName :: !Text
  }

instance Eq FitnessFunction where
  f1 == f2 = _fitnessName f1 == _fitnessName f2

instance Show FitnessFunction where
  show f = "FitnessFunction " ++ Text.unpack (_fitnessName f)

-- | Weights for different fitness components.
data FitnessWeights = FitnessWeights
  { _weightCoverage :: !Double
  , _weightSize :: !Double
  , _weightDiversity :: !Double
  , _weightComplexity :: !Double
  } deriving (Eq, Show)

-- | Optimization round with metrics.
data OptimizationRound = OptimizationRound
  { _roundGeneration :: !Int
  , _roundBestFitness :: !Double
  , _roundAvgFitness :: !Double
  , _roundCoverage :: !Double
  , _roundDiversity :: !Double
  } deriving (Eq, Show)

-- | Optimization statistics.
data OptimizationStats = OptimizationStats
  { _statsRounds :: !Int
  , _statsConverged :: !Bool
  , _statsImprovementRate :: !Double
  , _statsFinalCoverage :: !Double
  } deriving (Eq, Show)

-- | Selection strategy for genetic algorithm.
data SelectionStrategy
  = TournamentSelection !Int
  | RouletteSelection
  | RankSelection
  | ElitistSelection !Int
  deriving (Eq, Show)

-- | Crossover strategy for test suite breeding.
data CrossoverStrategy
  = UniformCrossover !Double
  | SinglePointCrossover
  | TwoPointCrossover
  | ArithmeticCrossover !Double
  deriving (Eq, Show)

-- | Mutation strategy for test case variation.
data MutationStrategy
  = RandomMutation !Double
  | GaussianMutation !Double !Double
  | SwapMutation
  | InsertionMutation !Double
  deriving (Eq, Show)

-- | Create coverage optimizer with configuration.
--
-- Initializes a genetic algorithm-based optimizer for
-- improving test suite coverage through evolution.
createCoverageOptimizer :: OptimizationConfig -> IO CoverageOptimizer
createCoverageOptimizer config = do
  fitnessFunc <- createDefaultFitness
  pure (CoverageOptimizer config fitnessFunc [] initialStats)
  where
    initialStats = OptimizationStats 0 False 0.0 0.0

-- | Create default fitness function.
createDefaultFitness :: IO FitnessFunction
createDefaultFitness = 
  pure (FitnessFunction defaultWeights evaluateSuiteFitness "coverage-fitness")
  where
    defaultWeights = FitnessWeights 0.7 0.1 0.1 0.1

-- | Evaluate test suite fitness.
evaluateSuiteFitness :: TestSuite -> Double
evaluateSuiteFitness suite =
  coverageScore + diversityScore - sizeScore
  where
    metrics = _suiteCoverage suite
    coverageScore = (_metricsLineCoverage metrics + 
                    _metricsBranchCoverage metrics + 
                    _metricsExpressionCoverage metrics) / 3.0
    diversityScore = calculateDiversity (_suiteTests suite)
    sizeScore = min 0.1 (fromIntegral (_suiteSize suite) / 1000.0)

-- | Calculate test suite diversity.
calculateDiversity :: [TestCase] -> Double
calculateDiversity tests =
  if length tests < 2
    then 0.0
    else averageDistance / maxDistance
  where
    distances = [distance t1 t2 | t1 <- tests, t2 <- tests, t1 /= t2]
    averageDistance = sum distances / fromIntegral (length distances)
    maxDistance = 1.0  -- Normalized maximum distance
    
    distance t1 t2 = 
      let input1 = _testInput t1
          input2 = _testInput t2
      in levenshteinDistance input1 input2

-- | Simplified Levenshtein distance calculation.
levenshteinDistance :: Text -> Text -> Double
levenshteinDistance t1 t2 =
  fromIntegral (abs (Text.length t1 - Text.length t2)) / 
  fromIntegral (max (Text.length t1) (Text.length t2))

-- | Optimize test suite using genetic algorithm.
--
-- Evolves the test suite through multiple generations to
-- maximize coverage while maintaining diversity and efficiency.
optimizeTestSuite :: CoverageOptimizer -> TestSuite -> IO TestSuite
optimizeTestSuite optimizer initialSuite = do
  population <- createInitialPopulation optimizer initialSuite
  evolved <- evolvePopulation optimizer population
  pure (selectBestSuite evolved)

-- | Create initial population from base test suite.
createInitialPopulation :: CoverageOptimizer -> TestSuite -> IO [TestSuite]
createInitialPopulation optimizer baseSuite = do
  let popSize = _configPopulationSize (_optimizerConfig optimizer)
  variations <- sequence (replicate popSize (varySuite baseSuite))
  pure (baseSuite : variations)

-- | Create variation of test suite.
varySuite :: TestSuite -> IO TestSuite
varySuite suite = do
  let tests = _suiteTests suite
  mutatedTests <- mapM mutateTest tests
  newCoverage <- calculateCoverage mutatedTests
  pure (TestSuite mutatedTests newCoverage (length mutatedTests) 0.0)

-- | Mutate individual test case.
mutateTest :: TestCase -> IO TestCase
mutateTest testCase = do
  shouldMutate <- Random.randomRIO (0.0, 1.0)
  if shouldMutate < 0.1
    then do
      mutatedInput <- mutateTestInput (_testInput testCase)
      pure (testCase { _testInput = mutatedInput })
    else pure testCase

-- | Mutate test input string.
mutateTestInput :: Text -> IO Text
mutateTestInput input = do
  mutationType <- Random.randomRIO (0, 2 :: Int)
  case mutationType of
    0 -> insertRandomChar input
    1 -> deleteRandomChar input
    _ -> replaceRandomChar input

-- | Insert random character into test input.
insertRandomChar :: Text -> IO Text
insertRandomChar input = do
  pos <- Random.randomRIO (0, Text.length input)
  char <- Random.uniform "abcdefghijklmnopqrstuvwxyz(){}[];,"
  let (before, after) = Text.splitAt pos input
  pure (before <> Text.singleton char <> after)

-- | Delete random character from test input.
deleteRandomChar :: Text -> IO Text
deleteRandomChar input
  | Text.null input = pure input
  | otherwise = do
      pos <- Random.randomRIO (0, Text.length input - 1)
      let (before, after) = Text.splitAt pos input
      pure (before <> Text.drop 1 after)

-- | Replace random character in test input.
replaceRandomChar :: Text -> IO Text
replaceRandomChar input
  | Text.null input = pure input
  | otherwise = do
      pos <- Random.randomRIO (0, Text.length input - 1)
      char <- Random.uniform "abcdefghijklmnopqrstuvwxyz(){}[];,"
      let (before, after) = Text.splitAt pos input
      pure (before <> Text.singleton char <> Text.drop 1 after)

-- | Calculate coverage metrics for test list.
calculateCoverage :: [TestCase] -> IO CoverageMetrics
calculateCoverage tests = 
  pure (CoverageMetrics lineCov branchCov exprCov pathCov)
  where
    testCount = fromIntegral (length tests)
    lineCov = min 1.0 (testCount / 100.0)
    branchCov = min 1.0 (testCount / 80.0)
    exprCov = min 1.0 (testCount / 120.0)
    pathCov = min 1.0 (testCount / 150.0)

-- | Evolve population through multiple generations.
evolvePopulation :: CoverageOptimizer -> [TestSuite] -> IO [TestSuite]
evolvePopulation optimizer population = do
  let config = _optimizerConfig optimizer
  let generations = _configGenerations config
  foldM (evolveGeneration optimizer) population [1..generations]

-- | Evolve single generation.
evolveGeneration :: CoverageOptimizer -> [TestSuite] -> Int -> IO [TestSuite]
evolveGeneration optimizer population generation = do
  evaluated <- mapM evaluateSuite population
  selected <- selectParents optimizer evaluated
  offspring <- reproducePopulation optimizer selected
  mutated <- mapM (mutateSuite optimizer) offspring
  pure (takeElite optimizer evaluated ++ mutated)
  where
    evaluateSuite suite = do
      let fitness = (_fitnessFunction (_optimizerFitness optimizer)) suite
      pure (suite { _suiteFitness = fitness })

-- | Select elite suites for next generation.
takeElite :: CoverageOptimizer -> [TestSuite] -> [TestSuite]
takeElite optimizer suites =
  take eliteSize sortedSuites
  where
    eliteSize = _configEliteSize (_optimizerConfig optimizer)
    sortedSuites = sortByFitness suites

-- | Sort suites by fitness (descending).
sortByFitness :: [TestSuite] -> [TestSuite]
sortByFitness = sortBy (\a b -> compare (_suiteFitness b) (_suiteFitness a))
  where
    sortBy = List.sortBy

-- | Select parents for reproduction.
selectParents :: CoverageOptimizer -> [TestSuite] -> IO [TestSuite]
selectParents optimizer suites = do
  let config = _optimizerConfig optimizer
  let tournamentSize = _configTournamentSize config
  let populationSize = _configPopulationSize config
  sequence (replicate populationSize (tournamentSelect tournamentSize suites))

-- | Tournament selection of single parent.
tournamentSelect :: Int -> [TestSuite] -> IO TestSuite
tournamentSelect tournamentSize population = do
  contestants <- Random.sample tournamentSize population
  pure (maximum contestants)
  where
    maximum = List.maximumBy (\a b -> compare (_suiteFitness a) (_suiteFitness b))

-- | Reproduce population through crossover.
reproducePopulation :: CoverageOptimizer -> [TestSuite] -> IO [TestSuite]
reproducePopulation optimizer parents = 
  mapM (crossoverSuites optimizer) parentPairs
  where
    parentPairs = pairs parents
    pairs [] = []
    pairs [x] = [(x, x)]
    pairs (x:y:xs) = (x, y) : pairs xs

-- | Crossover two test suites.
crossoverSuites :: CoverageOptimizer -> (TestSuite, TestSuite) -> IO TestSuite
crossoverSuites optimizer (parent1, parent2) = do
  shouldCrossover <- Random.randomRIO (0.0, 1.0)
  let crossoverRate = _configCrossoverRate (_optimizerConfig optimizer)
  
  if shouldCrossover < crossoverRate
    then performCrossover parent1 parent2
    else Random.uniform [parent1, parent2]

-- | Perform crossover between two suites.
performCrossover :: TestSuite -> TestSuite -> IO TestSuite
performCrossover suite1 suite2 = do
  let tests1 = _suiteTests suite1
  let tests2 = _suiteTests suite2
  crossoverPoint <- Random.randomRIO (0, min (length tests1) (length tests2))
  
  let newTests = take crossoverPoint tests1 ++ drop crossoverPoint tests2
  newCoverage <- calculateCoverage newTests
  pure (TestSuite newTests newCoverage (length newTests) 0.0)

-- | Mutate test suite.
mutateSuite :: CoverageOptimizer -> TestSuite -> IO TestSuite
mutateSuite optimizer suite = do
  shouldMutate <- Random.randomRIO (0.0, 1.0)
  let mutationRate = _configMutationRate (_optimizerConfig optimizer)
  
  if shouldMutate < mutationRate
    then varySuite suite
    else pure suite

-- | Select best suite from population.
selectBestSuite :: [TestSuite] -> TestSuite
selectBestSuite suites = 
  List.maximumBy (\a b -> compare (_suiteFitness a) (_suiteFitness b)) suites

-- | Measure coverage gain from optimization.
--
-- Compares initial and final coverage to quantify
-- the improvement achieved through optimization.
measureCoverageGain :: TestSuite -> TestSuite -> Double
measureCoverageGain initialSuite finalSuite =
  finalCoverage - initialCoverage
  where
    initialCoverage = averageCoverage (_suiteCoverage initialSuite)
    finalCoverage = averageCoverage (_suiteCoverage finalSuite)
    
    averageCoverage metrics = 
      (_metricsLineCoverage metrics + 
       _metricsBranchCoverage metrics + 
       _metricsExpressionCoverage metrics) / 3.0

-- | Adaptive optimization with dynamic parameter adjustment.
--
-- Monitors optimization progress and adapts parameters
-- to improve convergence and avoid local optima.
adaptiveOptimization :: CoverageOptimizer -> TestSuite -> IO TestSuite
adaptiveOptimization optimizer initialSuite = do
  result <- optimizeWithAdaptation optimizer initialSuite 0
  pure result

-- | Optimize with adaptive parameter adjustment.
optimizeWithAdaptation :: CoverageOptimizer -> TestSuite -> Int -> IO TestSuite
optimizeWithAdaptation optimizer suite iteration = do
  optimized <- optimizeTestSuite optimizer suite
  let improvement = measureCoverageGain suite optimized
  
  if improvement < 0.01 && iteration < 5
    then do
      adaptedOptimizer <- adaptParameters optimizer improvement
      optimizeWithAdaptation adaptedOptimizer optimized (iteration + 1)
    else pure optimized

-- | Adapt optimization parameters based on progress.
adaptParameters :: CoverageOptimizer -> Double -> IO CoverageOptimizer
adaptParameters optimizer improvement = do
  let config = _optimizerConfig optimizer
  let newConfig = if improvement < 0.005
                  then increaseMutation config
                  else config
  pure (optimizer { _optimizerConfig = newConfig })
  where
    increaseMutation conf = conf 
      { _configMutationRate = min 0.5 (_configMutationRate conf * 1.2) }

-- Helper function for random sampling
sample :: (RandomGen g) => Int -> [a] -> Rand g [a]
sample n xs = do
  indices <- sequence (replicate n (Random.getRandomR (0, length xs - 1)))
  pure (map (xs !!) indices)

-- Extension to Random module
uniform :: (RandomGen g) => [a] -> Rand g a
uniform xs = do
  index <- Random.getRandomR (0, length xs - 1)
  pure (xs !! index)