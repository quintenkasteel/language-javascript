{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Machine learning-driven test case generation.
--
-- This module implements intelligent test case synthesis using
-- machine learning techniques to generate tests that target
-- specific coverage gaps and improve overall coverage.
--
-- ==== Examples
--
-- >>> generator <- createMLGenerator config
-- >>> testCases <- generateTestCases generator gaps
-- >>> length testCases
-- 25
--
-- @since 1.0.0
module Coverage.Generation
  ( MLGenerator(..)
  , TestCase(..)
  , GenerationConfig(..)
  , GenerationStrategy(..)
  , createMLGenerator
  , generateTestCases
  , evaluateTestCase
  , optimizeGeneration
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Monad (foldM)
import Control.Monad.Random (Rand, RandomGen)
import qualified Control.Monad.Random as Random
import Data.List (sortBy)
import System.Random (StdGen)

import Coverage.Analysis
  ( CoverageGap(..)
  , GapType(..)
  )

-- | Machine learning-based test generator.
data MLGenerator = MLGenerator
  { _generatorConfig :: !GenerationConfig
  , _generatorModel :: !MLModel
  , _generatorHistory :: ![TestCase]
  , _generatorStats :: !GenerationStats
  } deriving (Eq, Show)

-- | Configuration for test generation.
data GenerationConfig = GenerationConfig
  { _configStrategy :: !GenerationStrategy
  , _configMaxTests :: !Int
  , _configTargetCoverage :: !Double
  , _configMutationRate :: !Double
  } deriving (Eq, Show)

-- | Test generation strategy.
data GenerationStrategy
  = RandomGeneration
  | GeneticAlgorithm !GeneticConfig
  | MachineLearning !MLConfig
  | HybridApproach !HybridConfig
  deriving (Eq, Show)

-- | Genetic algorithm configuration.
data GeneticConfig = GeneticConfig
  { _geneticPopSize :: !Int
  , _geneticGenerations :: !Int
  , _geneticCrossoverRate :: !Double
  , _geneticMutationRate :: !Double
  } deriving (Eq, Show)

-- | Machine learning configuration.
data MLConfig = MLConfig
  { _mlModelType :: !MLModelType
  , _mlTrainingSize :: !Int
  , _mlFeatureSet :: ![Text]
  , _mlOptimizer :: !OptimizerType
  } deriving (Eq, Show)

-- | Hybrid approach configuration.
data HybridConfig = HybridConfig
  { _hybridStrategies :: ![GenerationStrategy]
  , _hybridWeights :: ![Double]
  , _hybridAdaptive :: !Bool
  } deriving (Eq, Show)

-- | Machine learning model type.
data MLModelType
  = NeuralNetwork !NetworkConfig
  | DecisionTree !TreeConfig
  | RandomForest !ForestConfig
  | GradientBoosting !BoostingConfig
  deriving (Eq, Show)

-- | Neural network configuration.
data NetworkConfig = NetworkConfig
  { _networkLayers :: ![Int]
  , _networkActivation :: !ActivationType
  , _networkDropout :: !Double
  } deriving (Eq, Show)

-- | Decision tree configuration.
data TreeConfig = TreeConfig
  { _treeMaxDepth :: !Int
  , _treeMinSamples :: !Int
  , _treeCriterion :: !SplitCriterion
  } deriving (Eq, Show)

-- | Random forest configuration.
data ForestConfig = ForestConfig
  { _forestTrees :: !Int
  , _forestFeatures :: !Int
  , _forestBootstrap :: !Bool
  } deriving (Eq, Show)

-- | Gradient boosting configuration.
data BoostingConfig = BoostingConfig
  { _boostingEstimators :: !Int
  , _boostingLearningRate :: !Double
  , _boostingMaxDepth :: !Int
  } deriving (Eq, Show)

-- | Activation function type.
data ActivationType = ReLU | Sigmoid | Tanh | Softmax
  deriving (Eq, Show)

-- | Split criterion for decision trees.
data SplitCriterion = Gini | Entropy | MSE
  deriving (Eq, Show)

-- | Optimizer type for ML training.
data OptimizerType = SGD | Adam | RMSprop | AdaGrad
  deriving (Eq, Show)

-- | Machine learning model.
data MLModel = MLModel
  { _modelWeights :: !(Vector Double)
  , _modelBiases :: !(Vector Double)
  , _modelFeatures :: ![Text]
  , _modelAccuracy :: !Double
  } deriving (Eq, Show)

-- | Generated test case.
data TestCase = TestCase
  { _testInput :: !Text  -- JavaScript code
  , _testExpected :: !TestExpectation
  , _testTargetGaps :: ![CoverageGap]
  , _testFitness :: !Double
  } deriving (Eq, Show)

-- | Test case expectation.
data TestExpectation
  = ShouldParse
  | ShouldFail !Text  -- Expected error message
  | ShouldCover ![Int]  -- Expected covered lines
  deriving (Eq, Show)

-- | Generation statistics.
data GenerationStats = GenerationStats
  { _statsGenerated :: !Int
  , _statsSuccessful :: !Int
  , _statsCoverageGain :: !Double
  , _statsGenerationTime :: !Double
  } deriving (Eq, Show)

-- | Create ML-based test generator.
--
-- Initializes a new machine learning test generator with
-- the specified configuration and training data.
createMLGenerator :: GenerationConfig -> IO MLGenerator
createMLGenerator config = do
  model <- initializeModel config
  pure (MLGenerator config model [] initialStats)
  where
    initialStats = GenerationStats 0 0 0.0 0.0

-- | Initialize ML model based on configuration.
initializeModel :: GenerationConfig -> IO MLModel
initializeModel config =
  case _configStrategy config of
    MachineLearning mlConfig -> 
      initializeMLModel mlConfig
    _ -> 
      pure defaultModel
  where
    defaultModel = MLModel Vector.empty Vector.empty [] 0.0

-- | Initialize specific ML model type.
initializeMLModel :: MLConfig -> IO MLModel
initializeMLModel config =
  case _mlModelType config of
    NeuralNetwork netConfig -> 
      initializeNeuralNetwork netConfig
    DecisionTree treeConfig -> 
      initializeDecisionTree treeConfig
    RandomForest forestConfig -> 
      initializeRandomForest forestConfig
    GradientBoosting boostConfig -> 
      initializeGradientBoosting boostConfig

-- | Initialize neural network model.
initializeNeuralNetwork :: NetworkConfig -> IO MLModel
initializeNeuralNetwork config = do
  let layers = _networkLayers config
  weights <- initializeWeights layers
  biases <- initializeBiases layers
  pure (MLModel weights biases defaultFeatures 0.0)
  where
    defaultFeatures = ["input_length", "complexity", "nesting_depth"]

-- | Initialize decision tree model.
initializeDecisionTree :: TreeConfig -> IO MLModel
initializeDecisionTree _config = 
  pure (MLModel Vector.empty Vector.empty defaultFeatures 0.0)
  where
    defaultFeatures = ["ast_depth", "token_count", "branch_count"]

-- | Initialize random forest model.
initializeRandomForest :: ForestConfig -> IO MLModel
initializeRandomForest _config = 
  pure (MLModel Vector.empty Vector.empty defaultFeatures 0.0)
  where
    defaultFeatures = ["syntax_complexity", "semantic_features"]

-- | Initialize gradient boosting model.
initializeGradientBoosting :: BoostingConfig -> IO MLModel
initializeGradientBoosting _config = 
  pure (MLModel Vector.empty Vector.empty defaultFeatures 0.0)
  where
    defaultFeatures = ["coverage_potential", "error_likelihood"]

-- | Initialize neural network weights.
initializeWeights :: [Int] -> IO (Vector Double)
initializeWeights layers = do
  let totalWeights = sum (zipWith (*) layers (tail layers))
  weights <- sequence (replicate totalWeights (Random.randomRIO (-1.0, 1.0)))
  pure (Vector.fromList weights)

-- | Initialize neural network biases.
initializeBiases :: [Int] -> IO (Vector Double)
initializeBiases layers = do
  let totalBiases = sum (tail layers)
  biases <- sequence (replicate totalBiases (Random.randomRIO (-0.1, 0.1)))
  pure (Vector.fromList biases)

-- | Generate test cases targeting specific coverage gaps.
--
-- Uses the configured generation strategy to create test cases
-- that specifically target the identified coverage gaps.
generateTestCases :: MLGenerator -> [CoverageGap] -> IO [TestCase]
generateTestCases generator gaps = 
  case _configStrategy (_generatorConfig generator) of
    RandomGeneration -> 
      generateRandomTests generator gaps
    GeneticAlgorithm genConfig -> 
      generateGeneticTests generator genConfig gaps
    MachineLearning mlConfig -> 
      generateMLTests generator mlConfig gaps
    HybridApproach hybridConfig -> 
      generateHybridTests generator hybridConfig gaps

-- | Generate random test cases.
generateRandomTests :: MLGenerator -> [CoverageGap] -> IO [TestCase]
generateRandomTests generator gaps = do
  let maxTests = _configMaxTests (_generatorConfig generator)
  sequence (take maxTests (map generateRandomTest gaps))
  where
    generateRandomTest gap = do
      input <- generateRandomInput gap
      pure (TestCase input ShouldParse [gap] 0.5)

-- | Generate test cases using genetic algorithm.
generateGeneticTests :: MLGenerator -> GeneticConfig -> [CoverageGap] -> IO [TestCase]
generateGeneticTests generator genConfig gaps = do
  initialPop <- generateInitialPopulation genConfig gaps
  evolvedPop <- evolvePopulation genConfig initialPop
  pure (take (_configMaxTests (_generatorConfig generator)) evolvedPop)

-- | Generate test cases using machine learning.
generateMLTests :: MLGenerator -> MLConfig -> [CoverageGap] -> IO [TestCase]
generateMLTests generator mlConfig gaps = do
  features <- extractFeatures gaps
  predictions <- predictTestCases (_generatorModel generator) features
  pure (zipWith (createMLTestCase) gaps predictions)
  where
    createMLTestCase gap prediction = TestCase
      { _testInput = generateInputFromPrediction gap prediction
      , _testExpected = ShouldParse
      , _testTargetGaps = [gap]
      , _testFitness = prediction
      }

-- | Generate test cases using hybrid approach.
generateHybridTests :: MLGenerator -> HybridConfig -> [CoverageGap] -> IO [TestCase]
generateHybridTests generator hybridConfig gaps = do
  results <- mapM generateStrategyTests strategiesWithWeights
  let weightedResults = concatMap weightTests results
  pure (take maxTests weightedResults)
  where
    strategies = _hybridStrategies hybridConfig
    weights = _hybridWeights hybridConfig
    strategiesWithWeights = zip strategies weights
    maxTests = _configMaxTests (_generatorConfig generator)
    
    generateStrategyTests (strategy, weight) = do
      let tempConfig = (_generatorConfig generator) { _configStrategy = strategy }
      let tempGenerator = generator { _generatorConfig = tempConfig }
      tests <- generateTestCases tempGenerator gaps
      pure (tests, weight)
    
    weightTests (tests, weight) = 
      map (\test -> test { _testFitness = _testFitness test * weight }) tests

-- | Generate initial population for genetic algorithm.
generateInitialPopulation :: GeneticConfig -> [CoverageGap] -> IO [TestCase]
generateInitialPopulation config gaps = 
  sequence (replicate popSize generateRandomTestCase)
  where
    popSize = _geneticPopSize config
    generateRandomTestCase = do
      gap <- Random.uniform gaps
      input <- generateRandomInput gap
      pure (TestCase input ShouldParse [gap] 0.0)

-- | Evolve population using genetic algorithm.
evolvePopulation :: GeneticConfig -> [TestCase] -> IO [TestCase]
evolvePopulation config population = do
  let generations = _geneticGenerations config
  foldM evolveGeneration population [1..generations]
  where
    evolveGeneration pop _gen = do
      scored <- mapM evaluateTestCase pop
      selected <- selectParents config scored
      offspring <- reproducePopulation config selected
      pure offspring

-- | Select parents for reproduction.
selectParents :: GeneticConfig -> [TestCase] -> IO [TestCase]
selectParents _config population = 
  pure (take (length population `div` 2) sortedPop)
  where
    sortedPop = sortByFitness population
    sortByFitness = sortBy (\a b -> compare (_testFitness b) (_testFitness a))

-- | Reproduce population through crossover and mutation.
reproducePopulation :: GeneticConfig -> [TestCase] -> IO [TestCase]
reproducePopulation config parents = do
  offspring <- mapM (crossoverTests config) parentPairs
  mutated <- mapM (mutateTest config) offspring
  pure mutated
  where
    parentPairs = pairs parents
    pairs [] = []
    pairs [x] = [(x, x)]
    pairs (x:y:xs) = (x, y) : pairs xs

-- | Crossover two test cases.
crossoverTests :: GeneticConfig -> (TestCase, TestCase) -> IO TestCase
crossoverTests _config (parent1, parent2) = do
  crossoverPoint <- Random.randomRIO (0.0 :: Double, 1.0 :: Double)
  let input1 = _testInput parent1
  let input2 = _testInput parent2
  let crossedInput = if crossoverPoint < 0.5 then input1 else input2
  pure (parent1 { _testInput = crossedInput })

-- | Mutate a test case.
mutateTest :: GeneticConfig -> TestCase -> IO TestCase
mutateTest config testCase = do
  shouldMutate <- Random.randomRIO (0.0, 1.0)
  if shouldMutate < _geneticMutationRate config
    then do
      mutatedInput <- mutateInput (_testInput testCase)
      pure (testCase { _testInput = mutatedInput })
    else pure testCase

-- | Mutate test input.
mutateInput :: Text -> IO Text
mutateInput input = do
  let chars = Text.unpack input
  mutatedChars <- mapM mutateChar chars
  pure (Text.pack mutatedChars)
  where
    mutateChar c = do
      shouldMutate <- Random.randomRIO (0.0 :: Double, 1.0 :: Double)
      if shouldMutate < 0.1
        then Random.uniform ['a'..'z']
        else pure c

-- | Extract features from coverage gaps for ML.
extractFeatures :: [CoverageGap] -> IO (Vector Double)
extractFeatures gaps = 
  pure (Vector.fromList features)
  where
    features = [gapCount, avgPriority, lineRatio, branchRatio]
    gapCount = fromIntegral (length gaps)
    avgPriority = if null gaps then 0.0 else average (map _gapPriority gaps)
    lineRatio = ratio isLineGap
    branchRatio = ratio isBranchGap
    
    average xs = sum xs / fromIntegral (length xs)
    ratio predicate = fromIntegral (length (filter predicate gaps)) / gapCount
    
    isLineGap gap = case _gapType gap of
      UncoveredLine _ -> True
      _ -> False
    
    isBranchGap gap = case _gapType gap of
      UncoveredBranch _ -> True
      _ -> False

-- | Predict test cases using ML model.
predictTestCases :: MLModel -> Vector Double -> IO [Double]
predictTestCases model features = 
  pure [dotProduct (_modelWeights model) features]
  where
    dotProduct v1 v2 = Vector.sum (Vector.zipWith (*) v1 v2)

-- | Generate random input for coverage gap.
generateRandomInput :: CoverageGap -> IO Text
generateRandomInput gap = 
  case _gapType gap of
    UncoveredLine _ -> pure "var x = 42;"
    UncoveredBranch _ -> pure "if (true) { x = 1; } else { x = 2; }"
    UncoveredExpression _ -> pure "x + y * z"
    UntestedPath _ -> pure "while (x < 10) { x++; }"

-- | Generate input from ML prediction.
generateInputFromPrediction :: CoverageGap -> Double -> Text
generateInputFromPrediction gap prediction =
  if prediction > 0.5
    then enhanceInput gap
    else basicInput gap
  where
    basicInput _ = "var x = 1;"
    enhanceInput _ = "function test() { return 42; }"

-- | Evaluate test case fitness.
--
-- Calculates the fitness score for a test case based on
-- its potential to improve coverage and code quality.
evaluateTestCase :: TestCase -> IO TestCase
evaluateTestCase testCase = do
  fitness <- calculateFitness testCase
  pure (testCase { _testFitness = fitness })

-- | Calculate fitness score for test case.
calculateFitness :: TestCase -> IO Double
calculateFitness testCase = do
  let input = _testInput testCase
  let targets = _testTargetGaps testCase
  let complexityScore = min 0.3 (fromIntegral (Text.length input) / 100.0)
  let targetScore = min 0.4 (fromIntegral (length targets) / 10.0)
  let diversityScore = 0.3  -- Simplified for now
  
  pure (complexityScore + targetScore + diversityScore)

-- | Optimize generation strategy based on results.
--
-- Analyzes generation results and adapts the strategy
-- to improve coverage gains and test quality.
optimizeGeneration :: MLGenerator -> [TestCase] -> IO MLGenerator
optimizeGeneration generator testCases = do
  let stats = analyzeResults testCases
  let newConfig = adaptConfig (_generatorConfig generator) stats
  pure (generator { _generatorConfig = newConfig, _generatorStats = stats })

-- | Analyze test generation results.
analyzeResults :: [TestCase] -> GenerationStats
analyzeResults testCases = GenerationStats
  { _statsGenerated = length testCases
  , _statsSuccessful = length (filter isSuccessful testCases)
  , _statsCoverageGain = average (map _testFitness testCases)
  , _statsGenerationTime = 0.0  -- Would be measured in real implementation
  }
  where
    isSuccessful testCase = _testFitness testCase > 0.5
    average xs = if null xs then 0.0 else sum xs / fromIntegral (length xs)

-- | Adapt configuration based on analysis.
adaptConfig :: GenerationConfig -> GenerationStats -> GenerationConfig
adaptConfig config stats =
  if _statsSuccessful stats < _statsGenerated stats `div` 2
    then increaseExploration config
    else config
  where
    increaseExploration conf = conf { _configMutationRate = _configMutationRate conf * 1.1 }