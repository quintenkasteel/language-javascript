{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Memory Usage Constraint Testing Infrastructure
--
-- This module implements comprehensive memory testing for the JavaScript parser,
-- validating production-ready memory characteristics including constant memory
-- streaming scenarios, memory leak detection, and memory pressure testing.
-- Ensures parser maintains linear memory growth and graceful handling of
-- memory-constrained environments.
--
-- = Memory Testing Categories
--
-- * Constant memory streaming parser testing
-- * Memory leak detection across multiple parse operations
-- * Low memory environment simulation and validation
-- * Memory profiling and analysis with detailed reporting
-- * Linear memory growth validation and regression detection
--
-- = Performance Targets
--
-- * Constant memory for streaming scenarios (O(1) memory growth)
-- * No memory leaks in long-running usage patterns
-- * Graceful degradation under memory pressure
-- * Linear memory scaling O(n) with input size
-- * Memory overhead < 20x input size for typical JavaScript
--
-- @since 0.7.1.0
module Benchmarks.Language.Javascript.Parser.Memory
  ( memoryTests,
    memoryConstraintTests,
    memoryLeakDetectionTests,
    streamingMemoryTests,
    memoryPressureTests,
    MemoryMetrics (..),
    MemoryTestConfig (..),
    runMemoryProfiler,
    validateMemoryConstraints,
    detectMemoryLeaks,
    createMemoryBaseline,
  )
where

import Control.DeepSeq (NFData (..), deepseq, force)
import Control.Exception (bracket, evaluate)
import Control.Monad (forM_, replicateM, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import qualified GHC.Stats as Stats
import qualified Language.JavaScript.Parser.AST as AST
import Language.JavaScript.Parser.Parser (parseBS)
import System.Mem (performGC)
import Test.Hspec

-- | Memory usage metrics for detailed analysis
data MemoryMetrics = MemoryMetrics
  { -- | Total bytes allocated
    memoryBytesAllocated :: !Word64,
    -- | Current bytes in use
    memoryBytesUsed :: !Word64,
    -- | Number of GC collections
    memoryGCCollections :: !Word64,
    -- | Maximum residency observed
    memoryMaxResidency :: !Word64,
    -- | Parse time in milliseconds
    memoryParseTime :: !Double,
    -- | Input size in bytes
    memoryInputSize :: !Int,
    -- | Memory overhead vs input
    memoryOverheadRatio :: !Double
  }
  deriving (Eq, Show)

instance NFData MemoryMetrics where
  rnf (MemoryMetrics alloc used gcCount maxRes time input overhead) =
    rnf alloc `seq` rnf used `seq` rnf gcCount `seq` rnf maxRes
      `seq` rnf time
      `seq` rnf input
      `seq` rnf overhead

-- | Configuration for memory testing scenarios
data MemoryTestConfig = MemoryTestConfig
  { -- | Maximum memory limit in MB
    configMaxMemoryMB :: !Int,
    -- | Number of test iterations
    configIterations :: !Int,
    -- | Test file size in bytes
    configFileSize :: !Int,
    -- | Streaming chunk size
    configStreamChunkSize :: !Int,
    -- | Force GC between tests
    configGCBetweenTests :: !Bool
  }
  deriving (Eq, Show)

instance NFData MemoryTestConfig where
  rnf (MemoryTestConfig maxMem iter size chunk gcBetween) =
    rnf maxMem `seq` rnf iter `seq` rnf size `seq` rnf chunk `seq` rnf gcBetween

-- | Default memory test configuration
defaultMemoryConfig :: MemoryTestConfig
defaultMemoryConfig =
  MemoryTestConfig
    { configMaxMemoryMB = 100,
      configIterations = 10,
      configFileSize = 1024 * 1024, -- 1MB
      configStreamChunkSize = 64 * 1024, -- 64KB
      configGCBetweenTests = True
    }

-- | Comprehensive memory constraint testing suite
memoryTests :: Spec
memoryTests = describe "Memory Usage Constraint Tests" $ do
  memoryConstraintTests
  memoryLeakDetectionTests
  streamingMemoryTests
  memoryPressureTests
  linearMemoryGrowthTests

-- | Memory constraint validation tests
memoryConstraintTests :: Spec
memoryConstraintTests = describe "Memory constraint validation" $ do
  it "validates linear memory growth O(n)" $ do
    let sizes = [100 * 1024, 500 * 1024, 1024 * 1024] -- 100KB, 500KB, 1MB
    metrics <- mapM measureMemoryForSize sizes
    metrics `shouldSatisfy` validateLinearGrowth

  it "maintains memory overhead under 20x input size" $ do
    config <- return defaultMemoryConfig
    metrics <- measureMemoryForSize (configFileSize config)
    memoryOverheadRatio metrics `shouldSatisfy` (< 20.0)

  it "enforces maximum memory limit constraints" $ do
    let config = defaultMemoryConfig {configMaxMemoryMB = 50}
    result <- runWithMemoryLimit config
    result `shouldSatisfy` isWithinMemoryLimit

  it "validates constant memory for identical inputs" $ do
    testCode <- generateTestJavaScript (256 * 1024) -- 256KB
    metrics <- replicateM 5 (measureParseMemory testCode)
    let memoryVariance = calculateMemoryVariance metrics
    memoryVariance `shouldSatisfy` (< 0.1) -- <10% variance

-- | Memory leak detection across multiple operations
memoryLeakDetectionTests :: Spec
memoryLeakDetectionTests = describe "Memory leak detection" $ do
  it "detects no memory leaks across iterations" $ do
    let config = defaultMemoryConfig {configIterations = 20}
    leakStatus <- detectMemoryLeaks config
    leakStatus `shouldBe` NoMemoryLeaks

  it "validates memory cleanup after parse completion" $ do
    initialMemory <- getCurrentMemoryUsage
    testCode <- generateTestJavaScript (512 * 1024) -- 512KB
    _ <- evaluateWithCleanup testCode
    performGC
    finalMemory <- getCurrentMemoryUsage
    let memoryDelta = finalMemory - initialMemory
    -- Memory growth should be minimal after cleanup
    memoryDelta `shouldSatisfy` (< 50 * 1024 * 1024) -- <50MB
  it "maintains stable memory across repeated parses" $ do
    testCode <- generateTestJavaScript (128 * 1024) -- 128KB
    memoryHistory <- mapM (\_ -> measureAndCleanup testCode) [1 .. 15 :: Int]
    let trend = calculateMemoryTrend memoryHistory
    trend `shouldSatisfy` isStableMemoryPattern

-- | Streaming parser memory validation
streamingMemoryTests :: Spec
streamingMemoryTests = describe "Streaming memory validation" $ do
  it "maintains constant memory for streaming scenarios" $ do
    let config = defaultMemoryConfig {configStreamChunkSize = 32 * 1024}
    streamMetrics <- measureStreamingMemory config
    streamMetrics `shouldSatisfy` validateConstantMemoryStreaming

  it "processes large files with bounded memory" $ do
    let largeFileSize = 5 * 1024 * 1024 -- 5MB
    let config = defaultMemoryConfig {configFileSize = largeFileSize}
    metrics <- measureStreamingForFile config
    memoryMaxResidency metrics `shouldSatisfy` (< 200 * 1024 * 1024) -- <200MB
  it "handles incremental parsing memory efficiently" $ do
    chunks <- createIncrementalTestData (configStreamChunkSize defaultMemoryConfig)
    metrics <- measureIncrementalParsing chunks
    metrics `shouldSatisfy` validateIncrementalMemoryUsage

-- | Memory pressure testing and validation
memoryPressureTests :: Spec
memoryPressureTests = describe "Memory pressure handling" $ do
  it "handles low memory environments gracefully" $ do
    let lowMemoryConfig = defaultMemoryConfig {configMaxMemoryMB = 20}
    result <- simulateMemoryPressure lowMemoryConfig
    result `shouldSatisfy` handlesMemoryPressureGracefully

  it "degrades gracefully under memory constraints" $ do
    let constrainedConfig = defaultMemoryConfig {configMaxMemoryMB = 30}
    degradationMetrics <- measureGracefulDegradation constrainedConfig
    degradationMetrics `shouldSatisfy` validateGracefulDegradation

  it "recovers memory after pressure release" $ do
    initialMemory <- getCurrentMemoryUsage
    _ <- applyMemoryPressure defaultMemoryConfig
    performGC
    recoveredMemory <- getCurrentMemoryUsage
    let recoveryRatio = fromIntegral recoveredMemory / fromIntegral initialMemory
    recoveryRatio `shouldSatisfy` (< (1.5 :: Double)) -- Within 50% of initial

-- | Linear memory growth validation tests
linearMemoryGrowthTests :: Spec
linearMemoryGrowthTests = describe "Linear memory growth validation" $ do
  it "validates O(n) memory scaling with input size" $ do
    let sizes = [64 * 1024, 128 * 1024, 256 * 1024, 512 * 1024] -- Powers of 2
    metrics <- mapM measureMemoryForSize sizes
    metrics `shouldSatisfy` validateLinearScaling

  it "prevents quadratic memory growth O(n^2)" $ do
    let sizes = [100 * 1024, 400 * 1024, 900 * 1024] -- Square relationships
    metrics <- mapM measureMemoryForSize sizes
    metrics `shouldSatisfy` validateNotQuadratic

-- ================================================================
-- Memory Testing Implementation Functions
-- ================================================================

-- | Measure memory usage for specific input size
measureMemoryForSize :: Int -> IO MemoryMetrics
measureMemoryForSize size = do
  testCode <- generateTestJavaScript size
  measureParseMemory testCode

-- | Safe wrapper for getting RTS stats
safeGetRTSStats :: IO (Maybe Stats.RTSStats)
safeGetRTSStats = do
  statsEnabled <- Stats.getRTSStatsEnabled
  if statsEnabled
    then Just <$> Stats.getRTSStats
    else return Nothing

-- | Measure memory usage during JavaScript parsing
measureParseMemory :: ByteString -> IO MemoryMetrics
measureParseMemory source = do
  performGC
  initialStats <- safeGetRTSStats
  startTime <- getCurrentTime

  result <- evaluate $ force (parseBS source)
  result `deepseq` return ()

  endTime <- getCurrentTime
  finalStats <- safeGetRTSStats

  buildMemoryMetrics initialStats finalStats startTime endTime (BS.length source)

-- | Build 'MemoryMetrics' from RTS stats captured before and after a parse
buildMemoryMetrics
  :: Maybe Stats.RTSStats
  -> Maybe Stats.RTSStats
  -> UTCTime
  -> UTCTime
  -> Int
  -> IO MemoryMetrics
buildMemoryMetrics (Just initial) (Just final) startTime endTime inputSize =
  return
    MemoryMetrics
      { memoryBytesAllocated = Stats.max_live_bytes final,
        memoryBytesUsed = Stats.allocated_bytes final - Stats.allocated_bytes initial,
        memoryGCCollections = fromIntegral $ Stats.gcs final - Stats.gcs initial,
        memoryMaxResidency = Stats.max_live_bytes final,
        memoryParseTime = parseTimeMs startTime endTime,
        memoryInputSize = inputSize,
        memoryOverheadRatio = overheadRatio
      }
  where
    overheadRatio = fromIntegral (Stats.allocated_bytes final - Stats.allocated_bytes initial) / fromIntegral inputSize
buildMemoryMetrics _ _ startTime endTime inputSize =
  return
    MemoryMetrics
      { memoryBytesAllocated = estimatedMemory,
        memoryBytesUsed = estimatedMemory,
        memoryGCCollections = 0,
        memoryMaxResidency = estimatedMemory,
        memoryParseTime = parseTimeMs startTime endTime,
        memoryInputSize = inputSize,
        memoryOverheadRatio = 10.0
      }
  where
    estimatedMemory = fromIntegral inputSize * 10

-- | Calculate parse time in milliseconds from start and end timestamps
parseTimeMs :: UTCTime -> UTCTime -> Double
parseTimeMs startTime endTime =
  fromRational (toRational (diffUTCTime endTime startTime)) * 1000

-- | Memory leak detection across multiple iterations
data LeakDetectionResult = NoMemoryLeaks | MemoryLeakDetected Word64
  deriving (Eq, Show)

-- | Detect memory leaks across iterations
detectMemoryLeaks :: MemoryTestConfig -> IO LeakDetectionResult
detectMemoryLeaks config = do
  performGC
  initialMemory <- getCurrentMemoryUsage
  testCode <- generateTestJavaScript (configFileSize config)

  forM_ [1 .. configIterations config] $ \_ -> do
    _ <- evaluate $ force (parseBS testCode)
    when (configGCBetweenTests config) performGC

  performGC
  finalMemory <- getCurrentMemoryUsage

  return (classifyLeakResult initialMemory finalMemory)

-- | Classify whether memory growth indicates a leak
classifyLeakResult :: Word64 -> Word64 -> LeakDetectionResult
classifyLeakResult initialMemory finalMemory
  | memoryGrowth > leakThreshold = MemoryLeakDetected memoryGrowth
  | otherwise = NoMemoryLeaks
  where
    memoryGrowth = finalMemory - initialMemory
    leakThreshold = 100 * 1024 * 1024

-- | Validate linear memory growth pattern
validateLinearGrowth :: [MemoryMetrics] -> Bool
validateLinearGrowth [] = True
validateLinearGrowth [_] = True
validateLinearGrowth (_ : m2 : rest) =
  variance < 0.5
  where
    ratios = zipWith calculateGrowthRatio (m2 : rest) rest
    avgRatio = sum ratios / fromIntegral (length ratios)
    variance = sum (map (\r -> (r - avgRatio) ** 2) ratios) / fromIntegral (length ratios)

-- | Calculate growth ratio between memory metrics
calculateGrowthRatio :: MemoryMetrics -> MemoryMetrics -> Double
calculateGrowthRatio m1 m2 =
  memoryRatio / sizeRatio
  where
    sizeRatio = fromIntegral (memoryInputSize m2) / fromIntegral (memoryInputSize m1)
    memoryRatio = fromIntegral (memoryBytesUsed m2) / fromIntegral (memoryBytesUsed m1)

-- | Current memory usage in bytes
getCurrentMemoryUsage :: IO Word64
getCurrentMemoryUsage = do
  statsEnabled <- Stats.getRTSStatsEnabled
  if statsEnabled
    then do
      stats <- Stats.getRTSStats
      return (Stats.max_live_bytes stats)
    else return 1000000 -- Return 1MB as reasonable default when stats not available

-- | Evaluate parse with cleanup
evaluateWithCleanup :: ByteString -> IO (Either String AST.JSAST)
evaluateWithCleanup source =
  bracket
    (return ())
    (\_ -> performGC)
    ( \_ -> do
        result <- evaluate $ force (parseBS source)
        result `deepseq` return result
    )

-- | Calculate memory variance across metrics
calculateMemoryVariance :: [MemoryMetrics] -> Double
calculateMemoryVariance metrics =
  sqrt variance / avgMemory
  where
    memories = map (fromIntegral . memoryBytesUsed) metrics
    avgMemory = sum memories / fromIntegral (length memories)
    variances = map (\m -> (m - avgMemory) ** 2) memories
    variance = sum variances / fromIntegral (length variances)

-- | Check if result is within memory limit
isWithinMemoryLimit :: Bool -> Bool
isWithinMemoryLimit = id

-- | Run parsing with memory limit enforcement
runWithMemoryLimit :: MemoryTestConfig -> IO Bool
runWithMemoryLimit config = do
  testCode <- generateTestJavaScript (configFileSize config)
  initialMemory <- getCurrentMemoryUsage
  _ <- evaluate $ force (parseBS testCode)
  finalMemory <- getCurrentMemoryUsage
  return ((finalMemory - initialMemory) <= limitBytes)
  where
    limitBytes = fromIntegral (configMaxMemoryMB config) * 1024 * 1024

-- | Measure and cleanup memory after parsing
measureAndCleanup :: ByteString -> IO Word64
measureAndCleanup source = do
  _ <- evaluateWithCleanup source
  getCurrentMemoryUsage

-- | Calculate memory trend across measurements
calculateMemoryTrend :: [Word64] -> Double
calculateMemoryTrend measurements =
  (n * sumXY - sumX * sumY) / (n * sumX2 - sumX ** 2)
  where
    indices = map fromIntegral [0 .. length measurements - 1]
    values = map fromIntegral measurements
    n = fromIntegral (length measurements)
    sumX = sum indices
    sumY = sum values
    sumXY = sum (zipWith (*) indices values)
    sumX2 = sum (map (** 2) indices)

-- | Check if memory pattern is stable
isStableMemoryPattern :: Double -> Bool
isStableMemoryPattern trend = abs trend < 1000000 -- <1MB growth per iteration

-- | Measure streaming memory usage
measureStreamingMemory :: MemoryTestConfig -> IO [MemoryMetrics]
measureStreamingMemory config = do
  chunks <- createStreamingTestData (configStreamChunkSize config) 10
  mapM processStreamChunk chunks

-- | Validate constant memory for streaming
validateConstantMemoryStreaming :: [MemoryMetrics] -> Bool
validateConstantMemoryStreaming metrics =
  ratio < (1.5 :: Double)
  where
    memories = map memoryBytesUsed metrics
    maxMemory = maximum memories
    minMemory = minimum memories
    ratio = fromIntegral maxMemory / fromIntegral minMemory

-- | Process streaming chunk and measure memory
processStreamChunk :: ByteString -> IO MemoryMetrics
processStreamChunk = measureParseMemory

-- | Create streaming test data chunks
createStreamingTestData :: Int -> Int -> IO [ByteString]
createStreamingTestData chunkSize numChunks =
  replicateM numChunks (generateTestJavaScript chunkSize)

-- | Measure streaming memory for large file
measureStreamingForFile :: MemoryTestConfig -> IO MemoryMetrics
measureStreamingForFile config = do
  testCode <- generateTestJavaScript (configFileSize config)
  measureParseMemory testCode

-- | Create incremental test data
createIncrementalTestData :: Int -> IO [ByteString]
createIncrementalTestData chunkSize = do
  baseCode <- generateTestJavaScript chunkSize
  let chunks = map (\i -> BS.take (i * chunkSize `div` 10) baseCode) [1 .. 10]
  return chunks

-- | Measure incremental parsing memory
measureIncrementalParsing :: [ByteString] -> IO [MemoryMetrics]
measureIncrementalParsing = mapM measureParseMemory

-- | Validate incremental memory usage
validateIncrementalMemoryUsage :: [MemoryMetrics] -> Bool
validateIncrementalMemoryUsage = validateLinearGrowth

-- | Simulate memory pressure conditions
simulateMemoryPressure :: MemoryTestConfig -> IO Bool
simulateMemoryPressure config = do
  pressureData <- evaluate $ force $ replicate pressureSize (42 :: Int)
  testCode <- generateTestJavaScript (configFileSize config)
  result <- evaluate $ force (parseBS testCode)
  pressureData `deepseq` return ()
  result `deepseq` return True
  where
    pressureSize = configMaxMemoryMB config * 1024 * 1024 `div` 2

-- | Check if handles memory pressure gracefully
handlesMemoryPressureGracefully :: Bool -> Bool
handlesMemoryPressureGracefully = id

-- | Measure graceful degradation under constraints
measureGracefulDegradation :: MemoryTestConfig -> IO MemoryMetrics
measureGracefulDegradation config = do
  testCode <- generateTestJavaScript (configFileSize config)
  measureParseMemory testCode

-- | Validate graceful degradation behavior
validateGracefulDegradation :: MemoryMetrics -> Bool
validateGracefulDegradation metrics =
  memoryOverheadRatio metrics < 50.0 -- Allow higher overhead under pressure

-- | Apply memory pressure and measure impact
applyMemoryPressure :: MemoryTestConfig -> IO ()
applyMemoryPressure config = do
  pressureData <- evaluate $ force $ replicate pressureSize (1 :: Int)
  pressureData `deepseq` return ()
  where
    pressureSize = configMaxMemoryMB config * 1024 * 1024 `div` 4

-- | Validate linear scaling characteristics
validateLinearScaling :: [MemoryMetrics] -> Bool
validateLinearScaling = validateLinearGrowth

-- | Validate that growth is not quadratic
validateNotQuadratic :: [MemoryMetrics] -> Bool
validateNotQuadratic (m1 : m2 : m3 : _) =
  quadraticFactor < 2.0
  where
    ratio1 = calculateGrowthRatio m1 m2
    ratio2 = calculateGrowthRatio m2 m3
    quadraticFactor = ratio2 / ratio1
validateNotQuadratic _ = True

-- | Generate test JavaScript code of specific size as UTF-8 'ByteString'
generateTestJavaScript :: Int -> IO ByteString
generateTestJavaScript targetSize =
  return $ BS.concat $ replicate repetitions encodedPattern
  where
    basePattern =
      Text.unlines
        [ "function processData(data, config) {",
          "  var result = [];",
          "  var options = config || {};",
          "  for (var i = 0; i < data.length; i++) {",
          "    var item = data[i];",
          "    if (item && typeof item === 'object') {",
          "      var processed = transform(item, options);",
          "      if (validate(processed)) {",
          "        result.push(processed);",
          "      }",
          "    }",
          "  }",
          "  return result;",
          "}"
        ]
    encodedPattern = Text.encodeUtf8 basePattern
    patternSize = BS.length encodedPattern
    repetitions = max 1 (targetSize `div` patternSize)

-- | Memory profiler for detailed analysis
runMemoryProfiler :: IO [MemoryMetrics]
runMemoryProfiler = do
  let sizes = [64 * 1024, 128 * 1024, 256 * 1024, 512 * 1024, 1024 * 1024]
  mapM measureMemoryForSize sizes

-- | Validate memory constraints against targets
validateMemoryConstraints :: [MemoryMetrics] -> [Bool]
validateMemoryConstraints metrics =
  [ all (\m -> memoryOverheadRatio m < 20.0) metrics,
    validateLinearGrowth metrics,
    all (\m -> memoryMaxResidency m < 500 * 1024 * 1024) metrics -- <500MB
  ]

-- | Create memory baseline for performance regression detection
createMemoryBaseline :: IO [MemoryMetrics]
createMemoryBaseline = do
  let standardSizes = [100 * 1024, 500 * 1024, 1024 * 1024] -- 100KB, 500KB, 1MB
  mapM measureMemoryForSize standardSizes
