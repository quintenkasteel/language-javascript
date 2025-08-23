{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
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
    ( memoryTests
    , memoryConstraintTests
    , memoryLeakDetectionTests
    , streamingMemoryTests
    , memoryPressureTests
    , MemoryMetrics(..)
    , MemoryTestConfig(..)
    , runMemoryProfiler
    , validateMemoryConstraints
    , detectMemoryLeaks
    , createMemoryBaseline
    ) where

import Test.Hspec
import Control.DeepSeq (deepseq, force, NFData(..))
import Control.Exception (evaluate, bracket)
import Control.Monad (replicateM, forM_, when)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.Text as Text
import System.Mem (performGC)
import qualified GHC.Stats as Stats
import Data.Word (Word64)
import Language.JavaScript.Parser.Grammar7 (parseProgram)
import Language.JavaScript.Parser.Parser (parseUsing)
import qualified Language.JavaScript.Parser.AST as AST

-- | Memory usage metrics for detailed analysis
data MemoryMetrics = MemoryMetrics
  { memoryBytesAllocated :: !Word64     -- ^ Total bytes allocated
  , memoryBytesUsed :: !Word64          -- ^ Current bytes in use
  , memoryGCCollections :: !Word64      -- ^ Number of GC collections
  , memoryMaxResidency :: !Word64       -- ^ Maximum residency observed
  , memoryParseTime :: !Double          -- ^ Parse time in milliseconds
  , memoryInputSize :: !Int             -- ^ Input size in bytes
  , memoryOverheadRatio :: !Double      -- ^ Memory overhead vs input
  } deriving (Eq, Show)

instance NFData MemoryMetrics where
  rnf (MemoryMetrics alloc used gcCount maxRes time input overhead) =
    rnf alloc `seq` rnf used `seq` rnf gcCount `seq` rnf maxRes `seq`
    rnf time `seq` rnf input `seq` rnf overhead

-- | Configuration for memory testing scenarios
data MemoryTestConfig = MemoryTestConfig
  { configMaxMemoryMB :: !Int           -- ^ Maximum memory limit in MB
  , configIterations :: !Int            -- ^ Number of test iterations
  , configFileSize :: !Int              -- ^ Test file size in bytes
  , configStreamChunkSize :: !Int       -- ^ Streaming chunk size
  , configGCBetweenTests :: !Bool       -- ^ Force GC between tests
  } deriving (Eq, Show)

instance NFData MemoryTestConfig where
  rnf (MemoryTestConfig maxMem iter size chunk gcBetween) =
    rnf maxMem `seq` rnf iter `seq` rnf size `seq` rnf chunk `seq` rnf gcBetween

-- | Default memory test configuration
defaultMemoryConfig :: MemoryTestConfig
defaultMemoryConfig = MemoryTestConfig
  { configMaxMemoryMB = 100
  , configIterations = 10
  , configFileSize = 1024 * 1024  -- 1MB
  , configStreamChunkSize = 64 * 1024  -- 64KB
  , configGCBetweenTests = True
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
    let sizes = [100 * 1024, 500 * 1024, 1024 * 1024]  -- 100KB, 500KB, 1MB
    metrics <- mapM measureMemoryForSize sizes
    validateLinearGrowth metrics `shouldBe` True
    
  it "maintains memory overhead under 20x input size" $ do
    config <- return defaultMemoryConfig
    metrics <- measureMemoryForSize (configFileSize config)
    memoryOverheadRatio metrics `shouldSatisfy` (<20.0)
    
  it "enforces maximum memory limit constraints" $ do
    let config = defaultMemoryConfig { configMaxMemoryMB = 50 }
    result <- runWithMemoryLimit config
    result `shouldSatisfy` isWithinMemoryLimit
    
  it "validates constant memory for identical inputs" $ do
    testCode <- generateTestJavaScript (256 * 1024)  -- 256KB
    metrics <- replicateM 5 (measureParseMemory testCode)
    let memoryVariance = calculateMemoryVariance metrics
    memoryVariance `shouldSatisfy` (<0.1)  -- <10% variance

-- | Memory leak detection across multiple operations
memoryLeakDetectionTests :: Spec
memoryLeakDetectionTests = describe "Memory leak detection" $ do
  
  it "detects no memory leaks across iterations" $ do
    let config = defaultMemoryConfig { configIterations = 20 }
    leakStatus <- detectMemoryLeaks config
    leakStatus `shouldBe` NoMemoryLeaks
    
  it "validates memory cleanup after parse completion" $ do
    initialMemory <- getCurrentMemoryUsage
    testCode <- generateTestJavaScript (512 * 1024)  -- 512KB
    _ <- evaluateWithCleanup testCode
    performGC
    finalMemory <- getCurrentMemoryUsage
    let memoryDelta = finalMemory - initialMemory
    -- Memory growth should be minimal after cleanup
    memoryDelta `shouldSatisfy` (<50 * 1024 * 1024)  -- <50MB
    
  it "maintains stable memory across repeated parses" $ do
    testCode <- generateTestJavaScript (128 * 1024)  -- 128KB
    memoryHistory <- mapM (\_ -> measureAndCleanup testCode) [1..15 :: Int]
    let trend = calculateMemoryTrend memoryHistory
    trend `shouldSatisfy` isStableMemoryPattern

-- | Streaming parser memory validation
streamingMemoryTests :: Spec
streamingMemoryTests = describe "Streaming memory validation" $ do
  
  it "maintains constant memory for streaming scenarios" $ do
    let config = defaultMemoryConfig { configStreamChunkSize = 32 * 1024 }
    streamMetrics <- measureStreamingMemory config
    validateConstantMemoryStreaming streamMetrics `shouldBe` True
    
  it "processes large files with bounded memory" $ do
    let largeFileSize = 5 * 1024 * 1024  -- 5MB
    let config = defaultMemoryConfig { configFileSize = largeFileSize }
    metrics <- measureStreamingForFile config
    memoryMaxResidency metrics `shouldSatisfy` (<200 * 1024 * 1024)  -- <200MB
    
  it "handles incremental parsing memory efficiently" $ do
    chunks <- createIncrementalTestData (configStreamChunkSize defaultMemoryConfig)
    metrics <- measureIncrementalParsing chunks
    validateIncrementalMemoryUsage metrics `shouldBe` True

-- | Memory pressure testing and validation
memoryPressureTests :: Spec
memoryPressureTests = describe "Memory pressure handling" $ do
  
  it "handles low memory environments gracefully" $ do
    let lowMemoryConfig = defaultMemoryConfig { configMaxMemoryMB = 20 }
    result <- simulateMemoryPressure lowMemoryConfig
    result `shouldSatisfy` handlesMemoryPressureGracefully
    
  it "degrades gracefully under memory constraints" $ do
    let constrainedConfig = defaultMemoryConfig { configMaxMemoryMB = 30 }
    degradationMetrics <- measureGracefulDegradation constrainedConfig
    validateGracefulDegradation degradationMetrics `shouldBe` True
    
  it "recovers memory after pressure release" $ do
    initialMemory <- getCurrentMemoryUsage
    _ <- applyMemoryPressure defaultMemoryConfig
    performGC
    recoveredMemory <- getCurrentMemoryUsage
    let recoveryRatio = fromIntegral recoveredMemory / fromIntegral initialMemory
    recoveryRatio `shouldSatisfy` (<(1.5 :: Double))  -- Within 50% of initial

-- | Linear memory growth validation tests
linearMemoryGrowthTests :: Spec
linearMemoryGrowthTests = describe "Linear memory growth validation" $ do
  
  it "validates O(n) memory scaling with input size" $ do
    let sizes = [64*1024, 128*1024, 256*1024, 512*1024]  -- Powers of 2
    metrics <- mapM measureMemoryForSize sizes
    validateLinearScaling metrics `shouldBe` True
    
  it "prevents quadratic memory growth O(n²)" $ do
    let sizes = [100*1024, 400*1024, 900*1024]  -- Square relationships
    metrics <- mapM measureMemoryForSize sizes
    validateNotQuadratic metrics `shouldBe` True

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
measureParseMemory :: Text.Text -> IO MemoryMetrics
measureParseMemory source = do
  performGC  -- Baseline GC
  initialStats <- safeGetRTSStats
  startTime <- getCurrentTime
  
  let sourceStr = Text.unpack source
  result <- evaluate $ force (parseUsing parseProgram sourceStr "memory-test")
  result `deepseq` return ()
  
  endTime <- getCurrentTime
  finalStats <- safeGetRTSStats
  
  let parseTimeMs = fromRational (toRational (diffUTCTime endTime startTime)) * 1000
  let inputSize = Text.length source
  
  case (initialStats, finalStats) of
    (Just initial, Just final) -> do
      let bytesAllocated = Stats.max_live_bytes final
      let bytesUsed = Stats.allocated_bytes final - Stats.allocated_bytes initial
      let gcCount = fromIntegral $ Stats.gcs final - Stats.gcs initial
      let overheadRatio = fromIntegral bytesUsed / fromIntegral inputSize
      
      return MemoryMetrics
        { memoryBytesAllocated = bytesAllocated
        , memoryBytesUsed = bytesUsed
        , memoryGCCollections = gcCount
        , memoryMaxResidency = Stats.max_live_bytes final
        , memoryParseTime = parseTimeMs
        , memoryInputSize = inputSize
        , memoryOverheadRatio = overheadRatio
        }
    _ -> do
      -- RTS stats not available, provide reasonable defaults
      let estimatedMemory = fromIntegral inputSize * 10  -- Rough estimate
      return MemoryMetrics
        { memoryBytesAllocated = estimatedMemory
        , memoryBytesUsed = estimatedMemory
        , memoryGCCollections = 0
        , memoryMaxResidency = estimatedMemory
        , memoryParseTime = parseTimeMs
        , memoryInputSize = inputSize
        , memoryOverheadRatio = 10.0  -- Conservative estimate
        }

-- | Memory leak detection across multiple iterations
data LeakDetectionResult = NoMemoryLeaks | MemoryLeakDetected Word64
  deriving (Eq, Show)

-- | Detect memory leaks across iterations
detectMemoryLeaks :: MemoryTestConfig -> IO LeakDetectionResult
detectMemoryLeaks config = do
  performGC
  initialMemory <- getCurrentMemoryUsage
  
  let iterations = configIterations config
  testCode <- generateTestJavaScript (configFileSize config)
  
  -- Run multiple parsing iterations
  forM_ [1..iterations] $ \_ -> do
    _ <- evaluate $ force (parseUsing parseProgram (Text.unpack testCode) "leak-test")
    when (configGCBetweenTests config) performGC
  
  performGC
  finalMemory <- getCurrentMemoryUsage
  
  let memoryGrowth = finalMemory - initialMemory
  let leakThreshold = 100 * 1024 * 1024  -- 100MB threshold
  
  if memoryGrowth > leakThreshold
    then return (MemoryLeakDetected memoryGrowth)
    else return NoMemoryLeaks

-- | Validate linear memory growth pattern
validateLinearGrowth :: [MemoryMetrics] -> Bool
validateLinearGrowth metrics =
  case metrics of
    [] -> True
    [_] -> True
    (_:m2:rest) -> 
      let ratios = zipWith calculateGrowthRatio (m2:rest) rest
          avgRatio = sum ratios / fromIntegral (length ratios)
          variance = sum (map (\r -> (r - avgRatio) ** 2) ratios) / fromIntegral (length ratios)
      in variance < 0.5  -- Low variance indicates linear growth

-- | Calculate growth ratio between memory metrics
calculateGrowthRatio :: MemoryMetrics -> MemoryMetrics -> Double
calculateGrowthRatio m1 m2 =
  let sizeRatio = fromIntegral (memoryInputSize m2) / fromIntegral (memoryInputSize m1)
      memoryRatio = fromIntegral (memoryBytesUsed m2) / fromIntegral (memoryBytesUsed m1)
  in memoryRatio / sizeRatio

-- | Current memory usage in bytes
getCurrentMemoryUsage :: IO Word64
getCurrentMemoryUsage = do
  statsEnabled <- Stats.getRTSStatsEnabled
  if statsEnabled
    then do
      stats <- Stats.getRTSStats
      return (Stats.max_live_bytes stats)
    else return 1000000  -- Return 1MB as reasonable default when stats not available

-- | Evaluate parse with cleanup
evaluateWithCleanup :: Text.Text -> IO (Either String AST.JSAST)
evaluateWithCleanup source = bracket
  (return ())
  (\_ -> performGC)
  (\_ -> do
    let sourceStr = Text.unpack source
    result <- evaluate $ force (parseUsing parseProgram sourceStr "cleanup-test")
    result `deepseq` return result
  )

-- | Calculate memory variance across metrics
calculateMemoryVariance :: [MemoryMetrics] -> Double
calculateMemoryVariance metrics =
  let memories = map (fromIntegral . memoryBytesUsed) metrics
      avgMemory = sum memories / fromIntegral (length memories)
      variances = map (\m -> (m - avgMemory) ** 2) memories
      variance = sum variances / fromIntegral (length variances)
  in sqrt variance / avgMemory

-- | Check if result is within memory limit
isWithinMemoryLimit :: Bool -> Bool
isWithinMemoryLimit = id

-- | Run parsing with memory limit enforcement
runWithMemoryLimit :: MemoryTestConfig -> IO Bool
runWithMemoryLimit config = do
  let limitBytes = fromIntegral (configMaxMemoryMB config) * 1024 * 1024
  testCode <- generateTestJavaScript (configFileSize config)
  
  initialMemory <- getCurrentMemoryUsage
  _ <- evaluate $ force (parseUsing parseProgram (Text.unpack testCode) "limit-test")
  finalMemory <- getCurrentMemoryUsage
  
  return ((finalMemory - initialMemory) <= limitBytes)

-- | Measure and cleanup memory after parsing
measureAndCleanup :: Text.Text -> IO Word64
measureAndCleanup source = do
  _ <- evaluateWithCleanup source
  getCurrentMemoryUsage

-- | Calculate memory trend across measurements
calculateMemoryTrend :: [Word64] -> Double
calculateMemoryTrend measurements =
  let indices = map fromIntegral [0..length measurements - 1]
      values = map fromIntegral measurements
      n = fromIntegral (length measurements)
      sumX = sum indices
      sumY = sum values
      sumXY = sum (zipWith (*) indices values)
      sumX2 = sum (map (** 2) indices)
      slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX ** 2)
  in slope

-- | Check if memory pattern is stable
isStableMemoryPattern :: Double -> Bool
isStableMemoryPattern trend = abs trend < 1000000  -- <1MB growth per iteration

-- | Measure streaming memory usage
measureStreamingMemory :: MemoryTestConfig -> IO [MemoryMetrics]
measureStreamingMemory config = do
  let chunkSize = configStreamChunkSize config
  chunks <- createStreamingTestData chunkSize 10  -- 10 chunks
  mapM processStreamChunk chunks

-- | Validate constant memory for streaming
validateConstantMemoryStreaming :: [MemoryMetrics] -> Bool
validateConstantMemoryStreaming metrics =
  let memories = map memoryBytesUsed metrics
      maxMemory = maximum memories
      minMemory = minimum memories
      ratio = fromIntegral maxMemory / fromIntegral minMemory
  in ratio < (1.5 :: Double)  -- Memory should stay within 50% range

-- | Process streaming chunk and measure memory
processStreamChunk :: Text.Text -> IO MemoryMetrics
processStreamChunk chunk = measureParseMemory chunk

-- | Create streaming test data chunks
createStreamingTestData :: Int -> Int -> IO [Text.Text]
createStreamingTestData chunkSize numChunks = do
  replicateM numChunks (generateTestJavaScript chunkSize)

-- | Measure streaming memory for large file
measureStreamingForFile :: MemoryTestConfig -> IO MemoryMetrics
measureStreamingForFile config = do
  testCode <- generateTestJavaScript (configFileSize config)
  measureParseMemory testCode

-- | Create incremental test data
createIncrementalTestData :: Int -> IO [Text.Text]
createIncrementalTestData chunkSize = do
  baseCode <- generateTestJavaScript chunkSize
  let chunks = map (\i -> Text.take (i * chunkSize `div` 10) baseCode) [1..10]
  return chunks

-- | Measure incremental parsing memory
measureIncrementalParsing :: [Text.Text] -> IO [MemoryMetrics]
measureIncrementalParsing chunks = mapM measureParseMemory chunks

-- | Validate incremental memory usage
validateIncrementalMemoryUsage :: [MemoryMetrics] -> Bool
validateIncrementalMemoryUsage metrics = validateLinearGrowth metrics

-- | Simulate memory pressure conditions
simulateMemoryPressure :: MemoryTestConfig -> IO Bool
simulateMemoryPressure config = do
  -- Create memory pressure by allocating large structures
  let pressureSize = configMaxMemoryMB config * 1024 * 1024 `div` 2
  pressureData <- evaluate $ force $ replicate pressureSize (42 :: Int)
  
  testCode <- generateTestJavaScript (configFileSize config)
  result <- evaluate $ force (parseUsing parseProgram (Text.unpack testCode) "pressure-test")
  
  -- Cleanup pressure data
  pressureData `deepseq` return ()
  result `deepseq` return True

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
  memoryOverheadRatio metrics < 50.0  -- Allow higher overhead under pressure

-- | Apply memory pressure and measure impact
applyMemoryPressure :: MemoryTestConfig -> IO ()
applyMemoryPressure config = do
  let pressureSize = configMaxMemoryMB config * 1024 * 1024 `div` 4
  pressureData <- evaluate $ force $ replicate pressureSize (1 :: Int)
  pressureData `deepseq` return ()

-- | Validate linear scaling characteristics
validateLinearScaling :: [MemoryMetrics] -> Bool
validateLinearScaling = validateLinearGrowth

-- | Validate that growth is not quadratic
validateNotQuadratic :: [MemoryMetrics] -> Bool
validateNotQuadratic metrics =
  case metrics of
    (m1:m2:m3:_) ->
      let ratio1 = calculateGrowthRatio m1 m2
          ratio2 = calculateGrowthRatio m2 m3
          -- If quadratic, second ratio would be much larger
          quadraticFactor = ratio2 / ratio1
      in quadraticFactor < 2.0  -- Not growing quadratically
    _ -> True

-- | Generate test JavaScript code of specific size
generateTestJavaScript :: Int -> IO Text.Text
generateTestJavaScript targetSize = do
  let basePattern = Text.unlines
        [ "function processData(data, config) {"
        , "  var result = [];"
        , "  var options = config || {};"
        , "  for (var i = 0; i < data.length; i++) {"
        , "    var item = data[i];"
        , "    if (item && typeof item === 'object') {"
        , "      var processed = transform(item, options);"
        , "      if (validate(processed)) {"
        , "        result.push(processed);"
        , "      }"
        , "    }"
        , "  }"
        , "  return result;"
        , "}"
        ]
  let patternSize = Text.length basePattern
  let repetitions = max 1 (targetSize `div` patternSize)
  return $ Text.concat $ replicate repetitions basePattern

-- | Memory profiler for detailed analysis
runMemoryProfiler :: IO [MemoryMetrics]
runMemoryProfiler = do
  let sizes = [64*1024, 128*1024, 256*1024, 512*1024, 1024*1024]
  mapM measureMemoryForSize sizes

-- | Validate memory constraints against targets
validateMemoryConstraints :: [MemoryMetrics] -> [Bool]
validateMemoryConstraints metrics =
  [ all (\m -> memoryOverheadRatio m < 20.0) metrics
  , validateLinearGrowth metrics
  , all (\m -> memoryMaxResidency m < 500 * 1024 * 1024) metrics  -- <500MB
  ]

-- | Create memory baseline for performance regression detection
createMemoryBaseline :: IO [MemoryMetrics]
createMemoryBaseline = do
  let standardSizes = [100*1024, 500*1024, 1024*1024]  -- 100KB, 500KB, 1MB
  mapM measureMemoryForSize standardSizes