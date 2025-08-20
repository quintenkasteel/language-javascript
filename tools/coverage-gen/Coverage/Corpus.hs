{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Real-world JavaScript corpus analysis for test generation.
--
-- This module analyzes large collections of real-world JavaScript
-- code to extract patterns, features, and structures that can be
-- used to generate realistic and comprehensive test cases.
--
-- ==== Examples
--
-- >>> corpus <- loadCorpus "corpus/real-world/"
-- >>> patterns <- extractPatterns corpus
-- >>> tests <- generateFromCorpus patterns gaps
-- >>> length tests
-- 156
--
-- @since 1.0.0
module Coverage.Corpus
  ( JavaScriptCorpus(..)
  , CorpusEntry(..)
  , CodePattern(..)
  , FeatureExtractor(..)
  , loadCorpus
  , extractPatterns
  , generateFromCorpus
  , analyzeCorpusFeatures
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import System.Directory (listDirectory, doesFileExist)
import qualified System.Directory as Directory
import Control.Monad (filterM, foldM)
import Data.List (sortBy)
import qualified Data.List as List

import Coverage.Analysis
  ( CoverageGap(..)
  , GapType(..)
  )
import Coverage.Generation
  ( TestCase(..)
  , TestExpectation(..)
  )

-- | Collection of real-world JavaScript code samples.
data JavaScriptCorpus = JavaScriptCorpus
  { _corpusEntries :: ![CorpusEntry]
  , _corpusMetadata :: !CorpusMetadata
  , _corpusPatterns :: !(Map Text [CodePattern])
  , _corpusFeatures :: !FeatureSet
  } deriving (Eq, Show)

-- | Individual JavaScript file in the corpus.
data CorpusEntry = CorpusEntry
  { _entryPath :: !FilePath
  , _entryContent :: !Text
  , _entryMetadata :: !EntryMetadata
  , _entryFeatures :: ![Text]
  } deriving (Eq, Show)

-- | Metadata for the entire corpus.
data CorpusMetadata = CorpusMetadata
  { _metaTotalFiles :: !Int
  , _metaTotalLines :: !Int
  , _metaLanguageFeatures :: !(Set Text)
  , _metaLibraries :: !(Set Text)
  } deriving (Eq, Show)

-- | Metadata for individual corpus entry.
data EntryMetadata = EntryMetadata
  { _entryLineCount :: !Int
  , _entryComplexity :: !Double
  , _entryLanguageLevel :: !LanguageLevel
  , _entryCategory :: !CodeCategory
  } deriving (Eq, Show)

-- | JavaScript language level/version.
data LanguageLevel
  = ES3
  | ES5
  | ES6
  | ES2017
  | ES2018
  | ES2019
  | ES2020
  | ES2021
  | ES2022
  deriving (Eq, Show, Ord, Enum)

-- | Category of JavaScript code.
data CodeCategory
  = Frontend
  | Backend
  | Library
  | Framework
  | Application
  | Test
  | Configuration
  | Other
  deriving (Eq, Show)

-- | Code pattern extracted from corpus.
data CodePattern = CodePattern
  { _patternType :: !PatternType
  , _patternFrequency :: !Int
  , _patternExample :: !Text
  , _patternFeatures :: ![Text]
  } deriving (Eq, Show)

-- | Type of code pattern.
data PatternType
  = SyntaxPattern !Text
  | StructuralPattern !Text
  | SemanticPattern !Text
  | ErrorPattern !Text
  deriving (Eq, Show)

-- | Feature extraction engine.
data FeatureExtractor = FeatureExtractor
  { _extractorRules :: ![ExtractionRule]
  , _extractorConfig :: !ExtractionConfig
  , _extractorStats :: !ExtractionStats
  } deriving (Eq, Show)

-- | Rule for feature extraction.
data ExtractionRule = ExtractionRule
  { _ruleName :: !Text
  , _rulePattern :: !Text
  , _ruleExtractor :: Text -> [Text]
  , _ruleWeight :: !Double
  }

instance Eq ExtractionRule where
  r1 == r2 = _ruleName r1 == _ruleName r2

instance Show ExtractionRule where
  show rule = "ExtractionRule " ++ Text.unpack (_ruleName rule)

-- | Configuration for feature extraction.
data ExtractionConfig = ExtractionConfig
  { _configMinPatternFreq :: !Int
  , _configMaxPatternLen :: !Int
  , _configFeatureThreshold :: !Double
  , _configContextWindow :: !Int
  } deriving (Eq, Show)

-- | Statistics for feature extraction.
data ExtractionStats = ExtractionStats
  { _statsExtracted :: !Int
  , _statsFiltered :: !Int
  , _statsUnique :: !Int
  , _statsProcessingTime :: !Double
  } deriving (Eq, Show)

-- | Set of extracted features.
data FeatureSet = FeatureSet
  { _featureSyntactic :: !(Map Text Int)
  , _featureStructural :: !(Map Text Int)
  , _featureSemantic :: !(Map Text Int)
  , _featureComplexity :: !(Map Text Double)
  } deriving (Eq, Show)

-- | Load JavaScript corpus from directory.
--
-- Recursively scans the specified directory for JavaScript files
-- and loads them into a corpus for analysis and pattern extraction.
loadCorpus :: FilePath -> IO (Either Text JavaScriptCorpus)
loadCorpus corpusDir = do
  exists <- Directory.doesDirectoryExist corpusDir
  if not exists
    then pure (Left ("Corpus directory not found: " <> Text.pack corpusDir))
    else do
      files <- findJavaScriptFiles corpusDir
      entries <- mapM loadCorpusEntry files
      let validEntries = [e | Right e <- entries]
      
      if null validEntries
        then pure (Left "No valid JavaScript files found in corpus")
        else do
          let metadata = calculateMetadata validEntries
          let patterns = Map.empty  -- Will be populated by extractPatterns
          let features = FeatureSet Map.empty Map.empty Map.empty Map.empty
          pure (Right (JavaScriptCorpus validEntries metadata patterns features))

-- | Find all JavaScript files in directory tree.
findJavaScriptFiles :: FilePath -> IO [FilePath]
findJavaScriptFiles dir = do
  contents <- listDirectory dir
  let fullPaths = map (dir </>) contents
  files <- filterM Directory.doesFileExist fullPaths
  dirs <- filterM Directory.doesDirectoryExist fullPaths
  
  let jsFiles = filter isJavaScriptFile files
  subFiles <- concat <$> mapM findJavaScriptFiles dirs
  pure (jsFiles ++ subFiles)
  where
    isJavaScriptFile path = 
      FilePath.takeExtension path `elem` [".js", ".mjs", ".jsx"]

-- | Load individual corpus entry.
loadCorpusEntry :: FilePath -> IO (Either Text CorpusEntry)
loadCorpusEntry path = do
  exists <- doesFileExist path
  if not exists
    then pure (Left ("File not found: " <> Text.pack path))
    else do
      content <- Text.readFile path
      metadata <- analyzeEntryMetadata content
      features <- extractBasicFeatures content
      pure (Right (CorpusEntry path content metadata features))

-- | Analyze metadata for corpus entry.
analyzeEntryMetadata :: Text -> IO EntryMetadata
analyzeEntryMetadata content = do
  let lineCount = length (Text.lines content)
  let complexity = calculateComplexity content
  let langLevel = detectLanguageLevel content
  let category = classifyCode content
  pure (EntryMetadata lineCount complexity langLevel category)

-- | Calculate code complexity metric.
calculateComplexity :: Text -> Double
calculateComplexity content =
  cyclomaticComplexity + nestingComplexity + lengthComplexity
  where
    lines = Text.lines content
    lineCount = fromIntegral (length lines)
    
    cyclomaticComplexity = fromIntegral (countKeywords content keywords) / lineCount
    nestingComplexity = fromIntegral (maxNesting content) / 10.0
    lengthComplexity = min 1.0 (lineCount / 1000.0)
    
    keywords = ["if", "else", "while", "for", "switch", "case", "try", "catch"]

-- | Count keyword occurrences.
countKeywords :: Text -> [Text] -> Int
countKeywords content keywords =
  sum (map (countOccurrences content) keywords)
  where
    countOccurrences text keyword =
      length (Text.splitOn keyword text) - 1

-- | Calculate maximum nesting depth.
maxNesting :: Text -> Int
maxNesting content =
  maximum (0 : map (calculateNesting 0 0) (Text.lines content))
  where
    calculateNesting current maxSeen line =
      let opens = Text.count "{" line
      let closes = Text.count "}" line
      let newCurrent = current + opens - closes
      let newMax = max maxSeen newCurrent
      in newMax

-- | Detect JavaScript language level.
detectLanguageLevel :: Text -> LanguageLevel
detectLanguageLevel content
  | hasFeatures es2022Features = ES2022
  | hasFeatures es2021Features = ES2021  
  | hasFeatures es2020Features = ES2020
  | hasFeatures es2019Features = ES2019
  | hasFeatures es2018Features = ES2018
  | hasFeatures es2017Features = ES2017
  | hasFeatures es6Features = ES6
  | hasFeatures es5Features = ES5
  | otherwise = ES3
  where
    hasFeatures features = any (`Text.isInfixOf` content) features
    
    es2022Features = ["class static", "private #"]
    es2021Features = ["??=", "||=", "&&="]
    es2020Features = ["optional chaining", "nullish coalescing", "BigInt"]
    es2019Features = ["Array.flat", "Object.fromEntries"]
    es2018Features = ["async", "await", "..."]
    es2017Features = ["async function", "Object.values"]
    es6Features = ["=>", "let", "const", "class", "import", "export"]
    es5Features = ["JSON.stringify", "Object.keys", "Array.forEach"]

-- | Classify code category.
classifyCode :: Text -> CodeCategory
classifyCode content
  | Text.isInfixOf "describe(" content || Text.isInfixOf "it(" content = Test
  | Text.isInfixOf "React" content || Text.isInfixOf "jsx" content = Frontend
  | Text.isInfixOf "express" content || Text.isInfixOf "require(" content = Backend
  | Text.isInfixOf "module.exports" content = Library
  | Text.isInfixOf "angular" content || Text.isInfixOf "vue" content = Framework
  | Text.isInfixOf "config" content = Configuration
  | otherwise = Application

-- | Extract basic features from code.
extractBasicFeatures :: Text -> IO [Text]
extractBasicFeatures content = 
  pure (syntacticFeatures ++ structuralFeatures ++ semanticFeatures)
  where
    syntacticFeatures = extractSyntacticFeatures content
    structuralFeatures = extractStructuralFeatures content
    semanticFeatures = extractSemanticFeatures content

-- | Extract syntactic features.
extractSyntacticFeatures :: Text -> [Text]
extractSyntacticFeatures content =
  filter (not . Text.null) $
    [ if "function" `Text.isInfixOf` content then "has-functions" else ""
    , if "=>" `Text.isInfixOf` content then "has-arrow-functions" else ""
    , if "class" `Text.isInfixOf` content then "has-classes" else ""
    , if "async" `Text.isInfixOf` content then "has-async" else ""
    , if "import" `Text.isInfixOf` content then "has-imports" else ""
    ]

-- | Extract structural features.
extractStructuralFeatures :: Text -> [Text]
extractStructuralFeatures content =
  filter (not . Text.null) $
    [ if maxNesting content > 5 then "deeply-nested" else ""
    , if length (Text.lines content) > 100 then "large-file" else ""
    , if Text.count "function" content > 10 then "many-functions" else ""
    ]

-- | Extract semantic features.
extractSemanticFeatures :: Text -> [Text]
extractSemanticFeatures content =
  filter (not . Text.null) $
    [ if "error" `Text.isInfixOf` content then "error-handling" else ""
    , if "test" `Text.isInfixOf` content then "testing-code" else ""
    , if "api" `Text.isInfixOf` content then "api-usage" else ""
    ]

-- | Calculate metadata for entire corpus.
calculateMetadata :: [CorpusEntry] -> CorpusMetadata
calculateMetadata entries = CorpusMetadata
  { _metaTotalFiles = length entries
  , _metaTotalLines = sum (map (_entryLineCount . _entryMetadata) entries)
  , _metaLanguageFeatures = Set.fromList (concatMap _entryFeatures entries)
  , _metaLibraries = Set.empty  -- Could be extracted from imports
  }

-- | Extract patterns from corpus.
--
-- Analyzes the corpus to identify recurring code patterns
-- that can be used for intelligent test case generation.
extractPatterns :: JavaScriptCorpus -> IO (Map Text [CodePattern])
extractPatterns corpus = do
  extractor <- createFeatureExtractor
  patterns <- foldM (processEntry extractor) Map.empty (_corpusEntries corpus)
  pure (filterPatterns patterns)
  where
    processEntry extractor acc entry = do
      entryPatterns <- extractEntryPatterns extractor entry
      pure (mergePatterns acc entryPatterns)

-- | Create feature extractor with default rules.
createFeatureExtractor :: IO FeatureExtractor
createFeatureExtractor = 
  pure (FeatureExtractor extractionRules defaultConfig initialStats)
  where
    extractionRules = 
      [ createRule "function-declarations" "function\\s+\\w+" extractFunctions
      , createRule "variable-declarations" "(let|const|var)\\s+\\w+" extractVariables
      , createRule "control-structures" "(if|while|for|switch)" extractControlFlow
      , createRule "error-patterns" "(try|catch|throw)" extractErrorHandling
      ]
    
    createRule name pattern extractor = ExtractionRule name pattern extractor 1.0
    defaultConfig = ExtractionConfig 2 100 0.1 5
    initialStats = ExtractionStats 0 0 0 0.0

-- | Extract function declarations.
extractFunctions :: Text -> [Text]
extractFunctions content =
  map ("function:" <>) (extractMatches "function\\s+(\\w+)" content)

-- | Extract variable declarations.
extractVariables :: Text -> [Text]
extractVariables content =
  map ("variable:" <>) (extractMatches "(let|const|var)\\s+(\\w+)" content)

-- | Extract control flow structures.
extractControlFlow :: Text -> [Text]
extractControlFlow content =
  map ("control:" <>) (extractMatches "(if|while|for|switch)" content)

-- | Extract error handling patterns.
extractErrorHandling :: Text -> [Text]
extractErrorHandling content =
  map ("error:" <>) (extractMatches "(try|catch|throw)" content)

-- | Extract regex matches (simplified implementation).
extractMatches :: Text -> Text -> [Text]
extractMatches _pattern content =
  -- Simplified - would use regex library in real implementation
  filter (not . Text.null) (Text.words content)

-- | Extract patterns from single corpus entry.
extractEntryPatterns :: FeatureExtractor -> CorpusEntry -> IO (Map Text [CodePattern])
extractEntryPatterns extractor entry = do
  let content = _entryContent entry
  let rules = _extractorRules extractor
  patterns <- mapM (applyRule content) rules
  pure (Map.fromListWith (++) (concat patterns))
  where
    applyRule content rule = do
      let features = (_ruleExtractor rule) content
      let patterns = map (createPattern rule) features
      pure [(_ruleName rule, patterns)]
    
    createPattern rule feature = CodePattern
      { _patternType = SyntaxPattern (_ruleName rule)
      , _patternFrequency = 1
      , _patternExample = feature
      , _patternFeatures = [feature]
      }

-- | Merge pattern maps.
mergePatterns :: Map Text [CodePattern] -> Map Text [CodePattern] -> Map Text [CodePattern]
mergePatterns = Map.unionWith (++)

-- | Filter patterns by frequency and relevance.
filterPatterns :: Map Text [CodePattern] -> Map Text [CodePattern]
filterPatterns patterns = 
  Map.map (filter isRelevantPattern) patterns
  where
    isRelevantPattern pattern = _patternFrequency pattern >= 2

-- | Generate test cases from corpus patterns.
--
-- Uses extracted patterns to generate realistic test cases
-- that target specific coverage gaps while maintaining
-- real-world JavaScript characteristics.
generateFromCorpus :: Map Text [CodePattern] -> [CoverageGap] -> IO [TestCase]
generateFromCorpus patterns gaps = do
  mapM (generateTestFromPattern patterns) gaps

-- | Generate test case from pattern for specific gap.
generateTestFromPattern :: Map Text [CodePattern] -> CoverageGap -> IO TestCase
generateTestFromPattern patterns gap = do
  relevantPatterns <- selectRelevantPatterns patterns gap
  testInput <- synthesizeTest relevantPatterns gap
  pure (TestCase testInput ShouldParse [gap] 0.8)

-- | Select patterns relevant to coverage gap.
selectRelevantPatterns :: Map Text [CodePattern] -> CoverageGap -> IO [CodePattern]
selectRelevantPatterns patterns gap = 
  case _gapType gap of
    UncoveredLine _ -> pure (getPatterns "function-declarations")
    UncoveredBranch _ -> pure (getPatterns "control-structures")
    UncoveredExpression _ -> pure (getPatterns "variable-declarations")
    UntestedPath _ -> pure (getPatterns "error-patterns")
  where
    getPatterns key = concat (Map.lookup key patterns)

-- | Synthesize test from patterns.
synthesizeTest :: [CodePattern] -> CoverageGap -> IO Text
synthesizeTest patterns _gap = 
  if null patterns
    then pure "var x = 42;"
    else do
      pattern <- selectRandomPattern patterns
      pure (_patternExample pattern)

-- | Select random pattern from list.
selectRandomPattern :: [CodePattern] -> IO CodePattern
selectRandomPattern patterns = 
  pure (head patterns)  -- Simplified - would use random selection

-- | Analyze corpus features for insights.
--
-- Performs statistical analysis of the corpus to identify
-- trends, feature distributions, and optimization opportunities.
analyzeCorpusFeatures :: JavaScriptCorpus -> IO FeatureSet
analyzeCorpusFeatures corpus = do
  let entries = _corpusEntries corpus
  syntactic <- analyzeSyntacticFeatures entries
  structural <- analyzeStructuralFeatures entries
  semantic <- analyzeSemanticFeatures entries
  complexity <- analyzeComplexityFeatures entries
  
  pure (FeatureSet syntactic structural semantic complexity)

-- | Analyze syntactic features across corpus.
analyzeSyntacticFeatures :: [CorpusEntry] -> IO (Map Text Int)
analyzeSyntacticFeatures entries = 
  pure (countFeatures (concatMap _entryFeatures entries) syntacticKeywords)
  where
    syntacticKeywords = ["has-functions", "has-arrow-functions", "has-classes"]

-- | Analyze structural features across corpus.
analyzeStructuralFeatures :: [CorpusEntry] -> IO (Map Text Int)
analyzeStructuralFeatures entries = 
  pure (countFeatures (concatMap _entryFeatures entries) structuralKeywords)
  where
    structuralKeywords = ["deeply-nested", "large-file", "many-functions"]

-- | Analyze semantic features across corpus.
analyzeSemanticFeatures :: [CorpusEntry] -> IO (Map Text Int)
analyzeSemanticFeatures entries = 
  pure (countFeatures (concatMap _entryFeatures entries) semanticKeywords)
  where
    semanticKeywords = ["error-handling", "testing-code", "api-usage"]

-- | Analyze complexity features across corpus.
analyzeComplexityFeatures :: [CorpusEntry] -> IO (Map Text Double)
analyzeComplexityFeatures entries = do
  let complexities = map (_entryComplexity . _entryMetadata) entries
  pure (Map.fromList 
    [ ("avg-complexity", average complexities)
    , ("max-complexity", maximum complexities)
    , ("min-complexity", minimum complexities)
    ])
  where
    average xs = sum xs / fromIntegral (length xs)

-- | Count feature occurrences.
countFeatures :: [Text] -> [Text] -> Map Text Int
countFeatures features keywords = 
  Map.fromList [(k, countFeature features k) | k <- keywords]
  where
    countFeature fs keyword = length (filter (== keyword) fs)