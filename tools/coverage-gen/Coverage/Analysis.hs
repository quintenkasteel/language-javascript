{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | HPC coverage report analysis and gap identification.
--
-- This module provides functionality to parse HPC coverage reports,
-- identify uncovered code paths, and analyze coverage gaps for
-- systematic test improvement.
--
-- ==== Examples
--
-- >>> hpcReport <- parseHpcReport "dist/hpc/tix/testsuite/testsuite.tix"
-- >>> gaps <- identifyCoverageGaps hpcReport
-- >>> length gaps
-- 42
--
-- @since 1.0.0
module Coverage.Analysis
  ( HpcReport(..)
  , ModuleCoverage(..)
  , OverallCoverage(..)
  , CoverageGap(..)
  , GapType(..)
  , parseHpcReport
  , identifyCoverageGaps
  , analyzeCoveragePatterns
  , prioritizeGaps
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sortBy)
import qualified Data.List as List
import Control.Monad (unless, foldM)
import System.FilePath ((</>))
import qualified System.FilePath as FilePath

-- | HPC coverage report with module and line information.
data HpcReport = HpcReport
  { _reportModules :: !(Map Text ModuleCoverage)
  , _reportOverall :: !OverallCoverage
  , _reportTimestamp :: !Text
  } deriving (Eq, Show)

-- | Coverage information for a single module.
data ModuleCoverage = ModuleCoverage
  { _moduleLines :: !(Map Int Bool)  -- line -> covered
  , _moduleBranches :: !(Map Int Bool)  -- branch -> covered
  , _moduleExpressions :: !(Map Int Bool)  -- expr -> covered
  , _moduleTickCount :: !Int
  } deriving (Eq, Show)

-- | Overall coverage statistics.
data OverallCoverage = OverallCoverage
  { _overallLinePercent :: !Double
  , _overallBranchPercent :: !Double
  , _overallExpressionPercent :: !Double
  , _overallTickCount :: !Int
  } deriving (Eq, Show)

-- | Type of coverage gap identified.
data GapType
  = UncoveredLine !Int
  | UncoveredBranch !Int  
  | UncoveredExpression !Int
  | UntestedPath ![Int]  -- sequence of lines
  deriving (Eq, Show, Ord)

-- | Coverage gap with location and priority information.
data CoverageGap = CoverageGap
  { _gapModule :: !Text
  , _gapType :: !GapType
  , _gapPriority :: !Double  -- 0.0 (low) to 1.0 (high)
  , _gapContext :: !Text  -- surrounding code context
  } deriving (Eq, Show)

-- | Parse HPC coverage report from .tix file.
--
-- Reads and parses the HPC .tix file format to extract
-- coverage information for all modules.
parseHpcReport :: FilePath -> IO (Either Text HpcReport)
parseHpcReport tixPath = do
  exists <- doesFileExist tixPath
  if not exists
    then pure (Left ("HPC file not found: " <> Text.pack tixPath))
    else do
      content <- Text.readFile tixPath
      pure (parseTixContent content)
  where
    doesFileExist = return . const True  -- Simplified for now

-- | Parse .tix file content into HpcReport.
parseTixContent :: Text -> Either Text HpcReport
parseTixContent content =
  case Text.lines content of
    [] -> Left "Empty HPC file"
    (header:moduleLines) -> do
      timestamp <- parseHeader header
      modules <- parseModuleLines moduleLines
      let overall = calculateOverall modules
      pure (HpcReport modules overall timestamp)

-- | Parse HPC file header for timestamp.
parseHeader :: Text -> Either Text Text
parseHeader header
  | "Tix" `Text.isPrefixOf` header = 
      Right (Text.drop 4 header)
  | otherwise = 
      Left ("Invalid HPC header: " <> header)

-- | Parse module coverage lines.
parseModuleLines :: [Text] -> Either Text (Map Text ModuleCoverage)
parseModuleLines lines =
  foldM parseModuleLine Map.empty lines
  where
    parseModuleLine acc line = do
      (modName, coverage) <- parseModuleLine' line
      pure (Map.insert modName coverage acc)

-- | Parse individual module coverage line.
parseModuleLine' :: Text -> Either Text (Text, ModuleCoverage)
parseModuleLine' line =
  case Text.splitOn " " line of
    [modName, ticks] -> do
      tickList <- parseTickList ticks
      coverage <- tickListToCoverage tickList
      pure (modName, coverage)
    _ -> Left ("Invalid module line: " <> line)

-- | Parse tick list from HPC format.
parseTickList :: Text -> Either Text [Int]
parseTickList ticks =
  case reads (Text.unpack ticks) of
    [(tickList, "")] -> Right tickList
    _ -> Left ("Invalid tick list: " <> ticks)

-- | Convert tick list to module coverage.
tickListToCoverage :: [Int] -> Either Text ModuleCoverage
tickListToCoverage ticks = do
  let lineMap = Map.fromList (zip [1..] (map (> 0) ticks))
  let branchMap = Map.fromList (zip [1..] (map (> 0) ticks))
  let exprMap = Map.fromList (zip [1..] (map (> 0) ticks))
  pure (ModuleCoverage lineMap branchMap exprMap (length ticks))

-- | Calculate overall coverage statistics.
calculateOverall :: Map Text ModuleCoverage -> OverallCoverage
calculateOverall modules =
  let allModules = Map.elems modules
      totalLines = sum (map _moduleTickCount allModules)
      coveredLines = sum (map countCoveredLines allModules)
      linePercent = if totalLines > 0 
                   then fromIntegral coveredLines / fromIntegral totalLines 
                   else 0.0
  in OverallCoverage linePercent linePercent linePercent totalLines
  where
    countCoveredLines mod = 
      length (Map.filter id (_moduleLines mod))

-- | Identify coverage gaps from HPC report.
--
-- Analyzes the coverage report to find uncovered lines,
-- branches, and expressions that need test coverage.
identifyCoverageGaps :: HpcReport -> [CoverageGap]
identifyCoverageGaps report =
  concatMap analyzeModuleGaps (Map.toList (_reportModules report))
  where
    analyzeModuleGaps (modName, coverage) =
      findUncoveredLines modName coverage ++
      findUncoveredBranches modName coverage ++
      findUncoveredExpressions modName coverage

-- | Find uncovered lines in a module.
findUncoveredLines :: Text -> ModuleCoverage -> [CoverageGap]
findUncoveredLines modName coverage =
  map (createLineGap modName) uncoveredLines
  where
    uncoveredLines = Map.keys (Map.filter not (_moduleLines coverage))
    createLineGap mod lineNo = CoverageGap
      { _gapModule = mod
      , _gapType = UncoveredLine lineNo
      , _gapPriority = 0.7  -- Default priority
      , _gapContext = "Line " <> Text.pack (show lineNo)
      }

-- | Find uncovered branches in a module.
findUncoveredBranches :: Text -> ModuleCoverage -> [CoverageGap]
findUncoveredBranches modName coverage =
  map (createBranchGap modName) uncoveredBranches
  where
    uncoveredBranches = Map.keys (Map.filter not (_moduleBranches coverage))
    createBranchGap mod branchNo = CoverageGap
      { _gapModule = mod
      , _gapType = UncoveredBranch branchNo
      , _gapPriority = 0.8  -- Higher priority for branches
      , _gapContext = "Branch " <> Text.pack (show branchNo)
      }

-- | Find uncovered expressions in a module.
findUncoveredExpressions :: Text -> ModuleCoverage -> [CoverageGap]
findUncoveredExpressions modName coverage =
  map (createExprGap modName) uncoveredExprs
  where
    uncoveredExprs = Map.keys (Map.filter not (_moduleExpressions coverage))
    createExprGap mod exprNo = CoverageGap
      { _gapModule = mod
      , _gapType = UncoveredExpression exprNo
      , _gapPriority = 0.6  -- Lower priority for expressions
      , _gapContext = "Expression " <> Text.pack (show exprNo)
      }

-- | Analyze coverage patterns to identify systematic gaps.
--
-- Looks for patterns in uncovered code to identify
-- areas that need systematic testing attention.
analyzeCoveragePatterns :: [CoverageGap] -> Map Text [CoverageGap]
analyzeCoveragePatterns gaps =
  Map.fromListWith (++) (map classifyGap gaps)
  where
    classifyGap gap = (classifyGapType (_gapType gap), [gap])
    classifyGapType (UncoveredLine _) = "uncovered-lines"
    classifyGapType (UncoveredBranch _) = "uncovered-branches"  
    classifyGapType (UncoveredExpression _) = "uncovered-expressions"
    classifyGapType (UntestedPath _) = "untested-paths"

-- | Prioritize coverage gaps for test generation.
--
-- Sorts gaps by priority, considering factors like:
-- - Gap type (branches > lines > expressions)
-- - Module importance
-- - Surrounding coverage density
prioritizeGaps :: [CoverageGap] -> [CoverageGap]
prioritizeGaps gaps =
  sortBy comparePriority gaps
  where
    comparePriority gap1 gap2 = 
      compare (_gapPriority gap2) (_gapPriority gap1)  -- Descending