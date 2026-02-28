{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Dead code elimination (tree shaking) for JavaScript ASTs.
--
-- This module implements comprehensive tree shaking to remove unused imports,
-- exports, functions, variables, and statements while preserving program semantics.
-- The implementation provides:
--
--   * Usage analysis with lexical scoping support
--   * Side effect detection for safe elimination
--   * Module system analysis (ES6 imports/exports)
--   * Configurable optimization levels
--   * Cross-module dependency tracking
--   * Semantic preservation guarantees
--
-- The tree shaking process operates in two phases:
--
-- 1. **Analysis Phase**: Build usage maps by traversing the AST and tracking
--    identifier references, declarations, and module dependencies
--
-- 2. **Elimination Phase**: Remove unused code while preserving side effects
--    and maintaining program semantics
--
-- ==== Examples
--
-- Basic usage with default configuration:
--
-- >>> treeShake defaultOptions ast
-- <optimized AST with unused code removed>
--
-- Aggressive optimization with preserved exports:
--
-- >>> let opts = aggressiveShaking $ preserveExports ["main", "init"] defaultOptions
-- >>> treeShake opts ast
-- <heavily optimized AST with specified exports preserved>
--
-- Analysis-only for debugging and tooling:
--
-- >>> analyzeUsage ast
-- UsageAnalysis { totalIdentifiers = 42, unusedCount = 7, ... }
--
-- @since 0.8.0.0
module Language.JavaScript.Process.TreeShake
  ( -- * Tree Shaking
    treeShake,
    treeShakeWithAnalysis,
    
    -- * Usage Analysis
    analyzeUsage,
    analyzeUsageWithOptions,
    
    -- * Configuration
    TreeShakeOptions (..),
    defaultOptions,
    configureAggressive,
    configurePreserveExports,
    configurePreserveSideEffects,
    
    -- * Analysis Results
    UsageAnalysis (..),
    UsageInfo (..),
    ModuleDependency (..),
    
    -- * Advanced API
    buildUsageMap,
    eliminateDeadCode,
    validateTreeShaking,
    
    -- * Utilities
    hasIdentifierUsage,
    checkSideEffects,
    isExportedIdentifier,
  )
where

import Control.Lens ((^.), (.~), (&))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Language.JavaScript.Parser.AST
import Language.JavaScript.Process.TreeShake.Types
import qualified Language.JavaScript.Process.TreeShake.Analysis as Analysis
import qualified Language.JavaScript.Process.TreeShake.Elimination as Elimination

-- | Default tree shaking options with conservative settings.
--
-- Provides a safe starting point for tree shaking that prioritizes
-- correctness over optimization aggressiveness.
defaultOptions :: TreeShakeOptions
defaultOptions = defaultTreeShakeOptions

-- | Enable aggressive tree shaking optimizations.
--
-- Modifies options to remove more code with increased risk.
-- Should be used carefully and with thorough testing.
configureAggressive :: TreeShakeOptions -> TreeShakeOptions
configureAggressive opts = opts
  & Language.JavaScript.Process.TreeShake.Types.aggressiveShaking .~ True
  & Language.JavaScript.Process.TreeShake.Types.preserveTopLevel .~ False
  & Language.JavaScript.Process.TreeShake.Types.preserveSideEffectImports .~ False
  & Language.JavaScript.Process.TreeShake.Types.optimizationLevel .~ Aggressive

-- | Preserve specific export names from elimination.
--
-- Ensures that the specified identifiers are never removed
-- even if they appear unused within the analyzed code.
configurePreserveExports :: [Text.Text] -> TreeShakeOptions -> TreeShakeOptions
configurePreserveExports exports opts = opts
  & Language.JavaScript.Process.TreeShake.Types.preserveExports .~ Set.fromList exports

-- | Enable preservation of side effect statements.
--
-- Maintains statements that may have observable effects
-- even if their results are unused.
configurePreserveSideEffects :: Bool -> TreeShakeOptions -> TreeShakeOptions
configurePreserveSideEffects preserve opts = opts
  & Language.JavaScript.Process.TreeShake.Types.preserveSideEffects .~ preserve

-- | Remove unused code from JavaScript AST.
--
-- Performs comprehensive dead code elimination while preserving
-- program semantics. Returns optimized AST with unused code removed.
treeShake :: TreeShakeOptions -> JSAST -> JSAST
treeShake opts = iterativeTreeShake opts

-- | Iterative tree shaking to handle transitive dependencies.
--
-- Repeatedly performs analysis and elimination until a fixpoint is reached,
-- ensuring all transitively unused code is eliminated.
iterativeTreeShake :: TreeShakeOptions -> JSAST -> JSAST
iterativeTreeShake opts ast = go ast (0 :: Int)
  where
    maxIterations = 10  -- Prevent infinite loops

    go currentAst iteration
      | iteration >= maxIterations = currentAst  -- Safety limit
      | otherwise =
          let analysis = analyzeUsageWithOptions opts currentAst
              usageMapData = analysis ^. Language.JavaScript.Process.TreeShake.Types.usageMap
              optimizedAst = Elimination.eliminateDeadCode opts usageMapData currentAst
          in if astEqual currentAst optimizedAst
             then currentAst  -- Fixpoint reached
             else go optimizedAst (iteration + 1)

-- | Check if two ASTs are structurally equal.
-- Simple implementation using string representation for now.
astEqual :: JSAST -> JSAST -> Bool
astEqual ast1 ast2 = show ast1 == show ast2

-- | Remove unused code and return analysis information.
--
-- Combines tree shaking with detailed usage analysis for
-- debugging and optimization insights.
treeShakeWithAnalysis :: TreeShakeOptions -> JSAST -> (JSAST, UsageAnalysis)
treeShakeWithAnalysis opts ast = (optimizedAst, analysis)
  where
    analysis = analyzeUsageWithOptions opts ast
    usageMapData = analysis ^. Language.JavaScript.Process.TreeShake.Types.usageMap
    optimizedAst = Elimination.eliminateDeadCode opts usageMapData ast

-- | Analyze identifier usage without performing elimination.
--
-- Useful for understanding code structure, debugging unused code,
-- and integration with other analysis tools.
analyzeUsage :: JSAST -> UsageAnalysis
analyzeUsage = Analysis.analyzeUsage

-- | Analyze usage with specific configuration options.
--
-- Provides fine-grained control over the analysis process
-- including cross-module analysis and side effect detection.
analyzeUsageWithOptions :: TreeShakeOptions -> JSAST -> UsageAnalysis
analyzeUsageWithOptions = Analysis.analyzeUsageWithOptions

-- | Build usage map from AST analysis.
--
-- Core analysis function that traverses the AST and builds
-- comprehensive usage information for all identifiers.
buildUsageMap :: TreeShakeOptions -> JSAST -> UsageMap
buildUsageMap = Analysis.buildUsageMap

-- | Eliminate dead code using usage map.
--
-- Second phase of tree shaking that removes unused code
-- while preserving program semantics and configured exports.
eliminateDeadCode :: UsageMap -> JSAST -> JSAST
eliminateDeadCode uMap ast = Elimination.eliminateDeadCode defaultOptions uMap ast

-- | Validate tree shaking results for correctness.
--
-- Performs semantic validation to ensure that tree shaking
-- has not introduced errors or changed program behavior.
validateTreeShaking :: JSAST -> JSAST -> Bool
validateTreeShaking original optimized = 
  Elimination.validateTreeShaking original optimized

-- | Check if identifier has any usage in the AST.
--
-- Utility function for quick usage checks without full analysis.
hasIdentifierUsage :: Text.Text -> JSAST -> Bool
hasIdentifierUsage identifier ast = 
  Analysis.hasIdentifierUsage identifier ast

-- | Detect if AST node may have side effects.
--
-- Conservative analysis that identifies statements and expressions
-- that may have observable effects beyond their return values.
checkSideEffects :: JSAST -> Bool
checkSideEffects ast = Analysis.hasSideEffects ast

-- | Check if identifier is exported from module.
--
-- Determines whether an identifier is part of the module's
-- public API and should be preserved regardless of internal usage.
isExportedIdentifier :: Text.Text -> JSAST -> Bool
isExportedIdentifier identifier ast = 
  Analysis.isExportedIdentifier identifier ast