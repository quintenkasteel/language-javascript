{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | Core data types for tree shaking analysis and configuration.
--
-- This module defines the fundamental types used throughout the tree
-- shaking implementation, including configuration options, usage tracking,
-- and analysis results. All types are designed for performance and
-- comprehensive analysis coverage.
--
-- The type system provides strong guarantees about analysis correctness
-- and enables efficient implementation of complex dependency tracking.
--
-- @since 0.8.0.0
module Language.JavaScript.Process.TreeShake.Types
  ( -- * Configuration Types
    TreeShakeOptions (..),
    OptimizationLevel (..),
    
    -- * Usage Analysis Types  
    UsageInfo (..),
    UsageMap,
    ScopeInfo (..),
    ScopeType (..),
    ScopeStack,
    
    -- * Module Analysis Types
    ModuleDependency (..),
    ImportInfo (..),
    ExportInfo (..),
    
    -- * Analysis Results
    UsageAnalysis (..),
    EliminationResult (..),
    
    -- * Lenses for TreeShakeOptions
    preserveTopLevel,
    preserveSideEffects,
    aggressiveShaking,
    preserveExports,
    preserveSideEffectImports,
    crossModuleAnalysis,
    optimizationLevel,
    
    -- * Lenses for UsageInfo
    isUsed,
    isExported,
    directReferences,
    hasSideEffects,
    declarationLocation,
    importedBy,
    scopeDepth,
    
    -- * Lenses for UsageAnalysis
    usageMap,
    moduleDependencies,
    totalIdentifiers,
    unusedCount,
    sideEffectCount,
    estimatedReduction,
    hasEvalCall,
    evalCallCount,
    dynamicAccessObjects,

    -- * Lenses for ImportInfo
    importModule,
    importedNames,
    importDefault,
    importNamespace,
    importLocation,
    isImportTypeOnly,

    -- * Lenses for ExportInfo
    exportedName,
    localName,
    exportModule,
    exportLocation,
    isDefaultExport,
    isExportTypeOnly,

    -- * Lenses for ModuleDependency
    moduleName,
    imports,
    exports,
    hasImportSideEffects,
    reExportsFrom,

    -- * Lenses for EliminationResult
    eliminatedIdentifiers,
    preservedIdentifiers,
    eliminationReasons,
    preservationReasons,
    actualReduction,

    -- * Lenses for ScopeInfo
    scopeType,
    scopeLevel,
    scopeBindings,
    parentScope,

    -- * Smart Constructors
    defaultUsageInfo,
    emptyUsageAnalysis,
    defaultTreeShakeOptions,
    
    -- * Utility Functions
    isIdentifierUsed,
    hasDirectReferences,
    isPreservedExport,
  )
where

import Control.DeepSeq (NFData)
import Lens.Micro.TH (makeLenses)
import Data.Data
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Language.JavaScript.Parser.SrcLocation (TokenPosn)

-- | Optimization levels for tree shaking aggressiveness.
--
-- Provides predefined configurations balancing safety and optimization.
-- Higher levels remove more code but with increased risk of incorrect elimination.
data OptimizationLevel
  = Conservative  -- ^ Safe optimization, preserves potentially used code
  | Balanced      -- ^ Default optimization level with good safety/performance balance  
  | Aggressive    -- ^ Maximum optimization, may remove seemingly unused code
  | Debug         -- ^ Minimal optimization, useful for debugging
  deriving (Data, Eq, Generic, NFData, Ord, Show, Typeable)

-- | Configuration options for tree shaking optimization.
--
-- Comprehensive configuration system allowing fine-tuned control
-- over the tree shaking process. Uses lenses for convenient modification.
data TreeShakeOptions = TreeShakeOptions
  { _preserveTopLevel :: !Bool
    -- ^ Preserve all top-level statements even if unused
    
  , _preserveSideEffects :: !Bool
    -- ^ Preserve statements that may have side effects
    
  , _aggressiveShaking :: !Bool
    -- ^ Enable aggressive optimizations with higher risk
    
  , _preserveExports :: !(Set.Set Text.Text)
    -- ^ Set of export names to always preserve
    
  , _preserveSideEffectImports :: !Bool
    -- ^ Preserve unused imports that may have side effects
    
  , _crossModuleAnalysis :: !Bool
    -- ^ Enable cross-module dependency analysis
    
  , _optimizationLevel :: !OptimizationLevel
    -- ^ Overall optimization level setting
  } deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Information about identifier usage and context.
--
-- Comprehensive tracking of how identifiers are used throughout
-- the codebase, enabling precise elimination decisions.
data UsageInfo = UsageInfo
  { _isUsed :: !Bool
    -- ^ Whether this identifier is referenced anywhere
    
  , _isExported :: !Bool
    -- ^ Whether this identifier is exported from the module
    
  , _directReferences :: !Int
    -- ^ Number of direct references to this identifier
    
  , _hasSideEffects :: !Bool
    -- ^ Whether this identifier may have side effects
    
  , _declarationLocation :: !(Maybe TokenPosn)
    -- ^ Source location of the identifier declaration
    
  , _importedBy :: !(Set.Set Text.Text)
    -- ^ Set of modules that import this identifier
    
  , _scopeDepth :: !Int
    -- ^ Lexical scope depth where identifier is declared
  } deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Map from identifier names to usage information.
--
-- Central data structure for tracking identifier usage across
-- the entire AST analysis process.
type UsageMap = Map.Map Text.Text UsageInfo

-- | Information about lexical scope context.
--
-- Tracks variable bindings and their visibility within
-- different scope levels of the JavaScript program.
data ScopeInfo = ScopeInfo
  { _scopeType :: !ScopeType
    -- ^ Type of scope (function, block, module, etc.)
    
  , _scopeBindings :: !(Set.Set Text.Text)
    -- ^ Variables bound in this scope
    
  , _scopeLevel :: !Int
    -- ^ Nesting level of this scope
    
  , _parentScope :: !(Maybe ScopeInfo)
    -- ^ Parent scope information
  } deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Different types of JavaScript scopes.
--
-- Distinguishes between various scope types which have
-- different binding and visibility rules.
data ScopeType
  = GlobalScope     -- ^ Global/module scope
  | FunctionScope   -- ^ Function parameter and body scope
  | BlockScope      -- ^ Block scope (let/const)
  | ClassScope      -- ^ Class body scope
  | CatchScope      -- ^ try/catch exception scope
  deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Stack of scope information during AST traversal.
--
-- Maintains the current scope context while analyzing
-- identifier usage and variable binding.
type ScopeStack = [ScopeInfo]

-- | Import information for module analysis.
--
-- Detailed tracking of how modules import from other modules,
-- enabling cross-module dependency analysis.
data ImportInfo = ImportInfo
  { _importModule :: !Text.Text
    -- ^ Name of the module being imported from
    
  , _importedNames :: !(Set.Set Text.Text)
    -- ^ Set of specific names imported
    
  , _importDefault :: !(Maybe Text.Text)
    -- ^ Default import name if present
    
  , _importNamespace :: !(Maybe Text.Text)
    -- ^ Namespace import name if present
    
  , _importLocation :: !TokenPosn
    -- ^ Source location of import statement
    
  , _isImportTypeOnly :: !Bool
    -- ^ Whether this is a type-only import (TypeScript)
  } deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Export information for module analysis.
--
-- Tracks what identifiers are exported from modules and how,
-- supporting various export patterns and re-exports.
data ExportInfo = ExportInfo
  { _exportedName :: !Text.Text
    -- ^ Name as exported (may differ from local name)
    
  , _localName :: !(Maybe Text.Text)
    -- ^ Local name if different from exported name
    
  , _exportModule :: !(Maybe Text.Text)
    -- ^ Module name for re-exports
    
  , _exportLocation :: !TokenPosn
    -- ^ Source location of export statement
    
  , _isDefaultExport :: !Bool
    -- ^ Whether this is the default export
    
  , _isExportTypeOnly :: !Bool
    -- ^ Whether this is a type-only export (TypeScript)
  } deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Module dependency information.
--
-- Comprehensive representation of relationships between modules
-- including imports, exports, and re-export patterns.
data ModuleDependency = ModuleDependency
  { _moduleName :: !Text.Text
    -- ^ Name of the module
    
  , _imports :: ![ImportInfo]
    -- ^ List of imports from other modules
    
  , _exports :: ![ExportInfo]
    -- ^ List of exports to other modules
    
  , _hasImportSideEffects :: !Bool
    -- ^ Whether this module has side effects on import
    
  , _reExportsFrom :: !(Set.Set Text.Text)
    -- ^ Modules that this module re-exports from
  } deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Complete usage analysis results.
--
-- Aggregates all analysis information including usage patterns,
-- module dependencies, and optimization opportunities.
data UsageAnalysis = UsageAnalysis
  { _usageMap :: !UsageMap
    -- ^ Map from identifier names to usage information
    
  , _moduleDependencies :: ![ModuleDependency]
    -- ^ Module dependency graph
    
  , _totalIdentifiers :: !Int
    -- ^ Total number of identifiers analyzed
    
  , _unusedCount :: !Int
    -- ^ Number of unused identifiers found
    
  , _sideEffectCount :: !Int
    -- ^ Number of identifiers with side effects
    
  , _estimatedReduction :: !Double
    -- ^ Estimated size reduction from tree shaking (0.0 to 1.0)
  , _hasEvalCall :: !Bool
    -- ^ Whether eval() was detected (affects conservative optimization)
  , _evalCallCount :: !Int
    -- ^ Number of eval/Function constructor calls detected
  , _dynamicAccessObjects :: !(Set.Set Text.Text)
    -- ^ Objects that are accessed with dynamic/computed property names
  } deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- | Result of code elimination process.
--
-- Provides detailed information about what was eliminated
-- and the impact of the tree shaking process.
data EliminationResult = EliminationResult
  { _eliminatedIdentifiers :: !(Set.Set Text.Text)
    -- ^ Set of identifiers that were eliminated
    
  , _preservedIdentifiers :: !(Set.Set Text.Text)
    -- ^ Set of identifiers that were preserved
    
  , _eliminationReasons :: !(Map.Map Text.Text Text.Text)
    -- ^ Reasons why specific identifiers were eliminated
    
  , _preservationReasons :: !(Map.Map Text.Text Text.Text)
    -- ^ Reasons why specific identifiers were preserved
    
  , _actualReduction :: !Double
    -- ^ Actual size reduction achieved
  } deriving (Data, Eq, Generic, NFData, Show, Typeable)

-- Generate lenses for all record types
makeLenses ''TreeShakeOptions
makeLenses ''UsageInfo
makeLenses ''UsageAnalysis
makeLenses ''ImportInfo
makeLenses ''ExportInfo
makeLenses ''ModuleDependency
makeLenses ''EliminationResult
makeLenses ''ScopeInfo

-- | Default usage information for new identifiers.
--
-- Provides safe defaults that err on the side of preservation
-- until usage analysis determines otherwise.
defaultUsageInfo :: UsageInfo
defaultUsageInfo = UsageInfo
  { _isUsed = False
  , _isExported = False
  , _directReferences = 0
  , _hasSideEffects = False
  , _declarationLocation = Nothing
  , _importedBy = Set.empty
  , _scopeDepth = 0
  }

-- | Empty usage analysis for initialization.
--
-- Provides a clean starting state for usage analysis
-- that can be incrementally populated during AST traversal.
emptyUsageAnalysis :: UsageAnalysis
emptyUsageAnalysis = UsageAnalysis
  { _usageMap = Map.empty
  , _moduleDependencies = []
  , _totalIdentifiers = 0
  , _unusedCount = 0
  , _sideEffectCount = 0
  , _estimatedReduction = 0.0
  , _hasEvalCall = False
  , _evalCallCount = 0
  , _dynamicAccessObjects = Set.empty
  }

-- | Default tree shaking options with conservative settings.
--
-- Safe starting configuration that prioritizes correctness
-- over optimization aggressiveness.
defaultTreeShakeOptions :: TreeShakeOptions
defaultTreeShakeOptions = TreeShakeOptions
  { _preserveTopLevel = False  -- Allow elimination of unused top-level code
  , _preserveSideEffects = True
  , _aggressiveShaking = False
  , _preserveExports = Set.empty
  , _preserveSideEffectImports = True
  , _crossModuleAnalysis = False
  , _optimizationLevel = Balanced
  }

-- | Check if identifier is marked as used.
--
-- Utility function for quick usage checks during elimination.
isIdentifierUsed :: Text.Text -> UsageMap -> Bool
isIdentifierUsed identifier uMap =
  case Map.lookup identifier uMap of
    Just info -> _isUsed info
    Nothing -> False

-- | Check if identifier has any direct references.
--
-- Determines if identifier has explicit references in the code
-- beyond just being declared.
hasDirectReferences :: Text.Text -> UsageMap -> Bool
hasDirectReferences identifier uMap =
  case Map.lookup identifier uMap of
    Just info -> _directReferences info > 0
    Nothing -> False

-- | Check if identifier is a preserved export.
--
-- Determines if identifier should be preserved due to being
-- in the configured preservation list.
isPreservedExport :: Text.Text -> TreeShakeOptions -> Bool
isPreservedExport identifier opts = 
  identifier `Set.member` _preserveExports opts