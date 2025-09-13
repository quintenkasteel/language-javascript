# Tree Shaking Implementation TODO

## Overview
Implement dead code elimination (tree shaking) for JavaScript ASTs to remove unused imports, exports, functions, variables, and statements while preserving program semantics.

Based on analysis of the current codebase, this implementation will:
- Build on existing AST definitions (`Language.JavaScript.Parser.AST`)
- Follow patterns from minification module (`Language.JavaScript.Process.Minify`) 
- Use existing validation framework (`Language.JavaScript.Parser.Validator`)
- Integrate with comprehensive test suite structure

## Phase 1: Core Tree Shaking Infrastructure

### 1.1 Create Tree Shaking Module Structure
- [ ] Create `src/Language/JavaScript/Process/TreeShake.hs` module
- [ ] Define tree shaking configuration data types
- [ ] Create tree shaking result types with usage statistics
- [ ] Add exports to main `Language.JavaScript.Parser` module

#### Test Coverage:
- [ ] Unit test for module creation and basic types
- [ ] Test configuration validation

### 1.2 Define Usage Analysis Types
- [ ] Create `UsageMap` type for tracking identifier usage
- [ ] Define `Scope` and `ScopeStack` types for lexical scoping
- [ ] Create `ExportInfo` and `ImportInfo` types for module analysis  
- [ ] Add `TreeShakeOptions` configuration type

```haskell
data TreeShakeOptions = TreeShakeOptions
  { preserveTopLevel :: Bool          -- Keep top-level statements
  , preserveSideEffects :: Bool       -- Keep statements with side effects
  , aggressiveShaking :: Bool         -- Remove more conservatively
  , preserveExports :: [String]       -- Always keep these exports
  }

data UsageInfo = UsageInfo
  { isUsed :: Bool
  , isExported :: Bool
  , hasDirectReferences :: Int
  , hasSideEffects :: Bool
  }
```

#### Test Coverage:
- [ ] Unit tests for data type construction
- [ ] Property tests for UsageMap operations
- [ ] Test scope stack manipulation

### 1.3 Implement AST Traversal Infrastructure  
- [ ] Create generic AST traversal functions using existing patterns from `Minify.hs`
- [ ] Implement usage analysis traversal (find all identifier references)
- [ ] Create scope-aware traversal for lexical binding analysis
- [ ] Add utility functions for identifier extraction

```haskell
class TreeShakeTraversal a where
  analyzeUsage :: ScopeStack -> a -> State UsageMap ()
  eliminateUnused :: UsageMap -> a -> a
```

#### Test Coverage:
- [ ] Unit tests for traversal functions on each AST node type
- [ ] Test scope handling for functions, blocks, modules
- [ ] Property tests: traversal preserves structure for used code

## Phase 2: Usage Analysis Implementation

### 2.1 Identifier Reference Analysis
- [ ] Implement identifier usage tracking for expressions
- [ ] Handle variable references in `JSIdentifier` nodes
- [ ] Track member access patterns (`JSMemberDot`, `JSMemberSquare`)
- [ ] Analyze function call usage (`JSCallExpression`)
- [ ] Handle destructuring patterns in assignments

#### Test Coverage:
- [ ] Test simple variable reference tracking
- [ ] Test complex member access chains (`obj.prop.method()`)
- [ ] Test destructuring assignment tracking `{a, b} = obj`
- [ ] Test function parameter usage analysis

### 2.2 Declaration Analysis  
- [ ] Track variable declarations (`JSVariable`, `JSLet`, `JSConstant`)
- [ ] Analyze function declarations (`JSFunction`, `JSAsyncFunction`, `JSGenerator`) 
- [ ] Handle class declarations (`JSClass`) and methods
- [ ] Track import/export declarations (`JSModuleItem`)

#### Test Coverage:
- [ ] Test variable declaration analysis: `var a = 1, b = 2;`
- [ ] Test function declaration tracking with parameters
- [ ] Test class declaration with methods and inheritance
- [ ] Test module import/export analysis

### 2.3 Module System Analysis
- [ ] Analyze ES6 import statements (`JSImportDeclaration`)
- [ ] Track named imports, default imports, namespace imports
- [ ] Analyze export statements (`JSExportDeclaration`)  
- [ ] Handle re-exports and export-from patterns
- [ ] Track import attributes and dynamic imports

#### Test Coverage:
- [ ] Test named import analysis: `import {a, b as c} from 'module'`
- [ ] Test default import tracking: `import React from 'react'`
- [ ] Test namespace imports: `import * as Utils from 'utils'`
- [ ] Test export analysis: `export {a, b as c}`, `export default`, `export * from`
- [ ] Test re-export patterns: `export {a} from 'other'`

### 2.4 Side Effect Analysis
- [ ] Detect statements with side effects (assignments, function calls)
- [ ] Analyze property assignments and mutations
- [ ] Track `delete`, `typeof`, `void` operations
- [ ] Handle constructor calls and `new` expressions
- [ ] Detect eval and indirect eval usage

#### Test Coverage:
- [ ] Test assignment side effect detection: `obj.prop = value`
- [ ] Test function call side effects: `sideEffectFunction()`
- [ ] Test constructor side effects: `new SomeClass()`
- [ ] Test property deletion: `delete obj.prop`
- [ ] Test complex side effect chains

## Phase 3: Dead Code Elimination

### 3.1 Statement-Level Elimination
- [ ] Remove unused variable declarations
- [ ] Eliminate unreferenced functions
- [ ] Remove unused class declarations
- [ ] Handle unused statement blocks and control structures
- [ ] Preserve side-effect statements even if unused

#### Test Coverage:
- [ ] Test unused variable removal: `var unused = 5; var used = 10; console.log(used);`
- [ ] Test unused function elimination with preserved used functions
- [ ] Test unused class removal with inheritance chains
- [ ] Test preservation of side-effect statements: `console.log('side effect');`

### 3.2 Expression-Level Elimination  
- [ ] Remove unused object properties in literals
- [ ] Eliminate unused array elements where safe
- [ ] Clean up unused parameters in arrow functions
- [ ] Remove unused destructuring properties
- [ ] Optimize conditional expressions with unused branches

#### Test Coverage:
- [ ] Test object property elimination: `{used: 1, unused: 2}` → `{used: 1}`
- [ ] Test unused parameter removal in arrows: `(used, unused) => used`
- [ ] Test destructuring cleanup: `const {used, unused} = obj`
- [ ] Test conditional branch elimination in dead code paths

### 3.3 Import/Export Optimization
- [ ] Remove unused import specifiers
- [ ] Eliminate unused import declarations
- [ ] Clean up unused export specifiers  
- [ ] Handle module re-export optimization
- [ ] Preserve dynamic imports and side-effect imports

#### Test Coverage:
- [ ] Test unused import removal: `import {used, unused} from 'mod'` → `import {used} from 'mod'`
- [ ] Test full import elimination when nothing used
- [ ] Test export cleanup with cross-module analysis
- [ ] Test preservation of side-effect imports: `import 'polyfill'`

### 3.4 Advanced Elimination Patterns
- [ ] Handle circular dependency elimination 
- [ ] Implement cross-module usage analysis
- [ ] Remove unused switch cases and default branches
- [ ] Eliminate unreachable code after returns/throws
- [ ] Optimize logical expressions with unused operands

#### Test Coverage:
- [ ] Test circular dependency handling between modules
- [ ] Test unreachable code removal after `return`/`throw`
- [ ] Test switch case elimination when condition is constant
- [ ] Test logical expression optimization: `false && unused()` → `false`

## Phase 4: Integration & API

### 4.1 Public API Design
- [ ] Create main `treeShake` function with configuration
- [ ] Add `treeShakeWithAnalysis` for debugging information
- [ ] Create `analyzeUsage` function for usage reporting
- [ ] Add utility functions for incremental analysis
- [ ] Design fluent configuration API

```haskell
-- Primary API functions
treeShake :: TreeShakeOptions -> JSAST -> JSAST
treeShakeWithAnalysis :: TreeShakeOptions -> JSAST -> (JSAST, UsageAnalysis)
analyzeUsage :: JSAST -> UsageAnalysis

-- Configuration builders  
defaultOptions :: TreeShakeOptions
aggressiveShaking :: TreeShakeOptions -> TreeShakeOptions
preserveExports :: [String] -> TreeShakeOptions -> TreeShakeOptions
```

#### Test Coverage:
- [ ] Test public API functions with various input configurations
- [ ] Test fluent configuration API construction
- [ ] Integration test with full tree shaking pipeline

### 4.2 Integration with Existing Modules
- [ ] Add tree shaking option to `Language.JavaScript.Parser` module
- [ ] Integration with Pretty Printer for output verification
- [ ] Coordinate with Minify module for combined optimization
- [ ] Add tree shaking statistics to validation output

#### Test Coverage:
- [ ] Test integration with parser module exports
- [ ] Test combined tree shaking + minification workflow
- [ ] Test pretty printing of tree-shaken output
- [ ] Round-trip test: parse → tree shake → pretty print → parse

## Phase 5: Advanced Features

### 5.1 Cross-Module Analysis  
- [ ] Implement multi-file dependency analysis
- [ ] Create module graph construction and traversal
- [ ] Add cross-module dead code detection
- [ ] Handle dynamic imports and conditional requires
- [ ] Support CommonJS and ES6 module interop

#### Test Coverage:
- [ ] Test multi-file tree shaking with module graph
- [ ] Test cross-module dependency resolution
- [ ] Test dynamic import preservation
- [ ] Test CommonJS/ES6 mixed module handling

### 5.2 Control Flow Analysis
- [ ] Implement basic constant folding for conditionals
- [ ] Analyze reachable code paths in if/switch statements
- [ ] Handle function purity analysis for call elimination
- [ ] Track variable mutability for more aggressive optimization
- [ ] Implement basic escape analysis

#### Test Coverage:
- [ ] Test constant folding: `if (true) { /* keep */ } else { /* remove */ }`
- [ ] Test unreachable switch case elimination
- [ ] Test pure function call elimination
- [ ] Test escape analysis for local variables

### 5.3 Performance Optimizations
- [ ] Implement incremental analysis for large codebases  
- [ ] Add parallel processing for independent modules
- [ ] Create caching layer for repeated analysis
- [ ] Optimize memory usage for large ASTs
- [ ] Add progress reporting for long operations

#### Test Coverage:
- [ ] Benchmark tests for large file performance
- [ ] Memory usage tests for incremental analysis
- [ ] Correctness tests for parallel processing
- [ ] Performance regression tests

## Phase 6: Testing & Validation

### 6.1 Unit Test Suite
- [ ] Core algorithm unit tests (50+ test cases)
- [ ] AST traversal correctness tests
- [ ] Usage analysis verification tests  
- [ ] Edge case handling tests
- [ ] Error condition tests

#### Specific Test Categories:
- [ ] **Basic elimination**: Simple unused variable/function removal
- [ ] **Scoping**: Variable shadowing, lexical scopes, closures
- [ ] **Side effects**: Assignment, function calls, property access
- [ ] **Modules**: Import/export analysis, re-exports
- [ ] **Control flow**: Conditionals, loops, early returns
- [ ] **Edge cases**: Empty modules, circular deps, eval usage

### 6.2 Integration Tests
- [ ] Round-trip parsing tests (tree shake → pretty print → parse)
- [ ] Combined optimization tests (tree shake + minify)
- [ ] Real-world JavaScript library tests
- [ ] Performance benchmarks on large codebases
- [ ] Memory usage validation tests

#### Test Files:
- [ ] Create `test/Unit/Language/Javascript/Process/TreeShake/Core.hs`
- [ ] Create `test/Unit/Language/Javascript/Process/TreeShake/Usage.hs` 
- [ ] Create `test/Unit/Language/Javascript/Process/TreeShake/Elimination.hs`
- [ ] Create `test/Integration/Language/Javascript/Process/TreeShake.hs`
- [ ] Create `test/Benchmarks/Language/Javascript/Process/TreeShake.hs`

### 6.3 Property-Based Testing
- [ ] Semantic preservation properties
- [ ] Idempotence properties (tree shake twice = tree shake once)
- [ ] Monotonicity properties (more usage → less elimination)
- [ ] Commutativity with other transformations

```haskell
-- Key properties to test
prop_semanticPreservation :: JSAST -> Bool
prop_idempotent :: JSAST -> Bool  
prop_monotonic :: UsageMap -> JSAST -> Bool
prop_combinesWithMinify :: JSAST -> Bool
```

#### Test Coverage:
- [ ] Generate random valid JavaScript ASTs for testing
- [ ] Property test for semantic preservation 
- [ ] Property test for idempotence
- [ ] Property test for combination with existing tools

### 6.4 Golden Tests  
- [ ] Real-world JavaScript examples with expected outputs
- [ ] Library code examples (React components, utilities)
- [ ] Module system examples (ES6, CommonJS)
- [ ] Complex control flow examples
- [ ] Edge case examples

#### Golden Test Files:
- [ ] `test/Golden/TreeShake/BasicElimination.js`
- [ ] `test/Golden/TreeShake/ModuleAnalysis.js`
- [ ] `test/Golden/TreeShake/SideEffects.js`
- [ ] `test/Golden/TreeShake/ControlFlow.js`
- [ ] `test/Golden/TreeShake/RealWorld.js`

## Phase 7: Documentation & Deployment

### 7.1 API Documentation
- [ ] Comprehensive Haddock documentation for all public functions
- [ ] Usage examples and tutorials
- [ ] Performance characteristics documentation  
- [ ] Integration guide with existing tools
- [ ] Troubleshooting guide

### 7.2 Performance Documentation
- [ ] Benchmark results and performance characteristics
- [ ] Memory usage guidelines
- [ ] Scalability recommendations
- [ ] Configuration tuning guide

### 7.3 Testing Integration
- [ ] Add tree shaking tests to main test suite
- [ ] Update `testsuite.hs` to include new test modules
- [ ] Add CI/CD integration for tree shaking tests
- [ ] Create performance regression tests

## Implementation Notes

### Following CLAUDE.md Standards

**Function Size & Complexity**:
- All functions ≤15 lines, ≤4 parameters, ≤4 branching points
- Extract complex logic into smaller, focused functions
- Use record types for complex configuration

**Code Style**:
- Qualified imports (functions qualified, types unqualified)
- Lens usage for record access/updates
- `where` clauses preferred over `let`
- No comments unless explicitly needed
- Comprehensive Haddock documentation

**Testing Requirements**:
- Minimum 85% test coverage
- NO MOCK FUNCTIONS - test actual functionality
- Property tests for invariants
- Golden tests for expected outputs
- Unit tests for every public function

### AST Integration Patterns

Follow existing patterns from `Minify.hs`:
```haskell
class TreeShake a where
  shakeTree :: TreeShakeOptions -> UsageMap -> a -> a

-- Pattern for AST traversal
instance TreeShake JSExpression where
  shakeTree opts usage (JSIdentifier ann name)
    | name `Map.member` usage = JSIdentifier ann name  
    | otherwise = eliminateUnused opts (JSIdentifier ann name)
```

### Module Structure

```
src/Language/JavaScript/Process/
├── TreeShake.hs                    -- Main module
├── TreeShake/
│   ├── Analysis.hs                 -- Usage analysis
│   ├── Elimination.hs              -- Dead code removal  
│   ├── Types.hs                    -- Data types
│   └── Utilities.hs                -- Helper functions
```

This comprehensive plan provides a robust foundation for implementing tree shaking while following the codebase's existing patterns, architectural principles, and testing standards.