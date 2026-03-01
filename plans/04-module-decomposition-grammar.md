# Plan 04: Decompose Grammar.hs (2452 Lines)

**Priority**: P1 (Significant — SRP violation, maintainability blocker)
**Effort**: Large (4-6 hours)
**Risk**: Medium (must preserve parser behavior exactly)

## Problem

`src/Language/JavaScript/Parser/Grammar.hs` is 2452 lines — the single largest module in the codebase. It contains:

1. **Expression parsing** (~800 lines): primaryExpression, binary expressions, unary, ternary, assignment, arrow functions, class expressions, template literals
2. **Statement parsing** (~600 lines): variable declarations, if/else, for/while/do, switch/case, try/catch, with, labelled statements, class declarations
3. **Module parsing** (~300 lines): import/export declarations, module items
4. **Utility/combinator functions** (~200 lines): whitespace handling, token matchers, annotation builders
5. **Literal parsing** (~150 lines): number, string, regex, boolean, null, template literals
6. **Shared infrastructure** (~400 lines): type definitions, parser helpers, operator tables

The module has 130 `<|>` alternatives and 19-way dispatch in `primaryExpression`.

## Current Structure

```
Grammar.hs (2452 lines)
├── Module exports (~50 lines)
├── Imports (~30 lines)
├── Type aliases and helpers (~100 lines)
├── Program/Module parsers (~80 lines)
├── Statement parsers (~600 lines)
├── Expression parsers (~800 lines)
├── Literal parsers (~150 lines)
├── Pattern/Destructuring (~200 lines)
├── Operator combinators (~200 lines)
└── Token utilities (~250 lines)
```

## Solution: Split into 5 Modules

### New Module Structure

```
src/Language/JavaScript/Parser/
├── Grammar.hs              -- Re-export module (preserves existing imports)
├── Grammar/
│   ├── Expression.hs       -- Expression parsing (~800 lines)
│   ├── Statement.hs        -- Statement parsing (~600 lines)
│   ├── Module.hs           -- Import/export parsing (~300 lines)
│   ├── Literal.hs          -- Literal and primary expression parsing (~350 lines)
│   └── Combinators.hs      -- Shared parser combinators, utilities (~400 lines)
```

### Module Dependencies

```
Combinators.hs  (no internal deps — tokens, annotations, whitespace)
     ↑
Literal.hs      (depends on Combinators)
     ↑
Expression.hs   (depends on Combinators, Literal, Statement for function bodies)
     ↑
Statement.hs    (depends on Combinators, Expression)
     ↑
Module.hs       (depends on Combinators, Statement, Expression)
```

Note: Expression.hs and Statement.hs have a circular dependency (expressions can contain function bodies which contain statements, and statements contain expressions). This must be broken with either:
- A mutual recursion module pattern (`.hs-boot` file)
- A shared `Internal` module that both import
- Forward declaration via parser state

### Grammar.hs Becomes a Re-export Facade

```haskell
module Language.JavaScript.Parser.Grammar
  ( -- * Re-exports for backward compatibility
    module Language.JavaScript.Parser.Grammar.Expression
  , module Language.JavaScript.Parser.Grammar.Statement
  , module Language.JavaScript.Parser.Grammar.Module
  , module Language.JavaScript.Parser.Grammar.Literal
  , module Language.JavaScript.Parser.Grammar.Combinators
  ) where

import Language.JavaScript.Parser.Grammar.Expression
import Language.JavaScript.Parser.Grammar.Statement
import Language.JavaScript.Parser.Grammar.Module
import Language.JavaScript.Parser.Grammar.Literal
import Language.JavaScript.Parser.Grammar.Combinators
```

## Execution Steps

1. Create `Grammar/Combinators.hs` — extract all shared utilities first
2. Create `Grammar/Literal.hs` — extract literal parsers
3. Create `Grammar/Expression.hs` — extract expression parsers (handle circular dep)
4. Create `Grammar/Statement.hs` — extract statement parsers
5. Create `Grammar/Module.hs` — extract module parsers
6. Reduce `Grammar.hs` to re-export facade
7. Verify `cabal build` succeeds
8. Verify `cabal test` — all tests pass identically

## Circular Dependency Strategy

Expression parsing calls statement parsing (function bodies) and vice versa. Options:

**Option A: `.hs-boot` forward declaration** (preferred)
```haskell
-- Grammar/Statement.hs-boot
module Language.JavaScript.Parser.Grammar.Statement where
import Language.JavaScript.Parser.AST (JSStatement)
statement :: JSParser JSStatement
```

**Option B: Shared internal module**
```haskell
-- Grammar/Internal.hs
-- Contains mutually recursive parsers that reference both expressions and statements
```

## Files Changed

- `src/Language/JavaScript/Parser/Grammar.hs` — reduced to facade
- New: `src/Language/JavaScript/Parser/Grammar/Combinators.hs`
- New: `src/Language/JavaScript/Parser/Grammar/Literal.hs`
- New: `src/Language/JavaScript/Parser/Grammar/Expression.hs`
- New: `src/Language/JavaScript/Parser/Grammar/Statement.hs`
- New: `src/Language/JavaScript/Parser/Grammar/Module.hs`
- `language-javascript.cabal` — add new modules to `exposed-modules` or `other-modules`

## Verification

1. `cabal build` — zero warnings
2. `cabal test` — all 1488 tests pass
3. `cabal bench` — no performance regression (module splitting should be zero-cost)
4. Downstream: any code importing `Language.JavaScript.Parser.Grammar` still compiles
