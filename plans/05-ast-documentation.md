# Plan 05: Comprehensive AST Haddock Documentation

**Priority**: P1 (Significant — unusable API without docs)
**Effort**: Large (4-6 hours)
**Risk**: Zero (documentation only, no code changes)

## Problem

`src/Language/JavaScript/Parser/AST.hs` (1622 lines) defines 80+ AST constructors but uses inline comments instead of Haddock notation. This means:

1. Generated Haddock docs show **no constructor documentation**
2. Users must read source code to understand what each constructor represents
3. IDE hover-docs show nothing useful

### Current State (Example)

```haskell
data JSExpression
  = JSIdentifier JSAnnot JSIdent                          -- ^ name
  | JSDecimal JSAnnot Double                              -- ^ 42, 3.14
  | JSLiteral JSAnnot String                              -- ^ null, true, false
  | JSHexInteger JSAnnot Integer                          -- ^ 0xFF
  | JSOctal JSAnnot Integer                               -- ^ 0o77
  | JSBinaryInteger JSAnnot Integer                       -- ^ 0b1010
  | JSBigIntLiteral JSAnnot Integer                       -- ^ 42n
  | JSStringLiteral JSAnnot ByteString                    -- ^ "hello", 'world'
```

The `-- ^` notation is correct Haddock, but the descriptions are terse one-word labels. They don't explain:
- What JavaScript syntax the constructor represents
- What each field means
- How it relates to the ECMAScript specification
- Example JavaScript that produces this AST node

## Solution

Add comprehensive Haddock documentation to all AST constructors across these types:

### Types to Document

| Type | Constructor Count | Current Doc Quality |
|------|-------------------|---------------------|
| `JSExpression` | ~35 | Terse inline labels |
| `JSStatement` | ~25 | Terse inline labels |
| `JSAST` | 6 | Minimal |
| `JSBinOp` | ~20 | None |
| `JSUnaryOp` | ~10 | None |
| `JSAssignOp` | ~12 | None |
| `JSAnnot` | 2 | Adequate |
| `JSBlock` | 1 | Adequate |
| `JSSemi` | 2 | Adequate |
| `JSArrayElement` | 2 | Minimal |
| `JSObjectProperty` | ~6 | Minimal |
| `JSSwitchParts` | 2 | Minimal |
| `JSTryCatch` | 1 | Minimal |
| `JSTryFinally` | 1 | Minimal |
| `JSModuleItem` | ~6 | Minimal |
| `JSImportClause` | ~4 | Minimal |
| `JSExportClause` | ~4 | Minimal |

### Documentation Format

Each constructor gets:

```haskell
data JSExpression
  = -- | JavaScript identifier reference.
    --
    -- Represents a variable name, function name, or any identifier
    -- in expression position. The identifier is stored as a 'JSIdent'
    -- (ByteString) preserving the original source text.
    --
    -- JavaScript: @foo@, @myVariable@, @$element@, @_private@
    JSIdentifier JSAnnot JSIdent
  | -- | Decimal numeric literal.
    --
    -- Represents decimal numbers including integers and floating point.
    -- Stored as 'Double' for uniform numeric representation.
    --
    -- JavaScript: @42@, @3.14@, @1e10@, @.5@
    JSDecimal JSAnnot Double
  | -- | Hexadecimal integer literal.
    --
    -- JavaScript: @0xFF@, @0x1A3F@
    JSHexInteger JSAnnot Integer
```

### Operator Types Documentation

```haskell
data JSBinOp
  = -- | Logical AND operator (@&&@).
    --
    -- Short-circuit evaluation: if left operand is falsy,
    -- right operand is not evaluated.
    JSBinOpAnd JSAnnot
  | -- | Bitwise AND operator (@&@).
    JSBinOpBitAnd JSAnnot
  | -- | Nullish coalescing operator (@??@).
    --
    -- Returns right operand when left is @null@ or @undefined@.
    -- ES2020 feature.
    JSBinOpNullishCoalescing JSAnnot
```

## Execution Steps

1. Document `JSAST` constructors (top-level: program, module, expression, statement, literal)
2. Document `JSExpression` constructors (~35, largest group)
3. Document `JSStatement` constructors (~25)
4. Document `JSBinOp`, `JSUnaryOp`, `JSAssignOp` operators
5. Document remaining types (`JSBlock`, `JSSemi`, `JSArrayElement`, etc.)
6. Document module-level types (`JSModuleItem`, `JSImportClause`, etc.)
7. Add module-level documentation with overview and examples
8. Verify Haddock renders: `cabal haddock`

## Files Changed

- `src/Language/JavaScript/Parser/AST.hs` — documentation additions only

## Verification

1. `cabal haddock` — renders without warnings
2. `cabal build` — still compiles (doc-only changes)
3. `cabal test` — still passes
4. Visual inspection of generated HTML docs
