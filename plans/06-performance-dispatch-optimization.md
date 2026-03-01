# Plan 06: Performance — First-Byte Dispatch Optimization

**Priority**: P2 (Enhancement — measurable throughput improvement)
**Effort**: Large (5-8 hours)
**Risk**: Medium (parser hot path changes, requires thorough testing)

## Problem

The parser's primary performance bottleneck is the 19-way `<|>` chain in `primaryExpression` (Grammar.hs:751-771). Each `<|>` alternative backtracks on failure, meaning worst-case an identifier (the most common expression type) tries and fails 18 alternatives before succeeding.

Similarly, `statement` has a 17-way `<|>` chain.

Current benchmark results:
- Parse 5KB ES5: 1.1 ms (4.5 MB/s)
- Parse 11KB mixed: 2.4 ms (4.6 MB/s)
- Scaling: linear but with high constant factor

## Root Cause

FlatParse's `<|>` uses backtracking. Each failed alternative:
1. Saves parser state
2. Attempts parse
3. On failure, restores state and tries next alternative

For `primaryExpression`, the first byte of input is enough to determine which alternative to try in most cases:

| First Byte | Expression Type |
|------------|----------------|
| `0-9`, `.` | Numeric literal |
| `'`, `"` | String literal |
| `` ` `` | Template literal |
| `[` | Array literal |
| `{` | Object literal |
| `(` | Parenthesized expression |
| `/` | Regex literal |
| `#` | Private identifier |
| `t` | `this`, `true`, `typeof`, identifier |
| `f` | `false`, `function`, `for`, identifier |
| `n` | `null`, `new`, identifier |
| `s` | `super`, `switch`, identifier |
| `c` | `class`, `const`, identifier |
| `a` | `async`, `await`, identifier |
| `i` | `import`, `if`, `in`, identifier |
| `...` | Spread |
| `_`, `$`, letter | Identifier |

## Solution: Peek-Dispatch Pattern

### 1. First-Byte Dispatch Table for `primaryExpression`

```haskell
primaryExpression :: JSParser JSExpression
primaryExpression = do
  w <- peekByte
  dispatchPrimary w

dispatchPrimary :: Word8 -> JSParser JSExpression
dispatchPrimary w
  -- Numeric literals: 0-9 or leading dot
  | isDigit w = literalExpression
  | w == 0x2E = literalExpression  -- '.'
  -- String literals
  | w == 0x27 = literalExpression  -- '\''
  | w == 0x22 = literalExpression  -- '"'
  -- Template literal
  | w == 0x60 = templateLiteral   -- '`'
  -- Grouping
  | w == 0x5B = arrayLiteral      -- '['
  | w == 0x7B = objectLiteral     -- '{'
  | w == 0x28 = parenthesizedExpression  -- '('
  -- Regex
  | w == 0x2F = regexLiteral      -- '/'
  -- Spread
  | w == 0x2E = spreadExpression  -- '...' (handled after dot-number check)
  -- Private identifier
  | w == 0x23 = privateIdentifierExpression  -- '#'
  -- Keyword-starting identifiers need secondary dispatch
  | otherwise = keywordOrIdentifier
```

### 2. Keyword Secondary Dispatch

For bytes that could be keywords or identifiers, use keyword prefix matching:

```haskell
keywordOrIdentifier :: JSParser JSExpression
keywordOrIdentifier =
  thisLiteral FP.<|>
  superLiteral FP.<|>
  nullLiteral FP.<|>
  booleanLiteral FP.<|>
  importExpression FP.<|>
  asyncGeneratorExpr FP.<|>
  asyncFunctionExpr FP.<|>
  generatorExpression FP.<|>
  functionExpression FP.<|>
  classExpression FP.<|>
  identifierExpression
```

This is still a chain but only reached for identifier-like tokens (~5 alternatives for any given first letter).

### 3. Statement Dispatch

Same pattern for `statement`:

```haskell
statementDispatch :: Word8 -> JSParser JSStatement
statementDispatch w
  | w == 0x7B = blockStatement      -- '{'
  | w == 0x3B = emptyStatement      -- ';'
  | w == 0x69 = ifOrImportStatement -- 'i' → if/import
  | w == 0x77 = whileOrWithStatement -- 'w'
  | w == 0x66 = forOrFunctionStatement -- 'f'
  | w == 0x72 = returnStatement     -- 'r'
  | w == 0x74 = tryOrThrowStatement -- 't'
  | w == 0x73 = switchStatement     -- 's'
  | w == 0x62 = breakStatement      -- 'b'
  | w == 0x63 = continueOrClassStatement -- 'c'
  | w == 0x64 = doOrDebuggerStatement -- 'd'
  | w == 0x6C = labelledOrLetStatement -- 'l'
  | w == 0x65 = exportStatement     -- 'e'
  | otherwise = expressionStatement
```

### 4. Add INLINE Pragmas

Add `{-# INLINE #-}` to hot path functions (currently zero INLINE pragmas in Grammar.hs):

```haskell
{-# INLINE primaryExpression #-}
{-# INLINE dispatchPrimary #-}
{-# INLINE keywordOrIdentifier #-}
{-# INLINE peekByte #-}
{-# INLINE literalExpression #-}
{-# INLINE identifierExpression #-}
{-# INLINE statement #-}
{-# INLINE statementDispatch #-}
```

## Expected Performance Improvement

- **Primary expression dispatch**: 19 alternatives → 1 lookup + 1-5 alternatives = ~3-4x fewer backtracks
- **Statement dispatch**: 17 alternatives → 1 lookup + 1-3 alternatives = ~3-5x fewer backtracks
- **INLINE pragmas**: eliminate function call overhead in inner loop
- **Overall**: ~1.5-2x throughput improvement estimated

## Files Changed

- `src/Language/JavaScript/Parser/Grammar.hs` (or `Grammar/Expression.hs` if Plan 04 is done first)
- Potentially `src/Language/JavaScript/Parser/Grammar/Statement.hs`

## Verification

1. `cabal test` — all 1488 tests pass identically
2. `cabal bench` — measure before/after:
   - Parse 5KB target: <0.7 ms (was 1.1 ms)
   - Parse 11KB target: <1.5 ms (was 2.4 ms)
3. Run full golden test suite — no output differences
4. Fuzz test: 10,000 random valid JS programs parse identically
