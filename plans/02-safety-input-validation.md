# Plan 02: Add Input Validation and Resource Limits

**Priority**: P0 (Blocking — DoS risk in production)
**Effort**: Medium (2-3 hours)
**Risk**: Low (additive, no behavior change for valid inputs)

## Problem

The parser has zero input validation. Any caller can submit:
- A 1 GB JavaScript file → unbounded memory allocation
- A 10,000-level nested expression → stack overflow
- Invalid UTF-8 bytes on the ByteString path → undefined behavior

There are no guardrails between untrusted input and the parser internals.

## Current State

```haskell
-- Parser.hs: No size check
parseBS :: ByteString -> Either String AST.JSAST
parseBS = handleResult . FlatParser.parseProgramByteString

-- Grammar.hs: No nesting depth tracking
primaryExpression :: JSParser JSExpression
primaryExpression = ... -- recursive descent with no depth limit
```

## Solution

### 1. Input Size Limits

Add size validation at the three API entry points in `Parser.hs`:

```haskell
-- | Maximum allowed input size (10 MB).
maxInputSize :: Int
maxInputSize = 10 * 1024 * 1024

parseBS :: ByteString -> Either String AST.JSAST
parseBS input
  | BS.length input > maxInputSize =
      Left ("Input too large: " <> show (BS.length input)
            <> " bytes (maximum: " <> show maxInputSize <> ")")
  | otherwise = handleResult (FlatParser.parseProgramByteString input)
```

Apply same pattern to `parseModuleBS`, `parseText`, `parseModuleText`.
The `String`-based `parse`/`parseModule` convert to Text then ByteString, so they'll hit the Text path guard.

### 2. Nesting Depth Limits

Add a depth counter to the parser state in `Core.hs`:

```haskell
-- In the parser state or as a Reader:
maxNestingDepth :: Int
maxNestingDepth = 512

-- Guard function for Grammar.hs recursive rules:
withNesting :: JSParser a -> JSParser a
withNesting inner = do
  depth <- getDepth
  if depth >= maxNestingDepth
    then FP.err (NestingTooDeep depth)
    else withIncrementedDepth inner
```

Apply `withNesting` to:
- `parenthesizedExpression` (most common nesting vector)
- `arrayLiteral`
- `objectLiteral`
- `blockStatement`
- `functionExpression`/`arrowFunction`

### 3. UTF-8 Validation

The ByteString path should validate UTF-8 before parsing:

```haskell
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

parseBS :: ByteString -> Either String AST.JSAST
parseBS input
  | BS.length input > maxInputSize = Left "Input too large"
  | not (isValidUtf8 input) = Left "Invalid UTF-8 encoding"
  | otherwise = handleResult (FlatParser.parseProgramByteString input)
  where
    isValidUtf8 bs = case Text.decodeUtf8' bs of
      Right _ -> True
      Left _ -> False
```

## Files Changed

- `src/Language/JavaScript/Parser/Parser.hs` — size + UTF-8 validation at entry points
- `src/Language/JavaScript/Parser/Core.hs` — nesting depth tracking in parser state
- `src/Language/JavaScript/Parser/Grammar.hs` — `withNesting` guards on recursive productions
- New tests: `test/Unit/Language/Javascript/Parser/Validation/InputLimits.hs`

## Verification

1. `cabal test` — all existing tests pass (inputs are small, well-formed)
2. New test: 11 MB input → `Left "Input too large"`
3. New test: `(((((...))))` 600 levels deep → `Left "Nesting too deep"`
4. New test: invalid UTF-8 bytes → `Left "Invalid UTF-8 encoding"`
5. Benchmark: verify no regression for normal inputs (size check is O(1))
