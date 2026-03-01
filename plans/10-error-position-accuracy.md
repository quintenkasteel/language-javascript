# Plan 10: Fix Error Position Reporting

**Priority**: P1 (Significant — misleading error messages)
**Effort**: Medium (3-4 hours)
**Risk**: Medium (changes error output, golden tests need updating)

## Problem

Parse errors report position `(1,1)` regardless of where the actual error occurs in the source. This makes the error messages nearly useless for anything beyond trivial inputs.

### Current Behavior

```haskell
-- Parsing "var x = ;" (missing expression at column 9)
-- Expected: "test:1:9: Expected expression"
-- Actual:   "test: unexpected input"  (no position at all)
--   or:     "test:1:1: ..."           (wrong position)
```

### Root Cause

The flatparse-based `Core.hs` parser tracks byte offsets internally but the conversion to human-readable `(line, column)` positions has issues:

1. `FP.Err` carries a `ParseFailure` with a byte offset, but `formatParseError` may not translate it correctly
2. `FP.Fail` (soft failure) carries no position information at all
3. The `prependSrcName` function in `Parser.hs` just prepends the filename without position info

### In Lexer.hs

The `error` positions appear to be hardcoded or default to `(1,1)`:
```haskell
-- Need to investigate: where does the (1,1) come from?
-- FlatParse tracks byte offset but line/column conversion
-- may not account for newlines correctly
```

## Solution

### 1. Improve Error Position Tracking in Core.hs

Ensure `ParseFailure` includes the correct byte offset:

```haskell
data ParseFailure = ParseFailure
  { failureError  :: !ParseError
  , failureInput  :: !ByteString   -- original full input
  , failureOffset :: !Int          -- byte offset where error occurred
  }
```

### 2. Correct Line/Column Calculation

```haskell
-- | Convert byte offset to (line, column) by counting newlines.
offsetToLineCol :: ByteString -> Int -> (Int, Int)
offsetToLineCol input offset =
  let prefix = BS.take offset input
      lines = BS8.count '\n' prefix
      lastNewline = maybe 0 (+1) (BS8.elemIndexEnd '\n' prefix)
      col = offset - lastNewline
  in (lines + 1, col + 1)
```

### 3. Improve Error Message Formatting

```haskell
formatParseError :: ParseFailure -> Text
formatParseError (ParseFailure err input offset) =
  let (line, col) = offsetToLineCol input offset
  in Text.pack (show line) <> ":" <> Text.pack (show col) <> ": " <> renderError err
```

### 4. Update Parser.hs to Include Position

```haskell
prependSrcName :: String -> String -> String
prependSrcName srcName msg = srcName <> ":" <> msg
-- Error already contains line:col from formatParseError
```

### 5. Handle FP.Fail (No Error Info)

When FlatParse returns `Fail` (not `Err`), we need to at least report the furthest position reached:

```haskell
-- Track furthest position in parser state for better error recovery
```

## Investigation Needed

Before implementing, verify:
1. What does `FP.getPos` return? (byte offset or position object?)
2. Does `FP.Err` preserve the correct offset where the error occurred?
3. Are there nested parser calls that lose position information?
4. Does the current `formatParseError` already handle line/column?

```bash
# Check current error formatting
grep -n "formatParseError\|offsetToLineCol\|failureOffset" src/Language/JavaScript/Parser/Core.hs
```

## Files Changed

- `src/Language/JavaScript/Parser/Core.hs` — error position calculation
- `src/Language/JavaScript/Parser/Parser.hs` — error message formatting
- `test/Golden/**/*.golden` — update golden files with new error positions
- `test/Unit/Language/Javascript/Parser/Error/*.hs` — update error expectations

## Verification

1. `cabal build` — compiles
2. `cabal test` — update golden files, then all tests pass
3. Manual test: `parse "var x = ;" "test"` → error includes `1:9`
4. Manual test: multiline input → error includes correct line number
5. Manual test: Unicode input → column numbers are correct (byte vs char?)
