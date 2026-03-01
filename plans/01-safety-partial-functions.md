# Plan 01: Eliminate Partial Functions in Minify.hs

**Priority**: P0 (Blocking — runtime crash risk)
**Effort**: Small (1-2 hours)
**Risk**: Low (localized changes with clear semantics)

## Problem

`src/Language/JavaScript/Process/Minify.hs` uses 8 partial functions that crash on empty `ByteString` input:

| Line | Call | Crash Condition |
|------|------|-----------------|
| 254 | `BS8.init xs` | `BS.null xs` (guarded, but `xs` could be `"'"` → single byte → init = empty) |
| 254 | `BS8.init (BS.drop 1 ys)` | `ys` is 1 byte |
| 261 | `BS8.head str` | `str` is empty (guarded line 260) |
| 262 | `BS8.head str` | Same guard |
| 268 | `BS8.head bs` | `bs` empty (guarded line 266) |
| 269 | `BS8.head bs` | Same guard |
| 270 | `BS8.head bs` | Same guard (duplicated in `otherwise`) |
| 273 | `BS8.head rest` | `rest` empty (guarded line 272) |

Also in `src/Language/JavaScript/Parser/Literals.hs:178`:
- `BS8.init bs` in `parseBigIntValue` — strips trailing `n` from BigInt literal

## Root Cause

The code was written with implicit assumptions about ByteString length but does not use safe access patterns. While some calls are guarded by `BS.null` checks, the `otherwise` branches and `BS8.init` calls are still vulnerable to edge cases.

## Solution

Replace all `BS8.head` with `BS.uncons` pattern matching and `BS8.init` with safe slicing.

### Minify.hs Changes

**`normalizeToSQ`** (lines 258-274):
```haskell
normalizeToSQ :: ByteString -> ByteString
normalizeToSQ str = case BS.uncons str of
  Nothing -> str
  Just (w, _)
    | w == fromIntegral (fromEnum '\'') -> str
    | w == fromIntegral (fromEnum '"') -> BS8.cons '\'' (convertSQ (BS.drop 1 str))
    | otherwise -> str
  where
    convertSQ bs = case BS.uncons bs of
      Nothing -> BS.empty
      Just _ | BS.length bs == 1 -> "'"
      Just (w, rest)
        | w == fromIntegral (fromEnum '\'') -> "\\'" <> convertSQ rest
        | w == fromIntegral (fromEnum '\\') -> handleEscape rest
        | otherwise -> BS.cons w (convertSQ rest)
    handleEscape rest = case BS.uncons rest of
      Nothing -> "\\"
      Just (w, rest')
        | w == fromIntegral (fromEnum '"') -> BS.cons w (convertSQ rest')
        | otherwise -> BS8.cons '\\' (convertSQ rest)
```

**`stringLitConcat`** (line 254):
```haskell
-- Use BS.take (BS.length xs - 1) instead of BS8.init
| otherwise = JSStringLiteral emptyAnnot
    (BS.take (BS.length xs - 1) xs <> BS.take (BS.length ys - 2) (BS.drop 1 ys) <> "'")
```

Or better: use `BS.unsnoc` (available since bytestring-0.11) or manual safe slicing.

### Literals.hs Change

**`parseBigIntValue`** (line 178):
```haskell
parseBigIntValue bs = classifyAndParse (BS.take (BS.length bs - 1) bs)
```

## Verification

1. `cabal build` — zero warnings
2. `cabal test` — all 1488 tests pass
3. Manual test: parse `""` (empty string literal) through minifier
4. Manual test: minify `''` (empty single-quoted string)
5. QuickCheck: generate random ByteStrings, feed to `normalizeToSQ`, no crashes

## Files Changed

- `src/Language/JavaScript/Process/Minify.hs` (lines 248-274)
- `src/Language/JavaScript/Parser/Literals.hs` (line 178)
