# Plan 07: Remove Deprecated Functions and Clean Exports

**Priority**: P2 (Enhancement — API hygiene)
**Effort**: Small (1-2 hours)
**Risk**: Medium (breaking for anyone using deprecated functions)

## Problem

The public API still exports deprecated functions that add confusion:

### Deprecated Functions Still Exported

```haskell
-- Parser.hs:178-188
{-# DEPRECATED readJsSafe "Use 'parse' instead." #-}
readJsSafe :: String -> Either String AST.JSAST
readJsSafe input = parse input "src"

{-# DEPRECATED readJsModuleSafe "Use 'parseModule' instead." #-}
readJsModuleSafe :: String -> Either String AST.JSAST
readJsModuleSafe input = parseModule input "src"
```

### Duplicate Function Exported

```haskell
-- Parser.hs:229-236
parseProgram :: String -> String -> Either String AST.JSAST
parseProgram input srcName = parse input srcName
-- This is literally just `parse` with a different name
```

### `parseFile` Uses `fail` (Throws IO Exception)

```haskell
-- Parser.hs:194-197
parseFile :: FilePath -> IO AST.JSAST
parseFile filename = do
  x <- readFile filename
  either fail pure (parse x filename)  -- 'fail' throws IOException!
```

This is surprising behavior — users expect `IO AST.JSAST` to not throw parse-related exceptions.

## Solution

### Step 1: Add Safe File Parsing

```haskell
-- | Parse a JavaScript file, returning structured errors.
--
-- Unlike 'parseFile', this function does not throw exceptions on
-- parse failure. IO exceptions (file not found, permission denied)
-- are still possible.
parseFileSafe :: FilePath -> IO (Either String AST.JSAST)
parseFileSafe filename = do
  x <- readFile filename
  pure (parse x filename)

-- | Parse a JavaScript file with explicit UTF-8 encoding.
parseFileUtf8Safe :: FilePath -> IO (Either String AST.JSAST)
parseFileUtf8Safe filename = do
  h <- openFile filename ReadMode
  hSetEncoding h utf8
  x <- hGetContents h
  pure (parse x filename)
```

### Step 2: Deprecate Unsafe File Parsing

```haskell
{-# DEPRECATED parseFile "Use 'parseFileSafe' instead (does not throw on parse error)." #-}
{-# DEPRECATED parseFileUtf8 "Use 'parseFileUtf8Safe' instead (does not throw on parse error)." #-}
```

### Step 3: Update Exports in Parser.hs

```haskell
module Language.JavaScript.Parser.Parser
  ( -- * String-based Parsing
    parse,
    parseModule,

    -- * ByteString Parsing (zero-copy)
    parseBS,
    parseModuleBS,

    -- * Text Parsing
    parseText,
    parseModuleText,

    -- * File Parsing (safe)
    parseFileSafe,
    parseFileUtf8Safe,

    -- * Expression and Statement Parsing
    parseExpression,
    parseStatement,

    -- * Display
    showStripped,
    showStrippedMaybe,

    -- * Deprecated
    readJsSafe,
    readJsModuleSafe,
    parseProgram,
    parseFile,
    parseFileUtf8,
  ) where
```

### Step 4: Update Main Module Exports

Update `Language.JavaScript.Parser` to match, with the deprecated section clearly separated.

### Step 5: Update Internal Uses

Search all test files and internal code for deprecated function usage and migrate:

```bash
grep -r "readJsSafe\|readJsModuleSafe\|parseProgram\b" test/ src/
```

## Files Changed

- `src/Language/JavaScript/Parser/Parser.hs` — add safe file functions, deprecations
- `src/Language/JavaScript/Parser.hs` — update export list
- Test files using deprecated functions — migrate to new names

## Verification

1. `cabal build` — zero errors (deprecation warnings expected in test files if not migrated)
2. `cabal test` — all tests pass
3. Verify no internal code uses deprecated functions
