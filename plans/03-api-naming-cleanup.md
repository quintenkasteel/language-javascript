# Plan 03: API Naming and Export Cleanup

**Priority**: P1 (Significant — confusing public API)
**Effort**: Medium (3-4 hours)
**Risk**: Medium (breaking changes for consumers, needs major version bump)

## Problem

The public API in `Language.JavaScript.Parser` has several issues:

### 1. Abbreviation: `parseBS` is unclear
```haskell
parseBS :: ByteString -> Either String AST.JSAST
-- "BS" means ByteString but reads as "parse bullshit" to newcomers
```

### 2. Duplicate: `parseProgram` ≡ `parse`
```haskell
-- Parser.hs:236
parseProgram input srcName = parse input srcName  -- Exact duplicate!
```

### 3. Three conflicting error types exposed
```haskell
-- String-based API returns Either String
parse :: String -> String -> Either String JSAST

-- Core API returns structured ParseResult
parseProgramByteString :: ByteString -> ParseResult JSAST

-- File API throws IO exceptions
parseFile :: FilePath -> IO JSAST  -- calls 'fail' on error!
```

### 4. Deprecated functions still exported
```haskell
readJsSafe      -- deprecated, delegates to parse
readJsModuleSafe -- deprecated, delegates to parseModule
```

### 5. Quasi-quoter submodules exposed unnecessarily
```haskell
-- Re-exported in main module but also available as:
Language.JavaScript.QQ (js, jsast, jsx)
```

## Solution

### Phase 1: Add Better Names (Non-breaking)

In `Parser.hs`, add properly named aliases:

```haskell
-- | Parse JavaScript from a UTF-8 ByteString.
-- Preferred over 'parseBS' for clarity.
parseByteString :: ByteString -> Either String AST.JSAST
parseByteString = parseBS

-- | Parse a JavaScript module from a UTF-8 ByteString.
parseModuleByteString :: ByteString -> Either String AST.JSAST
parseModuleByteString = parseModuleBS
```

In `Language.JavaScript.Parser`, export both names:

```haskell
-- * ByteString Parsing
PA.parseByteString,
PA.parseModuleByteString,
PA.parseBS,         -- kept for backward compat
PA.parseModuleBS,   -- kept for backward compat
```

### Phase 2: Deprecate Old Names

```haskell
{-# DEPRECATED parseBS "Use 'parseByteString' instead." #-}
{-# DEPRECATED parseModuleBS "Use 'parseModuleByteString' instead." #-}
{-# DEPRECATED parseProgram "Use 'parse' instead (identical function)." #-}
```

### Phase 3: Unify Error Types

Create a migration path toward `ParseResult` everywhere:

```haskell
-- New: Structured error versions of String-based API
parseStructured :: String -> String -> ParseResult JSAST
parseModuleStructured :: String -> String -> ParseResult JSAST

-- New: Safe file parsing (no IO exceptions)
parseFileSafe :: FilePath -> IO (Either String JSAST)
parseFileUtf8Safe :: FilePath -> IO (Either String JSAST)
```

### Phase 4: Clean Up Exports

Move quasi-quoters to a separate section with a note:

```haskell
-- * Quasi-quoters (require TemplateHaskell)
-- | These are also available from "Language.JavaScript.QQ".
js, jsast, jsx,
```

## Migration Guide (for CHANGELOG)

```
## Migration from 0.8.x to 0.9.0

- `parseBS` → `parseByteString`
- `parseModuleBS` → `parseModuleByteString`
- `parseProgram` → `parse` (always was identical)
- `readJsSafe` → `parse input "src"`
- `readJsModuleSafe` → `parseModule input "src"`
- `parseFile` → `parseFileSafe` (returns Either instead of throwing)
```

## Files Changed

- `src/Language/JavaScript/Parser/Parser.hs` — add aliases, deprecations
- `src/Language/JavaScript/Parser.hs` — update export list
- `CHANGELOG.md` — migration guide

## Verification

1. `cabal build` — zero warnings (deprecation warnings expected only in internal uses)
2. `cabal test` — all tests pass
3. Check haddock renders correctly: `cabal haddock`
4. Verify all three API tiers work: String, Text, ByteString
