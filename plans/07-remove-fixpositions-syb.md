# Plan 07: Remove `fixPositions` SYB Traversal

## Summary

Replace the post-parse `fixPositions` SYB generic traversal with inline position computation during parsing. This eliminates a full AST walk using runtime type dispatch after every parse.

## Current Architecture

### Two-Phase Position Strategy

**Phase 1 (During parsing):** `FP.getPos` returns `FP.Pos` (remaining bytes from end of input). Stored as placeholder `TokenPn remainingBytes 0 0` via `fpPosToAnnot`.

**Phase 2 (Post-parse):** `fixPositions` walks the entire AST with SYB `everywhere (mkT fixTokenPosn)`, converting each placeholder to proper `TokenPn offset line col`.

### Current Code

**Grammar.hs:124-128:**
```haskell
fpPosToAnnot :: FP.Pos -> JSAnnot
fpPosToAnnot fpPos = JSAnnot (TokenPn (FP.unPos fpPos) 0 0) []
```

**Parser.hs:318-331:**
```haskell
fixPositions :: Data a => ByteString -> a -> a
fixPositions input = everywhere (mkT fixTokenPosn)
  where
    inputLen = BS.length input
    lineStarts = buildLineStarts input
    fixTokenPosn (TokenPn remainingBytes 0 0)
      | remainingBytes > 0 = let offset = inputLen - remainingBytes
                                  (line, col) = offsetToLineCol lineStarts offset
                              in TokenPn offset line col
    fixTokenPosn tp = tp
```

### Call Sites (4)
- `Parser.hs:170` — `parseProgramByteString`
- `Parser.hs:202` — `parseModuleProgramByteString`
- `Parser.hs:218` — `parseExpressionByteString`
- `Parser.hs:240` — `parseStatement`

### Scale
- 118 `FP.getPos` calls in Grammar.hs
- 133 `fpPosToAnnot` uses in Grammar.hs
- 57 `defaultAnnot` uses (synthetic positions, not fixed)

## Approach: FlatParse.Stateful with Reader Parameter

### Step 1: Switch from FlatParse.Basic to FlatParse.Stateful

**File:** `src/Language/JavaScript/Parser/Flatparse/Primitives.hs`

```haskell
-- Before:
import FlatParse.Basic as FP
type JSParser = FP.Parser ByteString

-- After:
import FlatParse.Stateful as FP
type JSParser = FP.ParserT PureMode LineInfo ByteString

data LineInfo = LineInfo
  { liInputLen :: !Int
  , liLineStarts :: !(VU.Vector Int)
  }
```

The `r` reader parameter in `FlatParse.Stateful.ParserT PureMode r e a` carries immutable data accessible during parsing. `LineInfo` is computed once before parsing starts.

### Step 2: Change `fpPosToAnnot` to Convert Inline

**File:** `src/Language/JavaScript/Parser/Flatparse/Grammar.hs`

```haskell
-- Before:
fpPosToAnnot :: FP.Pos -> JSAnnot
fpPosToAnnot fpPos = JSAnnot (TokenPn (FP.unPos fpPos) 0 0) []

-- After:
fpPosToAnnot :: FP.Pos -> JSParser JSAnnot
fpPosToAnnot fpPos = do
  LineInfo inputLen lineStarts <- FP.ask  -- read the LineInfo from reader
  let remainingBytes = FP.unPos fpPos
      offset = inputLen - remainingBytes
      (line, col) = offsetToLineCol lineStarts offset
  pure (JSAnnot (TokenPn offset line col) [])
```

**Note:** `fpPosToAnnot` changes from a pure function to a parser action. This requires updating all 133 call sites from:
```haskell
pure (JSFoo (fpPosToAnnot pos) ...)
```
to:
```haskell
annot <- fpPosToAnnot pos
pure (JSFoo annot ...)
```

Since all call sites are already inside `do` blocks, this is mechanical.

### Step 3: Update Parser Entry Points

**File:** `src/Language/JavaScript/Parser/Flatparse/Parser.hs`

```haskell
-- Before:
parseProgramByteString input =
  case FP.runParser program input of
    OK result remaining -> ParseOK (ParseSuccess (fixPositions input result) remaining ...)
    ...

-- After:
parseProgramByteString input =
  let lineInfo = LineInfo (BS.length input) (buildLineStarts input)
  in case FP.runParserT program lineInfo input of
    OK result remaining -> ParseOK (ParseSuccess result remaining ...)  -- no fixPositions!
    ...
```

### Step 4: Remove Dead Code

Remove from `Parser.hs`:
- `fixPositions` function (lines 318-331)
- `import Data.Generics (everywhere, mkT)` (line 82)

Remove from `language-javascript.cabal`:
- `syb >= 0.7` dependency (if no other module uses it)

### Step 5: Handle `defaultAnnot`

`defaultAnnot` remains unchanged — it's `JSAnnot (TokenPn 0 0 0) []` for synthetic positions. Since these have `remainingBytes = 0`, they were already skipped by `fixPositions`. No change needed.

## Alternative: Use FlatParse's `posLineCols`

FlatParse provides `posLineCols :: ByteString -> [Pos] -> [(Int, Int)]` which batch-converts positions. This still requires a post-parse traversal to collect and apply positions, so it doesn't eliminate the SYB walk. The inline approach is cleaner.

## Impact on Grammar.hs

All 133 sites follow the same mechanical transformation. Example:

```haskell
-- Before:
identName :: JSParser JSIdent
identName = do
  pos <- FP.getPos
  name <- identifier
  pure (JSIdentName (fpPosToAnnot pos) (Text.unpack name))

-- After:
identName :: JSParser JSIdent
identName = do
  pos <- FP.getPos
  annot <- fpPosToAnnot pos
  name <- identifier
  pure (JSIdentName annot name)
```

## Files Changed

| File | Nature of Change |
|------|-----------------|
| `src/.../Flatparse/Primitives.hs` | Switch to `FlatParse.Stateful`, define `LineInfo`, update `JSParser` type |
| `src/.../Flatparse/Grammar.hs` | Change `fpPosToAnnot` to parser action, update 133 call sites |
| `src/.../Flatparse/Parser.hs` | Remove `fixPositions`, update `runParser` → `runParserT`, build `LineInfo` |
| `src/.../Flatparse/Lexer.hs` | Update type if it uses `JSParser` (should be compatible) |
| `src/.../Flatparse/Expression.hs` | Re-export updated types |
| `src/.../Flatparse/Statement.hs` | Re-export updated types |
| `src/.../Flatparse/Modern.hs` | Same pattern — update `fpPosToAnnot` calls |
| `language-javascript.cabal` | Remove `syb` dependency if unused elsewhere |

## Verification

- `grep -rn 'fixPositions' src/` returns 0 results
- `grep -rn 'everywhere\|mkT\|Data\.Generics' src/Language/JavaScript/Parser/Flatparse/` returns 0 results
- `cabal build` compiles without warnings
- `cabal test` passes
- Parse results have correct line/column numbers (spot-check with known inputs)
- Benchmark shows measurable improvement for large files (no SYB traversal overhead)
