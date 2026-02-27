# Plan 11: Remove Unnecessary Dependencies

## Summary

The `language-javascript.cabal` includes several heavy dependencies that appear unused or underused by the core parser. Removing them reduces build time, install footprint, and attack surface.

## Candidates

### 1. `hashtables >= 1.2`

**Status:** Listed as dependency but no import of `Data.HashTable` found in any source file.

**Action:** Remove from `build-depends`.

### 2. `lens >= 4.0`

**Status:** Used only in `src/Language/JavaScript/Process/TreeShake/Types.hs` for `makeLenses`. This is a very heavy dependency (pulls in ~30 transitive packages) for a single `makeLenses` call.

**Options:**
- **A (Recommended):** Replace `lens` with `microlens-th` (~3 transitive deps) which provides `makeLenses` without the full lens ecosystem
- **B:** Hand-write the lenses (only a few fields in `TreeShakeState` and `TreeShakeOptions`)
- **C:** Keep `lens` if tree shaking is a first-class feature

**Action:** Replace with `microlens-th >= 0.4` + `microlens >= 0.4`.

### 3. `syb >= 0.7`

**Status:** Used only for `fixPositions` in `Flatparse/Parser.hs` (`Data.Generics.everywhere`, `mkT`).

**Action:** Remove after Plan 07 (remove fixPositions). Also check if TreeShake tests or other modules use SYB.

### 4. `time >= 1.4`

**Status:** Check if any source file imports `Data.Time`. If only used in benchmarks/tests, move to test dependency only.

**Action:** Verify usage and move or remove.

### 5. `utf8-string >= 0.3.7`

**Status:** Used in Printer.hs for `Codec.Binary.UTF8.String.decode`. After Plan 06 (ByteString AST), this may no longer be needed since ByteString is already UTF-8.

**Action:** Remove after Plan 06 if no longer imported.

## Research Steps

1. `grep -rn 'import.*HashTable\|import.*Data\.HashTable' src/` — verify hashtables unused
2. `grep -rn 'import.*Control\.Lens\|import.*Lens' src/` — find all lens usage
3. `grep -rn 'import.*Data\.Generics\|import.*Data\.Data' src/` — find all SYB usage
4. `grep -rn 'import.*Data\.Time' src/` — find time usage
5. `grep -rn 'import.*Codec\.Binary\.UTF8' src/` — find utf8-string usage

## Files Changed

| File | Change |
|------|--------|
| `language-javascript.cabal` | Remove/replace dependencies |
| `src/.../TreeShake/Types.hs` | Change `import Control.Lens` to `import Lens.Micro.TH` |

## Verification

- `cabal build` succeeds with reduced dependencies
- `cabal test` passes
- Package install pulls fewer transitive dependencies
