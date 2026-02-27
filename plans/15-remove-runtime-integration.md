# Plan 15: Remove Runtime.Integration Demo Module

## Summary

`Language.JavaScript.Runtime.Integration` is a demo/showcase module that performs `IO` actions, prints to stdout with emoji characters, and demonstrates runtime validation features. This is inappropriate for a parser library.

## Current State

The module contains functions like:
- `validateRuntimeCall` — prints validation results to stdout
- Various demo functions with `putStrLn` and emoji output
- `IO`-based functions that don't belong in a pure parser library

## Changes

### Option A (Recommended): Remove entirely

1. Delete `src/Language/JavaScript/Runtime/Integration.hs`
2. Remove from `exposed-modules` in `language-javascript.cabal`
3. Remove any imports of this module from test files

### Option B: Move to examples/

If the code has educational value, move it to an `examples/` directory outside the library.

## Files Changed

| File | Change |
|------|--------|
| `src/Language/JavaScript/Runtime/Integration.hs` | Delete |
| `language-javascript.cabal` | Remove from exposed-modules |
| Any test files importing it | Remove import |

## Verification

- Module no longer importable by library consumers
- `cabal build` succeeds
- `cabal test` passes
