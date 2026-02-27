# Plan 17: Re-enable Suppressed GHC Warnings

## Summary

The library suppresses 4 GHC warnings that mask dead code and potential bugs. Re-enable them and fix the underlying issues.

## Current Setting

**language-javascript.cabal:84:**
```
ghc-options: -Wall ... -Wno-unused-imports -Wno-unused-top-binds -Wno-unused-matches -Wno-name-shadowing
```

## Plan

### Step 1: Remove suppressions one at a time

#### `-Wno-unused-imports`
Remove, rebuild, fix all unused import warnings. These indicate dead imports that should be cleaned up.

#### `-Wno-unused-top-binds`
Remove, rebuild, fix all unused top-level binding warnings. These indicate dead code that should be removed or exported.

#### `-Wno-unused-matches`
Remove, rebuild, fix all unused match warnings. Replace unused bindings with `_`:
```haskell
-- Before (warns):
parseToken tok pos = ...  -- pos unused

-- After:
parseToken tok _pos = ...
```

#### `-Wno-name-shadowing`
Remove, rebuild, fix all name shadowing warnings. Rename shadowed variables to be unique:
```haskell
-- Before (warns):
f x = let x = x + 1 in x  -- x shadows x

-- After:
f x = let x' = x + 1 in x'
```

### Step 2: Add `-Werror` for CI (optional)

Consider adding `-Werror` to the test suite GHC options to catch warnings in CI:
```cabal
test-suite testsuite
    ghc-options: -Wall -Werror
```

## Files Changed

| File | Change |
|------|--------|
| `language-javascript.cabal` | Remove 4 `-Wno-*` flags |
| Various `src/*.hs` files | Fix unused imports, bindings, matches, shadowing |

## Verification

- `cabal build` compiles with zero warnings
- `cabal test` passes
- No `-Wno-*` suppressions remain in cabal file (except `-Wno-orphans` if needed for test instances)
