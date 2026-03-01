# Plan 11: Register Missing Test Modules in Cabal File

**Priority**: P1 (Significant — tests exist but don't run)
**Effort**: Small (30 minutes)
**Risk**: Low (additive change)

## Problem

There are test modules on disk that are not registered in the `.cabal` file's test suite, meaning they compile but are never executed by `cabal test`.

## Investigation Needed

Compare the list of test `.hs` files on disk with the modules listed in `language-javascript.cabal`:

```bash
# All test modules on disk
find test/ -name "*.hs" | sort

# Modules registered in cabal
grep -A 500 "test-suite" language-javascript.cabal | grep "other-modules\|hs-source-dirs"
```

### Known Candidates (from audit)

The audit identified 4 unregistered test modules. Need to verify which ones by checking the cabal file.

## Solution

1. Read the cabal file's test suite section
2. Compare against actual test files on disk
3. For each missing module:
   - Add to `other-modules` in the test suite
   - Ensure it's imported and called from the test runner (`test/Main.hs` or `test/Spec.hs`)
4. If the module defines a `Spec` or test function, wire it into the runner

## Files Changed

- `language-javascript.cabal` — add missing test modules
- Test runner file (e.g., `test/Spec.hs`) — import and call the new test modules

## Verification

1. `cabal build test:language-javascript-test` — compiles with new modules
2. `cabal test` — all tests pass, including newly registered ones
3. Verify test count increased from 1488
