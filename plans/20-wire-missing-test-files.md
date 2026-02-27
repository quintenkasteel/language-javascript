# Plan 20: Wire Missing Test Files Into Test Suite

## Summary

11 test files exist on disk but are never imported in `testsuite.hs`. 6 more are commented out. Assess each and either wire them in or delete them.

## Files Never Imported (11)

| File | Assessment | Action |
|------|-----------|--------|
| `Flatparse/LexerTest.hs` | All 30 tests pending, but tests the real Flatparse lexer | Wire in after un-pending (Plan 03) |
| `Flatparse/ExpressionTest.hs` | All 42 tests pending + 30 stub predicates | Delete and rewrite (Plan 03) |
| `Flatparse/Test.hs` | Smoke tests for Pos, AST, Primitives | Wire in — these are quick sanity checks |
| `Flatparse/ModernTest.hs` | Tests Modern.hs which isn't used by main parser | Delete — tests a dead code path |
| `TreeShake/ModernJS.hs` | Tree shake tests for modern JS patterns | Wire in if tests compile and pass |
| `Fuzz/FuzzTest.hs` | Fuzz testing integration | Wire in if meaningful tests exist |
| `Fuzz/FuzzGenerators.hs` | QuickCheck generators (support module) | Not a test runner — keep as support |
| `Fuzz/DifferentialTesting.hs` | Differential testing vs other parsers | Wire in if tests are real (not stubs) |
| `Fuzz/FuzzHarness.hs` | Has 2 stub functions | Fix stubs, then wire in |
| `Fuzz/CoverageGuided.hs` | Coverage-guided fuzzing | Wire in if functional |
| `Generators.hs` | QuickCheck generators (support module) | Not a test runner — keep as support |

## Files Commented Out (6)

| File | Reason | Action |
|------|--------|--------|
| `AdvancedFeatures` | "constructor issues" | Fix constructor issues, uncomment |
| `TreeShake/Usage` | No reason | Verify compiles, uncomment |
| `TreeShake/Elimination` | No reason | Verify compiles, uncomment |
| `QQ/Validate` | "flatparse+TH compatibility" | Test if TH works with Flatparse, uncomment or delete |
| `QQ/Compile` | Same | Same |
| `QQ/Antiquote` | Same | Same |

## Process Per File

1. Add import to `testsuite.hs`
2. Try `cabal build test:testsuite`
3. If compilation fails: fix errors or delete file
4. If compilation succeeds: run `cabal test`
5. If tests pass: keep
6. If tests fail with real failures: investigate and fix
7. If tests fail because they test dead code: delete

## Changes

| File | Change |
|------|--------|
| `test/testsuite.hs` | Add/uncomment ~8-12 imports |
| Various test files | Fix compilation errors as needed |

## Verification

- Every `.hs` file under `test/` is either imported in `testsuite.hs` or deleted
- `cabal test` passes with all wired-in tests
- No orphaned test files exist
