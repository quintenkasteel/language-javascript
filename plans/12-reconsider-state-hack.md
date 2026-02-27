# Plan 12: Reconsider `-fno-state-hack` GHC Flag

## Summary

The cabal file includes `-fno-state-hack` in GHC options. This disables a GHC optimization that can improve throughput of monadic/stateful code (like parsers) but can also cause space leaks. Profile with and without to determine the optimal setting.

## Current Setting

**language-javascript.cabal:84:**
```
ghc-options: -Wall -fwarn-tabs -O2 -funbox-strict-fields -fspec-constr-count=6 -fno-state-hack ...
```

## What the State Hack Does

The GHC "state hack" eta-expands functions that take a `State#` token (i.e., IO/ST/monadic functions) to expose more inlining opportunities. This generally improves performance of monadic code by enabling more aggressive optimization.

**Downside:** Can cause space leaks when closures capture large data that should have been garbage collected.

## Plan

### Step 1: Benchmark with `-fno-state-hack` (current)
```bash
cabal clean && cabal build -O2
cabal test --test-options="--match Performance"
```
Record parse times for jQuery, React, Angular benchmarks.

### Step 2: Benchmark without `-fno-state-hack`
Temporarily remove the flag, rebuild:
```bash
cabal clean && cabal build -O2
cabal test --test-options="--match Performance"
```
Record parse times.

### Step 3: Profile memory with both settings
```bash
cabal run language-javascript -- +RTS -s -RTS < large-file.js
```
Compare peak memory usage and GC activity.

### Step 4: Decision
- If removing `-fno-state-hack` improves throughput without increasing peak memory: **remove it**
- If it causes measurable space leaks: **keep it**
- If negligible difference: **remove it** (fewer custom flags = simpler)

## Files Changed

| File | Change |
|------|--------|
| `language-javascript.cabal:84` | Potentially remove `-fno-state-hack` |

## Verification

- Benchmark results documented
- No regression in either throughput or memory usage
