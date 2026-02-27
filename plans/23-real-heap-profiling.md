# Plan 23: Add Real Heap Profiling to Benchmarks

## Summary

The current memory benchmark uses fake estimation (`let estimatedMemory = inputSize * 12`), not actual heap measurement. Replace with real GHC runtime statistics.

## Current Problem

**`test/Benchmarks/.../Memory.hs`:**
```haskell
let estimatedMemory = inputSize * 12  -- constant factor, not real measurement
```

This tells us nothing about actual memory behavior.

## Plan

### Option A: Use `weigh` Library

The `weigh` library measures actual heap allocations:

```haskell
import Weigh

main :: IO ()
main = mainWith $ do
  func "parse small JS" parseSmall smallInput
  func "parse medium JS" parseMedium mediumInput
  func "parse large JS" parseLarge largeInput
```

### Option B: Use GHC RTS Statistics

```haskell
import GHC.Stats
import System.Mem (performGC)

measureMemory :: String -> IO (Int, Int)
measureMemory input = do
  performGC
  before <- getRTSStats
  let !ast = parse input "bench"
  performGC
  after <- getRTSStats
  let allocated = allocated_bytes after - allocated_bytes before
      peakMem = max_live_bytes after
  pure (fromIntegral allocated, fromIntegral peakMem)
```

### Option C (Recommended): Use Criterion with `nfIO`

The existing `criterionBenchmarks` definition exists but is never called. Wire it into a separate benchmark executable:

```cabal
benchmark parser-bench
    type: exitcode-stdio-1.0
    main-is: Bench.hs
    build-depends: base, language-javascript, criterion, bytestring, text, deepseq
    ghc-options: -O2 -rtsopts
```

```haskell
-- test/Bench.hs
import Criterion.Main
import Language.JavaScript.Parser

main :: IO ()
main = defaultMain
  [ bgroup "parse"
      [ bench "small (1KB)" $ nf (parse "src") smallJS
      , bench "medium (100KB)" $ nf (parse "src") mediumJS
      , bench "large (1MB)" $ nf (parse "src") largeJS
      ]
  , bgroup "render"
      [ bench "small" $ nf renderToString smallAST
      , bench "large" $ nf renderToString largeAST
      ]
  ]
```

## Files Changed

| File | Change |
|------|--------|
| `language-javascript.cabal` | Add `benchmark` stanza |
| `test/Bench.hs` or `bench/Main.hs` | New benchmark executable |
| `test/Benchmarks/.../Memory.hs` | Replace fake estimation with real measurement |
| `test/Benchmarks/.../Performance.hs` | Wire Criterion benchmarks to be callable |

## Verification

- `cabal bench` runs and produces real allocation/timing numbers
- Memory measurements reflect actual heap usage, not estimates
- Results are reproducible across runs (within expected variance)
