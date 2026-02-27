# Plan 16: Add Upper Bounds to All Dependencies

## Summary

Most dependencies in `language-javascript.cabal` lack upper bounds. This risks broken builds when dependencies release breaking changes. Add upper bounds following PVP (Package Versioning Policy).

## Current Dependencies (library)

```cabal
build-depends:
    base >= 4 && < 5,
    bytestring >= 0.9.1,          -- no upper bound
    containers >= 0.2,             -- no upper bound
    text >= 0.11,                  -- no upper bound
    array >= 0.3,                  -- no upper bound
    mtl >= 1.1,                    -- no upper bound
    blaze-builder >= 0.2,          -- no upper bound
    utf8-string >= 0.3.7 && < 2,  -- has upper bound
    semigroups >= 0.18 && < 1,     -- has upper bound
    flatparse >= 0.5.1,            -- no upper bound
    template-haskell >= 2.16,      -- no upper bound
    haskell-src-meta >= 0.8,       -- no upper bound
    syb >= 0.7,                    -- no upper bound
    lens >= 4.0,                   -- no upper bound
    hashtables >= 1.2,             -- no upper bound
    time >= 1.4,                   -- no upper bound
    deepseq >= 1.3,                -- no upper bound
    vector >= 0.12                 -- no upper bound
```

## Proposed Bounds

Based on current Hackage versions and API stability:

```cabal
build-depends:
    base >= 4.14 && < 5,
    bytestring >= 0.10 && < 0.13,
    containers >= 0.6 && < 0.8,
    text >= 1.2 && < 2.2,
    array >= 0.5 && < 0.6,
    mtl >= 2.2 && < 2.4,
    blaze-builder >= 0.4 && < 0.5,
    utf8-string >= 0.3.7 && < 2,
    semigroups >= 0.18 && < 1,
    flatparse >= 0.5.1 && < 0.6,
    template-haskell >= 2.16 && < 2.23,
    haskell-src-meta >= 0.8 && < 0.9,
    syb >= 0.7 && < 0.8,
    deepseq >= 1.4 && < 1.6,
    vector >= 0.12 && < 0.14
```

Remove if Plan 11 executes:
- `hashtables` (unused)
- `lens` (replaced with microlens-th)
- `time` (verify if used)

## Files Changed

| File | Change |
|------|--------|
| `language-javascript.cabal` | Add upper bounds to all dependencies |

## Verification

- `cabal build` succeeds with bounded dependencies
- `cabal check` produces no warnings about missing bounds
