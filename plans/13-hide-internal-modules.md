# Plan 13: Hide Flatparse Internal Modules

## Summary

30 modules are exposed in the cabal file. Implementation details like the 6 Flatparse internal modules should not be part of the public API. Move them to `other-modules` or use a Cabal internal library.

## Current Exposed Modules (30)

### Should remain exposed (core API — 10 modules):
- `Language.JavaScript.Parser` — Main entry point
- `Language.JavaScript.Parser.AST` — AST types
- `Language.JavaScript.Parser.ParseError` — Error types
- `Language.JavaScript.Parser.SrcLocation` — Source locations
- `Language.JavaScript.Parser.Token` — Token types
- `Language.JavaScript.Pretty.Printer` — Pretty printing
- `Language.JavaScript.Process.Minify` — Minification
- `Language.JavaScript.Process.TreeShake` — Tree shaking
- `Language.JavaScript.Process.TreeShake.Types` — Tree shake config types
- `Language.JavaScript.Parser.Validator` — Validation

### Should be hidden (implementation details — 12 modules):
- `Language.JavaScript.Parser.Flatparse.Pos` — Internal position encoding
- `Language.JavaScript.Parser.Flatparse.Primitives` — Internal parser primitives
- `Language.JavaScript.Parser.Flatparse.Lexer` — Internal lexer
- `Language.JavaScript.Parser.Flatparse.Expression` — Re-export shim
- `Language.JavaScript.Parser.Flatparse.Statement` — Re-export shim
- `Language.JavaScript.Parser.Flatparse.Grammar` — Internal grammar
- `Language.JavaScript.Parser.Flatparse.Parser` — Internal parser entry points
- `Language.JavaScript.Parser.Flatparse.Benchmark` — Benchmark infrastructure
- `Language.JavaScript.Parser.Parser` — Internal (re-exported through main module)
- `Language.JavaScript.Parser.LexerUtils` — Dead code from Alex era
- `Language.JavaScript.Parser.Flatparse.Modern` — Standalone module not used by main parser
- `Language.JavaScript.Runtime.Integration` — Demo module with IO/putStrLn

### Questionable (move to separate package or optional feature — 8 modules):
- `Language.JavaScript.QQ` — Quasi-quoter entry
- `Language.JavaScript.QQ.Compile` — QQ compile-time
- `Language.JavaScript.QQ.Antiquote` — QQ antiquotation
- `Language.JavaScript.QQ.Validate` — QQ validation
- `Language.JavaScript.Pretty.JSON` — Incomplete JSON serializer
- `Language.JavaScript.Pretty.XML` — Incomplete XML serializer
- `Language.JavaScript.Pretty.SExpr` — Incomplete S-expr serializer
- `Language.JavaScript.Flatparse.Modern` — Unused standalone parser

## Changes

### Option A: Move to `other-modules` (Simple)

In `language-javascript.cabal`, move internal modules from `exposed-modules` to `other-modules`:

```cabal
exposed-modules:
    Language.JavaScript.Parser
    Language.JavaScript.Parser.AST
    Language.JavaScript.Parser.ParseError
    Language.JavaScript.Parser.SrcLocation
    Language.JavaScript.Parser.Token
    Language.JavaScript.Parser.Validator
    Language.JavaScript.Pretty.Printer
    Language.JavaScript.Process.Minify
    Language.JavaScript.Process.TreeShake
    Language.JavaScript.Process.TreeShake.Types

other-modules:
    Language.JavaScript.Parser.Parser
    Language.JavaScript.Parser.LexerUtils
    Language.JavaScript.Parser.Flatparse.Pos
    Language.JavaScript.Parser.Flatparse.Primitives
    Language.JavaScript.Parser.Flatparse.Lexer
    Language.JavaScript.Parser.Flatparse.Expression
    Language.JavaScript.Parser.Flatparse.Statement
    Language.JavaScript.Parser.Flatparse.Grammar
    Language.JavaScript.Parser.Flatparse.Parser
    Language.JavaScript.Parser.Flatparse.Benchmark
    Language.JavaScript.Parser.Flatparse.Modern
    Language.JavaScript.Runtime.Integration
```

### Option B: Internal Library (Modern Cabal)

Requires updating `Cabal-version` from `>= 1.9.2` to `>= 3.0`:

```cabal
library language-javascript-internal
    exposed-modules: Language.JavaScript.Parser.Flatparse.*
    ...

library
    build-depends: language-javascript-internal
    exposed-modules: Language.JavaScript.Parser, ...
```

**Recommendation:** Option A is simpler and doesn't require Cabal version changes.

## Files Changed

| File | Change |
|------|--------|
| `language-javascript.cabal` | Reorganize exposed-modules vs other-modules |

## Verification

- `cabal build` succeeds
- Only intended public modules are importable by consumers
- `cabal haddock` generates documentation only for public API
