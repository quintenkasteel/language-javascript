# Production Readiness Plans

Detailed implementation plans for making `language-javascript` production-ready, organized by priority phase.

## Phase 1: Safety & Correctness (Blocking)

| # | Plan | Difficulty | Impact |
|---|------|-----------|--------|
| [01](01-replace-error-calls.md) | Replace all `error` calls with safe alternatives | Low | Critical — prevents runtime crashes |
| [02](02-fix-missing-js-features.md) | Fix 7 missing JS features (async arrows, class fields, debugger, etc.) | High | Critical — parse failures on common modern JS |
| [03](03-enable-pending-tests.md) | Enable 117 pending tests, remove 37 stub predicates, 25 no-op helpers | Medium | High — 3-5x inflation of real test count |
| [04](04-expose-rich-parse-errors.md) | Expose rich ParseError type through public API | Medium | High — structured errors for consumers |
| [05](05-remove-stub-assertions.md) | Remove 10 stub assertions from TreeShake tests | Low | Medium — tests that can't fail become real |

## Phase 2: Performance (High Impact)

| # | Plan | Difficulty | Impact |
|---|------|-----------|--------|
| [06](06-ast-string-to-bytestring.md) | Switch AST from String to ByteString | High | Critical — 8-12x memory reduction, 10x throughput |
| [07](07-remove-fixpositions-syb.md) | Remove fixPositions SYB traversal | Medium | High — eliminates full AST walk post-parse |
| [08](08-fix-strictness-annotations.md) | Add missing strictness annotations | Low | Low — prevents potential space leaks |
| [09](09-expose-bytestring-api.md) | Expose ByteString entry points in public API | Low | Medium — zero-conversion parsing for consumers |
| [10](10-use-bytestringof-in-lexer.md) | Use `byteStringOf` for zero-copy token capture | Medium | High — core optimization enabling Plan 06 |
| [11](11-remove-unnecessary-deps.md) | Remove unused dependencies (hashtables, lens) | Low | Low — cleaner dependency footprint |
| [12](12-reconsider-state-hack.md) | Reconsider `-fno-state-hack` flag | Low | Unknown — needs benchmarking |

## Phase 3: Package Quality (Before Release)

| # | Plan | Difficulty | Impact |
|---|------|-----------|--------|
| [13](13-hide-internal-modules.md) | Hide Flatparse internal modules | Low | Medium — clean public API surface |
| [14](14-fix-serializers.md) | Fix or remove incomplete JSON/XML/SExpr serializers | Medium | High — prevents silent data loss |
| [15](15-remove-runtime-integration.md) | Remove Runtime.Integration demo module | Low | Low — removes inappropriate IO code |
| [16](16-add-dependency-upper-bounds.md) | Add upper bounds to all dependencies | Low | Medium — prevents broken builds |
| [17](17-reenable-ghc-warnings.md) | Re-enable suppressed GHC warnings | Medium | Medium — catches dead code and bugs |
| [18](18-fix-module-headers.md) | Fix stale module headers (Python references) | Low | Low — correctness of documentation |
| [19](19-add-golden-test-files.md) | Add real golden test files | Medium | Medium — regression detection |

## Phase 4: Test Hardening

| # | Plan | Difficulty | Impact |
|---|------|-----------|--------|
| [20](20-wire-missing-test-files.md) | Wire 17 missing test files into test suite | Medium | Medium — no orphaned tests |
| [21](21-add-negative-tests.md) | Add negative tests for all parser features | Medium | High — verify parser rejects invalid JS |
| [22](22-real-property-generators.md) | Build real property-based test generators | High | High — finds bugs unit tests miss |
| [23](23-real-heap-profiling.md) | Add real heap profiling to benchmarks | Low | Medium — actual performance measurement |

## Recommended Execution Order

Plans with dependencies:
- **06, 09, 10** are tightly coupled (ByteString migration) — do 10 → 06 → 09
- **07** (fixPositions removal) is independent, can be done before or after 06
- **11** (remove deps) depends on 07 (syb removal) and 06 (possibly utf8-string)
- **03, 20** (test cleanup) should come before 21, 22 (test hardening)

Suggested order: 01 → 02 → 08 → 17 → 18 → 15 → 13 → 14 → 16 → 10 → 06 → 07 → 09 → 11 → 12 → 04 → 05 → 03 → 20 → 19 → 21 → 22 → 23
