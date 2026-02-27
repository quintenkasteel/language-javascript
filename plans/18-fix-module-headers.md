# Plan 18: Fix Stale Module Headers

## Summary

Several module headers contain incorrect or stale information from the original project fork.

## Issues

### Token.hs Header (line 13)

```haskell
-- Module: Language.Python.Common.Token
```

Should be:
```haskell
-- Module: Language.JavaScript.Parser.Token
```

### Other Headers to Audit

Check all module headers in `src/` for:
- Incorrect module names (referencing Python or other projects)
- Stale author information
- Outdated descriptions

## Changes

| File | Change |
|------|--------|
| `src/Language/JavaScript/Parser/Token.hs:13` | Fix module name in header comment |
| Other files as found | Fix stale headers |

## Verification

- `grep -rn 'Python\|python' src/` returns 0 results
- All module headers match actual module names
