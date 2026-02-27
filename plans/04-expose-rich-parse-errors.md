# Plan 04: Expose Rich ParseError Type Through Public API

## Summary

The `ParseError` type in `ParseError.hs` has 14 rich constructors with context, severity, suggestions, and recovery strategies. But the public API returns `Either String JSAST`, discarding all structure. Expose the real type.

## Current State

### The Rich Type (ParseError.hs)
```haskell
data ParseError
  = UnexpectedToken { errorToken, errorContext, expectedTokens, errorSeverity, recoveryStrategy }
  | UnexpectedChar { errorChar, errorPosition, errorContext, errorSeverity }
  | SyntaxError { errorMessage, errorPosition, errorContext, errorSeverity, suggestions }
  | SemanticError { errorMessage, errorPosition, errorContext, errorDetails }
  | InvalidNumericLiteral { ... }
  | InvalidPropertyAccess { ... }
  | InvalidAssignmentTarget { ... }
  | InvalidControlFlowLabel { ... }
  | MissingConstInitializer { ... }
  | InvalidIdentifier { ... }
  | InvalidArrowParameter { ... }
  | InvalidEscapeSequence { ... }
  | InvalidRegexPattern { ... }
  | InvalidUnicodeSequence { ... }
  | StrError String  -- legacy
```

### Current Stringification (Parser.hs:107-113)
```haskell
parseFlatparse input =
  case FlatParser.parseProgram (Text.pack input) of
    FlatParser.ParseOK success -> Right (FlatParser.parseResult success)
    FlatParser.ParseError failure -> Left (show (FlatParser.parseError failure))
```

The Flatparse parser returns its own error type which is `show`-ed to String. The rich `ParseError` type is **never constructed** by the actual parser.

## Changes

### 1. Define a Unified Error Type

Create a clean error type that bridges Flatparse errors and the rich ParseError:

```haskell
-- In ParseError.hs, add:
data JSParseError
  = JSParseError
      { parseErrorPosition :: !TokenPosn
      , parseErrorMessage :: !String
      , parseErrorContext :: !ParseContext
      , parseErrorExpected :: ![String]
      , parseErrorSeverity :: !ErrorSeverity
      }
  deriving (Eq, Show)
```

### 2. Convert Flatparse Errors to JSParseError

In `Flatparse/Parser.hs`, convert the Flatparse error into the structured type instead of using `show`:

```haskell
flatparseErrorToJSError :: ByteString -> FP.Error ByteString -> JSParseError
flatparseErrorToJSError input fpErr =
  JSParseError
    { parseErrorPosition = offsetToPos input (errorOffset fpErr)
    , parseErrorMessage = decodeError fpErr
    , parseErrorContext = TopLevelContext
    , parseErrorExpected = extractExpected fpErr
    , parseErrorSeverity = CriticalError
    }
```

### 3. Update Public API Signatures

```haskell
-- New primary API (Parser.hs):
parse :: String -> String -> Either JSParseError JSAST
parseModule :: String -> String -> Either JSParseError JSAST

-- Backwards-compatible wrappers:
parseString :: String -> String -> Either String JSAST
parseString input src = first show (parse input src)
```

### 4. Export from Main Module

In `src/Language/JavaScript/Parser.hs`, add to exports:
```haskell
module Language.JavaScript.Parser
  ( ...
  , JSParseError(..)
  , ParseContext(..)
  , ErrorSeverity(..)
  , renderParseError
  ) where
```

### 5. Wire `renderParseError` (Currently Dead Code)

The `renderParseError` function exists but is never called. Wire it as the `Show` instance or export it as the primary error rendering function.

## Files Changed

| File | Change |
|------|--------|
| `src/Language/JavaScript/Parser/ParseError.hs` | Add `JSParseError` type or reuse existing |
| `src/Language/JavaScript/Parser/Flatparse/Parser.hs` | Convert Flatparse errors to structured type |
| `src/Language/JavaScript/Parser/Parser.hs` | Change return types, add compat wrappers |
| `src/Language/JavaScript/Parser.hs` | Export error types |

## Verification

- `parse "invalid" "src"` returns `Left (JSParseError { parseErrorPosition = ..., ... })`
- `parseErrorPosition` gives correct line/column
- `parseErrorExpected` lists expected tokens
- `renderParseError` produces human-readable output
- Backwards-compatible `parseString` still works for existing consumers
