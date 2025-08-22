# Detailed JavaScript Parser Transformation Implementation Plan
## Complete Infrastructure Overhaul - Breaking Changes Version

### 🏗️ Architecture Overview

**Current:**
```
String Input → Alex Lexer → [Token with String] → Happy Parser → Either String JSAST
```

**Target:**
```
ByteString Input → Fast Lexer → [FastToken with ByteString] → Enhanced Parser → Either [ParseError] JSAST
```

---

## Phase 1: Token System Transformation (Week 1)

### 1.1 Create New FastToken Type
**File:** `src/Language/JavaScript/Parser/FastToken.hs`

```haskell
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JavaScript.Parser.FastToken
  ( FastToken(..)
  , TokenType(..)
  , mkFastToken
  , fastTokenText
  , fastTokenBytes
  , tokenLength
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Generics (Generic)
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.ParseError (ParseContext)

-- | High-performance token with ByteString storage
data FastToken = FastToken
  { ftType :: !TokenType                    -- Token classification
  , ftSpan :: !TokenPosn                    -- Source location
  , ftBytes :: {-# UNPACK #-} !ByteString   -- Raw UTF-8 bytes (primary storage)
  , ftText :: !(Maybe Text)                 -- Cached Unicode text (lazy conversion)
  , ftContext :: !ParseContext              -- Parse context when token was created
  , ftComments :: ![FastToken]              -- Associated comments
  } deriving (Generic, NFData)

-- | Token classification for fast pattern matching
data TokenType
  -- Literals
  = DecimalTok | HexIntegerTok | BinaryIntegerTok | OctalTok | BigIntTok
  | StringTok | RegExTok | TemplateLiteralTok
  
  -- Keywords
  | VarTok | LetTok | ConstTok | FunctionTok | ClassTok | IfTok | ElseTok
  | ForTok | WhileTok | DoTok | BreakTok | ContinueTok | ReturnTok
  | TryTok | CatchTok | FinallyTok | ThrowTok | SwitchTok | CaseTok | DefaultTok
  | NewTok | ThisTok | SuperTok | TrueTok | FalseTok | NullTok | UndefinedTok
  | ImportTok | ExportTok | FromTok | AsTok | StaticTok | ExtendsTok
  | AsyncTok | AwaitTok | YieldTok | OfTok | InTok | InstanceofTok
  | TypeofTok | DeleteTok | VoidTok | WithTok | DebuggerTok
  
  -- Identifiers
  | IdentifierTok | PrivateNameTok
  
  -- Operators
  | PlusAssignTok | MinusAssignTok | TimesAssignTok | DivideAssignTok
  | ModAssignTok | LshAssignTok | RshAssignTok | UrshAssignTok
  | AndAssignTok | XorAssignTok | OrAssignTok | SimpleAssignTok
  | LogicalAndAssignTok | LogicalOrAssignTok | NullishAssignTok
  | EqTok | StrictEqTok | NeTok | StrictNeTok
  | LtTok | LeTok | GtTok | GeTok
  | PlusTok | MinusTok | TimesTok | DivideTok | ModTok
  | LshTok | RshTok | UrshTok | BitwiseAndTok | BitwiseOrTok | BitwiseXorTok
  | LogicalAndTok | LogicalOrTok | NullishCoalescingTok
  | PlusIncrementTok | MinusIncrementTok | BitwiseNotTok | LogicalNotTok
  
  -- Delimiters  
  | LeftParenTok | RightParenTok | LeftBracketTok | RightBracketTok
  | LeftCurlTok | RightCurlyTok | SemiColonTok | CommaTok
  | HookTok | ColonTok | DotTok | ArrowTok | SpreadTok
  | OptionalChainingTok | OptionalBracketTok
  
  -- Special
  | CommentTok | WhiteSpaceTok | LineFeedTok | AutoSemiTok | EOFTok
  | ErrorTok -- For error recovery
  
  deriving (Eq, Show, Generic, NFData, Ord)

-- | Create FastToken with automatic text caching for small tokens
mkFastToken :: TokenType -> TokenPosn -> ByteString -> ParseContext -> FastToken
mkFastToken tokenType pos bytes ctx = FastToken
  { ftType = tokenType
  , ftSpan = pos  
  , ftBytes = bytes
  , ftText = if BS.length bytes <= 32 then Just (Text.decodeUtf8 bytes) else Nothing
  , ftContext = ctx
  , ftComments = []
  }

-- | Get text representation, caching result
fastTokenText :: FastToken -> Text
fastTokenText FastToken{ftText = Just txt} = txt
fastTokenText ft@FastToken{ftBytes = bytes} = 
  let txt = Text.decodeUtf8 bytes
  in txt -- TODO: Cache this in IORef for mutable caching

-- | Get raw bytes (always available)  
fastTokenBytes :: FastToken -> ByteString
fastTokenBytes = ftBytes

-- | Get token length in bytes
tokenLength :: FastToken -> Int
tokenLength = BS.length . ftBytes

instance Show FastToken where
  show FastToken{..} = show ftType ++ "@" ++ show ftSpan ++ ":" ++ show (Text.decodeUtf8 ftBytes)

instance Eq FastToken where
  a == b = ftType a == ftType b && ftBytes a == ftBytes b && ftSpan a == ftSpan b
```

### 1.2 Update ParseError to Use FastToken
**File:** `src/Language/JavaScript/Parser/ParseError.hs` (modify existing)

```haskell
-- Replace all Token references with FastToken
-- Update imports
import Language.JavaScript.Parser.FastToken

-- Modify ParseError constructors
data ParseError
   = UnexpectedToken 
     { errorToken :: !FastToken              -- Changed from Token
     , errorContext :: !ParseContext
     , expectedTokens :: ![TokenType]        -- Changed from [String] for performance
     , errorSeverity :: !ErrorSeverity
     , recoveryStrategy :: !RecoveryStrategy
     , sourceLines :: ![Text]                -- Add source context
     , highlightSpan :: !(Int, Int)          -- Character span to highlight
     }
   -- ... update all other constructors similarly

-- Add enhanced error rendering with source context
renderParseErrorWithContext :: ParseError -> ByteString -> Text
renderParseErrorWithContext err sourceBytes = 
  let sourceText = Text.decodeUtf8 sourceBytes
      sourceLines = Text.lines sourceText
      errorLine = tokenPosn2Line (getErrorPosition err)
      contextLines = getSourceContext sourceLines errorLine 3 -- 3 lines before/after
  in Text.unlines
    [ "Parse Error: " <> renderErrorType (errorType err)
    , "  at " <> renderLocation (getErrorPosition err)
    , ""
    , renderSourceSnippet contextLines (getErrorPosition err)
    , ""
    , renderSuggestions (getErrorSuggestions err)
    ]

-- Add source snippet rendering with syntax highlighting
renderSourceSnippet :: [Text] -> TokenPosn -> Text
renderSourceSnippet contextLines pos = Text.unlines $
  [ Text.justifyRight 4 ' ' (Text.pack (show lineNum)) <> " | " <> line
  | (lineNum, line) <- zip [startLine..] contextLines
  ] ++ [renderHighlight pos]
  where
    startLine = max 1 (tokenPosn2Line pos - 1)
```

### 1.3 Create Token Conversion Utilities
**File:** `src/Language/JavaScript/Parser/TokenConversion.hs`

```haskell
-- Utilities for converting between old Token and FastToken
-- This helps during migration period

module Language.JavaScript.Parser.TokenConversion where

import Language.JavaScript.Parser.Token (Token)
import qualified Language.JavaScript.Parser.Token as Old
import Language.JavaScript.Parser.FastToken (FastToken)
import qualified Language.JavaScript.Parser.FastToken as Fast

-- Convert old Token to FastToken (lossy - no ByteString original)
tokenToFastToken :: Token -> FastToken
tokenToFastToken oldToken = Fast.mkFastToken
  (convertTokenType (Old.tokenType oldToken))
  (Old.tokenSpan oldToken)
  (encodeUtf8 (Text.pack (Old.tokenLiteral oldToken)))
  TopLevelContext -- Default context

-- Convert FastToken back to old Token (for compatibility)
fastTokenToToken :: FastToken -> Token
fastTokenToToken fastToken = undefined -- Implementation based on TokenType
```

---

## Phase 2: ByteString Lexer Implementation (Week 2-3)

### 2.1 Create New Alex Lexer with ByteString
**File:** `src/Language/JavaScript/Parser/FastLexer.x`

```alex
{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Language.JavaScript.Parser.FastLexer
  ( FastLexer(..)
  , AlexState(..)
  , lexToken
  , lexAll
  , runFastLexer
  ) where

import Control.Monad (when, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Word (Word8)
import Language.JavaScript.Parser.FastToken
import Language.JavaScript.Parser.ParseError
import Language.JavaScript.Parser.SrcLocation
}

-- Use ByteString wrapper for maximum performance
%wrapper "monad-bytestring"
%encoding "utf8"

-- Enhanced lexer state with error collection and context tracking
%monadUserState FastLexerState
%lexer { lexToken } { FastToken EOFTok }

-- Character classes optimized for JavaScript
$space = [ \t\f\v ]
$eol = [\r\n]
$digit = [0-9]
$nonzero = [1-9]
$octDigit = [0-7]
$hexDigit = [0-9a-fA-F]
$binDigit = [01]
$alpha = [a-zA-Z]
$identStart = [$alpha _ \$]
$identContinue = [$identStart $digit]

-- String/regex character handling
$stringChar = $printable # [\"\\]
$stringCharSingle = $printable # [\'\\]
$regexChar = $printable # [\/\\]

-- Tokens with optimized patterns
tokens :-

-- Skip whitespace but track it for ASI (Automatic Semicolon Insertion)
<0> $space+                           { skip }
<0> "//" .* $                         { mkCommentToken }
<0> "/*" ([^*] | \* [^/])* "*/"       { mkCommentToken }

-- Line endings (important for ASI)
<0> $eol                              { mkLineEndToken }

-- Keywords (ordered by frequency for branch prediction)
<0> "var"                             { mkKeywordToken VarTok }
<0> "function"                        { mkKeywordToken FunctionTok }
<0> "if"                              { mkKeywordToken IfTok }
<0> "return"                          { mkKeywordToken ReturnTok }
<0> "let"                             { mkKeywordToken LetTok }
<0> "const"                           { mkKeywordToken ConstTok }
<0> "for"                             { mkKeywordToken ForTok }
<0> "else"                            { mkKeywordToken ElseTok }
<0> "while"                           { mkKeywordToken DoTok }
<0> "do"                              { mkKeywordToken DoTok }
<0> "break"                           { mkKeywordToken BreakTok }
<0> "continue"                        { mkKeywordToken ContinueTok }
<0> "try"                             { mkKeywordToken TryTok }
<0> "catch"                           { mkKeywordToken CatchTok }
<0> "finally"                         { mkKeywordToken FinallyTok }
<0> "throw"                           { mkKeywordToken ThrowTok }
<0> "switch"                          { mkKeywordToken SwitchTok }
<0> "case"                            { mkKeywordToken CaseTok }
<0> "default"                         { mkKeywordToken DefaultTok }
<0> "class"                           { mkKeywordToken ClassTok }
<0> "extends"                         { mkKeywordToken ExtendsTok }
<0> "static"                          { mkKeywordToken StaticTok }
<0> "new"                             { mkKeywordToken NewTok }
<0> "this"                            { mkKeywordToken ThisTok }
<0> "super"                           { mkKeywordToken SuperTok }
<0> "true"                            { mkKeywordToken TrueTok }
<0> "false"                           { mkKeywordToken FalseTok }
<0> "null"                            { mkKeywordToken NullTok }
<0> "undefined"                       { mkKeywordToken UndefinedTok }
<0> "import"                          { mkKeywordToken ImportTok }
<0> "export"                          { mkKeywordToken ExportTok }
<0> "from"                            { mkKeywordToken FromTok }
<0> "as"                              { mkKeywordToken AsTok }
<0> "async"                           { mkKeywordToken AsyncTok }
<0> "await"                           { mkKeywordToken AwaitTok }
<0> "yield"                           { mkKeywordToken YieldTok }
<0> "of"                              { mkKeywordToken OfTok }
<0> "in"                              { mkKeywordToken InTok }
<0> "instanceof"                      { mkKeywordToken InstanceofTok }
<0> "typeof"                          { mkKeywordToken TypeofTok }
<0> "delete"                          { mkKeywordToken DeleteTok }
<0> "void"                            { mkKeywordToken VoidTok }
<0> "with"                            { mkKeywordToken WithTok }
<0> "debugger"                        { mkKeywordToken DebuggerTok }

-- Numeric literals with validation
<0> "0x" $hexDigit+                   { validateAndMkToken HexIntegerTok validateHex }
<0> "0X" $hexDigit+                   { validateAndMkToken HexIntegerTok validateHex }
<0> "0b" $binDigit+                   { validateAndMkToken BinaryIntegerTok validateBinary }
<0> "0B" $binDigit+                   { validateAndMkToken BinaryIntegerTok validateBinary }
<0> "0o" $octDigit+                   { validateAndMkToken OctalTok validateOctal }
<0> "0O" $octDigit+                   { validateAndMkToken OctalTok validateOctal }
<0> $digit+ "n"                       { mkTokenType BigIntTok }
<0> $digit+ (\. $digit*)? ([eE] [\+\-]? $digit+)? { validateAndMkToken DecimalTok validateDecimal }

-- String literals with escape sequence handling
<0> \" ($stringChar | \\ .)* \"       { mkStringToken }
<0> \' ($stringCharSingle | \\ .)* \' { mkStringToken }
<0> \` ([^\`\\] | \\ .)* \`           { mkTemplateToken }

-- Regular expression literals (complex state handling needed)
<0> \/ ($regexChar | \\ .)+ \/ [gimuy]* { mkRegexToken }

-- Identifiers and private names  
<0> $identStart $identContinue*       { mkIdentifierToken }
<0> \# $identStart $identContinue*    { mkPrivateNameToken }

-- Operators (ordered by frequency)
<0> "==="                             { mkOpToken StrictEqTok }
<0> "!=="                             { mkOpToken StrictNeTok }
<0> "=="                              { mkOpToken EqTok }
<0> "!="                              { mkOpToken NeTok }
<0> "<="                              { mkOpToken LeTok }
<0> ">="                              { mkOpToken GeTok }
<0> "<<"                              { mkOpToken LshTok }
<0> ">>"                              { mkOpToken RshTok }
<0> ">>>"                             { mkOpToken UrshTok }
<0> "&&"                              { mkOpToken LogicalAndTok }
<0> "||"                              { mkOpToken LogicalOrTok }
<0> "++"                              { mkOpToken PlusIncrementTok }
<0> "--"                              { mkOpToken MinusIncrementTok }
<0> "+="                              { mkOpToken PlusAssignTok }
<0> "-="                              { mkOpToken MinusAssignTok }
<0> "*="                              { mkOpToken TimesAssignTok }
<0> "/="                              { mkOpToken DivideAssignTok }
<0> "%="                              { mkOpToken ModAssignTok }
<0> "<<="                             { mkOpToken LshAssignTok }
<0> ">>="                             { mkOpToken RshAssignTok }
<0> ">>>="                            { mkOpToken UrshAssignTok }
<0> "&="                              { mkOpToken AndAssignTok }
<0> "^="                              { mkOpToken XorAssignTok }
<0> "|="                              { mkOpToken OrAssignTok }
<0> "&&="                             { mkOpToken LogicalAndAssignTok }
<0> "||="                             { mkOpToken LogicalOrAssignTok }
<0> "??="                             { mkOpToken NullishAssignTok }
<0> "=>"                              { mkOpToken ArrowTok }
<0> "..."                             { mkOpToken SpreadTok }
<0> "?."                              { mkOpToken OptionalChainingTok }
<0> "?.[" / "]"                       { mkOpToken OptionalBracketTok }
<0> "??"                              { mkOpToken NullishCoalescingTok }
<0> "="                               { mkOpToken SimpleAssignTok }
<0> "<"                               { mkOpToken LtTok }
<0> ">"                               { mkOpToken GtTok }
<0> "+"                               { mkOpToken PlusTok }
<0> "-"                               { mkOpToken MinusTok }
<0> "*"                               { mkOpToken TimesTok }
<0> "/"                               { mkOpToken DivideTok }
<0> "%"                               { mkOpToken ModTok }
<0> "&"                               { mkOpToken BitwiseAndTok }
<0> "|"                               { mkOpToken BitwiseOrTok }
<0> "^"                               { mkOpToken BitwiseXorTok }
<0> "~"                               { mkOpToken BitwiseNotTok }
<0> "!"                               { mkOpToken LogicalNotTok }

-- Delimiters
<0> "("                               { mkDelimToken LeftParenTok }
<0> ")"                               { mkDelimToken RightParenTok }  
<0> "["                               { mkDelimToken LeftBracketTok }
<0> "]"                               { mkDelimToken RightBracketTok }
<0> "{"                               { mkDelimToken LeftCurlyTok }
<0> "}"                               { mkDelimToken RightCurlyTok }
<0> ";"                               { mkDelimToken SemiColonTok }
<0> ","                               { mkDelimToken CommaTok }
<0> "?"                               { mkDelimToken HookTok }
<0> ":"                               { mkDelimToken ColonTok }
<0> "."                               { mkDelimToken DotTok }

-- Error handling - any unrecognized character
<0> .                                 { lexError }

{
-- Enhanced lexer state with error collection and context
data FastLexerState = FastLexerState
  { flsErrors :: ![ParseError]                    -- Accumulated errors
  , flsContext :: !ParseContext                  -- Current parse context
  , flsPreviousToken :: !(Maybe FastToken)      -- For ASI decisions
  , flsParenDepth :: !Int                        -- Track nesting for regex/division ambiguity  
  , flsBraceDepth :: !Int                        -- Track brace nesting
  , flsBracketDepth :: !Int                      -- Track bracket nesting
  , flsInArrowParams :: !Bool                    -- Track arrow function parameters
  , flsSource :: !ByteString                     -- Original source for error reporting
  } deriving (Show)

-- Initial lexer state
initFastLexerState :: ByteString -> FastLexerState
initFastLexerState source = FastLexerState
  { flsErrors = []
  , flsContext = TopLevelContext
  , flsPreviousToken = Nothing  
  , flsParenDepth = 0
  , flsBraceDepth = 0
  , flsBracketDepth = 0
  , flsInArrowParams = False
  , flsSource = source
  }

-- Action type for token creation
type AlexAction = AlexInput -> Int -> Alex FastToken

-- Core token creation with context tracking
mkTokenType :: TokenType -> AlexAction
mkTokenType tokenType (pos, _, input, _) len = do
  let tokenBytes = BS.take len input
  context <- getContext
  updateContext tokenType -- Update context based on token
  return $ mkFastToken tokenType (alexPosnToTokenPosn pos) tokenBytes context

-- Keyword token creation (most common case)
mkKeywordToken :: TokenType -> AlexAction  
mkKeywordToken = mkTokenType

-- Operator token creation
mkOpToken :: TokenType -> AlexAction
mkOpToken = mkTokenType

-- Delimiter token creation with nesting tracking
mkDelimToken :: TokenType -> AlexAction
mkDelimToken tokenType input len = do
  updateNesting tokenType -- Track delimiter nesting
  mkTokenType tokenType input len

-- String token with escape sequence validation
mkStringToken :: AlexAction
mkStringToken (pos, _, input, _) len = do
  let tokenBytes = BS.take len input
  case validateStringLiteral tokenBytes of
    Left err -> do
      addError (createInvalidEscapeError pos err)
      return $ mkFastToken ErrorTok (alexPosnToTokenPosn pos) tokenBytes TopLevelContext
    Right _ -> do
      context <- getContext
      return $ mkFastToken StringTok (alexPosnToTokenPosn pos) tokenBytes context

-- Numeric token with comprehensive validation
validateAndMkToken :: TokenType -> (ByteString -> Either String ()) -> AlexAction
validateAndMkToken tokenType validator (pos, _, input, _) len = do
  let tokenBytes = BS.take len input
  case validator tokenBytes of
    Left err -> do
      addError (createInvalidNumericError pos tokenBytes err)
      return $ mkFastToken ErrorTok (alexPosnToTokenPosn pos) tokenBytes TopLevelContext
    Right _ -> do
      context <- getContext  
      return $ mkFastToken tokenType (alexPosnToTokenPosn pos) tokenBytes context

-- Enhanced numeric validation
validateDecimal :: ByteString -> Either String ()
validateDecimal bytes
  | ".." `BS8.isInfixOf` bytes = Left "Invalid decimal: consecutive dots"
  | bytes `elem` [".", ".."] = Left "Invalid decimal: standalone dots"
  | BS8.count '.' bytes > 1 = Left "Invalid decimal: multiple decimal points"
  | otherwise = Right ()

validateHex :: ByteString -> Either String ()
validateHex bytes
  | BS.length bytes <= 2 = Left "Invalid hex: no digits after 0x"
  | otherwise = Right ()

validateBinary :: ByteString -> Either String ()
validateBinary bytes
  | BS.length bytes <= 2 = Left "Invalid binary: no digits after 0b"
  | otherwise = Right ()

validateOctal :: ByteString -> Either String ()  
validateOctal bytes
  | BS.length bytes <= 2 = Left "Invalid octal: no digits after 0o"
  | otherwise = Right ()

-- String literal validation with detailed error reporting
validateStringLiteral :: ByteString -> Either String ()
validateStringLiteral bytes = do
  unless (isValidlyTerminated bytes) $
    Left "Unterminated string literal"
  validateEscapeSequences bytes
  where
    isValidlyTerminated bs = 
      let len = BS.length bs
      in len >= 2 && BS.head bs == BS.last bs && BS.head bs `elem` [34, 39] -- " or '
    
    validateEscapeSequences bs = 
      case findInvalidEscape bs of
        Nothing -> Right ()
        Just invalidSeq -> Left ("Invalid escape sequence: " ++ BS8.unpack invalidSeq)

-- Context management for better error reporting
getContext :: Alex ParseContext
getContext = do
  FastLexerState{..} <- alexGetUserState
  return flsContext

updateContext :: TokenType -> Alex ()
updateContext tokenType = do
  state <- alexGetUserState
  let newContext = case tokenType of
        FunctionTok -> FunctionContext
        ClassTok -> ClassContext  
        LeftCurlyTok -> ObjectLiteralContext
        LeftBracketTok -> ArrayLiteralContext
        ImportTok -> ImportContext
        ExportTok -> ExportContext
        _ -> flsContext state
  alexSetUserState state { flsContext = newContext }

-- Nesting tracking for better error recovery
updateNesting :: TokenType -> Alex ()
updateNesting tokenType = do
  state <- alexGetUserState  
  let newState = case tokenType of
        LeftParenTok -> state { flsParenDepth = flsParenDepth state + 1 }
        RightParenTok -> state { flsParenDepth = flsParenDepth state - 1 }
        LeftBracketTok -> state { flsBracketDepth = flsBracketDepth state + 1 }
        RightBracketTok -> state { flsBracketDepth = flsBracketDepth state - 1 }
        LeftCurlyTok -> state { flsBraceDepth = flsBraceDepth state + 1 }
        RightCurlyTok -> state { flsBraceDepth = flsBraceDepth state - 1 }
        _ -> state
  alexSetUserState newState

-- Enhanced error handling
addError :: ParseError -> Alex ()
addError err = do
  state <- alexGetUserState
  alexSetUserState state { flsErrors = err : flsErrors state }

lexError :: AlexAction
lexError (pos, _, input, _) _ = do
  let badChar = BS8.head input
  addError (createUnexpectedCharError pos badChar)
  return $ mkFastToken ErrorTok (alexPosnToTokenPosn pos) (BS8.singleton badChar) TopLevelContext

-- Error creation utilities
createInvalidEscapeError :: AlexPosn -> String -> ParseError
createInvalidEscapeError pos msg = InvalidEscapeSequence
  { errorSequence = msg
  , errorPosition = alexPosnToTokenPosn pos
  , errorContext = TopLevelContext
  , suggestions = ["Check escape sequence syntax", "Use raw string if needed"]
  }

createInvalidNumericError :: AlexPosn -> ByteString -> String -> ParseError
createInvalidNumericError pos bytes msg = InvalidNumericLiteral
  { errorLiteral = BS8.unpack bytes
  , errorPosition = alexPosnToTokenPosn pos
  , errorContext = TopLevelContext
  , suggestions = ["Check numeric literal syntax"]
  }

createUnexpectedCharError :: AlexPosn -> Char -> ParseError
createUnexpectedCharError pos char = UnexpectedChar
  { errorChar = char
  , errorPosition = alexPosnToTokenPosn pos
  , errorContext = TopLevelContext
  , errorSeverity = CriticalError
  }

-- Conversion utilities
alexPosnToTokenPosn :: AlexPosn -> TokenPosn
alexPosnToTokenPosn (AlexPn offset line col) = TokenPn offset line col

-- Main lexing interface
runFastLexer :: ByteString -> Either [ParseError] [FastToken]
runFastLexer input = 
  case runAlex input lexAll of
    Left errMsg -> Left [createGenericError errMsg]
    Right (tokens, state) -> 
      let errors = flsErrors state
      in if null errors then Right tokens else Left errors

lexAll :: Alex [FastToken]
lexAll = do
  token <- lexToken  
  if ftType token == EOFTok
    then return [token]
    else (token:) <$> lexAll

createGenericError :: String -> ParseError
createGenericError msg = SyntaxError
  { errorMessage = msg
  , errorPosition = TokenPn 0 0 0
  , errorContext = TopLevelContext
  , errorSeverity = CriticalError
  , suggestions = []
  }
}
```

---

## Phase 3: Happy Parser Integration (Week 4-5)

### 3.1 Update Happy Grammar for FastToken and ParseError
**File:** `src/Language/JavaScript/Parser/FastGrammar.y`

```happy
{
{-# LANGUAGE OverloadedStrings #-}

module Language.JavaScript.Parser.FastGrammar where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Data.List (intercalate)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.FastToken
import Language.JavaScript.Parser.ParseError
import Language.JavaScript.Parser.SrcLocation
}

-- Enhanced grammar with error recovery and FastToken integration
%name parseProgram Program
%name parseModule Module
%name parseExpression Expression
%name parseStatement Statement

-- Error handling integration
%error { parseError }
%errorhandlertype explist
%monad { ParseM } { >>= } { return }

-- FastToken integration
%tokentype { FastToken }

-- Token definitions with FastToken pattern matching
%token
    -- Literals
    NUM        { FastToken { ftType = DecimalTok, ftBytes = $$ } }
    HEXNUM     { FastToken { ftType = HexIntegerTok, ftBytes = $$ } }
    BINNUM     { FastToken { ftType = BinaryIntegerTok, ftBytes = $$ } }
    OCTNUM     { FastToken { ftType = OctalTok, ftBytes = $$ } }
    BIGINT     { FastToken { ftType = BigIntTok, ftBytes = $$ } }
    STRING     { FastToken { ftType = StringTok, ftBytes = $$ } }
    REGEX      { FastToken { ftType = RegExTok, ftBytes = $$ } }
    TEMPLATE   { FastToken { ftType = TemplateLiteralTok, ftBytes = $$ } }
    
    -- Keywords  
    'var'      { FastToken { ftType = VarTok } }
    'let'      { FastToken { ftType = LetTok } }
    'const'    { FastToken { ftType = ConstTok } }
    'function' { FastToken { ftType = FunctionTok } }
    'class'    { FastToken { ftType = ClassTok } }
    'if'       { FastToken { ftType = IfTok } }
    'else'     { FastToken { ftType = ElseTok } }
    'for'      { FastToken { ftType = ForTok } }
    'while'    { FastToken { ftType = WhileTok } }
    'do'       { FastToken { ftType = DoTok } }
    'break'    { FastToken { ftType = BreakTok } }
    'continue' { FastToken { ftType = ContinueTok } }
    'return'   { FastToken { ftType = ReturnTok } }
    'try'      { FastToken { ftType = TryTok } }
    'catch'    { FastToken { ftType = CatchTok } }
    'finally'  { FastToken { ftType = FinallyTok } }
    'throw'    { FastToken { ftType = ThrowTok } }
    'switch'   { FastToken { ftType = SwitchTok } }
    'case'     { FastToken { ftType = CaseTok } }
    'default'  { FastToken { ftType = DefaultTok } }
    'new'      { FastToken { ftType = NewTok } }
    'this'     { FastToken { ftType = ThisTok } }
    'super'    { FastToken { ftType = SuperTok } }
    'true'     { FastToken { ftType = TrueTok } }
    'false'    { FastToken { ftType = FalseTok } }
    'null'     { FastToken { ftType = NullTok } }
    'import'   { FastToken { ftType = ImportTok } }
    'export'   { FastToken { ftType = ExportTok } }
    'from'     { FastToken { ftType = FromTok } }
    'as'       { FastToken { ftType = AsTok } }
    'static'   { FastToken { ftType = StaticTok } }
    'extends'  { FastToken { ftType = ExtendsTok } }
    'async'    { FastToken { ftType = AsyncTok } }
    'await'    { FastToken { ftType = AwaitTok } }
    'yield'    { FastToken { ftType = YieldTok } }
    'of'       { FastToken { ftType = OfTok } }
    'in'       { FastToken { ftType = InTok } }
    'instanceof' { FastToken { ftType = InstanceofTok } }
    'typeof'   { FastToken { ftType = TypeofTok } }
    'delete'   { FastToken { ftType = DeleteTok } }
    'void'     { FastToken { ftType = VoidTok } }
    'with'     { FastToken { ftType = WithTok } }
    'debugger' { FastToken { ftType = DebuggerTok } }
    
    -- Identifiers
    IDENT      { FastToken { ftType = IdentifierTok, ftBytes = $$ } }
    PRIVATENAME { FastToken { ftType = PrivateNameTok, ftBytes = $$ } }
    
    -- Operators
    '==='      { FastToken { ftType = StrictEqTok } }
    '!=='      { FastToken { ftType = StrictNeTok } }
    '=='       { FastToken { ftType = EqTok } }
    '!='       { FastToken { ftType = NeTok } }
    '<='       { FastToken { ftType = LeTok } }
    '>='       { FastToken { ftType = GeTok } }
    '<<'       { FastToken { ftType = LshTok } }
    '>>'       { FastToken { ftType = RshTok } }
    '>>>'      { FastToken { ftType = UrshTok } }
    '&&'       { FastToken { ftType = LogicalAndTok } }
    '||'       { FastToken { ftType = LogicalOrTok } }
    '++'       { FastToken { ftType = PlusIncrementTok } }
    '--'       { FastToken { ftType = MinusIncrementTok } }
    '+='       { FastToken { ftType = PlusAssignTok } }
    '-='       { FastToken { ftType = MinusAssignTok } }
    '*='       { FastToken { ftType = TimesAssignTok } }
    '/='       { FastToken { ftType = DivideAssignTok } }
    '%='       { FastToken { ftType = ModAssignTok } }
    '<<='      { FastToken { ftType = LshAssignTok } }
    '>>='      { FastToken { ftType = RshAssignTok } }
    '>>>='     { FastToken { ftType = UrshAssignTok } }
    '&='       { FastToken { ftType = AndAssignTok } }
    '^='       { FastToken { ftType = XorAssignTok } }
    '|='       { FastToken { ftType = OrAssignTok } }
    '&&='      { FastToken { ftType = LogicalAndAssignTok } }
    '||='      { FastToken { ftType = LogicalOrAssignTok } }
    '??='      { FastToken { ftType = NullishAssignTok } }
    '=>'       { FastToken { ftType = ArrowTok } }
    '...'      { FastToken { ftType = SpreadTok } }
    '?.'       { FastToken { ftType = OptionalChainingTok } }
    '??'       { FastToken { ftType = NullishCoalescingTok } }
    '='        { FastToken { ftType = SimpleAssignTok } }
    '<'        { FastToken { ftType = LtTok } }
    '>'        { FastToken { ftType = GtTok } }
    '+'        { FastToken { ftType = PlusTok } }
    '-'        { FastToken { ftType = MinusTok } }
    '*'        { FastToken { ftType = TimesTok } }
    '/'        { FastToken { ftType = DivideTok } }
    '%'        { FastToken { ftType = ModTok } }
    '&'        { FastToken { ftType = BitwiseAndTok } }
    '|'        { FastToken { ftType = BitwiseOrTok } }
    '^'        { FastToken { ftType = BitwiseXorTok } }
    '~'        { FastToken { ftType = BitwiseNotTok } }
    '!'        { FastToken { ftType = LogicalNotTok } }
    
    -- Delimiters
    '('        { FastToken { ftType = LeftParenTok } }
    ')'        { FastToken { ftType = RightParenTok } }
    '['        { FastToken { ftType = LeftBracketTok } }
    ']'        { FastToken { ftType = RightBracketTok } }
    '{'        { FastToken { ftType = LeftCurlyTok } }
    '}'        { FastToken { ftType = RightCurlyTok } }
    ';'        { FastToken { ftType = SemiColonTok } }
    ','        { FastToken { ftType = CommaTok } }
    '?'        { FastToken { ftType = HookTok } }
    ':'        { FastToken { ftType = ColonTok } }
    '.'        { FastToken { ftType = DotTok } }

-- Operator precedence and associativity (unchanged)
%right '=' '+=' '-=' '*=' '/=' '%=' '<<=' '>>=' '>>>=' '&=' '^=' '|=' '&&=' '||=' '??='
%right '?' ':'
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%left '==' '!=' '===' '!=='
%left '<' '<=' '>' '>=' 'instanceof' 'in'
%left '<<' '>>' '>>>'
%left '+' '-'
%left '*' '/' '%'
%right '!' '~' '++' '--' 'typeof' 'void' 'delete' 'await'
%left '.' '[' '('
%right '=>'

%%

-- Enhanced grammar with error recovery productions
Program :: { JSAST }
Program : SourceElements                     { JSAstProgram $1 noAnnot }
        | {- empty -}                        { JSAstProgram [] noAnnot }
        | error                              {% recoverProgram $1 }

Module :: { JSAST }
Module  : ModuleItems                        { JSAstModule $1 noAnnot }
        | {- empty -}                        { JSAstModule [] noAnnot }
        | error                              {% recoverModule $1 }

-- Source elements with error recovery
SourceElements :: { [JSStatement] }
SourceElements : SourceElement                    { [$1] }
           | SourceElements SourceElement         { $1 ++ [$2] }
           | SourceElements error SourceElement  {% recoverSourceElements $1 $2 $3 }

SourceElement :: { JSStatement }
SourceElement : Statement                    { $1 }
          | FunctionDeclaration             { $1 }
          | error ';'                       {% recoverStatement $1 }
          | error '}'                       {% recoverBlock $1 }

-- Statements with comprehensive error recovery
Statement :: { JSStatement }
Statement : Block                           { $1 }
        | VariableStatement                 { $1 }
        | EmptyStatement                    { $1 }
        | ExpressionStatement               { $1 }
        | IfStatement                       { $1 }
        | IterationStatement                { $1 }
        | ContinueStatement                 { $1 }
        | BreakStatement                    { $1 }
        | ReturnStatement                   { $1 }
        | WithStatement                     { $1 }
        | LabelledStatement                 { $1 }
        | SwitchStatement                   { $1 }
        | ThrowStatement                    { $1 }
        | TryStatement                      { $1 }
        | DebuggerStatement                 { $1 }

-- Variable declarations with enhanced error reporting
VariableStatement :: { JSStatement }
VariableStatement : VarDeclaration ';'      { $1 }
            | VarDeclaration error          {% recoverVariableDeclaration $1 $2 }

VarDeclaration :: { JSStatement }  
VarDeclaration : 'var' VariableDeclarationList      { JSVariable (ann $1) $2 JSSemiAuto }
           | 'let' VariableDeclarationList      { JSLet (ann $1) $2 JSSemiAuto }
           | 'const' ConstDeclarationList       { JSConst (ann $1) $2 JSSemiAuto }

VariableDeclarationList :: { JSCommaList JSVariableDeclarator }
VariableDeclarationList : VariableDeclaration                                { JSLOne $1 }
                    | VariableDeclarationList ',' VariableDeclaration    { JSLCons $1 (ann $2) $3 }
                    | VariableDeclarationList error VariableDeclaration  {% recoverVariableList $1 $2 $3 }

VariableDeclaration :: { JSVariableDeclarator }
VariableDeclaration : IDENT Initializer              { JSVarDeclarator (JSIdentifier (ann $1) (fastTokenText $1)) $2 }
                | IDENT                              { JSVarDeclarator (JSIdentifier (ann $1) (fastTokenText $1)) JSVarInitNone }
                | error                              {% recoverVariableDeclarator $1 }

-- Const declarations with mandatory initializer validation
ConstDeclarationList :: { JSCommaList JSVariableDeclarator }
ConstDeclarationList : ConstDeclaration                            { JSLOne $1 }
                 | ConstDeclarationList ',' ConstDeclaration       { JSLCons $1 (ann $2) $3 }

ConstDeclaration :: { JSVariableDeclarator }
ConstDeclaration : IDENT Initializer                  { JSVarDeclarator (JSIdentifier (ann $1) (fastTokenText $1)) $2 }
           | IDENT error                           {% constWithoutInitializer $1 $2 }

-- Function declarations with comprehensive error handling
FunctionDeclaration :: { JSStatement }
FunctionDeclaration : 'function' IDENT '(' FormalParameterList ')' Block 
                    { JSFunction (ann $1) (JSIdentifier (ann $2) (fastTokenText $2)) (ann $3) $4 (ann $5) $6 }
                | 'function' IDENT '(' ')' Block
                    { JSFunction (ann $1) (JSIdentifier (ann $2) (fastTokenText $2)) (ann $3) JSLNil (ann $4) $5 }
                | 'function' error '(' FormalParameterList ')' Block
                    {% recoverFunctionName $1 $2 $3 $4 $5 $6 }
                | 'function' IDENT error FormalParameterList ')' Block
                    {% recoverFunctionParams $1 $2 $3 $4 $5 $6 }
                | 'function' IDENT '(' FormalParameterList error Block
                    {% recoverFunctionParamsClose $1 $2 $3 $4 $5 $6 }

-- Expressions with enhanced error recovery
Expression :: { JSExpression }
Expression : AssignmentExpression              { $1 }
        | Expression ',' AssignmentExpression  { JSCommaExpression $1 (ann $2) $3 }
        | error                                {% recoverExpression $1 }

AssignmentExpression :: { JSExpression }
AssignmentExpression : ConditionalExpression                          { $1 }
                 | LeftHandSideExpression AssignmentOperator AssignmentExpression
                                                                      { JSAssignExpression $1 $2 $3 }
                 | LeftHandSideExpression error AssignmentExpression  {% recoverAssignment $1 $2 $3 }

-- Binary expressions with detailed error recovery
ConditionalExpression :: { JSExpression }
ConditionalExpression : LogicalORExpression                         { $1 }
                  | LogicalORExpression '?' AssignmentExpression ':' AssignmentExpression
                                                                   { JSConditionalExpression $1 (ann $2) $3 (ann $4) $5 }
                  | LogicalORExpression '?' AssignmentExpression error AssignmentExpression
                                                                   {% recoverTernary $1 $2 $3 $4 $5 }

-- Continue with all other expression types...
LogicalORExpression :: { JSExpression }
LogicalORExpression : LogicalANDExpression                         { $1 }
                | LogicalORExpression '||' LogicalANDExpression    { JSExpressionBinary $1 (JSBinOpOr (ann $2)) $3 }

-- ... (continue with all expression types)

-- Primary expressions with literals
PrimaryExpression :: { JSExpression }
PrimaryExpression : 'this'                     { JSLiteral (ann $1) "this" }
              | IDENT                          { JSIdentifier (ann $1) (fastTokenText $1) }
              | Literal                        { $1 }
              | ArrayLiteral                   { $1 }
              | ObjectLiteral                  { $1 }
              | '(' Expression ')'             { JSExpressionParen (ann $1) $2 (ann $3) }
              | '(' error ')'                  {% recoverParenExpression $1 $2 $3 }

-- Literals with FastToken integration
Literal :: { JSExpression }
Literal : 'null'        { JSLiteral (ann $1) "null" }
        | 'true'        { JSLiteral (ann $1) "true" }
        | 'false'       { JSLiteral (ann $1) "false" }
        | NUM           { JSDecimal (ann $1) (fastTokenText $1) }
        | HEXNUM        { JSHexInteger (ann $1) (fastTokenText $1) }
        | BINNUM        { JSBinaryInteger (ann $1) (fastTokenText $1) }
        | OCTNUM        { JSOctal (ann $1) (fastTokenText $1) }
        | BIGINT        { JSBigInt (ann $1) (fastTokenText $1) }
        | STRING        { JSStringLiteral (ann $1) (fastTokenText $1) }
        | REGEX         { JSRegEx (ann $1) (fastTokenText $1) }
        | TEMPLATE      { JSTemplateLiteral (ann $1) Nothing [JSTemplatePart (fastTokenText $1) Nothing] Nothing }

{
-- Enhanced ParseM monad with error accumulation
type ParseM = Either [ParseError]

-- Convert FastToken to JSAnnot for AST construction
ann :: FastToken -> JSAnnot
ann FastToken{ftSpan = pos} = JSAnnot pos []

-- Utility for extracting annotation from position
noAnnot :: JSAnnot
noAnnot = JSAnnot (TokenPn 0 0 0) []

-- Enhanced error handling with detailed context
parseError :: [FastToken] -> ParseM a
parseError [] = Left [createEOFError]
parseError (token:_) = Left [createUnexpectedTokenError token]

createUnexpectedTokenError :: FastToken -> ParseError
createUnexpectedTokenError token = UnexpectedToken
  { errorToken = token
  , errorContext = ftContext token
  , expectedTokens = [] -- TODO: Extract from Happy's expected token set
  , errorSeverity = MajorError
  , recoveryStrategy = selectRecoveryStrategy (ftType token)
  , sourceLines = [] -- TODO: Extract from source context
  , highlightSpan = (0, tokenLength token)
  }

createEOFError :: ParseError
createEOFError = UnexpectedChar
  { errorChar = '\0'
  , errorPosition = TokenPn 0 0 0
  , errorContext = TopLevelContext
  , errorSeverity = CriticalError
  }

-- Smart recovery strategy selection
selectRecoveryStrategy :: TokenType -> RecoveryStrategy
selectRecoveryStrategy tokenType = case tokenType of
  SemiColonTok -> SyncToSemicolon
  RightCurlyTok -> SyncToCloseBrace
  RightParenTok -> SyncToCloseBrace
  _ | isKeyword tokenType -> SyncToKeyword
  _ -> SyncToSemicolon
  where
    isKeyword VarTok = True
    isKeyword LetTok = True
    isKeyword ConstTok = True
    isKeyword FunctionTok = True
    isKeyword ClassTok = True
    isKeyword IfTok = True
    isKeyword ForTok = True
    isKeyword WhileTok = True
    isKeyword DoTok = True
    isKeyword TryTok = True
    isKeyword SwitchTok = True
    isKeyword _ = False

-- Specific error recovery functions
recoverProgram :: [FastToken] -> ParseM JSAST
recoverProgram tokens = do
  -- Try to recover by skipping to next statement
  case dropWhile (not . isStatementStart . ftType) tokens of
    [] -> return (JSAstProgram [] noAnnot)
    recovered -> parseError recovered
  where
    isStatementStart VarTok = True
    isStatementStart LetTok = True
    isStatementStart ConstTok = True
    isStatementStart FunctionTok = True
    isStatementStart ClassTok = True
    isStatementStart IfTok = True
    isStatementStart ForTok = True
    isStatementStart WhileTok = True
    isStatementStart DoTok = True
    isStatementStart TryTok = True
    isStatementStart SwitchTok = True
    isStatementStart LeftCurlyTok = True
    isStatementStart _ = False

recoverModule :: [FastToken] -> ParseM JSAST
recoverModule tokens = recoverProgram tokens -- Similar recovery

recoverStatement :: [FastToken] -> ParseM JSStatement
recoverStatement tokens = do
  -- Create empty statement as recovery
  return (JSEmpty noAnnot)

recoverSourceElements :: [JSStatement] -> [FastToken] -> JSStatement -> ParseM [JSStatement]
recoverSourceElements prev _errorTokens stmt = do
  -- Continue with recovered statement
  return (prev ++ [stmt])

recoverVariableDeclaration :: JSStatement -> [FastToken] -> ParseM JSStatement
recoverVariableDeclaration stmt _errorTokens = do
  -- Return the partial statement
  return stmt

recoverVariableList :: JSCommaList JSVariableDeclarator -> [FastToken] -> JSVariableDeclarator -> ParseM (JSCommaList JSVariableDeclarator)
recoverVariableList prev _errorTokens decl = do
  -- Assume missing comma
  return (JSLCons prev (JSAnnot (TokenPn 0 0 0) []) decl)

recoverVariableDeclarator :: [FastToken] -> ParseM JSVariableDeclarator
recoverVariableDeclarator _errorTokens = do
  -- Create placeholder declarator
  return (JSVarDeclarator (JSIdentifier noAnnot "unknown") JSVarInitNone)

constWithoutInitializer :: FastToken -> [FastToken] -> ParseM JSVariableDeclarator
constWithoutInitializer identToken _errorTokens = do
  let err = MissingConstInitializer
        { errorIdentifier = fastTokenText identToken
        , errorPosition = ftSpan identToken
        , errorContext = ftContext identToken
        , suggestions = ["Add initializer: const " ++ fastTokenText identToken ++ " = value"]
        }
  Left [err]

-- More recovery functions...
recoverExpression :: [FastToken] -> ParseM JSExpression
recoverExpression _errorTokens = do
  return (JSIdentifier noAnnot "recovered")

recoverAssignment :: JSExpression -> [FastToken] -> JSExpression -> ParseM JSExpression
recoverAssignment left _errorTokens right = do
  -- Assume simple assignment
  return (JSAssignExpression left (JSAssign noAnnot) right)

recoverTernary :: JSExpression -> FastToken -> JSExpression -> [FastToken] -> JSExpression -> ParseM JSExpression  
recoverTernary cond _quest trueExpr _errorTokens falseExpr = do
  -- Assume missing colon
  return (JSConditionalExpression cond (JSAnnot (ftSpan _quest) []) trueExpr (JSAnnot (TokenPn 0 0 0) []) falseExpr)

recoverParenExpression :: FastToken -> [FastToken] -> FastToken -> ParseM JSExpression
recoverParenExpression _leftParen _errorTokens _rightParen = do
  -- Return placeholder expression
  return (JSIdentifier noAnnot "recovered")

recoverFunctionName :: FastToken -> [FastToken] -> FastToken -> JSCommaList JSExpression -> FastToken -> JSStatement -> ParseM JSStatement
recoverFunctionName funcTok _errorTokens leftParen params rightParen body = do
  -- Create function with placeholder name
  return (JSFunction (ann funcTok) (JSIdentifier noAnnot "recovered") (ann leftParen) params (ann rightParen) body)

recoverFunctionParams :: FastToken -> FastToken -> [FastToken] -> JSCommaList JSExpression -> FastToken -> JSStatement -> ParseM JSStatement
recoverFunctionParams funcTok ident _errorTokens params rightParen body = do
  -- Assume missing left paren
  return (JSFunction (ann funcTok) (JSIdentifier (ann ident) (fastTokenText ident)) (JSAnnot (TokenPn 0 0 0) []) params (ann rightParen) body)

recoverFunctionParamsClose :: FastToken -> FastToken -> FastToken -> JSCommaList JSExpression -> [FastToken] -> JSStatement -> ParseM JSStatement  
recoverFunctionParamsClose funcTok ident leftParen params _errorTokens body = do
  -- Assume missing right paren
  return (JSFunction (ann funcTok) (JSIdentifier (ann ident) (fastTokenText ident)) (ann leftParen) params (JSAnnot (TokenPn 0 0 0) []) body)

recoverBlock :: [FastToken] -> ParseM JSStatement
recoverBlock _errorTokens = do
  return (JSEmpty noAnnot)
}
```

### 3.2 Update Main Parser Interface
**File:** `src/Language/JavaScript/Parser/FastParser.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Language.JavaScript.Parser.FastParser
  ( -- * Parsing functions
    parseProgram
  , parseModule  
  , parseExpression
  , parseStatement
  , parseFromFile
  , parseFromByteString
    -- * Types
  , ParseResult(..)
  , ParserOptions(..)
  , defaultParserOptions
    -- * Error handling
  , renderParseErrors
  , ParseError(..)
  , ErrorSeverity(..)
  , ParseContext(..)
  ) where

import Control.Exception (try, IOException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.FastGrammar (parseProgram, parseModule, parseExpression, parseStatement)
import Language.JavaScript.Parser.FastLexer (runFastLexer)
import Language.JavaScript.Parser.FastToken
import Language.JavaScript.Parser.ParseError
import System.IO

-- | Parse result with rich error information
data ParseResult a = ParseResult
  { prResult :: Either [ParseError] a    -- Parse result or errors
  , prWarnings :: [ParseError]           -- Non-fatal warnings  
  , prTokens :: [FastToken]              -- All tokens for tooling
  , prSource :: ByteString               -- Original source
  } deriving (Show)

-- | Parser configuration options
data ParserOptions = ParserOptions
  { poStrictMode :: Bool                 -- Enable strict mode validation
  , poES6Features :: Bool                -- Enable ES6+ features
  , poJSXSupport :: Bool                 -- Enable JSX support
  , poCollectTokens :: Bool              -- Collect tokens for IDE support
  , poMaxErrors :: Int                   -- Maximum errors before stopping
  , poRecoveryMode :: Bool               -- Enable error recovery
  } deriving (Show)

-- | Default parser options
defaultParserOptions :: ParserOptions
defaultParserOptions = ParserOptions
  { poStrictMode = False
  , poES6Features = True
  , poJSXSupport = False
  , poCollectTokens = True
  , poMaxErrors = 10
  , poRecoveryMode = True
  }

-- | Parse JavaScript program from ByteString
parseFromByteString :: ParserOptions -> ByteString -> FilePath -> ParseResult JSAST
parseFromByteString opts source filename = 
  case runFastLexer source of
    Left lexErrors -> ParseResult (Left lexErrors) [] [] source
    Right tokens -> 
      case parseProgram tokens of
        Left parseErrors -> 
          let allErrors = takeWhile ((<= poMaxErrors opts) . length) [lexErrors ++ parseErrors]
          in ParseResult (Left (head allErrors)) [] tokens source
        Right ast ->
          let warnings = if poStrictMode opts then validateStrict ast else []
          in ParseResult (Right ast) warnings tokens source

-- | Parse JavaScript module from ByteString  
parseModuleFromByteString :: ParserOptions -> ByteString -> FilePath -> ParseResult JSAST
parseModuleFromByteString opts source filename =
  case runFastLexer source of
    Left lexErrors -> ParseResult (Left lexErrors) [] [] source
    Right tokens ->
      case parseModule tokens of
        Left parseErrors -> 
          ParseResult (Left parseErrors) [] tokens source
        Right ast ->
          let warnings = if poStrictMode opts then validateStrict ast else []
          in ParseResult (Right ast) warnings tokens source

-- | Parse from file with automatic encoding detection
parseFromFile :: ParserOptions -> FilePath -> IO (ParseResult JSAST)
parseFromFile opts filename = do
  result <- try (BS.readFile filename)
  case result of
    Left (err :: IOException) -> 
      return $ ParseResult (Left [createIOError err filename]) [] [] BS.empty
    Right source -> 
      return $ parseFromByteString opts source filename

-- | High-level convenience functions with String input (UTF-8 conversion)
parseProgram :: String -> FilePath -> Either [ParseError] JSAST
parseProgram source filename = 
  prResult $ parseFromByteString defaultParserOptions (Text.encodeUtf8 (Text.pack source)) filename

parseModule :: String -> FilePath -> Either [ParseError] JSAST  
parseModule source filename =
  prResult $ parseModuleFromByteString defaultParserOptions (Text.encodeUtf8 (Text.pack source)) filename

-- | Parse expression only
parseExpression :: String -> Either [ParseError] JSExpression
parseExpression source = 
  case runFastLexer (Text.encodeUtf8 (Text.pack source)) of
    Left errors -> Left errors
    Right tokens -> parseExpression tokens

-- | Parse single statement
parseStatement :: String -> Either [ParseError] JSStatement  
parseStatement source =
  case runFastLexer (Text.encodeUtf8 (Text.pack source)) of
    Left errors -> Left errors
    Right tokens -> parseStatement tokens

-- | Render parse errors to user-friendly text
renderParseErrors :: [ParseError] -> ByteString -> Text
renderParseErrors errors source = Text.intercalate "\n\n" $
  map (`renderParseErrorWithContext` source) errors

-- | Strict mode validation (placeholder)
validateStrict :: JSAST -> [ParseError]
validateStrict _ast = [] -- TODO: Implement strict mode validation

-- | Create IO error
createIOError :: IOException -> FilePath -> ParseError
createIOError ioErr filename = SyntaxError
  { errorMessage = "IO Error: " ++ show ioErr
  , errorPosition = TokenPn 0 0 0
  , errorContext = TopLevelContext
  , errorSeverity = CriticalError
  , suggestions = ["Check file permissions", "Verify file exists: " ++ filename]
  }
```

---

## Phase 4: Performance Optimization (Week 6)

### 4.1 Memory Layout Optimization
**File:** `src/Language/JavaScript/Parser/Compact.hs`

```haskell
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Language.JavaScript.Parser.Compact where

import Control.DeepSeq
import Data.Compact
import Data.ByteString (ByteString)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.FastToken

-- | Compact representation for memory efficiency
data CompactParseResult = CompactParseResult
  { cprAST :: !(Compact JSAST)
  , cprTokens :: !(Compact [FastToken])  
  , cprSource :: !(Compact ByteString)
  , cprSize :: !Int                      -- Total memory size
  } deriving (Show)

-- | Create compact representation
compactify :: JSAST -> [FastToken] -> ByteString -> IO CompactParseResult
compactify ast tokens source = do
  -- Force full evaluation first
  ast `deepseq` tokens `deepseq` source `deepseq` return ()
  
  -- Create compact regions
  compactAST <- compact ast
  compactTokens <- compact tokens  
  compactSource <- compact source
  
  -- Calculate total size
  let totalSize = compactSize compactAST + compactSize compactTokens + compactSize compactSource
  
  return CompactParseResult
    { cprAST = compactAST
    , cprTokens = compactTokens
    , cprSource = compactSource  
    , cprSize = fromIntegral totalSize
    }

-- | Extract AST from compact representation
extractAST :: CompactParseResult -> JSAST
extractAST = getCompact . cprAST

-- | Extract tokens from compact representation
extractTokens :: CompactParseResult -> [FastToken]
extractTokens = getCompact . cprTokens

-- | Extract source from compact representation  
extractSource :: CompactParseResult -> ByteString
extractSource = getCompact . cprSource
```

### 4.2 Streaming Support for Large Files
**File:** `src/Language/JavaScript/Parser/Streaming.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Language.JavaScript.Parser.Streaming where

import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Language.JavaScript.Parser.FastLexer
import Language.JavaScript.Parser.FastToken
import Language.JavaScript.Parser.ParseError
import System.IO

-- | Streaming lexer configuration
data StreamConfig = StreamConfig
  { scChunkSize :: !Int                    -- Bytes per chunk
  , scBufferSize :: !Int                   -- Token buffer size
  , scConcurrency :: !Int                  -- Number of lexer threads
  } deriving (Show)

-- | Default streaming configuration  
defaultStreamConfig :: StreamConfig
defaultStreamConfig = StreamConfig
  { scChunkSize = 64 * 1024                -- 64KB chunks
  , scBufferSize = 1000                     -- 1000 token buffer
  , scConcurrency = 4                       -- 4 concurrent lexers
  }

-- | Stream tokens from large file
streamTokensFromFile :: StreamConfig -> FilePath -> IO (TBQueue (Either ParseError FastToken))
streamTokensFromFile config filename = do
  queue <- newTBQueueIO (scBufferSize config)
  
  _ <- forkIO $ do
    handle <- openBinaryFile filename ReadMode
    streamFromHandle config handle queue
    hClose handle
    atomically $ closeTBQueue queue
    
  return queue

-- | Stream tokens from handle  
streamFromHandle :: StreamConfig -> Handle -> TBQueue (Either ParseError FastToken) -> IO ()
streamFromHandle StreamConfig{..} handle queue = do
  chunk <- BS.hGet handle scChunkSize
  unless (BS.null chunk) $ do
    case runFastLexer chunk of
      Left errors -> mapM_ (atomically . writeTBQueue queue . Left) errors
      Right tokens -> mapM_ (atomically . writeTBQueue queue . Right) tokens
    streamFromHandle StreamConfig{..} handle queue

-- | Consume token stream
consumeTokens :: TBQueue (Either ParseError FastToken) -> IO ([ParseError], [FastToken])
consumeTokens queue = go [] []
  where
    go errors tokens = do
      maybeToken <- atomically $ readTBQueue queue
      case maybeToken of
        Nothing -> return (reverse errors, reverse tokens)
        Just (Left err) -> go (err:errors) tokens
        Just (Right token) -> go errors (token:tokens)

-- | Stream-based parsing for very large files
parseStreamingFile :: StreamConfig -> FilePath -> IO (Either [ParseError] JSAST)
parseStreamingFile config filename = do
  tokenQueue <- streamTokensFromFile config filename
  (errors, tokens) <- consumeTokens tokenQueue
  
  if null errors
    then case parseProgram tokens of
      Left parseErrors -> return $ Left parseErrors
      Right ast -> return $ Right ast
    else return $ Left errors
```

---

## Phase 5: Testing & Validation (Week 7-8)

### 5.1 Comprehensive Test Suite
**File:** `test/Language/JavaScript/Parser/FastParserTest.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Language.JavaScript.Parser.FastParserTest where

import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.CPUTime (getCPUTime)
import Test.Hspec

import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.FastParser
import Language.JavaScript.Parser.ParseError

-- | Performance test suite
performanceSpecs :: Spec
performanceSpecs = describe "Performance Tests" $ do
  
  describe "jQuery-style parsing" $ do
    it "parses under 200ms" $ do
      jqueryCode <- createJQueryStyleCode
      (parseTime, result) <- timeParseAction (parseFromByteString defaultParserOptions jqueryCode "jquery.js")
      
      parseTime `shouldSatisfy` (< 200)  -- 200ms target
      prResult result `shouldSatisfy` isRight
      
    it "achieves >1 MB/s throughput" $ do
      jqueryCode <- createJQueryStyleCode  
      let inputSize = BS.length jqueryCode
      
      (parseTime, result) <- timeParseAction (parseFromByteString defaultParserOptions jqueryCode "jquery.js")
      prResult result `shouldSatisfy` isRight
      
      let throughputMBs = fromIntegral inputSize / (fromIntegral parseTime / 1000.0) / (1024 * 1024)
      throughputMBs `shouldSatisfy` (> 1.0)  -- >1 MB/s
      
  describe "Large file parsing" $ do
    it "parses 1MB files under 1000ms" $ do
      largeCode <- createLargeJavaScriptCode (1024 * 1024)  -- 1MB
      (parseTime, result) <- timeParseAction (parseFromByteString defaultParserOptions largeCode "large.js")
      
      parseTime `shouldSatisfy` (< 1000)  -- 1000ms target
      prResult result `shouldSatisfy` isRight
      
    it "parses 10MB files under 2000ms" $ do
      veryLargeCode <- createLargeJavaScriptCode (10 * 1024 * 1024)  -- 10MB
      (parseTime, result) <- timeParseAction (parseFromByteString defaultParserOptions veryLargeCode "very-large.js")
      
      parseTime `shouldSatisfy` (< 2000)  -- 2s target
      prResult result `shouldSatisfy` isRight
      
    it "maintains >0.5MB/s throughput for large files" $ do
      largeCode <- createLargeJavaScriptCode (5 * 1024 * 1024)  -- 5MB
      let inputSize = BS.length largeCode
      
      (parseTime, result) <- timeParseAction (parseFromByteString defaultParserOptions largeCode "large.js")
      prResult result `shouldSatisfy` isRight
      
      let throughputMBs = fromIntegral inputSize / (fromIntegral parseTime / 1000.0) / (1024 * 1024)
      throughputMBs `shouldSatisfy` (> 0.5)  -- >0.5 MB/s

-- | Error quality test suite
errorQualitySpecs :: Spec
errorQualitySpecs = describe "Error Quality Tests" $ do

  describe "Syntax error reporting" $ do
    it "provides helpful suggestions for missing semicolons" $ do
      let badCode = "const x = 42\nconst y = 24"
      case parseProgram badCode "test.js" of
        Left [err] -> do
          errorMessage err `shouldContain` "semicolon"
          suggestions err `shouldContain` "Add ';' after statement"
        result -> expectationFailure $ "Expected single error, got: " ++ show result
        
    it "provides context for missing braces" $ do  
      let badCode = "if (true) console.log('test')\nelse console.log('fail'"
      case parseProgram badCode "test.js" of
        Left errors -> do
          length errors `shouldBe` 1
          let err = head errors
          errorMessage err `shouldContain` "brace"
          errorContext err `shouldBe` StatementContext
        Right _ -> expectationFailure "Expected parse error"
        
    it "suggests fixes for common mistakes" $ do
      let badCode = "function foo( { return 42; }"  -- Missing closing paren
      case parseProgram badCode "test.js" of
        Left [err] -> do
          errorMessage err `shouldContain` "parenthesis"
          suggestions err `shouldNotBe` []
        result -> expectationFailure $ "Expected error, got: " ++ show result

  describe "Lexical error reporting" $ do
    it "reports invalid escape sequences with context" $ do
      let badCode = "const str = \"hello\\q world\""  -- Invalid escape
      case parseProgram badCode "test.js" of
        Left [err] -> do
          case err of
            InvalidEscapeSequence{..} -> do
              errorSequence `shouldContain` "\\q"
              suggestions `shouldContain` "escape"
            _ -> expectationFailure $ "Expected InvalidEscapeSequence, got: " ++ show err
        result -> expectationFailure $ "Expected error, got: " ++ show result
        
    it "reports invalid numeric literals" $ do
      let badCode = "const num = 1.2.3"  -- Invalid number
      case parseProgram badCode "test.js" of
        Left [err] -> do
          case err of
            InvalidNumericLiteral{..} -> do
              errorLiteral `shouldContain` "1.2.3"
              suggestions `shouldNotBe` []
            _ -> expectationFailure $ "Expected InvalidNumericLiteral, got: " ++ show err
        result -> expectationFailure $ "Expected error, got: " ++ show result

  describe "Error recovery" $ do
    it "recovers from missing semicolons and continues parsing" $ do
      let badCode = "const x = 42\nconst y = 24;\nconst z = 84;"
      let opts = defaultParserOptions { poRecoveryMode = True }
      let ParseResult result warnings tokens source = parseFromByteString opts (BS8.pack badCode) "test.js"
      
      case result of
        Left errors -> length errors `shouldBe` 1  -- Only one error
        Right ast -> expectationFailure "Expected error with recovery"
        
    it "provides multiple errors in one pass" $ do
      let badCode = "const x =;\nfunction foo( {\nconst y = 1.2.3;"
      let opts = defaultParserOptions { poRecoveryMode = True, poMaxErrors = 5 }
      let ParseResult result warnings tokens source = parseFromByteString opts (BS8.pack badCode) "test.js"
      
      case result of  
        Left errors -> length errors `shouldSatisfy` (> 1)
        Right _ -> expectationFailure "Expected multiple errors"

-- | Performance measurement utilities
timeParseAction :: ParseResult a -> IO (Int, ParseResult a)
timeParseAction parseResult = do
  start <- getCPUTime
  result <- evaluate (force parseResult)
  end <- getCPUTime
  let timeMs = fromIntegral (end - start) `div` (10^9)  -- Convert to milliseconds
  return (timeMs, result)

-- | Test data generators  
createJQueryStyleCode :: IO ByteString
createJQueryStyleCode = do
  let jqueryPattern = Text.unlines
        [ "$.fn.extend({"
        , "  addClass: function(value) {"
        , "    return this.each(function() {"
        , "      $(this).toggleClass(value, true);"
        , "    });"
        , "  },"
        , "  removeClass: function(value) {"
        , "    return this.each(function() {"
        , "      $(this).toggleClass(value, false);"
        , "    });"
        , "  }"
        , "});"
        ]
  -- Repeat pattern to simulate jQuery-sized file (~280KB)
  let repeatedCode = Text.concat (replicate 100 jqueryPattern)
  return (Text.encodeUtf8 repeatedCode)

createLargeJavaScriptCode :: Int -> IO ByteString
createLargeJavaScriptCode targetSize = do
  let pattern = Text.unlines
        [ "function processData" <> Text.pack (show i) <> "(data) {"
        , "  if (!data || data.length === 0) {"
        , "    return null;"
        , "  }"
        , "  const result = data.map(item => {"
        , "    return {"
        , "      id: item.id,"
        , "      value: item.value * 2,"
        , "      processed: true"
        , "    };"
        , "  });"
        , "  return result.filter(item => item.value > 10);"
        , "}"
        ] | i <- [1..1000]]
  
  let baseCode = Text.concat pattern
  let currentSize = BS.length (Text.encodeUtf8 baseCode)
  
  if currentSize >= targetSize
    then return (Text.encodeUtf8 baseCode)
    else do
      -- Repeat until target size  
      let repetitions = (targetSize `div` currentSize) + 1
      let expandedCode = Text.concat (replicate repetitions baseCode)
      return (BS.take targetSize (Text.encodeUtf8 expandedCode))

-- | Utility functions
isRight :: Either a b -> Bool
isRight (Right _) = True  
isRight (Left _) = False

isLeft :: Either a b -> Bool
isLeft = not . isRight
```

### 5.2 Benchmark Suite
**File:** `bench/ParserBench.hs`

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.DeepSeq (force)  
import Control.Exception (evaluate)
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Language.JavaScript.Parser.FastParser
import qualified Language.JavaScript.Parser as OldParser -- Original parser

main :: IO ()
main = do
  -- Prepare test data
  smallJS <- createSmallJavaScript      -- ~1KB
  mediumJS <- createMediumJavaScript    -- ~50KB  
  largeJS <- createLargeJavaScript      -- ~1MB
  jqueryJS <- loadJQueryCode            -- ~280KB real jQuery
  
  defaultMain
    [ bgroup "Small files (<10KB)"
      [ bench "language-javascript (old)" $ nf parseOld (BS8.unpack smallJS)
      , bench "language-javascript (new)" $ nf parseNew smallJS
      ]
      
    , bgroup "Medium files (50KB)"
      [ bench "language-javascript (old)" $ nf parseOld (BS8.unpack mediumJS)
      , bench "language-javascript (new)" $ nf parseNew mediumJS  
      ]
      
    , bgroup "Large files (1MB)"
      [ bench "language-javascript (new)" $ nf parseNew largeJS
      ]
      
    , bgroup "Real-world code"
      [ bench "jQuery (280KB)" $ nf parseNew jqueryJS
      ]
      
    , bgroup "Memory usage"
      [ bench "Parse + hold reference (1MB)" $ nf parseAndHold largeJS
      ]
    ]
  where
    parseOld source = OldParser.parse source "bench.js"
    parseNew source = parseFromByteString defaultParserOptions source "bench.js"
    parseAndHold source = 
      let result = parseFromByteString defaultParserOptions source "bench.js"
      in (prResult result, prTokens result)  -- Hold both AST and tokens

-- Test data creation functions
createSmallJavaScript :: IO ByteString
createSmallJavaScript = return $ Text.encodeUtf8 $ Text.unlines
  [ "function hello(name) {"
  , "  console.log('Hello, ' + name + '!');"
  , "}"
  , ""
  , "const users = ['Alice', 'Bob', 'Charlie'];"
  , "users.forEach(hello);"
  ]

createMediumJavaScript :: IO ByteString  
createMediumJavaScript = do
  let pattern = Text.unlines
        [ "class Component" <> Text.pack (show i) <> " {"
        , "  constructor(props) {"
        , "    this.props = props;"
        , "    this.state = { count: 0 };"
        , "  }"
        , ""
        , "  render() {"
        , "    return {"
        , "      tag: 'div',"  
        , "      children: ["
        , "        { tag: 'h1', text: `Component ${this.props.name}` },"
        , "        { tag: 'p', text: `Count: ${this.state.count}` },"
        , "        { tag: 'button', onClick: () => this.setState({ count: this.state.count + 1 }), text: 'Increment' }"
        , "      ]"
        , "    };"
        , "  }"
        , "}"
        ] | i <- [1..200]]
  return (Text.encodeUtf8 (Text.concat pattern))

createLargeJavaScript :: IO ByteString
createLargeJavaScript = do
  medium <- createMediumJavaScript
  let mediumSize = BS.length medium
  let targetSize = 1024 * 1024  -- 1MB
  let repetitions = targetSize `div` mediumSize + 1
  return (BS.take targetSize (BS.concat (replicate repetitions medium)))

loadJQueryCode :: IO ByteString  
loadJQueryCode = do
  -- Load actual jQuery code for realistic benchmarking
  -- In real implementation, this would load jquery.min.js
  jqueryPattern <- createJQueryStyleCode
  return jqueryPattern
  where
    createJQueryStyleCode = return $ Text.encodeUtf8 $ Text.concat $ replicate 50 $
      Text.unlines
        [ "(function($) {"
        , "  $.fn.slideUp = function(duration, callback) {"
        , "    return this.each(function() {"
        , "      var $this = $(this);"
        , "      var height = $this.height();"
        , "      $this.animate({ height: 0 }, duration, function() {"
        , "        $this.hide();"
        , "        if (callback) callback.call(this);"
        , "      });"
        , "    });"
        , "  };"
        , "})(jQuery);"
        ]
```

---

## Phase 6: Integration & Documentation (Week 9-10)

### 6.1 Update Main Module Interface  
**File:** `src/Language/JavaScript/Parser.hs`

```haskell
-- BREAKING CHANGES: New interface for version 1.0.0
module Language.JavaScript.Parser
  ( -- * High-performance parsing
    parseProgram
  , parseModule
  , parseExpression  
  , parseStatement
  , parseFromFile
  , parseFromByteString
    -- * Parser configuration
  , ParserOptions(..)
  , defaultParserOptions
    -- * Parse results
  , ParseResult(..)
    -- * Error handling
  , ParseError(..)
  , ErrorSeverity(..)
  , ParseContext(..)
  , RecoveryStrategy(..)
  , renderParseErrors
    -- * Token access (for tooling)
  , FastToken(..)
  , TokenType(..)
  , fastTokenText
  , fastTokenBytes
    -- * AST types (re-exported)
  , JSAST(..)
  , JSStatement(..)
  , JSExpression(..)
  , JSAnnot(..)
  , module Language.JavaScript.Parser.AST
  ) where

import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.FastParser
import Language.JavaScript.Parser.FastToken  
import Language.JavaScript.Parser.ParseError
```

### 6.2 Migration Guide
**File:** `MIGRATION.md`

```markdown
# Migration Guide: language-javascript 1.0.0

## Breaking Changes Summary

### New Parser Interface
```haskell
-- OLD (0.x):
parse :: String -> String -> Either String JSAST

-- NEW (1.0):  
parseProgram :: String -> FilePath -> Either [ParseError] JSAST
parseFromByteString :: ParserOptions -> ByteString -> FilePath -> ParseResult JSAST
```

### Enhanced Error Information
```haskell
-- OLD: Generic string errors
Left "Unexpected token at line 5"

-- NEW: Structured errors with context
Left [UnexpectedToken { 
  errorToken = token,
  errorContext = FunctionContext,  
  expectedTokens = [SemiColonTok],
  suggestions = ["Add ';' after statement"]
}]
```

### Performance Improvements
- **5x faster** parsing on large files
- **3x lower** memory usage  
- **ByteString input** support for zero-copy parsing
- **Streaming support** for multi-GB files

## Migration Steps

### 1. Update Dependencies
```cabal
-- In .cabal file:
build-depends: language-javascript >= 1.0.0 && < 2.0.0
```

### 2. Update Imports
```haskell
-- OLD:
import Language.JavaScript.Parser (parse)

-- NEW:
import Language.JavaScript.Parser (parseProgram, ParseError(..), renderParseErrors)
```

### 3. Update Error Handling
```haskell
-- OLD:
case parse source filename of
  Left err -> putStrLn err
  Right ast -> processAST ast

-- NEW:
case parseProgram source filename of  
  Left errors -> putStrLn (Text.unpack (renderParseErrors errors source))
  Right ast -> processAST ast
```

### 4. Leverage New Features

#### High-Performance Parsing
```haskell
-- For maximum performance with large files:
import qualified Data.Text.Encoding as Text

let sourceBytes = Text.encodeUtf8 (Text.pack source)
    options = defaultParserOptions { poRecoveryMode = True }
    
case parseFromByteString options sourceBytes filename of
  ParseResult (Right ast) warnings tokens _ -> do
    processAST ast
    when (not (null warnings)) $ reportWarnings warnings
```

#### Error Recovery
```haskell
-- Enable error recovery for IDE-like experience:
let options = defaultParserOptions { 
      poRecoveryMode = True,
      poMaxErrors = 20 
    }
    
case parseFromByteString options source filename of
  ParseResult (Left errors) _ _ _ -> 
    -- Show multiple errors at once
    mapM_ showError errors
  ParseResult (Right ast) warnings _ _ ->
    -- AST may be partial but usable
    processPartialAST ast
```

#### Token Access for IDE Features
```haskell
let ParseResult result warnings tokens source = parseFromByteString options input filename

-- Use tokens for syntax highlighting, auto-complete, etc.
let identifiers = [fastTokenText t | t <- tokens, ftType t == IdentifierTok]
    keywords = [fastTokenText t | t <- tokens, isKeyword (ftType t)]
```

## Performance Optimization Tips

### 1. Use ByteString Input
```haskell
-- SLOW: String conversion overhead
parseProgram stringSource filename

-- FAST: Direct ByteString processing  
parseFromByteString options bytestringSource filename
```

### 2. Configure Parser Options
```haskell
-- For maximum speed:
let fastOptions = ParserOptions {
  poStrictMode = False,      -- Skip strict mode validation
  poCollectTokens = False,   -- Skip token collection if not needed
  poRecoveryMode = False,    -- Skip error recovery for speed
  poMaxErrors = 1            -- Stop at first error
}
```

### 3. Stream Large Files
```haskell
-- For files > 100MB:
import Language.JavaScript.Parser.Streaming

result <- parseStreamingFile defaultStreamConfig "huge-file.js"
```

## Common Migration Patterns

### Pattern 1: Basic Parsing
```haskell
-- OLD:
parseJavaScript :: String -> Either String JSAST
parseJavaScript source = parse source "input.js"

-- NEW:  
parseJavaScript :: String -> Either [ParseError] JSAST
parseJavaScript source = parseProgram source "input.js"
```

### Pattern 2: File Parsing
```haskell
-- OLD:
parseFile :: FilePath -> IO (Either String JSAST)
parseFile filename = do
  content <- readFile filename
  return $ parse content filename

-- NEW:
parseFile :: FilePath -> IO (Either [ParseError] JSAST)  
parseFile filename = do
  result <- parseFromFile defaultParserOptions filename
  return $ prResult result
```

### Pattern 3: Error Reporting
```haskell
-- OLD:
reportError :: Either String JSAST -> IO ()
reportError (Left err) = putStrLn $ "Parse error: " ++ err
reportError (Right _) = return ()

-- NEW:
reportErrors :: ByteString -> Either [ParseError] JSAST -> IO ()
reportErrors source (Left errors) = do
  let errorText = renderParseErrors errors source
  putStrLn $ Text.unpack errorText
reportErrors _ (Right _) = return ()
```

## Compatibility Notes

### AST Structure
- AST node types remain the same
- Node constructors unchanged  
- Pretty printing functions unchanged

### Token Types
- New FastToken type for internal use
- Old Token type still available for compatibility
- Conversion functions provided

### Source Locations  
- TokenPosn type unchanged
- Line/column numbering unchanged
- Character offset tracking improved

## Troubleshooting

### Common Issues

#### "Module not found" errors
```haskell
-- If you get import errors, update imports:
import Language.JavaScript.Parser.ParseError  -- NEW module
```

#### Performance regressions
```haskell
-- Ensure you're using ByteString input for best performance:
let source = Text.encodeUtf8 (Text.pack stringSource)
parseFromByteString defaultParserOptions source filename
```

#### Missing error context
```haskell
-- Pattern match on specific error types for detailed information:
case parseProgram source filename of
  Left (UnexpectedToken{..} : _) -> do
    putStrLn $ "Unexpected " ++ show errorToken
    putStrLn $ "Expected one of: " ++ show expectedTokens
    mapM_ putStrLn suggestions
```

## Getting Help

- **Documentation**: See updated Haddock documentation  
- **Examples**: Check `examples/` directory
- **Issues**: Report bugs at https://github.com/erikd/language-javascript/issues
- **Performance**: Use `+RTS -s -RTS` to profile memory usage
```