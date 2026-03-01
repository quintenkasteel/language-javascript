{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O2 #-}

-- | Core lexical analysis using flatparse for JavaScript parsing.
--
-- This module provides high-performance lexical analysis for JavaScript
-- source code using flatparse combinators. It implements all JavaScript
-- lexical elements including:
--
--   * String literals with full escape sequence support
--   * Numeric literals (decimal, hex, binary, octal, BigInt)
--   * Identifiers and keywords with Unicode support
--   * Operators and punctuation
--   * Comments and whitespace handling
--
-- ==== Design Principles
--
--   * **Performance**: Use Template Haskell switches for optimal dispatch
--   * **Correctness**: Full ECMAScript specification compliance
--   * **Position tracking**: Accurate source location information
--   * **Error handling**: Rich error messages with context
--
-- ==== Examples
--
-- String literal parsing:
--
-- >>> runParser stringLiteral "\"hello world\""
-- Right "hello world"
--
-- >>> runParser stringLiteral "\"line1\\nline2\""
-- Right "line1\nline2"
--
-- Numeric literal parsing:
--
-- >>> runParser numericLiteral "42.5e-10"
-- Right "42.5e-10"
--
-- >>> runParser numericLiteral "0x1BEEF"
-- Right "0x1BEEF"
--
-- @since 0.8.0.0
module Language.JavaScript.Parser.Lexer
  ( -- * Core Parser Type
    JSParser,

    -- * String Literals
    stringLiteral,

    -- * Numeric Literals
    numericLiteral,

    -- * Identifiers and Keywords
    identifier,
    rawIdentifier,
    keyword,
    isKeyword,

    -- * Whitespace and Comments
    whitespace,
    lineComment,
    blockComment,

    -- * Position Utilities
    withPos,

    -- * Error Handling
    parseError,
    unexpected,
  )
where

import Data.ByteString (ByteString)
import Data.Char (chr, digitToInt)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import FlatParse.Basic (Pos, (<|>), many, some, satisfy, anyChar, optional, skipMany, empty)
import qualified FlatParse.Basic as FP
import Language.JavaScript.Parser.Primitives (JSParser, ParseError(..), isIdentifierStart, isIdentifierContinue, isWhitespace, isDecimalDigit, isBinaryDigit, isOctalDigit, isHexDigit)
import qualified Language.JavaScript.Parser.Pos as JSPos

-- ---------------------------------------------------------------------
-- Core Parser Infrastructure
-- ---------------------------------------------------------------------

-- | Parse specific ASCII character using native byte comparison.
-- Avoids UTF-8 decode overhead for the common case of matching ASCII punctuation.
{-# INLINE parseChar #-}
parseChar :: Char -> JSParser ()
parseChar c = FP.word8 (fromIntegral (fromEnum c))

-- | Parse specific ASCII string using native memcmp.
-- Takes 'ByteString' directly to avoid runtime 'pack' overhead;
-- with @OverloadedStrings@, string literals become compile-time constants.
{-# INLINE parseString #-}
parseString :: ByteString -> JSParser ()
parseString = FP.byteString

-- | Create a fatal parse error at current position.
-- Uses 'FP.err' to throw a non-backtrackable error with the message.
parseError :: Text -> JSParser a
parseError msg = FP.err (SyntaxError (JSPos.mkPos 1 1) msg [])

-- | Report unexpected character as a fatal error.
unexpected :: Char -> Text -> JSParser a
unexpected found expected =
  FP.err (UnexpectedChar (JSPos.mkPos 1 1) found expected)

-- | Parse with position tracking.
withPos :: JSParser a -> JSParser (Pos, a)
withPos parser = do
  pos <- FP.getPos
  result <- parser
  pure (pos, result)

-- ---------------------------------------------------------------------
-- String Literals
-- ---------------------------------------------------------------------

-- | Parse any JavaScript string literal with escape processing.
--
-- Handles both single and double quoted strings with full escape
-- sequence support including Unicode escapes. Returns processed content
-- as UTF-8 encoded ByteString.
stringLiteral :: JSParser ByteString
stringLiteral = Text.encodeUtf8 <$> (singleQuotedString <|> doubleQuotedString)

-- | Parse single-quoted string literal.
singleQuotedString :: JSParser Text
singleQuotedString = do
  parseChar '\''
  content <- Text.pack <$> many (stringChar '\'')
  parseChar '\''
  pure content

-- | Parse double-quoted string literal.
doubleQuotedString :: JSParser Text
doubleQuotedString = do
  parseChar '"'
  content <- Text.pack <$> many (stringChar '"')
  parseChar '"'
  pure content

-- | Parse character inside string literal.
stringChar :: Char -> JSParser Char
stringChar quote =
  (parseChar '\\' *> escapeSequence) <|>
  satisfy (\c -> c /= quote && c /= '\\' && c /= '\n' && c /= '\r')

-- | Parse escape sequence inside string.
escapeSequence :: JSParser Char
escapeSequence = do
  c <- anyChar
  case c of
    'n'  -> pure '\n'
    't'  -> pure '\t'
    'r'  -> pure '\r'
    'b'  -> pure '\b'
    'f'  -> pure '\f'
    'v'  -> pure '\v'
    '0'  -> pure '\0'
    '\\' -> pure '\\'
    '\'' -> pure '\''
    '"'  -> pure '"'
    'u'  -> unicodeEscape
    'x'  -> hexEscape
    '\n' -> pure '\n'  -- Line continuation
    '\r' -> do
      _ <- optional (parseChar '\n')  -- Handle CRLF
      pure '\n'
    _    -> parseError "Invalid escape sequence"

-- | Parse Unicode escape sequence (\uXXXX or \u{XXXXXX}).
unicodeEscape :: JSParser Char
unicodeEscape = bracedUnicode <|> fixedUnicode
  where
    bracedUnicode = do
      parseChar '{'
      digits <- some hexDigit
      parseChar '}'
      let codePoint = hexToInt digits
      if codePoint <= 0x10FFFF
        then pure (chr codePoint)
        else parseError "Invalid Unicode code point"

    fixedUnicode = do
      d1 <- hexDigit
      d2 <- hexDigit
      d3 <- hexDigit
      d4 <- hexDigit
      pure (chr (hexToInt [d1, d2, d3, d4]))

-- | Parse hex escape sequence (\xXX).
hexEscape :: JSParser Char
hexEscape = do
  d1 <- hexDigit
  d2 <- hexDigit
  pure (chr (hexToInt [d1, d2]))

-- | Parse hexadecimal digit.
hexDigit :: JSParser Char
hexDigit = satisfy isHexDigit

-- | Convert hex digits to integer.
hexToInt :: [Char] -> Int
hexToInt = foldl (\acc c -> acc * 16 + digitToInt c) 0

-- ---------------------------------------------------------------------
-- Numeric Literals
-- ---------------------------------------------------------------------

-- | Parse any JavaScript numeric literal as zero-copy ByteString slice.
--
-- Uses 'FP.byteStringOf' to capture the raw numeric literal directly from
-- the input buffer. Supports all numeric formats: decimal, hex, binary,
-- octal, BigInt, and scientific notation.
numericLiteral :: JSParser ByteString
numericLiteral = FP.byteStringOf numericLiteralConsume

-- | Consume a numeric literal (internal, result discarded by byteStringOf).
numericLiteralConsume :: JSParser ()
numericLiteralConsume =
  consumeBigInt <|>
  consumeHex <|>
  consumeBinary <|>
  consumeOctal <|>
  consumeDecimal <|>
  consumeDotDecimal
  where
    consumeBigInt = (consumeHex <|> consumeBinary <|> consumeOctal <|> consumeDecimal) *> parseChar 'n'
    consumeHex = parseChar '0' *> (parseChar 'x' <|> parseChar 'X') *> skipSome hexDigitWithSeparator
    consumeBinary = parseChar '0' *> (parseChar 'b' <|> parseChar 'B') *> skipSome binaryDigitWithSeparator
    consumeOctal = parseChar '0' *> (parseChar 'o' <|> parseChar 'O') *> skipSome octalDigitWithSeparator
    consumeDecimal = do
      _ <- digitChar
      skipMany_ digitCharWithSeparator
      _ <- optional (parseChar '.' *> skipMany_ digitCharWithSeparator)
      _ <- optional consumeExponent
      pure ()
    consumeDotDecimal = do
      parseChar '.'
      _ <- FP.lookahead digitChar
      skipSome digitCharWithSeparator
      _ <- optional consumeExponent
      pure ()
    consumeExponent = do
      _ <- parseChar 'e' <|> parseChar 'E'
      _ <- optional (satisfy (\c -> c == '+' || c == '-'))
      skipSome digitChar

-- | Skip one or more occurrences.
skipSome :: JSParser a -> JSParser ()
skipSome p = p *> skipMany p *> pure ()

-- | Skip zero or more occurrences (discarding results).
skipMany_ :: JSParser a -> JSParser ()
skipMany_ p = skipMany p *> pure ()

-- | Parse decimal digit.
digitChar :: JSParser Char
digitChar = satisfy isDecimalDigit

-- | Parse decimal digit with optional numeric separators (ES2021).
-- Separators are only consumed when followed by another digit.
digitCharWithSeparator :: JSParser Char
digitCharWithSeparator = digitChar <|> separatorBeforeDigit isDecimalDigit

-- | Parse binary digit.
binaryDigit :: JSParser Char
binaryDigit = satisfy isBinaryDigit

-- | Parse binary digit with optional numeric separators.
binaryDigitWithSeparator :: JSParser Char
binaryDigitWithSeparator = binaryDigit <|> separatorBeforeDigit isBinaryDigit

-- | Parse octal digit.
octalDigit :: JSParser Char
octalDigit = satisfy isOctalDigit

-- | Parse octal digit with optional numeric separators.
octalDigitWithSeparator :: JSParser Char
octalDigitWithSeparator = octalDigit <|> separatorBeforeDigit isOctalDigit

-- | Parse hex digit with optional numeric separators.
hexDigitWithSeparator :: JSParser Char
hexDigitWithSeparator = hexDigit <|> separatorBeforeDigit isHexDigit

-- | Parse numeric separator only when followed by a valid digit.
separatorBeforeDigit :: (Char -> Bool) -> JSParser Char
separatorBeforeDigit isDigitType = do
  parseChar '_'
  _ <- FP.lookahead (satisfy isDigitType)
  pure '_'

-- ---------------------------------------------------------------------
-- Identifiers and Keywords
-- ---------------------------------------------------------------------

-- | Parse JavaScript identifier as zero-copy ByteString slice.
--
-- Uses 'FP.byteStringOf' to capture the identifier directly from the input
-- buffer without intermediate allocation. Rejects keywords with
-- backtrackable failure so callers can try alternative parsers.
identifier :: JSParser ByteString
identifier = do
  ident <- FP.byteStringOf (satisfy isIdentifierStart *> skipMany (satisfy isIdentifierContinue))
  if isKeyword ident
    then empty
    else pure ident

-- | Parse specific keyword (backtrackable on mismatch).
--
-- Matches the keyword bytes directly using native memcmp, then checks
-- that the next character is not an identifier continuation. This fails
-- on the first non-matching byte instead of reading the entire identifier.
keyword :: ByteString -> JSParser ()
keyword kw = FP.byteString kw *> notIdentCont
  where
    notIdentCont = do
      mc <- optional (FP.lookahead FP.anyChar)
      case mc of
        Just c | isIdentifierContinue c -> empty
        _ -> pure ()

-- | Parse identifier without keyword check as zero-copy ByteString slice.
rawIdentifier :: JSParser ByteString
rawIdentifier = FP.byteStringOf (satisfy isIdentifierStart *> skipMany (satisfy isIdentifierContinue))

-- | Check if a ByteString is a JavaScript keyword.
-- Uses a Set for O(log n) lookup instead of O(n) list membership.
isKeyword :: ByteString -> Bool
isKeyword bs = Set.member bs keywordSet

-- | Set of all JavaScript keywords for efficient lookup.
-- Top-level CAF ensures the Set is built once.
keywordSet :: Set ByteString
keywordSet = Set.fromList
  [ "break", "case", "catch", "class", "const", "continue"
  , "debugger", "default", "delete", "do", "else", "export"
  , "extends", "false", "finally", "for", "function", "if"
  , "import", "in", "instanceof", "new", "null", "return"
  , "super", "switch", "this", "throw", "true", "try"
  , "typeof", "var", "void", "while", "with"
  , "let", "static", "enum", "implements", "package"
  , "protected", "interface", "private", "public"
  , "await", "async"
  ]

-- ---------------------------------------------------------------------
-- Whitespace and Comments
-- ---------------------------------------------------------------------

-- | Skip whitespace including comments.
-- Optimized to try ASCII whitespace first (the 99.9% case) before comments
-- or Unicode whitespace, avoiding UTF-8 decode overhead for common characters.
whitespace :: JSParser ()
whitespace = skipMany (asciiWS <|> lineComment <|> blockComment <|> unicodeWS)
  where
    asciiWS = FP.skipSatisfyAscii isSimpleWS
    isSimpleWS c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v'
    unicodeWS = FP.skipSatisfy (\c -> c > '\x7f' && isWhitespace c)

-- | Parse single-line comment.
-- Terminates at any ECMAScript line terminator: LF, CR, LS, PS.
lineComment :: JSParser ()
lineComment = do
  parseString "//"
  skipMany (satisfy (not . isLineTerminator))
  _ <- optional (satisfy isLineTerminator)
  pure ()

-- | Check if character is a JavaScript line terminator.
isLineTerminator :: Char -> Bool
isLineTerminator '\n' = True
isLineTerminator '\r' = True
isLineTerminator '\x2028' = True
isLineTerminator '\x2029' = True
isLineTerminator _ = False

-- | Parse multi-line comment.
-- Handles consecutive @*@ before @/@ correctly (e.g., @/* **/@).
blockComment :: JSParser ()
blockComment = do
  parseString "/*"
  commentBody
  where
    commentBody = do
      skipMany (satisfy (/= '*'))
      parseChar '*'
      closingOrContinue
    closingOrContinue = do
      c <- anyChar
      case c of
        '/' -> pure ()
        '*' -> closingOrContinue
        _   -> commentBody