{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

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
module Language.JavaScript.Parser.Flatparse.Lexer
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
    ParseError (..),
    parseError,
    unexpected,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Char (chr, digitToInt)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import FlatParse.Basic (Pos, (<|>), many, some, satisfy, anyChar, optional, skipMany, empty)
import Control.Applicative (pure, (*>))
import Control.Monad (mapM_)
import qualified FlatParse.Basic as FP
import qualified Language.JavaScript.Parser.Flatparse.Pos as JSPos
import Language.JavaScript.Parser.Flatparse.Primitives (JSParser, isIdentifierStart, isIdentifierContinue, isWhitespace, isDecimalDigit, isBinaryDigit, isOctalDigit, isHexDigit)

-- ---------------------------------------------------------------------
-- Core Parser Infrastructure
-- ---------------------------------------------------------------------

-- | Parse specific character
parseChar :: Char -> JSParser ()
parseChar c = do
  actual <- FP.anyChar
  if actual == c
    then pure ()
    else FP.empty

-- | Parse specific string
parseString :: String -> JSParser ()
parseString str = mapM_ parseChar str

-- | Parse error with position and context information.
data ParseError
  = SyntaxError !JSPos.Pos !Text ![Text]     -- position, message, suggestions
  | UnexpectedEOF !JSPos.Pos                 -- position
  | UnexpectedChar !JSPos.Pos !Char !Text    -- position, found, expected
  | InvalidEscape !JSPos.Pos !Text           -- position, escape sequence
  | InvalidNumeric !JSPos.Pos !Text          -- position, numeric format
  deriving (Eq, Show)

-- | Create a parse error at current position.
parseError :: Text -> JSParser a
parseError msg = FP.err (Text.encodeUtf8 msg)

-- | Report unexpected character.
unexpected :: Char -> Text -> JSParser a
unexpected found expected = FP.err (BS8.pack ("unexpected '" ++ [found] ++ "', expected " ++ Text.unpack expected))

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

-- | Parse decimal literal with optional fractional and exponent parts.
-- Supports ES2021 numeric separators (underscores) - preserves original format.
-- The first character must be an actual digit (not a separator).
decimalLiteral :: JSParser Text
decimalLiteral = do
  first <- digitChar
  rest <- many digitCharWithSeparator
  let integer = Text.pack (first : rest)
  fractional <- optional (parseChar '.' *> (Text.pack <$> many digitCharWithSeparator))
  exponent <- optional exponentPart
  pure (integer <> maybe "" ("." <>) fractional <> maybe "" id exponent)

-- | Parse decimal literal starting with @.@ (e.g., @.5@, @.123@).
-- Uses lookahead to only consume @.@ when followed by a digit,
-- preventing ambiguity with member access operator.
dotDecimalLiteral :: JSParser Text
dotDecimalLiteral = do
  parseChar '.'
  _ <- FP.lookahead digitChar
  digits <- some digitCharWithSeparator
  exponent <- optional exponentPart
  pure ("." <> Text.pack digits <> maybe "" id exponent)

-- | Parse scientific notation (e.g., 1.5e-10).
scientificNotation :: JSParser Text
scientificNotation = do
  base <- decimalLiteral
  eChar <- (parseChar 'e' *> pure 'e') <|> (parseChar 'E' *> pure 'E')
  sign <- optional ((satisfy (== '+') *> pure '+') <|> (satisfy (== '-') *> pure '-'))
  exponent <- Text.pack <$> some digitChar
  pure (base <> Text.singleton eChar <> maybe "" Text.singleton sign <> exponent)

-- | Parse exponent part of scientific notation.
exponentPart :: JSParser Text
exponentPart = do
  eChar <- (parseChar 'e' *> pure 'e') <|> (parseChar 'E' *> pure 'E')
  sign <- optional ((satisfy (== '+') *> pure '+') <|> (satisfy (== '-') *> pure '-'))
  digits <- Text.pack <$> some digitChar
  pure (Text.singleton eChar <> maybe "" Text.singleton sign <> digits)

-- | Parse hexadecimal literal (0x...) with optional separators.
hexLiteral :: JSParser Text
hexLiteral = do
  prefix1 <- parseChar '0' *> pure '0'
  prefix2 <- (parseChar 'x' *> pure 'x') <|> (parseChar 'X' *> pure 'X')
  digits <- Text.pack <$> some hexDigitWithSeparator
  -- Preserve original format including separators
  pure (Text.pack [prefix1, prefix2] <> digits)

-- | Parse binary literal (0b...) with optional separators.
binaryLiteral :: JSParser Text
binaryLiteral = do
  prefix1 <- parseChar '0' *> pure '0'
  prefix2 <- (parseChar 'b' *> pure 'b') <|> (parseChar 'B' *> pure 'B')
  digits <- Text.pack <$> some binaryDigitWithSeparator
  -- Preserve original format including separators
  pure (Text.pack [prefix1, prefix2] <> digits)

-- | Parse octal literal (0o...) with optional separators.
octalLiteral :: JSParser Text
octalLiteral = do
  prefix1 <- parseChar '0' *> pure '0'
  prefix2 <- (parseChar 'o' *> pure 'o') <|> (parseChar 'O' *> pure 'O')
  digits <- Text.pack <$> some octalDigitWithSeparator
  -- Preserve original format including separators
  pure (Text.pack [prefix1, prefix2] <> digits)

-- | Parse BigInt literal (ends with 'n').
bigIntLiteral :: JSParser Text
bigIntLiteral = do
  base <- hexLiteral <|> binaryLiteral <|> octalLiteral <|> decimalLiteral
  _ <- parseChar 'n'
  pure (base <> "n")

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
-- Parses an identifier-like token and checks it matches the keyword exactly.
-- Uses backtrackable failure so callers can try alternative keywords.
keyword :: ByteString -> JSParser ()
keyword kw = do
  ident <- rawIdentifier
  if ident == kw
    then pure ()
    else empty

-- | Parse identifier without keyword check as zero-copy ByteString slice.
rawIdentifier :: JSParser ByteString
rawIdentifier = FP.byteStringOf (satisfy isIdentifierStart *> skipMany (satisfy isIdentifierContinue))

-- | Check if a ByteString is a JavaScript keyword.
isKeyword :: ByteString -> Bool
isKeyword bs = bs `elem` keywords
  where
    keywords :: [ByteString]
    keywords =
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
whitespace :: JSParser ()
whitespace = skipMany (whitespaceChar <|> lineComment <|> blockComment)

-- | Parse single whitespace character.
whitespaceChar :: JSParser ()
whitespaceChar = satisfy isWhitespace *> pure ()

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