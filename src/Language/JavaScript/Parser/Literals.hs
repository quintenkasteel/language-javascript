{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Self-contained literal parsers for JavaScript.
--
-- This module provides parsers for all JavaScript literal expressions
-- that don't participate in the expression/statement mutual recursion:
--
--   * Keyword literals (@this@, @super@, @null@, @true@, @false@)
--   * String literals (single and double-quoted)
--   * Numeric literals (decimal, hex, binary, octal, BigInt, scientific)
--   * Regular expression literals (@\/pattern\/flags@)
--
-- These parsers only depend on the lexer and parsing utilities — they
-- never call back into expression or statement parsers. This makes them
-- independently compilable and cacheable for incremental builds.
--
-- Note: Complex literals like array literals, object literals, and
-- template literals remain in "Language.JavaScript.Parser.Grammar"
-- because they recursively contain expressions.
--
-- @since 0.8.0.0
module Language.JavaScript.Parser.Literals
  ( -- * Keyword literals
    thisLiteral
  , superLiteral
  , nullLiteral
  , booleanLiteral
    -- * String and numeric literals
  , literalExpression
  , stringLit
  , numericLit
    -- * Numeric classification
  , classifyNumeric
  , parseDecimalValue
  , parseHexValue
  , parseBinaryValue
  , parseOctalValue
  , parseBigIntValue
    -- * Regex literals
  , regexLiteral
  , regexBodySkip
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified FlatParse.Basic as FP

import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Lexer (keyword, numericLiteral)
import Language.JavaScript.Parser.Operators (fpPosToAnnot, parseChar, stringLiteralRaw)
import Language.JavaScript.Parser.Primitives

-- =====================================================================
-- Keyword Literals
-- =====================================================================

-- | Parse @this@ literal.
thisLiteral :: JSParser JSExpression
thisLiteral = do
  pos <- FP.getPos
  keyword "this"
  pure (JSLiteral (fpPosToAnnot pos) "this")

-- | Parse @super@ literal.
superLiteral :: JSParser JSExpression
superLiteral = do
  pos <- FP.getPos
  keyword "super"
  pure (JSLiteral (fpPosToAnnot pos) "super")

-- | Parse @null@ literal.
nullLiteral :: JSParser JSExpression
nullLiteral = do
  pos <- FP.getPos
  keyword "null"
  pure (JSLiteral (fpPosToAnnot pos) "null")

-- | Parse boolean literal.
booleanLiteral :: JSParser JSExpression
booleanLiteral = do
  pos <- FP.getPos
  val <- (keyword "true" *> pure "true") FP.<|> (keyword "false" *> pure "false")
  pure (JSLiteral (fpPosToAnnot pos) val)

-- =====================================================================
-- String and Numeric Literals
-- =====================================================================

-- | Parse string or numeric literal.
literalExpression :: JSParser JSExpression
literalExpression = stringLit FP.<|> numericLit

-- | Parse string literal preserving raw source form (with quotes).
stringLit :: JSParser JSExpression
stringLit = do
  pos <- FP.getPos
  raw <- stringLiteralRaw
  pure (JSStringLiteral (fpPosToAnnot pos) raw)

-- | Parse numeric literal, detecting format.
numericLit :: JSParser JSExpression
numericLit = do
  pos <- FP.getPos
  raw <- numericLiteral
  pure (classifyNumeric (fpPosToAnnot pos) raw)

-- | Classify numeric literal by format and parse into proper numeric type.
-- Decimal literals become 'Double', all integer formats become 'Integer'.
classifyNumeric :: JSAnnot -> ByteString -> JSExpression
classifyNumeric a s
  | hasBigIntSuffix = JSBigIntLiteral a (parseBigIntValue s)
  | hasPrefix "0x" || hasPrefix "0X" = JSHexInteger a (parseHexValue s)
  | hasPrefix "0b" || hasPrefix "0B" = JSBinaryInteger a (parseBinaryValue s)
  | hasOctalPrefix = JSOctal a (parseOctalValue s)
  | otherwise = JSDecimal a (parseDecimalValue s)
  where
    hasBigIntSuffix = not (BS8.null s) && BS8.last s == 'n'
    hasPrefix p = p `BS8.isPrefixOf` s
    hasOctalPrefix
      | BS8.length s >= 2
      , BS8.index s 0 == '0' =
          let c = BS8.index s 1
          in c == 'o' || c == 'O' || (c >= '0' && c <= '7')
      | otherwise = False

-- | Parse a decimal numeric string to Double, stripping numeric separators.
-- Handles leading-dot decimals like @.5@ which Haskell's 'read' rejects.
-- Returns 0 for malformed input rather than crashing.
parseDecimalValue :: ByteString -> Double
parseDecimalValue bs = fromMaybe 0 (readMaybe cleaned)
  where
    cleaned = prependZero (filter (/= '_') (BS8.unpack bs))
    prependZero ('.':rest) = '0' : '.' : rest
    prependZero other = other

-- | Parse a hex numeric string (with 0x prefix) to Integer.
-- Returns 0 for malformed input rather than crashing.
parseHexValue :: ByteString -> Integer
parseHexValue bs = fromMaybe 0 (readMaybe ("0x" <> filter (/= '_') (BS8.unpack (BS8.drop 2 bs))))

-- | Parse a binary numeric string (with 0b prefix) to Integer.
parseBinaryValue :: ByteString -> Integer
parseBinaryValue bs = foldl' (\acc c -> acc * 2 + binDigitVal c) 0 digits
  where
    digits = filter (/= '_') (BS8.unpack (BS8.drop 2 bs))
    binDigitVal '0' = 0
    binDigitVal '1' = 1
    binDigitVal _ = 0

-- | Parse an octal numeric string to Integer.
-- Returns 0 for malformed input rather than crashing.
parseOctalValue :: ByteString -> Integer
parseOctalValue bs
  | BS8.length bs >= 2, c == 'o' || c == 'O' =
      fromMaybe 0 (readMaybe ("0o" <> filter (/= '_') (BS8.unpack (BS8.drop 2 bs))))
  | otherwise =
      fromMaybe 0 (readMaybe ("0o" <> filter (/= '_') (BS8.unpack (BS8.drop 1 bs))))
  where
    c = BS8.index bs 1

-- | Parse a BigInt literal to Integer (strip trailing 'n' and classify base).
-- Handles edge cases like @123.456n@ (truncates to integer) and @123e4n@
-- (evaluates scientific notation then truncates) for parser tolerance.
-- Returns 0 for malformed input rather than crashing.
parseBigIntValue :: ByteString -> Integer
parseBigIntValue bs = classifyAndParse (BS8.init bs)
  where
    classifyAndParse s
      | "0x" `BS8.isPrefixOf` s || "0X" `BS8.isPrefixOf` s = parseHexValue s
      | "0b" `BS8.isPrefixOf` s || "0B" `BS8.isPrefixOf` s = parseBinaryValue s
      | "0o" `BS8.isPrefixOf` s || "0O" `BS8.isPrefixOf` s = parseOctalValue s
      | otherwise = parseDecimalAsInteger (filter (/= '_') (BS8.unpack s))
    parseDecimalAsInteger str
      | any (`elem` (".eE" :: String)) str =
          truncate (fromMaybe 0 (readMaybe (prependZero str) :: Maybe Double))
      | otherwise = fromMaybe 0 (readMaybe str)
    prependZero ('.':rest) = '0' : '.' : rest
    prependZero other = other

-- =====================================================================
-- Regex Literals
-- =====================================================================

-- | Parse regex literal: @/pattern/flags@ (zero-copy).
regexLiteral :: JSParser JSExpression
regexLiteral = do
  pos <- FP.getPos
  raw <- FP.byteStringOf regexSkip
  pure (JSRegEx (fpPosToAnnot pos) raw)
  where
    regexSkip = do
      parseChar '/'
      regexBodySkip
      FP.skipMany (FP.satisfy isRegexFlag)
    isRegexFlag c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

-- | Skip regex body up to the closing @/@.
regexBodySkip :: JSParser ()
regexBodySkip = goNormal
  where
    goNormal = do
      c <- FP.anyChar
      handleNormal c
    handleNormal '/' = pure ()
    handleNormal '\\' = FP.anyChar *> goNormal
    handleNormal '[' = goClass
    handleNormal '\n' = FP.empty
    handleNormal '\r' = FP.empty
    handleNormal _ = goNormal
    goClass = do
      c <- FP.anyChar
      handleClass c
    handleClass ']' = goNormal
    handleClass '\\' = FP.anyChar >>= handleClassEscape
    handleClass '\n' = FP.empty
    handleClass '\r' = FP.empty
    handleClass _ = goClass
    handleClassEscape ']' = goNormal
    handleClassEscape _ = goClass
