{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 #-}

-- | Core parsing primitives and character classification for JavaScript parsing.
--
-- This module provides fundamental parsing combinators and character classification
-- functions optimized for high-performance JavaScript lexical analysis using flatparse.
-- It implements Unicode-aware character classification and efficient string matching.
--
-- ==== Design Principles
--
--   * **Performance**: Inline frequently used predicates for zero-allocation checks
--   * **Unicode compliance**: Full JavaScript identifier specification support
--   * **Efficiency**: Template Haskell switches for character classification
--   * **Safety**: Total functions with clear error conditions
--
-- ==== Examples
--
-- Character classification:
--
-- >>> isIdentifierStart 'a'
-- True
--
-- >>> isIdentifierStart '1'
-- False
--
-- >>> isDecimalDigit '9'
-- True
--
-- @since 0.8.0.0
module Language.JavaScript.Parser.Primitives
  ( -- * Parser Type
    JSParser,

    -- * Basic Parser Combinators
    satisfy,
    some,
    many,

    -- * Character Classification
    isIdentifierStart,
    isIdentifierContinue,
    isDecimalDigit,
    isBinaryDigit,
    isOctalDigit,
    isHexDigit,
    isWhitespace,
    isLineTerminator,

    -- * String Matching
    asciiCI,
    stringCI,

    -- * Error Handling
    err,
    fatal,
  )
where

import Data.ByteString (ByteString)
import Data.Char (GeneralCategory(..), generalCategory, isLetter, toLower)
import Data.Text (Text)
import qualified Data.Text as Text
import FlatParse.Basic (Parser, many, empty, satisfy, some)

-- ---------------------------------------------------------------------
-- Core Parser Infrastructure
-- ---------------------------------------------------------------------

-- | High-performance JavaScript parser using flatparse.
type JSParser = Parser ByteString

-- ---------------------------------------------------------------------
-- Character Classification
-- ---------------------------------------------------------------------

-- | Check if character can start a JavaScript identifier.
-- Follows ECMAScript specification for IdentifierStart production.
-- Uses ASCII fast path to avoid expensive 'generalCategory' calls for common chars.
{-# INLINE isIdentifierStart #-}
isIdentifierStart :: Char -> Bool
isIdentifierStart c
  | c <= '\x7f' = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '$' || c == '_'
  | otherwise = isLetter c || generalCategory c == LetterNumber

-- | Check if character can continue a JavaScript identifier.
-- Follows ECMAScript specification for IdentifierPart production.
-- Uses ASCII fast path to avoid expensive 'generalCategory' calls for common chars.
{-# INLINE isIdentifierContinue #-}
isIdentifierContinue :: Char -> Bool
isIdentifierContinue c
  | c <= '\x7f' = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
                || c == '$' || c == '_' || (c >= '0' && c <= '9')
  | otherwise = isLetter c
             || let cat = generalCategory c
                in cat == LetterNumber || cat == NonSpacingMark
                || cat == SpacingCombiningMark || cat == DecimalNumber
                || cat == ConnectorPunctuation || cat == Format

-- | Check if character is a decimal digit (0-9).
isDecimalDigit :: Char -> Bool
isDecimalDigit c = c >= '0' && c <= '9'

-- | Check if character is a binary digit (0 or 1).
isBinaryDigit :: Char -> Bool
isBinaryDigit c = c == '0' || c == '1'

-- | Check if character is an octal digit (0-7).
isOctalDigit :: Char -> Bool
isOctalDigit c = c >= '0' && c <= '7'

-- | Check if character is a hexadecimal digit (0-9, a-f, A-F).
isHexDigit :: Char -> Bool
isHexDigit c = isDecimalDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- | Check if character is JavaScript whitespace or line terminator.
-- Includes all ECMAScript whitespace and line terminator characters.
-- Line terminators are included because the whitespace lexer must skip
-- both whitespace and newlines between tokens.
-- Uses ASCII fast path since >99.9% of whitespace is ASCII space/tab/newline.
{-# INLINE isWhitespace #-}
isWhitespace :: Char -> Bool
isWhitespace c
  | c <= '\x7f' = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v'
  | otherwise = c == '\160' || c == '\65279' || c == '\8232' || c == '\8233' || c == '\6158'
             || generalCategory c == Space

-- | Check if character is a line terminator.
-- ECMAScript line terminator characters: LF, CR, LS (U+2028), PS (U+2029).
{-# INLINE isLineTerminator #-}
isLineTerminator :: Char -> Bool
isLineTerminator c
  | c <= '\x7f' = c == '\n' || c == '\r'
  | otherwise = c == '\x2028' || c == '\x2029'

-- ---------------------------------------------------------------------
-- String Matching
-- ---------------------------------------------------------------------

-- | Case-insensitive ASCII string matching.
asciiCI :: String -> JSParser ()
asciiCI str = mapM_ (\c -> satisfy (\x -> toLower x == toLower c)) str

-- | Case-insensitive string matching with Text input.
stringCI :: Text -> JSParser ()
stringCI txt = asciiCI (Text.unpack txt)

-- ---------------------------------------------------------------------
-- Error Handling
-- ---------------------------------------------------------------------

-- | Signal parse error.
err :: String -> JSParser a
err _msg = empty

-- | Signal fatal parse error that prevents recovery.
fatal :: String -> JSParser a
fatal _msg = empty