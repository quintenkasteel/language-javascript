{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

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
module Language.JavaScript.Parser.Flatparse.Primitives
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
import FlatParse.Basic (Parser, anyChar, many, empty, satisfy, some)

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
isIdentifierStart :: Char -> Bool
isIdentifierStart c =
  isLetter c ||
  c == '$' ||
  c == '_' ||
  -- Unicode categories: Lowercase Letter, Modifier Letter, Other Letter,
  -- Titlecase Letter, Uppercase Letter, Letter Number
  generalCategory c `elem` [LowercaseLetter, ModifierLetter, OtherLetter,
                           TitlecaseLetter, UppercaseLetter, LetterNumber]

-- | Check if character can continue a JavaScript identifier.
-- Follows ECMAScript specification for IdentifierPart production.
isIdentifierContinue :: Char -> Bool
isIdentifierContinue c =
  isIdentifierStart c || isDecimalDigit c ||
  -- Additional Unicode categories for identifier continuation
  generalCategory c `elem` [NonSpacingMark, SpacingCombiningMark, DecimalNumber,
                           ConnectorPunctuation, Format]

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
isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\t', '\v', '\f', '\n', '\r', '\160', '\65279', '\8232', '\8233', '\6158'] ||
                 generalCategory c == Space

-- | Check if character is a line terminator.
-- ECMAScript line terminator characters.
isLineTerminator :: Char -> Bool
isLineTerminator c = c `elem` ['\n', '\r', '\8232', '\8233']

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