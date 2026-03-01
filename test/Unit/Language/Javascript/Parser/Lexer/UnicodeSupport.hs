{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- |
-- Module      : Unit.Language.Javascript.Parser.Lexer.UnicodeSupport
-- Description : Comprehensive Unicode testing for JavaScript lexer
-- Copyright   : (c) Language-JavaScript Project
-- License     : BSD-style
-- Maintainer  : language-javascript@example.com
-- Stability   : experimental
-- Portability : GHC
--
-- Comprehensive Unicode testing for the JavaScript lexer via full parsing.
--
-- This test suite validates the current Unicode capabilities of the parser and
-- documents expected behavior for various Unicode scenarios. Tests use the
-- 'parse' function from the flatparse-based parser to verify end-to-end
-- behavior including lexing, parsing, and AST construction.
--
-- === Current Unicode Support Status:
--
-- * BOM (U+FEFF) handling as whitespace
-- * Unicode line separators (U+2028, U+2029) as statement terminators
-- * Unicode content in comments (preserved in annotations)
-- * Non-breaking space (U+00A0) as whitespace between tokens
-- * Unicode escape sequences in string literals (preserved verbatim)
-- * Non-ASCII Unicode identifiers (supported via UTF-8)
-- * Direct Unicode characters in string literals (UTF-8 encoded)
--
-- @since 0.7.1.0
module Unit.Language.Javascript.Parser.Lexer.UnicodeSupport
  ( testUnicodeSupport,
  )
where

import Data.Either (isRight)
import Language.JavaScript.Parser.AST
  ( JSAST (..),
    JSExpression (..),
    JSStatement (..),
  )
import Language.JavaScript.Parser.Parser (parse)
import Test.Hspec

-- | Parse a string using the flatparse-based parser with a fixed source name.
testParse :: String -> Either String JSAST
testParse input = parse input "test"

-- | Main test suite for Unicode support in the JavaScript parser.
testUnicodeSupport :: Spec
testUnicodeSupport = describe "Unicode Support" $ do
  bomHandlingTests
  unicodeLineSeparatorTests
  unicodeCommentTests
  unicodeWhitespaceTests
  unicodeEscapeSequenceTests
  unicodeIdentifierTests
  unicodeStringLiteralTests

-- | BOM (U+FEFF) at start of input is treated as whitespace.
bomHandlingTests :: Spec
bomHandlingTests = describe "BOM handling" $ do
  it "BOM followed by decimal literal parses successfully" $
    testParse ("\xFEFF" ++ "42") `shouldSatisfy` isDecimalProgram 42.0

  it "BOM followed by var statement parses successfully" $
    testParse ("\xFEFF" ++ "var x = 1") `shouldSatisfy` isRight

-- | U+2028 (line separator) and U+2029 (paragraph separator) act as
-- line terminators, allowing separate statements on either side.
unicodeLineSeparatorTests :: Spec
unicodeLineSeparatorTests = describe "Unicode line separators" $ do
  it "U+2028 separates two var statements" $
    testParse ("var x = 1" ++ ['\x2028'] ++ "var y = 2")
      `shouldSatisfy` isTwoStatementProgram

  it "U+2029 separates two var statements" $
    testParse ("var x = 1" ++ ['\x2029'] ++ "var y = 2")
      `shouldSatisfy` isTwoStatementProgram

-- | Unicode characters inside comments are preserved in comment annotations
-- and do not interfere with parsing the surrounding code.
unicodeCommentTests :: Spec
unicodeCommentTests = describe "Unicode in comments" $ do
  it "single-line comment with Japanese characters" $
    testParse ("// Unicode comment: \26085\26412\35486\n42")
      `shouldSatisfy` isDecimalProgram 42.0

  it "multi-line comment with accented characters" $
    testParse ("/* Unicode: caf\233 */42")
      `shouldSatisfy` isDecimalProgram 42.0

-- | Non-breaking space (U+00A0) is treated as whitespace between tokens,
-- allowing it to separate an expression and an operator.
unicodeWhitespaceTests :: Spec
unicodeWhitespaceTests = describe "Unicode whitespace" $ do
  it "non-breaking space U+00A0 between tokens" $
    testParse ("42" ++ ['\x00A0'] ++ "+ 1") `shouldSatisfy` isRight

-- | Unicode escape sequences in string literals are preserved verbatim
-- in the AST (not decoded to their character equivalents).
unicodeEscapeSequenceTests :: Spec
unicodeEscapeSequenceTests = describe "Unicode escape sequences" $ do
  it "\\u0041 escape sequence parses as string literal" $
    testParse "'\\u0041'" `shouldSatisfy` isStringLiteralProgram

  it "\\u00E9 escape sequence parses as string literal" $
    testParse "'\\u00E9'" `shouldSatisfy` isStringLiteralProgram

-- | Non-ASCII Unicode characters are accepted as identifiers when they
-- are valid UTF-8 letter characters. The identifier bytes are stored as
-- UTF-8 encoded ByteString in the AST.
unicodeIdentifierTests :: Spec
unicodeIdentifierTests = describe "Unicode identifiers" $ do
  it "Greek letter lambda as identifier" $
    testParse "\955 = 1" `shouldSatisfy` isRight

  it "underscore-prefixed Unicode identifier" $
    testParse "_\955 = 1" `shouldSatisfy` isRight

-- | Direct Unicode characters in string literals are stored as UTF-8
-- encoded bytes in the ByteString, including the enclosing quotes.
unicodeStringLiteralTests :: Spec
unicodeStringLiteralTests = describe "Unicode string literals" $ do
  it "accented characters in single-quoted string" $
    testParse "'caf\233'" `shouldSatisfy` isStringLiteralProgram

  it "Japanese characters in single-quoted string" $
    testParse "'\26085\26412\35486'" `shouldSatisfy` isStringLiteralProgram

-- ---------------------------------------------------------------------
-- Predicate Helpers
-- ---------------------------------------------------------------------

-- | Check whether a parse result is a single-statement program with
-- a decimal expression matching the given value.
isDecimalProgram :: Double -> Either String JSAST -> Bool
isDecimalProgram expected result =
  case result of
    Right (JSAstProgram [JSExpressionStatement (JSDecimal _ val) _] _) -> val == expected
    _ -> False

-- | Check whether a parse result is a program with exactly two statements.
isTwoStatementProgram :: Either String JSAST -> Bool
isTwoStatementProgram result =
  case result of
    Right (JSAstProgram [_, _] _) -> True
    _ -> False

-- | Check whether a parse result is a single-statement program with
-- a string literal expression.
isStringLiteralProgram :: Either String JSAST -> Bool
isStringLiteralProgram result =
  case result of
    Right (JSAstProgram [JSExpressionStatement (JSStringLiteral _ _) _] _) -> True
    _ -> False
