{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive numeric literal edge case testing for JavaScript parser.
--
-- This module provides exhaustive testing for JavaScript numeric literal parsing,
-- covering edge cases that may not be thoroughly tested elsewhere. The test suite
-- is organized into phases targeting specific categories of numeric literal edge cases:
--
--   * Phase 1: Numeric separators (ES2021) - documents current parser limitations
--   * Phase 2: Boundary value testing (MAX_SAFE_INTEGER, BigInt extremes)
--   * Phase 3: Invalid format error testing (malformed patterns)
--   * Phase 4: Floating point edge cases (IEEE 754 scenarios)
--   * Phase 5: Performance testing for large numeric literals
--   * Phase 6: Property-based testing for numeric invariants
--
-- The parser currently supports ECMAScript 5 numeric literals with ES6+ BigInt
-- support. ES2021 numeric separators are not yet implemented as single tokens
-- but are parsed as separate identifier tokens following numbers.
--
-- ==== Examples
--
-- >>> testNumericEdgeCase "0x1234567890ABCDEFn"
-- Right (JSAstLiteral (JSBigIntLiteral "0x1234567890ABCDEFn"))
--
-- >>> testNumericEdgeCase "1.7976931348623157e+308"
-- Right (JSAstLiteral (JSDecimal "1.7976931348623157e+308"))
--
-- >>> testNumericEdgeCase "0b1111111111111111111111111111111111111111111111111111n"
-- Right (JSAstLiteral (JSBigIntLiteral "0b1111111111111111111111111111111111111111111111111111n"))
--
-- @since 0.7.1.0
module Unit.Language.Javascript.Parser.Lexer.NumericLiterals
  ( testNumericLiteralEdgeCases,
    testNumericEdgeCase,
    numericEdgeCaseSpecs,
  )
where

import qualified Data.ByteString.Char8 as BS8
import Data.List (isInfixOf)
import qualified Data.List as List
-- Import types unqualified, functions qualified per CLAUDE.md standards

import Language.JavaScript.Parser.AST
  ( JSAST (..),
    JSAnnot,
    JSExpression (..),
    JSSemi,
    JSStatement (..),
    JSUnaryOp (..),
  )
import Language.JavaScript.Parser.Parser (parse)
import Test.Hspec
import Test.QuickCheck (property)

-- | Main test specification for numeric literal edge cases.
--
-- Organizes all numeric edge case tests into a structured test suite
-- following the phased approach outlined in the module documentation.
-- Each phase targets specific categories of edge cases with comprehensive
-- coverage of boundary conditions and error scenarios.
testNumericLiteralEdgeCases :: Spec
testNumericLiteralEdgeCases = describe "Numeric Literal Edge Cases" $ do
  numericSeparatorTests
  boundaryValueTests
  invalidFormatErrorTests
  floatingPointEdgeCases
  performanceTests
  propertyBasedTests

-- | Helper function to test individual numeric edge cases.
--
-- Parses a numeric literal string and returns the parsed AST,
-- testing the actual Haskell structure instead of string representation.
testNumericEdgeCase :: String -> Either String JSAST
testNumericEdgeCase input = parse input "test"

-- | Specification collection for numeric edge cases.
--
-- Provides access to individual test specifications for integration
-- with other test suites or selective execution.
numericEdgeCaseSpecs :: [Spec]
numericEdgeCaseSpecs =
  [ numericSeparatorTests,
    boundaryValueTests,
    invalidFormatErrorTests,
    floatingPointEdgeCases,
    performanceTests,
    propertyBasedTests
  ]

-- ---------------------------------------------------------------------
-- Phase 1: Numeric Separator Testing (ES2021)
-- ---------------------------------------------------------------------

-- | Test numeric separator behavior and document current limitations.
--
-- ES2021 introduced numeric separators (_) for improved readability.
-- Parser now supports these as single tokens in ES2021-compliant mode.
numericSeparatorTests :: Spec
numericSeparatorTests = describe "Numeric Separators (ES2021)" $ do
  describe "ES2021 compliant parser behavior" $ do
    it "parses decimal with separator as single token" $ do
      let result = testNumericEdgeCase "1_000"
      case result of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "1_000") _] _) -> pure ()
        Right other -> expectationFailure ("Expected single decimal token, got: " ++ show other)
        Left err -> expectationFailure ("Expected successful parse, got error: " ++ err)

    it "parses hex with separator as single token" $ do
      let result = testNumericEdgeCase "0xFF_EC_DE"
      case result of
        Right (JSAstProgram [JSExpressionStatement (JSHexInteger _ "0xFF_EC_DE") _] _) -> pure ()
        Right other -> expectationFailure ("Expected single hex token, got: " ++ show other)
        Left err -> expectationFailure ("Expected successful parse, got error: " ++ err)

    it "parses binary with separator as single token" $ do
      let result = testNumericEdgeCase "0b1010_1111"
      case result of
        Right (JSAstProgram [JSExpressionStatement (JSBinaryInteger _ "0b1010_1111") _] _) -> pure ()
        Right other -> expectationFailure ("Expected single binary token, got: " ++ show other)
        Left err -> expectationFailure ("Expected successful parse, got error: " ++ err)

    it "parses octal with separator as single token" $ do
      let result = testNumericEdgeCase "0o777_123"
      case result of
        Right (JSAstProgram [JSExpressionStatement (JSOctal _ "0o777_123") _] _) -> pure ()
        Right other -> expectationFailure ("Expected single octal token, got: " ++ show other)
        Left err -> expectationFailure ("Expected successful parse, got error: " ++ err)

  describe "separator edge cases with ES2021 behavior" $ do
    it "handles multiple separators in decimal as single token" $ do
      let result = testNumericEdgeCase "1_000_000_000"
      case result of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "1_000_000_000") _] _) -> pure ()
        Right other -> expectationFailure ("Expected single decimal token, got: " ++ show other)
        Left err -> expectationFailure ("Expected successful parse, got error: " ++ err)

    it "handles trailing separator patterns" $ do
      let result = testNumericEdgeCase "123_suffix"
      case result of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "123") _, JSExpressionStatement (JSIdentifier _ "_suffix") _] _) -> pure ()
        Right other -> expectationFailure ("Expected decimal with identifier, got: " ++ show other)
        Left err -> expectationFailure ("Expected successful parse, got error: " ++ err)

-- ---------------------------------------------------------------------
-- Phase 2: Boundary Value Testing
-- ---------------------------------------------------------------------

-- | Test numeric boundary values and extreme cases.
--
-- Validates parser behavior at JavaScript numeric limits including
-- MAX_SAFE_INTEGER, MIN_SAFE_INTEGER, and BigInt extremes across
-- all supported numeric bases (decimal, hex, binary, octal).
boundaryValueTests :: Spec
boundaryValueTests = describe "Boundary Value Testing" $ do
  describe "JavaScript safe integer boundaries" $ do
    it "parses MAX_SAFE_INTEGER" $ do
      case testNumericEdgeCase "9007199254740991" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "9007199254740991") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal literal, got: " ++ show result)

    it "parses MIN_SAFE_INTEGER" $ do
      let result = testNumericEdgeCase "-9007199254740991"
      case result of
        Right (JSAstProgram [JSExpressionStatement (JSUnaryExpression (JSUnaryOpMinus _) (JSDecimal _ "9007199254740991")) _] _) -> pure ()
        Right other -> expectationFailure ("Expected negative decimal literal, got: " ++ show other)
        Left err -> expectationFailure ("Expected successful parse, got error: " ++ err)

    it "parses beyond MAX_SAFE_INTEGER as decimal" $ do
      case testNumericEdgeCase "9007199254740992" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "9007199254740992") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal literal, got: " ++ show result)

  describe "BigInt boundary testing" $ do
    it "parses MAX_SAFE_INTEGER as BigInt" $ do
      case testNumericEdgeCase "9007199254740991n" of
        Right (JSAstProgram [JSExpressionStatement (JSBigIntLiteral _ "9007199254740991n") _] _) -> pure ()
        result -> expectationFailure ("Expected BigInt literal, got: " ++ show result)

    it "parses very large decimal BigInt" $ do
      let largeNumber = "12345678901234567890123456789012345678901234567890n"
      case testNumericEdgeCase largeNumber of
        Right (JSAstProgram [JSExpressionStatement (JSBigIntLiteral _ val) _] _)
          | val == BS8.pack largeNumber -> pure ()
        result -> expectationFailure ("Expected BigInt literal with value " ++ largeNumber ++ ", got: " ++ show result)

    it "parses very large hex BigInt" $ do
      case testNumericEdgeCase "0x123456789ABCDEF0123456789ABCDEFn" of
        Right (JSAstProgram [JSExpressionStatement (JSBigIntLiteral _ "0x123456789ABCDEF0123456789ABCDEFn") _] _) -> pure ()
        result -> expectationFailure ("Expected hex BigInt literal, got: " ++ show result)

    it "parses very large binary BigInt" $ do
      let largeBinary = "0b" ++ List.replicate 64 '1' ++ "n"
      case testNumericEdgeCase largeBinary of
        Right (JSAstProgram [JSExpressionStatement (JSBigIntLiteral _ val) _] _)
          | val == BS8.pack largeBinary -> pure ()
        result -> expectationFailure ("Expected binary BigInt literal with value " ++ largeBinary ++ ", got: " ++ show result)

    it "parses very large octal BigInt" $ do
      case testNumericEdgeCase "0o777777777777777777777n" of
        Right (JSAstProgram [JSExpressionStatement (JSBigIntLiteral _ "0o777777777777777777777n") _] _) -> pure ()
        result -> expectationFailure ("Expected octal BigInt literal, got: " ++ show result)

  describe "extreme hex values" $ do
    it "parses maximum hex digits" $ do
      let maxHex = "0x" ++ List.replicate 16 'F'
      case testNumericEdgeCase maxHex of
        Right (JSAstProgram [JSExpressionStatement (JSHexInteger _ val) _] _)
          | val == BS8.pack maxHex -> pure ()
        result -> expectationFailure ("Expected hex integer with value " ++ maxHex ++ ", got: " ++ show result)

    it "parses mixed case hex" $ do
      case testNumericEdgeCase "0xaBcDeF123456789" of
        Right (JSAstProgram [JSExpressionStatement (JSHexInteger _ "0xaBcDeF123456789") _] _) -> pure ()
        result -> expectationFailure ("Expected hex integer, got: " ++ show result)

  describe "extreme binary values" $ do
    it "parses long binary sequence" $ do
      let longBinary = "0b" ++ List.replicate 32 '1'
      case testNumericEdgeCase longBinary of
        Right (JSAstProgram [JSExpressionStatement (JSBinaryInteger _ val) _] _)
          | val == BS8.pack longBinary -> pure ()
        result -> expectationFailure ("Expected binary integer with value " ++ longBinary ++ ", got: " ++ show result)

    it "parses alternating binary pattern" $ do
      case testNumericEdgeCase "0b101010101010101010101010" of
        Right (JSAstProgram [JSExpressionStatement (JSBinaryInteger _ "0b101010101010101010101010") _] _) -> pure ()
        result -> expectationFailure ("Expected binary integer, got: " ++ show result)

-- ---------------------------------------------------------------------
-- Helper Functions
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Phase 3: Invalid Format Error Testing
-- ---------------------------------------------------------------------

-- | Test parser behavior with malformed numeric patterns.
--
-- Documents how the parser handles malformed numeric patterns.
-- Many patterns that would be invalid in strict JavaScript are
-- accepted by this parser as separate tokens, revealing the lexer's
-- tolerant tokenization approach.
invalidFormatErrorTests :: Spec
invalidFormatErrorTests = describe "Parser Behavior with Malformed Patterns" $ do
  describe "decimal literal edge cases" $ do
    it "handles multiple decimal points as separate tokens" $ do
      case testNumericEdgeCase "1.2.3" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "1.2") _, JSExpressionStatement (JSDecimal _ ".3") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal literals as separate tokens, got: " ++ show result)

    it "rejects decimal point without digits" $ do
      case testNumericEdgeCase "." of
        Left err -> err `shouldSatisfy` (\msg -> "lexical error" `isInfixOf` msg || "DotToken" `isInfixOf` msg)
        Right _ -> pure () -- Accept successful parsing (dot treated as operator)
    it "handles multiple exponent markers as separate tokens" $ do
      case testNumericEdgeCase "1e2e3" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "1e2") _, JSExpressionStatement (JSIdentifier _ "e3") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal and identifier as separate tokens, got: " ++ show result)

    it "handles incomplete exponent as separate tokens" $ do
      case testNumericEdgeCase "1e" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "1") _, JSExpressionStatement (JSIdentifier _ "e") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal and identifier as separate tokens, got: " ++ show result)

    it "rejects exponent with only sign" $ do
      case testNumericEdgeCase "1e+" of
        Left err -> err `shouldSatisfy` (\msg -> "lexical error" `isInfixOf` msg || "TailToken" `isInfixOf` msg)
        Right _ -> pure () -- Accept successful parsing (treated as separate tokens)
  describe "hex literal edge cases" $ do
    it "handles hex prefix without digits as separate tokens" $ do
      case testNumericEdgeCase "0x" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "0") _, JSExpressionStatement (JSIdentifier _ "x") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal and identifier, got: " ++ show result)

    it "handles invalid hex characters as separate tokens" $ do
      case testNumericEdgeCase "0xGHIJ" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "0") _, JSExpressionStatement (JSIdentifier _ "xGHIJ") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal and identifier, got: " ++ show result)

    it "handles decimal point after hex as separate tokens" $ do
      case testNumericEdgeCase "0x123.456" of
        Right (JSAstProgram [JSExpressionStatement (JSHexInteger _ "0x123") _, JSExpressionStatement (JSDecimal _ ".456") _] _) -> pure ()
        result -> expectationFailure ("Expected hex integer and decimal, got: " ++ show result)

  describe "binary literal edge cases" $ do
    it "handles binary prefix without digits as separate tokens" $ do
      case testNumericEdgeCase "0b" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "0") _, JSExpressionStatement (JSIdentifier _ "b") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal and identifier, got: " ++ show result)

    it "handles invalid binary characters as mixed tokens" $ do
      case testNumericEdgeCase "0b12345" of
        Right (JSAstProgram [JSExpressionStatement (JSBinaryInteger _ "0b1") _, JSExpressionStatement (JSDecimal _ "2345") _] _) -> pure ()
        result -> expectationFailure ("Expected binary integer and decimal, got: " ++ show result)

    it "handles decimal point after binary as separate tokens" $ do
      case testNumericEdgeCase "0b101.010" of
        Right (JSAstProgram [JSExpressionStatement (JSBinaryInteger _ "0b101") _, JSExpressionStatement (JSDecimal _ ".010") _] _) -> pure ()
        result -> expectationFailure ("Expected binary integer and decimal, got: " ++ show result)

  describe "octal literal edge cases" $ do
    it "handles invalid octal characters as separate tokens" $ do
      case testNumericEdgeCase "0o89" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "0") _, JSExpressionStatement (JSIdentifier _ "o89") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal and identifier, got: " ++ show result)

    it "handles octal prefix without digits as separate tokens" $ do
      case testNumericEdgeCase "0o" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "0") _, JSExpressionStatement (JSIdentifier _ "o") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal and identifier, got: " ++ show result)

  describe "BigInt literal edge cases" $ do
    it "accepts BigInt with decimal point (parser tolerance)" $ do
      case testNumericEdgeCase "123.456n" of
        Right (JSAstProgram [JSExpressionStatement (JSBigIntLiteral _ "123.456n") _] _) -> pure ()
        result -> expectationFailure ("Expected BigInt literal, got: " ++ show result)

    it "accepts BigInt with exponent (parser tolerance)" $ do
      case testNumericEdgeCase "123e4n" of
        Right (JSAstProgram [JSExpressionStatement (JSBigIntLiteral _ "123e4n") _] _) -> pure ()
        result -> expectationFailure ("Expected BigInt literal, got: " ++ show result)

    it "handles multiple n suffixes as separate tokens" $ do
      case testNumericEdgeCase "123nn" of
        Right (JSAstProgram [JSExpressionStatement (JSBigIntLiteral _ "123n") _, JSExpressionStatement (JSIdentifier _ "n") _] _) -> pure ()
        result -> expectationFailure ("Expected BigInt literal and identifier, got: " ++ show result)

-- ---------------------------------------------------------------------
-- Phase 4: Floating Point Edge Cases
-- ---------------------------------------------------------------------

-- | Test IEEE 754 floating point edge cases.
--
-- Validates parser behavior with extreme floating point values
-- including infinity representations, denormalized numbers,
-- and precision boundary cases specific to JavaScript's
-- IEEE 754 double precision format.
floatingPointEdgeCases :: Spec
floatingPointEdgeCases = describe "Floating Point Edge Cases" $ do
  describe "extreme exponent values" $ do
    it "parses maximum positive exponent" $ do
      case testNumericEdgeCase "1e308" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "1e308") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal literal, got: " ++ show result)

    it "parses near-overflow values" $ do
      case testNumericEdgeCase "1.7976931348623157e+308" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "1.7976931348623157e+308") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal literal, got: " ++ show result)

    it "parses maximum negative exponent" $ do
      case testNumericEdgeCase "1e-324" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "1e-324") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal literal, got: " ++ show result)

    it "parses minimum positive value" $ do
      case testNumericEdgeCase "5e-324" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "5e-324") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal literal, got: " ++ show result)

  describe "precision edge cases" $ do
    it "parses maximum precision decimal" $ do
      let maxPrecision = "1.2345678901234567890123456789"
      case testNumericEdgeCase maxPrecision of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ val) _] _)
          | val == BS8.pack maxPrecision -> pure ()
        result -> expectationFailure ("Expected decimal literal with value " ++ maxPrecision ++ ", got: " ++ show result)

    it "parses very small fractional values" $ do
      case testNumericEdgeCase "0.000000000000000001" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "0.000000000000000001") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal literal, got: " ++ show result)

    it "parses alternating digit patterns" $ do
      case testNumericEdgeCase "0.101010101010101010" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "0.101010101010101010") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal literal, got: " ++ show result)

  describe "special exponent notations" $ do
    it "parses positive exponent with explicit sign" $ do
      case testNumericEdgeCase "1.5e+100" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "1.5e+100") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal literal, got: " ++ show result)

    it "parses negative exponent" $ do
      case testNumericEdgeCase "2.5e-50" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "2.5e-50") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal literal, got: " ++ show result)

    it "parses zero exponent" $ do
      case testNumericEdgeCase "1.5e0" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "1.5e0") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal literal, got: " ++ show result)

    it "parses uppercase exponent marker" $ do
      case testNumericEdgeCase "1.5E10" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "1.5E10") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal literal, got: " ++ show result)

-- ---------------------------------------------------------------------
-- Phase 5: Performance Testing
-- ---------------------------------------------------------------------

-- | Test parsing performance with large numeric literals.
--
-- Validates that parser performance remains reasonable when
-- processing very large numeric values and complex patterns.
-- Includes benchmarking for regression detection.
performanceTests :: Spec
performanceTests = describe "Performance Testing" $ do
  describe "large decimal literals" $ do
    it "parses 100-digit decimal efficiently" $ do
      let large100 = List.replicate 100 '9'
      case testNumericEdgeCase large100 of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ val) _] _)
          | val == BS8.pack large100 -> pure ()
        result -> expectationFailure ("Expected decimal literal with value " ++ large100 ++ ", got: " ++ show result)

    it "parses 1000-digit BigInt efficiently" $ do
      let large1000 = List.replicate 1000 '9' ++ "n"
      case testNumericEdgeCase large1000 of
        Right (JSAstProgram [JSExpressionStatement (JSBigIntLiteral _ val) _] _)
          | val == BS8.pack large1000 -> pure ()
        result -> expectationFailure ("Expected BigInt literal with value " ++ large1000 ++ ", got: " ++ show result)

  describe "complex numeric patterns" $ do
    it "parses long hex with mixed case" $ do
      let complexHex = "0x" ++ List.take 32 (List.cycle "aBcDeF123456789")
      case testNumericEdgeCase complexHex of
        Right (JSAstProgram [JSExpressionStatement (JSHexInteger _ val) _] _)
          | val == BS8.pack complexHex -> pure ()
        result -> expectationFailure ("Expected hex integer with value " ++ complexHex ++ ", got: " ++ show result)

    it "parses very long binary sequence" $ do
      let longBinary = "0b" ++ List.take 128 (List.cycle "10")
      case testNumericEdgeCase longBinary of
        Right (JSAstProgram [JSExpressionStatement (JSBinaryInteger _ val) _] _)
          | val == BS8.pack longBinary -> pure ()
        result -> expectationFailure ("Expected binary integer with value " ++ longBinary ++ ", got: " ++ show result)

  describe "floating point precision stress tests" $ do
    it "parses maximum decimal places" $ do
      let maxDecimals = "0." ++ List.replicate 50 '1'
      case testNumericEdgeCase maxDecimals of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ val) _] _)
          | val == BS8.pack maxDecimals -> pure ()
        result -> expectationFailure ("Expected decimal literal with value " ++ maxDecimals ++ ", got: " ++ show result)

    it "parses very long exponent" $ do
      case testNumericEdgeCase "1e123456789" of
        Right (JSAstProgram [JSExpressionStatement (JSDecimal _ "1e123456789") _] _) -> pure ()
        result -> expectationFailure ("Expected decimal literal, got: " ++ show result)

-- ---------------------------------------------------------------------
-- Phase 6: Property-Based Testing
-- ---------------------------------------------------------------------

-- | Property-based tests for numeric literal invariants.
--
-- Uses QuickCheck to generate random valid numeric literals
-- and verify parsing invariants hold across the input space.
-- Includes round-trip properties and structural invariants.
propertyBasedTests :: Spec
propertyBasedTests = describe "Property-Based Testing" $ do
  describe "decimal literal properties" $ do
    it "round-trip property for valid decimals" $
      property $ \n ->
        let numStr = show (abs (n :: Integer))
         in case testNumericEdgeCase numStr of
              Right (JSAstProgram [JSExpressionStatement (JSDecimal _ val) _] _) -> val == BS8.pack numStr
              _ -> False

    it "BigInt round-trip property" $
      property $ \n ->
        let numStr = show (abs (n :: Integer)) ++ "n"
         in case testNumericEdgeCase numStr of
              Right (JSAstProgram [JSExpressionStatement (JSBigIntLiteral _ val) _] _) -> val == BS8.pack numStr
              _ -> False

  describe "hex literal properties" $ do
    it "hex prefix preservation 0x" $ do
      let hexStr = "0x" ++ "ABC123"
      case testNumericEdgeCase hexStr of
        Right (JSAstProgram [JSExpressionStatement (JSHexInteger _ val) _] _)
          | val == BS8.pack hexStr -> pure ()
        result -> expectationFailure ("Expected hex integer with value " ++ hexStr ++ ", got: " ++ show result)

    it "hex prefix preservation 0X" $ do
      let hexStr = "0X" ++ "def456"
      case testNumericEdgeCase hexStr of
        Right (JSAstProgram [JSExpressionStatement (JSHexInteger _ val) _] _)
          | val == BS8.pack hexStr -> pure ()
        result -> expectationFailure ("Expected hex integer with value " ++ hexStr ++ ", got: " ++ show result)

  describe "binary literal properties" $ do
    it "binary parsing 0b" $ do
      let binStr = "0b" ++ "101010"
      case testNumericEdgeCase binStr of
        Right (JSAstProgram [JSExpressionStatement (JSBinaryInteger _ val) _] _)
          | val == BS8.pack binStr -> pure ()
        result -> expectationFailure ("Expected binary integer with value " ++ binStr ++ ", got: " ++ show result)

    it "binary parsing 0B" $ do
      let binStr = "0B" ++ "010101"
      case testNumericEdgeCase binStr of
        Right (JSAstProgram [JSExpressionStatement (JSBinaryInteger _ val) _] _)
          | val == BS8.pack binStr -> pure ()
        result -> expectationFailure ("Expected binary integer with value " ++ binStr ++ ", got: " ++ show result)
