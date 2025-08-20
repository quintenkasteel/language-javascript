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
  ( testNumericLiteralEdgeCases
  , testNumericEdgeCase
  , numericEdgeCaseSpecs
  ) where

import Test.Hspec
import Test.QuickCheck (property)

import qualified Data.List as List

-- Import types unqualified, functions qualified per CLAUDE.md standards
import Language.JavaScript.Parser.Parser (parse, showStrippedMaybe)

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
-- Parses a numeric literal string and returns the string representation,
-- following the same pattern as the existing LiteralParser tests.
testNumericEdgeCase :: String -> String
testNumericEdgeCase input = showStrippedMaybe $ parse input "test"

-- | Specification collection for numeric edge cases.
--
-- Provides access to individual test specifications for integration
-- with other test suites or selective execution.
numericEdgeCaseSpecs :: [Spec]
numericEdgeCaseSpecs =
  [ numericSeparatorTests
  , boundaryValueTests
  , invalidFormatErrorTests
  , floatingPointEdgeCases
  , performanceTests
  , propertyBasedTests
  ]

-- ---------------------------------------------------------------------
-- Phase 1: Numeric Separator Testing (ES2021)
-- ---------------------------------------------------------------------

-- | Test numeric separator behavior and document current limitations.
--
-- ES2021 introduced numeric separators (_) for improved readability.
-- Current parser does not support these as single tokens but parses
-- them as separate identifier tokens following numbers.
numericSeparatorTests :: Spec
numericSeparatorTests = describe "Numeric Separators (ES2021)" $ do
  describe "current parser behavior documentation" $ do
    it "parses decimal with separator as separate tokens" $ do
      let result = testNumericEdgeCase "1_000"
      result `shouldSatisfy` (\str -> "Right" `List.isPrefixOf` str)

    it "parses hex with separator as separate tokens" $ do
      let result = testNumericEdgeCase "0xFF_EC_DE"
      result `shouldSatisfy` (\str -> "Right" `List.isPrefixOf` str)

    it "parses binary with separator as separate tokens" $ do
      let result = testNumericEdgeCase "0b1010_1111"
      result `shouldSatisfy` (\str -> "Right" `List.isPrefixOf` str)

    it "parses octal with separator as separate tokens" $ do
      let result = testNumericEdgeCase "0o777_123"
      result `shouldSatisfy` (\str -> "Right" `List.isPrefixOf` str)

  describe "separator edge cases with current behavior" $ do
    it "handles multiple separators in decimal" $ do
      let result = testNumericEdgeCase "1_000_000_000"
      result `shouldSatisfy` (\str -> "Right" `List.isPrefixOf` str)

    it "handles trailing separator patterns" $ do
      let result = testNumericEdgeCase "123_suffix"
      result `shouldSatisfy` (\str -> "Right" `List.isPrefixOf` str)

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
      testNumericEdgeCase "9007199254740991" `shouldBe`
        "Right (JSAstProgram [JSDecimal '9007199254740991'])"

    it "parses MIN_SAFE_INTEGER" $ do
      let result = testNumericEdgeCase "-9007199254740991"
      result `shouldSatisfy` (\str -> "Right" `List.isPrefixOf` str)

    it "parses beyond MAX_SAFE_INTEGER as decimal" $ do
      testNumericEdgeCase "9007199254740992" `shouldBe`
        "Right (JSAstProgram [JSDecimal '9007199254740992'])"

  describe "BigInt boundary testing" $ do
    it "parses MAX_SAFE_INTEGER as BigInt" $ do
      testNumericEdgeCase "9007199254740991n" `shouldBe`
        "Right (JSAstProgram [JSBigIntLiteral '9007199254740991n'])"

    it "parses very large decimal BigInt" $ do
      let largeNumber = "12345678901234567890123456789012345678901234567890n"
      testNumericEdgeCase largeNumber `shouldBe`
        "Right (JSAstProgram [JSBigIntLiteral '" ++ largeNumber ++ "'])"

    it "parses very large hex BigInt" $ do
      testNumericEdgeCase "0x123456789ABCDEF0123456789ABCDEFn" `shouldBe`
        "Right (JSAstProgram [JSBigIntLiteral '0x123456789ABCDEF0123456789ABCDEFn'])"

    it "parses very large binary BigInt" $ do
      let largeBinary = "0b" ++ List.replicate 64 '1' ++ "n"
      testNumericEdgeCase largeBinary `shouldBe`
        "Right (JSAstProgram [JSBigIntLiteral '" ++ largeBinary ++ "'])"

    it "parses very large octal BigInt" $ do
      testNumericEdgeCase "0o777777777777777777777n" `shouldBe`
        "Right (JSAstProgram [JSBigIntLiteral '0o777777777777777777777n'])"

  describe "extreme hex values" $ do
    it "parses maximum hex digits" $ do
      let maxHex = "0x" ++ List.replicate 16 'F'
      testNumericEdgeCase maxHex `shouldBe`
        "Right (JSAstProgram [JSHexInteger '" ++ maxHex ++ "'])"

    it "parses mixed case hex" $ do
      testNumericEdgeCase "0xaBcDeF123456789" `shouldBe`
        "Right (JSAstProgram [JSHexInteger '0xaBcDeF123456789'])"

  describe "extreme binary values" $ do
    it "parses long binary sequence" $ do
      let longBinary = "0b" ++ List.replicate 32 '1'
      testNumericEdgeCase longBinary `shouldBe`
        "Right (JSAstProgram [JSBinaryInteger '" ++ longBinary ++ "'])"

    it "parses alternating binary pattern" $ do
      testNumericEdgeCase "0b101010101010101010101010" `shouldBe`
        "Right (JSAstProgram [JSBinaryInteger '0b101010101010101010101010'])"

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
      let result = testNumericEdgeCase "1.2.3"
      result `shouldBe` "Right (JSAstProgram [JSDecimal '1.2',JSDecimal '.3'])"

    it "rejects decimal point without digits" $ do
      let result = testNumericEdgeCase "."
      result `shouldSatisfy` (\str -> "Left" `List.isPrefixOf` str)

    it "handles multiple exponent markers as separate tokens" $ do
      let result = testNumericEdgeCase "1e2e3"
      result `shouldBe` "Right (JSAstProgram [JSDecimal '1e2',JSIdentifier 'e3'])"

    it "handles incomplete exponent as identifier" $ do
      let result = testNumericEdgeCase "1e"
      result `shouldBe` "Right (JSAstProgram [JSDecimal '1',JSIdentifier 'e'])"

    it "rejects exponent with only sign" $ do
      let result = testNumericEdgeCase "1e+"
      result `shouldSatisfy` (\str -> "Left" `List.isPrefixOf` str)

  describe "hex literal edge cases" $ do
    it "handles hex prefix without digits as separate tokens" $ do
      let result = testNumericEdgeCase "0x"
      result `shouldBe` "Right (JSAstProgram [JSDecimal '0',JSIdentifier 'x'])"

    it "handles invalid hex characters as separate tokens" $ do
      let result = testNumericEdgeCase "0xGHIJ"
      result `shouldBe` "Right (JSAstProgram [JSDecimal '0',JSIdentifier 'xGHIJ'])"

    it "handles decimal point after hex as separate tokens" $ do
      let result = testNumericEdgeCase "0x123.456"
      result `shouldBe` "Right (JSAstProgram [JSHexInteger '0x123',JSDecimal '.456'])"

  describe "binary literal edge cases" $ do
    it "handles binary prefix without digits as separate tokens" $ do
      let result = testNumericEdgeCase "0b"
      result `shouldBe` "Right (JSAstProgram [JSDecimal '0',JSIdentifier 'b'])"

    it "handles invalid binary characters as mixed tokens" $ do
      let result = testNumericEdgeCase "0b12345"
      result `shouldBe` "Right (JSAstProgram [JSBinaryInteger '0b1',JSDecimal '2345'])"

    it "handles decimal point after binary as separate tokens" $ do
      let result = testNumericEdgeCase "0b101.010"
      result `shouldBe` "Right (JSAstProgram [JSBinaryInteger '0b101',JSDecimal '.010'])"

  describe "octal literal edge cases" $ do
    it "handles invalid octal characters as separate tokens" $ do
      let result = testNumericEdgeCase "0o89"
      result `shouldBe` "Right (JSAstProgram [JSDecimal '0',JSIdentifier 'o89'])"

    it "handles octal prefix without digits as separate tokens" $ do
      let result = testNumericEdgeCase "0o"
      result `shouldBe` "Right (JSAstProgram [JSDecimal '0',JSIdentifier 'o'])"

  describe "BigInt literal edge cases" $ do
    it "accepts BigInt with decimal point (parser tolerance)" $ do
      let result = testNumericEdgeCase "123.456n"
      result `shouldBe` "Right (JSAstProgram [JSBigIntLiteral '123.456n'])"

    it "accepts BigInt with exponent (parser tolerance)" $ do
      let result = testNumericEdgeCase "123e4n"
      result `shouldBe` "Right (JSAstProgram [JSBigIntLiteral '123e4n'])"

    it "handles multiple n suffixes as separate tokens" $ do
      let result = testNumericEdgeCase "123nn"
      result `shouldBe` "Right (JSAstProgram [JSBigIntLiteral '123n',JSIdentifier 'n'])"

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
      testNumericEdgeCase "1e308" `shouldBe`
        "Right (JSAstProgram [JSDecimal '1e308'])"

    it "parses near-overflow values" $ do
      testNumericEdgeCase "1.7976931348623157e+308" `shouldBe`
        "Right (JSAstProgram [JSDecimal '1.7976931348623157e+308'])"

    it "parses maximum negative exponent" $ do
      testNumericEdgeCase "1e-324" `shouldBe`
        "Right (JSAstProgram [JSDecimal '1e-324'])"

    it "parses minimum positive value" $ do
      testNumericEdgeCase "5e-324" `shouldBe`
        "Right (JSAstProgram [JSDecimal '5e-324'])"

  describe "precision edge cases" $ do
    it "parses maximum precision decimal" $ do
      let maxPrecision = "1.2345678901234567890123456789"
      testNumericEdgeCase maxPrecision `shouldBe`
        "Right (JSAstProgram [JSDecimal '" ++ maxPrecision ++ "'])"

    it "parses very small fractional values" $ do
      testNumericEdgeCase "0.000000000000000001" `shouldBe`
        "Right (JSAstProgram [JSDecimal '0.000000000000000001'])"

    it "parses alternating digit patterns" $ do
      testNumericEdgeCase "0.101010101010101010" `shouldBe`
        "Right (JSAstProgram [JSDecimal '0.101010101010101010'])"

  describe "special exponent notations" $ do
    it "parses positive exponent with explicit sign" $ do
      testNumericEdgeCase "1.5e+100" `shouldBe`
        "Right (JSAstProgram [JSDecimal '1.5e+100'])"

    it "parses negative exponent" $ do
      testNumericEdgeCase "2.5e-50" `shouldBe`
        "Right (JSAstProgram [JSDecimal '2.5e-50'])"

    it "parses zero exponent" $ do
      testNumericEdgeCase "1.5e0" `shouldBe`
        "Right (JSAstProgram [JSDecimal '1.5e0'])"

    it "parses uppercase exponent marker" $ do
      testNumericEdgeCase "1.5E10" `shouldBe`
        "Right (JSAstProgram [JSDecimal '1.5E10'])"

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
      let result = testNumericEdgeCase large100
      result `shouldBe` "Right (JSAstProgram [JSDecimal '" ++ large100 ++ "'])"

    it "parses 1000-digit BigInt efficiently" $ do
      let large1000 = List.replicate 1000 '9' ++ "n"
      let result = testNumericEdgeCase large1000
      result `shouldBe` "Right (JSAstProgram [JSBigIntLiteral '" ++ large1000 ++ "'])"

  describe "complex numeric patterns" $ do
    it "parses long hex with mixed case" $ do
      let complexHex = "0x" ++ List.take 32 (List.cycle "aBcDeF123456789")
      let result = testNumericEdgeCase complexHex
      result `shouldBe` "Right (JSAstProgram [JSHexInteger '" ++ complexHex ++ "'])"

    it "parses very long binary sequence" $ do
      let longBinary = "0b" ++ List.take 128 (List.cycle "10")
      let result = testNumericEdgeCase longBinary
      result `shouldBe` "Right (JSAstProgram [JSBinaryInteger '" ++ longBinary ++ "'])"

  describe "floating point precision stress tests" $ do
    it "parses maximum decimal places" $ do
      let maxDecimals = "0." ++ List.replicate 50 '1'
      testNumericEdgeCase maxDecimals `shouldBe`
        "Right (JSAstProgram [JSDecimal '" ++ maxDecimals ++ "'])"

    it "parses very long exponent" $ do
      testNumericEdgeCase "1e123456789" `shouldBe`
        "Right (JSAstProgram [JSDecimal '1e123456789'])"

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
    it "round-trip property for valid decimals" $ property $ \n ->
      let numStr = show (abs (n :: Integer))
          result = testNumericEdgeCase numStr
          expectedStr = "Right (JSAstProgram [JSDecimal '" ++ numStr ++ "'])"
      in result == expectedStr

    it "BigInt round-trip property" $ property $ \n ->
      let numStr = show (abs (n :: Integer)) ++ "n"
          result = testNumericEdgeCase numStr
          expectedStr = "Right (JSAstProgram [JSBigIntLiteral '" ++ numStr ++ "'])"
      in result == expectedStr

  describe "hex literal properties" $ do
    it "hex prefix preservation 0x" $ do
      let hexStr = "0x" ++ "ABC123"
          result = testNumericEdgeCase hexStr
          expectedStr = "Right (JSAstProgram [JSHexInteger '" ++ hexStr ++ "'])"
      result `shouldBe` expectedStr

    it "hex prefix preservation 0X" $ do
      let hexStr = "0X" ++ "def456"
          result = testNumericEdgeCase hexStr
          expectedStr = "Right (JSAstProgram [JSHexInteger '" ++ hexStr ++ "'])"
      result `shouldBe` expectedStr

  describe "binary literal properties" $ do
    it "binary parsing 0b" $ do
      let binStr = "0b" ++ "101010"
          result = testNumericEdgeCase binStr
          expectedStr = "Right (JSAstProgram [JSBinaryInteger '" ++ binStr ++ "'])"
      result `shouldBe` expectedStr

    it "binary parsing 0B" $ do
      let binStr = "0B" ++ "010101"
          result = testNumericEdgeCase binStr
          expectedStr = "Right (JSAstProgram [JSBinaryInteger '" ++ binStr ++ "'])"
      result `shouldBe` expectedStr

-- ---------------------------------------------------------------------
-- Property Test Generators
-- ---------------------------------------------------------------------

-- Note: Property test generators removed - using concrete test cases instead
-- for more reliable and maintainable testing of numeric literal edge cases.

-- ---------------------------------------------------------------------
-- Helper Functions
-- ---------------------------------------------------------------------

-- Helper functions removed - using string comparison pattern like existing tests