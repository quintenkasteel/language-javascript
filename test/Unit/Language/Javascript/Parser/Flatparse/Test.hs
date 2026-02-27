{-# LANGUAGE OverloadedStrings #-}

-- | Basic unit tests for flatparse modules to verify Phase 1 implementation.
--
-- These tests verify that the new flatparse-based modules compile correctly
-- and provide the expected interfaces. They serve as smoke tests for the
-- Phase 1 foundation before implementing full parsing functionality.
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Parser.Flatparse.Test where

import Test.Hspec
import Language.JavaScript.Parser.Flatparse.Pos
import qualified Language.JavaScript.Parser.AST as AST
import Language.JavaScript.Parser.Flatparse.Primitives
import Language.JavaScript.Parser.Flatparse.Benchmark

-- | Test specification for flatparse modules.
tests :: Spec
tests = describe "Flatparse Foundation Tests" $ do
  positionTests
  astTests
  primitivesTests
  benchmarkTests

-- | Test position encoding functionality.
positionTests :: Spec
positionTests = describe "Position encoding" $ do
  it "creates position correctly" $ do
    let pos = mkPos 42 17
    posLine pos `shouldBe` 42
    posColumn pos `shouldBe` 17

  it "shows position in expected format" $ do
    showPos (mkPos 10 5) `shouldBe` "10:5"

  it "advances column correctly" $ do
    let pos = mkPos 10 15
    let advanced = advanceColumn 5 pos
    posLine advanced `shouldBe` 10
    posColumn advanced `shouldBe` 20

  it "advances line correctly" $ do
    let pos = mkPos 10 15
    let advanced = advanceLine pos
    posLine advanced `shouldBe` 11
    posColumn advanced `shouldBe` 1

  it "handles noPos correctly" $ do
    posLine noPos `shouldBe` 0
    posColumn noPos `shouldBe` 0

-- | Test AST construction and smart constructors.
astTests :: Spec
astTests = describe "AST construction" $ do
  it "creates identifier expression" $ do
    let pos = mkPos 1 5
    let ident = AST.mkIdentifier pos "myVar"
    AST.getExpressionPos ident `shouldBe` pos
    AST.isIdentifier ident `shouldBe` True

  it "creates string literal" $ do
    let pos = mkPos 2 10
    let literal = AST.mkStringLiteral pos "hello"
    AST.getExpressionPos literal `shouldBe` pos
    AST.isLiteral literal `shouldBe` True

  it "creates numeric literal" $ do
    let pos = mkPos 3 1
    let literal = AST.mkNumericLiteral pos "42"
    AST.getExpressionPos literal `shouldBe` pos
    AST.isLiteral literal `shouldBe` True

  it "creates binary operation" $ do
    let pos = mkPos 4 8
    let left = AST.mkIdentifier pos "a"
    let right = AST.mkIdentifier pos "b"
    let binOp = AST.mkBinaryOp pos AST.JSBinOpPlus left right
    AST.getExpressionPos binOp `shouldBe` pos

  it "creates function call" $ do
    let pos = mkPos 5 12
    let func = AST.mkIdentifier pos "myFunc"
    let args = [AST.mkStringLiteral pos "arg1"]
    let call = AST.mkFunctionCall pos func args
    AST.getExpressionPos call `shouldBe` pos

-- | Test primitive character class functions.
primitivesTests :: Spec
primitivesTests = describe "Character classes" $ do
  it "identifies identifier start characters" $ do
    isIdentifierStart 'a' `shouldBe` True
    isIdentifierStart 'Z' `shouldBe` True
    isIdentifierStart '$' `shouldBe` True
    isIdentifierStart '_' `shouldBe` True
    isIdentifierStart '1' `shouldBe` False
    isIdentifierStart '+' `shouldBe` False

  it "identifies identifier continue characters" $ do
    isIdentifierContinue 'a' `shouldBe` True
    isIdentifierContinue '5' `shouldBe` True
    isIdentifierContinue '_' `shouldBe` True
    isIdentifierContinue '+' `shouldBe` False

  it "identifies digits correctly" $ do
    isDigit '0' `shouldBe` True
    isDigit '9' `shouldBe` True
    isDigit 'a' `shouldBe` False

  it "identifies hex digits correctly" $ do
    isHexDigit '0' `shouldBe` True
    isHexDigit '9' `shouldBe` True
    isHexDigit 'a' `shouldBe` True
    isHexDigit 'F' `shouldBe` True
    isHexDigit 'g' `shouldBe` False

  it "identifies octal digits correctly" $ do
    isOctalDigit '0' `shouldBe` True
    isOctalDigit '7' `shouldBe` True
    isOctalDigit '8' `shouldBe` False

  it "identifies binary digits correctly" $ do
    isBinaryDigit '0' `shouldBe` True
    isBinaryDigit '1' `shouldBe` True
    isBinaryDigit '2' `shouldBe` False

  it "identifies whitespace correctly" $ do
    isWhitespace ' ' `shouldBe` True
    isWhitespace '\t' `shouldBe` True
    isWhitespace '\n' `shouldBe` True
    isWhitespace 'a' `shouldBe` False

-- | Test benchmark infrastructure.
benchmarkTests :: Spec
benchmarkTests = describe "Benchmark infrastructure" $ do
  it "has micro samples" $ do
    length microSamples `shouldSatisfy` (> 0)
    let firstSample = head microSamples
    sampleName firstSample `shouldNotBe` ""
    sampleInput firstSample `shouldNotBe` ""

  it "has expression samples" $ do
    length expressionSamples `shouldSatisfy` (> 0)

  it "has statement samples" $ do
    length statementSamples `shouldSatisfy` (> 0)

  it "has file samples" $ do
    length fileSamples `shouldSatisfy` (> 0)

  it "can run benchmark suites" $ do
    -- These are placeholders for Phase 1, just verify they return results
    suite <- runMicroBenchmarks
    suiteName suite `shouldNotBe` ""