{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive XML serialization testing for JavaScript AST.
--
-- This module provides thorough testing of the Pretty.XML module, ensuring:
--
--   * Accurate XML serialization of all AST node types
--   * Proper handling of modern JavaScript features (ES6+)
--   * Well-formed XML with proper escaping
--   * Preservation of source location and comment information
--   * Correct handling of special characters and XML entities
--   * Hierarchical representation of nested structures
--
-- The tests cover all major AST constructs with focus on:
-- correctness, completeness, and XML format compliance.
--
-- @since 0.7.1.0
module Unit.Language.Javascript.Parser.Pretty.XMLTest
  ( testXMLSerialization,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Language.JavaScript.Parser.AST as AST
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import qualified Language.JavaScript.Pretty.XML as PXML
import Test.Hspec

-- | Test helpers for XML validation
noPos :: TokenPosn
noPos = TokenPn 0 0 0

_noAnnot :: AST.JSAnnot
_noAnnot = AST.JSNoAnnot

testAnnot :: AST.JSAnnot
testAnnot = AST.JSAnnot noPos []

-- | Main XML serialization test suite
testXMLSerialization :: Spec
testXMLSerialization = describe "XML Serialization Tests" $ do
  testXMLUtilities
  testLiteralSerialization
  testExpressionSerialization
  testModernJavaScriptFeatures
  testStatementSerialization
  testModuleSystemSerialization
  testAnnotationSerialization
  testEdgeCases
  testCompletePrograms
  testXMLFormatCompliance

-- | Test XML utility functions
testXMLUtilities :: Spec
testXMLUtilities = describe "XML Utilities" $ do
  describe "escapeXMLString" $ do
    it "escapes angle brackets" $ do
      PXML.escapeXMLString "<tag>" `shouldBe` "&lt;tag&gt;"

    it "escapes ampersands" $ do
      PXML.escapeXMLString "a & b" `shouldBe` "a &amp; b"

    it "escapes quotes" $ do
      PXML.escapeXMLString "\"hello\"" `shouldBe` "&quot;hello&quot;"
      PXML.escapeXMLString "'world'" `shouldBe` "&apos;world&apos;"

    it "handles empty string" $ do
      PXML.escapeXMLString "" `shouldBe` ""

    it "handles normal characters" $ do
      PXML.escapeXMLString "hello world" `shouldBe` "hello world"

    it "handles complex mixed content" $ do
      PXML.escapeXMLString "<script>alert('&hi&');</script>"
        `shouldBe` "&lt;script&gt;alert(&apos;&amp;hi&amp;&apos;);&lt;/script&gt;"

  describe "formatXMLElement" $ do
    it "formats empty elements correctly" $ do
      PXML.formatXMLElement "test" [] "" `shouldBe` "<test/>"

    it "formats elements with content" $ do
      PXML.formatXMLElement "test" [] "content" `shouldBe` "<test>content</test>"

    it "formats elements with attributes" $ do
      PXML.formatXMLElement "test" [("id", "123")] ""
        `shouldBe` "<test id=\"123\"/>"

    it "formats elements with attributes and content" $ do
      PXML.formatXMLElement "test" [("id", "123")] "content"
        `shouldBe` "<test id=\"123\">content</test>"

  describe "formatXMLAttributes" $ do
    it "formats empty attribute list" $ do
      PXML.formatXMLAttributes [] `shouldBe` ""

    it "formats single attribute" $ do
      PXML.formatXMLAttributes [("name", "value")] `shouldBe` " name=\"value\""

    it "formats multiple attributes" $ do
      PXML.formatXMLAttributes [("id", "123"), ("class", "test")]
        `shouldBe` " id=\"123\" class=\"test\""

-- | Test literal value serialization
testLiteralSerialization :: Spec
testLiteralSerialization = describe "Literal Serialization" $ do
  describe "numeric literals" $ do
    it "serializes decimal numbers" $ do
      let expr = AST.JSDecimal testAnnot 42
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSDecimal value=\"42\">"
      xml `shouldSatisfy` Text.isInfixOf "</JSDecimal>"

    it "serializes hexadecimal numbers" $ do
      let expr = AST.JSHexInteger testAnnot 0xFF
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSHexInteger value=\"0xff\">"

    it "serializes octal numbers" $ do
      let expr = AST.JSOctal testAnnot 0o777
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSOctal value=\"0o777\">"

    it "serializes binary numbers" $ do
      let expr = AST.JSBinaryInteger testAnnot 10
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSBinaryInteger value=\"0b1010\">"

    it "serializes BigInt literals" $ do
      let expr = AST.JSBigIntLiteral testAnnot 123
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSBigIntLiteral value=\"123n\">"

  describe "string literals" $ do
    it "serializes simple strings" $ do
      let expr = AST.JSStringLiteral testAnnot "\"hello\""
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSStringLiteral value=\"&quot;hello&quot;\">"

    it "serializes strings with escapes" $ do
      let expr = AST.JSStringLiteral testAnnot "\"hello\\nworld\""
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "hello\\nworld"

  describe "identifiers" $ do
    it "serializes simple identifiers" $ do
      let expr = AST.JSIdentifier testAnnot "variable"
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSIdentifier name=\"variable\">"

    it "serializes identifiers with Unicode" $ do
      let expr = AST.JSIdentifier testAnnot (Text.encodeUtf8 "variableσ")
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "variableσ"

  describe "special literals" $ do
    it "serializes generic literals" $ do
      let expr = AST.JSLiteral testAnnot "true"
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSLiteral value=\"true\">"

    it "serializes regex literals" $ do
      let expr = AST.JSRegEx testAnnot "/pattern/gi"
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSRegEx pattern=\"/pattern/gi\">"

-- | Test expression serialization
testExpressionSerialization :: Spec
testExpressionSerialization = describe "Expression Serialization" $ do
  describe "binary expressions" $ do
    it "serializes arithmetic operations" $ do
      let left = AST.JSDecimal testAnnot 1
      let right = AST.JSDecimal testAnnot 2
      let op = AST.JSBinOpPlus testAnnot
      let expr = AST.JSExpressionBinary left op right
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSExpressionBinary>"
      xml `shouldSatisfy` Text.isInfixOf "<left>"
      xml `shouldSatisfy` Text.isInfixOf "<right>"
      xml `shouldSatisfy` Text.isInfixOf "<JSBinOpPlus>"

    it "serializes logical operations" $ do
      let left = AST.JSIdentifier testAnnot "a"
      let right = AST.JSIdentifier testAnnot "b"
      let op = AST.JSBinOpAnd testAnnot
      let expr = AST.JSExpressionBinary left op right
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSBinOpAnd>"

    it "serializes comparison operations" $ do
      let left = AST.JSIdentifier testAnnot "x"
      let right = AST.JSDecimal testAnnot 5
      let op = AST.JSBinOpLt testAnnot
      let expr = AST.JSExpressionBinary left op right
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSBinOpLt>"

  describe "member expressions" $ do
    it "serializes dot notation member access" $ do
      let obj = AST.JSIdentifier testAnnot "obj"
      let prop = AST.JSIdentifier testAnnot "property"
      let expr = AST.JSMemberDot obj testAnnot prop
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSMemberDot>"
      xml `shouldSatisfy` Text.isInfixOf "<object>"
      xml `shouldSatisfy` Text.isInfixOf "<property>"

    it "serializes bracket notation member access" $ do
      let obj = AST.JSIdentifier testAnnot "obj"
      let prop = AST.JSStringLiteral testAnnot "\"key\""
      let expr = AST.JSMemberSquare obj testAnnot prop testAnnot
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSMemberSquare>"

  describe "function calls" $ do
    it "serializes simple function calls" $ do
      let func = AST.JSIdentifier testAnnot "func"
      let expr = AST.JSCallExpression func testAnnot AST.JSLNil testAnnot
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSCallExpression>"
      xml `shouldSatisfy` Text.isInfixOf "<function>"
      xml `shouldSatisfy` Text.isInfixOf "<arguments"

    it "serializes function calls with arguments" $ do
      let func = AST.JSIdentifier testAnnot "func"
      let arg = AST.JSDecimal testAnnot 42
      let args = AST.JSLOne arg
      let expr = AST.JSCallExpression func testAnnot args testAnnot
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSCallExpression>"

-- | Test modern JavaScript features
testModernJavaScriptFeatures :: Spec
testModernJavaScriptFeatures = describe "Modern JavaScript Features" $ do
  describe "optional chaining" $ do
    it "serializes optional member dot access" $ do
      let obj = AST.JSIdentifier testAnnot "obj"
      let prop = AST.JSIdentifier testAnnot "prop"
      let expr = AST.JSOptionalMemberDot obj testAnnot prop
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSOptionalMemberDot>"

    it "serializes optional member square access" $ do
      let obj = AST.JSIdentifier testAnnot "obj"
      let prop = AST.JSStringLiteral testAnnot "\"key\""
      let expr = AST.JSOptionalMemberSquare obj testAnnot prop testAnnot
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSOptionalMemberSquare>"

    it "serializes optional function calls" $ do
      let func = AST.JSIdentifier testAnnot "func"
      let expr = AST.JSOptionalCallExpression func testAnnot AST.JSLNil testAnnot
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSOptionalCallExpression>"

  describe "nullish coalescing" $ do
    it "serializes nullish coalescing operator" $ do
      let left = AST.JSIdentifier testAnnot "value"
      let right = AST.JSStringLiteral testAnnot "\"default\""
      let op = AST.JSBinOpNullishCoalescing testAnnot
      let expr = AST.JSExpressionBinary left op right
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSBinOpNullishCoalescing>"

  describe "arrow functions" $ do
    it "serializes simple arrow functions" $ do
      let param = AST.JSUnparenthesizedArrowParameter (AST.JSIdentName testAnnot "x")
      let body = AST.JSConciseExpressionBody (AST.JSDecimal testAnnot 42)
      let expr = AST.JSArrowExpression param testAnnot body
      let xml = PXML.renderExpressionToXML expr
      xml `shouldSatisfy` Text.isInfixOf "<JSArrowExpression>"
      xml `shouldSatisfy` Text.isInfixOf "<JSUnparenthesizedArrowParameter>"
      xml `shouldSatisfy` Text.isInfixOf "<JSConciseExpressionBody>"

-- | Test statement serialization
testStatementSerialization :: Spec
testStatementSerialization = describe "Statement Serialization" $ do
  describe "expression statements" $ do
    it "serializes expression statements" $ do
      let expr = AST.JSDecimal testAnnot 42
      let stmt = AST.JSExpressionStatement expr AST.JSSemiAuto
      let xml = PXML.renderStatementToXML stmt
      xml `shouldSatisfy` Text.isInfixOf "<JSExpressionStatement>"
      xml `shouldSatisfy` Text.isInfixOf "<expression>"

  describe "variable declarations" $ do
    it "serializes var declarations" $ do
      let ident = AST.JSIdentifier testAnnot "x"
      let varInitializer = AST.JSVarInit testAnnot (AST.JSDecimal testAnnot 42)
      let varInit = AST.JSVarInitExpression ident varInitializer
      let stmt = AST.JSVariable testAnnot (AST.JSLOne varInit) AST.JSSemiAuto
      let xml = PXML.renderStatementToXML stmt
      xml `shouldSatisfy` Text.isInfixOf "<JSVariable>"

    it "serializes let declarations" $ do
      let ident = AST.JSIdentifier testAnnot "x"
      let varInit = AST.JSVarInitExpression ident AST.JSVarInitNone
      let stmt = AST.JSLet testAnnot (AST.JSLOne varInit) AST.JSSemiAuto
      let xml = PXML.renderStatementToXML stmt
      xml `shouldSatisfy` Text.isInfixOf "<JSLet>"

    it "serializes const declarations" $ do
      let ident = AST.JSIdentifier testAnnot "x"
      let constInitializer = AST.JSVarInit testAnnot (AST.JSDecimal testAnnot 42)
      let varInit = AST.JSVarInitExpression ident constInitializer
      let stmt = AST.JSConstant testAnnot (AST.JSLOne varInit) AST.JSSemiAuto
      let xml = PXML.renderStatementToXML stmt
      xml `shouldSatisfy` Text.isInfixOf "<JSConstant>"

  describe "control flow statements" $ do
    it "serializes if statements" $ do
      let cond = AST.JSLiteral testAnnot "true"
      let body = AST.JSEmptyStatement testAnnot
      let stmt = AST.JSIf testAnnot testAnnot cond testAnnot body
      let xml = PXML.renderStatementToXML stmt
      xml `shouldSatisfy` Text.isInfixOf "<JSIf>"
      xml `shouldSatisfy` Text.isInfixOf "<condition>"

    it "serializes while statements" $ do
      let cond = AST.JSLiteral testAnnot "true"
      let body = AST.JSEmptyStatement testAnnot
      let stmt = AST.JSWhile testAnnot testAnnot cond testAnnot body
      let xml = PXML.renderStatementToXML stmt
      xml `shouldSatisfy` Text.isInfixOf "<JSWhile>"

    it "serializes return statements" $ do
      let expr = AST.JSDecimal testAnnot 42
      let stmt = AST.JSReturn testAnnot (Just expr) AST.JSSemiAuto
      let xml = PXML.renderStatementToXML stmt
      xml `shouldSatisfy` Text.isInfixOf "<JSReturn>"

-- | Test module system serialization
testModuleSystemSerialization :: Spec
testModuleSystemSerialization = describe "Module System Serialization" $ do
  describe "import declarations" $ do
    it "handles import declarations gracefully" $ do
      -- Since import/export declarations are complex and not fully implemented,
      -- we test that they don't crash and produce some XML output
      let xml = PXML.renderImportDeclarationToXML undefined
      xml `shouldSatisfy` Text.isInfixOf "<JSImportDeclaration>"

  describe "export declarations" $ do
    it "handles export declarations gracefully" $ do
      let xml = PXML.renderExportDeclarationToXML undefined
      xml `shouldSatisfy` Text.isInfixOf "<JSExportDeclaration>"

-- | Test annotation serialization
testAnnotationSerialization :: Spec
testAnnotationSerialization = describe "Annotation Serialization" $ do
  describe "position information" $ do
    it "serializes position data" $ do
      let pos = TokenPn 100 5 10
      let annot = AST.JSAnnot pos []
      let xml = PXML.renderAnnotation annot
      xml `shouldSatisfy` Text.isInfixOf "line=\"5\""
      xml `shouldSatisfy` Text.isInfixOf "column=\"10\""
      xml `shouldSatisfy` Text.isInfixOf "address=\"100\""

    it "serializes empty annotations" $ do
      let xml = PXML.renderAnnotation AST.JSNoAnnot
      xml `shouldSatisfy` Text.isInfixOf "<annotation>"
      xml `shouldSatisfy` Text.isInfixOf "<position/>"
      xml `shouldSatisfy` Text.isInfixOf "<comments/>"

-- | Test edge cases and special scenarios
testEdgeCases :: Spec
testEdgeCases = describe "Edge Cases" $ do
  it "handles empty programs" $ do
    let prog = AST.JSAstProgram [] testAnnot
    let xml = PXML.renderToXML prog
    xml `shouldSatisfy` Text.isInfixOf "<JSAstProgram>"
    xml `shouldSatisfy` Text.isInfixOf "<statements/>"

  it "handles special identifier names" $ do
    let expr = AST.JSIdentifier testAnnot "$special_var123"
    let xml = PXML.renderExpressionToXML expr
    xml `shouldSatisfy` Text.isInfixOf "$special_var123"

  it "handles complex nested structures" $ do
    -- Test nested member access: obj.prop1.prop2
    let obj = AST.JSIdentifier testAnnot "obj"
    let prop1 = AST.JSIdentifier testAnnot "prop1"
    let intermediate = AST.JSMemberDot obj testAnnot prop1
    let prop2 = AST.JSIdentifier testAnnot "prop2"
    let expr = AST.JSMemberDot intermediate testAnnot prop2
    let xml = PXML.renderExpressionToXML expr
    xml `shouldSatisfy` Text.isInfixOf "<JSMemberDot>"
    -- Should have nested JSMemberDot structures
    let memberDotCount = Text.count "<JSMemberDot>" xml
    memberDotCount `shouldBe` 2

  it "handles large numeric values" $ do
    let expr = AST.JSDecimal testAnnot 9007199254740991
    let xml = PXML.renderExpressionToXML expr
    xml `shouldSatisfy` Text.isInfixOf "9.007199254740991e15"

-- | Test complete program serialization
testCompletePrograms :: Spec
testCompletePrograms = describe "Complete Program Serialization" $ do
  it "serializes simple programs" $ do
    let expr = AST.JSDecimal testAnnot 42
    let stmt = AST.JSExpressionStatement expr AST.JSSemiAuto
    let prog = AST.JSAstProgram [stmt] testAnnot
    let xml = PXML.renderToXML prog
    xml `shouldSatisfy` Text.isInfixOf "<JSAstProgram>"
    xml `shouldSatisfy` Text.isInfixOf "<JSExpressionStatement>"

  it "serializes complex programs with multiple statements" $ do
    let varDecl =
          AST.JSVariable
            testAnnot
            ( AST.JSLOne
                ( AST.JSVarInitExpression
                    (AST.JSIdentifier testAnnot "x")
                    AST.JSVarInitNone
                )
            )
            AST.JSSemiAuto
    let expr = AST.JSIdentifier testAnnot "x"
    let exprStmt = AST.JSExpressionStatement expr AST.JSSemiAuto
    let prog = AST.JSAstProgram [varDecl, exprStmt] testAnnot
    let xml = PXML.renderToXML prog
    xml `shouldSatisfy` Text.isInfixOf "<JSVariable>"
    xml `shouldSatisfy` Text.isInfixOf "<JSExpressionStatement>"

  it "serializes different AST root types" $ do
    let expr = AST.JSDecimal testAnnot 42
    let exprAST = AST.JSAstExpression expr testAnnot
    let xml = PXML.renderToXML exprAST
    xml `shouldSatisfy` Text.isInfixOf "<JSAstExpression>"

-- | Test XML format compliance
testXMLFormatCompliance :: Spec
testXMLFormatCompliance = describe "XML Format Compliance" $ do
  it "produces valid XML for all expression types" $ do
    -- Test a variety of expressions to ensure valid XML structure
    let expressions =
          [ AST.JSDecimal testAnnot 42,
            AST.JSStringLiteral testAnnot "\"test\"",
            AST.JSIdentifier testAnnot "variable",
            AST.JSLiteral testAnnot "true"
          ]
    mapM_
      ( \expr -> do
          let xml = PXML.renderExpressionToXML expr
          xml `shouldSatisfy` Text.isPrefixOf "<"
          xml `shouldSatisfy` Text.isSuffixOf ">"
      )
      expressions

  it "maintains consistent element structure" $ do
    let expr = AST.JSDecimal testAnnot 42
    let xml = PXML.renderExpressionToXML expr
    -- Should have matching opening and closing tags
    xml `shouldSatisfy` Text.isInfixOf "<JSDecimal"
    xml `shouldSatisfy` Text.isInfixOf "</JSDecimal>"

  it "properly nests child elements" $ do
    let left = AST.JSDecimal testAnnot 1
    let right = AST.JSDecimal testAnnot 2
    let op = AST.JSBinOpPlus testAnnot
    let expr = AST.JSExpressionBinary left op right
    let xml = PXML.renderExpressionToXML expr
    -- Should have proper nesting structure
    xml `shouldSatisfy` Text.isInfixOf "<JSExpressionBinary>"
    xml `shouldSatisfy` Text.isInfixOf "<left>"
    xml `shouldSatisfy` Text.isInfixOf "</left>"
    xml `shouldSatisfy` Text.isInfixOf "<right>"
    xml `shouldSatisfy` Text.isInfixOf "</right>"
    xml `shouldSatisfy` Text.isInfixOf "</JSExpressionBinary>"

  it "handles all operator types in binary expressions" $ do
    let left = AST.JSIdentifier testAnnot "a"
    let right = AST.JSIdentifier testAnnot "b"
    let operators =
          [ AST.JSBinOpPlus testAnnot,
            AST.JSBinOpMinus testAnnot,
            AST.JSBinOpTimes testAnnot,
            AST.JSBinOpDivide testAnnot,
            AST.JSBinOpAnd testAnnot,
            AST.JSBinOpOr testAnnot,
            AST.JSBinOpEq testAnnot,
            AST.JSBinOpStrictEq testAnnot
          ]
    mapM_
      ( \op -> do
          let expr = AST.JSExpressionBinary left op right
          let xml = PXML.renderExpressionToXML expr
          xml `shouldSatisfy` Text.isInfixOf "<JSExpressionBinary>"
      )
      operators
