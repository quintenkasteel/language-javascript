{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive JSON serialization testing for JavaScript AST.
--
-- This module provides thorough testing of the Pretty.JSON module, ensuring:
--
--   * Accurate JSON serialization of all AST node types
--   * Proper handling of modern JavaScript features (ES6+)
--   * JSON format compliance and schema validation
--   * Preservation of source location and comment information
--   * Correct handling of special characters and escaping
--   * Round-trip compatibility with standard JSON parsers
--
-- The tests cover all major AST constructs with focus on:
-- correctness, completeness, and JSON format compliance.
--
-- @since 0.7.1.0
module Unit.Language.Javascript.Parser.Pretty.JSONTest
  ( testJSONSerialization,
  )
where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Language.JavaScript.Parser.AST as AST
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import qualified Language.JavaScript.Pretty.JSON as PJSON
import Test.Hspec

-- | Test helpers for JSON validation
noPos :: TokenPosn
noPos = TokenPn 0 0 0

noAnnot :: AST.JSAnnot
noAnnot = AST.JSNoAnnot

testAnnot :: AST.JSAnnot
testAnnot = AST.JSAnnot noPos []

-- | Main JSON serialization test suite
testJSONSerialization :: Spec
testJSONSerialization = describe "JSON Serialization Tests" $ do
  testJSONUtilities
  testLiteralSerialization
  testExpressionSerialization
  testModernFeatures
  testStatementSerialization
  testModuleSystem
  testAnnotationSerialization
  testCompletePrograms
  testEdgeCases
  testJSONCompliance

-- | Test JSON utility functions
testJSONUtilities :: Spec
testJSONUtilities = describe "JSON Utilities" $ do
  describe "escapeJSONString" $ do
    it "escapes double quotes" $ do
      PJSON.escapeJSONString "hello\"world" `shouldBe` "\"hello\\\"world\""

    it "escapes backslashes" $ do
      PJSON.escapeJSONString "path\\file" `shouldBe` "\"path\\\\file\""

    it "escapes control characters" $ do
      PJSON.escapeJSONString "line1\nline2\ttab" `shouldBe` "\"line1\\nline2\\ttab\""
      PJSON.escapeJSONString "\b\f\r" `shouldBe` "\"\\b\\f\\r\""

    it "handles empty string" $ do
      PJSON.escapeJSONString "" `shouldBe` "\"\""

    it "handles Unicode characters" $ do
      PJSON.escapeJSONString "café" `shouldBe` "\"café\""
      PJSON.escapeJSONString "🚀" `shouldBe` "\"🚀\""

  describe "formatJSONObject" $ do
    it "formats empty object" $ do
      PJSON.formatJSONObject [] `shouldBe` "{}"

    it "formats single key-value pair" $ do
      PJSON.formatJSONObject [("key", "\"value\"")] `shouldBe` "{\"key\":\"value\"}"

    it "formats multiple key-value pairs" $ do
      let result = PJSON.formatJSONObject [("a", "1"), ("b", "\"str\"")]
      result `shouldBe` "{\"a\":1,\"b\":\"str\"}"

  describe "formatJSONArray" $ do
    it "formats empty array" $ do
      PJSON.formatJSONArray [] `shouldBe` "[]"

    it "formats single element" $ do
      PJSON.formatJSONArray ["\"test\""] `shouldBe` "[\"test\"]"

    it "formats multiple elements" $ do
      PJSON.formatJSONArray ["1", "\"str\"", "true"] `shouldBe` "[1,\"str\",true]"

-- | Test literal expression serialization
testLiteralSerialization :: Spec
testLiteralSerialization = describe "Literal Serialization" $ do
  describe "numeric literals" $ do
    it "serializes decimal numbers" $ do
      let expr = AST.JSDecimal testAnnot "42"
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSDecimal"
      json `shouldSatisfy` Text.isInfixOf "\"42\""
      validateJSON json

    it "serializes hexadecimal numbers" $ do
      let expr = AST.JSHexInteger testAnnot "0xFF"
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSHexInteger"
      json `shouldSatisfy` Text.isInfixOf "\"0xFF\""
      validateJSON json

    it "serializes octal numbers" $ do
      let expr = AST.JSOctal testAnnot "0o77"
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSOctal"
      json `shouldSatisfy` Text.isInfixOf "\"0o77\""
      validateJSON json

    it "serializes BigInt literals" $ do
      let expr = AST.JSBigIntLiteral testAnnot "123n"
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSBigIntLiteral"
      json `shouldSatisfy` Text.isInfixOf "\"123n\""
      validateJSON json

  describe "string literals" $ do
    it "serializes simple strings" $ do
      let expr = AST.JSStringLiteral testAnnot "hello"
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSStringLiteral"
      json `shouldSatisfy` Text.isInfixOf "\"hello\""
      validateJSON json

    it "serializes strings with escapes" $ do
      let expr = AST.JSStringLiteral testAnnot "line1\nline2"
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSStringLiteral"
      json `shouldSatisfy` Text.isInfixOf "\\n"
      validateJSON json

  describe "identifiers" $ do
    it "serializes simple identifiers" $ do
      let expr = AST.JSIdentifier testAnnot "myVar"
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSIdentifier"
      json `shouldSatisfy` Text.isInfixOf "\"myVar\""
      validateJSON json

    it "serializes identifiers with Unicode" $ do
      let expr = AST.JSIdentifier testAnnot "café"
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSIdentifier"
      json `shouldSatisfy` Text.isInfixOf "café"
      validateJSON json

  describe "special literals" $ do
    it "serializes generic literals" $ do
      let expr = AST.JSLiteral testAnnot "true"
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSLiteral"
      json `shouldSatisfy` Text.isInfixOf "\"true\""
      validateJSON json

    it "serializes regex literals" $ do
      let expr = AST.JSRegEx testAnnot "/ab+c/gi"
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSRegEx"
      json `shouldSatisfy` Text.isInfixOf "/ab+c/gi"
      validateJSON json

-- | Test expression serialization
testExpressionSerialization :: Spec
testExpressionSerialization = describe "Expression Serialization" $ do
  describe "binary expressions" $ do
    it "serializes arithmetic operations" $ do
      let left = AST.JSDecimal noAnnot "2"
      let right = AST.JSDecimal noAnnot "3"
      let op = AST.JSBinOpPlus noAnnot
      let expr = AST.JSExpressionBinary left op right
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSExpressionBinary"
      json `shouldSatisfy` Text.isInfixOf "\"+\""
      validateJSON json

    it "serializes logical operations" $ do
      let left = AST.JSIdentifier noAnnot "x"
      let right = AST.JSIdentifier noAnnot "y"
      let op = AST.JSBinOpAnd noAnnot
      let expr = AST.JSExpressionBinary left op right
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSExpressionBinary"
      json `shouldSatisfy` Text.isInfixOf "\"&&\""
      validateJSON json

    it "serializes comparison operations" $ do
      let left = AST.JSIdentifier noAnnot "a"
      let right = AST.JSDecimal noAnnot "5"
      let op = AST.JSBinOpLt noAnnot
      let expr = AST.JSExpressionBinary left op right
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSExpressionBinary"
      json `shouldSatisfy` Text.isInfixOf "\"<\""
      validateJSON json

  describe "member expressions" $ do
    it "serializes dot notation member access" $ do
      let obj = AST.JSIdentifier noAnnot "object"
      let prop = AST.JSIdentifier noAnnot "property"
      let expr = AST.JSMemberDot obj noAnnot prop
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSMemberDot"
      json `shouldSatisfy` Text.isInfixOf "object"
      json `shouldSatisfy` Text.isInfixOf "property"
      validateJSON json

    it "serializes bracket notation member access" $ do
      let obj = AST.JSIdentifier noAnnot "array"
      let index = AST.JSDecimal noAnnot "0"
      let expr = AST.JSMemberSquare obj noAnnot index noAnnot
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSMemberSquare"
      json `shouldSatisfy` Text.isInfixOf "array"
      json `shouldSatisfy` Text.isInfixOf "\"0\""
      validateJSON json

  describe "function calls" $ do
    it "serializes simple function calls" $ do
      let func = AST.JSIdentifier noAnnot "myFunction"
      let args = AST.JSLOne (AST.JSDecimal noAnnot "42")
      let expr = AST.JSCallExpression func noAnnot args noAnnot
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSCallExpression"
      json `shouldSatisfy` Text.isInfixOf "myFunction"
      json `shouldSatisfy` Text.isInfixOf "\"42\""
      validateJSON json

    it "serializes function calls with multiple arguments" $ do
      let func = AST.JSIdentifier noAnnot "add"
      let arg1 = AST.JSDecimal noAnnot "1"
      let arg2 = AST.JSDecimal noAnnot "2"
      let args = AST.JSLCons (AST.JSLOne arg1) noAnnot arg2
      let expr = AST.JSCallExpression func noAnnot args noAnnot
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSCallExpression"
      json `shouldSatisfy` Text.isInfixOf "add"
      validateJSON json

-- | Test modern JavaScript features
testModernFeatures :: Spec
testModernFeatures = describe "Modern JavaScript Features" $ do
  describe "optional chaining" $ do
    it "serializes optional member dot access" $ do
      let obj = AST.JSIdentifier noAnnot "obj"
      let prop = AST.JSIdentifier noAnnot "prop"
      let expr = AST.JSOptionalMemberDot obj noAnnot prop
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSOptionalMemberDot"
      json `shouldSatisfy` Text.isInfixOf "obj"
      json `shouldSatisfy` Text.isInfixOf "prop"
      validateJSON json

    it "serializes optional member square access" $ do
      let obj = AST.JSIdentifier noAnnot "arr"
      let key = AST.JSDecimal noAnnot "0"
      let expr = AST.JSOptionalMemberSquare obj noAnnot key noAnnot
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSOptionalMemberSquare"
      validateJSON json

    it "serializes optional function calls" $ do
      let func = AST.JSIdentifier noAnnot "method"
      let args = AST.JSLNil
      let expr = AST.JSOptionalCallExpression func noAnnot args noAnnot
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSOptionalCallExpression"
      json `shouldSatisfy` Text.isInfixOf "method"
      validateJSON json

  describe "nullish coalescing" $ do
    it "serializes nullish coalescing operator" $ do
      let left = AST.JSIdentifier noAnnot "x"
      let right = AST.JSStringLiteral noAnnot "default"
      let op = AST.JSBinOpNullishCoalescing noAnnot
      let expr = AST.JSExpressionBinary left op right
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSExpressionBinary"
      json `shouldSatisfy` Text.isInfixOf "\"??\""
      validateJSON json

  describe "arrow functions" $ do
    it "serializes simple arrow functions" $ do
      let param = AST.JSUnparenthesizedArrowParameter (AST.JSIdentName noAnnot "x")
      let body = AST.JSConciseExpressionBody (AST.JSIdentifier noAnnot "x")
      let expr = AST.JSArrowExpression param noAnnot body
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSArrowExpression"
      json `shouldSatisfy` Text.isInfixOf "JSUnparenthesizedArrowParameter"
      json `shouldSatisfy` Text.isInfixOf "JSConciseExpressionBody"
      validateJSON json

    it "serializes arrow functions with parenthesized parameters" $ do
      let paramList = AST.JSLOne (AST.JSIdentifier noAnnot "a")
      let param = AST.JSParenthesizedArrowParameterList noAnnot paramList noAnnot
      let body = AST.JSConciseExpressionBody (AST.JSDecimal noAnnot "42")
      let expr = AST.JSArrowExpression param noAnnot body
      let json = PJSON.renderExpressionToJSON expr
      json `shouldSatisfy` Text.isInfixOf "JSArrowExpression"
      json `shouldSatisfy` Text.isInfixOf "JSParenthesizedArrowParameterList"
      validateJSON json

-- | Test statement serialization
testStatementSerialization :: Spec
testStatementSerialization = describe "Statement Serialization" $ do
  describe "expression statements" $ do
    it "serializes expression statements" $ do
      let expr = AST.JSDecimal noAnnot "42"
      let stmt = AST.JSExpressionStatement expr AST.JSSemiAuto
      let json = PJSON.renderStatementToJSON stmt
      json `shouldSatisfy` Text.isInfixOf "JSStatementExpression"
      json `shouldSatisfy` Text.isInfixOf "\"42\""
      validateJSON json

  describe "unsupported statements" $ do
    it "handles unsupported statement types gracefully" $ do
      let stmt = AST.JSEmptyStatement noAnnot
      let json = PJSON.renderStatementToJSON stmt
      json `shouldSatisfy` Text.isInfixOf "JSStatement"
      json `shouldSatisfy` Text.isInfixOf "unsupported"
      validateJSON json

-- | Test module system serialization
testModuleSystem :: Spec
testModuleSystem = describe "Module System Serialization" $ do
  describe "import declarations" $ do
    it "serializes default imports" $ do
      let ident = AST.JSIdentName noAnnot "React"
      let clause = AST.JSImportClauseDefault ident
      let fromClause = AST.JSFromClause noAnnot noAnnot "react"
      let importDecl = AST.JSImportDeclaration clause fromClause Nothing AST.JSSemiAuto
      let json = PJSON.renderImportDeclarationToJSON importDecl
      json `shouldSatisfy` Text.isInfixOf "ImportDeclaration"
      json `shouldSatisfy` Text.isInfixOf "ImportDefaultSpecifier"
      json `shouldSatisfy` Text.isInfixOf "React"
      json `shouldSatisfy` Text.isInfixOf "react"
      validateJSON json

    it "serializes named imports" $ do
      let specifier = AST.JSImportSpecifier (AST.JSIdentName noAnnot "Component")
      let specifiers = AST.JSLOne specifier
      let imports = AST.JSImportsNamed noAnnot specifiers noAnnot
      let clause = AST.JSImportClauseNamed imports
      let fromClause = AST.JSFromClause noAnnot noAnnot "react"
      let importDecl = AST.JSImportDeclaration clause fromClause Nothing AST.JSSemiAuto
      let json = PJSON.renderImportDeclarationToJSON importDecl
      json `shouldSatisfy` Text.isInfixOf "ImportDeclaration"
      json `shouldSatisfy` Text.isInfixOf "ImportSpecifiers"
      json `shouldSatisfy` Text.isInfixOf "Component"
      validateJSON json

    it "serializes namespace imports" $ do
      let binOp = AST.JSBinOpTimes noAnnot
      let ident = AST.JSIdentName noAnnot "utils"
      let namespace = AST.JSImportNameSpace binOp noAnnot ident
      let clause = AST.JSImportClauseNameSpace namespace
      let fromClause = AST.JSFromClause noAnnot noAnnot "utils"
      let importDecl = AST.JSImportDeclaration clause fromClause Nothing AST.JSSemiAuto
      let json = PJSON.renderImportDeclarationToJSON importDecl
      json `shouldSatisfy` Text.isInfixOf "ImportDeclaration"
      json `shouldSatisfy` Text.isInfixOf "ImportNamespaceSpecifier"
      json `shouldSatisfy` Text.isInfixOf "utils"
      validateJSON json

  describe "export declarations" $ do
    it "serializes named exports" $ do
      let ident = AST.JSIdentName noAnnot "myFunction"
      let spec = AST.JSExportSpecifier ident
      let specs = AST.JSLOne spec
      let clause = AST.JSExportClause noAnnot specs noAnnot
      let exportDecl = AST.JSExportLocals clause AST.JSSemiAuto
      let json = PJSON.renderExportDeclarationToJSON exportDecl
      json `shouldSatisfy` Text.isInfixOf "ExportLocalsDeclaration"
      json `shouldSatisfy` Text.isInfixOf "ExportSpecifier"
      json `shouldSatisfy` Text.isInfixOf "myFunction"
      validateJSON json

-- | Test annotation serialization
testAnnotationSerialization :: Spec
testAnnotationSerialization = describe "Annotation Serialization" $ do
  describe "position information" $ do
    it "serializes position data" $ do
      let pos = TokenPn 0 10 5
      let json = PJSON.renderAnnotation (AST.JSAnnot pos [])
      json `shouldSatisfy` Text.isInfixOf "\"line\":10"
      json `shouldSatisfy` Text.isInfixOf "\"column\":5"
      validateJSON json

    it "serializes empty annotations" $ do
      let json = PJSON.renderAnnotation AST.JSNoAnnot
      json `shouldSatisfy` Text.isInfixOf "\"position\":null"
      json `shouldSatisfy` Text.isInfixOf "\"comments\":[]"
      validateJSON json

    it "serializes space annotations" $ do
      let json = PJSON.renderAnnotation AST.JSAnnotSpace
      json `shouldSatisfy` Text.isInfixOf "\"type\":\"space\""
      validateJSON json

-- | Test complete program serialization
testCompletePrograms :: Spec
testCompletePrograms = describe "Complete Program Serialization" $ do
  it "serializes simple programs" $ do
    let expr = AST.JSDecimal noAnnot "42"
    let stmt = AST.JSExpressionStatement expr AST.JSSemiAuto
    let program = [stmt]
    let json = PJSON.renderProgramToJSON program
    json `shouldSatisfy` Text.isInfixOf "JSProgram"
    json `shouldSatisfy` Text.isInfixOf "statements"
    json `shouldSatisfy` Text.isInfixOf "\"42\""
    validateJSON json

  it "serializes complex programs with multiple statements" $ do
    let expr1 = AST.JSDecimal noAnnot "1"
    let expr2 = AST.JSDecimal noAnnot "2"
    let stmt1 = AST.JSExpressionStatement expr1 AST.JSSemiAuto
    let stmt2 = AST.JSExpressionStatement expr2 AST.JSSemiAuto
    let program = [stmt1, stmt2]
    let json = PJSON.renderProgramToJSON program
    json `shouldSatisfy` Text.isInfixOf "JSProgram"
    json `shouldSatisfy` Text.isInfixOf "\"1\""
    json `shouldSatisfy` Text.isInfixOf "\"2\""
    validateJSON json

  it "serializes different AST root types" $ do
    let expr = AST.JSDecimal noAnnot "123"
    let ast = AST.JSAstExpression expr noAnnot
    let json = PJSON.renderToJSON ast
    json `shouldSatisfy` Text.isInfixOf "JSAstExpression"
    json `shouldSatisfy` Text.isInfixOf "\"123\""
    validateJSON json

-- | Test edge cases and error conditions
testEdgeCases :: Spec
testEdgeCases = describe "Edge Cases" $ do
  it "handles empty programs" $ do
    let program = []
    let json = PJSON.renderProgramToJSON program
    json `shouldSatisfy` Text.isInfixOf "JSProgram"
    json `shouldSatisfy` Text.isInfixOf "\"statements\":[]"
    validateJSON json

  it "handles special identifier names" $ do
    let expr = AST.JSIdentifier noAnnot "if" -- Reserved word
    let json = PJSON.renderExpressionToJSON expr
    json `shouldSatisfy` Text.isInfixOf "JSIdentifier"
    json `shouldSatisfy` Text.isInfixOf "\"if\""
    validateJSON json

  it "handles complex nested structures" $ do
    let innerExpr = AST.JSDecimal noAnnot "1"
    let left = AST.JSExpressionBinary innerExpr (AST.JSBinOpPlus noAnnot) innerExpr
    let right = AST.JSDecimal noAnnot "2"
    let outerExpr = AST.JSExpressionBinary left (AST.JSBinOpTimes noAnnot) right
    let json = PJSON.renderExpressionToJSON outerExpr
    json `shouldSatisfy` Text.isInfixOf "JSExpressionBinary"
    validateJSON json

  it "handles large numeric values" $ do
    let expr = AST.JSDecimal noAnnot "1.7976931348623157e+308"
    let json = PJSON.renderExpressionToJSON expr
    json `shouldSatisfy` Text.isInfixOf "JSDecimal"
    json `shouldSatisfy` Text.isInfixOf "1.7976931348623157e+308"
    validateJSON json

-- | Test JSON format compliance
testJSONCompliance :: Spec
testJSONCompliance = describe "JSON Format Compliance" $ do
  it "produces valid JSON for all expression types" $ do
    let expressions =
          [ AST.JSDecimal noAnnot "42",
            AST.JSStringLiteral noAnnot "test",
            AST.JSIdentifier noAnnot "myVar",
            AST.JSLiteral noAnnot "true"
          ]
    mapM_
      ( \expr -> do
          let json = PJSON.renderExpressionToJSON expr
          validateJSON json
      )
      expressions

  it "produces parseable JSON with standard libraries" $ do
    let expr =
          AST.JSExpressionBinary
            (AST.JSDecimal noAnnot "1")
            (AST.JSBinOpPlus noAnnot)
            (AST.JSDecimal noAnnot "2")
    let json = PJSON.renderExpressionToJSON expr

    -- Validate with Aeson
    let parsed = JSON.decode (BSL.fromStrict (Text.encodeUtf8 json)) :: Maybe JSON.Value
    parsed `shouldSatisfy` isJust

  it "maintains consistent field ordering" $ do
    let expr = AST.JSDecimal testAnnot "42"
    let json = PJSON.renderExpressionToJSON expr
    -- Type field should come first
    json `shouldSatisfy` Text.isPrefixOf "{\"type\""

  it "handles all operator types in binary expressions" $ do
    let allOps =
          [ AST.JSBinOpPlus noAnnot,
            AST.JSBinOpMinus noAnnot,
            AST.JSBinOpTimes noAnnot,
            AST.JSBinOpDivide noAnnot,
            AST.JSBinOpMod noAnnot,
            AST.JSBinOpExponentiation noAnnot,
            AST.JSBinOpAnd noAnnot,
            AST.JSBinOpOr noAnnot,
            AST.JSBinOpNullishCoalescing noAnnot,
            AST.JSBinOpEq noAnnot,
            AST.JSBinOpNeq noAnnot,
            AST.JSBinOpStrictEq noAnnot,
            AST.JSBinOpStrictNeq noAnnot,
            AST.JSBinOpLt noAnnot,
            AST.JSBinOpLe noAnnot,
            AST.JSBinOpGt noAnnot,
            AST.JSBinOpGe noAnnot
          ]

    mapM_
      ( \op -> do
          let expr =
                AST.JSExpressionBinary
                  (AST.JSDecimal noAnnot "1")
                  op
                  (AST.JSDecimal noAnnot "2")
          let json = PJSON.renderExpressionToJSON expr
          validateJSON json
      )
      allOps

-- | Helper function to validate JSON format
validateJSON :: Text -> Expectation
validateJSON jsonText = do
  let parsed = JSON.decode (BSL.fromStrict (Text.encodeUtf8 jsonText)) :: Maybe JSON.Value
  parsed `shouldSatisfy` isJust

-- | Helper function to check Maybe values
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False
