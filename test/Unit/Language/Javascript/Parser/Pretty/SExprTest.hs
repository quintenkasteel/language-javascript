{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive S-expression serialization testing for JavaScript AST.
--
-- This module provides thorough testing of the Pretty.SExpr module, ensuring:
--
--   * Accurate S-expression serialization of all AST node types
--   * Proper handling of modern JavaScript features (ES6+)
--   * Lisp-compatible S-expression syntax
--   * Preservation of source location and comment information
--   * Correct handling of special characters and escaping
--   * Hierarchical representation of nested structures
--
-- The tests cover all major AST constructs with focus on:
-- correctness, completeness, and S-expression format compliance.
--
-- @since 0.7.1.0
module Unit.Language.Javascript.Parser.Pretty.SExprTest
  ( testSExprSerialization,
  )
where

import qualified Data.Text as Text
import qualified Language.JavaScript.Parser.AST as AST
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import qualified Language.JavaScript.Pretty.SExpr as PSExpr
import Test.Hspec

-- | Test helpers for S-expression validation
noPos :: TokenPosn
noPos = TokenPn 0 0 0

testAnnot :: AST.JSAnnot
testAnnot = AST.JSAnnot noPos []

-- | Main S-expression serialization test suite
testSExprSerialization :: Spec
testSExprSerialization = describe "S-Expression Serialization Tests" $ do
  testSExprUtilities
  testLiteralSerialization
  testExpressionSerialization
  testModernJavaScriptFeatures
  testStatementSerialization
  testModuleSystemSerialization
  testAnnotationSerialization
  testEdgeCases
  testCompletePrograms
  testSExprFormatCompliance

-- | Test S-expression utility functions
testSExprUtilities :: Spec
testSExprUtilities = describe "S-Expression Utilities" $ do
  describe "escapeSExprString" $ do
    it "escapes double quotes" $ do
      PSExpr.escapeSExprString "hello \"world\"" `shouldBe` "\"hello \\\"world\\\"\""

    it "escapes backslashes" $ do
      PSExpr.escapeSExprString "path\\to\\file" `shouldBe` "\"path\\\\to\\\\file\""

    it "escapes newlines" $ do
      PSExpr.escapeSExprString "line1\nline2" `shouldBe` "\"line1\\nline2\""

    it "escapes carriage returns" $ do
      PSExpr.escapeSExprString "line1\rline2" `shouldBe` "\"line1\\rline2\""

    it "escapes tabs" $ do
      PSExpr.escapeSExprString "col1\tcol2" `shouldBe` "\"col1\\tcol2\""

    it "handles empty string" $ do
      PSExpr.escapeSExprString "" `shouldBe` "\"\""

    it "handles normal characters" $ do
      PSExpr.escapeSExprString "hello world" `shouldBe` "\"hello world\""

  describe "formatSExprList" $ do
    it "formats empty list" $ do
      PSExpr.formatSExprList [] `shouldBe` "()"

    it "formats single element list" $ do
      PSExpr.formatSExprList ["atom"] `shouldBe` "(atom)"

    it "formats multiple element list" $ do
      PSExpr.formatSExprList ["func", "arg1", "arg2"] `shouldBe` "(func arg1 arg2)"

    it "formats nested lists" $ do
      PSExpr.formatSExprList ["outer", "(inner element)"] `shouldBe` "(outer (inner element))"

  describe "formatSExprAtom" $ do
    it "formats atoms correctly" $ do
      PSExpr.formatSExprAtom "identifier" `shouldBe` "identifier"
      PSExpr.formatSExprAtom "123" `shouldBe` "123"

-- | Test literal value serialization
testLiteralSerialization :: Spec
testLiteralSerialization = describe "Literal Serialization" $ do
  describe "numeric literals" $ do
    it "serializes decimal numbers" $ do
      let expr = AST.JSDecimal testAnnot 42
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSDecimal"
      sexpr `shouldSatisfy` Text.isInfixOf "\"42\""

    it "serializes hexadecimal numbers" $ do
      let expr = AST.JSHexInteger testAnnot 0xFF
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSHexInteger"
      sexpr `shouldSatisfy` Text.isInfixOf "\"0xff\""

    it "serializes octal numbers" $ do
      let expr = AST.JSOctal testAnnot 0o777
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSOctal"
      sexpr `shouldSatisfy` Text.isInfixOf "\"0o777\""

    it "serializes binary numbers" $ do
      let expr = AST.JSBinaryInteger testAnnot 10
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSBinaryInteger"
      sexpr `shouldSatisfy` Text.isInfixOf "\"0b1010\""

    it "serializes BigInt literals" $ do
      let expr = AST.JSBigIntLiteral testAnnot 123
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSBigIntLiteral"
      sexpr `shouldSatisfy` Text.isInfixOf "\"123n\""

  describe "string literals" $ do
    it "serializes simple strings" $ do
      let expr = AST.JSStringLiteral testAnnot "\"hello\""
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSStringLiteral"
      sexpr `shouldSatisfy` Text.isInfixOf "\\\"hello\\\""

    it "serializes strings with escapes" $ do
      let expr = AST.JSStringLiteral testAnnot "\"hello\\nworld\""
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isInfixOf "hello\\\\nworld"

  describe "identifiers" $ do
    it "serializes simple identifiers" $ do
      let expr = AST.JSIdentifier testAnnot "variable"
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSIdentifier"
      sexpr `shouldSatisfy` Text.isInfixOf "\"variable\""

    it "serializes identifiers with special characters" $ do
      let expr = AST.JSIdentifier testAnnot "$special_var123"
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isInfixOf "$special_var123"

  describe "special literals" $ do
    it "serializes generic literals" $ do
      let expr = AST.JSLiteral testAnnot "true"
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSLiteral"
      sexpr `shouldSatisfy` Text.isInfixOf "\"true\""

    it "serializes regex literals" $ do
      let expr = AST.JSRegEx testAnnot "/pattern/gi"
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSRegEx"
      sexpr `shouldSatisfy` Text.isInfixOf "/pattern/gi"

-- | Test expression serialization
testExpressionSerialization :: Spec
testExpressionSerialization = describe "Expression Serialization" $ do
  describe "binary expressions" $ do
    it "serializes arithmetic operations" $ do
      let left = AST.JSDecimal testAnnot 1
      let right = AST.JSDecimal testAnnot 2
      let op = AST.JSBinOpPlus testAnnot
      let expr = AST.JSExpressionBinary left op right
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSExpressionBinary"
      sexpr `shouldSatisfy` Text.isInfixOf "(JSBinOpPlus"
      sexpr `shouldSatisfy` Text.isInfixOf "(JSDecimal \"1\""
      sexpr `shouldSatisfy` Text.isInfixOf "(JSDecimal \"2\""

    it "serializes logical operations" $ do
      let left = AST.JSIdentifier testAnnot "a"
      let right = AST.JSIdentifier testAnnot "b"
      let op = AST.JSBinOpAnd testAnnot
      let expr = AST.JSExpressionBinary left op right
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isInfixOf "(JSBinOpAnd"

    it "serializes comparison operations" $ do
      let left = AST.JSIdentifier testAnnot "x"
      let right = AST.JSDecimal testAnnot 5
      let op = AST.JSBinOpLt testAnnot
      let expr = AST.JSExpressionBinary left op right
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isInfixOf "(JSBinOpLt"

  describe "member expressions" $ do
    it "serializes dot notation member access" $ do
      let obj = AST.JSIdentifier testAnnot "obj"
      let prop = AST.JSIdentifier testAnnot "property"
      let expr = AST.JSMemberDot obj testAnnot prop
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSMemberDot"
      sexpr `shouldSatisfy` Text.isInfixOf "\"obj\""
      sexpr `shouldSatisfy` Text.isInfixOf "\"property\""

    it "serializes bracket notation member access" $ do
      let obj = AST.JSIdentifier testAnnot "obj"
      let prop = AST.JSStringLiteral testAnnot "\"key\""
      let expr = AST.JSMemberSquare obj testAnnot prop testAnnot
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSMemberSquare"

  describe "function calls" $ do
    it "serializes simple function calls" $ do
      let func = AST.JSIdentifier testAnnot "func"
      let expr = AST.JSCallExpression func testAnnot AST.JSLNil testAnnot
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSCallExpression"
      sexpr `shouldSatisfy` Text.isInfixOf "\"func\""
      sexpr `shouldSatisfy` Text.isInfixOf "(comma-list)"

    it "serializes function calls with arguments" $ do
      let func = AST.JSIdentifier testAnnot "func"
      let arg = AST.JSDecimal testAnnot 42
      let args = AST.JSLOne arg
      let expr = AST.JSCallExpression func testAnnot args testAnnot
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSCallExpression"
      sexpr `shouldSatisfy` Text.isInfixOf "(comma-list"
      sexpr `shouldSatisfy` Text.isInfixOf "\"42\""

-- | Test modern JavaScript features
testModernJavaScriptFeatures :: Spec
testModernJavaScriptFeatures = describe "Modern JavaScript Features" $ do
  describe "optional chaining" $ do
    it "serializes optional member dot access" $ do
      let obj = AST.JSIdentifier testAnnot "obj"
      let prop = AST.JSIdentifier testAnnot "prop"
      let expr = AST.JSOptionalMemberDot obj testAnnot prop
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSOptionalMemberDot"

    it "serializes optional member square access" $ do
      let obj = AST.JSIdentifier testAnnot "obj"
      let prop = AST.JSStringLiteral testAnnot "\"key\""
      let expr = AST.JSOptionalMemberSquare obj testAnnot prop testAnnot
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSOptionalMemberSquare"

    it "serializes optional function calls" $ do
      let func = AST.JSIdentifier testAnnot "func"
      let expr = AST.JSOptionalCallExpression func testAnnot AST.JSLNil testAnnot
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSOptionalCallExpression"

  describe "nullish coalescing" $ do
    it "serializes nullish coalescing operator" $ do
      let left = AST.JSIdentifier testAnnot "value"
      let right = AST.JSStringLiteral testAnnot "\"default\""
      let op = AST.JSBinOpNullishCoalescing testAnnot
      let expr = AST.JSExpressionBinary left op right
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isInfixOf "(JSBinOpNullishCoalescing"

  describe "arrow functions" $ do
    it "serializes simple arrow functions" $ do
      let param = AST.JSUnparenthesizedArrowParameter (AST.JSIdentName testAnnot "x")
      let body = AST.JSConciseExpressionBody (AST.JSDecimal testAnnot 42)
      let expr = AST.JSArrowExpression param testAnnot body
      let sexpr = PSExpr.renderExpressionToSExpr expr
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSArrowExpression"
      sexpr `shouldSatisfy` Text.isInfixOf "(JSUnparenthesizedArrowParameter"
      sexpr `shouldSatisfy` Text.isInfixOf "(JSConciseExpressionBody"

-- | Test statement serialization
testStatementSerialization :: Spec
testStatementSerialization = describe "Statement Serialization" $ do
  describe "expression statements" $ do
    it "serializes expression statements" $ do
      let expr = AST.JSDecimal testAnnot 42
      let stmt = AST.JSExpressionStatement expr AST.JSSemiAuto
      let sexpr = PSExpr.renderStatementToSExpr stmt
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSExpressionStatement"
      sexpr `shouldSatisfy` Text.isInfixOf "(JSDecimal"
      sexpr `shouldSatisfy` Text.isInfixOf "(JSSemiAuto)"

  describe "variable declarations" $ do
    it "serializes var declarations" $ do
      let ident = AST.JSIdentifier testAnnot "x"
      let initializer = AST.JSVarInit testAnnot (AST.JSDecimal testAnnot 42)
      let varInit = AST.JSVarInitExpression ident initializer
      let stmt = AST.JSVariable testAnnot (AST.JSLOne varInit) AST.JSSemiAuto
      let sexpr = PSExpr.renderStatementToSExpr stmt
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSVariable"

    it "serializes let declarations" $ do
      let ident = AST.JSIdentifier testAnnot "x"
      let varInit = AST.JSVarInitExpression ident AST.JSVarInitNone
      let stmt = AST.JSLet testAnnot (AST.JSLOne varInit) AST.JSSemiAuto
      let sexpr = PSExpr.renderStatementToSExpr stmt
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSLet"

    it "serializes const declarations" $ do
      let ident = AST.JSIdentifier testAnnot "x"
      let initializer = AST.JSVarInit testAnnot (AST.JSDecimal testAnnot 42)
      let varInit = AST.JSVarInitExpression ident initializer
      let stmt = AST.JSConstant testAnnot (AST.JSLOne varInit) AST.JSSemiAuto
      let sexpr = PSExpr.renderStatementToSExpr stmt
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSConstant"

  describe "control flow statements" $ do
    it "serializes empty statements" $ do
      let stmt = AST.JSEmptyStatement testAnnot
      let sexpr = PSExpr.renderStatementToSExpr stmt
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSEmptyStatement"

    it "serializes return statements" $ do
      let expr = AST.JSDecimal testAnnot 42
      let stmt = AST.JSReturn testAnnot (Just expr) AST.JSSemiAuto
      let sexpr = PSExpr.renderStatementToSExpr stmt
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSReturn"
      sexpr `shouldSatisfy` Text.isInfixOf "(maybe-expression"

-- | Test module system serialization
testModuleSystemSerialization :: Spec
testModuleSystemSerialization = describe "Module System Serialization" $ do
  describe "import declarations" $ do
    it "serializes bare import declarations" $ do
      let decl = AST.JSImportDeclarationBare testAnnot "'./module'" Nothing AST.JSSemiAuto
      let sexpr = PSExpr.renderImportDeclarationToSExpr decl
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSImportDeclarationBare"
      sexpr `shouldSatisfy` Text.isInfixOf "'./module'"

    it "serializes default import declarations" $ do
      let clause = AST.JSImportClauseDefault (AST.JSIdentName testAnnot "foo")
      let fromClause = AST.JSFromClause testAnnot testAnnot "'./foo'"
      let decl = AST.JSImportDeclaration clause fromClause Nothing AST.JSSemiAuto
      let sexpr = PSExpr.renderImportDeclarationToSExpr decl
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSImportDeclaration"
      sexpr `shouldSatisfy` Text.isInfixOf "(JSImportClauseDefault"
      sexpr `shouldSatisfy` Text.isInfixOf "(JSFromClause"

    it "serializes named import declarations" $ do
      let spec1 = AST.JSImportSpecifier (AST.JSIdentName testAnnot "bar")
      let imports = AST.JSImportsNamed testAnnot (AST.JSLOne spec1) testAnnot
      let clause = AST.JSImportClauseNamed imports
      let fromClause = AST.JSFromClause testAnnot testAnnot "'./bar'"
      let decl = AST.JSImportDeclaration clause fromClause Nothing AST.JSSemiAuto
      let sexpr = PSExpr.renderImportDeclarationToSExpr decl
      sexpr `shouldSatisfy` Text.isInfixOf "(JSImportClauseNamed"
      sexpr `shouldSatisfy` Text.isInfixOf "(JSImportsNamed"
      sexpr `shouldSatisfy` Text.isInfixOf "(JSImportSpecifier"

  describe "export declarations" $ do
    it "serializes export statement declarations" $ do
      let stmt = AST.JSExpressionStatement (AST.JSDecimal testAnnot 42) AST.JSSemiAuto
      let decl = AST.JSExport stmt AST.JSSemiAuto
      let sexpr = PSExpr.renderExportDeclarationToSExpr decl
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSExport"

    it "serializes export default declarations" $ do
      let stmt = AST.JSExpressionStatement (AST.JSDecimal testAnnot 42) AST.JSSemiAuto
      let decl = AST.JSExportDefault testAnnot stmt AST.JSSemiAuto
      let sexpr = PSExpr.renderExportDeclarationToSExpr decl
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSExportDefault"

    it "serializes export locals declarations" $ do
      let spec1 = AST.JSExportSpecifier (AST.JSIdentName testAnnot "foo")
      let clause = AST.JSExportClause testAnnot (AST.JSLOne spec1) testAnnot
      let decl = AST.JSExportLocals clause AST.JSSemiAuto
      let sexpr = PSExpr.renderExportDeclarationToSExpr decl
      sexpr `shouldSatisfy` Text.isPrefixOf "(JSExportLocals"
      sexpr `shouldSatisfy` Text.isInfixOf "(JSExportClause"
      sexpr `shouldSatisfy` Text.isInfixOf "(JSExportSpecifier"

-- | Test annotation serialization
testAnnotationSerialization :: Spec
testAnnotationSerialization = describe "Annotation Serialization" $ do
  describe "position information" $ do
    it "serializes position data" $ do
      let pos = TokenPn 100 5 10
      let annot = AST.JSAnnot pos []
      let sexpr = PSExpr.renderAnnotation annot
      sexpr `shouldSatisfy` Text.isInfixOf "(position 100 5 10)"

    it "serializes empty annotations" $ do
      let sexpr = PSExpr.renderAnnotation AST.JSNoAnnot
      sexpr `shouldSatisfy` Text.isPrefixOf "(annotation"
      sexpr `shouldSatisfy` Text.isInfixOf "(position)"
      sexpr `shouldSatisfy` Text.isInfixOf "(comments)"

    it "serializes annotation space" $ do
      let sexpr = PSExpr.renderAnnotation AST.JSAnnotSpace
      sexpr `shouldSatisfy` Text.isPrefixOf "(annotation-space)"

-- | Test edge cases and special scenarios
testEdgeCases :: Spec
testEdgeCases = describe "Edge Cases" $ do
  it "handles empty programs" $ do
    let prog = AST.JSAstProgram [] testAnnot
    let sexpr = PSExpr.renderToSExpr prog
    sexpr `shouldSatisfy` Text.isPrefixOf "(JSAstProgram"
    sexpr `shouldSatisfy` Text.isInfixOf "(statements)"

  it "handles special identifier names" $ do
    let expr = AST.JSIdentifier testAnnot "$special_var123"
    let sexpr = PSExpr.renderExpressionToSExpr expr
    sexpr `shouldSatisfy` Text.isInfixOf "$special_var123"

  it "handles complex nested structures" $ do
    -- Test nested member access: obj.prop1.prop2
    let obj = AST.JSIdentifier testAnnot "obj"
    let prop1 = AST.JSIdentifier testAnnot "prop1"
    let intermediate = AST.JSMemberDot obj testAnnot prop1
    let prop2 = AST.JSIdentifier testAnnot "prop2"
    let expr = AST.JSMemberDot intermediate testAnnot prop2
    let sexpr = PSExpr.renderExpressionToSExpr expr
    sexpr `shouldSatisfy` Text.isPrefixOf "(JSMemberDot"
    -- Should have nested JSMemberDot structures
    let memberDotCount = Text.count "(JSMemberDot" sexpr
    memberDotCount `shouldBe` 2

  it "handles large numeric values" $ do
    let expr = AST.JSDecimal testAnnot 9007199254740991
    let sexpr = PSExpr.renderExpressionToSExpr expr
    sexpr `shouldSatisfy` Text.isInfixOf "9.007199254740991e15"

-- | Test complete program serialization
testCompletePrograms :: Spec
testCompletePrograms = describe "Complete Program Serialization" $ do
  it "serializes simple programs" $ do
    let expr = AST.JSDecimal testAnnot 42
    let stmt = AST.JSExpressionStatement expr AST.JSSemiAuto
    let prog = AST.JSAstProgram [stmt] testAnnot
    let sexpr = PSExpr.renderToSExpr prog
    sexpr `shouldSatisfy` Text.isPrefixOf "(JSAstProgram"
    sexpr `shouldSatisfy` Text.isInfixOf "(JSExpressionStatement"

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
    let sexpr = PSExpr.renderToSExpr prog
    sexpr `shouldSatisfy` Text.isInfixOf "(JSVariable"
    sexpr `shouldSatisfy` Text.isInfixOf "(JSExpressionStatement"

  it "serializes different AST root types" $ do
    let expr = AST.JSDecimal testAnnot 42
    let exprAST = AST.JSAstExpression expr testAnnot
    let sexpr = PSExpr.renderToSExpr exprAST
    sexpr `shouldSatisfy` Text.isPrefixOf "(JSAstExpression"

-- | Test S-expression format compliance
testSExprFormatCompliance :: Spec
testSExprFormatCompliance = describe "S-Expression Format Compliance" $ do
  it "produces valid S-expressions for all expression types" $ do
    -- Test a variety of expressions to ensure valid S-expression structure
    let expressions =
          [ AST.JSDecimal testAnnot 42,
            AST.JSStringLiteral testAnnot "\"test\"",
            AST.JSIdentifier testAnnot "variable",
            AST.JSLiteral testAnnot "true"
          ]
    mapM_
      ( \expr -> do
          let sexpr = PSExpr.renderExpressionToSExpr expr
          sexpr `shouldSatisfy` Text.isPrefixOf "("
          sexpr `shouldSatisfy` Text.isSuffixOf ")"
      )
      expressions

  it "maintains proper list structure" $ do
    let expr = AST.JSDecimal testAnnot 42
    let sexpr = PSExpr.renderExpressionToSExpr expr
    -- Should have matching parentheses
    let openCount = Text.count "(" sexpr
    let closeCount = Text.count ")" sexpr
    openCount `shouldBe` closeCount

  it "properly nests child expressions" $ do
    let left = AST.JSDecimal testAnnot 1
    let right = AST.JSDecimal testAnnot 2
    let op = AST.JSBinOpPlus testAnnot
    let expr = AST.JSExpressionBinary left op right
    let sexpr = PSExpr.renderExpressionToSExpr expr
    -- Should have proper nesting structure
    sexpr `shouldSatisfy` Text.isPrefixOf "(JSExpressionBinary"
    sexpr `shouldSatisfy` Text.isInfixOf "(JSDecimal \"1\""
    sexpr `shouldSatisfy` Text.isInfixOf "(JSDecimal \"2\""
    sexpr `shouldSatisfy` Text.isInfixOf "(JSBinOpPlus"

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
          let sexpr = PSExpr.renderExpressionToSExpr expr
          sexpr `shouldSatisfy` Text.isPrefixOf "(JSExpressionBinary"
      )
      operators

  it "properly escapes strings in S-expressions" $ do
    let testStrings =
          [ "simple",
            "with\"quotes",
            "with\\backslashes",
            "with\nnewlines"
          ]
    mapM_
      ( \str -> do
          let escaped = PSExpr.escapeSExprString str
          escaped `shouldSatisfy` Text.isPrefixOf "\""
          escaped `shouldSatisfy` Text.isSuffixOf "\""
      )
      testStrings
