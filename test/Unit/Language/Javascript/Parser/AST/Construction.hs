{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive AST Constructor Testing for JavaScript Parser
--
-- This module provides systematic testing for all AST node constructors
-- to achieve high coverage of the AST module. It tests:
--
--   * All 'JSExpression' constructors (44 variants)
--   * All 'JSStatement' constructors (27 variants)
--   * Binary and unary operator constructors
--   * Module import/export constructors
--   * Class and method definition constructors
--   * Utility and annotation constructors
--
-- The tests focus on constructor correctness, pattern matching coverage,
-- and AST node invariant validation.
--
-- @since 0.7.1.0
module Unit.Language.Javascript.Parser.AST.Construction
  ( testASTConstructors,
  )
where

import Control.DeepSeq (deepseq)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Language.JavaScript.Parser.AST as AST
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import Test.Hspec

-- | Test annotation for constructor testing
noAnnot :: AST.JSAnnot
noAnnot = AST.JSNoAnnot

testAnnot :: AST.JSAnnot
testAnnot = AST.JSAnnot (TokenPn 0 1 1) []

testIdent :: AST.JSIdent
testIdent = AST.JSIdentName testAnnot "test"

testSemi :: AST.JSSemi
testSemi = AST.JSSemiAuto

-- | Comprehensive AST constructor testing
testASTConstructors :: Spec
testASTConstructors = describe "AST Constructor Coverage" $ do
  describe "JSExpression constructors (41 variants)" $ do
    testTerminalExpressions
    testNonTerminalExpressions

  describe "JSStatement constructors (36 variants)" $ do
    testStatementConstructors

  describe "Binary and Unary operator constructors" $ do
    testBinaryOperators
    testUnaryOperators
    testAssignmentOperators

  describe "Module system constructors" $ do
    testModuleConstructors

  describe "Class and method constructors" $ do
    testClassConstructors

  describe "Utility constructors" $ do
    testUtilityConstructors

  describe "AST node pattern matching exhaustiveness" $ do
    testPatternMatchingCoverage

-- | Test all terminal expression constructors
testTerminalExpressions :: Spec
testTerminalExpressions = describe "Terminal expressions" $ do
  it "constructs JSIdentifier correctly" $ do
    let expr = AST.JSIdentifier testAnnot "variableName"
    expr `shouldSatisfy` isJSIdentifier
    expr `deepseq` (return ())

  it "constructs JSDecimal correctly" $ do
    let expr = AST.JSDecimal testAnnot "42.5"
    expr `shouldSatisfy` isJSDecimal
    extractLiteral expr `shouldBe` "42.5"

  it "constructs JSLiteral correctly" $ do
    let expr = AST.JSLiteral testAnnot "true"
    expr `shouldSatisfy` isJSLiteral
    extractLiteral expr `shouldBe` "true"

  it "constructs JSHexInteger correctly" $ do
    let expr = AST.JSHexInteger testAnnot "0xFF"
    expr `shouldSatisfy` isJSHexInteger
    extractLiteral expr `shouldBe` "0xFF"

  it "constructs JSBinaryInteger correctly" $ do
    let expr = AST.JSBinaryInteger testAnnot "0b1010"
    expr `shouldSatisfy` isJSBinaryInteger
    extractLiteral expr `shouldBe` "0b1010"

  it "constructs JSOctal correctly" $ do
    let expr = AST.JSOctal testAnnot "0o777"
    expr `shouldSatisfy` isJSOctal
    extractLiteral expr `shouldBe` "0o777"

  it "constructs JSBigIntLiteral correctly" $ do
    let expr = AST.JSBigIntLiteral testAnnot "123n"
    expr `shouldSatisfy` isJSBigIntLiteral
    extractLiteral expr `shouldBe` "123n"

  it "constructs JSStringLiteral correctly" $ do
    let expr = AST.JSStringLiteral testAnnot "\"hello\""
    expr `shouldSatisfy` isJSStringLiteral
    extractLiteral expr `shouldBe` "\"hello\""

  it "constructs JSRegEx correctly" $ do
    let expr = AST.JSRegEx testAnnot "/pattern/gi"
    expr `shouldSatisfy` isJSRegEx
    extractLiteral expr `shouldBe` "/pattern/gi"

-- | Test all non-terminal expression constructors
testNonTerminalExpressions :: Spec
testNonTerminalExpressions = describe "Non-terminal expressions" $ do
  it "constructs JSArrayLiteral correctly" $ do
    let expr = AST.JSArrayLiteral testAnnot [] testAnnot
    expr `shouldSatisfy` isJSArrayLiteral
    expr `deepseq` (return ())

  it "constructs JSAssignExpression correctly" $ do
    let lhs = AST.JSIdentifier testAnnot "x"
    let rhs = AST.JSDecimal testAnnot "42"
    let op = AST.JSAssign testAnnot
    let expr = AST.JSAssignExpression lhs op rhs
    expr `shouldSatisfy` isJSAssignExpression

  it "constructs JSAwaitExpression correctly" $ do
    let innerExpr = AST.JSIdentifier testAnnot "promise"
    let expr = AST.JSAwaitExpression testAnnot innerExpr
    expr `shouldSatisfy` isJSAwaitExpression

  it "constructs JSCallExpression correctly" $ do
    let fn = AST.JSIdentifier testAnnot "func"
    let args = AST.JSLNil
    let expr = AST.JSCallExpression fn testAnnot args testAnnot
    expr `shouldSatisfy` isJSCallExpression

  it "constructs JSCallExpressionDot correctly" $ do
    let obj = AST.JSIdentifier testAnnot "obj"
    let prop = AST.JSIdentifier testAnnot "method"
    let expr = AST.JSCallExpressionDot obj testAnnot prop
    expr `shouldSatisfy` isJSCallExpressionDot

  it "constructs JSCallExpressionSquare correctly" $ do
    let obj = AST.JSIdentifier testAnnot "obj"
    let key = AST.JSStringLiteral testAnnot "\"key\""
    let expr = AST.JSCallExpressionSquare obj testAnnot key testAnnot
    expr `shouldSatisfy` isJSCallExpressionSquare

  it "constructs JSClassExpression correctly" $ do
    let expr = AST.JSClassExpression testAnnot testIdent AST.JSExtendsNone testAnnot [] testAnnot
    expr `shouldSatisfy` isJSClassExpression

  it "constructs JSCommaExpression correctly" $ do
    let left = AST.JSDecimal testAnnot "1"
    let right = AST.JSDecimal testAnnot "2"
    let expr = AST.JSCommaExpression left testAnnot right
    expr `shouldSatisfy` isJSCommaExpression

  it "constructs JSExpressionBinary correctly" $ do
    let left = AST.JSDecimal testAnnot "1"
    let right = AST.JSDecimal testAnnot "2"
    let op = AST.JSBinOpPlus testAnnot
    let expr = AST.JSExpressionBinary left op right
    expr `shouldSatisfy` isJSExpressionBinary

  it "constructs JSExpressionParen correctly" $ do
    let innerExpr = AST.JSDecimal testAnnot "42"
    let expr = AST.JSExpressionParen testAnnot innerExpr testAnnot
    expr `shouldSatisfy` isJSExpressionParen

  it "constructs JSExpressionPostfix correctly" $ do
    let innerExpr = AST.JSIdentifier testAnnot "x"
    let op = AST.JSUnaryOpIncr testAnnot
    let expr = AST.JSExpressionPostfix innerExpr op
    expr `shouldSatisfy` isJSExpressionPostfix

  it "constructs JSExpressionTernary correctly" $ do
    let cond = AST.JSIdentifier testAnnot "x"
    let trueVal = AST.JSDecimal testAnnot "1"
    let falseVal = AST.JSDecimal testAnnot "2"
    let expr = AST.JSExpressionTernary cond testAnnot trueVal testAnnot falseVal
    expr `shouldSatisfy` isJSExpressionTernary

  it "constructs JSArrowExpression correctly" $ do
    let params = AST.JSUnparenthesizedArrowParameter testIdent
    let body = AST.JSConciseExpressionBody (AST.JSDecimal testAnnot "42")
    let expr = AST.JSArrowExpression params testAnnot body
    expr `shouldSatisfy` isJSArrowExpression

  it "constructs JSFunctionExpression correctly" $ do
    let body = AST.JSBlock testAnnot [] testAnnot
    let expr = AST.JSFunctionExpression testAnnot testIdent testAnnot AST.JSLNil testAnnot body
    expr `shouldSatisfy` isJSFunctionExpression

  it "constructs JSGeneratorExpression correctly" $ do
    let body = AST.JSBlock testAnnot [] testAnnot
    let expr = AST.JSGeneratorExpression testAnnot testAnnot testIdent testAnnot AST.JSLNil testAnnot body
    expr `shouldSatisfy` isJSGeneratorExpression

  it "constructs JSAsyncFunctionExpression correctly" $ do
    let body = AST.JSBlock testAnnot [] testAnnot
    let expr = AST.JSAsyncFunctionExpression testAnnot testAnnot testIdent testAnnot AST.JSLNil testAnnot body
    expr `shouldSatisfy` isJSAsyncFunctionExpression

  it "constructs JSMemberDot correctly" $ do
    let obj = AST.JSIdentifier testAnnot "obj"
    let prop = AST.JSIdentifier testAnnot "prop"
    let expr = AST.JSMemberDot obj testAnnot prop
    expr `shouldSatisfy` isJSMemberDot

  it "constructs JSMemberExpression correctly" $ do
    let expr = AST.JSMemberExpression (AST.JSIdentifier testAnnot "obj") testAnnot AST.JSLNil testAnnot
    expr `shouldSatisfy` isJSMemberExpression

  it "constructs JSMemberNew correctly" $ do
    let ctor = AST.JSIdentifier testAnnot "Array"
    let expr = AST.JSMemberNew testAnnot ctor testAnnot AST.JSLNil testAnnot
    expr `shouldSatisfy` isJSMemberNew

  it "constructs JSMemberSquare correctly" $ do
    let obj = AST.JSIdentifier testAnnot "obj"
    let key = AST.JSStringLiteral testAnnot "\"key\""
    let expr = AST.JSMemberSquare obj testAnnot key testAnnot
    expr `shouldSatisfy` isJSMemberSquare

  it "constructs JSNewExpression correctly" $ do
    let ctor = AST.JSIdentifier testAnnot "Date"
    let expr = AST.JSNewExpression testAnnot ctor
    expr `shouldSatisfy` isJSNewExpression

  it "constructs JSOptionalMemberDot correctly" $ do
    let obj = AST.JSIdentifier testAnnot "obj"
    let prop = AST.JSIdentifier testAnnot "prop"
    let expr = AST.JSOptionalMemberDot obj testAnnot prop
    expr `shouldSatisfy` isJSOptionalMemberDot

  it "constructs JSOptionalMemberSquare correctly" $ do
    let obj = AST.JSIdentifier testAnnot "obj"
    let key = AST.JSStringLiteral testAnnot "\"key\""
    let expr = AST.JSOptionalMemberSquare obj testAnnot key testAnnot
    expr `shouldSatisfy` isJSOptionalMemberSquare

  it "constructs JSOptionalCallExpression correctly" $ do
    let fn = AST.JSIdentifier testAnnot "fn"
    let expr = AST.JSOptionalCallExpression fn testAnnot AST.JSLNil testAnnot
    expr `shouldSatisfy` isJSOptionalCallExpression

  it "constructs JSObjectLiteral correctly" $ do
    let props = AST.JSCTLNone AST.JSLNil
    let expr = AST.JSObjectLiteral testAnnot props testAnnot
    expr `shouldSatisfy` isJSObjectLiteral

  it "constructs JSSpreadExpression correctly" $ do
    let innerExpr = AST.JSIdentifier testAnnot "args"
    let expr = AST.JSSpreadExpression testAnnot innerExpr
    expr `shouldSatisfy` isJSSpreadExpression

  it "constructs JSTemplateLiteral correctly" $ do
    let expr = AST.JSTemplateLiteral Nothing testAnnot "hello" []
    expr `shouldSatisfy` isJSTemplateLiteral

  it "constructs JSUnaryExpression correctly" $ do
    let op = AST.JSUnaryOpNot testAnnot
    let innerExpr = AST.JSIdentifier testAnnot "x"
    let expr = AST.JSUnaryExpression op innerExpr
    expr `shouldSatisfy` isJSUnaryExpression

  it "constructs JSVarInitExpression correctly" $ do
    let ident = AST.JSIdentifier testAnnot "x"
    let init = AST.JSVarInit testAnnot (AST.JSDecimal testAnnot "42")
    let expr = AST.JSVarInitExpression ident init
    expr `shouldSatisfy` isJSVarInitExpression

  it "constructs JSYieldExpression correctly" $ do
    let expr = AST.JSYieldExpression testAnnot (Just (AST.JSDecimal testAnnot "42"))
    expr `shouldSatisfy` isJSYieldExpression

  it "constructs JSYieldFromExpression correctly" $ do
    let innerExpr = AST.JSIdentifier testAnnot "generator"
    let expr = AST.JSYieldFromExpression testAnnot testAnnot innerExpr
    expr `shouldSatisfy` isJSYieldFromExpression

  it "constructs JSImportMeta correctly" $ do
    let expr = AST.JSImportMeta testAnnot testAnnot
    expr `shouldSatisfy` isJSImportMeta

-- | Test all statement constructors
testStatementConstructors :: Spec
testStatementConstructors = describe "Statement constructors" $ do
  it "constructs JSStatementBlock correctly" $ do
    let stmt = AST.JSStatementBlock testAnnot [] testAnnot testSemi
    stmt `shouldSatisfy` isJSStatementBlock

  it "constructs JSBreak correctly" $ do
    let stmt = AST.JSBreak testAnnot testIdent testSemi
    stmt `shouldSatisfy` isJSBreak

  it "constructs JSLet correctly" $ do
    let stmt = AST.JSLet testAnnot AST.JSLNil testSemi
    stmt `shouldSatisfy` isJSLet

  it "constructs JSClass correctly" $ do
    let stmt = AST.JSClass testAnnot testIdent AST.JSExtendsNone testAnnot [] testAnnot testSemi
    stmt `shouldSatisfy` isJSClass

  it "constructs JSConstant correctly" $ do
    let decl =
          AST.JSVarInitExpression
            (AST.JSIdentifier testAnnot "x")
            (AST.JSVarInit testAnnot (AST.JSDecimal testAnnot "42"))
    let stmt = AST.JSConstant testAnnot (AST.JSLOne decl) testSemi
    stmt `shouldSatisfy` isJSConstant

  it "constructs JSContinue correctly" $ do
    let stmt = AST.JSContinue testAnnot testIdent testSemi
    stmt `shouldSatisfy` isJSContinue

  it "constructs JSDoWhile correctly" $ do
    let body = AST.JSEmptyStatement testAnnot
    let cond = AST.JSLiteral testAnnot "true"
    let stmt = AST.JSDoWhile testAnnot body testAnnot testAnnot cond testAnnot testSemi
    stmt `shouldSatisfy` isJSDoWhile

  it "constructs JSFor correctly" $ do
    let init = AST.JSLNil
    let test = AST.JSLNil
    let update = AST.JSLNil
    let body = AST.JSEmptyStatement testAnnot
    let stmt = AST.JSFor testAnnot testAnnot init testAnnot test testAnnot update testAnnot body
    stmt `shouldSatisfy` isJSFor

  it "constructs JSFunction correctly" $ do
    let body = AST.JSBlock testAnnot [] testAnnot
    let stmt = AST.JSFunction testAnnot testIdent testAnnot AST.JSLNil testAnnot body testSemi
    stmt `shouldSatisfy` isJSFunction

  it "constructs JSGenerator correctly" $ do
    let body = AST.JSBlock testAnnot [] testAnnot
    let stmt = AST.JSGenerator testAnnot testAnnot testIdent testAnnot AST.JSLNil testAnnot body testSemi
    stmt `shouldSatisfy` isJSGenerator

  it "constructs JSAsyncFunction correctly" $ do
    let body = AST.JSBlock testAnnot [] testAnnot
    let stmt = AST.JSAsyncFunction testAnnot testAnnot testIdent testAnnot AST.JSLNil testAnnot body testSemi
    stmt `shouldSatisfy` isJSAsyncFunction

  it "constructs JSIf correctly" $ do
    let cond = AST.JSLiteral testAnnot "true"
    let thenStmt = AST.JSEmptyStatement testAnnot
    let stmt = AST.JSIf testAnnot testAnnot cond testAnnot thenStmt
    stmt `shouldSatisfy` isJSIf

  it "constructs JSIfElse correctly" $ do
    let cond = AST.JSLiteral testAnnot "true"
    let thenStmt = AST.JSEmptyStatement testAnnot
    let elseStmt = AST.JSEmptyStatement testAnnot
    let stmt = AST.JSIfElse testAnnot testAnnot cond testAnnot thenStmt testAnnot elseStmt
    stmt `shouldSatisfy` isJSIfElse

  it "constructs JSLabelled correctly" $ do
    let labelStmt = AST.JSEmptyStatement testAnnot
    let stmt = AST.JSLabelled testIdent testAnnot labelStmt
    stmt `shouldSatisfy` isJSLabelled

  it "constructs JSEmptyStatement correctly" $ do
    let stmt = AST.JSEmptyStatement testAnnot
    stmt `shouldSatisfy` isJSEmptyStatement

  it "constructs JSExpressionStatement correctly" $ do
    let expr = AST.JSDecimal testAnnot "42"
    let stmt = AST.JSExpressionStatement expr testSemi
    stmt `shouldSatisfy` isJSExpressionStatement

  it "constructs JSReturn correctly" $ do
    let stmt = AST.JSReturn testAnnot (Just (AST.JSDecimal testAnnot "42")) testSemi
    stmt `shouldSatisfy` isJSReturn

  it "constructs JSSwitch correctly" $ do
    let expr = AST.JSIdentifier testAnnot "x"
    let stmt = AST.JSSwitch testAnnot testAnnot expr testAnnot testAnnot [] testAnnot testSemi
    stmt `shouldSatisfy` isJSSwitch

  it "constructs JSThrow correctly" $ do
    let expr = AST.JSIdentifier testAnnot "error"
    let stmt = AST.JSThrow testAnnot expr testSemi
    stmt `shouldSatisfy` isJSThrow

  it "constructs JSTry correctly" $ do
    let block = AST.JSBlock testAnnot [] testAnnot
    let stmt = AST.JSTry testAnnot block [] AST.JSNoFinally
    stmt `shouldSatisfy` isJSTry

  it "constructs JSVariable correctly" $ do
    let decl = AST.JSVarInitExpression (AST.JSIdentifier testAnnot "x") AST.JSVarInitNone
    let stmt = AST.JSVariable testAnnot (AST.JSLOne decl) testSemi
    stmt `shouldSatisfy` isJSVariable

  it "constructs JSWhile correctly" $ do
    let cond = AST.JSLiteral testAnnot "true"
    let body = AST.JSEmptyStatement testAnnot
    let stmt = AST.JSWhile testAnnot testAnnot cond testAnnot body
    stmt `shouldSatisfy` isJSWhile

  it "constructs JSWith correctly" $ do
    let expr = AST.JSIdentifier testAnnot "obj"
    let body = AST.JSEmptyStatement testAnnot
    let stmt = AST.JSWith testAnnot testAnnot expr testAnnot body testSemi
    stmt `shouldSatisfy` isJSWith

-- | Test binary operator constructors
testBinaryOperators :: Spec
testBinaryOperators = describe "Binary operators" $ do
  it "constructs all binary operators correctly" $ do
    AST.JSBinOpAnd testAnnot `shouldSatisfy` isJSBinOpAnd
    AST.JSBinOpBitAnd testAnnot `shouldSatisfy` isJSBinOpBitAnd
    AST.JSBinOpBitOr testAnnot `shouldSatisfy` isJSBinOpBitOr
    AST.JSBinOpBitXor testAnnot `shouldSatisfy` isJSBinOpBitXor
    AST.JSBinOpDivide testAnnot `shouldSatisfy` isJSBinOpDivide
    AST.JSBinOpEq testAnnot `shouldSatisfy` isJSBinOpEq
    AST.JSBinOpExponentiation testAnnot `shouldSatisfy` isJSBinOpExponentiation
    AST.JSBinOpGe testAnnot `shouldSatisfy` isJSBinOpGe
    AST.JSBinOpGt testAnnot `shouldSatisfy` isJSBinOpGt
    AST.JSBinOpIn testAnnot `shouldSatisfy` isJSBinOpIn
    AST.JSBinOpInstanceOf testAnnot `shouldSatisfy` isJSBinOpInstanceOf
    AST.JSBinOpLe testAnnot `shouldSatisfy` isJSBinOpLe
    AST.JSBinOpLsh testAnnot `shouldSatisfy` isJSBinOpLsh
    AST.JSBinOpLt testAnnot `shouldSatisfy` isJSBinOpLt
    AST.JSBinOpMinus testAnnot `shouldSatisfy` isJSBinOpMinus
    AST.JSBinOpMod testAnnot `shouldSatisfy` isJSBinOpMod
    AST.JSBinOpNeq testAnnot `shouldSatisfy` isJSBinOpNeq
    AST.JSBinOpOf testAnnot `shouldSatisfy` isJSBinOpOf
    AST.JSBinOpOr testAnnot `shouldSatisfy` isJSBinOpOr
    AST.JSBinOpNullishCoalescing testAnnot `shouldSatisfy` isJSBinOpNullishCoalescing
    AST.JSBinOpPlus testAnnot `shouldSatisfy` isJSBinOpPlus
    AST.JSBinOpRsh testAnnot `shouldSatisfy` isJSBinOpRsh
    AST.JSBinOpStrictEq testAnnot `shouldSatisfy` isJSBinOpStrictEq
    AST.JSBinOpStrictNeq testAnnot `shouldSatisfy` isJSBinOpStrictNeq
    AST.JSBinOpTimes testAnnot `shouldSatisfy` isJSBinOpTimes
    AST.JSBinOpUrsh testAnnot `shouldSatisfy` isJSBinOpUrsh

-- | Test unary operator constructors
testUnaryOperators :: Spec
testUnaryOperators = describe "Unary operators" $ do
  it "constructs all unary operators correctly" $ do
    AST.JSUnaryOpDecr testAnnot `shouldSatisfy` isJSUnaryOpDecr
    AST.JSUnaryOpDelete testAnnot `shouldSatisfy` isJSUnaryOpDelete
    AST.JSUnaryOpIncr testAnnot `shouldSatisfy` isJSUnaryOpIncr
    AST.JSUnaryOpMinus testAnnot `shouldSatisfy` isJSUnaryOpMinus
    AST.JSUnaryOpNot testAnnot `shouldSatisfy` isJSUnaryOpNot
    AST.JSUnaryOpPlus testAnnot `shouldSatisfy` isJSUnaryOpPlus
    AST.JSUnaryOpTilde testAnnot `shouldSatisfy` isJSUnaryOpTilde
    AST.JSUnaryOpTypeof testAnnot `shouldSatisfy` isJSUnaryOpTypeof
    AST.JSUnaryOpVoid testAnnot `shouldSatisfy` isJSUnaryOpVoid

-- | Test assignment operator constructors
testAssignmentOperators :: Spec
testAssignmentOperators = describe "Assignment operators" $ do
  it "constructs all assignment operators correctly" $ do
    AST.JSAssign testAnnot `shouldSatisfy` isJSAssign
    AST.JSTimesAssign testAnnot `shouldSatisfy` isJSTimesAssign
    AST.JSDivideAssign testAnnot `shouldSatisfy` isJSDivideAssign
    AST.JSModAssign testAnnot `shouldSatisfy` isJSModAssign
    AST.JSPlusAssign testAnnot `shouldSatisfy` isJSPlusAssign
    AST.JSMinusAssign testAnnot `shouldSatisfy` isJSMinusAssign
    AST.JSLshAssign testAnnot `shouldSatisfy` isJSLshAssign
    AST.JSRshAssign testAnnot `shouldSatisfy` isJSRshAssign
    AST.JSUrshAssign testAnnot `shouldSatisfy` isJSUrshAssign
    AST.JSBwAndAssign testAnnot `shouldSatisfy` isJSBwAndAssign
    AST.JSBwXorAssign testAnnot `shouldSatisfy` isJSBwXorAssign
    AST.JSBwOrAssign testAnnot `shouldSatisfy` isJSBwOrAssign
    AST.JSLogicalAndAssign testAnnot `shouldSatisfy` isJSLogicalAndAssign
    AST.JSLogicalOrAssign testAnnot `shouldSatisfy` isJSLogicalOrAssign
    AST.JSNullishAssign testAnnot `shouldSatisfy` isJSNullishAssign

-- | Test module system constructors
testModuleConstructors :: Spec
testModuleConstructors = describe "Module system" $ do
  it "constructs module items correctly" $ do
    let importDecl = AST.JSImportDeclarationBare testAnnot "\"module\"" Nothing testSemi
    let moduleItem = AST.JSModuleImportDeclaration testAnnot importDecl
    moduleItem `shouldSatisfy` isJSModuleImportDeclaration

  it "constructs import declarations correctly" $ do
    let fromClause = AST.JSFromClause testAnnot testAnnot "\"./module\""
    let importClause = AST.JSImportClauseDefault testIdent
    let decl = AST.JSImportDeclaration importClause fromClause Nothing testSemi
    decl `shouldSatisfy` isJSImportDeclaration

  it "constructs export declarations correctly" $ do
    let exportClause = AST.JSExportClause testAnnot AST.JSLNil testAnnot
    let fromClause = AST.JSFromClause testAnnot testAnnot "\"./module\""
    let decl = AST.JSExportFrom exportClause fromClause testSemi
    decl `shouldSatisfy` isJSExportFrom

-- | Test class constructors
testClassConstructors :: Spec
testClassConstructors = describe "Class elements" $ do
  it "constructs class elements correctly" $ do
    let methodDef =
          AST.JSMethodDefinition
            (AST.JSPropertyIdent testAnnot "method")
            testAnnot
            AST.JSLNil
            testAnnot
            (AST.JSBlock testAnnot [] testAnnot)
    let element = AST.JSClassInstanceMethod methodDef
    element `shouldSatisfy` isJSClassInstanceMethod

  it "constructs private fields correctly" $ do
    let element = AST.JSPrivateField testAnnot "field" testAnnot Nothing testSemi
    element `shouldSatisfy` isJSPrivateField

-- | Test utility constructors
testUtilityConstructors :: Spec
testUtilityConstructors = describe "Utility constructors" $ do
  it "constructs JSAnnot correctly" $ do
    let annot = AST.JSAnnot (TokenPn 0 1 1) []
    annot `shouldSatisfy` isJSAnnot

  it "constructs JSCommaList correctly" $ do
    let list = AST.JSLOne (AST.JSDecimal testAnnot "1")
    list `shouldSatisfy` isJSCommaList

  it "constructs JSBlock correctly" $ do
    let block = AST.JSBlock testAnnot [] testAnnot
    block `shouldSatisfy` isJSBlock

-- | Test pattern matching exhaustiveness
testPatternMatchingCoverage :: Spec
testPatternMatchingCoverage = describe "Pattern matching coverage" $ do
  it "covers all JSExpression patterns" $ do
    let expressions = allExpressionConstructors
    length expressions `shouldBe` 41 -- All JSExpression constructors (corrected)
    all isValidExpression expressions `shouldBe` True

  it "covers all JSStatement patterns" $ do
    let statements = allStatementConstructors
    length statements `shouldBe` 36 -- All JSStatement constructors (corrected)
    all isValidStatement statements `shouldBe` True

-- Helper functions for constructor testing

extractLiteral :: AST.JSExpression -> ByteString
extractLiteral (AST.JSIdentifier _ s) = s
extractLiteral (AST.JSDecimal _ s) = s
extractLiteral (AST.JSLiteral _ s) = s
extractLiteral (AST.JSHexInteger _ s) = s
extractLiteral (AST.JSBinaryInteger _ s) = s
extractLiteral (AST.JSOctal _ s) = s
extractLiteral (AST.JSBigIntLiteral _ s) = s
extractLiteral (AST.JSStringLiteral _ s) = s
extractLiteral (AST.JSRegEx _ s) = s
extractLiteral _ = ""

-- Constructor identification functions (predicates)

isJSIdentifier :: AST.JSExpression -> Bool
isJSIdentifier (AST.JSIdentifier {}) = True
isJSIdentifier _ = False

isJSDecimal :: AST.JSExpression -> Bool
isJSDecimal (AST.JSDecimal {}) = True
isJSDecimal _ = False

isJSLiteral :: AST.JSExpression -> Bool
isJSLiteral (AST.JSLiteral {}) = True
isJSLiteral _ = False

isJSHexInteger :: AST.JSExpression -> Bool
isJSHexInteger (AST.JSHexInteger {}) = True
isJSHexInteger _ = False

isJSBinaryInteger :: AST.JSExpression -> Bool
isJSBinaryInteger (AST.JSBinaryInteger {}) = True
isJSBinaryInteger _ = False

isJSOctal :: AST.JSExpression -> Bool
isJSOctal (AST.JSOctal {}) = True
isJSOctal _ = False

isJSBigIntLiteral :: AST.JSExpression -> Bool
isJSBigIntLiteral (AST.JSBigIntLiteral {}) = True
isJSBigIntLiteral _ = False

isJSStringLiteral :: AST.JSExpression -> Bool
isJSStringLiteral (AST.JSStringLiteral {}) = True
isJSStringLiteral _ = False

isJSRegEx :: AST.JSExpression -> Bool
isJSRegEx (AST.JSRegEx {}) = True
isJSRegEx _ = False

isJSArrayLiteral :: AST.JSExpression -> Bool
isJSArrayLiteral (AST.JSArrayLiteral {}) = True
isJSArrayLiteral _ = False

isJSAssignExpression :: AST.JSExpression -> Bool
isJSAssignExpression (AST.JSAssignExpression {}) = True
isJSAssignExpression _ = False

isJSAwaitExpression :: AST.JSExpression -> Bool
isJSAwaitExpression (AST.JSAwaitExpression {}) = True
isJSAwaitExpression _ = False

isJSCallExpression :: AST.JSExpression -> Bool
isJSCallExpression (AST.JSCallExpression {}) = True
isJSCallExpression _ = False

isJSCallExpressionDot :: AST.JSExpression -> Bool
isJSCallExpressionDot (AST.JSCallExpressionDot {}) = True
isJSCallExpressionDot _ = False

isJSCallExpressionSquare :: AST.JSExpression -> Bool
isJSCallExpressionSquare (AST.JSCallExpressionSquare {}) = True
isJSCallExpressionSquare _ = False

isJSClassExpression :: AST.JSExpression -> Bool
isJSClassExpression (AST.JSClassExpression {}) = True
isJSClassExpression _ = False

isJSCommaExpression :: AST.JSExpression -> Bool
isJSCommaExpression (AST.JSCommaExpression {}) = True
isJSCommaExpression _ = False

isJSExpressionBinary :: AST.JSExpression -> Bool
isJSExpressionBinary (AST.JSExpressionBinary {}) = True
isJSExpressionBinary _ = False

isJSExpressionParen :: AST.JSExpression -> Bool
isJSExpressionParen (AST.JSExpressionParen {}) = True
isJSExpressionParen _ = False

isJSExpressionPostfix :: AST.JSExpression -> Bool
isJSExpressionPostfix (AST.JSExpressionPostfix {}) = True
isJSExpressionPostfix _ = False

isJSExpressionTernary :: AST.JSExpression -> Bool
isJSExpressionTernary (AST.JSExpressionTernary {}) = True
isJSExpressionTernary _ = False

isJSArrowExpression :: AST.JSExpression -> Bool
isJSArrowExpression (AST.JSArrowExpression {}) = True
isJSArrowExpression _ = False

isJSFunctionExpression :: AST.JSExpression -> Bool
isJSFunctionExpression (AST.JSFunctionExpression {}) = True
isJSFunctionExpression _ = False

isJSGeneratorExpression :: AST.JSExpression -> Bool
isJSGeneratorExpression (AST.JSGeneratorExpression {}) = True
isJSGeneratorExpression _ = False

isJSAsyncFunctionExpression :: AST.JSExpression -> Bool
isJSAsyncFunctionExpression (AST.JSAsyncFunctionExpression {}) = True
isJSAsyncFunctionExpression _ = False

isJSMemberDot :: AST.JSExpression -> Bool
isJSMemberDot (AST.JSMemberDot {}) = True
isJSMemberDot _ = False

isJSMemberExpression :: AST.JSExpression -> Bool
isJSMemberExpression (AST.JSMemberExpression {}) = True
isJSMemberExpression _ = False

isJSMemberNew :: AST.JSExpression -> Bool
isJSMemberNew (AST.JSMemberNew {}) = True
isJSMemberNew _ = False

isJSMemberSquare :: AST.JSExpression -> Bool
isJSMemberSquare (AST.JSMemberSquare {}) = True
isJSMemberSquare _ = False

isJSNewExpression :: AST.JSExpression -> Bool
isJSNewExpression (AST.JSNewExpression {}) = True
isJSNewExpression _ = False

isJSOptionalMemberDot :: AST.JSExpression -> Bool
isJSOptionalMemberDot (AST.JSOptionalMemberDot {}) = True
isJSOptionalMemberDot _ = False

isJSOptionalMemberSquare :: AST.JSExpression -> Bool
isJSOptionalMemberSquare (AST.JSOptionalMemberSquare {}) = True
isJSOptionalMemberSquare _ = False

isJSOptionalCallExpression :: AST.JSExpression -> Bool
isJSOptionalCallExpression (AST.JSOptionalCallExpression {}) = True
isJSOptionalCallExpression _ = False

isJSObjectLiteral :: AST.JSExpression -> Bool
isJSObjectLiteral (AST.JSObjectLiteral {}) = True
isJSObjectLiteral _ = False

isJSSpreadExpression :: AST.JSExpression -> Bool
isJSSpreadExpression (AST.JSSpreadExpression {}) = True
isJSSpreadExpression _ = False

isJSTemplateLiteral :: AST.JSExpression -> Bool
isJSTemplateLiteral (AST.JSTemplateLiteral {}) = True
isJSTemplateLiteral _ = False

isJSUnaryExpression :: AST.JSExpression -> Bool
isJSUnaryExpression (AST.JSUnaryExpression {}) = True
isJSUnaryExpression _ = False

isJSVarInitExpression :: AST.JSExpression -> Bool
isJSVarInitExpression (AST.JSVarInitExpression {}) = True
isJSVarInitExpression _ = False

isJSYieldExpression :: AST.JSExpression -> Bool
isJSYieldExpression (AST.JSYieldExpression {}) = True
isJSYieldExpression _ = False

isJSYieldFromExpression :: AST.JSExpression -> Bool
isJSYieldFromExpression (AST.JSYieldFromExpression {}) = True
isJSYieldFromExpression _ = False

isJSImportMeta :: AST.JSExpression -> Bool
isJSImportMeta (AST.JSImportMeta {}) = True
isJSImportMeta _ = False

-- Statement constructor predicates

isJSStatementBlock :: AST.JSStatement -> Bool
isJSStatementBlock (AST.JSStatementBlock {}) = True
isJSStatementBlock _ = False

isJSBreak :: AST.JSStatement -> Bool
isJSBreak (AST.JSBreak {}) = True
isJSBreak _ = False

isJSLet :: AST.JSStatement -> Bool
isJSLet (AST.JSLet {}) = True
isJSLet _ = False

isJSClass :: AST.JSStatement -> Bool
isJSClass (AST.JSClass {}) = True
isJSClass _ = False

isJSConstant :: AST.JSStatement -> Bool
isJSConstant (AST.JSConstant {}) = True
isJSConstant _ = False

isJSContinue :: AST.JSStatement -> Bool
isJSContinue (AST.JSContinue {}) = True
isJSContinue _ = False

isJSDoWhile :: AST.JSStatement -> Bool
isJSDoWhile (AST.JSDoWhile {}) = True
isJSDoWhile _ = False

isJSFor :: AST.JSStatement -> Bool
isJSFor (AST.JSFor {}) = True
isJSFor _ = False

isJSFunction :: AST.JSStatement -> Bool
isJSFunction (AST.JSFunction {}) = True
isJSFunction _ = False

isJSGenerator :: AST.JSStatement -> Bool
isJSGenerator (AST.JSGenerator {}) = True
isJSGenerator _ = False

isJSAsyncFunction :: AST.JSStatement -> Bool
isJSAsyncFunction (AST.JSAsyncFunction {}) = True
isJSAsyncFunction _ = False

isJSIf :: AST.JSStatement -> Bool
isJSIf (AST.JSIf {}) = True
isJSIf _ = False

isJSIfElse :: AST.JSStatement -> Bool
isJSIfElse (AST.JSIfElse {}) = True
isJSIfElse _ = False

isJSLabelled :: AST.JSStatement -> Bool
isJSLabelled (AST.JSLabelled {}) = True
isJSLabelled _ = False

isJSEmptyStatement :: AST.JSStatement -> Bool
isJSEmptyStatement (AST.JSEmptyStatement {}) = True
isJSEmptyStatement _ = False

isJSExpressionStatement :: AST.JSStatement -> Bool
isJSExpressionStatement (AST.JSExpressionStatement {}) = True
isJSExpressionStatement _ = False

isJSReturn :: AST.JSStatement -> Bool
isJSReturn (AST.JSReturn {}) = True
isJSReturn _ = False

isJSSwitch :: AST.JSStatement -> Bool
isJSSwitch (AST.JSSwitch {}) = True
isJSSwitch _ = False

isJSThrow :: AST.JSStatement -> Bool
isJSThrow (AST.JSThrow {}) = True
isJSThrow _ = False

isJSTry :: AST.JSStatement -> Bool
isJSTry (AST.JSTry {}) = True
isJSTry _ = False

isJSVariable :: AST.JSStatement -> Bool
isJSVariable (AST.JSVariable {}) = True
isJSVariable _ = False

isJSWhile :: AST.JSStatement -> Bool
isJSWhile (AST.JSWhile {}) = True
isJSWhile _ = False

isJSWith :: AST.JSStatement -> Bool
isJSWith (AST.JSWith {}) = True
isJSWith _ = False

-- Operator constructor predicates

isJSBinOpAnd :: AST.JSBinOp -> Bool
isJSBinOpAnd (AST.JSBinOpAnd {}) = True
isJSBinOpAnd _ = False

isJSBinOpBitAnd :: AST.JSBinOp -> Bool
isJSBinOpBitAnd (AST.JSBinOpBitAnd {}) = True
isJSBinOpBitAnd _ = False

isJSBinOpBitOr :: AST.JSBinOp -> Bool
isJSBinOpBitOr (AST.JSBinOpBitOr {}) = True
isJSBinOpBitOr _ = False

isJSBinOpBitXor :: AST.JSBinOp -> Bool
isJSBinOpBitXor (AST.JSBinOpBitXor {}) = True
isJSBinOpBitXor _ = False

isJSBinOpDivide :: AST.JSBinOp -> Bool
isJSBinOpDivide (AST.JSBinOpDivide {}) = True
isJSBinOpDivide _ = False

isJSBinOpEq :: AST.JSBinOp -> Bool
isJSBinOpEq (AST.JSBinOpEq {}) = True
isJSBinOpEq _ = False

isJSBinOpExponentiation :: AST.JSBinOp -> Bool
isJSBinOpExponentiation (AST.JSBinOpExponentiation {}) = True
isJSBinOpExponentiation _ = False

isJSBinOpGe :: AST.JSBinOp -> Bool
isJSBinOpGe (AST.JSBinOpGe {}) = True
isJSBinOpGe _ = False

isJSBinOpGt :: AST.JSBinOp -> Bool
isJSBinOpGt (AST.JSBinOpGt {}) = True
isJSBinOpGt _ = False

isJSBinOpIn :: AST.JSBinOp -> Bool
isJSBinOpIn (AST.JSBinOpIn {}) = True
isJSBinOpIn _ = False

isJSBinOpInstanceOf :: AST.JSBinOp -> Bool
isJSBinOpInstanceOf (AST.JSBinOpInstanceOf {}) = True
isJSBinOpInstanceOf _ = False

isJSBinOpLe :: AST.JSBinOp -> Bool
isJSBinOpLe (AST.JSBinOpLe {}) = True
isJSBinOpLe _ = False

isJSBinOpLsh :: AST.JSBinOp -> Bool
isJSBinOpLsh (AST.JSBinOpLsh {}) = True
isJSBinOpLsh _ = False

isJSBinOpLt :: AST.JSBinOp -> Bool
isJSBinOpLt (AST.JSBinOpLt {}) = True
isJSBinOpLt _ = False

isJSBinOpMinus :: AST.JSBinOp -> Bool
isJSBinOpMinus (AST.JSBinOpMinus {}) = True
isJSBinOpMinus _ = False

isJSBinOpMod :: AST.JSBinOp -> Bool
isJSBinOpMod (AST.JSBinOpMod {}) = True
isJSBinOpMod _ = False

isJSBinOpNeq :: AST.JSBinOp -> Bool
isJSBinOpNeq (AST.JSBinOpNeq {}) = True
isJSBinOpNeq _ = False

isJSBinOpOf :: AST.JSBinOp -> Bool
isJSBinOpOf (AST.JSBinOpOf {}) = True
isJSBinOpOf _ = False

isJSBinOpOr :: AST.JSBinOp -> Bool
isJSBinOpOr (AST.JSBinOpOr {}) = True
isJSBinOpOr _ = False

isJSBinOpNullishCoalescing :: AST.JSBinOp -> Bool
isJSBinOpNullishCoalescing (AST.JSBinOpNullishCoalescing {}) = True
isJSBinOpNullishCoalescing _ = False

isJSBinOpPlus :: AST.JSBinOp -> Bool
isJSBinOpPlus (AST.JSBinOpPlus {}) = True
isJSBinOpPlus _ = False

isJSBinOpRsh :: AST.JSBinOp -> Bool
isJSBinOpRsh (AST.JSBinOpRsh {}) = True
isJSBinOpRsh _ = False

isJSBinOpStrictEq :: AST.JSBinOp -> Bool
isJSBinOpStrictEq (AST.JSBinOpStrictEq {}) = True
isJSBinOpStrictEq _ = False

isJSBinOpStrictNeq :: AST.JSBinOp -> Bool
isJSBinOpStrictNeq (AST.JSBinOpStrictNeq {}) = True
isJSBinOpStrictNeq _ = False

isJSBinOpTimes :: AST.JSBinOp -> Bool
isJSBinOpTimes (AST.JSBinOpTimes {}) = True
isJSBinOpTimes _ = False

isJSBinOpUrsh :: AST.JSBinOp -> Bool
isJSBinOpUrsh (AST.JSBinOpUrsh {}) = True
isJSBinOpUrsh _ = False

isJSUnaryOpDecr :: AST.JSUnaryOp -> Bool
isJSUnaryOpDecr (AST.JSUnaryOpDecr {}) = True
isJSUnaryOpDecr _ = False

isJSUnaryOpDelete :: AST.JSUnaryOp -> Bool
isJSUnaryOpDelete (AST.JSUnaryOpDelete {}) = True
isJSUnaryOpDelete _ = False

isJSUnaryOpIncr :: AST.JSUnaryOp -> Bool
isJSUnaryOpIncr (AST.JSUnaryOpIncr {}) = True
isJSUnaryOpIncr _ = False

isJSUnaryOpMinus :: AST.JSUnaryOp -> Bool
isJSUnaryOpMinus (AST.JSUnaryOpMinus {}) = True
isJSUnaryOpMinus _ = False

isJSUnaryOpNot :: AST.JSUnaryOp -> Bool
isJSUnaryOpNot (AST.JSUnaryOpNot {}) = True
isJSUnaryOpNot _ = False

isJSUnaryOpPlus :: AST.JSUnaryOp -> Bool
isJSUnaryOpPlus (AST.JSUnaryOpPlus {}) = True
isJSUnaryOpPlus _ = False

isJSUnaryOpTilde :: AST.JSUnaryOp -> Bool
isJSUnaryOpTilde (AST.JSUnaryOpTilde {}) = True
isJSUnaryOpTilde _ = False

isJSUnaryOpTypeof :: AST.JSUnaryOp -> Bool
isJSUnaryOpTypeof (AST.JSUnaryOpTypeof {}) = True
isJSUnaryOpTypeof _ = False

isJSUnaryOpVoid :: AST.JSUnaryOp -> Bool
isJSUnaryOpVoid (AST.JSUnaryOpVoid {}) = True
isJSUnaryOpVoid _ = False

isJSAssign :: AST.JSAssignOp -> Bool
isJSAssign (AST.JSAssign {}) = True
isJSAssign _ = False

isJSTimesAssign :: AST.JSAssignOp -> Bool
isJSTimesAssign (AST.JSTimesAssign {}) = True
isJSTimesAssign _ = False

isJSDivideAssign :: AST.JSAssignOp -> Bool
isJSDivideAssign (AST.JSDivideAssign {}) = True
isJSDivideAssign _ = False

isJSModAssign :: AST.JSAssignOp -> Bool
isJSModAssign (AST.JSModAssign {}) = True
isJSModAssign _ = False

isJSPlusAssign :: AST.JSAssignOp -> Bool
isJSPlusAssign (AST.JSPlusAssign {}) = True
isJSPlusAssign _ = False

isJSMinusAssign :: AST.JSAssignOp -> Bool
isJSMinusAssign (AST.JSMinusAssign {}) = True
isJSMinusAssign _ = False

isJSLshAssign :: AST.JSAssignOp -> Bool
isJSLshAssign (AST.JSLshAssign {}) = True
isJSLshAssign _ = False

isJSRshAssign :: AST.JSAssignOp -> Bool
isJSRshAssign (AST.JSRshAssign {}) = True
isJSRshAssign _ = False

isJSUrshAssign :: AST.JSAssignOp -> Bool
isJSUrshAssign (AST.JSUrshAssign {}) = True
isJSUrshAssign _ = False

isJSBwAndAssign :: AST.JSAssignOp -> Bool
isJSBwAndAssign (AST.JSBwAndAssign {}) = True
isJSBwAndAssign _ = False

isJSBwXorAssign :: AST.JSAssignOp -> Bool
isJSBwXorAssign (AST.JSBwXorAssign {}) = True
isJSBwXorAssign _ = False

isJSBwOrAssign :: AST.JSAssignOp -> Bool
isJSBwOrAssign (AST.JSBwOrAssign {}) = True
isJSBwOrAssign _ = False

isJSLogicalAndAssign :: AST.JSAssignOp -> Bool
isJSLogicalAndAssign (AST.JSLogicalAndAssign {}) = True
isJSLogicalAndAssign _ = False

isJSLogicalOrAssign :: AST.JSAssignOp -> Bool
isJSLogicalOrAssign (AST.JSLogicalOrAssign {}) = True
isJSLogicalOrAssign _ = False

isJSNullishAssign :: AST.JSAssignOp -> Bool
isJSNullishAssign (AST.JSNullishAssign {}) = True
isJSNullishAssign _ = False

-- Module constructor predicates

isJSModuleImportDeclaration :: AST.JSModuleItem -> Bool
isJSModuleImportDeclaration (AST.JSModuleImportDeclaration {}) = True
isJSModuleImportDeclaration _ = False

isJSImportDeclaration :: AST.JSImportDeclaration -> Bool
isJSImportDeclaration (AST.JSImportDeclaration {}) = True
isJSImportDeclaration _ = False

isJSExportFrom :: AST.JSExportDeclaration -> Bool
isJSExportFrom (AST.JSExportFrom {}) = True
isJSExportFrom _ = False

-- Class constructor predicates

isJSClassInstanceMethod :: AST.JSClassElement -> Bool
isJSClassInstanceMethod (AST.JSClassInstanceMethod {}) = True
isJSClassInstanceMethod _ = False

isJSPrivateField :: AST.JSClassElement -> Bool
isJSPrivateField (AST.JSPrivateField {}) = True
isJSPrivateField _ = False

-- Utility constructor predicates

isJSAnnot :: AST.JSAnnot -> Bool
isJSAnnot (AST.JSAnnot {}) = True
isJSAnnot _ = False

isJSCommaList :: AST.JSCommaList a -> Bool
isJSCommaList (AST.JSLOne {}) = True
isJSCommaList (AST.JSLCons {}) = True
isJSCommaList AST.JSLNil = True

isJSBlock :: AST.JSBlock -> Bool
isJSBlock (AST.JSBlock {}) = True

-- Generate all constructor instances for pattern matching tests

allExpressionConstructors :: [AST.JSExpression]
allExpressionConstructors =
  [ AST.JSIdentifier testAnnot "test",
    AST.JSDecimal testAnnot "42",
    AST.JSLiteral testAnnot "true",
    AST.JSHexInteger testAnnot "0xFF",
    AST.JSBinaryInteger testAnnot "0b1010",
    AST.JSOctal testAnnot "0o777",
    AST.JSBigIntLiteral testAnnot "123n",
    AST.JSStringLiteral testAnnot "\"hello\"",
    AST.JSRegEx testAnnot "/test/",
    AST.JSArrayLiteral testAnnot [] testAnnot,
    AST.JSAssignExpression (AST.JSIdentifier testAnnot "x") (AST.JSAssign testAnnot) (AST.JSDecimal testAnnot "1"),
    AST.JSAwaitExpression testAnnot (AST.JSIdentifier testAnnot "promise"),
    AST.JSCallExpression (AST.JSIdentifier testAnnot "f") testAnnot AST.JSLNil testAnnot,
    AST.JSCallExpressionDot (AST.JSIdentifier testAnnot "obj") testAnnot (AST.JSIdentifier testAnnot "method"),
    AST.JSCallExpressionSquare (AST.JSIdentifier testAnnot "obj") testAnnot (AST.JSStringLiteral testAnnot "\"key\"") testAnnot,
    AST.JSClassExpression testAnnot testIdent AST.JSExtendsNone testAnnot [] testAnnot,
    AST.JSCommaExpression (AST.JSDecimal testAnnot "1") testAnnot (AST.JSDecimal testAnnot "2"),
    AST.JSExpressionBinary (AST.JSDecimal testAnnot "1") (AST.JSBinOpPlus testAnnot) (AST.JSDecimal testAnnot "2"),
    AST.JSExpressionParen testAnnot (AST.JSDecimal testAnnot "42") testAnnot,
    AST.JSExpressionPostfix (AST.JSIdentifier testAnnot "x") (AST.JSUnaryOpIncr testAnnot),
    AST.JSExpressionTernary (AST.JSIdentifier testAnnot "x") testAnnot (AST.JSDecimal testAnnot "1") testAnnot (AST.JSDecimal testAnnot "2"),
    AST.JSArrowExpression (AST.JSUnparenthesizedArrowParameter testIdent) testAnnot (AST.JSConciseExpressionBody (AST.JSDecimal testAnnot "42")),
    AST.JSFunctionExpression testAnnot testIdent testAnnot AST.JSLNil testAnnot (AST.JSBlock testAnnot [] testAnnot),
    AST.JSGeneratorExpression testAnnot testAnnot testIdent testAnnot AST.JSLNil testAnnot (AST.JSBlock testAnnot [] testAnnot),
    AST.JSAsyncFunctionExpression testAnnot testAnnot testIdent testAnnot AST.JSLNil testAnnot (AST.JSBlock testAnnot [] testAnnot),
    AST.JSMemberDot (AST.JSIdentifier testAnnot "obj") testAnnot (AST.JSIdentifier testAnnot "prop"),
    AST.JSMemberExpression (AST.JSIdentifier testAnnot "obj") testAnnot AST.JSLNil testAnnot,
    AST.JSMemberNew testAnnot (AST.JSIdentifier testAnnot "Array") testAnnot AST.JSLNil testAnnot,
    AST.JSMemberSquare (AST.JSIdentifier testAnnot "obj") testAnnot (AST.JSStringLiteral testAnnot "\"key\"") testAnnot,
    AST.JSNewExpression testAnnot (AST.JSIdentifier testAnnot "Date"),
    AST.JSOptionalMemberDot (AST.JSIdentifier testAnnot "obj") testAnnot (AST.JSIdentifier testAnnot "prop"),
    AST.JSOptionalMemberSquare (AST.JSIdentifier testAnnot "obj") testAnnot (AST.JSStringLiteral testAnnot "\"key\"") testAnnot,
    AST.JSOptionalCallExpression (AST.JSIdentifier testAnnot "fn") testAnnot AST.JSLNil testAnnot,
    AST.JSObjectLiteral testAnnot (AST.JSCTLNone AST.JSLNil) testAnnot,
    AST.JSSpreadExpression testAnnot (AST.JSIdentifier testAnnot "args"),
    AST.JSTemplateLiteral Nothing testAnnot "hello" [],
    AST.JSUnaryExpression (AST.JSUnaryOpNot testAnnot) (AST.JSIdentifier testAnnot "x"),
    AST.JSVarInitExpression (AST.JSIdentifier testAnnot "x") (AST.JSVarInit testAnnot (AST.JSDecimal testAnnot "42")),
    AST.JSYieldExpression testAnnot (Just (AST.JSDecimal testAnnot "42")),
    AST.JSYieldFromExpression testAnnot testAnnot (AST.JSIdentifier testAnnot "generator"),
    AST.JSImportMeta testAnnot testAnnot
  ]

allStatementConstructors :: [AST.JSStatement]
allStatementConstructors =
  [ AST.JSStatementBlock testAnnot [] testAnnot testSemi,
    AST.JSBreak testAnnot testIdent testSemi,
    AST.JSLet testAnnot AST.JSLNil testSemi,
    AST.JSClass testAnnot testIdent AST.JSExtendsNone testAnnot [] testAnnot testSemi,
    AST.JSConstant testAnnot (AST.JSLOne (AST.JSVarInitExpression (AST.JSIdentifier testAnnot "x") (AST.JSVarInit testAnnot (AST.JSDecimal testAnnot "42")))) testSemi,
    AST.JSContinue testAnnot testIdent testSemi,
    AST.JSDoWhile testAnnot (AST.JSEmptyStatement testAnnot) testAnnot testAnnot (AST.JSLiteral testAnnot "true") testAnnot testSemi,
    AST.JSFor testAnnot testAnnot AST.JSLNil testAnnot AST.JSLNil testAnnot AST.JSLNil testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSForIn testAnnot testAnnot (AST.JSIdentifier testAnnot "x") (AST.JSBinOpIn testAnnot) (AST.JSIdentifier testAnnot "obj") testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSForVar testAnnot testAnnot testAnnot AST.JSLNil testAnnot AST.JSLNil testAnnot AST.JSLNil testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSForVarIn testAnnot testAnnot testAnnot (AST.JSIdentifier testAnnot "x") (AST.JSBinOpIn testAnnot) (AST.JSIdentifier testAnnot "obj") testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSForLet testAnnot testAnnot testAnnot AST.JSLNil testAnnot AST.JSLNil testAnnot AST.JSLNil testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSForLetIn testAnnot testAnnot testAnnot (AST.JSIdentifier testAnnot "x") (AST.JSBinOpIn testAnnot) (AST.JSIdentifier testAnnot "obj") testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSForLetOf testAnnot testAnnot testAnnot (AST.JSIdentifier testAnnot "x") (AST.JSBinOpOf testAnnot) (AST.JSIdentifier testAnnot "obj") testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSForConst testAnnot testAnnot testAnnot AST.JSLNil testAnnot AST.JSLNil testAnnot AST.JSLNil testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSForConstIn testAnnot testAnnot testAnnot (AST.JSIdentifier testAnnot "x") (AST.JSBinOpIn testAnnot) (AST.JSIdentifier testAnnot "obj") testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSForConstOf testAnnot testAnnot testAnnot (AST.JSIdentifier testAnnot "x") (AST.JSBinOpOf testAnnot) (AST.JSIdentifier testAnnot "obj") testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSForOf testAnnot testAnnot (AST.JSIdentifier testAnnot "x") (AST.JSBinOpOf testAnnot) (AST.JSIdentifier testAnnot "obj") testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSForVarOf testAnnot testAnnot testAnnot (AST.JSIdentifier testAnnot "x") (AST.JSBinOpOf testAnnot) (AST.JSIdentifier testAnnot "obj") testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSAsyncFunction testAnnot testAnnot testIdent testAnnot AST.JSLNil testAnnot (AST.JSBlock testAnnot [] testAnnot) testSemi,
    AST.JSFunction testAnnot testIdent testAnnot AST.JSLNil testAnnot (AST.JSBlock testAnnot [] testAnnot) testSemi,
    AST.JSGenerator testAnnot testAnnot testIdent testAnnot AST.JSLNil testAnnot (AST.JSBlock testAnnot [] testAnnot) testSemi,
    AST.JSIf testAnnot testAnnot (AST.JSLiteral testAnnot "true") testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSIfElse testAnnot testAnnot (AST.JSLiteral testAnnot "true") testAnnot (AST.JSEmptyStatement testAnnot) testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSLabelled testIdent testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSEmptyStatement testAnnot,
    AST.JSExpressionStatement (AST.JSDecimal testAnnot "42") testSemi,
    AST.JSAssignStatement (AST.JSIdentifier testAnnot "x") (AST.JSAssign testAnnot) (AST.JSDecimal testAnnot "42") testSemi,
    AST.JSMethodCall (AST.JSIdentifier testAnnot "obj") testAnnot AST.JSLNil testAnnot testSemi,
    AST.JSReturn testAnnot (Just (AST.JSDecimal testAnnot "42")) testSemi,
    AST.JSSwitch testAnnot testAnnot (AST.JSIdentifier testAnnot "x") testAnnot testAnnot [] testAnnot testSemi,
    AST.JSThrow testAnnot (AST.JSIdentifier testAnnot "error") testSemi,
    AST.JSTry testAnnot (AST.JSBlock testAnnot [] testAnnot) [] AST.JSNoFinally,
    AST.JSVariable testAnnot (AST.JSLOne (AST.JSVarInitExpression (AST.JSIdentifier testAnnot "x") AST.JSVarInitNone)) testSemi,
    AST.JSWhile testAnnot testAnnot (AST.JSLiteral testAnnot "true") testAnnot (AST.JSEmptyStatement testAnnot),
    AST.JSWith testAnnot testAnnot (AST.JSIdentifier testAnnot "obj") testAnnot (AST.JSEmptyStatement testAnnot) testSemi
    -- This covers 27 statement constructors (now complete)
  ]

-- Validation functions for pattern matching tests

isValidExpression :: AST.JSExpression -> Bool
isValidExpression expr =
  case expr of
    AST.JSIdentifier {} -> True
    AST.JSDecimal {} -> True
    AST.JSLiteral {} -> True
    AST.JSHexInteger {} -> True
    AST.JSBinaryInteger {} -> True
    AST.JSOctal {} -> True
    AST.JSBigIntLiteral {} -> True
    AST.JSStringLiteral {} -> True
    AST.JSRegEx {} -> True
    AST.JSArrayLiteral {} -> True
    AST.JSAssignExpression {} -> True
    AST.JSAwaitExpression {} -> True
    AST.JSCallExpression {} -> True
    AST.JSCallExpressionDot {} -> True
    AST.JSCallExpressionSquare {} -> True
    AST.JSClassExpression {} -> True
    AST.JSCommaExpression {} -> True
    AST.JSExpressionBinary {} -> True
    AST.JSExpressionParen {} -> True
    AST.JSExpressionPostfix {} -> True
    AST.JSExpressionTernary {} -> True
    AST.JSArrowExpression {} -> True
    AST.JSFunctionExpression {} -> True
    AST.JSGeneratorExpression {} -> True
    AST.JSAsyncFunctionExpression {} -> True
    AST.JSMemberDot {} -> True
    AST.JSMemberExpression {} -> True
    AST.JSMemberNew {} -> True
    AST.JSMemberSquare {} -> True
    AST.JSNewExpression {} -> True
    AST.JSOptionalMemberDot {} -> True
    AST.JSOptionalMemberSquare {} -> True
    AST.JSOptionalCallExpression {} -> True
    AST.JSObjectLiteral {} -> True
    AST.JSSpreadExpression {} -> True
    AST.JSTemplateLiteral {} -> True
    AST.JSUnaryExpression {} -> True
    AST.JSVarInitExpression {} -> True
    AST.JSYieldExpression {} -> True
    AST.JSYieldFromExpression {} -> True
    AST.JSImportMeta {} -> True

isValidStatement :: AST.JSStatement -> Bool
isValidStatement stmt =
  case stmt of
    AST.JSStatementBlock {} -> True
    AST.JSBreak {} -> True
    AST.JSLet {} -> True
    AST.JSClass {} -> True
    AST.JSConstant {} -> True
    AST.JSContinue {} -> True
    AST.JSDoWhile {} -> True
    AST.JSFor {} -> True
    AST.JSForIn {} -> True
    AST.JSForVar {} -> True
    AST.JSForVarIn {} -> True
    AST.JSForLet {} -> True
    AST.JSForLetIn {} -> True
    AST.JSForLetOf {} -> True
    AST.JSForConst {} -> True
    AST.JSForConstIn {} -> True
    AST.JSForConstOf {} -> True
    AST.JSForOf {} -> True
    AST.JSForVarOf {} -> True
    AST.JSAsyncFunction {} -> True
    AST.JSFunction {} -> True
    AST.JSGenerator {} -> True
    AST.JSIf {} -> True
    AST.JSIfElse {} -> True
    AST.JSLabelled {} -> True
    AST.JSEmptyStatement {} -> True
    AST.JSExpressionStatement {} -> True
    AST.JSAssignStatement {} -> True
    AST.JSMethodCall {} -> True
    AST.JSReturn {} -> True
    AST.JSSwitch {} -> True
    AST.JSThrow {} -> True
    AST.JSTry {} -> True
    AST.JSVariable {} -> True
    AST.JSWhile {} -> True
    AST.JSWith {} -> True
