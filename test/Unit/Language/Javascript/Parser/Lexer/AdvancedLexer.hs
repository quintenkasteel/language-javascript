{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Advanced lexer feature testing for the JavaScript parser.
--
-- Tests sophisticated lexer capabilities including:
--
--   * Context-dependent regex vs division disambiguation
--   * Automatic Semicolon Insertion (ASI) comprehensive testing
--   * Multi-state lexer transition testing
--   * Lexer error recovery testing
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Parser.Lexer.AdvancedLexer
  ( testAdvancedLexer,
  )
where

import Data.Either (isLeft)
import Language.JavaScript.Parser.AST (JSAST)
import Language.JavaScript.Parser.Parser (parse, showStrippedMaybe)
import Test.Hspec

-- | Parse a JavaScript source string, returning the AST or an error.
testParse :: String -> Either String JSAST
testParse input = parse input "test"

-- | Convenience wrapper: parse and show the stripped AST representation.
testShow :: String -> String
testShow = showStrippedMaybe . testParse

-- | Main test suite for advanced lexer features.
testAdvancedLexer :: Spec
testAdvancedLexer = describe "Advanced Lexer Features" $ do
  testRegexDivisionDisambiguation
  testASIComprehensive
  testMultiStateLexerTransitions
  testLexerErrorRecovery

-- | Phase 1: Regex/Division disambiguation testing.
--
-- Tests context-dependent parsing where @\/@ can be either:
--
--   * Division operator in expression contexts
--   * Regular expression literal in regex contexts
testRegexDivisionDisambiguation :: Spec
testRegexDivisionDisambiguation = describe "Regex/Division Disambiguation" $ do
  it "parses /pattern/g as regex literal in var initializer" $
    testShow "var r = /pattern/g" `shouldBe`
      "Right (JSAstProgram [JSVariable (JSVarInitExpression (JSIdentifier 'r') [JSRegEx '/pattern/g'])])"

  it "parses 4 / 2 as division in var initializer" $
    testShow "var x = 4 / 2" `shouldBe`
      "Right (JSAstProgram [JSVariable (JSVarInitExpression (JSIdentifier 'x') [JSExpressionBinary ('/',JSDecimal '4',JSDecimal '2')])])"

  it "parses /regex/ as regex literal in conditional context" $
    testShow "if (true) /regex/" `shouldBe`
      "Right (JSAstProgram [JSIf (JSLiteral 'true') (JSRegEx '/regex/')])"

  it "parses y / z as division after identifier" $
    testShow "x = y / z" `shouldBe`
      "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSExpressionBinary ('/',JSIdentifier 'y',JSIdentifier 'z'))])"

-- | Phase 2: Comprehensive ASI testing.
--
-- Verifies that automatic semicolons are correctly inserted at line
-- boundaries for restricted productions like @return@ and @throw@.
testASIComprehensive :: Spec
testASIComprehensive = describe "Comprehensive ASI Testing" $ do
  it "inserts semicolon after return before newline" $
    testShow "return\n42" `shouldBe`
      "Right (JSAstProgram [JSReturn ,JSDecimal '42'])"

  it "does not insert semicolon after throw before newline" $
    testShow "throw\nnew Error()" `shouldBe`
      "Right (JSAstProgram [JSThrow (JSMemberNew (JSIdentifier 'Error',JSArguments ()))])"

  it "treats newline-separated expressions as separate statements in block" $
    testShow "{ 1\n2 }" `shouldBe`
      "Right (JSAstProgram [JSStatementBlock [JSDecimal '1',JSDecimal '2']])"

  it "parses newline before ++ as postfix on previous line" $
    testShow "a = b\n++c" `shouldBe`
      "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'a',JSExpressionPostfix ('++',JSIdentifier 'b')),JSIdentifier 'c'])"

-- | Phase 3: Multi-state lexer transitions.
--
-- Tests transitions between token contexts where the lexer must
-- correctly switch between regex and division modes.
testMultiStateLexerTransitions :: Spec
testMultiStateLexerTransitions = describe "Multi-state Lexer Transitions" $ do
  it "handles regex then division in same program" $
    testShow "var x = /regex/; x / 2" `shouldBe`
      "Right (JSAstProgram [JSVariable (JSVarInitExpression (JSIdentifier 'x') [JSRegEx '/regex/']),JSExpressionBinary ('/',JSIdentifier 'x',JSDecimal '2')])"

  it "handles regex in function return context" $
    testShow "function f() { return /re/ }" `shouldBe`
      "Right (JSAstProgram [JSFunction 'f' () (JSBlock [JSReturn JSRegEx '/re/' ])])"

  it "handles division after method call" $
    testShow "obj.method() / 2" `shouldBe`
      "Right (JSAstProgram [JSExpressionBinary ('/',JSMemberExpression (JSMemberDot (JSIdentifier 'obj',JSIdentifier 'method'),JSArguments ()),JSDecimal '2')])"

-- | Phase 4: Lexer error recovery.
--
-- Tests that malformed JavaScript input produces parse errors rather
-- than crashing or producing incorrect AST nodes.
testLexerErrorRecovery :: Spec
testLexerErrorRecovery = describe "Lexer Error Recovery" $ do
  it "produces error for @ character" $
    testParse "var x = @" `shouldSatisfy` isLeft

  it "lexes 0x as decimal zero followed by identifier x" $
    testShow "0x" `shouldBe`
      "Right (JSAstProgram [JSDecimal '0',JSIdentifier 'x'])"

  it "produces error for bare backslash" $
    testParse "\\" `shouldSatisfy` isLeft

  it "produces error for unterminated string" $
    testParse "\"unterminated" `shouldSatisfy` isLeft
