{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | ASI edge cases and line terminator handling tests.
--
-- Tests automatic semicolon insertion behavior across different line
-- terminator types (LF, CR, CRLF, Unicode line separator) within
-- single-line comments, and additional ASI edge cases for restricted
-- productions.
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Parser.Lexer.ASIHandling
  ( testASIEdgeCases,
  )
where

import Language.JavaScript.Parser.AST (JSAST)
import Language.JavaScript.Parser.Parser (parse, showStrippedMaybe)
import Test.Hspec

-- | Parse a JavaScript source string, returning the AST or an error.
testParse :: String -> Either String JSAST
testParse input = parse input "test"

-- | Convenience wrapper: parse and show the stripped AST representation.
testShow :: String -> String
testShow = showStrippedMaybe . testParse

-- | Comprehensive test suite for automatic semicolon insertion edge cases.
testASIEdgeCases :: Spec
testASIEdgeCases = describe "ASI Edge Cases and Error Conditions" $ do
  describe "different line terminator types" $ do
    testLFInComments
    testCRInComments
    testCRLFInComments
    testUnicodeLineSeparatorInComments

  describe "other ASI edge cases" $
    testAdditionalASIScenarios

-- | LF (\\n) in single-line comments: the comment absorbs the newline,
-- so @return@ does not trigger ASI and @4@ becomes the return value.
testLFInComments :: Spec
testLFInComments = it "handles LF (\\n) in comments" $
  testShow "return // comment\n4" `shouldBe`
    "Right (JSAstProgram [JSReturn JSDecimal '4' ])"

-- | CR (\\r) in single-line comments: behaves identically to LF.
-- The comment absorbs the carriage return, so @4@ is the return value.
testCRInComments :: Spec
testCRInComments = it "handles CR (\\r) in comments" $
  testShow "return // comment\r4" `shouldBe`
    "Right (JSAstProgram [JSReturn JSDecimal '4' ])"

-- | CRLF (\\r\\n) in single-line comments: behaves identically to LF.
-- The comment absorbs the line ending, so @4@ is the return value.
testCRLFInComments :: Spec
testCRLFInComments = it "handles CRLF (\\r\\n) in comments" $
  testShow "return // comment\r\n4" `shouldBe`
    "Right (JSAstProgram [JSReturn JSDecimal '4' ])"

-- | Unicode line separator (U+2028) in single-line comments: behaves
-- identically to LF. The comment absorbs the separator, so @4@ is
-- the return value.
testUnicodeLineSeparatorInComments :: Spec
testUnicodeLineSeparatorInComments = it "handles Unicode line separator (\\u2028) in comments" $
  testShow "return // comment\x2028\&4" `shouldBe`
    "Right (JSAstProgram [JSReturn JSDecimal '4' ])"

-- | Additional ASI scenarios covering expression statements, return
-- with values, and if/else across line boundaries.
testAdditionalASIScenarios :: Spec
testAdditionalASIScenarios = do
  it "splits identifiers across newlines into separate statements" $
    testShow "a\nb" `shouldBe`
      "Right (JSAstProgram [JSIdentifier 'a',JSIdentifier 'b'])"

  it "keeps return value when on same line before newline" $
    testShow "return a\nb" `shouldBe`
      "Right (JSAstProgram [JSReturn JSIdentifier 'a' ,JSIdentifier 'b'])"

  it "attaches else to preceding if across newline" $
    testShow "if (true) {}\n else {}" `shouldBe`
      "Right (JSAstProgram [JSIfElse (JSLiteral 'true') (JSStatementBlock []) (JSStatementBlock [])])"

  it "inserts ASI after return when followed by newline then literal" $
    testShow "return\n42" `shouldBe`
      "Right (JSAstProgram [JSReturn ,JSDecimal '42'])"

  it "does not insert ASI after throw (throw is not restricted)" $
    testShow "throw\nnew Error()" `shouldBe`
      "Right (JSAstProgram [JSThrow (JSMemberNew (JSIdentifier 'Error',JSArguments ()))])"
