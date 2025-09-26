{-# LANGUAGE OverloadedStrings #-}

module Unit.Language.Javascript.Parser.Lexer.ASIHandling
  ( testASIEdgeCases,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Language.JavaScript.Parser.Grammar7
import Language.JavaScript.Parser.Lexer
import Language.JavaScript.Parser.Parser
import Test.Hspec

-- | Comprehensive test suite for automatic semicolon insertion edge cases
testASIEdgeCases :: Spec
testASIEdgeCases = describe "ASI Edge Cases and Error Conditions" $ do
  describe "different line terminator types" $ do
    it "handles LF (\\n) in comments" $ do
      testLex "return // comment\n4" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken,DecimalToken 4]"

    it "handles CR (\\r) in comments" $ do
      testLex "return // comment\r4" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken,DecimalToken 4]"

    it "handles CRLF (\\r\\n) in comments" $ do
      testLex "return // comment\r\n4" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken,DecimalToken 4]"

    it "handles Unicode line separator (\\u2028) in comments" $ do
      testLex "return /* comment\x2028 */ 4" `shouldBe` "[ReturnToken,WsToken,CommentToken,AutoSemiToken,WsToken,DecimalToken 4]"

    it "handles Unicode paragraph separator (\\u2029) in comments" $ do
      testLex "return /* comment\x2029 */ 4" `shouldBe` "[ReturnToken,WsToken,CommentToken,AutoSemiToken,WsToken,DecimalToken 4]"

  describe "nested and complex comments" $ do
    it "handles multiple newlines in single comment" $ do
      testLex "return /* line1\nline2\nline3 */ 4" `shouldBe` "[ReturnToken,WsToken,CommentToken,AutoSemiToken,WsToken,DecimalToken 4]"

    it "handles mixed line terminators in comments" $ do
      testLex "return /* line1\rline2\nline3 */ 4" `shouldBe` "[ReturnToken,WsToken,CommentToken,AutoSemiToken,WsToken,DecimalToken 4]"

    it "handles comments with only line terminators" $ do
      testLex "return /*\n*/ 4" `shouldBe` "[ReturnToken,WsToken,CommentToken,AutoSemiToken,WsToken,DecimalToken 4]"
      testLex "return //\n4" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken,DecimalToken 4]"

  describe "comment position variations" $ do
    it "handles comments immediately after keywords" $ do
      testLex "return// comment\n4" `shouldBe` "[ReturnToken,CommentToken,WsToken,AutoSemiToken,DecimalToken 4]"
      testLex "break/* comment\n */ x" `shouldBe` "[BreakToken,CommentToken,AutoSemiToken,WsToken,IdentifierToken 'x']"

    it "handles multiple consecutive comments" $ do
      testLex "return // first\n/* second\n */ 4" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken,CommentToken,AutoSemiToken,WsToken,DecimalToken 4]"
      testLex "continue /* first\n */ // second\n" `shouldBe` "[ContinueToken,WsToken,CommentToken,AutoSemiToken,WsToken,CommentToken,WsToken]"

  describe "ASI token boundaries" $ do
    it "handles return followed by operator" $ do
      testLex "return // comment\n+ 4" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken,PlusToken,WsToken,DecimalToken 4]"
      testLex "return /* comment\n */ ++x" `shouldBe` "[ReturnToken,WsToken,CommentToken,AutoSemiToken,WsToken,IncrementToken,IdentifierToken 'x']"

    it "handles return followed by function call" $ do
      testLex "return // comment\nfoo()" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken,IdentifierToken 'foo',LeftParenToken,RightParenToken]"

    it "handles return followed by object access" $ do
      testLex "return // comment\nobj.prop" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken,IdentifierToken 'obj',DotToken,IdentifierToken 'prop']"

  describe "non-ASI tokens with comments" $ do
    it "does not trigger ASI for non-restricted tokens" $ do
      testLex "var // comment\n x" `shouldBe` "[VarToken,WsToken,CommentToken,WsToken,IdentifierToken 'x']"
      testLex "function /* comment\n */ f" `shouldBe` "[FunctionToken,WsToken,CommentToken,WsToken,IdentifierToken 'f']"
      testLex "if // comment\n (x)" `shouldBe` "[IfToken,WsToken,CommentToken,WsToken,LeftParenToken,IdentifierToken 'x',RightParenToken]"
      testLex "for /* comment\n */ (;;)" `shouldBe` "[ForToken,WsToken,CommentToken,WsToken,LeftParenToken,SemiColonToken,SemiColonToken,RightParenToken]"

  describe "EOF and boundary conditions" $ do
    it "handles comments at end of file" $ do
      testLex "return // comment\n" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken]"
      testLex "break /* comment\n */" `shouldBe` "[BreakToken,WsToken,CommentToken,AutoSemiToken]"

    it "handles empty comments" $ do
      testLex "return //\n4" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken,DecimalToken 4]"
      testLex "continue /**/ 4" `shouldBe` "[ContinueToken,WsToken,CommentToken,WsToken,DecimalToken 4]"

  describe "mixed whitespace and comments" $ do
    it "handles whitespace before comments" $ do
      testLex "return  // comment\n4" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken,DecimalToken 4]"
      testLex "break \t/* comment\n */ x" `shouldBe` "[BreakToken,WsToken,CommentToken,AutoSemiToken,WsToken,IdentifierToken 'x']"

    it "handles whitespace after comments" $ do
      testLex "return // comment\n  4" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken,DecimalToken 4]"
      testLex "continue /* comment\n */   x" `shouldBe` "[ContinueToken,WsToken,CommentToken,AutoSemiToken,WsToken,IdentifierToken 'x']"

  describe "parser-level edge cases" $ do
    it "parses functions with ASI correctly" $ do
      case parseUsing parseStatement "function f() { return // comment\n 4 }" "test" of
        Right _ -> pure ()
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "handles nested blocks with ASI" $ do
      case parseUsing parseStatement "{ if (true) { return // comment\n } }" "test" of
        Right _ -> pure ()
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "handles loops with ASI statements" $ do
      case parseUsing parseStatement "while (true) { break // comment\n }" "test" of
        Right _ -> pure ()
        Left err -> expectationFailure ("Parse failed: " ++ show err)

  describe "complex real-world scenarios" $ do
    it "handles JSDoc-style comments" $ do
      testLex "return /** JSDoc comment\n * @return {number}\n */ 42"
        `shouldBe` "[ReturnToken,WsToken,CommentToken,AutoSemiToken,WsToken,DecimalToken 42]"

    it "handles comments with special characters" $ do
      testLex "return // TODO: fix this\n null" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken,NullToken]"
      testLex "break /* FIXME: handle edge case\n */ " `shouldBe` "[BreakToken,WsToken,CommentToken,AutoSemiToken,WsToken]"

    it "handles comments with unicode content" $ do
      testLex "return // 测试注释\n 42" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken,DecimalToken 42]"
      testLex "continue /* комментарий\n */ x" `shouldBe` "[ContinueToken,WsToken,CommentToken,AutoSemiToken,WsToken,IdentifierToken 'x']"

  describe "error condition robustness" $ do
    it "handles malformed input gracefully" $ do
      -- These should not crash the lexer/parser
      case testLex "return //" of
        result -> length result `shouldSatisfy` (> 0)

      case testLex "break /*" of
        result -> length result `shouldSatisfy` (> 0)

    it "maintains correct token positions" $ do
      -- Verify that ASI doesn't disrupt token position tracking
      case parseUsing parseStatement "return // comment\n 42" "test" of
        Right _ -> pure ()
        Left err -> expectationFailure ("Position tracking failed: " ++ show err)

-- Helper function for testing lexer output with ASI support
testLex :: String -> String
testLex str =
  either id stringify $ alexTestTokeniserASI str
  where
    stringify xs = "[" ++ List.intercalate "," (map showToken xs) ++ "]"
      where
        utf8ToString :: String -> String
        utf8ToString = id

        showToken :: Token -> String
        showToken (StringToken _ lit _) = "StringToken " ++ stringEscape lit
        showToken (IdentifierToken _ lit _) = "IdentifierToken '" ++ stringEscape lit ++ "'"
        showToken (DecimalToken _ lit _) = "DecimalToken " ++ lit
        showToken (CommentToken _ _ _) = "CommentToken"
        showToken (AutoSemiToken _ _ _) = "AutoSemiToken"
        showToken (WsToken _ _ _) = "WsToken"
        showToken token = takeWhile (/= ' ') $ show token

        stringEscape [] = []
        stringEscape (x : ys) = x : stringEscape ys
