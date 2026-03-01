{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Basic lexer tests exercised through the public parser API.
--
-- These tests verify that the flatparse-based lexer correctly handles
-- fundamental JavaScript token categories: comments, numbers, strings,
-- escape sequences, spread, assignment, keywords, BigInt, optional
-- chaining, nullish coalescing, and ASI with comments.
--
-- All tests use 'Language.JavaScript.Parser.Parser.parse' to parse
-- complete programs and then pattern-match on the resulting AST
-- to confirm the lexer produced correct tokens.
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Parser.Lexer.BasicLexer
  ( testLexer,
  )
where

import Data.ByteString (ByteString)
import Data.Either (isRight)
import Language.JavaScript.Parser.AST
  ( JSAnnot (..),
    JSArrayElement (..),
    JSAST (..),
    JSAssignOp (..),
    JSBinOp (..),
    JSExpression (..),
    JSIdent (..),
    JSSemi (..),
    JSStatement (..),
  )
import Language.JavaScript.Parser.Parser (parse)
import Test.Hspec

-- | Parse a JavaScript source string through the public API.
testParse :: String -> Either String JSAST
testParse input = parse input "test"

-- | Main test suite for basic lexer functionality.
testLexer :: Spec
testLexer = describe "Lexer:" $ do
  commentTests
  numberTests
  stringTests
  escapeCharTests
  spreadTokenTests
  assignmentTests
  keywordTests
  bigIntTests
  optionalChainingTests
  nullishCoalescingTests
  asiWithCommentTests

-- | Verify that line and block comments do not affect parsing.
commentTests :: Spec
commentTests = it "comments" $ do
  assertDecimalProgram "// line comment\n42" 42
  assertDecimalProgram "/* block comment */42" 42

-- | Verify numeric literal parsing for decimal and hex integers.
numberTests :: Spec
numberTests = it "numbers" $ do
  assertDecimalProgram "123" 123
  assertHexProgram "0xab" 0xab
  assertHexProgram "0xCD" 0xCD

-- | Verify single-quoted and double-quoted string literal parsing.
stringTests :: Spec
stringTests = it "string" $ do
  assertStringProgram "'cat'" "'cat'"
  assertStringProgram "\"dog\"" "\"dog\""

-- | Verify escape sequences within string literals parse successfully.
escapeCharTests :: Spec
escapeCharTests = it "strings with escape chars" $ do
  testParse "'\\n'" `shouldSatisfy` isRight
  testParse "'\\\\'" `shouldSatisfy` isRight
  testParse "'\\0'" `shouldSatisfy` isRight

-- | Verify the spread operator is parsed inside array literals.
spreadTokenTests :: Spec
spreadTokenTests = it "spread token" $
  case testParse "[...a]" of
    Right (JSAstProgram [JSExpressionStatement (JSArrayLiteral _ [JSArrayElement (JSSpreadExpression _ (JSIdentifier _ "a"))] _) _] _) -> pure ()
    result -> expectationFailure ("Expected array with spread expression, got: " ++ show result)

-- | Verify simple assignment parsing.
assignmentTests :: Spec
assignmentTests = it "assignment" $
  case testParse "x=1" of
    Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSDecimal _ 1.0) _] _) -> pure ()
    result -> expectationFailure ("Expected assignment x=1, got: " ++ show result)

-- | Verify keyword-triggered ASI produces break followed by assignment.
keywordTests :: Spec
keywordTests = it "break/keyword ASI" $
  case testParse "break\nx=1" of
    Right (JSAstProgram (JSBreak {} : _) _) -> pure ()
    result -> expectationFailure ("Expected break statement first, got: " ++ show result)

-- | Verify BigInt literal parsing.
bigIntTests :: Spec
bigIntTests = it "bigint literals" $
  case testParse "123n" of
    Right (JSAstProgram [JSExpressionStatement (JSBigIntLiteral _ 123) _] _) -> pure ()
    result -> expectationFailure ("Expected BigInt literal 123n, got: " ++ show result)

-- | Verify optional chaining operator parses as optional member dot.
optionalChainingTests :: Spec
optionalChainingTests = it "optional chaining" $
  case testParse "obj?.prop" of
    Right (JSAstProgram [JSExpressionStatement (JSOptionalMemberDot (JSIdentifier _ "obj") _ (JSIdentifier _ "prop")) _] _) -> pure ()
    result -> expectationFailure ("Expected optional chaining obj?.prop, got: " ++ show result)

-- | Verify nullish coalescing operator parses as binary expression.
nullishCoalescingTests :: Spec
nullishCoalescingTests = it "nullish coalescing" $
  case testParse "x ?? y" of
    Right (JSAstProgram [JSExpressionStatement (JSExpressionBinary (JSIdentifier _ "x") (JSBinOpNullishCoalescing _) (JSIdentifier _ "y")) _] _) -> pure ()
    result -> expectationFailure ("Expected nullish coalescing x ?? y, got: " ++ show result)

-- | Verify ASI interacts correctly with comments after return.
asiWithCommentTests :: Spec
asiWithCommentTests = it "automatic semicolon insertion with comments" $
  case testParse "return // comment\n4" of
    Right (JSAstProgram (JSReturn {} : _) _) -> pure ()
    result -> expectationFailure ("Expected return statement followed by expression, got: " ++ show result)

-- | Assert that input parses as a program with a single decimal expression.
assertDecimalProgram :: String -> Double -> IO ()
assertDecimalProgram input expected =
  case testParse input of
    Right (JSAstProgram [JSExpressionStatement (JSDecimal _ val) _] _)
      | val == expected -> pure ()
    result -> expectationFailure ("Expected JSDecimal " ++ show expected ++ " for input " ++ show input ++ ", got: " ++ show result)

-- | Assert that input parses as a program with a single hex integer expression.
assertHexProgram :: String -> Integer -> IO ()
assertHexProgram input expected =
  case testParse input of
    Right (JSAstProgram [JSExpressionStatement (JSHexInteger _ val) _] _)
      | val == expected -> pure ()
    result -> expectationFailure ("Expected JSHexInteger " ++ show expected ++ " for input " ++ show input ++ ", got: " ++ show result)

-- | Assert that input parses as a program with a single string literal.
assertStringProgram :: String -> ByteString -> IO ()
assertStringProgram input expected =
  case testParse input of
    Right (JSAstProgram [JSExpressionStatement (JSStringLiteral _ val) _] _)
      | val == expected -> pure ()
    result -> expectationFailure ("Expected JSStringLiteral " ++ show expected ++ " for input " ++ show input ++ ", got: " ++ show result)
