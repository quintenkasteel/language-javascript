{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | String literal complexity testing for JavaScript parser.
--
-- Tests JavaScript string literal parsing across escape sequences, unicode
-- handling, template literals, and edge cases. Organized into three phases:
--
--   * Phase 1: Extended string literal tests (escape sequences, unicode, cross-quotes, errors)
--   * Phase 2: Template literal tests (interpolation, nesting, escapes, tagged)
--   * Phase 3: Edge cases (long strings, complex escapes, boundary conditions)
--
-- @since 0.7.1.0
module Unit.Language.Javascript.Parser.Lexer.StringLiterals
  ( testStringLiteralComplexity,
  )
where

import Data.Either (isLeft, isRight)
import Language.JavaScript.Parser.AST (JSAST)
import Language.JavaScript.Parser.Parser (parse, showStrippedMaybe)
import Test.Hspec

-- | Parse a JavaScript source string, returning the AST or an error.
testParse :: String -> Either String JSAST
testParse input = parse input "test"

-- | Main test suite for string literal complexity.
testStringLiteralComplexity :: Spec
testStringLiteralComplexity = describe "String Literal Complexity" $ do
  describe "Extended string literal tests" $ do
    escapeSequenceTests
    unicodeHandlingTests
    crossQuoteTests
    errorCaseTests
  describe "Template literal comprehensive tests" $ do
    interpolationTests
    nestingTests
    templateEscapeTests
    taggedTemplateTests
  describe "Edge cases and performance" $ do
    longStringTests
    complexEscapeTests
    boundaryConditionTests

-- | Phase 1: Verify common escape sequences in single-quoted strings.
escapeSequenceTests :: Spec
escapeSequenceTests = it "escape sequences" $ do
  testShow "'hello\\nworld'" `shouldBe` "Right (JSAstProgram [JSStringLiteral 'hello\\nworld'])"
  testShow "'tab\\there'" `shouldBe` "Right (JSAstProgram [JSStringLiteral 'tab\\there'])"
  testShow "'null\\0char'" `shouldBe` "Right (JSAstProgram [JSStringLiteral 'null\\0char'])"
  testShow "'backslash\\\\path'" `shouldBe` "Right (JSAstProgram [JSStringLiteral 'backslash\\\\path'])"
  testShow "'\\'s'" `shouldBe` "Right (JSAstProgram [JSStringLiteral '\\'s'])"

-- | Phase 1: Verify unicode escape sequences in strings.
unicodeHandlingTests :: Spec
unicodeHandlingTests = it "unicode handling" $ do
  testShow "'\\u0041'" `shouldBe` "Right (JSAstProgram [JSStringLiteral '\\u0041'])"
  testShow "'\\u{1F600}'" `shouldBe` "Right (JSAstProgram [JSStringLiteral '\\u{1F600}'])"

-- | Phase 1: Verify strings containing the opposite quote type.
crossQuoteTests :: Spec
crossQuoteTests = it "cross-quotes" $ do
  testShow "'He said \"hello\"'" `shouldBe` "Right (JSAstProgram [JSStringLiteral 'He said \"hello\"'])"
  testShow "\"She said 'hi'\"" `shouldBe` "Right (JSAstProgram [JSStringLiteral \"She said 'hi'\"])"

-- | Phase 1: Verify that malformed strings produce parse errors.
errorCaseTests :: Spec
errorCaseTests = it "error cases" $
  testParse "'unterminated" `shouldSatisfy` isLeft

-- | Phase 2: Verify template literal interpolation parsing.
interpolationTests :: Spec
interpolationTests = it "interpolation" $
  testShow "`hello ${name}`" `shouldBe`
    "Right (JSAstProgram [JSTemplateLiteral ((),'`hello ${'," <>
    "[(JSIdentifier 'name','}`')])])"

-- | Phase 2: Verify nested template literals parse correctly.
nestingTests :: Spec
nestingTests = it "nesting" $
  testShow "`outer ${`inner`}`" `shouldBe`
    "Right (JSAstProgram [JSTemplateLiteral ((),'`outer ${'," <>
    "[(JSTemplateLiteral ((),'`inner`',[]),'}`')])])"

-- | Phase 2: Verify escape sequences inside template literals.
templateEscapeTests :: Spec
templateEscapeTests = it "escapes" $
  testShow "`line1\\nline2`" `shouldBe`
    "Right (JSAstProgram [JSTemplateLiteral ((),'`line1\\nline2`',[])])"

-- | Phase 2: Verify tagged template literal parsing.
taggedTemplateTests :: Spec
taggedTemplateTests = it "tagged templates" $
  testShow "html`<div></div>`" `shouldBe`
    "Right (JSAstProgram [JSTemplateLiteral ((JSIdentifier 'html'),'`<div></div>`',[])])"

-- | Phase 3: Verify parsing of a string with 1000 characters.
longStringTests :: Spec
longStringTests = it "long strings" $
  testParse longInput `shouldSatisfy` isRight
  where
    longInput = "'" <> replicate 1000 'a' <> "'"

-- | Phase 3: Verify multiple consecutive escape sequences.
complexEscapeTests :: Spec
complexEscapeTests = it "complex escapes" $
  testShow "'\\n\\t\\r\\0'" `shouldBe`
    "Right (JSAstProgram [JSStringLiteral '\\n\\t\\r\\0'])"

-- | Phase 3: Verify empty string edge cases.
boundaryConditionTests :: Spec
boundaryConditionTests = it "boundary conditions" $ do
  testShow "''" `shouldBe` "Right (JSAstProgram [JSStringLiteral ''])"
  testShow "\"\"" `shouldBe` "Right (JSAstProgram [JSStringLiteral \"\"])"

-- | Convenience wrapper: parse and show the stripped AST representation.
testShow :: String -> String
testShow = showStrippedMaybe . testParse
