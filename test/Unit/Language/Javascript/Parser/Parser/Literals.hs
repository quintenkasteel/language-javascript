{-# LANGUAGE OverloadedStrings #-}

module Unit.Language.Javascript.Parser.Parser.Literals
  ( testLiteralParser,
  )
where

import Control.Monad (forM_)
import Data.Char (chr, isPrint)
import Data.List (isInfixOf)
import Language.JavaScript.Parser
import Test.Hspec

testLiteralParser :: Spec
testLiteralParser = describe "Parse literals:" $ do
  it "null/true/false" $ do
    case testLiteral "null" of
      Right (JSAstLiteral (JSLiteral _ "null") _) -> pure ()
      result -> expectationFailure ("Expected null literal, got: " ++ show result)
    case testLiteral "false" of
      Right (JSAstLiteral (JSLiteral _ "false") _) -> pure ()
      result -> expectationFailure ("Expected false literal, got: " ++ show result)
    case testLiteral "true" of
      Right (JSAstLiteral (JSLiteral _ "true") _) -> pure ()
      result -> expectationFailure ("Expected true literal, got: " ++ show result)
  it "hex numbers" $ do
    case testLiteral "0x1234fF" of
      Right (JSAstLiteral (JSHexInteger _ 0x1234fF) _) -> pure ()
      result -> expectationFailure ("Expected hex integer 0x1234fF, got: " ++ show result)
    case testLiteral "0X1234fF" of
      Right (JSAstLiteral (JSHexInteger _ 0X1234fF) _) -> pure ()
      result -> expectationFailure ("Expected hex integer 0X1234fF, got: " ++ show result)
  it "binary numbers (ES2015)" $ do
    case testLiteral "0b1010" of
      Right (JSAstLiteral (JSBinaryInteger _ 10) _) -> pure ()
      result -> expectationFailure ("Expected binary integer 0b1010, got: " ++ show result)
    case testLiteral "0B1111" of
      Right (JSAstLiteral (JSBinaryInteger _ 15) _) -> pure ()
      result -> expectationFailure ("Expected binary integer 0B1111, got: " ++ show result)
    case testLiteral "0b0" of
      Right (JSAstLiteral (JSBinaryInteger _ 0) _) -> pure ()
      result -> expectationFailure ("Expected binary integer 0b0, got: " ++ show result)
    case testLiteral "0B101010" of
      Right (JSAstLiteral (JSBinaryInteger _ 42) _) -> pure ()
      result -> expectationFailure ("Expected binary integer 0B101010, got: " ++ show result)
  it "decimal numbers" $ do
    case testLiteral "1.0e4" of
      Right (JSAstLiteral (JSDecimal _ 1.0e4) _) -> pure ()
      result -> expectationFailure ("Expected decimal 1.0e4, got: " ++ show result)
    case testLiteral "2.3E6" of
      Right (JSAstLiteral (JSDecimal _ 2.3E6) _) -> pure ()
      result -> expectationFailure ("Expected decimal 2.3E6, got: " ++ show result)
    case testLiteral "4.5" of
      Right (JSAstLiteral (JSDecimal _ 4.5) _) -> pure ()
      result -> expectationFailure ("Expected decimal 4.5, got: " ++ show result)
    case testLiteral "0.7e8" of
      Right (JSAstLiteral (JSDecimal _ 0.7e8) _) -> pure ()
      result -> expectationFailure ("Expected decimal 0.7e8, got: " ++ show result)
    case testLiteral "0.7E8" of
      Right (JSAstLiteral (JSDecimal _ 0.7E8) _) -> pure ()
      result -> expectationFailure ("Expected decimal 0.7E8, got: " ++ show result)
    case testLiteral "10" of
      Right (JSAstLiteral (JSDecimal _ 10) _) -> pure ()
      result -> expectationFailure ("Expected decimal 10, got: " ++ show result)
    case testLiteral "0" of
      Right (JSAstLiteral (JSDecimal _ 0) _) -> pure ()
      result -> expectationFailure ("Expected decimal 0, got: " ++ show result)
    case testLiteral "0.03" of
      Right (JSAstLiteral (JSDecimal _ 0.03) _) -> pure ()
      result -> expectationFailure ("Expected decimal 0.03, got: " ++ show result)
    case testLiteral "0.7e+8" of
      Right (JSAstLiteral (JSDecimal _ 0.7e+8) _) -> pure ()
      result -> expectationFailure ("Expected decimal 0.7e+8, got: " ++ show result)
    case testLiteral "0.7e-18" of
      Right (JSAstLiteral (JSDecimal _ 0.7e-18) _) -> pure ()
      result -> expectationFailure ("Expected decimal 0.7e-18, got: " ++ show result)
    case testLiteral "1.0e+4" of
      Right (JSAstLiteral (JSDecimal _ 1.0e+4) _) -> pure ()
      result -> expectationFailure ("Expected decimal 1.0e+4, got: " ++ show result)
    case testLiteral "1.0e-4" of
      Right (JSAstLiteral (JSDecimal _ 1.0e-4) _) -> pure ()
      result -> expectationFailure ("Expected decimal 1.0e-4, got: " ++ show result)
    case testLiteral "1e18" of
      Right (JSAstLiteral (JSDecimal _ 1e18) _) -> pure ()
      result -> expectationFailure ("Expected decimal 1e18, got: " ++ show result)
    case testLiteral "1e+18" of
      Right (JSAstLiteral (JSDecimal _ 1e+18) _) -> pure ()
      result -> expectationFailure ("Expected decimal 1e+18, got: " ++ show result)
    case testLiteral "1e-18" of
      Right (JSAstLiteral (JSDecimal _ 1e-18) _) -> pure ()
      result -> expectationFailure ("Expected decimal 1e-18, got: " ++ show result)
    case testLiteral "1E-01" of
      Right (JSAstLiteral (JSDecimal _ 1E-01) _) -> pure ()
      result -> expectationFailure ("Expected decimal 1E-01, got: " ++ show result)
  it "octal numbers" $ do
    case testLiteral "070" of
      Right (JSAstLiteral (JSOctal _ 0o70) _) -> pure ()
      result -> expectationFailure ("Expected octal 070, got: " ++ show result)
    case testLiteral "010234567" of
      Right (JSAstLiteral (JSOctal _ 0o10234567) _) -> pure ()
      result -> expectationFailure ("Expected octal 010234567, got: " ++ show result)
    -- Modern octal syntax (ES2015)
    case testLiteral "0o777" of
      Right (JSAstLiteral (JSOctal _ 0o777) _) -> pure ()
      result -> expectationFailure ("Expected octal 0o777, got: " ++ show result)
    case testLiteral "0O123" of
      Right (JSAstLiteral (JSOctal _ 0O123) _) -> pure ()
      result -> expectationFailure ("Expected octal 0O123, got: " ++ show result)
    case testLiteral "0o0" of
      Right (JSAstLiteral (JSOctal _ 0o0) _) -> pure ()
      result -> expectationFailure ("Expected octal 0o0, got: " ++ show result)
  it "bigint numbers" $ do
    case testLiteral "123n" of
      Right (JSAstLiteral (JSBigIntLiteral _ 123) _) -> pure ()
      result -> expectationFailure ("Expected bigint 123n, got: " ++ show result)
    case testLiteral "0n" of
      Right (JSAstLiteral (JSBigIntLiteral _ 0) _) -> pure ()
      result -> expectationFailure ("Expected bigint 0n, got: " ++ show result)
    case testLiteral "9007199254740991n" of
      Right (JSAstLiteral (JSBigIntLiteral _ 9007199254740991) _) -> pure ()
      result -> expectationFailure ("Expected bigint 9007199254740991n, got: " ++ show result)
    case testLiteral "0x1234n" of
      Right (JSAstLiteral (JSBigIntLiteral _ 0x1234) _) -> pure ()
      result -> expectationFailure ("Expected bigint 0x1234n, got: " ++ show result)
    case testLiteral "0X1234n" of
      Right (JSAstLiteral (JSBigIntLiteral _ 0X1234) _) -> pure ()
      result -> expectationFailure ("Expected bigint 0X1234n, got: " ++ show result)
    case testLiteral "0b1010n" of
      Right (JSAstLiteral (JSBigIntLiteral _ 10) _) -> pure ()
      result -> expectationFailure ("Expected bigint 0b1010n, got: " ++ show result)
    case testLiteral "0B1111n" of
      Right (JSAstLiteral (JSBigIntLiteral _ 15) _) -> pure ()
      result -> expectationFailure ("Expected bigint 0B1111n, got: " ++ show result)
    case testLiteral "0o777n" of
      Right (JSAstLiteral (JSBigIntLiteral _ 0o777) _) -> pure ()
      result -> expectationFailure ("Expected bigint 0o777n, got: " ++ show result)

  it "numeric separators (ES2021) - ES2021 compliant behavior" $ do
    -- Note: Parser now correctly supports ES2021 numeric separators as single tokens
    -- These tests verify ES2021-compliant parsing behavior
    case parse "1_000" "test" of
      Right (JSAstProgram [JSExpressionStatement (JSDecimal _ 1000) _] _) -> pure ()
      Left err -> expectationFailure ("Expected parse to succeed for 1_000, got: " ++ show err)
      Right result -> expectationFailure ("Expected JSDecimal 1000 for 1_000, got: " ++ show result)
    case parse "1_000_000" "test" of
      Right (JSAstProgram [JSExpressionStatement (JSDecimal _ 1000000) _] _) -> pure ()
      Left err -> expectationFailure ("Expected parse to succeed for 1_000_000, got: " ++ show err)
      Right result -> expectationFailure ("Expected JSDecimal 1000000 for 1_000_000, got: " ++ show result)
    case parse "0xFF_EC_DE" "test" of
      Right (JSAstProgram [JSExpressionStatement (JSHexInteger _ 0xFFECDE) _] _) -> pure ()
      Left err -> expectationFailure ("Expected parse to succeed for 0xFF_EC_DE, got: " ++ show err)
      Right result -> expectationFailure ("Expected JSHexInteger for 0xFF_EC_DE, got: " ++ show result)
    case parse "3.14_15" "test" of
      Right (JSAstProgram [JSExpressionStatement (JSDecimal _ 3.1415) _] _) -> pure ()
      Left err -> expectationFailure ("Expected parse to succeed for 3.14_15, got: " ++ show err)
      Right result -> expectationFailure ("Expected JSDecimal 3.1415 for 3.14_15, got: " ++ show result)
    case parse "123_456n" "test" of
      Right (JSAstProgram [JSExpressionStatement (JSBigIntLiteral _ 123456) _] _) -> pure ()
      Left err -> expectationFailure ("Expected parse to succeed for 123_456n, got: " ++ show err)
      Right result -> expectationFailure ("Expected JSBigIntLiteral 123456 for 123_456n, got: " ++ show result)
    case testLiteral "077n" of
      Right (JSAstLiteral (JSBigIntLiteral _ 077) _) -> pure ()
      result -> expectationFailure ("Expected bigint 077n, got: " ++ show result)
  it "strings" $ do
    case testLiteral "'cat'" of
      Right (JSAstLiteral (JSStringLiteral _ "'cat'") _) -> pure ()
      result -> expectationFailure ("Expected string 'cat', got: " ++ show result)
    case testLiteral "\"cat\"" of
      Right (JSAstLiteral (JSStringLiteral _ "\"cat\"") _) -> pure ()
      result -> expectationFailure ("Expected string \"cat\", got: " ++ show result)
    case testLiteral "'\\u1234'" of
      Right (JSAstLiteral (JSStringLiteral _ "'\\u1234'") _) -> pure ()
      result -> expectationFailure ("Expected string '\\u1234', got: " ++ show result)
    case testLiteral "'\\uabcd'" of
      Right (JSAstLiteral (JSStringLiteral _ "'\\uabcd'") _) -> pure ()
      result -> expectationFailure ("Expected string '\\uabcd', got: " ++ show result)
    case testLiteral "\"\\r\\n\"" of
      Right (JSAstLiteral (JSStringLiteral _ "\"\\r\\n\"") _) -> pure ()
      result -> expectationFailure ("Expected string \"\\r\\n\", got: " ++ show result)
    case testLiteral "\"\\b\"" of
      Right (JSAstLiteral (JSStringLiteral _ "\"\\b\"") _) -> pure ()
      result -> expectationFailure ("Expected string \"\\b\", got: " ++ show result)
    case testLiteral "\"\\f\"" of
      Right (JSAstLiteral (JSStringLiteral _ "\"\\f\"") _) -> pure ()
      result -> expectationFailure ("Expected string \"\\f\", got: " ++ show result)
    case testLiteral "\"\\t\"" of
      Right (JSAstLiteral (JSStringLiteral _ "\"\\t\"") _) -> pure ()
      result -> expectationFailure ("Expected string \"\\t\", got: " ++ show result)
    case testLiteral "\"\\v\"" of
      Right (JSAstLiteral (JSStringLiteral _ "\"\\v\"") _) -> pure ()
      result -> expectationFailure ("Expected string \"\\v\", got: " ++ show result)
    case testLiteral "\"\\0\"" of
      Right (JSAstLiteral (JSStringLiteral _ "\"\\0\"") _) -> pure ()
      result -> expectationFailure ("Expected string \"\\0\", got: " ++ show result)
    case testLiteral "\"hello\\nworld\"" of
      Right (JSAstLiteral (JSStringLiteral _ "\"hello\\nworld\"") _) -> pure ()
      result -> expectationFailure ("Expected string \"hello\\nworld\", got: " ++ show result)
    case testLiteral "'hello\\nworld'" of
      Right (JSAstLiteral (JSStringLiteral _ "'hello\\nworld'") _) -> pure ()
      result -> expectationFailure ("Expected string 'hello\\nworld', got: " ++ show result)

    case testLiteral "'char \n'" of
      Left err -> err `shouldSatisfy` ("lexical error" `isInfixOf`)
      result -> expectationFailure ("Expected parse error for invalid string, got: " ++ show result)

    forM_ (mkTestStrings SingleQuote) $ \str ->
      case testLiteral str of
        Right (JSAstLiteral (JSStringLiteral _ _) _) -> pure ()
        result -> expectationFailure ("Expected string literal for " ++ str ++ ", got: " ++ show result)

    forM_ (mkTestStrings DoubleQuote) $ \str ->
      case testLiteral str of
        Right (JSAstLiteral (JSStringLiteral _ _) _) -> pure ()
        result -> expectationFailure ("Expected string literal for " ++ str ++ ", got: " ++ show result)

  it "strings with escaped quotes" $ do
    case testLiteral "'\"'" of
      Right (JSAstLiteral (JSStringLiteral _ "'\"'") _) -> pure ()
      result -> expectationFailure ("Expected string '\"', got: " ++ show result)
    case testLiteral "\"\\\"\"" of
      Right (JSAstLiteral (JSStringLiteral _ "\"\\\"\"") _) -> pure ()
      result -> expectationFailure ("Expected string \"\\\"\", got: " ++ show result)

data Quote
  = SingleQuote
  | DoubleQuote
  deriving (Eq)

mkTestStrings :: Quote -> [String]
mkTestStrings quote =
  map mkString [0 .. 255]
  where
    mkString :: Int -> String
    mkString i =
      quoteString $ "char #" ++ show i ++ " " ++ showCh i

    showCh :: Int -> String
    showCh ch
      | ch == 34 = if quote == DoubleQuote then "\\\"" else "\""
      | ch == 39 = if quote == SingleQuote then "\\\'" else "'"
      | ch == 92 = "\\\\"
      | ch < 127 && isPrint (chr ch) = [chr ch]
      | otherwise =
        let str = "000" ++ show ch
            slen = length str
         in "\\" ++ drop (slen - 3) str

    quoteString s =
      if quote == SingleQuote
        then '\'' : (s ++ "'")
        else '"' : (s ++ ['"'])

-- | Parse a string as a literal expression, unwrapping the program wrapper.
-- Returns the expression wrapped in JSAstLiteral for pattern matching convenience.
testLiteral :: String -> Either String JSAST
testLiteral str =
  case parse str "src" of
    Right (JSAstProgram [JSExpressionStatement expr _] annot) ->
      Right (JSAstLiteral expr annot)
    Right other -> Right other
    Left err -> Left err
