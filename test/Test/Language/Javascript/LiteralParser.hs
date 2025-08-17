module Test.Language.Javascript.LiteralParser
    ( testLiteralParser
    ) where

import Test.Hspec

import Control.Monad (forM_)
import Data.Char (chr, isPrint)

import Language.JavaScript.Parser
import Language.JavaScript.Parser.Grammar7
import Language.JavaScript.Parser.Parser


testLiteralParser :: Spec
testLiteralParser = describe "Parse literals:" $ do
    it "null/true/false" $ do
        testLiteral "null"  `shouldBe` "Right (JSAstLiteral (JSLiteral 'null'))"
        testLiteral "false" `shouldBe` "Right (JSAstLiteral (JSLiteral 'false'))"
        testLiteral "true"  `shouldBe` "Right (JSAstLiteral (JSLiteral 'true'))"
    it "hex numbers" $ do
        testLiteral "0x1234fF"  `shouldBe` "Right (JSAstLiteral (JSHexInteger '0x1234fF'))"
        testLiteral "0X1234fF"  `shouldBe` "Right (JSAstLiteral (JSHexInteger '0X1234fF'))"
    it "decimal numbers" $ do
        testLiteral "1.0e4"     `shouldBe` "Right (JSAstLiteral (JSDecimal '1.0e4'))"
        testLiteral "2.3E6"     `shouldBe` "Right (JSAstLiteral (JSDecimal '2.3E6'))"
        testLiteral "4.5"       `shouldBe` "Right (JSAstLiteral (JSDecimal '4.5'))"
        testLiteral "0.7e8"     `shouldBe` "Right (JSAstLiteral (JSDecimal '0.7e8'))"
        testLiteral "0.7E8"     `shouldBe` "Right (JSAstLiteral (JSDecimal '0.7E8'))"
        testLiteral "10"        `shouldBe` "Right (JSAstLiteral (JSDecimal '10'))"
        testLiteral "0"         `shouldBe` "Right (JSAstLiteral (JSDecimal '0'))"
        testLiteral "0.03"      `shouldBe` "Right (JSAstLiteral (JSDecimal '0.03'))"
        testLiteral "0.7e+8"    `shouldBe` "Right (JSAstLiteral (JSDecimal '0.7e+8'))"
        testLiteral "0.7e-18"   `shouldBe` "Right (JSAstLiteral (JSDecimal '0.7e-18'))"
        testLiteral "1.0e+4"    `shouldBe` "Right (JSAstLiteral (JSDecimal '1.0e+4'))"
        testLiteral "1.0e-4"    `shouldBe` "Right (JSAstLiteral (JSDecimal '1.0e-4'))"
        testLiteral "1e18"      `shouldBe` "Right (JSAstLiteral (JSDecimal '1e18'))"
        testLiteral "1e+18"     `shouldBe` "Right (JSAstLiteral (JSDecimal '1e+18'))"
        testLiteral "1e-18"     `shouldBe` "Right (JSAstLiteral (JSDecimal '1e-18'))"
        testLiteral "1E-01"     `shouldBe` "Right (JSAstLiteral (JSDecimal '1E-01'))"
    it "octal numbers" $ do
        testLiteral "070"       `shouldBe` "Right (JSAstLiteral (JSOctal '070'))"
        testLiteral "010234567" `shouldBe` "Right (JSAstLiteral (JSOctal '010234567'))"
    it "bigint numbers" $ do
        testLiteral "123n"      `shouldBe` "Right (JSAstLiteral (JSBigIntLiteral '123n'))"
        testLiteral "0n"        `shouldBe` "Right (JSAstLiteral (JSBigIntLiteral '0n'))"
        testLiteral "9007199254740991n" `shouldBe` "Right (JSAstLiteral (JSBigIntLiteral '9007199254740991n'))"
        testLiteral "0x1234n"   `shouldBe` "Right (JSAstLiteral (JSBigIntLiteral '0x1234n'))"
        testLiteral "0X1234n"   `shouldBe` "Right (JSAstLiteral (JSBigIntLiteral '0X1234n'))"
        testLiteral "0o777n"    `shouldBe` "Right (JSAstLiteral (JSBigIntLiteral '0o777n'))"
        testLiteral "077n"      `shouldBe` "Right (JSAstLiteral (JSBigIntLiteral '077n'))"
    it "strings" $ do
        testLiteral "'cat'"    `shouldBe` "Right (JSAstLiteral (JSStringLiteral 'cat'))"
        testLiteral "\"cat\""  `shouldBe` "Right (JSAstLiteral (JSStringLiteral \"cat\"))"
        testLiteral "'\\u1234'"     `shouldBe` "Right (JSAstLiteral (JSStringLiteral '\\u1234'))"
        testLiteral "'\\uabcd'"     `shouldBe` "Right (JSAstLiteral (JSStringLiteral '\\uabcd'))"
        testLiteral "\"\\r\\n\""    `shouldBe` "Right (JSAstLiteral (JSStringLiteral \"\\r\\n\"))"
        testLiteral "\"\\b\""           `shouldBe` "Right (JSAstLiteral (JSStringLiteral \"\\b\"))"
        testLiteral "\"\\f\""           `shouldBe` "Right (JSAstLiteral (JSStringLiteral \"\\f\"))"
        testLiteral "\"\\t\""           `shouldBe` "Right (JSAstLiteral (JSStringLiteral \"\\t\"))"
        testLiteral "\"\\v\""           `shouldBe` "Right (JSAstLiteral (JSStringLiteral \"\\v\"))"
        testLiteral "\"\\0\""           `shouldBe` "Right (JSAstLiteral (JSStringLiteral \"\\0\"))"
        testLiteral "\"hello\\nworld\"" `shouldBe` "Right (JSAstLiteral (JSStringLiteral \"hello\\nworld\"))"
        testLiteral "'hello\\nworld'"   `shouldBe` "Right (JSAstLiteral (JSStringLiteral 'hello\\nworld'))"

        testLiteral "'char \n'" `shouldBe` "Left (\"lexical error @ line 1 and column 7\")"

        forM_ (mkTestStrings SingleQuote) $ \ str ->
            testLiteral str `shouldBe` ("Right (JSAstLiteral (JSStringLiteral " ++ str ++ "))")

        forM_ (mkTestStrings DoubleQuote) $ \ str ->
            testLiteral str `shouldBe` ("Right (JSAstLiteral (JSStringLiteral " ++ str ++ "))")

    it "strings with escaped quotes" $ do
        testLiteral "'\"'"      `shouldBe` "Right (JSAstLiteral (JSStringLiteral '\"'))"
        testLiteral "\"\\\"\""  `shouldBe` "Right (JSAstLiteral (JSStringLiteral \"\\\"\"))"


data Quote
    = SingleQuote
    | DoubleQuote
    deriving Eq


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


testLiteral :: String -> String
testLiteral str = showStrippedMaybe $ parseUsing parseLiteral str "src"
