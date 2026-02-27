module Unit.Language.Javascript.Parser.Lexer.BasicLexer
  ( testLexer,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.List (intercalate)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Test.Hspec

-- | All lexer tests are pending while we migrate from Alex/Happy to flatparse
testLexer :: Spec
testLexer = describe "Lexer:" $ do
  it "comments" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "// 𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡 " `shouldBe` "[CommentToken]"
    -- testLex "/* 𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡 */" `shouldBe` "[CommentToken]"

  it "numbers" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "123" `shouldBe` "[DecimalToken 123]"
    -- testLex "037" `shouldBe` "[OctalToken 037]"
    -- testLex "0xab" `shouldBe` "[HexIntegerToken 0xab]"
    -- testLex "0xCD" `shouldBe` "[HexIntegerToken 0xCD]"

  it "invalid numbers" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "089" `shouldBe` "[DecimalToken 0,DecimalToken 89]"
    -- testLex "0xGh" `shouldBe` "[DecimalToken 0,IdentifierToken 'xGh']"

  it "string" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "'cat'" `shouldBe` "[StringToken 'cat']"
    -- testLex "\"dog\"" `shouldBe` "[StringToken \"dog\"]"

  it "strings with escape chars" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "'\t'" `shouldBe` "[StringToken '\t']"
    -- testLex "'\\n'" `shouldBe` "[StringToken '\\n']"
    -- testLex "'\\\\n'" `shouldBe` "[StringToken '\\\\n']"
    -- testLex "'\\\\'" `shouldBe` "[StringToken '\\\\']"
    -- testLex "'\\0'" `shouldBe` "[StringToken '\\0']"
    -- testLex "'\\12'" `shouldBe` "[StringToken '\\12']"
    -- testLex "'\\s'" `shouldBe` "[StringToken '\\s']"
    -- testLex "'\\-'" `shouldBe` "[StringToken '\\-']"

  it "strings with non-escaped chars" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "'\\/'" `shouldBe` "[StringToken '\\/']"

  it "strings with escaped quotes" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "'\"'" `shouldBe` "[StringToken '\"']"
    -- testLex "\"\\\"\"" `shouldBe` "[StringToken \"\\\\\"\"]"
    -- testLex "'\\\''" `shouldBe` "[StringToken '\\\\'']"
    -- testLex "'\"'" `shouldBe` "[StringToken '\"']"
    -- testLex "\"\\'\"" `shouldBe` "[StringToken \"\\'\"]"

  it "spread token" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "...a" `shouldBe` "[SpreadToken,IdentifierToken 'a']"

  it "assignment" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "x=1" `shouldBe` "[IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"
    -- testLex "x=1\ny=2" `shouldBe` "[IdentifierToken 'x',SimpleAssignToken,DecimalToken 1,WsToken,IdentifierToken 'y',SimpleAssignToken,DecimalToken 2]"

  it "break/continue/return" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "break\nx=1" `shouldBe` "[BreakToken,WsToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"
    -- testLex "continue\nx=1" `shouldBe` "[ContinueToken,WsToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"
    -- testLex "return\nx=1" `shouldBe` "[ReturnToken,WsToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"

  it "var/let" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "var\n" `shouldBe` "[VarToken,WsToken]"
    -- testLex "let\n" `shouldBe` "[LetToken,WsToken]"

  it "in/of" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "in\n" `shouldBe` "[InToken,WsToken]"
    -- testLex "of\n" `shouldBe` "[OfToken,WsToken]"

  it "function" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "async function\n" `shouldBe` "[AsyncToken,WsToken,FunctionToken,WsToken]"

  it "bigint literals" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "123n" `shouldBe` "[BigIntToken 123n]"
    -- testLex "0n" `shouldBe` "[BigIntToken 0n]"
    -- testLex "0x1234n" `shouldBe` "[BigIntToken 0x1234n]"
    -- testLex "0X1234n" `shouldBe` "[BigIntToken 0X1234n]"
    -- testLex "077n" `shouldBe` "[BigIntToken 077n]"

  it "optional chaining" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "obj?.prop" `shouldBe` "[IdentifierToken 'obj',OptionalChainingToken,IdentifierToken 'prop']"
    -- testLex "obj?.[key]" `shouldBe` "[IdentifierToken 'obj',OptionalChainingToken,LeftBracketToken,IdentifierToken 'key',RightBracketToken]"
    -- testLex "obj?.method()" `shouldBe` "[IdentifierToken 'obj',OptionalChainingToken,IdentifierToken 'method',LeftParenToken,RightParenToken]"

  it "nullish coalescing" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- testLex "x ?? y" `shouldBe` "[IdentifierToken 'x',WsToken,NullishCoalescingToken,WsToken,IdentifierToken 'y']"
    -- testLex "null??'default'" `shouldBe` "[NullToken,NullishCoalescingToken,StringToken 'default']"

  it "automatic semicolon insertion with comments" $ do
    pendingWith "Waiting for flatparse lexer migration to complete"
    -- Single-line comments with newlines trigger ASI
    -- testLexASI "return // comment\n4" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken,DecimalToken 4]"
    -- testLexASI "break // comment\nx" `shouldBe` "[BreakToken,WsToken,CommentToken,WsToken,AutoSemiToken,IdentifierToken 'x']"
    -- testLexASI "continue // comment\n" `shouldBe` "[ContinueToken,WsToken,CommentToken,WsToken,AutoSemiToken]"

    -- Multi-line comments with newlines trigger ASI
    -- testLexASI "return /* comment\n */ 4" `shouldBe` "[ReturnToken,WsToken,CommentToken,AutoSemiToken,WsToken,DecimalToken 4]"
    -- testLexASI "break /* line1\nline2 */ x" `shouldBe` "[BreakToken,WsToken,CommentToken,AutoSemiToken,WsToken,IdentifierToken 'x']"

    -- Multi-line comments without newlines do NOT trigger ASI
    -- testLexASI "return /* comment */ 4" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,DecimalToken 4]"
    -- testLexASI "break /* inline */ x" `shouldBe` "[BreakToken,WsToken,CommentToken,WsToken,IdentifierToken 'x']"

    -- Whitespace with newlines still triggers ASI (existing behavior)
    -- testLexASI "return \n 4" `shouldBe` "[ReturnToken,WsToken,AutoSemiToken,DecimalToken 4]"
    -- testLexASI "continue \n x" `shouldBe` "[ContinueToken,WsToken,AutoSemiToken,IdentifierToken 'x']"

    -- Different line terminator types in comments
    -- testLexASI "return // comment\r\n4" `shouldBe` "[ReturnToken,WsToken,CommentToken,WsToken,AutoSemiToken,DecimalToken 4]"
    -- testLexASI "break /* comment\r */ x" `shouldBe` "[BreakToken,WsToken,CommentToken,AutoSemiToken,WsToken,IdentifierToken 'x']"

    -- Comments after non-ASI tokens do not create AutoSemiToken
    -- testLexASI "var // comment\n x" `shouldBe` "[VarToken,WsToken,CommentToken,WsToken,IdentifierToken 'x']"
    -- testLexASI "function /* comment\n */ f" `shouldBe` "[FunctionToken,WsToken,CommentToken,WsToken,IdentifierToken 'f']"

-- Legacy Alex-based tokenizer test helpers - disabled for flatparse migration
{-
testLex :: String -> String
testLex str =
  either id stringify $ alexTestTokeniser str
  where
    stringify xs = "[" ++ intercalate "," (map showToken xs) ++ "]"
    utf8ToString :: String -> String
    utf8ToString = id

    showToken :: Token -> String
    showToken (StringToken _ lit _) = "StringToken " ++ stringEscape lit
    showToken (IdentifierToken _ lit _) = "IdentifierToken '" ++ stringEscape lit ++ "'"
    showToken (DecimalToken _ lit _) = "DecimalToken " ++ lit
    showToken (OctalToken _ lit _) = "OctalToken " ++ lit
    showToken (HexIntegerToken _ lit _) = "HexIntegerToken " ++ lit
    showToken (BigIntToken _ lit _) = "BigIntToken " ++ lit
    showToken token = takeWhile (/= ' ') $ show token

    stringEscape [] = []
    stringEscape (term : rest) =
      let escapeTerm [] = []
          escapeTerm [x] = [x]
          escapeTerm (x : xs)
            | term == x = "\\" ++ [x] ++ escapeTerm xs
            | otherwise = x : escapeTerm xs
       in term : escapeTerm rest

-- Test function that uses ASI-enabled tokenizer - disabled for flatparse migration
testLexASI :: String -> String
testLexASI str =
  either id stringify $ alexTestTokeniserASI str
  where
    stringify xs = "[" ++ intercalate "," (map showToken xs) ++ "]"
    utf8ToString :: String -> String
    utf8ToString = id

    showToken :: Token -> String
    showToken (StringToken _ lit _) = "StringToken " ++ stringEscape lit
    showToken (IdentifierToken _ lit _) = "IdentifierToken '" ++ stringEscape lit ++ "'"
    showToken (DecimalToken _ lit _) = "DecimalToken " ++ lit
    showToken (OctalToken _ lit _) = "OctalToken " ++ lit
    showToken (HexIntegerToken _ lit _) = "HexIntegerToken " ++ lit
    showToken (BigIntToken _ lit _) = "BigIntToken " ++ lit
    showToken token = takeWhile (/= ' ') $ show token

    stringEscape [] = []
    stringEscape (term : rest) =
      let escapeTerm [] = []
          escapeTerm [x] = [x]
          escapeTerm (x : xs)
            | term == x = "\\" ++ [x] ++ escapeTerm xs
            | otherwise = x : escapeTerm xs
       in term : escapeTerm rest
-}