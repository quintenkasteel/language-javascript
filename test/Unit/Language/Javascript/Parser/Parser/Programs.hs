{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.Language.Javascript.Parser.Parser.Programs
  ( testProgramParser,
  )
where

#if ! MIN_VERSION_base(4,13,0)
import Control.Applicative ((<$>))
#endif

import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as BS8
import Data.List (isPrefixOf)
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
  ( JSAST (..),
    JSAnnot,
    JSBinOp (..),
    JSBlock (..),
    JSCommaList (..),
    JSCommaTrailingList (..),
    JSExpression (..),
    JSIdent (..),
    JSObjectProperty (..),
    JSPropertyName (..),
    JSSemi,
    JSStatement (..),
    JSVarInitializer (..),
  )
import Language.JavaScript.Parser.Grammar7
import Language.JavaScript.Parser.Parser (parseUsing, showStrippedMaybeString, showStrippedString)
import Test.Hspec

testProgramParser :: Spec
testProgramParser = describe "Program parser:" $ do
  it "function" $ do
    case testProg "function a(){}" of
      Right (JSAstProgram [JSFunction _ (JSIdentName _ "a") _ JSLNil _ (JSBlock _ [] _) _] _) -> pure ()
      result -> expectationFailure ("Expected function declaration, got: " ++ show result)
    case testProg "function a(b,c){}" of
      Right (JSAstProgram [JSFunction _ (JSIdentName _ "a") _ (JSLCons (JSLOne (JSIdentifier _ "b")) _ (JSIdentifier _ "c")) _ (JSBlock _ [] _) _] _) -> pure ()
      result -> expectationFailure ("Expected function declaration with params, got: " ++ show result)
  it "comments" $ do
    case testProg "//blah\nx=1;//foo\na" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSDecimal _ "1") _, JSExpressionStatement (JSIdentifier _ "a") _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with comment, got: " ++ show result)
    case testProg "/*x=1\ny=2\n*/z=2;//foo\na" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "z") (JSAssign _) (JSDecimal _ "2") _, JSExpressionStatement (JSIdentifier _ "a") _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with block comment, got: " ++ show result)
    case testProg "/* */\nfunction f() {\n/*  */\n}\n" of
      Right (JSAstProgram [JSFunction _ (JSIdentName _ "f") _ JSLNil _ (JSBlock _ [] _) _] _) -> pure ()
      result -> expectationFailure ("Expected function with comments, got: " ++ show result)
    case testProg "/* **/\nfunction f() {\n/*  */\n}\n" of
      Right (JSAstProgram [JSFunction _ (JSIdentName _ "f") _ JSLNil _ (JSBlock _ [] _) _] _) -> pure ()
      result -> expectationFailure ("Expected function with block comments, got: " ++ show result)

  it "if" $ do
    case testProg "if(x);x=1" of
      Right (JSAstProgram [JSIf _ _ (JSIdentifier _ "x") _ (JSEmptyStatement _), JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSDecimal _ "1") _] _) -> pure ()
      result -> expectationFailure ("Expected if statement with assignment, got: " ++ show result)
    case testProg "if(a)x=1;y=2" of
      Right (JSAstProgram [JSIf _ _ (JSIdentifier _ "a") _ (JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSDecimal _ "1") _), JSAssignStatement (JSIdentifier _ "y") (JSAssign _) (JSDecimal _ "2") _] _) -> pure ()
      result -> expectationFailure ("Expected if statement with assignments, got: " ++ show result)
    case testProg "if(a)x=a()y=2" of
      Right (JSAstProgram [JSIf _ _ (JSIdentifier _ "a") _ (JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSMemberExpression (JSIdentifier _ "a") _ JSLNil _) _), JSAssignStatement (JSIdentifier _ "y") (JSAssign _) (JSDecimal _ "2") _] _) -> pure ()
      result -> expectationFailure ("Expected if with assignment and call, got: " ++ show result)
    case testProg "if(true)break \nfoo();" of
      Right (JSAstProgram [JSIf _ _ (JSLiteral _ "true") _ (JSBreak _ _ _), JSMethodCall _ _ _ _ _] _) -> pure ()
      result -> expectationFailure ("Expected if with break and method call, got: " ++ show result)
    case testProg "if(true)continue \nfoo();" of
      Right (JSAstProgram [JSIf _ _ (JSLiteral _ "true") _ (JSContinue _ _ _), JSMethodCall _ _ _ _ _] _) -> pure ()
      result -> expectationFailure ("Expected if with continue and method call, got: " ++ show result)
    case testProg "if(true)break \nfoo();" of
      Right (JSAstProgram [JSIf _ _ (JSLiteral _ "true") _ (JSBreak _ _ _), JSMethodCall _ _ _ _ _] _) -> pure ()
      result -> expectationFailure ("Expected if with break and method call, got: " ++ show result)

  it "assign" $ do
    case testProg "x = 1\n  y=2;" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSDecimal _ "1") _, JSAssignStatement (JSIdentifier _ "y") (JSAssign _) (JSDecimal _ "2") _] _) -> pure ()
      result -> expectationFailure ("Expected assignment statements, got: " ++ show result)

  it "regex" $ do
    case testProg "x=/\\n/g" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSRegEx _ "/\\n/g") _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with regex, got: " ++ show result)
    case testProg "x=i(/^$/g,\"\\\\$&\")" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSMemberExpression (JSIdentifier _ "i") _ (JSLCons (JSLOne (JSRegEx _ "/^$/g")) _ (JSStringLiteral _ "\"\\\\$&\"")) _) _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with function call, got: " ++ show result)
    case testProg "x=i(/[?|^&(){}\\[\\]+\\-*\\/\\.]/g,\"\\\\$&\")" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSMemberExpression (JSIdentifier _ "i") _ (JSLCons (JSLOne (JSRegEx _ "/[?|^&(){}\\[\\]+\\-*\\/\\.]/g")) _ (JSStringLiteral _ "\"\\\\$&\"")) _) _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with complex regex call, got: " ++ show result)
    case testProg "(match = /^\"(?:\\\\.|[^\"])*\"|^'(?:[^']|\\\\.)*'/(input))" of
      Right (JSAstProgram [JSExpressionStatement (JSExpressionParen _ (JSAssignExpression (JSIdentifier _ "match") (JSAssign _) (JSMemberExpression (JSRegEx _ "/^\"(?:\\\\.|[^\"])*\"|^'(?:[^']|\\\\.)*'/") _ (JSLOne (JSIdentifier _ "input")) _)) _) _] _) -> pure ()
      result -> expectationFailure ("Expected parenthesized assignment with regex call, got: " ++ show result)
    case testProg "if(/^[a-z]/.test(t)){consts+=t.toUpperCase();keywords[t]=i}else consts+=(/^\\W/.test(t)?opTypeNames[t]:t);" of
      Right (JSAstProgram [JSIfElse _ _ _ _ _ _ _] _) -> pure ()
      result -> expectationFailure ("Expected if-else statement, got: " ++ show result)

  it "unicode" $ do
    case testProg "àáâãäå = 1;" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "àáâãäå") (JSAssign _) (JSDecimal _ "1") _] _) -> pure ()
      result -> expectationFailure ("Expected unicode assignment, got: " ++ show result)
    case testProg "//comment\x000Ax=1;" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSDecimal _ "1") _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with line feed, got: " ++ show result)
    case testProg "//comment\x000Dx=1;" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSDecimal _ "1") _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with carriage return, got: " ++ show result)
    case testProg "//comment\x2028x=1;" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSDecimal _ "1") _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with line separator, got: " ++ show result)
    case testProg "//comment\x2029x=1;" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSDecimal _ "1") _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with paragraph separator, got: " ++ show result)
    case testProg "$aà = 1;_b=2;\0065a=2" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "$aà") (JSAssign _) (JSDecimal _ "1") _, JSAssignStatement (JSIdentifier _ "_b") (JSAssign _) (JSDecimal _ "2") _, JSAssignStatement (JSIdentifier _ "Aa") (JSAssign _) (JSDecimal _ "2") _] _) -> pure ()
      result -> expectationFailure ("Expected three assignments, got: " ++ show result)
    case testProg "x=\"àáâãäå\";y='\3012a\0068'" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSStringLiteral _ "\"àáâãäå\"") _, JSAssignStatement (JSIdentifier _ "y") (JSAssign _) (JSStringLiteral _ "'\3012aD'") _] _) -> pure ()
      result -> expectationFailure ("Expected two assignments with unicode strings, got: " ++ show result)
    case testProg "a \f\v\t\r\n=\x00a0\x1680\x180e\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x2028\x2029\x202f\x205f\x3000\&1;" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "a") (JSAssign _) (JSDecimal _ "1") _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with unicode whitespace, got: " ++ show result)
    case testProg "/* * geolocation. пытаемся определить свое местоположение * если не получается то используем defaultLocation * @Param {object} map экземпляр карты * @Param {object LatLng} defaultLocation Координаты центра по умолчанию * @Param {function} callbackAfterLocation Фу-ия которая вызывается после * геолокации. Т.к запрос геолокации асинхронен */x" of
      Right (JSAstProgram [JSExpressionStatement (JSIdentifier _ "x") _] _) -> pure ()
      result -> expectationFailure ("Expected expression statement with identifier and russian comment, got: " ++ show result)
    testFileUtf8 "./test/Unicode.js" `shouldReturn` "JSAstProgram [JSOpAssign ('=',JSIdentifier 'àáâãäå',JSDecimal '1'),JSSemicolon]"

  it "strings" $ do
    -- Working in ECMASCRIPT 5.1 changes
    case testProg "x='abc\\ndef';" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSStringLiteral _ "'abc\\ndef'") _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with single-quoted string, got: " ++ show result)
    case testProg "x=\"abc\\ndef\";" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSStringLiteral _ "\"abc\\ndef\"") _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with double-quoted string, got: " ++ show result)
    case testProg "x=\"abc\\rdef\";" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSStringLiteral _ "\"abc\\rdef\"") _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with carriage return string, got: " ++ show result)
    case testProg "x=\"abc\\r\\ndef\";" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSStringLiteral _ "\"abc\\r\\ndef\"") _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with CRLF string, got: " ++ show result)
    case testProg "x=\"abc\\x2028 def\";" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSStringLiteral _ "\"abc\\x2028 def\"") _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with line separator string, got: " ++ show result)
    case testProg "x=\"abc\\x2029 def\";" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") (JSAssign _) (JSStringLiteral _ "\"abc\\x2029 def\"") _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with paragraph separator string, got: " ++ show result)

  it "object literal" $ do
    case testProg "x = { y: 1e8 }" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") _ (JSObjectLiteral _ (JSCTLNone (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "y") _ [JSDecimal _ "1e8"]))) _) _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with object literal, got: " ++ show result)
    case testProg "{ y: 1e8 }" of
      Right (JSAstProgram [JSStatementBlock _ _ _ _] _) -> pure ()
      result -> expectationFailure ("Expected statement block with label, got: " ++ show result)
    case testProg "{ y: 18 }" of
      Right (JSAstProgram [JSStatementBlock _ _ _ _] _) -> pure ()
      result -> expectationFailure ("Expected statement block with numeric label, got: " ++ show result)
    case testProg "x = { y: 18 }" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "x") _ (JSObjectLiteral _ (JSCTLNone (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "y") _ [JSDecimal _ "18"]))) _) _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with numeric object literal, got: " ++ show result)
    case testProg "var k = {\ny: somename\n}" of
      Right (JSAstProgram [JSVariable _ (JSLOne (JSVarInitExpression (JSIdentifier _ "k") (JSVarInit _ (JSObjectLiteral _ (JSCTLNone (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "y") _ [JSIdentifier _ "somename"]))) _)))) _] _) -> pure ()
      result -> expectationFailure ("Expected variable declaration with object literal, got: " ++ show result)
    case testProg "var k = {\ny: code\n}" of
      Right (JSAstProgram [JSVariable _ (JSLOne (JSVarInitExpression (JSIdentifier _ "k") (JSVarInit _ (JSObjectLiteral _ (JSCTLNone (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "y") _ [JSIdentifier _ "code"]))) _)))) _] _) -> pure ()
      result -> expectationFailure ("Expected variable declaration with code object, got: " ++ show result)
    case testProg "var k = {\ny: mode\n}" of
      Right (JSAstProgram [JSVariable _ (JSLOne (JSVarInitExpression (JSIdentifier _ "k") (JSVarInit _ (JSObjectLiteral _ (JSCTLNone (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "y") _ [JSIdentifier _ "mode"]))) _)))) _] _) -> pure ()
      result -> expectationFailure ("Expected variable declaration with mode object, got: " ++ show result)

  it "programs" $ do
    case testProg "newlines=spaces.match(/\\n/g)" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "newlines") _ _ _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with method call and regex, got: " ++ show result)
    case testProg "Animal=function(){return this.name};" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "Animal") _ (JSFunctionExpression _ JSIdentNone _ JSLNil _ (JSBlock _ [JSReturn _ (Just (JSMemberDot (JSLiteral _ "this") _ (JSIdentifier _ "name"))) _] _)) _] _) -> pure ()
      result -> expectationFailure ("Expected assignment with function expression and semicolon, got: " ++ show result)
    case testProg "$(img).click(function(){alert('clicked!')});" of
      Right (JSAstProgram [JSExpressionStatement (JSCallExpression (JSCallExpressionDot (JSMemberExpression (JSIdentifier _ "$") _ (JSLOne (JSIdentifier _ "img")) _) _ (JSIdentifier _ "click")) _ (JSLOne (JSFunctionExpression _ JSIdentNone _ JSLNil _ (JSBlock _ [JSMethodCall (JSIdentifier _ "alert") _ (JSLOne (JSStringLiteral _ "'clicked!'")) _ _] _))) _) _] _) -> pure ()
      result -> expectationFailure ("Expected expression statement with jQuery-style call and semicolon, got: " ++ show result)
    case testProg "function() {\nz = function z(o) {\nreturn r;\n};}" of
      Right (JSAstProgram [JSExpressionStatement (JSFunctionExpression _ JSIdentNone _ JSLNil _ (JSBlock _ [JSAssignStatement (JSIdentifier _ "z") _ (JSFunctionExpression _ (JSIdentName _ "z") _ (JSLOne (JSIdentifier _ "o")) _ (JSBlock _ [JSReturn _ (Just (JSIdentifier _ "r")) _] _)) _] _)) _] _) -> pure ()
      result -> expectationFailure ("Expected function expression with inner assignment, got: " ++ show result)
    case testProg "function() {\nz = function /*z*/(o) {\nreturn r;\n};}" of
      Right (JSAstProgram [JSExpressionStatement (JSFunctionExpression _ JSIdentNone _ JSLNil _ (JSBlock _ [JSAssignStatement (JSIdentifier _ "z") _ (JSFunctionExpression _ JSIdentNone _ (JSLOne (JSIdentifier _ "o")) _ (JSBlock _ [JSReturn _ (Just (JSIdentifier _ "r")) _] _)) _] _)) _] _) -> pure ()
      result -> expectationFailure ("Expected function expression with commented inner assignment, got: " ++ show result)
    case testProg "{zero}\nget;two\n{three\nfour;set;\n{\nsix;{seven;}\n}\n}" of
      Right (JSAstProgram [JSStatementBlock _ [JSExpressionStatement (JSIdentifier _ "zero") _] _ _, JSExpressionStatement (JSIdentifier _ "get") (JSSemi _), JSExpressionStatement (JSIdentifier _ "two") _, JSStatementBlock _ [JSExpressionStatement (JSIdentifier _ "three") _, JSExpressionStatement (JSIdentifier _ "four") (JSSemi _), JSExpressionStatement (JSIdentifier _ "set") (JSSemi _), JSStatementBlock _ [JSExpressionStatement (JSIdentifier _ "six") (JSSemi _), JSStatementBlock _ [JSExpressionStatement (JSIdentifier _ "seven") (JSSemi _)] _ _] _ _] _ _] _) -> pure ()
      result -> expectationFailure ("Expected complex nested statement blocks, got: " ++ show result)
    case testProg "{zero}\none1;two\n{three\nfour;five;\n{\nsix;{seven;}\n}\n}" of
      Right (JSAstProgram [JSStatementBlock _ [JSExpressionStatement (JSIdentifier _ "zero") _] _ _, JSExpressionStatement (JSIdentifier _ "one1") (JSSemi _), JSExpressionStatement (JSIdentifier _ "two") _, JSStatementBlock _ [JSExpressionStatement (JSIdentifier _ "three") _, JSExpressionStatement (JSIdentifier _ "four") (JSSemi _), JSExpressionStatement (JSIdentifier _ "five") (JSSemi _), JSStatementBlock _ [JSExpressionStatement (JSIdentifier _ "six") (JSSemi _), JSStatementBlock _ [JSExpressionStatement (JSIdentifier _ "seven") (JSSemi _)] _ _] _ _] _ _] _) -> pure ()
      result -> expectationFailure ("Expected complex nested statement blocks with one1, got: " ++ show result)
    case testProg "v = getValue(execute(n[0], x)) in getValue(execute(n[1], x));" of
      Right (JSAstProgram [JSAssignStatement (JSIdentifier _ "v") _ (JSExpressionBinary (JSMemberExpression (JSIdentifier _ "getValue") _ (JSLOne (JSMemberExpression (JSIdentifier _ "execute") _ (JSLCons (JSLOne (JSMemberSquare (JSIdentifier _ "n") _ (JSDecimal _ "0") _)) _ (JSIdentifier _ "x")) _)) _) (JSBinOpIn _) (JSMemberExpression (JSIdentifier _ "getValue") _ (JSLOne (JSMemberExpression (JSIdentifier _ "execute") _ (JSLCons (JSLOne (JSMemberSquare (JSIdentifier _ "n") _ (JSDecimal _ "1") _)) _ (JSIdentifier _ "x")) _)) _)) _] _) -> pure ()
      result -> expectationFailure ("Expected complex assignment with binary in expression, got: " ++ show result)
    case testProg "function Animal(name){if(!name)throw new Error('Must specify an animal name');this.name=name};Animal.prototype.toString=function(){return this.name};o=new Animal(\"bob\");o.toString()==\"bob\"" of
      Right (JSAstProgram [JSFunction _ (JSIdentName _ "Animal") _ (JSLOne (JSIdentifier _ "name")) _ (JSBlock _ [JSIf _ _ (JSUnaryExpression (JSUnaryOpNot _) (JSIdentifier _ "name")) _ (JSThrow _ (JSMemberNew _ (JSIdentifier _ "Error") _ (JSLOne (JSStringLiteral _ "'Must specify an animal name'")) _) _), JSAssignStatement (JSMemberDot (JSLiteral _ "this") _ (JSIdentifier _ "name")) (JSAssign _) (JSIdentifier _ "name") _] _) _, JSAssignStatement (JSMemberDot (JSMemberDot (JSIdentifier _ "Animal") _ (JSIdentifier _ "prototype")) _ (JSIdentifier _ "toString")) (JSAssign _) (JSFunctionExpression _ JSIdentNone _ JSLNil _ (JSBlock _ [JSReturn _ (Just (JSMemberDot (JSLiteral _ "this") _ (JSIdentifier _ "name"))) _] _)) _, JSAssignStatement (JSIdentifier _ "o") (JSAssign _) (JSMemberNew _ (JSIdentifier _ "Animal") _ (JSLOne (JSStringLiteral _ "\"bob\"")) _) _, JSExpressionStatement (JSExpressionBinary (JSMemberExpression (JSMemberDot (JSIdentifier _ "o") _ (JSIdentifier _ "toString")) _ JSLNil _) (JSBinOpEq _) (JSStringLiteral _ "\"bob\"")) _] _) -> pure ()
      result -> expectationFailure ("Expected complex Animal constructor and usage pattern, got: " ++ show result)

  it "automatic semicolon insertion with comments in functions" $ do
    -- Function with return statement and comment + newline - should parse successfully
    case testProg "function f1() { return // hello\n 4 }" of
      Right (JSAstProgram [JSFunction _ (JSIdentName _ "f1") _ JSLNil _ (JSBlock _ [JSReturn _ (Just (JSDecimal _ "4")) _] _) _] _) -> pure ()
      Left err -> expectationFailure ("Parse should succeed: " ++ show err)
      Right ast -> expectationFailure ("Expected program with function f1, got: " ++ show ast)
    case testProg "function f2() { return /* hello */ 4 }" of
      Right (JSAstProgram [JSFunction _ (JSIdentName _ "f2") _ JSLNil _ (JSBlock _ [JSReturn _ (Just (JSDecimal _ "4")) _] _) _] _) -> pure ()
      Left err -> expectationFailure ("Parse should succeed: " ++ show err)
      Right ast -> expectationFailure ("Expected program with function f2, got: " ++ show ast)
    case testProg "function f3() { return /* hello\n */ 4 }" of
      Right (JSAstProgram [JSFunction _ (JSIdentName _ "f3") _ JSLNil _ (JSBlock _ [JSReturn _ (Just (JSDecimal _ "4")) _] _) _] _) -> pure ()
      Left err -> expectationFailure ("Parse should succeed: " ++ show err)
      Right ast -> expectationFailure ("Expected program with function f3, got: " ++ show ast)
    case testProg "function f4() { return\n 4 }" of
      Right (JSAstProgram [JSFunction _ (JSIdentName _ "f4") _ JSLNil _ (JSBlock _ [JSReturn _ Nothing _, JSExpressionStatement (JSDecimal _ "4") _] _) _] _) -> pure ()
      Left err -> expectationFailure ("Parse should succeed: " ++ show err)
      Right ast -> expectationFailure ("Expected program with function f4, got: " ++ show ast)

    -- Functions with break/continue in loops - should parse successfully
    case testProg "function f() { while(true) { break // comment\n } }" of
      Right (JSAstProgram [JSFunction _ (JSIdentName _ "f") _ JSLNil _ (JSBlock _ [JSWhile _ _ (JSLiteral _ "true") _ (JSStatementBlock _ [JSBreak _ JSIdentNone _] _ _)] _) _] _) -> pure ()
      Left err -> expectationFailure ("Parse should succeed: " ++ show err)
      Right ast -> expectationFailure ("Expected program with while loop function, got: " ++ show ast)
    case testProg "function f() { for(;;) { continue /* comment\n */ } }" of
      Right (JSAstProgram [JSFunction _ (JSIdentName _ "f") _ JSLNil _ (JSBlock _ [JSFor _ _ JSLNil _ JSLNil _ JSLNil _ (JSStatementBlock _ [JSContinue _ JSIdentNone _] _ _)] _) _] _) -> pure ()
      Left err -> expectationFailure ("Parse should succeed: " ++ show err)
      Right ast -> expectationFailure ("Expected program with for loop function, got: " ++ show ast)

    -- Multiple statements with ASI - should parse successfully
    case testProg "function f() { return // first\n 1; return /* second\n */ 2 }" of
      Right (JSAstProgram [JSFunction _ (JSIdentName _ "f") _ JSLNil _ (JSBlock _ [JSReturn _ (Just (JSDecimal _ "1")) _, JSReturn _ (Just (JSDecimal _ "2")) _] _) _] _) -> pure ()
      Left err -> expectationFailure ("Parse should succeed: " ++ show err)
      Right ast -> expectationFailure ("Expected program with multi-return function, got: " ++ show ast)

    -- Mixed ASI scenarios - should parse successfully
    case testProg "var x = 5; function f() { return // comment\n x + 1 } f()" of
      Right (JSAstProgram [JSVariable _ (JSLOne (JSVarInitExpression (JSIdentifier _ "x") (JSVarInit _ (JSDecimal _ "5")))) _, JSFunction _ (JSIdentName _ "f") _ JSLNil _ (JSBlock _ [JSReturn _ (Just (JSExpressionBinary (JSIdentifier _ "x") (JSBinOpPlus _) (JSDecimal _ "1"))) _] _) _, JSMethodCall (JSIdentifier _ "f") _ JSLNil _ _] _) -> pure ()
      Left err -> expectationFailure ("Parse should succeed: " ++ show err)
      Right ast -> expectationFailure ("Expected program with var and function, got: " ++ show ast)

testProg :: String -> Either String JSAST
testProg str = parseUsing parseProgram str "src"

testFileUtf8 :: FilePath -> IO String
testFileUtf8 fileName = showStrippedString <$> parseFileUtf8 fileName
