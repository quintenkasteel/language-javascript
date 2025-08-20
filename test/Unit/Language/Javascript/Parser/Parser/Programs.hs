{-# LANGUAGE CPP #-}
module Unit.Language.Javascript.Parser.Parser.Programs
    ( testProgramParser
    ) where

#if ! MIN_VERSION_base(4,13,0)
import Control.Applicative ((<$>))
#endif
import Test.Hspec
import Data.List (isPrefixOf)

import Language.JavaScript.Parser
import Language.JavaScript.Parser.Grammar7
import Language.JavaScript.Parser.Parser


testProgramParser :: Spec
testProgramParser = describe "Program parser:" $ do
    it "function" $ do
        testProg "function a(){}"     `shouldBe` "Right (JSAstProgram [JSFunction 'a' () (JSBlock [])])"
        testProg "function a(b,c){}"  `shouldBe` "Right (JSAstProgram [JSFunction 'a' (JSIdentifier 'b',JSIdentifier 'c') (JSBlock [])])"
    it "comments" $ do
        testProg "//blah\nx=1;//foo\na"   `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1'),JSSemicolon,JSIdentifier 'a'])"
        testProg "/*x=1\ny=2\n*/z=2;//foo\na"  `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'z',JSDecimal '2'),JSSemicolon,JSIdentifier 'a'])"
        testProg "/* */\nfunction f() {\n/*  */\n}\n" `shouldBe` "Right (JSAstProgram [JSFunction 'f' () (JSBlock [])])"
        testProg "/* **/\nfunction f() {\n/*  */\n}\n" `shouldBe` "Right (JSAstProgram [JSFunction 'f' () (JSBlock [])])"

    it "if" $ do
        testProg "if(x);x=1"        `shouldBe` "Right (JSAstProgram [JSIf (JSIdentifier 'x') (JSEmptyStatement),JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1')])"
        testProg "if(a)x=1;y=2"     `shouldBe` "Right (JSAstProgram [JSIf (JSIdentifier 'a') (JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1'),JSSemicolon),JSOpAssign ('=',JSIdentifier 'y',JSDecimal '2')])"
        testProg "if(a)x=a()y=2"    `shouldBe` "Right (JSAstProgram [JSIf (JSIdentifier 'a') (JSOpAssign ('=',JSIdentifier 'x',JSMemberExpression (JSIdentifier 'a',JSArguments ()))),JSOpAssign ('=',JSIdentifier 'y',JSDecimal '2')])"
        testProg "if(true)break \nfoo();"       `shouldBe` "Right (JSAstProgram [JSIf (JSLiteral 'true') (JSBreak),JSMethodCall (JSIdentifier 'foo',JSArguments ()),JSSemicolon])"
        testProg "if(true)continue \nfoo();"    `shouldBe` "Right (JSAstProgram [JSIf (JSLiteral 'true') (JSContinue),JSMethodCall (JSIdentifier 'foo',JSArguments ()),JSSemicolon])"
        testProg "if(true)break \nfoo();"       `shouldBe` "Right (JSAstProgram [JSIf (JSLiteral 'true') (JSBreak),JSMethodCall (JSIdentifier 'foo',JSArguments ()),JSSemicolon])"

    it "assign" $
        testProg "x = 1\n  y=2;"    `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1'),JSOpAssign ('=',JSIdentifier 'y',JSDecimal '2'),JSSemicolon])"

    it "regex" $ do
        testProg "x=/\\n/g" `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSRegEx '/\\n/g')])"
        testProg "x=i(/^$/g,\"\\\\$&\")" `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSMemberExpression (JSIdentifier 'i',JSArguments (JSRegEx '/^$/g',JSStringLiteral \"\\\\$&\")))])"
        testProg "x=i(/[?|^&(){}\\[\\]+\\-*\\/\\.]/g,\"\\\\$&\")" `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSMemberExpression (JSIdentifier 'i',JSArguments (JSRegEx '/[?|^&(){}\\[\\]+\\-*\\/\\.]/g',JSStringLiteral \"\\\\$&\")))])"
        testProg "(match = /^\"(?:\\\\.|[^\"])*\"|^'(?:[^']|\\\\.)*'/(input))" `shouldBe` "Right (JSAstProgram [JSExpressionParen (JSOpAssign ('=',JSIdentifier 'match',JSMemberExpression (JSRegEx '/^\"(?:\\\\.|[^\"])*\"|^'(?:[^']|\\\\.)*'/',JSArguments (JSIdentifier 'input'))))])"
        testProg "if(/^[a-z]/.test(t)){consts+=t.toUpperCase();keywords[t]=i}else consts+=(/^\\W/.test(t)?opTypeNames[t]:t);"
                                `shouldBe` "Right (JSAstProgram [JSIfElse (JSMemberExpression (JSMemberDot (JSRegEx '/^[a-z]/',JSIdentifier 'test'),JSArguments (JSIdentifier 't'))) (JSStatementBlock [JSOpAssign ('+=',JSIdentifier 'consts',JSMemberExpression (JSMemberDot (JSIdentifier 't',JSIdentifier 'toUpperCase'),JSArguments ())),JSSemicolon,JSOpAssign ('=',JSMemberSquare (JSIdentifier 'keywords',JSIdentifier 't'),JSIdentifier 'i')]) (JSOpAssign ('+=',JSIdentifier 'consts',JSExpressionParen (JSExpressionTernary (JSMemberExpression (JSMemberDot (JSRegEx '/^\\W/',JSIdentifier 'test'),JSArguments (JSIdentifier 't')),JSMemberSquare (JSIdentifier 'opTypeNames',JSIdentifier 't'),JSIdentifier 't'))),JSSemicolon)])"

    it "unicode" $ do
        testProg "àáâãäå = 1;" `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier '\224\225\226\227\228\229',JSDecimal '1'),JSSemicolon])"
        testProg "//comment\x000Ax=1;" `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1'),JSSemicolon])"
        testProg "//comment\x000Dx=1;" `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1'),JSSemicolon])"
        testProg "//comment\x2028x=1;" `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1'),JSSemicolon])"
        testProg "//comment\x2029x=1;" `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1'),JSSemicolon])"
        testProg "$aà = 1;_b=2;\0065a=2"  `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier '$a\224',JSDecimal '1'),JSSemicolon,JSOpAssign ('=',JSIdentifier '_b',JSDecimal '2'),JSSemicolon,JSOpAssign ('=',JSIdentifier 'Aa',JSDecimal '2')])"
        testProg "x=\"àáâãäå\";y='\3012a\0068'" `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSStringLiteral \"\224\225\226\227\228\229\"),JSSemicolon,JSOpAssign ('=',JSIdentifier 'y',JSStringLiteral '\3012aD')])"
        testProg "a \f\v\t\r\n=\x00a0\x1680\x180e\x2000\x2001\x2002\x2003\x2004\x2005\x2006\x2007\x2008\x2009\x200a\x2028\x2029\x202f\x205f\x3000\&1;" `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'a',JSDecimal '1'),JSSemicolon])"
        testProg "/* * geolocation. пытаемся определить свое местоположение * если не получается то используем defaultLocation * @Param {object} map экземпляр карты * @Param {object LatLng} defaultLocation Координаты центра по умолчанию * @Param {function} callbackAfterLocation Фу-ия которая вызывается после * геолокации. Т.к запрос геолокации асинхронен */x" `shouldBe` "Right (JSAstProgram [JSIdentifier 'x'])"
        testFileUtf8 "./test/Unicode.js" `shouldReturn` "JSAstProgram [JSOpAssign ('=',JSIdentifier '\224\225\226\227\228\229',JSDecimal '1'),JSSemicolon]"

    it "strings" $ do
        -- Working in ECMASCRIPT 5.1 changes
        testProg "x='abc\\ndef';"       `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSStringLiteral 'abc\\ndef'),JSSemicolon])"
        testProg "x=\"abc\\ndef\";"     `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSStringLiteral \"abc\\ndef\"),JSSemicolon])"
        testProg "x=\"abc\\rdef\";"     `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSStringLiteral \"abc\\rdef\"),JSSemicolon])"
        testProg "x=\"abc\\r\\ndef\";"   `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSStringLiteral \"abc\\r\\ndef\"),JSSemicolon])"
        testProg "x=\"abc\\x2028 def\";"    `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSStringLiteral \"abc\\x2028 def\"),JSSemicolon])"
        testProg "x=\"abc\\x2029 def\";"    `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSStringLiteral \"abc\\x2029 def\"),JSSemicolon])"

    it "object literal" $ do
        testProg "x = { y: 1e8 }"   `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'y') [JSDecimal '1e8']])])"
        testProg "{ y: 1e8 }"       `shouldBe` "Right (JSAstProgram [JSStatementBlock [JSLabelled (JSIdentifier 'y') (JSDecimal '1e8')]])"
        testProg "{ y: 18 }"        `shouldBe` "Right (JSAstProgram [JSStatementBlock [JSLabelled (JSIdentifier 'y') (JSDecimal '18')]])"
        testProg "x = { y: 18 }"    `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'x',JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'y') [JSDecimal '18']])])"
        testProg "var k = {\ny: somename\n}"  `shouldBe` "Right (JSAstProgram [JSVariable (JSVarInitExpression (JSIdentifier 'k') [JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'y') [JSIdentifier 'somename']]])])"
        testProg "var k = {\ny: code\n}"      `shouldBe` "Right (JSAstProgram [JSVariable (JSVarInitExpression (JSIdentifier 'k') [JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'y') [JSIdentifier 'code']]])])"
        testProg "var k = {\ny: mode\n}"      `shouldBe` "Right (JSAstProgram [JSVariable (JSVarInitExpression (JSIdentifier 'k') [JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'y') [JSIdentifier 'mode']]])])"

    it "programs" $ do
        testProg "newlines=spaces.match(/\\n/g)" `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'newlines',JSMemberExpression (JSMemberDot (JSIdentifier 'spaces',JSIdentifier 'match'),JSArguments (JSRegEx '/\\n/g')))])"
        testProg "Animal=function(){return this.name};" `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'Animal',JSFunctionExpression '' () (JSBlock [JSReturn JSMemberDot (JSLiteral 'this',JSIdentifier 'name') ])),JSSemicolon])"
        testProg "$(img).click(function(){alert('clicked!')});" `shouldBe` "Right (JSAstProgram [JSCallExpression (JSCallExpressionDot (JSMemberExpression (JSIdentifier '$',JSArguments (JSIdentifier 'img')),JSIdentifier 'click'),JSArguments (JSFunctionExpression '' () (JSBlock [JSMethodCall (JSIdentifier 'alert',JSArguments (JSStringLiteral 'clicked!'))]))),JSSemicolon])"
        testProg "function() {\nz = function z(o) {\nreturn r;\n};}" `shouldBe` "Right (JSAstProgram [JSFunctionExpression '' () (JSBlock [JSOpAssign ('=',JSIdentifier 'z',JSFunctionExpression 'z' (JSIdentifier 'o') (JSBlock [JSReturn JSIdentifier 'r' JSSemicolon])),JSSemicolon])])"
        testProg "function() {\nz = function /*z*/(o) {\nreturn r;\n};}" `shouldBe` "Right (JSAstProgram [JSFunctionExpression '' () (JSBlock [JSOpAssign ('=',JSIdentifier 'z',JSFunctionExpression '' (JSIdentifier 'o') (JSBlock [JSReturn JSIdentifier 'r' JSSemicolon])),JSSemicolon])])"
        testProg "{zero}\nget;two\n{three\nfour;set;\n{\nsix;{seven;}\n}\n}" `shouldBe` "Right (JSAstProgram [JSStatementBlock [JSIdentifier 'zero'],JSIdentifier 'get',JSSemicolon,JSIdentifier 'two',JSStatementBlock [JSIdentifier 'three',JSIdentifier 'four',JSSemicolon,JSIdentifier 'set',JSSemicolon,JSStatementBlock [JSIdentifier 'six',JSSemicolon,JSStatementBlock [JSIdentifier 'seven',JSSemicolon]]]])"
        testProg "{zero}\none1;two\n{three\nfour;five;\n{\nsix;{seven;}\n}\n}" `shouldBe` "Right (JSAstProgram [JSStatementBlock [JSIdentifier 'zero'],JSIdentifier 'one1',JSSemicolon,JSIdentifier 'two',JSStatementBlock [JSIdentifier 'three',JSIdentifier 'four',JSSemicolon,JSIdentifier 'five',JSSemicolon,JSStatementBlock [JSIdentifier 'six',JSSemicolon,JSStatementBlock [JSIdentifier 'seven',JSSemicolon]]]])"
        testProg "v = getValue(execute(n[0], x)) in getValue(execute(n[1], x));"   `shouldBe` "Right (JSAstProgram [JSOpAssign ('=',JSIdentifier 'v',JSExpressionBinary ('in',JSMemberExpression (JSIdentifier 'getValue',JSArguments (JSMemberExpression (JSIdentifier 'execute',JSArguments (JSMemberSquare (JSIdentifier 'n',JSDecimal '0'),JSIdentifier 'x')))),JSMemberExpression (JSIdentifier 'getValue',JSArguments (JSMemberExpression (JSIdentifier 'execute',JSArguments (JSMemberSquare (JSIdentifier 'n',JSDecimal '1'),JSIdentifier 'x')))))),JSSemicolon])"
        testProg "function Animal(name){if(!name)throw new Error('Must specify an animal name');this.name=name};Animal.prototype.toString=function(){return this.name};o=new Animal(\"bob\");o.toString()==\"bob\""
                                    `shouldBe` "Right (JSAstProgram [JSFunction 'Animal' (JSIdentifier 'name') (JSBlock [JSIf (JSUnaryExpression ('!',JSIdentifier 'name')) (JSThrow (JSMemberNew (JSIdentifier 'Error',JSArguments (JSStringLiteral 'Must specify an animal name')))),JSOpAssign ('=',JSMemberDot (JSLiteral 'this',JSIdentifier 'name'),JSIdentifier 'name')]),JSOpAssign ('=',JSMemberDot (JSMemberDot (JSIdentifier 'Animal',JSIdentifier 'prototype'),JSIdentifier 'toString'),JSFunctionExpression '' () (JSBlock [JSReturn JSMemberDot (JSLiteral 'this',JSIdentifier 'name') ])),JSSemicolon,JSOpAssign ('=',JSIdentifier 'o',JSMemberNew (JSIdentifier 'Animal',JSArguments (JSStringLiteral \"bob\"))),JSSemicolon,JSExpressionBinary ('==',JSMemberExpression (JSMemberDot (JSIdentifier 'o',JSIdentifier 'toString'),JSArguments ()),JSStringLiteral \"bob\")])"

    it "automatic semicolon insertion with comments in functions" $ do
        -- Function with return statement and comment + newline - should parse successfully
        testProg "function f1() { return // hello\n 4 }" `shouldSatisfy` isPrefixOf "Right"
        testProg "function f2() { return /* hello */ 4 }" `shouldSatisfy` isPrefixOf "Right"
        testProg "function f3() { return /* hello\n */ 4 }" `shouldSatisfy` isPrefixOf "Right"
        testProg "function f4() { return\n 4 }" `shouldSatisfy` isPrefixOf "Right"
        
        -- Functions with break/continue in loops - should parse successfully
        testProg "function f() { while(true) { break // comment\n } }" `shouldSatisfy` isPrefixOf "Right"
        testProg "function f() { for(;;) { continue /* comment\n */ } }" `shouldSatisfy` isPrefixOf "Right"
        
        -- Multiple statements with ASI - should parse successfully
        testProg "function f() { return // first\n 1; return /* second\n */ 2 }" `shouldSatisfy` isPrefixOf "Right"
        
        -- Mixed ASI scenarios - should parse successfully
        testProg "var x = 5; function f() { return // comment\n x + 1 } f()" `shouldSatisfy` isPrefixOf "Right"


testProg :: String -> String
testProg str = showStrippedMaybe (parseUsing parseProgram str "src")

testFileUtf8 :: FilePath -> IO String
testFileUtf8 fileName = showStripped <$> parseFileUtf8 fileName

