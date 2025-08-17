module Test.Language.Javascript.StatementParser
    ( testStatementParser
    ) where


import Test.Hspec

import Language.JavaScript.Parser
import Language.JavaScript.Parser.Grammar7
import Language.JavaScript.Parser.Parser


testStatementParser :: Spec
testStatementParser = describe "Parse statements:" $ do
    it "simple" $ do
        testStmt "x"        `shouldBe` "Right (JSAstStatement (JSIdentifier 'x'))"
        testStmt "null"     `shouldBe` "Right (JSAstStatement (JSLiteral 'null'))"
        testStmt "true?1:2" `shouldBe` "Right (JSAstStatement (JSExpressionTernary (JSLiteral 'true',JSDecimal '1',JSDecimal '2')))"

    it "block" $ do
        testStmt "{}"           `shouldBe` "Right (JSAstStatement (JSStatementBlock []))"
        testStmt "{x=1}"        `shouldBe` "Right (JSAstStatement (JSStatementBlock [JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1')]))"
        testStmt "{x=1;y=2}"    `shouldBe` "Right (JSAstStatement (JSStatementBlock [JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1'),JSSemicolon,JSOpAssign ('=',JSIdentifier 'y',JSDecimal '2')]))"
        testStmt "{{}}"         `shouldBe` "Right (JSAstStatement (JSStatementBlock [JSStatementBlock []]))"
        testStmt "{{{}}}"       `shouldBe` "Right (JSAstStatement (JSStatementBlock [JSStatementBlock [JSStatementBlock []]]))"

    it "if" $
        testStmt "if (1) {}"    `shouldBe` "Right (JSAstStatement (JSIf (JSDecimal '1') (JSStatementBlock [])))"

    it "if/else" $ do
        testStmt "if (1) {} else {}"    `shouldBe` "Right (JSAstStatement (JSIfElse (JSDecimal '1') (JSStatementBlock []) (JSStatementBlock [])))"
        testStmt "if (1) x=1; else {}"  `shouldBe` "Right (JSAstStatement (JSIfElse (JSDecimal '1') (JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1'),JSSemicolon) (JSStatementBlock [])))"
        testStmt " if (1);else break"   `shouldBe` "Right (JSAstStatement (JSIfElse (JSDecimal '1') (JSEmptyStatement) (JSBreak)))"

    it "while" $
        testStmt "while(true);"            `shouldBe` "Right (JSAstStatement (JSWhile (JSLiteral 'true') (JSEmptyStatement)))"

    it "do/while" $ do
        testStmt "do {x=1} while (true);"   `shouldBe` "Right (JSAstStatement (JSDoWhile (JSStatementBlock [JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1')]) (JSLiteral 'true') (JSSemicolon)))"
        testStmt "do x=x+1;while(x<4);"     `shouldBe` "Right (JSAstStatement (JSDoWhile (JSOpAssign ('=',JSIdentifier 'x',JSExpressionBinary ('+',JSIdentifier 'x',JSDecimal '1')),JSSemicolon) (JSExpressionBinary ('<',JSIdentifier 'x',JSDecimal '4')) (JSSemicolon)))"

    it "for" $ do
        testStmt "for(;;);"             `shouldBe` "Right (JSAstStatement (JSFor () () () (JSEmptyStatement)))"
        testStmt "for(x=1;x<10;x++);"   `shouldBe` "Right (JSAstStatement (JSFor (JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1')) (JSExpressionBinary ('<',JSIdentifier 'x',JSDecimal '10')) (JSExpressionPostfix ('++',JSIdentifier 'x')) (JSEmptyStatement)))"

        testStmt "for(var x;;);"        `shouldBe` "Right (JSAstStatement (JSForVar (JSVarInitExpression (JSIdentifier 'x') ) () () (JSEmptyStatement)))"
        testStmt "for(var x=1;;);"      `shouldBe` "Right (JSAstStatement (JSForVar (JSVarInitExpression (JSIdentifier 'x') [JSDecimal '1']) () () (JSEmptyStatement)))"
        testStmt "for(var x;y;z){}"     `shouldBe` "Right (JSAstStatement (JSForVar (JSVarInitExpression (JSIdentifier 'x') ) (JSIdentifier 'y') (JSIdentifier 'z') (JSStatementBlock [])))"

        testStmt "for(x in 5){}"        `shouldBe` "Right (JSAstStatement (JSForIn JSIdentifier 'x' (JSDecimal '5') (JSStatementBlock [])))"

        testStmt "for(var x in 5){}"    `shouldBe` "Right (JSAstStatement (JSForVarIn (JSVarInitExpression (JSIdentifier 'x') ) (JSDecimal '5') (JSStatementBlock [])))"

        testStmt "for(let x;y;z){}"     `shouldBe` "Right (JSAstStatement (JSForLet (JSVarInitExpression (JSIdentifier 'x') ) (JSIdentifier 'y') (JSIdentifier 'z') (JSStatementBlock [])))"
        testStmt "for(let x in 5){}"    `shouldBe` "Right (JSAstStatement (JSForLetIn (JSVarInitExpression (JSIdentifier 'x') ) (JSDecimal '5') (JSStatementBlock [])))"
        testStmt "for(let x of 5){}"    `shouldBe` "Right (JSAstStatement (JSForLetOf (JSVarInitExpression (JSIdentifier 'x') ) (JSDecimal '5') (JSStatementBlock [])))"
        testStmt "for(const x;y;z){}"   `shouldBe` "Right (JSAstStatement (JSForConst (JSVarInitExpression (JSIdentifier 'x') ) (JSIdentifier 'y') (JSIdentifier 'z') (JSStatementBlock [])))"
        testStmt "for(const x in 5){}"  `shouldBe` "Right (JSAstStatement (JSForConstIn (JSVarInitExpression (JSIdentifier 'x') ) (JSDecimal '5') (JSStatementBlock [])))"
        testStmt "for(const x of 5){}"  `shouldBe` "Right (JSAstStatement (JSForConstOf (JSVarInitExpression (JSIdentifier 'x') ) (JSDecimal '5') (JSStatementBlock [])))"
        testStmt "for(x of 5){}"        `shouldBe` "Right (JSAstStatement (JSForOf JSIdentifier 'x' (JSDecimal '5') (JSStatementBlock [])))"
        testStmt "for(var x of 5){}"    `shouldBe` "Right (JSAstStatement (JSForVarOf (JSVarInitExpression (JSIdentifier 'x') ) (JSDecimal '5') (JSStatementBlock [])))"

    it "variable/constant/let declaration" $ do
        testStmt "var x=1;"         `shouldBe` "Right (JSAstStatement (JSVariable (JSVarInitExpression (JSIdentifier 'x') [JSDecimal '1'])))"
        testStmt "const x=1,y=2;"   `shouldBe` "Right (JSAstStatement (JSConstant (JSVarInitExpression (JSIdentifier 'x') [JSDecimal '1'],JSVarInitExpression (JSIdentifier 'y') [JSDecimal '2'])))"
        testStmt "let x=1,y=2;"     `shouldBe` "Right (JSAstStatement (JSLet (JSVarInitExpression (JSIdentifier 'x') [JSDecimal '1'],JSVarInitExpression (JSIdentifier 'y') [JSDecimal '2'])))"
        testStmt "var [a,b]=x"      `shouldBe` "Right (JSAstStatement (JSVariable (JSVarInitExpression (JSArrayLiteral [JSIdentifier 'a',JSComma,JSIdentifier 'b']) [JSIdentifier 'x'])))"
        testStmt "const {a:b}=x"    `shouldBe` "Right (JSAstStatement (JSConstant (JSVarInitExpression (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'a') [JSIdentifier 'b']]) [JSIdentifier 'x'])))"

    it "break" $ do
        testStmt "break;"       `shouldBe` "Right (JSAstStatement (JSBreak,JSSemicolon))"
        testStmt "break x;"     `shouldBe` "Right (JSAstStatement (JSBreak 'x',JSSemicolon))"
        testStmt "{break}"      `shouldBe` "Right (JSAstStatement (JSStatementBlock [JSBreak]))"

    it "continue" $ do
        testStmt "continue;"    `shouldBe` "Right (JSAstStatement (JSContinue,JSSemicolon))"
        testStmt "continue x;"  `shouldBe` "Right (JSAstStatement (JSContinue 'x',JSSemicolon))"
        testStmt "{continue}"   `shouldBe` "Right (JSAstStatement (JSStatementBlock [JSContinue]))"

    it "return" $ do
        testStmt "return;"      `shouldBe` "Right (JSAstStatement (JSReturn JSSemicolon))"
        testStmt "return x;"    `shouldBe` "Right (JSAstStatement (JSReturn JSIdentifier 'x' JSSemicolon))"
        testStmt "return 123;"  `shouldBe` "Right (JSAstStatement (JSReturn JSDecimal '123' JSSemicolon))"
        testStmt "{return}"     `shouldBe` "Right (JSAstStatement (JSStatementBlock [JSReturn ]))"

    it "automatic semicolon insertion with comments" $ do
        -- Return statements with comments and newlines should trigger ASI
        testStmt "return // comment\n4"    `shouldBe` "Right (JSAstStatement (JSReturn JSDecimal '4' ))"
        testStmt "return /* comment\n */4" `shouldBe` "Right (JSAstStatement (JSReturn JSDecimal '4' ))"
        
        -- Return statements with comments but no newlines should NOT trigger ASI
        testStmt "return /* comment */ 4"  `shouldBe` "Right (JSAstStatement (JSReturn JSDecimal '4' ))"
        
        -- Break and continue statements with comments and newlines
        testStmt "break // comment\n"      `shouldBe` "Right (JSAstStatement (JSBreak))"
        testStmt "continue /* line\n */"   `shouldBe` "Right (JSAstStatement (JSContinue))"
        
        -- Whitespace newlines still work (existing behavior) - but this should parse error because 4 is leftover
        testStmt "return \n"             `shouldBe` "Right (JSAstStatement (JSReturn ))"

    it "with" $
        testStmt "with (x) {};" `shouldBe` "Right (JSAstStatement (JSWith (JSIdentifier 'x') (JSStatementBlock [])))"

    it "assign" $
        testStmt "var z = x[i] / y;"    `shouldBe` "Right (JSAstStatement (JSVariable (JSVarInitExpression (JSIdentifier 'z') [JSExpressionBinary ('/',JSMemberSquare (JSIdentifier 'x',JSIdentifier 'i'),JSIdentifier 'y')])))"

    it "label" $
        testStmt "abc:x=1"      `shouldBe` "Right (JSAstStatement (JSLabelled (JSIdentifier 'abc') (JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1'))))"

    it "throw" $
        testStmt "throw 1"      `shouldBe` "Right (JSAstStatement (JSThrow (JSDecimal '1')))"

    it "switch" $ do
        testStmt "switch (x) {}"    `shouldBe` "Right (JSAstStatement (JSSwitch (JSIdentifier 'x') []))"
        testStmt "switch (x) {case 1:break;}"   `shouldBe` "Right (JSAstStatement (JSSwitch (JSIdentifier 'x') [JSCase (JSDecimal '1') ([JSBreak,JSSemicolon])]))"
        testStmt "switch (x) {case 0:\ncase 1:break;}"  `shouldBe` "Right (JSAstStatement (JSSwitch (JSIdentifier 'x') [JSCase (JSDecimal '0') ([]),JSCase (JSDecimal '1') ([JSBreak,JSSemicolon])]))"
        testStmt "switch (x) {default:break;}"          `shouldBe` "Right (JSAstStatement (JSSwitch (JSIdentifier 'x') [JSDefault ([JSBreak,JSSemicolon])]))"
        testStmt "switch (x) {default:\ncase 1:break;}" `shouldBe` "Right (JSAstStatement (JSSwitch (JSIdentifier 'x') [JSDefault ([]),JSCase (JSDecimal '1') ([JSBreak,JSSemicolon])]))"

    it "try/cathc/finally" $ do
        testStmt "try{}catch(a){}"             `shouldBe` "Right (JSAstStatement (JSTry (JSBlock [],[JSCatch (JSIdentifier 'a',JSBlock [])],JSFinally ())))"
        testStmt "try{}finally{}"              `shouldBe` "Right (JSAstStatement (JSTry (JSBlock [],[],JSFinally (JSBlock []))))"
        testStmt "try{}catch(a){}finally{}"    `shouldBe` "Right (JSAstStatement (JSTry (JSBlock [],[JSCatch (JSIdentifier 'a',JSBlock [])],JSFinally (JSBlock []))))"
        testStmt "try{}catch(a){}catch(b){}finally{}"   `shouldBe` "Right (JSAstStatement (JSTry (JSBlock [],[JSCatch (JSIdentifier 'a',JSBlock []),JSCatch (JSIdentifier 'b',JSBlock [])],JSFinally (JSBlock []))))"
        testStmt "try{}catch(a){}catch(b){}"            `shouldBe` "Right (JSAstStatement (JSTry (JSBlock [],[JSCatch (JSIdentifier 'a',JSBlock []),JSCatch (JSIdentifier 'b',JSBlock [])],JSFinally ())))"
        testStmt "try{}catch(a if true){}catch(b){}"    `shouldBe` "Right (JSAstStatement (JSTry (JSBlock [],[JSCatch (JSIdentifier 'a') if JSLiteral 'true' (JSBlock []),JSCatch (JSIdentifier 'b',JSBlock [])],JSFinally ())))"

    it "function" $ do
        testStmt "function x(){}"     `shouldBe` "Right (JSAstStatement (JSFunction 'x' () (JSBlock [])))"
        testStmt "function x(a){}"    `shouldBe` "Right (JSAstStatement (JSFunction 'x' (JSIdentifier 'a') (JSBlock [])))"
        testStmt "function x(a,b){}"  `shouldBe` "Right (JSAstStatement (JSFunction 'x' (JSIdentifier 'a',JSIdentifier 'b') (JSBlock [])))"
        testStmt "function x(...a){}" `shouldBe` "Right (JSAstStatement (JSFunction 'x' (JSSpreadExpression (JSIdentifier 'a')) (JSBlock [])))"
        testStmt "function x(a=1){}"  `shouldBe` "Right (JSAstStatement (JSFunction 'x' (JSOpAssign ('=',JSIdentifier 'a',JSDecimal '1')) (JSBlock [])))"
        testStmt "function x([a]){}"  `shouldBe` "Right (JSAstStatement (JSFunction 'x' (JSArrayLiteral [JSIdentifier 'a']) (JSBlock [])))"
        testStmt "function x({a}){}"  `shouldBe` "Right (JSAstStatement (JSFunction 'x' (JSObjectLiteral [JSPropertyIdentRef 'a']) (JSBlock [])))"

    it "generator" $ do
        testStmt "function* x(){}"       `shouldBe` "Right (JSAstStatement (JSGenerator 'x' () (JSBlock [])))"
        testStmt "function* x(a){}"      `shouldBe` "Right (JSAstStatement (JSGenerator 'x' (JSIdentifier 'a') (JSBlock [])))"
        testStmt "function* x(a,b){}"    `shouldBe` "Right (JSAstStatement (JSGenerator 'x' (JSIdentifier 'a',JSIdentifier 'b') (JSBlock [])))"
        testStmt "function* x(a,...b){}" `shouldBe` "Right (JSAstStatement (JSGenerator 'x' (JSIdentifier 'a',JSSpreadExpression (JSIdentifier 'b')) (JSBlock [])))"

    it "class" $ do
        testStmt "class Foo extends Bar { a(x,y) {} *b() {} }" `shouldBe` "Right (JSAstStatement (JSClass 'Foo' (JSIdentifier 'Bar') [JSMethodDefinition (JSIdentifier 'a') (JSIdentifier 'x',JSIdentifier 'y') (JSBlock []),JSGeneratorMethodDefinition (JSIdentifier 'b') () (JSBlock [])]))"
        testStmt "class Foo { static get [a]() {}; }" `shouldBe` "Right (JSAstStatement (JSClass 'Foo' () [JSClassStaticMethod (JSPropertyAccessor JSAccessorGet (JSPropertyComputed (JSIdentifier 'a')) () (JSBlock [])),JSClassSemi]))"
        testStmt "class Foo extends Bar { a(x,y) { super[x](y); } }" `shouldBe` "Right (JSAstStatement (JSClass 'Foo' (JSIdentifier 'Bar') [JSMethodDefinition (JSIdentifier 'a') (JSIdentifier 'x',JSIdentifier 'y') (JSBlock [JSMethodCall (JSMemberSquare (JSLiteral 'super',JSIdentifier 'x'),JSArguments (JSIdentifier 'y')),JSSemicolon])]))"


testStmt :: String -> String
testStmt str = showStrippedMaybe (parseUsing parseStatement str "src")
