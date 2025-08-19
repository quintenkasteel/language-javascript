module Test.Language.Javascript.ExpressionParser
    ( testExpressionParser
    ) where

import Test.Hspec

import Language.JavaScript.Parser
import Language.JavaScript.Parser.Grammar7
import Language.JavaScript.Parser.Parser


testExpressionParser :: Spec
testExpressionParser = describe "Parse expressions:" $ do
    it "this" $
        testExpr "this"     `shouldBe` "Right (JSAstExpression (JSLiteral 'this'))"
    it "regex" $ do
        testExpr "/blah/"   `shouldBe` "Right (JSAstExpression (JSRegEx '/blah/'))"
        testExpr "/$/g"     `shouldBe` "Right (JSAstExpression (JSRegEx '/$/g'))"
        testExpr "/\\n/g"   `shouldBe` "Right (JSAstExpression (JSRegEx '/\\n/g'))"
        testExpr "/(\\/)/"  `shouldBe` "Right (JSAstExpression (JSRegEx '/(\\/)/'))"
        testExpr "/a[/]b/"  `shouldBe` "Right (JSAstExpression (JSRegEx '/a[/]b/'))"
        testExpr "/[/\\]/"  `shouldBe` "Right (JSAstExpression (JSRegEx '/[/\\]/'))"
        testExpr "/(\\/|\\)/"  `shouldBe` "Right (JSAstExpression (JSRegEx '/(\\/|\\)/'))"
        testExpr "/a\\[|\\]$/g" `shouldBe` "Right (JSAstExpression (JSRegEx '/a\\[|\\]$/g'))"
        testExpr "/[(){}\\[\\]]/g" `shouldBe` "Right (JSAstExpression (JSRegEx '/[(){}\\[\\]]/g'))"
        testExpr "/^\"(?:\\.|[^\"])*\"|^'(?:[^']|\\.)*'/" `shouldBe` "Right (JSAstExpression (JSRegEx '/^\"(?:\\.|[^\"])*\"|^'(?:[^']|\\.)*'/'))"

    it "identifier" $ do
        testExpr "_$"       `shouldBe` "Right (JSAstExpression (JSIdentifier '_$'))"
        testExpr "this_"    `shouldBe` "Right (JSAstExpression (JSIdentifier 'this_'))"
    it "array literal" $ do
        testExpr "[]"       `shouldBe` "Right (JSAstExpression (JSArrayLiteral []))"
        testExpr "[,]"      `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSComma]))"
        testExpr "[,,]"     `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSComma,JSComma]))"
        testExpr "[,,x]"    `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSComma,JSComma,JSIdentifier 'x']))"
        testExpr "[,,x]"    `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSComma,JSComma,JSIdentifier 'x']))"
        testExpr "[,x,,x]"  `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSComma,JSIdentifier 'x',JSComma,JSComma,JSIdentifier 'x']))"
        testExpr "[x]"      `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSIdentifier 'x']))"
        testExpr "[x,]"     `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSIdentifier 'x',JSComma]))"
        testExpr "[,,,]"    `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSComma,JSComma,JSComma]))"
        testExpr "[a,,]"    `shouldBe` "Right (JSAstExpression (JSArrayLiteral [JSIdentifier 'a',JSComma,JSComma]))"
    it "operator precedence" $ do
        testExpr "2+3*4+5"  `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('+',JSExpressionBinary ('+',JSDecimal '2',JSExpressionBinary ('*',JSDecimal '3',JSDecimal '4')),JSDecimal '5')))"
        testExpr "2*3**4"   `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('*',JSDecimal '2',JSExpressionBinary ('**',JSDecimal '3',JSDecimal '4'))))"
        testExpr "2**3*4"   `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('*',JSExpressionBinary ('**',JSDecimal '2',JSDecimal '3'),JSDecimal '4')))"
    it "parentheses" $
        testExpr "(56)"     `shouldBe` "Right (JSAstExpression (JSExpressionParen (JSDecimal '56')))"
    it "string concatenation" $ do
        testExpr "'ab' + 'bc'"  `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('+',JSStringLiteral 'ab',JSStringLiteral 'bc')))"
        testExpr "'bc' + \"cd\""  `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('+',JSStringLiteral 'bc',JSStringLiteral \"cd\")))"
    it "object literal" $ do
        testExpr "{}"           `shouldBe` "Right (JSAstExpression (JSObjectLiteral []))"
        testExpr "{x:1}"        `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'x') [JSDecimal '1']]))"
        testExpr "{x:1,y:2}"    `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'x') [JSDecimal '1'],JSPropertyNameandValue (JSIdentifier 'y') [JSDecimal '2']]))"
        testExpr "{x:1,}"       `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'x') [JSDecimal '1'],JSComma]))"
        testExpr "{yield:1}"    `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'yield') [JSDecimal '1']]))"
        testExpr "{x}"          `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyIdentRef 'x']))"
        testExpr "{x,}"         `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyIdentRef 'x',JSComma]))"
        testExpr "{set x([a,b]=y) {this.a=a;this.b=b}}" `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyAccessor JSAccessorSet (JSIdentifier 'x') (JSOpAssign ('=',JSArrayLiteral [JSIdentifier 'a',JSComma,JSIdentifier 'b'],JSIdentifier 'y')) (JSBlock [JSOpAssign ('=',JSMemberDot (JSLiteral 'this',JSIdentifier 'a'),JSIdentifier 'a'),JSSemicolon,JSOpAssign ('=',JSMemberDot (JSLiteral 'this',JSIdentifier 'b'),JSIdentifier 'b')])]))"
        testExpr "a={if:1,interface:2}" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSIdentifier 'a',JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'if') [JSDecimal '1'],JSPropertyNameandValue (JSIdentifier 'interface') [JSDecimal '2']])))"
        testExpr "a={\n  values: 7,\n}\n"   `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSIdentifier 'a',JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'values') [JSDecimal '7'],JSComma])))"
        testExpr "x={get foo() {return 1},set foo(a) {x=a}}" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSIdentifier 'x',JSObjectLiteral [JSPropertyAccessor JSAccessorGet (JSIdentifier 'foo') () (JSBlock [JSReturn JSDecimal '1' ]),JSPropertyAccessor JSAccessorSet (JSIdentifier 'foo') (JSIdentifier 'a') (JSBlock [JSOpAssign ('=',JSIdentifier 'x',JSIdentifier 'a')])])))"
        testExpr "{evaluate:evaluate,load:function load(s){if(x)return s;1}}" `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'evaluate') [JSIdentifier 'evaluate'],JSPropertyNameandValue (JSIdentifier 'load') [JSFunctionExpression 'load' (JSIdentifier 's') (JSBlock [JSIf (JSIdentifier 'x') (JSReturn JSIdentifier 's' JSSemicolon),JSDecimal '1'])]]))"
        testExpr "obj = { name : 'A', 'str' : 'B', 123 : 'C', }" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSIdentifier 'obj',JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'name') [JSStringLiteral 'A'],JSPropertyNameandValue (JSIdentifier ''str'') [JSStringLiteral 'B'],JSPropertyNameandValue (JSIdentifier '123') [JSStringLiteral 'C'],JSComma])))"
        testExpr "{[x]:1}"      `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyNameandValue (JSPropertyComputed (JSIdentifier 'x')) [JSDecimal '1']]))"
        testExpr "{ a(x,y) {}, 'blah blah'() {} }" `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSMethodDefinition (JSIdentifier 'a') (JSIdentifier 'x',JSIdentifier 'y') (JSBlock []),JSMethodDefinition (JSIdentifier ''blah blah'') () (JSBlock [])]))"
        testExpr "{[x]() {}}"   `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSMethodDefinition (JSPropertyComputed (JSIdentifier 'x')) () (JSBlock [])]))"
        testExpr "{*a(x,y) {yield y;}}" `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSGeneratorMethodDefinition (JSIdentifier 'a') (JSIdentifier 'x',JSIdentifier 'y') (JSBlock [JSYieldExpression (JSIdentifier 'y'),JSSemicolon])]))"
        testExpr "{*[x]({y},...z) {}}"  `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSGeneratorMethodDefinition (JSPropertyComputed (JSIdentifier 'x')) (JSObjectLiteral [JSPropertyIdentRef 'y'],JSSpreadExpression (JSIdentifier 'z')) (JSBlock [])]))"
        
    it "object spread" $ do
        testExpr "{...obj}"             `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSObjectSpread (JSIdentifier 'obj')]))"
        testExpr "{a: 1, ...obj}"       `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'a') [JSDecimal '1'],JSObjectSpread (JSIdentifier 'obj')]))"
        testExpr "{...obj, b: 2}"       `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSObjectSpread (JSIdentifier 'obj'),JSPropertyNameandValue (JSIdentifier 'b') [JSDecimal '2']]))"
        testExpr "{a: 1, ...obj, b: 2}" `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'a') [JSDecimal '1'],JSObjectSpread (JSIdentifier 'obj'),JSPropertyNameandValue (JSIdentifier 'b') [JSDecimal '2']]))"
        testExpr "{...obj1, ...obj2}"   `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSObjectSpread (JSIdentifier 'obj1'),JSObjectSpread (JSIdentifier 'obj2')]))"
        testExpr "{...getObject()}"     `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSObjectSpread (JSMemberExpression (JSIdentifier 'getObject',JSArguments ()))]))"
        testExpr "{x, ...obj, y}"       `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSPropertyIdentRef 'x',JSObjectSpread (JSIdentifier 'obj'),JSPropertyIdentRef 'y']))"
        testExpr "{...obj, method() {}}" `shouldBe` "Right (JSAstExpression (JSObjectLiteral [JSObjectSpread (JSIdentifier 'obj'),JSMethodDefinition (JSIdentifier 'method') () (JSBlock [])]))"

    it "unary expression" $ do
        testExpr "delete y" `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('delete',JSIdentifier 'y')))"
        testExpr "void y"   `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('void',JSIdentifier 'y')))"
        testExpr "typeof y" `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('typeof',JSIdentifier 'y')))"
        testExpr "++y"      `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('++',JSIdentifier 'y')))"
        testExpr "--y"      `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('--',JSIdentifier 'y')))"
        testExpr "+y"       `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('+',JSIdentifier 'y')))"
        testExpr "-y"       `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('-',JSIdentifier 'y')))"
        testExpr "~y"       `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('~',JSIdentifier 'y')))"
        testExpr "!y"       `shouldBe` "Right (JSAstExpression (JSUnaryExpression ('!',JSIdentifier 'y')))"
        testExpr "y++"      `shouldBe` "Right (JSAstExpression (JSExpressionPostfix ('++',JSIdentifier 'y')))"
        testExpr "y--"      `shouldBe` "Right (JSAstExpression (JSExpressionPostfix ('--',JSIdentifier 'y')))"
        testExpr "...y"     `shouldBe` "Right (JSAstExpression (JSSpreadExpression (JSIdentifier 'y')))"


    it "new expression" $ do
        testExpr "new x()"  `shouldBe` "Right (JSAstExpression (JSMemberNew (JSIdentifier 'x',JSArguments ())))"
        testExpr "new x.y"  `shouldBe` "Right (JSAstExpression (JSNewExpression JSMemberDot (JSIdentifier 'x',JSIdentifier 'y')))"

    it "binary expression" $ do
        testExpr "x||y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('||',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x&&y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('&&',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x??y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('??',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x|y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('|',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x^y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('^',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x&y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('&',JSIdentifier 'x',JSIdentifier 'y')))"

        testExpr "x==y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('==',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x!=y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('!=',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x===y"    `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('===',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x!==y"    `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('!==',JSIdentifier 'x',JSIdentifier 'y')))"

        testExpr "x<y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('<',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x>y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('>',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x<=y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('<=',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x>=y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('>=',JSIdentifier 'x',JSIdentifier 'y')))"

        testExpr "x<<y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('<<',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x>>y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('>>',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x>>>y"    `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('>>>',JSIdentifier 'x',JSIdentifier 'y')))"

        testExpr "x+y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('+',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x-y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('-',JSIdentifier 'x',JSIdentifier 'y')))"

        testExpr "x*y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('*',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x**y"     `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('**',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x**y**z"  `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('**',JSIdentifier 'x',JSExpressionBinary ('**',JSIdentifier 'y',JSIdentifier 'z'))))"
        testExpr "2**3**2"   `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('**',JSDecimal '2',JSExpressionBinary ('**',JSDecimal '3',JSDecimal '2'))))"
        testExpr "x/y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('/',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x%y"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('%',JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x instanceof y" `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('instanceof',JSIdentifier 'x',JSIdentifier 'y')))"

    it "assign expression" $ do
        testExpr "x=1"          `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x*=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('*=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x/=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('/=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x%=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('%=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x+=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('+=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x-=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('-=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x<<=1"        `shouldBe` "Right (JSAstExpression (JSOpAssign ('<<=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x>>=1"        `shouldBe` "Right (JSAstExpression (JSOpAssign ('>>=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x>>>=1"       `shouldBe` "Right (JSAstExpression (JSOpAssign ('>>>=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x&=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('&=',JSIdentifier 'x',JSDecimal '1')))"

    it "destructuring assignment expressions (ES2015) - supported features" $ do
        -- Array destructuring assignment
        testExpr "[a, b] = arr" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSArrayLiteral [JSIdentifier 'a',JSComma,JSIdentifier 'b'],JSIdentifier 'arr')))"
        testExpr "[x, y, z] = coordinates" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSArrayLiteral [JSIdentifier 'x',JSComma,JSIdentifier 'y',JSComma,JSIdentifier 'z'],JSIdentifier 'coordinates')))"
        
        -- Object destructuring assignment
        testExpr "{a, b} = obj" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSObjectLiteral [JSPropertyIdentRef 'a',JSPropertyIdentRef 'b'],JSIdentifier 'obj')))"
        testExpr "{name, age} = person" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSObjectLiteral [JSPropertyIdentRef 'name',JSPropertyIdentRef 'age'],JSIdentifier 'person')))"
        
        -- Nested destructuring assignment
        testExpr "[a, [b, c]] = nested" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSArrayLiteral [JSIdentifier 'a',JSComma,JSArrayLiteral [JSIdentifier 'b',JSComma,JSIdentifier 'c']],JSIdentifier 'nested')))"
        testExpr "{a: {b}} = deep" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'a') [JSObjectLiteral [JSPropertyIdentRef 'b']]],JSIdentifier 'deep')))"
        
        -- Rest pattern assignment
        testExpr "[first, ...rest] = array" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSArrayLiteral [JSIdentifier 'first',JSComma,JSSpreadExpression (JSIdentifier 'rest')],JSIdentifier 'array')))"
        
        -- Sparse array assignment
        testExpr "[, , third] = sparse" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSArrayLiteral [JSComma,JSComma,JSIdentifier 'third'],JSIdentifier 'sparse')))"
        
        -- Property renaming assignment
        testExpr "{prop: newName} = obj" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'prop') [JSIdentifier 'newName']],JSIdentifier 'obj')))"
        
        -- Array destructuring with default values (parsed as assignment expressions)
        testExpr "[a = 1, b = 2] = arr" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSArrayLiteral [JSOpAssign ('=',JSIdentifier 'a',JSDecimal '1'),JSComma,JSOpAssign ('=',JSIdentifier 'b',JSDecimal '2')],JSIdentifier 'arr')))"
        testExpr "[x = 'default', y] = values" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSArrayLiteral [JSOpAssign ('=',JSIdentifier 'x',JSStringLiteral 'default'),JSComma,JSIdentifier 'y'],JSIdentifier 'values')))"
        
        -- Mixed array destructuring with defaults and rest
        testExpr "[first, second = 42, ...rest] = data" `shouldBe` "Right (JSAstExpression (JSOpAssign ('=',JSArrayLiteral [JSIdentifier 'first',JSComma,JSOpAssign ('=',JSIdentifier 'second',JSDecimal '42'),JSComma,JSSpreadExpression (JSIdentifier 'rest')],JSIdentifier 'data')))"
        testExpr "x^=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('^=',JSIdentifier 'x',JSDecimal '1')))"
        testExpr "x|=1"         `shouldBe` "Right (JSAstExpression (JSOpAssign ('|=',JSIdentifier 'x',JSDecimal '1')))"

    it "logical assignment operators" $ do
        testExpr "x&&=true"     `shouldBe` "Right (JSAstExpression (JSOpAssign ('&&=',JSIdentifier 'x',JSLiteral 'true')))"
        testExpr "x||=false"    `shouldBe` "Right (JSAstExpression (JSOpAssign ('||=',JSIdentifier 'x',JSLiteral 'false')))"
        testExpr "x??=null"     `shouldBe` "Right (JSAstExpression (JSOpAssign ('??=',JSIdentifier 'x',JSLiteral 'null')))"
        testExpr "obj.prop&&=value" `shouldBe` "Right (JSAstExpression (JSOpAssign ('&&=',JSMemberDot (JSIdentifier 'obj',JSIdentifier 'prop'),JSIdentifier 'value')))"
        testExpr "arr[0]||=defaultValue" `shouldBe` "Right (JSAstExpression (JSOpAssign ('||=',JSMemberSquare (JSIdentifier 'arr',JSDecimal '0'),JSIdentifier 'defaultValue')))"
        testExpr "config.timeout??=5000" `shouldBe` "Right (JSAstExpression (JSOpAssign ('??=',JSMemberDot (JSIdentifier 'config',JSIdentifier 'timeout'),JSDecimal '5000')))"
        testExpr "a&&=b&&=c"    `shouldBe` "Right (JSAstExpression (JSOpAssign ('&&=',JSIdentifier 'a',JSOpAssign ('&&=',JSIdentifier 'b',JSIdentifier 'c'))))"

    it "function expression" $ do
        testExpr "function(){}"     `shouldBe` "Right (JSAstExpression (JSFunctionExpression '' () (JSBlock [])))"
        testExpr "function(a){}"    `shouldBe` "Right (JSAstExpression (JSFunctionExpression '' (JSIdentifier 'a') (JSBlock [])))"
        testExpr "function(a,b){}"  `shouldBe` "Right (JSAstExpression (JSFunctionExpression '' (JSIdentifier 'a',JSIdentifier 'b') (JSBlock [])))"
        testExpr "function(...a){}" `shouldBe` "Right (JSAstExpression (JSFunctionExpression '' (JSSpreadExpression (JSIdentifier 'a')) (JSBlock [])))"
        testExpr "function(a=1){}"  `shouldBe` "Right (JSAstExpression (JSFunctionExpression '' (JSOpAssign ('=',JSIdentifier 'a',JSDecimal '1')) (JSBlock [])))"
        testExpr "function([a,b]){}" `shouldBe` "Right (JSAstExpression (JSFunctionExpression '' (JSArrayLiteral [JSIdentifier 'a',JSComma,JSIdentifier 'b']) (JSBlock [])))"
        testExpr "function([a,...b]){}" `shouldBe` "Right (JSAstExpression (JSFunctionExpression '' (JSArrayLiteral [JSIdentifier 'a',JSComma,JSSpreadExpression (JSIdentifier 'b')]) (JSBlock [])))"
        testExpr "function({a,b}){}" `shouldBe` "Right (JSAstExpression (JSFunctionExpression '' (JSObjectLiteral [JSPropertyIdentRef 'a',JSPropertyIdentRef 'b']) (JSBlock [])))"
        testExpr "a => {}"          `shouldBe` "Right (JSAstExpression (JSArrowExpression (JSIdentifier 'a') => JSConciseFunctionBody (JSBlock [])))"
        testExpr "(a) => { a + 2 }" `shouldBe` "Right (JSAstExpression (JSArrowExpression ((JSIdentifier 'a')) => JSConciseFunctionBody (JSBlock [JSExpressionBinary ('+',JSIdentifier 'a',JSDecimal '2')])))"
        testExpr "(a, b) => {}"     `shouldBe` "Right (JSAstExpression (JSArrowExpression ((JSIdentifier 'a',JSIdentifier 'b')) => JSConciseFunctionBody (JSBlock [])))"
        testExpr "(a, b) => a + b"  `shouldBe` "Right (JSAstExpression (JSArrowExpression ((JSIdentifier 'a',JSIdentifier 'b')) => JSConciseExpressionBody (JSExpressionBinary ('+',JSIdentifier 'a',JSIdentifier 'b'))))"
        testExpr "() => { 42 }"     `shouldBe` "Right (JSAstExpression (JSArrowExpression (()) => JSConciseFunctionBody (JSBlock [JSDecimal '42'])))"
        testExpr "(a, ...b) => b"   `shouldBe` "Right (JSAstExpression (JSArrowExpression ((JSIdentifier 'a',JSSpreadExpression (JSIdentifier 'b'))) => JSConciseExpressionBody (JSIdentifier 'b')))"
        testExpr "(a,b=1) => a + b" `shouldBe` "Right (JSAstExpression (JSArrowExpression ((JSIdentifier 'a',JSOpAssign ('=',JSIdentifier 'b',JSDecimal '1'))) => JSConciseExpressionBody (JSExpressionBinary ('+',JSIdentifier 'a',JSIdentifier 'b'))))"
        testExpr "([a,b]) => a + b" `shouldBe` "Right (JSAstExpression (JSArrowExpression ((JSArrayLiteral [JSIdentifier 'a',JSComma,JSIdentifier 'b'])) => JSConciseExpressionBody (JSExpressionBinary ('+',JSIdentifier 'a',JSIdentifier 'b'))))"

    it "trailing comma in function parameters" $ do
        -- Test trailing commas in function expressions
        testExpr "function(a,){}"   `shouldBe` "Right (JSAstExpression (JSFunctionExpression '' (JSIdentifier 'a') (JSBlock [])))"
        testExpr "function(a,b,){}" `shouldBe` "Right (JSAstExpression (JSFunctionExpression '' (JSIdentifier 'a',JSIdentifier 'b') (JSBlock [])))"
        -- Test named functions with trailing commas
        testExpr "function foo(x,){}" `shouldBe` "Right (JSAstExpression (JSFunctionExpression 'foo' (JSIdentifier 'x') (JSBlock [])))"
        -- Test generator functions with trailing commas
        testExpr "function*(a,){}" `shouldBe` "Right (JSAstExpression (JSGeneratorExpression '' (JSIdentifier 'a') (JSBlock [])))"
        testExpr "function* gen(x,y,){}" `shouldBe` "Right (JSAstExpression (JSGeneratorExpression 'gen' (JSIdentifier 'x',JSIdentifier 'y') (JSBlock [])))"

    it "generator expression" $ do
        testExpr "function*(){}"        `shouldBe` "Right (JSAstExpression (JSGeneratorExpression '' () (JSBlock [])))"
        testExpr "function*(a){}"       `shouldBe` "Right (JSAstExpression (JSGeneratorExpression '' (JSIdentifier 'a') (JSBlock [])))"
        testExpr "function*(a,b){}"     `shouldBe` "Right (JSAstExpression (JSGeneratorExpression '' (JSIdentifier 'a',JSIdentifier 'b') (JSBlock [])))"
        testExpr "function*(a,...b){}"  `shouldBe` "Right (JSAstExpression (JSGeneratorExpression '' (JSIdentifier 'a',JSSpreadExpression (JSIdentifier 'b')) (JSBlock [])))"
        testExpr "function*f(){}"       `shouldBe` "Right (JSAstExpression (JSGeneratorExpression 'f' () (JSBlock [])))"
        testExpr "function*f(a){}"      `shouldBe` "Right (JSAstExpression (JSGeneratorExpression 'f' (JSIdentifier 'a') (JSBlock [])))"
        testExpr "function*f(a,b){}"    `shouldBe` "Right (JSAstExpression (JSGeneratorExpression 'f' (JSIdentifier 'a',JSIdentifier 'b') (JSBlock [])))"
        testExpr "function*f(a,...b){}" `shouldBe` "Right (JSAstExpression (JSGeneratorExpression 'f' (JSIdentifier 'a',JSSpreadExpression (JSIdentifier 'b')) (JSBlock [])))"

    it "await expression" $ do
        testExpr "await fetch('/api')"              `shouldBe` "Right (JSAstExpression (JSAwaitExpresson JSMemberExpression (JSIdentifier 'fetch',JSArguments (JSStringLiteral '/api'))))"
        testExpr "await Promise.resolve(42)"        `shouldBe` "Right (JSAstExpression (JSAwaitExpresson JSMemberExpression (JSMemberDot (JSIdentifier 'Promise',JSIdentifier 'resolve'),JSArguments (JSDecimal '42'))))"
        testExpr "await (x + y)"                    `shouldBe` "Right (JSAstExpression (JSAwaitExpresson JSExpressionParen (JSExpressionBinary ('+',JSIdentifier 'x',JSIdentifier 'y'))))"
        testExpr "await x.then(y => y * 2)"         `shouldBe` "Right (JSAstExpression (JSAwaitExpresson JSMemberExpression (JSMemberDot (JSIdentifier 'x',JSIdentifier 'then'),JSArguments (JSArrowExpression (JSIdentifier 'y') => JSConciseExpressionBody (JSExpressionBinary ('*',JSIdentifier 'y',JSDecimal '2'))))))"
        testExpr "await response.json()"            `shouldBe` "Right (JSAstExpression (JSAwaitExpresson JSMemberExpression (JSMemberDot (JSIdentifier 'response',JSIdentifier 'json'),JSArguments ())))"
        testExpr "await new Promise(resolve => resolve(1))" `shouldBe` "Right (JSAstExpression (JSAwaitExpresson JSMemberNew (JSIdentifier 'Promise',JSArguments (JSArrowExpression (JSIdentifier 'resolve') => JSConciseExpressionBody (JSMemberExpression (JSIdentifier 'resolve',JSArguments (JSDecimal '1')))))))"

    it "async function expression" $ do
        testExpr "async function foo() {}"              `shouldBe` "Right (JSAstExpression (JSAsyncFunctionExpression 'foo' () (JSBlock [])))"
        testExpr "async function foo(a) {}"             `shouldBe` "Right (JSAstExpression (JSAsyncFunctionExpression 'foo' (JSIdentifier 'a') (JSBlock [])))"
        testExpr "async function foo(a, b) {}"          `shouldBe` "Right (JSAstExpression (JSAsyncFunctionExpression 'foo' (JSIdentifier 'a',JSIdentifier 'b') (JSBlock [])))"
        testExpr "async function() {}"                  `shouldBe` "Right (JSAstExpression (JSAsyncFunctionExpression '' () (JSBlock [])))"
        testExpr "async function(x) { return await x; }" `shouldBe` "Right (JSAstExpression (JSAsyncFunctionExpression '' (JSIdentifier 'x') (JSBlock [JSReturn JSAwaitExpresson JSIdentifier 'x' JSSemicolon])))"
        testExpr "async function fetch() { return await response.json(); }" `shouldBe` "Right (JSAstExpression (JSAsyncFunctionExpression 'fetch' () (JSBlock [JSReturn JSAwaitExpresson JSMemberExpression (JSMemberDot (JSIdentifier 'response',JSIdentifier 'json'),JSArguments ()) JSSemicolon])))"
        testExpr "async function handler(req, res) { const data = await db.query(); res.send(data); }" `shouldBe` "Right (JSAstExpression (JSAsyncFunctionExpression 'handler' (JSIdentifier 'req',JSIdentifier 'res') (JSBlock [JSConstant (JSVarInitExpression (JSIdentifier 'data') [JSAwaitExpresson JSMemberExpression (JSMemberDot (JSIdentifier 'db',JSIdentifier 'query'),JSArguments ())]),JSMethodCall (JSMemberDot (JSIdentifier 'res',JSIdentifier 'send'),JSArguments (JSIdentifier 'data')),JSSemicolon])))"

    it "member expression" $ do
        testExpr "x[y]"         `shouldBe` "Right (JSAstExpression (JSMemberSquare (JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x[y][z]"      `shouldBe` "Right (JSAstExpression (JSMemberSquare (JSMemberSquare (JSIdentifier 'x',JSIdentifier 'y'),JSIdentifier 'z')))"
        testExpr "x.y"          `shouldBe` "Right (JSAstExpression (JSMemberDot (JSIdentifier 'x',JSIdentifier 'y')))"
        testExpr "x.y.z"        `shouldBe` "Right (JSAstExpression (JSMemberDot (JSMemberDot (JSIdentifier 'x',JSIdentifier 'y'),JSIdentifier 'z')))"

    it "call expression" $ do
        testExpr "x()"         `shouldBe` "Right (JSAstExpression (JSMemberExpression (JSIdentifier 'x',JSArguments ())))"
        testExpr "x()()"       `shouldBe` "Right (JSAstExpression (JSCallExpression (JSMemberExpression (JSIdentifier 'x',JSArguments ()),JSArguments ())))"
        testExpr "x()[4]"      `shouldBe` "Right (JSAstExpression (JSCallExpressionSquare (JSMemberExpression (JSIdentifier 'x',JSArguments ()),JSDecimal '4')))"
        testExpr "x().x"       `shouldBe` "Right (JSAstExpression (JSCallExpressionDot (JSMemberExpression (JSIdentifier 'x',JSArguments ()),JSIdentifier 'x')))"
        testExpr "x(a,b=2).x"  `shouldBe` "Right (JSAstExpression (JSCallExpressionDot (JSMemberExpression (JSIdentifier 'x',JSArguments (JSIdentifier 'a',JSOpAssign ('=',JSIdentifier 'b',JSDecimal '2'))),JSIdentifier 'x')))"
        testExpr "foo (56.8379100, 60.5806664)" `shouldBe` "Right (JSAstExpression (JSMemberExpression (JSIdentifier 'foo',JSArguments (JSDecimal '56.8379100',JSDecimal '60.5806664'))))"
        
    it "trailing comma in function calls" $ do
        testExpr "f(x,)"       `shouldBe` "Right (JSAstExpression (JSMemberExpression (JSIdentifier 'f',JSArguments (JSIdentifier 'x'))))"
        testExpr "f(a,b,)"     `shouldBe` "Right (JSAstExpression (JSMemberExpression (JSIdentifier 'f',JSArguments (JSIdentifier 'a',JSIdentifier 'b'))))"
        testExpr "Math.max(10, 20,)" `shouldBe` "Right (JSAstExpression (JSMemberExpression (JSMemberDot (JSIdentifier 'Math',JSIdentifier 'max'),JSArguments (JSDecimal '10',JSDecimal '20'))))"
        -- Chained function calls with trailing commas
        testExpr "f(x,)(y,)"   `shouldBe` "Right (JSAstExpression (JSCallExpression (JSMemberExpression (JSIdentifier 'f',JSArguments (JSIdentifier 'x')),JSArguments (JSIdentifier 'y'))))"
        -- Complex expressions with trailing commas
        testExpr "obj.method(a + b, c * d,)" `shouldBe` "Right (JSAstExpression (JSMemberExpression (JSMemberDot (JSIdentifier 'obj',JSIdentifier 'method'),JSArguments (JSExpressionBinary ('+',JSIdentifier 'a',JSIdentifier 'b'),JSExpressionBinary ('*',JSIdentifier 'c',JSIdentifier 'd')))))"
        -- Single argument with trailing comma
        testExpr "console.log('hello',)" `shouldBe` "Right (JSAstExpression (JSMemberExpression (JSMemberDot (JSIdentifier 'console',JSIdentifier 'log'),JSArguments (JSStringLiteral 'hello'))))"

    it "dynamic imports (ES2020) - current parser limitations" $ do
        -- Note: Current parser does not support dynamic import() expressions
        -- import() is currently parsed as import statements, not expressions
        -- These tests document the existing behavior for future implementation
        parse "import('./module.js')" "test" `shouldSatisfy` (\result -> case result of Left _ -> True; Right _ -> False)
        parse "const mod = import('module')" "test" `shouldSatisfy` (\result -> case result of Left _ -> True; Right _ -> False)
        parse "import(moduleSpecifier)" "test" `shouldSatisfy` (\result -> case result of Left _ -> True; Right _ -> False)
        parse "import('./utils.js').then(m => m.helper())" "test" `shouldSatisfy` (\result -> case result of Left _ -> True; Right _ -> False)
        parse "await import('./async-module.js')" "test" `shouldSatisfy` (\result -> case result of Left _ -> True; Right _ -> False)

    it "spread expression" $
        testExpr "... x"        `shouldBe` "Right (JSAstExpression (JSSpreadExpression (JSIdentifier 'x')))"

    it "template literal" $ do
        testExpr "``"            `shouldBe` "Right (JSAstExpression (JSTemplateLiteral ((),'``',[])))"
        testExpr "`$`"           `shouldBe` "Right (JSAstExpression (JSTemplateLiteral ((),'`$`',[])))"
        testExpr "`$\\n`"        `shouldBe` "Right (JSAstExpression (JSTemplateLiteral ((),'`$\\n`',[])))"
        testExpr "`\\${x}`"      `shouldBe` "Right (JSAstExpression (JSTemplateLiteral ((),'`\\${x}`',[])))"
        testExpr "`$ {x}`"       `shouldBe` "Right (JSAstExpression (JSTemplateLiteral ((),'`$ {x}`',[])))"
        testExpr "`\n\n`"        `shouldBe` "Right (JSAstExpression (JSTemplateLiteral ((),'`\n\n`',[])))"
        testExpr "`${x+y} ${z}`" `shouldBe` "Right (JSAstExpression (JSTemplateLiteral ((),'`${',[(JSExpressionBinary ('+',JSIdentifier 'x',JSIdentifier 'y'),'} ${'),(JSIdentifier 'z','}`')])))"
        testExpr "`<${x} ${y}>`" `shouldBe` "Right (JSAstExpression (JSTemplateLiteral ((),'`<${',[(JSIdentifier 'x','} ${'),(JSIdentifier 'y','}>`')])))"
        testExpr "tag `xyz`"     `shouldBe` "Right (JSAstExpression (JSTemplateLiteral ((JSIdentifier 'tag'),'`xyz`',[])))"
        testExpr "tag()`xyz`"    `shouldBe` "Right (JSAstExpression (JSTemplateLiteral ((JSMemberExpression (JSIdentifier 'tag',JSArguments ())),'`xyz`',[])))"

    it "yield" $ do
        testExpr "yield"       `shouldBe` "Right (JSAstExpression (JSYieldExpression ()))"
        testExpr "yield a + b" `shouldBe` "Right (JSAstExpression (JSYieldExpression (JSExpressionBinary ('+',JSIdentifier 'a',JSIdentifier 'b'))))"
        testExpr "yield* g()"  `shouldBe` "Right (JSAstExpression (JSYieldFromExpression (JSMemberExpression (JSIdentifier 'g',JSArguments ()))))"

    it "class expression" $ do
        testExpr "class Foo extends Bar { a(x,y) {} *b() {} }" `shouldBe` "Right (JSAstExpression (JSClassExpression 'Foo' (JSIdentifier 'Bar') [JSMethodDefinition (JSIdentifier 'a') (JSIdentifier 'x',JSIdentifier 'y') (JSBlock []),JSGeneratorMethodDefinition (JSIdentifier 'b') () (JSBlock [])]))"
        testExpr "class { static get [a]() {}; }" `shouldBe` "Right (JSAstExpression (JSClassExpression '' () [JSClassStaticMethod (JSPropertyAccessor JSAccessorGet (JSPropertyComputed (JSIdentifier 'a')) () (JSBlock [])),JSClassSemi]))"
        testExpr "class Foo extends Bar { a(x,y) { super(x); } }" `shouldBe` "Right (JSAstExpression (JSClassExpression 'Foo' (JSIdentifier 'Bar') [JSMethodDefinition (JSIdentifier 'a') (JSIdentifier 'x',JSIdentifier 'y') (JSBlock [JSCallExpression (JSLiteral 'super',JSArguments (JSIdentifier 'x')),JSSemicolon])]))"

    it "optional chaining" $ do
        testExpr "obj?.prop"           `shouldBe` "Right (JSAstExpression (JSOptionalMemberDot (JSIdentifier 'obj',JSIdentifier 'prop')))"
        testExpr "obj?.[key]"          `shouldBe` "Right (JSAstExpression (JSOptionalMemberSquare (JSIdentifier 'obj',JSIdentifier 'key')))"
        testExpr "obj?.method()"       `shouldBe` "Right (JSAstExpression (JSMemberExpression (JSOptionalMemberDot (JSIdentifier 'obj',JSIdentifier 'method'),JSArguments ())))"
        testExpr "obj?.prop?.deep"     `shouldBe` "Right (JSAstExpression (JSOptionalMemberDot (JSOptionalMemberDot (JSIdentifier 'obj',JSIdentifier 'prop'),JSIdentifier 'deep')))"
        testExpr "obj?.method?.(args)" `shouldBe` "Right (JSAstExpression (JSOptionalCallExpression (JSOptionalMemberDot (JSIdentifier 'obj',JSIdentifier 'method'),JSArguments (JSIdentifier 'args'))))"
        testExpr "arr?.[0]?.value"     `shouldBe` "Right (JSAstExpression (JSOptionalMemberDot (JSOptionalMemberSquare (JSIdentifier 'arr',JSDecimal '0'),JSIdentifier 'value')))"

    it "nullish coalescing precedence" $ do
        testExpr "x ?? y || z"         `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('||',JSExpressionBinary ('??',JSIdentifier 'x',JSIdentifier 'y'),JSIdentifier 'z')))"
        testExpr "x || y ?? z"         `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('||',JSIdentifier 'x',JSExpressionBinary ('??',JSIdentifier 'y',JSIdentifier 'z'))))"
        testExpr "null ?? 'default'"   `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('??',JSLiteral 'null',JSStringLiteral 'default')))"
        testExpr "undefined ?? 0"      `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('??',JSIdentifier 'undefined',JSDecimal '0')))"
        testExpr "x ?? y ?? z"         `shouldBe` "Right (JSAstExpression (JSExpressionBinary ('??',JSExpressionBinary ('??',JSIdentifier 'x',JSIdentifier 'y'),JSIdentifier 'z')))"

    it "static class expressions (ES2015) - supported features" $ do
        -- Basic static method in class expression
        testExpr "class { static method() {} }" `shouldBe` "Right (JSAstExpression (JSClassExpression '' () [JSClassStaticMethod (JSMethodDefinition (JSIdentifier 'method') () (JSBlock []))]))"
        -- Named class expression with static methods
        testExpr "class Calculator { static add(a, b) { return a + b; } }" `shouldBe` "Right (JSAstExpression (JSClassExpression 'Calculator' () [JSClassStaticMethod (JSMethodDefinition (JSIdentifier 'add') (JSIdentifier 'a',JSIdentifier 'b') (JSBlock [JSReturn JSExpressionBinary ('+',JSIdentifier 'a',JSIdentifier 'b') JSSemicolon]))]))"
        -- Static getter in class expression  
        testExpr "class { static get version() { return '2.0'; } }" `shouldBe` "Right (JSAstExpression (JSClassExpression '' () [JSClassStaticMethod (JSPropertyAccessor JSAccessorGet (JSIdentifier 'version') () (JSBlock [JSReturn JSStringLiteral '2.0' JSSemicolon]))]))"
        -- Static setter in class expression
        testExpr "class { static set config(val) { this._config = val; } }" `shouldBe` "Right (JSAstExpression (JSClassExpression '' () [JSClassStaticMethod (JSPropertyAccessor JSAccessorSet (JSIdentifier 'config') (JSIdentifier 'val') (JSBlock [JSOpAssign ('=',JSMemberDot (JSLiteral 'this',JSIdentifier '_config'),JSIdentifier 'val'),JSSemicolon]))]))"
        -- Static computed property
        testExpr "class { static [Symbol.iterator]() {} }" `shouldBe` "Right (JSAstExpression (JSClassExpression '' () [JSClassStaticMethod (JSMethodDefinition (JSPropertyComputed (JSMemberDot (JSIdentifier 'Symbol',JSIdentifier 'iterator'))) () (JSBlock []))]))"
        -- Multiple static features
        testExpr "class Util { static method() {} static get prop() {} }" `shouldBe` "Right (JSAstExpression (JSClassExpression 'Util' () [JSClassStaticMethod (JSMethodDefinition (JSIdentifier 'method') () (JSBlock [])),JSClassStaticMethod (JSPropertyAccessor JSAccessorGet (JSIdentifier 'prop') () (JSBlock []))]))"


testExpr :: String -> String
testExpr str = showStrippedMaybe (parseUsing parseExpression str "src")
