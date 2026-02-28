module Integration.Language.Javascript.Parser.Minification
  ( testMinifyExpr,
    testMinifyStmt,
    testMinifyProg,
    testMinifyModule,
  )
where

import Control.Monad (forM_)
import Language.JavaScript.Parser
import qualified Language.JavaScript.Parser.AST as AST
import Language.JavaScript.Parser.Parser
import Language.JavaScript.Process.Minify
import Test.Hspec

testMinifyExpr :: Spec
testMinifyExpr = describe "Minify expressions:" $ do
  it "terminals" $ do
    minifyExpr " identifier " `shouldBe` "identifier"
    minifyExpr " 1 " `shouldBe` "1"
    minifyExpr " this " `shouldBe` "this"
    minifyExpr " 0x12ab " `shouldBe` "0x12ab"
    minifyExpr " 0567 " `shouldBe` "0o567"
    minifyExpr " 'helo' " `shouldBe` "'helo'"
    minifyExpr " \"good bye\" " `shouldBe` "\"good bye\""
    minifyExpr " /\\n/g " `shouldBe` "/\\n/g"

  it "array literals" $ do
    minifyExpr " [ ] " `shouldBe` "[]"
    minifyExpr " [ , ] " `shouldBe` "[,]"
    minifyExpr " [ , , ] " `shouldBe` "[,,]"
    minifyExpr " [ x ] " `shouldBe` "[x]"
    minifyExpr " [ x , y ] " `shouldBe` "[x,y]"

  it "object literals" $ do
    minifyExpr " { } " `shouldBe` "{}"
    minifyExpr " { a : 1 } " `shouldBe` "{a:1}"
    minifyExpr " { b : 2 , } " `shouldBe` "{b:2}"
    minifyExpr " { c : 3 , d : 4 , } " `shouldBe` "{c:3,d:4}"
    minifyExpr " { 'str' : true , 42 : false , } " `shouldBe` "{'str':true,42:false}"
    minifyExpr " { x , } " `shouldBe` "{x}"
    minifyExpr " { [ x + y ] : 1 } " `shouldBe` "{[x+y]:1}"
    minifyExpr " { a ( x, y ) { } } " `shouldBe` "{a(x,y){}}"
    minifyExpr " { [ x + y ] ( ) { } } " `shouldBe` "{[x+y](){}}"
    minifyExpr " { * a ( x, y ) { } } " `shouldBe` "{*a(x,y){}}"
    minifyExpr " { ... obj } " `shouldBe` "{...obj}"
    minifyExpr " { a : 1 , ... obj } " `shouldBe` "{a:1,...obj}"
    minifyExpr " { ... obj , b : 2 } " `shouldBe` "{...obj,b:2}"
    minifyExpr " { ... obj1 , ... obj2 } " `shouldBe` "{...obj1,...obj2}"

  it "parentheses" $ do
    minifyExpr " ( 'hello' ) " `shouldBe` "('hello')"
    minifyExpr " ( 12 ) " `shouldBe` "(12)"
    minifyExpr " ( 1 + 2 ) " `shouldBe` "(1+2)"

  it "unary" $ do
    minifyExpr " a -- " `shouldBe` "a--"
    minifyExpr " delete b " `shouldBe` "delete b"
    minifyExpr " c ++ " `shouldBe` "c++"
    minifyExpr " - d " `shouldBe` "-d"
    minifyExpr " ! e " `shouldBe` "!e"
    minifyExpr " + f " `shouldBe` "+f"
    minifyExpr " ~ g " `shouldBe` "~g"
    minifyExpr " typeof h " `shouldBe` "typeof h"
    minifyExpr " void i " `shouldBe` "void i"

  it "binary" $ do
    minifyExpr " a && z " `shouldBe` "a&&z"
    minifyExpr " b & z " `shouldBe` "b&z"
    minifyExpr " c | z " `shouldBe` "c|z"
    minifyExpr " d ^ z " `shouldBe` "d^z"
    minifyExpr " e / z " `shouldBe` "e/z"
    minifyExpr " f == z " `shouldBe` "f==z"
    minifyExpr " g >= z " `shouldBe` "g>=z"
    minifyExpr " h > z " `shouldBe` "h>z"
    minifyExpr " i in z " `shouldBe` "i in z"
    minifyExpr " j instanceof z " `shouldBe` "j instanceof z"
    minifyExpr " k <= z " `shouldBe` "k<=z"
    minifyExpr " l << z " `shouldBe` "l<<z"
    minifyExpr " m < z " `shouldBe` "m<z"
    minifyExpr " n - z " `shouldBe` "n-z"
    minifyExpr " o % z " `shouldBe` "o%z"
    minifyExpr " p != z " `shouldBe` "p!=z"
    minifyExpr " q || z " `shouldBe` "q||z"
    minifyExpr " r + z " `shouldBe` "r+z"
    minifyExpr " s >> z " `shouldBe` "s>>z"
    minifyExpr " t === z " `shouldBe` "t===z"
    minifyExpr " u !== z " `shouldBe` "u!==z"
    minifyExpr " v * z " `shouldBe` "v*z"
    minifyExpr " x ** z " `shouldBe` "x**z"
    minifyExpr " w >>> z " `shouldBe` "w>>>z"

  it "ternary" $ do
    minifyExpr "  true ? 1 : 2 " `shouldBe` "true?1:2"
    minifyExpr "  x ? y + 1 : j - 1 " `shouldBe` "x?y+1:j-1"

  it "member access" $ do
    minifyExpr " a . b " `shouldBe` "a.b"
    minifyExpr " c . d . e " `shouldBe` "c.d.e"

  it "new" $ do
    minifyExpr " new f ( ) " `shouldBe` "new f()"
    minifyExpr " new g ( 1 ) " `shouldBe` "new g(1)"
    minifyExpr " new h ( 1 , 2 ) " `shouldBe` "new h(1,2)"
    minifyExpr " new k . x " `shouldBe` "new k.x"

  it "array access" $ do
    minifyExpr " i [ a ] " `shouldBe` "i[a]"
    minifyExpr " j [ a ] [ b ]" `shouldBe` "j[a][b]"

  it "function" $ do
    minifyExpr " function ( ) { } " `shouldBe` "function(){}"
    minifyExpr " function ( a ) { } " `shouldBe` "function(a){}"
    minifyExpr " function ( a , b ) { return a + b ; } " `shouldBe` "function(a,b){return a+b}"
    minifyExpr " function ( a , ...b ) { return b ; } " `shouldBe` "function(a,...b){return b}"
    minifyExpr " function ( a = 1 , b = 2 ) { return a + b ; } " `shouldBe` "function(a=1,b=2){return a+b}"
    minifyExpr " function ( [ a , b ] ) { return b ; } " `shouldBe` "function([a,b]){return b}"
    minifyExpr " function ( { a , b , } ) { return a + b ; } " `shouldBe` "function({a,b}){return a+b}"

    minifyExpr "a => {}" `shouldBe` "a=>{}"
    minifyExpr "(a) => {}" `shouldBe` "(a)=>{}"
    minifyExpr "( a ) => { a + 2 }" `shouldBe` "(a)=>a+2"
    minifyExpr "(a, b) => a + b" `shouldBe` "(a,b)=>a+b"
    minifyExpr "() => { 42 }" `shouldBe` "()=>42"
    minifyExpr "(a, ...b) => b" `shouldBe` "(a,...b)=>b"
    minifyExpr "(a = 1, b = 2) => a + b" `shouldBe` "(a=1,b=2)=>a+b"
    minifyExpr "( [ a , b ] ) => a + b" `shouldBe` "([a,b])=>a+b"
    minifyExpr "( { a , b , } ) => a + b" `shouldBe` "({a,b})=>a+b"

  it "generator" $ do
    minifyExpr " function * ( ) { } " `shouldBe` "function*(){}"
    minifyExpr " function * ( a ) { yield * a ; } " `shouldBe` "function*(a){yield*a}"
    minifyExpr " function * ( a , b ) { yield a + b ; } " `shouldBe` "function*(a,b){yield a+b}"

  it "calls" $ do
    minifyExpr " a ( ) " `shouldBe` "a()"
    minifyExpr " b ( ) ( ) " `shouldBe` "b()()"
    minifyExpr " c ( ) [ x ] " `shouldBe` "c()[x]"
    minifyExpr " d ( ) . y " `shouldBe` "d().y"

  it "property accessor" $ do
    minifyExpr " { get foo ( ) { return x } } " `shouldBe` "{get foo(){return x}}"
    minifyExpr " { set foo ( a ) { x = a } } " `shouldBe` "{set foo(a){x=a}}"
    minifyExpr " { set foo ( [ a , b ] ) { x = a } } " `shouldBe` "{set foo([a,b]){x=a}}"

  it "string concatenation" $ do
    minifyExpr " 'ab' + \"cd\" " `shouldBe` "'abcd'"
    minifyExpr " \"bc\" + 'de' " `shouldBe` "'bcde'"
    minifyExpr " \"cd\" + 'ef' + 'gh' " `shouldBe` "'cdefgh'"

    minifyExpr " 'de' + '\"fg\"' + 'hi' " `shouldBe` "'de\"fg\"hi'"
    minifyExpr " 'ef' + \"'gh'\" + 'ij' " `shouldBe` "'ef\\'gh\\'ij'"

  -- minifyExpr " 'de' + '\"fg\"' + 'hi' " `shouldBe` "'de\"fg\"hi'"
  -- minifyExpr " 'ef' + \"'gh'\" + 'ij' " `shouldBe` "'ef'gh'ij'"

  it "spread exporession" $
    minifyExpr " ... x " `shouldBe` "...x"

  it "template literal" $ do
    minifyExpr " ` a + b + ${ c + d } + ... ` " `shouldBe` "` a + b + ${c+d} + ... `"
    minifyExpr " tagger () ` a + b ` " `shouldBe` "tagger()` a + b `"

  it "class" $ do
    minifyExpr " class   Foo   {\n  a() {\n    return 0;\n  };\n  static [ b ] ( x ) {}\n } " `shouldBe` "class Foo{a(){return 0}static[b](x){}}"
    minifyExpr " class { static get a() { return 0; } static set a(v) {} } " `shouldBe` "class{static get a(){return 0}static set a(v){}}"
    minifyExpr " class   { ; ; ; } " `shouldBe` "class{}"
    minifyExpr " class Foo extends Bar {} " `shouldBe` "class Foo extends Bar{}"
    minifyExpr " class extends (getBase()) {} " `shouldBe` "class extends(getBase()){}"
    minifyExpr " class extends [ Bar1, Bar2 ][getBaseIndex()] {} " `shouldBe` "class extends[Bar1,Bar2][getBaseIndex()]{}"

testMinifyStmt :: Spec
testMinifyStmt = describe "Minify statements:" $ do
  forM_ ["break", "continue", "return"] $ \kw ->
    it kw $ do
      minifyStmt (" " ++ kw ++ " ; ") `shouldBe` kw
      minifyStmt (" {" ++ kw ++ " ;} ") `shouldBe` kw
      minifyStmt (" " ++ kw ++ " x ; ") `shouldBe` (kw ++ " x")
      minifyStmt ("\n\n" ++ kw ++ " x ;\n") `shouldBe` (kw ++ " x")

  it "block" $ do
    minifyStmt "\n{ a = 1\nb = 2\n } " `shouldBe` "{a=1;b=2}"
    minifyStmt " { c = 3 ; d = 4 ; } " `shouldBe` "{c=3;d=4}"
    minifyStmt " { ; e = 1 } " `shouldBe` "e=1"
    minifyStmt " { { } ; f = 1 ; { } ; } ; " `shouldBe` "f=1"

  it "if" $ do
    minifyStmt " if ( 1 ) return ; " `shouldBe` "if(1)return"
    minifyStmt " if ( 1 ) ; " `shouldBe` "if(1);"

  it "if/else" $ do
    minifyStmt " if ( a ) ; else break ; " `shouldBe` "if(a);else break"
    minifyStmt " if ( b ) break ; else break ; " `shouldBe` "if(b){break}else break"
    minifyStmt " if ( c ) continue ; else continue ; " `shouldBe` "if(c){continue}else continue"
    minifyStmt " if ( d ) return ; else return ; " `shouldBe` "if(d){return}else return"
    minifyStmt " if ( e ) { b = 1 } else c = 2 ;" `shouldBe` "if(e){b=1}else c=2"
    minifyStmt " if ( f ) { b = 1 } else { c = 2 ; d = 4 ; } ;" `shouldBe` "if(f){b=1}else{c=2;d=4}"
    minifyStmt " if ( g ) { ex ; } else { ex ; } ; " `shouldBe` "if(g){ex}else ex"
    minifyStmt " if ( h ) ; else if ( 2 ){ 3 ; } " `shouldBe` "if(h);else if(2)3"

  it "while" $ do
    minifyStmt " while ( x < 2 ) x ++ ; " `shouldBe` "while(x<2)x++"
    minifyStmt " while ( x < 0x12 && y > 1 ) { x *= 3 ; y += 1 ; } ; " `shouldBe` "while(x<0x12&&y>1){x*=3;y+=1}"

  it "do/while" $ do
    minifyStmt " do x = foo (y) ; while ( x < y ) ; " `shouldBe` "do{x=foo(y)}while(x<y)"
    minifyStmt " do { x = foo (x, y) ; y -- ; } while ( x > y ) ; " `shouldBe` "do{x=foo(x,y);y--}while(x>y)"

  it "for" $ do
    minifyStmt " for ( ; ; ) ; " `shouldBe` "for(;;);"
    minifyStmt " for ( k = 0 ; k <= 10 ; k ++ ) ; " `shouldBe` "for(k=0;k<=10;k++);"
    minifyStmt " for ( k = 0, j = 1 ; k <= 10 && j < 10 ; k ++ , j -- ) ; " `shouldBe` "for(k=0,j=1;k<=10&&j<10;k++,j--);"
    minifyStmt " for (var x ; y ; z) { } " `shouldBe` "for(var x;y;z){}"
    minifyStmt " for ( x in 5 ) foo (x) ;" `shouldBe` "for(x in 5)foo(x)"
    minifyStmt " for ( var x in 5 ) { foo ( x++ ); y ++ ; } ;" `shouldBe` "for(var x in 5){foo(x++);y++}"
    minifyStmt " for (let x ; y ; z) { } " `shouldBe` "for(let x;y;z){}"
    minifyStmt " for ( let x in 5 ) { foo ( x++ ); y ++ ; } ;" `shouldBe` "for(let x in 5){foo(x++);y++}"
    minifyStmt " for ( let x of 5 ) { foo ( x++ ); y ++ ; } ;" `shouldBe` "for(let x of 5){foo(x++);y++}"
    minifyStmt " for (const x ; y ; z) { } " `shouldBe` "for(const x;y;z){}"
    minifyStmt " for ( const x in 5 ) { foo ( x ); y ++ ; } ;" `shouldBe` "for(const x in 5){foo(x);y++}"
    minifyStmt " for ( const x of 5 ) { foo ( x ); y ++ ; } ;" `shouldBe` "for(const x of 5){foo(x);y++}"
    minifyStmt " for ( x of 5 ) { foo ( x++ ); y ++ ; } ;" `shouldBe` "for(x of 5){foo(x++);y++}"
    minifyStmt " for ( var x of 5 ) { foo ( x++ ); y ++ ; } ;" `shouldBe` "for(var x of 5){foo(x++);y++}"
  it "labelled" $ do
    minifyStmt " start : while ( true ) { if ( i ++ < 3 ) continue start ; break ; } ; " `shouldBe` "start:while(true){if(i++<3)continue start;break}"
    minifyStmt " { k ++ ; start : while ( true ) { if ( i ++ < 3 ) continue start ; break ; } ; } ; " `shouldBe` "{k++;start:while(true){if(i++<3)continue start;break}}"

  it "function" $ do
    minifyStmt " function f ( ) { } ; " `shouldBe` "function f(){}"
    minifyStmt " function f ( a ) { } ; " `shouldBe` "function f(a){}"
    minifyStmt " function f ( a , b ) { return a + b ; } ; " `shouldBe` "function f(a,b){return a+b}"
    minifyStmt " function f ( a , ... b ) { return b ; } ; " `shouldBe` "function f(a,...b){return b}"
    minifyStmt " function f ( a = 1 , b = 2 ) { return a + b ; } ; " `shouldBe` "function f(a=1,b=2){return a+b}"
    minifyStmt " function f ( [ a , b ] ) { return a + b ; } ; " `shouldBe` "function f([a,b]){return a+b}"
    minifyStmt " function f ( { a , b , } ) { return a + b ; } ; " `shouldBe` "function f({a,b}){return a+b}"
    minifyStmt " async function f ( ) { } " `shouldBe` "async function f(){}"

  it "generator" $ do
    minifyStmt " function * f ( ) { } ; " `shouldBe` "function*f(){}"
    minifyStmt " function * f ( a ) { yield * a ; } ; " `shouldBe` "function*f(a){yield*a}"
    minifyStmt " function * f ( a , b ) { yield a + b ; } ; " `shouldBe` "function*f(a,b){yield a+b}"

  it "with" $ do
    minifyStmt " with ( x ) { } ; " `shouldBe` "with(x){}"
    minifyStmt " with ({ first: 'John' }) { foo ('Hello '+first); }" `shouldBe` "with({first:'John'})foo('Hello '+first)"

  it "throw" $ do
    minifyStmt " throw a " `shouldBe` "throw a"
    minifyStmt " throw b ; " `shouldBe` "throw b"
    minifyStmt " { throw c ; } ;" `shouldBe` "throw c"

  it "switch" $ do
    minifyStmt " switch ( a ) { } ; " `shouldBe` "switch(a){}"
    minifyStmt " switch ( b ) { case 1 : 1 ; case 2 : 2 ; } ;" `shouldBe` "switch(b){case 1:1;case 2:2}"
    minifyStmt " switch ( c ) { case 1 : case 'a': case \"b\" : break ; default : break ; } ; " `shouldBe` "switch(c){case 1:case'a':case\"b\":break;default:break}"
    minifyStmt " switch ( d ) { default : if (a) {x} else y ; if (b) { x } else y ; }" `shouldBe` "switch(d){default:if(a){x}else y;if(b){x}else y}"

  it "try/catch/finally" $ do
    minifyStmt " try { } catch ( a ) { } " `shouldBe` "try{}catch(a){}"
    minifyStmt " try { b } finally { } " `shouldBe` "try{b}finally{}"
    minifyStmt " try { } catch ( c ) { } finally { } " `shouldBe` "try{}catch(c){}finally{}"
    minifyStmt " try { } catch ( d ) { } catch ( x ){ } finally { } " `shouldBe` "try{}catch(d){}catch(x){}finally{}"
    minifyStmt " try { } catch ( e ) { } catch ( y ) { } " `shouldBe` "try{}catch(e){}catch(y){}"
    minifyStmt " try { } catch ( f  if f == x ) { } catch ( z ) { } " `shouldBe` "try{}catch(f if f==x){}catch(z){}"

  it "variable declaration" $ do
    minifyStmt " var a  " `shouldBe` "var a"
    minifyStmt " var b ; " `shouldBe` "var b"
    minifyStmt " var c = 1 ; " `shouldBe` "var c=1"
    minifyStmt " var d = 1, x = 2 ; " `shouldBe` "var d=1,x=2"
    minifyStmt " let c = 1 ; " `shouldBe` "let c=1"
    minifyStmt " let d = 1, x = 2 ; " `shouldBe` "let d=1,x=2"
    minifyStmt " const { a : [ b , c ] } = d; " `shouldBe` "const{a:[b,c]}=d"

  it "string concatenation" $
    minifyStmt " f (\"ab\"+\"cd\") " `shouldBe` "f('abcd')"

  it "class" $ do
    minifyStmt " class   Foo   {\n  a() {\n    return 0;\n  }\n  static b ( x ) {}\n } " `shouldBe` "class Foo{a(){return 0}static b(x){}}"
    minifyStmt " class Foo extends Bar {} " `shouldBe` "class Foo extends Bar{}"
    minifyStmt " class Foo extends (getBase()) {} " `shouldBe` "class Foo extends(getBase()){}"
    minifyStmt " class Foo extends [ Bar1, Bar2 ][getBaseIndex()] {} " `shouldBe` "class Foo extends[Bar1,Bar2][getBaseIndex()]{}"

  it "miscellaneous" $
    minifyStmt " let r = await p ; " `shouldBe` "let r=await p"

testMinifyProg :: Spec
testMinifyProg = describe "Minify programs:" $ do
  it "simple" $ do
    minifyProg " a = f ? e : g ; " `shouldBe` "a=f?e:g"
    minifyProg " for ( i = 0 ; ; ) { ; var t = 1 ; } " `shouldBe` "for(i=0;;)var t=1"
  it "if" $
    minifyProg " if ( x ) { } ; t ; " `shouldBe` "if(x);t"
  it "if/else" $ do
    minifyProg " if ( a ) { } else { } ; break ; " `shouldBe` "if(a){}else;break"
    minifyProg " if ( b ) {x = 1} else {x = 2} f () ; " `shouldBe` "if(b){x=1}else x=2;f()"
  it "empty block" $ do
    minifyProg " a = 1 ; { } ; " `shouldBe` "a=1"
    minifyProg " { } ; b = 1 ; " `shouldBe` "b=1"
  it "empty statement" $ do
    minifyProg " a = 1 + b ; c ; ; { d ; } ; " `shouldBe` "a=1+b;c;d"
    minifyProg " b = a + 2 ; c ; { d ; } ; ; " `shouldBe` "b=a+2;c;d"
  it "nested block" $ do
    minifyProg "{a;;x;};y;z;;" `shouldBe` "a;x;y;z"
    minifyProg "{b;;{x;y;};};z;;" `shouldBe` "b;x;y;z"
  it "functions" $
    minifyProg " function f() {} ; function g() {} ;" `shouldBe` "function f(){}\nfunction g(){}"
  it "variable declaration" $ do
    minifyProg " var a = 1 ; var b = 2 ;" `shouldBe` "var a=1,b=2"
    minifyProg " var c=1;var d=2;var e=3;" `shouldBe` "var c=1,d=2,e=3"
    minifyProg " const f = 1 ; const g = 2 ;" `shouldBe` "const f=1,g=2"
    minifyProg " var h = 1 ; const i = 2 ;" `shouldBe` "var h=1;const i=2"
  it "try/catch/finally" $
    minifyProg " try { } catch (a) {} finally {} ; try { } catch ( b ) { } ; " `shouldBe` "try{}catch(a){}finally{}try{}catch(b){}"

testMinifyModule :: Spec
testMinifyModule = describe "Minify modules:" $ do
  it "import" $ do
    minifyModule "import  def  from 'mod' ; " `shouldBe` "import def from'mod'"
    minifyModule "import   *  as  foo  from   \"mod\"  ; " `shouldBe` "import * as foo from\"mod\""
    minifyModule "import  def, * as foo  from   \"mod\"  ; " `shouldBe` "import def,* as foo from\"mod\""
    minifyModule "import  { baz,  bar as   foo }  from   \"mod\"  ; " `shouldBe` "import{baz,bar as foo}from\"mod\""
    minifyModule "import  def, { baz,  bar as   foo }  from   \"mod\"  ; " `shouldBe` "import def,{baz,bar as foo}from\"mod\""
    minifyModule "import     \"mod\"  ; " `shouldBe` "import\"mod\""

  it "export" $ do
    minifyModule " export { } ; " `shouldBe` "export{}"
    minifyModule " export { a } ; " `shouldBe` "export{a}"
    minifyModule " export { a, b } ; " `shouldBe` "export{a,b}"
    minifyModule " export { a, b as c , d } ; " `shouldBe` "export{a,b as c,d}"
    minifyModule " export { } from \"mod\" ; " `shouldBe` "export{}from\"mod\""
    minifyModule " export * from \"mod\" ; " `shouldBe` "export*from\"mod\""
    minifyModule " export   *   from   'module'   ; " `shouldBe` "export*from'module'"
    minifyModule " export * from './relative/path' ; " `shouldBe` "export*from'./relative/path'"
    minifyModule " export const a = 1 ; " `shouldBe` "export const a=1"
    minifyModule " export function f () {  } ; " `shouldBe` "export function f(){}"
    minifyModule " export function * f () {  } ; " `shouldBe` "export function*f(){}"

-- -----------------------------------------------------------------------------
-- Minify test helpers.

minifyExpr :: String -> String
minifyExpr = minifyWith parseExpression

minifyStmt :: String -> String
minifyStmt = minifyWith parseStatement

minifyProg :: String -> String
minifyProg = minifyWith parseProgram

minifyModule :: String -> String
minifyModule = minifyWith parseModule

minifyWith :: (String -> String -> Either String AST.JSAST) -> String -> String
minifyWith p str = either id (renderToString . minifyJS) (p str "src")
