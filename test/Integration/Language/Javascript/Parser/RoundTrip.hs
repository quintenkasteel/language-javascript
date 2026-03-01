module Integration.Language.Javascript.Parser.RoundTrip
  ( testRoundTrip,
    testES6RoundTrip,
  )
where

import Language.JavaScript.Parser
import qualified Language.JavaScript.Parser.AST as AST
import Test.Hspec

testRoundTrip :: Spec
testRoundTrip = describe "Roundtrip:" $ do
  it "multi comment" $ do
    testRT "/*a*/\n//foo\nnull"
    testRT "/*a*/x"
    testRT "/*a*/null"
    testRT "/*b*/false"
    testRT "true/*c*/"
    testRT "/*c*/true"
    testRT "/*d*/0x1234ff"
    testRT "/*e*/10000"
    testRT "/*x*/0o11"
    testRT "/*f*/\"hello\\nworld\""
    testRT "/*g*/'hello\\nworld'"
    testRT "/*h*/this"
    testRT "/*i*//blah/"
    testRT "//j\nthis_"

  it "arrays" $ do
    testRT "/*a*/[/*b*/]"
    testRT "/*a*/[/*b*/,/*c*/]"
    testRT "/*a*/[/*b*/,/*c*/,/*d*/]"
    testRT "/*a*/[/*b/*,/*c*/,/*d*/x/*e*/]"
    testRT "/*a*/[/*b*/,/*c*/,/*d*/x/*e*/]"
    testRT "/*a*/[/*b*/,/*c*/x/*d*/,/*e*/,/*f*/x/*g*/]"
    testRT "/*a*/[/*b*/x/*c*/]"
    testRT "/*a*/[/*b*/x/*c*/,/*d*/]"

  it "object literals" $ do
    testRT "/*a*/{/*b*/}"
    testRT "/*a*/{/*b*/x/*c*/:/*d*/1/*e*/}"
    testRT "/*a*/{/*b*/x/*c*/}"
    testRT "/*a*/{/*b*/of/*c*/}"
    testRT "x=/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/y/*g*/:/*h*/2/*i*/}"
    testRT "x=/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/y/*g*/:/*h*/2/*i*/,/*j*/z/*k*/:/*l*/3/*m*/}"
    testRT "a=/*a*/{/*b*/x/*c*/:/*d*/1/*e*/,/*f*/}"
    testRT "z=/*a*/{/*b*/[/*c*/x/*d*/+/*e*/y/*f*/]/*g*/:/*h*/1/*i*/}"
    testRT "/*a*/{/*b*/a/*c*/(/*d*/x/*e*/,/*f*/y/*g*/)/*h*/{/*i*/}/*j*/}"
    testRT "/*a*/{/*b*/[/*c*/x/*d*/+/*e*/y/*f*/]/*g*/(/*h*/)/*i*/{/*j*/}/*k*/}"
    testRT "z=/*a*/{/*b*/*/*c*/a/*d*/(/*e*/x/*f*/,/*g*/y/*h*/)/*i*/{/*j*/}/*k*/}"

  it "miscellaneous" $ do
    testRT "/*a*/(/*b*/56/*c*/)"
    testRT "/*a*/true/*b*/?/*c*/1/*d*/:/*e*/2"
    testRT "/*a*/x/*b*/||/*c*/y"
    testRT "/*a*/x/*b*/&&/*c*/y"
    testRT "/*a*/x/*b*/|/*c*/y"
    testRT "/*a*/x/*b*/^/*c*/y"
    testRT "/*a*/x/*b*/&/*c*/y"
    testRT "/*a*/x/*b*/==/*c*/y"
    testRT "/*a*/x/*b*/!=/*c*/y"
    testRT "/*a*/x/*b*/===/*c*/y"
    testRT "/*a*/x/*b*/!==/*c*/y"
    testRT "/*a*/x/*b*/</*c*/y"
    testRT "/*a*/x/*b*/>/*c*/y"
    testRT "/*a*/x/*b*/<=/*c*/y"
    testRT "/*a*/x/*b*/>=/*c*/y"
    testRT "/*a*/x/*b*/**/*c*/y"
    testRT "/*a*/x /*b*/instanceof /*c*/y"
    testRT "/*a*/x/*b*/=/*c*/{/*d*/get/*e*/ foo/*f*/(/*g*/)/*h*/ {/*i*/return/*j*/ 1/*k*/}/*l*/,/*m*/set/*n*/ foo/*o*/(/*p*/a/*q*/) /*r*/{/*s*/x/*t*/=/*u*/a/*v*/}/*w*/}"
    testRT "x = { set foo(/*a*/[/*b*/a/*c*/,/*d*/b/*e*/]/*f*/=/*g*/y/*h*/) {} }"
    testRT "... /*a*/ x"

    testRT "a => {}"
    testRT "(a) => { a + 2 }"
    testRT "(a, b) => {}"
    testRT "(a, b) => a + b"
    testRT "() => { 42 }"
    testRT "(...a) => a"
    testRT "(a=1, b=2) => a + b"
    testRT "([a, b]) => a + b"
    testRT "({a, b}) => a + b"

    testRT "function (...a) {}"
    testRT "function (a=1, b=2) {}"
    testRT "function ([a, ...b]) {}"
    testRT "function ({a, b: c}) {}"

    testRT "/*a*/function/*b*/*/*c*/f/*d*/(/*e*/)/*f*/{/*g*/yield/*h*/a/*i*/}/*j*/"
    testRT "function*(a, b) { yield a ; yield b ; }"

    testRT "/*a*/`<${/*b*/x/*c*/}>`/*d*/"
    testRT "`\\${}`"
    testRT "`\n\n`"
    testRT "{}+``"
  --  https://github.com/erikd/language-javascript/issues/104

  it "statement" $ do
    testRT "if (1) {}"
    testRT "if (1) {} else {}"
    testRT "if (1) x=1; else {}"
    testRT "do {x=1} while (true);"
    testRT "do x=x+1;while(x<4);"
    testRT "while(true);"
    testRT "for(;;);"
    testRT "for(x=1;x<10;x++);"
    testRT "for(var x;;);"
    testRT "for(var x=1;;);"
    testRT "for(var x;y;z){}"
    testRT "for(x in 5){}"
    testRT "for(var x in 5){}"
    testRT "for(let x;y;z){}"
    testRT "for(let x in 5){}"
    testRT "for(let x of 5){}"
    testRT "for(const x;y;z){}"
    testRT "for(const x in 5){}"
    testRT "for(const x of 5){}"
    testRT "for(x of 5){}"
    testRT "for(var x of 5){}"
    testRT "var x=1;"
    testRT "const x=1,y=2;"
    testRT "continue;"
    testRT "continue x;"
    testRT "break;"
    testRT "break x;"
    testRT "return;"
    testRT "return x;"
    testRT "with (x) {};"
    testRT "abc:x=1"
    testRT "switch (x) {}"
    testRT "switch (x) {case 1:break;}"
    testRT "switch (x) {case 0:\ncase 1:break;}"
    testRT "switch (x) {default:break;}"
    testRT "switch (x) {default:\ncase 1:break;}"
    testRT "var x=1;let y=2;"
    testRT "var [x, y]=z;"
    testRT "let {x: [y]}=z;"
    testRT "let yield=1"

  it "module" $ do
    testRTModule "import  def  from 'mod'"
    testRTModule "import  def  from   \"mod\";"
    testRTModule "import   * as foo  from   \"mod\"  ; "
    testRTModule "import  def, * as foo  from   \"mod\"  ; "
    testRTModule "import  { baz,  bar as   foo }  from   \"mod\"  ; "
    testRTModule "import  def, { baz,  bar as   foo }  from   \"mod\"  ; "

    testRTModule "export   {};"
    testRTModule "  export {}   ;  "
    testRTModule "export {  a  ,  b  ,  c  };"
    testRTModule "export {  a, X   as B,   c }"
    testRTModule "export   {}  from \"mod\";"
    testRTModule "export * from 'module';"
    testRTModule "export   *   from   \"utils\"   ;"
    testRTModule "export * from './relative/path';"
    testRTModule "export * from '../parent/module';"
    testRTModule "export const a = 1 ; "
    testRTModule "export function f () {  } ; "
    testRTModule "export function * f () {  } ; "
    testRTModule "export   class Foo\nextends Bar\n{ get a () { return 1 ; }  static b ( x,y ) {} ; }   ; "

testRT :: String -> Expectation
testRT str = testRTWith (\s -> parse s "test") str

testRTModule :: String -> Expectation
testRTModule str = testRTWith (\s -> parseModule s "test") str

testRTWith :: (String -> Either String AST.JSAST) -> String -> Expectation
testRTWith f str = case f str of
  Right ast -> renderToString ast `shouldBe` str
  Left err -> expectationFailure ("Parse failed: " ++ err)

-- Additional supported round-trip tests for comprehensive coverage
testES6RoundTrip :: Spec
testES6RoundTrip = describe "ES6+ Round-trip Coverage:" $ do
  it "class declarations and expressions" $ do
    testRT "class A {}"
    testRT "class A extends B {}"
    testRT "class A { constructor() {} }"
    testRT "class A { method() {} }"
    testRT "class A { static method() {} }"
    testRT "class A { get prop() { return 1; } }"
    testRT "class A { set prop(x) { this.x = x; } }"

  it "optional chaining and nullish coalescing" $ do
    testRT "obj?.prop"
    testRT "obj?.method?.()"
    testRT "obj?.[key]"
    testRT "x ?? y"
    testRT "x?.y ?? z"

  it "template literals with expressions" $ do
    testRT "`Hello ${name}!`"
    testRT "`Line 1\nLine 2`"
    testRT "`Nested ${`inner ${x}`}`"
    testRT "tag`template`"
    testRT "tag`Hello ${name}!`"

  it "generator and iterator patterns" $ do
    testRT "function* gen() { yield* other(); }"
    testRT "function* gen() { yield 1; yield 2; }"
    testRT "(function* () { yield 1; })"
