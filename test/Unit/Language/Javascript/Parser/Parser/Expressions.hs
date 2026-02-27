{-# LANGUAGE OverloadedStrings #-}

module Unit.Language.Javascript.Parser.Parser.Expressions
  ( testExpressionParser,
  )
where

import qualified Data.ByteString.Char8 as BS8
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
  ( JSAST (..),
    JSAccessor (..),
    JSAnnot,
    JSArrayElement (..),
    JSArrowParameterList (..),
    JSAssignOp (..),
    JSBinOp (..),
    JSClassHeritage (..),
    JSCommaList (..),
    JSCommaTrailingList (..),
    JSConciseBody (..),
    JSExpression (..),
    JSIdent (..),
    JSMethodDefinition (..),
    JSObjectProperty (..),
    JSPropertyName (..),
    JSSemi,
    JSStatement (..),
    JSTemplatePart (..),
    JSUnaryOp (..),
  )
import Language.JavaScript.Parser.Parser
import Language.JavaScript.Parser.Parser (parseUsing)
import Test.Hspec

testExpressionParser :: Spec
testExpressionParser = describe "Parse expressions:" $ do
  it "this" $
    case testExpr "this" of
      Right (JSAstExpression (JSLiteral _ "this") _) -> pure ()
      result -> expectationFailure ("Expected this literal, got: " ++ show result)
  it "regex" $ do
    case testExpr "/blah/" of
      Right (JSAstExpression (JSRegEx _ "/blah/") _) -> pure ()
      result -> expectationFailure ("Expected regex /blah/, got: " ++ show result)
    case testExpr "/$/g" of
      Right (JSAstExpression (JSRegEx _ "/$/g") _) -> pure ()
      result -> expectationFailure ("Expected regex /$/g, got: " ++ show result)
    case testExpr "/\\n/g" of
      Right (JSAstExpression (JSRegEx _ "/\\n/g") _) -> pure ()
      result -> expectationFailure ("Expected regex /\\n/g, got: " ++ show result)
    case testExpr "/(\\/)/" of
      Right (JSAstExpression (JSRegEx _ "/(\\/)/") _) -> pure ()
      result -> expectationFailure ("Expected regex /(\\/)/, got: " ++ show result)
    case testExpr "/a[/]b/" of
      Right (JSAstExpression (JSRegEx _ "/a[/]b/") _) -> pure ()
      result -> expectationFailure ("Expected regex /a[/]b/, got: " ++ show result)
    case testExpr "/[/\\]/" of
      Right (JSAstExpression (JSRegEx _ "/[/\\]/") _) -> pure ()
      result -> expectationFailure ("Expected regex /[/\\]/, got: " ++ show result)
    case testExpr "/(\\/|\\)/" of
      Right (JSAstExpression (JSRegEx _ "/(\\/|\\)/") _) -> pure ()
      result -> expectationFailure ("Expected regex /(\\/|\\)/, got: " ++ show result)
    case testExpr "/a\\[|\\]$/g" of
      Right (JSAstExpression (JSRegEx _ "/a\\[|\\]$/g") _) -> pure ()
      result -> expectationFailure ("Expected regex /a\\[|\\]$/g, got: " ++ show result)
    case testExpr "/[(){}\\[\\]]/g" of
      Right (JSAstExpression (JSRegEx _ "/[(){}\\[\\]]/g") _) -> pure ()
      result -> expectationFailure ("Expected regex /[(){}\\[\\]]/g, got: " ++ show result)
    case testExpr "/^\"(?:\\.|[^\"])*\"|^'(?:[^']|\\.)*'/" of
      Right (JSAstExpression (JSRegEx _ "/^\"(?:\\.|[^\"])*\"|^'(?:[^']|\\.)*'/") _) -> pure ()
      result -> expectationFailure ("Expected complex regex, got: " ++ show result)

  it "identifier" $ do
    case testExpr "_$" of
      Right (JSAstExpression (JSIdentifier _ "_$") _) -> pure ()
      result -> expectationFailure ("Expected identifier _$, got: " ++ show result)
    case testExpr "this_" of
      Right (JSAstExpression (JSIdentifier _ "this_") _) -> pure ()
      result -> expectationFailure ("Expected identifier this_, got: " ++ show result)
  it "array literal" $ do
    case testExpr "[]" of
      Right (JSAstExpression (JSArrayLiteral _ [] _) _) -> pure ()
      result -> expectationFailure ("Expected empty array literal, got: " ++ show result)
    case testExpr "[,]" of
      Right (JSAstExpression (JSArrayLiteral _ [JSArrayComma _] _) _) -> pure ()
      result -> expectationFailure ("Expected array with comma, got: " ++ show result)
    case testExpr "[,,]" of
      Right (JSAstExpression (JSArrayLiteral _ [JSArrayComma _, JSArrayComma _] _) _) -> pure ()
      result -> expectationFailure ("Expected array with two commas, got: " ++ show result)
    case testExpr "[,,x]" of
      Right (JSAstExpression (JSArrayLiteral _ [JSArrayComma _, JSArrayComma _, JSArrayElement (JSIdentifier _ "x")] _) _) -> pure ()
      result -> expectationFailure ("Expected array [,,x], got: " ++ show result)
    case testExpr "[,x,,x]" of
      Right (JSAstExpression (JSArrayLiteral _ [JSArrayComma _, JSArrayElement (JSIdentifier _ "x"), JSArrayComma _, JSArrayComma _, JSArrayElement (JSIdentifier _ "x")] _) _) -> pure ()
      result -> expectationFailure ("Expected array [,x,,x], got: " ++ show result)
    case testExpr "[x]" of
      Right (JSAstExpression (JSArrayLiteral _ [JSArrayElement (JSIdentifier _ "x")] _) _) -> pure ()
      result -> expectationFailure ("Expected array [x], got: " ++ show result)
    case testExpr "[x,]" of
      Right (JSAstExpression (JSArrayLiteral _ [JSArrayElement (JSIdentifier _ "x"), JSArrayComma _] _) _) -> pure ()
      result -> expectationFailure ("Expected array [x,], got: " ++ show result)
    case testExpr "[,,,]" of
      Right (JSAstExpression (JSArrayLiteral _ [JSArrayComma _, JSArrayComma _, JSArrayComma _] _) _) -> pure ()
      result -> expectationFailure ("Expected array [,,,], got: " ++ show result)
    case testExpr "[a,,]" of
      Right (JSAstExpression (JSArrayLiteral _ [JSArrayElement (JSIdentifier _ "a"), JSArrayComma _, JSArrayComma _] _) _) -> pure ()
      result -> expectationFailure ("Expected array [a,,], got: " ++ show result)
  it "operator precedence" $ do
    case testExpr "2+3*4+5" of
      Right (JSAstExpression (JSExpressionBinary (JSExpressionBinary (JSDecimal _num1Annot "2") (JSBinOpPlus _plus1Annot) (JSExpressionBinary (JSDecimal _num2Annot "3") (JSBinOpTimes _timesAnnot) (JSDecimal _num3Annot "4"))) (JSBinOpPlus _plus2Annot) (JSDecimal _num4Annot "5")) _astAnnot) -> pure ()
      Right other -> expectationFailure ("Expected binary expression with correct precedence for 2+3*4+5, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for 2+3*4+5, got: " ++ show result)
    case testExpr "2*3**4" of
      Right (JSAstExpression (JSExpressionBinary (JSDecimal _num1Annot "2") (JSBinOpTimes _timesAnnot) (JSExpressionBinary (JSDecimal _num2Annot "3") (JSBinOpExponentiation _expAnnot) (JSDecimal _num3Annot "4"))) _astAnnot) -> pure ()
      Right other -> expectationFailure ("Expected binary expression with correct precedence for 2*3**4, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for 2*3**4, got: " ++ show result)
    case testExpr "2**3*4" of
      Right (JSAstExpression (JSExpressionBinary (JSExpressionBinary (JSDecimal _num1Annot "2") (JSBinOpExponentiation _expAnnot) (JSDecimal _num2Annot "3")) (JSBinOpTimes _timesAnnot) (JSDecimal _num3Annot "4")) _astAnnot) -> pure ()
      Right other -> expectationFailure ("Expected binary expression with correct precedence for 2**3*4, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for 2**3*4, got: " ++ show result)
  it "parentheses" $
    case testExpr "(56)" of
      Right (JSAstExpression (JSExpressionParen _ (JSDecimal _ "56") _) _) -> pure ()
      result -> expectationFailure ("Expected parenthesized expression (56), got: " ++ show result)
  it "string concatenation" $ do
    case testExpr "'ab' + 'bc'" of
      Right (JSAstExpression (JSExpressionBinary (JSStringLiteral _str1Annot "'ab'") (JSBinOpPlus _plusAnnot) (JSStringLiteral _str2Annot "'bc'")) _astAnnot) -> pure ()
      Right other -> expectationFailure ("Expected string concatenation 'ab' + 'bc', got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for 'ab' + 'bc', got: " ++ show result)
    case testExpr "'bc' + \"cd\"" of
      Right (JSAstExpression (JSExpressionBinary (JSStringLiteral _str1Annot "'bc'") (JSBinOpPlus _plusAnnot) (JSStringLiteral _str2Annot "\"cd\"")) _astAnnot) -> pure ()
      Right other -> expectationFailure ("Expected string concatenation 'bc' + \"cd\", got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for 'bc' + \"cd\", got: " ++ show result)
  it "object literal" $ do
    case testExpr "{}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone JSLNil) _) _) -> pure ()
      Right other -> expectationFailure ("Expected empty object literal {}, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for {}, got: " ++ show result)
    case testExpr "{x:1}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "x") _ [JSDecimal _ "1"]))) _) _) -> pure ()
      Right other -> expectationFailure ("Expected object literal {x:1} with property x=1, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for {x:1}, got: " ++ show result)
    case testExpr "{x:1,y:2}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLCons (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "x") _ [JSDecimal _ "1"])) _ (JSPropertyNameandValue (JSPropertyIdent _ "y") _ [JSDecimal _ "2"]))) _) _) -> pure ()
      Right other -> expectationFailure ("Expected object literal {x:1,y:2} with properties x=1, y=2, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for {x:1,y:2}, got: " ++ show result)
    case testExpr "{x:1,}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLComma (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "x") _ [JSDecimal _ "1"])) _) _) _) -> pure ()
      Right other -> expectationFailure ("Expected object literal {x:1,} with trailing comma, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for {x:1,}, got: " ++ show result)
    case testExpr "{yield:1}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "yield") _ [JSDecimal _ "1"]))) _) _) -> pure ()
      Right other -> expectationFailure ("Expected object literal {yield:1} with yield property, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for {yield:1}, got: " ++ show result)
    case testExpr "{x}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLOne (JSPropertyIdentRef _ "x"))) _) _) -> pure ()
      Right other -> expectationFailure ("Expected object literal {x} with shorthand property, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for {x}, got: " ++ show result)
    case testExpr "{x,}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLComma (JSLOne (JSPropertyIdentRef _ "x")) _) _) _) -> pure ()
      Right other -> expectationFailure ("Expected object literal {x,} with shorthand property and trailing comma, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for {x,}, got: " ++ show result)
    case testExpr "{set x([a,b]=y) {this.a=a;this.b=b}}" of
      Right
        ( JSAstExpression
            ( JSObjectLiteral
                _
                ( JSCTLNone
                    ( JSLOne
                        ( JSObjectMethod
                            ( JSPropertyAccessor
                                (JSAccessorSet _)
                                (JSPropertyIdent _ "x")
                                _
                                ( JSLOne
                                    ( JSAssignExpression
                                        (JSArrayLiteral _ [JSArrayElement (JSIdentifier _ "a"), JSArrayComma _, JSArrayElement (JSIdentifier _ "b")] _)
                                        (JSAssign _)
                                        (JSIdentifier _ "y")
                                      )
                                  )
                                _
                                _
                              )
                          )
                      )
                  )
                _
              )
            _
          ) -> pure ()
      result -> expectationFailure ("Expected object literal with setter, got: " ++ show result)
    case testExpr "a={if:1,interface:2}" of
      Right
        ( JSAstExpression
            ( JSAssignExpression
                (JSIdentifier _ "a")
                (JSAssign _)
                ( JSObjectLiteral
                    _
                    ( JSCTLNone
                        ( JSLCons
                            (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "if") _ [JSDecimal _ "1"]))
                            _
                            (JSPropertyNameandValue (JSPropertyIdent _ "interface") _ [JSDecimal _ "2"])
                          )
                      )
                    _
                  )
              )
            _
          ) -> pure ()
      result -> expectationFailure ("Expected assignment with object literal, got: " ++ show result)
    case testExpr "a={\n  values: 7,\n}\n" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _ "a") (JSAssign _) (JSObjectLiteral _ (JSCTLComma (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "values") _ [JSDecimal _ "7"])) _) _)) _) -> pure ()
      result -> expectationFailure ("Expected assignment with object literal, got: " ++ show result)
    case testExpr "x={get foo() {return 1},set foo(a) {x=a}}" of
      Right
        ( JSAstExpression
            ( JSAssignExpression
                (JSIdentifier _ "x")
                (JSAssign _)
                ( JSObjectLiteral
                    _
                    ( JSCTLNone
                        ( JSLCons
                            ( JSLOne
                                ( JSObjectMethod
                                    ( JSPropertyAccessor
                                        (JSAccessorGet _)
                                        (JSPropertyIdent _ "foo")
                                        _
                                        JSLNil
                                        _
                                        _
                                      )
                                  )
                              )
                            _
                            ( JSObjectMethod
                                ( JSPropertyAccessor
                                    (JSAccessorSet _)
                                    (JSPropertyIdent _ "foo")
                                    _
                                    (JSLOne (JSIdentifier _ "a"))
                                    _
                                    _
                                  )
                              )
                          )
                      )
                    _
                  )
              )
            _
          ) -> pure ()
      result -> expectationFailure ("Expected assignment with object literal, got: " ++ show result)
    case testExpr "{evaluate:evaluate,load:function load(s){if(x)return s;1}}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLCons (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "evaluate") _ [JSIdentifier _ "evaluate"])) _ (JSPropertyNameandValue (JSPropertyIdent _ "load") _ [JSFunctionExpression _ (JSIdentName _ "load") _ (JSLOne (JSIdentifier _ "s")) _ _]))) _) _) -> pure ()
      result -> expectationFailure ("Expected object literal, got: " ++ show result)
    case testExpr "obj = { name : 'A', 'str' : 'B', 123 : 'C', }" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _ "obj") (JSAssign _) (JSObjectLiteral _ (JSCTLComma (JSLCons (JSLCons (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "name") _ [JSStringLiteral _ "'A'"])) _ (JSPropertyNameandValue (JSPropertyString _ "'str'") _ [JSStringLiteral _ "'B'"])) _ (JSPropertyNameandValue (JSPropertyNumber _ "123") _ [JSStringLiteral _ "'C'"])) _) _)) _) -> pure ()
      result -> expectationFailure ("Expected assignment with object literal, got: " ++ show result)
    case testExpr "{[x]:1}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLOne (JSPropertyNameandValue (JSPropertyComputed _ (JSIdentifier _ "x") _) _ [JSDecimal _ "1"]))) _) _) -> pure ()
      result -> expectationFailure ("Expected object literal with computed property, got: " ++ show result)
    case testExpr "{ a(x,y) {}, 'blah blah'() {} }" of
      Right
        ( JSAstExpression
            ( JSObjectLiteral
                _leftBrace
                ( JSCTLNone
                    ( JSLCons
                        ( JSLOne
                            ( JSObjectMethod
                                ( JSMethodDefinition
                                    (JSPropertyIdent _propAnnot1 "a")
                                    _leftParen1
                                    (JSLCons (JSLOne (JSIdentifier _paramAnnot1 "x")) _comma1 (JSIdentifier _paramAnnot2 "y"))
                                    _rightParen1
                                    _body1
                                  )
                              )
                          )
                        _comma
                        ( JSObjectMethod
                            ( JSMethodDefinition
                                (JSPropertyString _propAnnot2 "'blah blah'")
                                _leftParen2
                                JSLNil
                                _rightParen2
                                _body2
                              )
                          )
                      )
                  )
                _rightBrace
              )
            _astAnnot
          ) -> pure ()
      result -> expectationFailure ("Expected object literal with method definitions, got: " ++ show result)
    case testExpr "{[x]() {}}" of
      Right
        ( JSAstExpression
            ( JSObjectLiteral
                _leftBrace
                ( JSCTLNone
                    ( JSLOne
                        ( JSObjectMethod
                            ( JSMethodDefinition
                                (JSPropertyComputed _leftBracket (JSIdentifier _idAnnot "x") _rightBracket)
                                _leftParen
                                JSLNil
                                _rightParen
                                _body
                              )
                          )
                      )
                  )
                _rightBrace
              )
            _astAnnot
          ) -> pure ()
      result -> expectationFailure ("Expected object literal with computed method, got: " ++ show result)
    case testExpr "{*a(x,y) {yield y;}}" of
      Right
        ( JSAstExpression
            ( JSObjectLiteral
                _leftBrace
                ( JSCTLNone
                    ( JSLOne
                        ( JSObjectMethod
                            ( JSGeneratorMethodDefinition
                                _starAnnot
                                (JSPropertyIdent _propAnnot "a")
                                _leftParen
                                (JSLCons (JSLOne (JSIdentifier _paramAnnot1 "x")) _comma (JSIdentifier _paramAnnot2 "y"))
                                _rightParen
                                _body
                              )
                          )
                      )
                  )
                _rightBrace
              )
            _astAnnot
          ) -> pure ()
      result -> expectationFailure ("Expected object literal with generator method, got: " ++ show result)
    case testExpr "{*[x]({y},...z) {}}" of
      Right
        ( JSAstExpression
            ( JSObjectLiteral
                _leftBrace
                ( JSCTLNone
                    ( JSLOne
                        ( JSObjectMethod
                            ( JSGeneratorMethodDefinition
                                _starAnnot
                                (JSPropertyComputed _leftBracket (JSIdentifier _idAnnot "x") _rightBracket)
                                _leftParen
                                ( JSLCons
                                    (JSLOne (JSObjectLiteral _leftBrace2 (JSCTLNone (JSLOne (JSPropertyIdentRef _propAnnot "y"))) _rightBrace2))
                                    _comma
                                    (JSSpreadExpression _spreadAnnot (JSIdentifier _idAnnot2 "z"))
                                  )
                                _rightParen
                                _body
                              )
                          )
                      )
                  )
                _rightBrace
              )
            _astAnnot
          ) -> pure ()
      result -> expectationFailure ("Expected object literal with computed generator, got: " ++ show result)

  it "object spread" $ do
    case testExpr "{...obj}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLOne (JSObjectSpread _ (JSIdentifier _ "obj")))) _) _) -> pure ()
      result -> expectationFailure ("Expected object literal with spread, got: " ++ show result)
    case testExpr "{a: 1, ...obj}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLCons (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "a") _ [JSDecimal _ "1"])) _ (JSObjectSpread _ (JSIdentifier _ "obj")))) _) _) -> pure ()
      result -> expectationFailure ("Expected object literal with property and spread, got: " ++ show result)
    case testExpr "{...obj, b: 2}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLCons (JSLOne (JSObjectSpread _ (JSIdentifier _ "obj"))) _ (JSPropertyNameandValue (JSPropertyIdent _ "b") _ [JSDecimal _ "2"]))) _) _) -> pure ()
      result -> expectationFailure ("Expected object literal with spread and property, got: " ++ show result)
    case testExpr "{a: 1, ...obj, b: 2}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLCons (JSLCons (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "a") _ [JSDecimal _ "1"])) _ (JSObjectSpread _ (JSIdentifier _ "obj"))) _ (JSPropertyNameandValue (JSPropertyIdent _ "b") _ [JSDecimal _ "2"]))) _) _) -> pure ()
      result -> expectationFailure ("Expected object literal with mixed spread, got: " ++ show result)
    case testExpr "{...obj1, ...obj2}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLCons (JSLOne (JSObjectSpread _ (JSIdentifier _ "obj1"))) _ (JSObjectSpread _ (JSIdentifier _ "obj2")))) _) _) -> pure ()
      result -> expectationFailure ("Expected object literal with multiple spreads, got: " ++ show result)
    case testExpr "{...getObject()}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLOne (JSObjectSpread _ (JSMemberExpression (JSIdentifier _ "getObject") _ JSLNil _)))) _) _) -> pure ()
      result -> expectationFailure ("Expected object literal with function call spread, got: " ++ show result)
    case testExpr "{x, ...obj, y}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLCons (JSLCons (JSLOne (JSPropertyIdentRef _ "x")) _ (JSObjectSpread _ (JSIdentifier _ "obj"))) _ (JSPropertyIdentRef _ "y"))) _) _) -> pure ()
      result -> expectationFailure ("Expected object literal with properties and spread, got: " ++ show result)
    case testExpr "{...obj, method() {}}" of
      Right
        ( JSAstExpression
            ( JSObjectLiteral
                _
                ( JSCTLNone
                    ( JSLCons
                        (JSLOne (JSObjectSpread _ (JSIdentifier _ "obj")))
                        _
                        ( JSObjectMethod
                            ( JSMethodDefinition
                                (JSPropertyIdent _ "method")
                                _
                                JSLNil
                                _
                                _
                              )
                          )
                      )
                  )
                _
              )
            _
          ) -> pure ()
      result -> expectationFailure ("Expected object literal with spread and method, got: " ++ show result)

  it "unary expression" $ do
    case testExpr "delete y" of
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpDelete _opAnnot) (JSIdentifier _idAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary delete expression, got: " ++ show result)
    case testExpr "void y" of
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpVoid _opAnnot) (JSIdentifier _idAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary void expression, got: " ++ show result)
    case testExpr "typeof y" of
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpTypeof opAnnot) (JSIdentifier idAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary typeof expression, got: " ++ show result)
    case testExpr "++y" of
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpIncr opAnnot) (JSIdentifier idAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary increment expression, got: " ++ show result)
    case testExpr "--y" of
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpDecr opAnnot) (JSIdentifier idAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary decrement expression, got: " ++ show result)
    case testExpr "+y" of
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpPlus opAnnot) (JSIdentifier idAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary plus expression, got: " ++ show result)
    case testExpr "-y" of
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpMinus opAnnot) (JSIdentifier idAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary minus expression, got: " ++ show result)
    case testExpr "~y" of
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpTilde opAnnot) (JSIdentifier idAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary bitwise not expression, got: " ++ show result)
    case testExpr "!y" of
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpNot opAnnot) (JSIdentifier idAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary logical not expression, got: " ++ show result)
    case testExpr "y++" of
      Right (JSAstExpression (JSExpressionPostfix (JSIdentifier idAnnot "y") (JSUnaryOpIncr opAnnot)) astAnnot) -> pure ()
      result -> expectationFailure ("Expected postfix increment expression, got: " ++ show result)
    case testExpr "y--" of
      Right (JSAstExpression (JSExpressionPostfix (JSIdentifier idAnnot "y") (JSUnaryOpDecr opAnnot)) astAnnot) -> pure ()
      result -> expectationFailure ("Expected postfix decrement expression, got: " ++ show result)
    case testExpr "...y" of
      Right (JSAstExpression (JSSpreadExpression _ (JSIdentifier _ "y")) _) -> pure ()
      result -> expectationFailure ("Expected spread expression, got: " ++ show result)

  it "new expression" $ do
    case testExpr "new x()" of
      Right (JSAstExpression (JSMemberNew newAnnot (JSIdentifier idAnnot "x") leftParen JSLNil rightParen) astAnnot) -> pure ()
      result -> expectationFailure ("Expected new expression with call, got: " ++ show result)
    case testExpr "new x.y" of
      Right (JSAstExpression (JSNewExpression newAnnot (JSMemberDot (JSIdentifier idAnnot "x") dot (JSIdentifier memAnnot "y"))) astAnnot) -> pure ()
      result -> expectationFailure ("Expected new expression with member access, got: " ++ show result)

  it "binary expression" $ do
    case testExpr "x||y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpOr opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary logical or expression, got: " ++ show result)
    case testExpr "x&&y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpAnd opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary logical and expression, got: " ++ show result)
    case testExpr "x??y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpNullishCoalescing opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary nullish coalescing expression, got: " ++ show result)
    case testExpr "x|y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpBitOr opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary bitwise or expression, got: " ++ show result)
    case testExpr "x^y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpBitXor opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary bitwise xor expression, got: " ++ show result)
    case testExpr "x&y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpBitAnd opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary bitwise and expression, got: " ++ show result)

    case testExpr "x==y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpEq opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary equality expression, got: " ++ show result)
    case testExpr "x!=y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpNeq opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary inequality expression, got: " ++ show result)
    case testExpr "x===y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpStrictEq opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary strict equality expression, got: " ++ show result)
    case testExpr "x!==y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpStrictNeq opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary strict inequality expression, got: " ++ show result)

    case testExpr "x<y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpLt opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary less than expression, got: " ++ show result)
    case testExpr "x>y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpGt opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary greater than expression, got: " ++ show result)
    case testExpr "x<=y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpLe opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary less than or equal expression, got: " ++ show result)
    case testExpr "x>=y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpGe opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary greater than or equal expression, got: " ++ show result)

    case testExpr "x<<y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpLsh opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary left shift expression, got: " ++ show result)
    case testExpr "x>>y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpRsh opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary right shift expression, got: " ++ show result)
    case testExpr "x>>>y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpUrsh opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary unsigned right shift expression, got: " ++ show result)

    case testExpr "x+y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpPlus opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary addition expression, got: " ++ show result)
    case testExpr "x-y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpMinus opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary subtraction expression, got: " ++ show result)

    case testExpr "x*y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpTimes opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary multiplication expression, got: " ++ show result)
    case testExpr "x**y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpExponentiation opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary exponentiation expression, got: " ++ show result)
    case testExpr "x**y**z" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier idAnnot "x") (JSBinOpExponentiation opAnnot1) (JSExpressionBinary (JSIdentifier leftIdAnnot "y") (JSBinOpExponentiation opAnnot2) (JSIdentifier rightIdAnnot "z"))) astAnnot) -> pure ()
      result -> expectationFailure ("Expected nested exponentiation expression, got: " ++ show result)
    case testExpr "2**3**2" of
      Right (JSAstExpression (JSExpressionBinary (JSDecimal numAnnot1 "2") (JSBinOpExponentiation opAnnot1) (JSExpressionBinary (JSDecimal numAnnot2 "3") (JSBinOpExponentiation opAnnot2) (JSDecimal numAnnot3 "2"))) astAnnot) -> pure ()
      result -> expectationFailure ("Expected numeric exponentiation expression, got: " ++ show result)
    case testExpr "x/y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpDivide opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary division expression, got: " ++ show result)
    case testExpr "x%y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpMod opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary modulo expression, got: " ++ show result)
    case testExpr "x instanceof y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpInstanceOf opAnnot) (JSIdentifier rightIdAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected instanceof expression, got: " ++ show result)

  it "assign expression" $ do
    case testExpr "x=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier idAnnot "x") (JSAssign assignAnnot) (JSDecimal numAnnot "1")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected assignment expression x=1, got: " ++ show result)
    case testExpr "x*=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier idAnnot "x") (JSTimesAssign assignAnnot) (JSDecimal numAnnot "1")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected multiply assignment expression, got: " ++ show result)
    case testExpr "x/=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier idAnnot "x") (JSDivideAssign assignAnnot) (JSDecimal numAnnot "1")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected divide assignment expression, got: " ++ show result)
    case testExpr "x%=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier idAnnot "x") (JSModAssign assignAnnot) (JSDecimal numAnnot "1")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected modulo assignment expression, got: " ++ show result)
    case testExpr "x+=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier idAnnot "x") (JSPlusAssign assignAnnot) (JSDecimal numAnnot "1")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected add assignment expression, got: " ++ show result)
    case testExpr "x-=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier idAnnot "x") (JSMinusAssign assignAnnot) (JSDecimal numAnnot "1")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected subtract assignment expression, got: " ++ show result)
    case testExpr "x<<=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier idAnnot "x") (JSLshAssign assignAnnot) (JSDecimal numAnnot "1")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected left shift assignment expression, got: " ++ show result)
    case testExpr "x>>=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier idAnnot "x") (JSRshAssign assignAnnot) (JSDecimal numAnnot "1")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected right shift assignment expression, got: " ++ show result)
    case testExpr "x>>>=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier idAnnot "x") (JSUrshAssign assignAnnot) (JSDecimal numAnnot "1")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected unsigned right shift assignment expression, got: " ++ show result)
    case testExpr "x&=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier idAnnot "x") (JSBwAndAssign assignAnnot) (JSDecimal numAnnot "1")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected bitwise and assignment expression, got: " ++ show result)

  it "destructuring assignment expressions (ES2015) - supported features" $ do
    -- Array destructuring assignment
    case testExpr "[a, b] = arr" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral leftBracket [JSArrayElement (JSIdentifier elem1Annot "a"), JSArrayComma comma, JSArrayElement (JSIdentifier elem2Annot "b")] rightBracket) (JSAssign assignAnnot) (JSIdentifier idAnnot "arr")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected array destructuring assignment, got: " ++ show result)
    case testExpr "[x, y, z] = coordinates" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral leftBracket [JSArrayElement (JSIdentifier elem1Annot "x"), JSArrayComma comma1, JSArrayElement (JSIdentifier elem2Annot "y"), JSArrayComma comma2, JSArrayElement (JSIdentifier elem3Annot "z")] rightBracket) (JSAssign assignAnnot) (JSIdentifier idAnnot "coordinates")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected array destructuring assignment, got: " ++ show result)

    -- Object destructuring assignment
    case testExpr "{a, b} = obj" of
      Right (JSAstExpression (JSAssignExpression (JSObjectLiteral leftBrace (JSCTLNone (JSLCons (JSLOne (JSPropertyIdentRef prop1Annot "a")) comma (JSPropertyIdentRef prop2Annot "b"))) rightBrace) (JSAssign assignAnnot) (JSIdentifier idAnnot "obj")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected object destructuring assignment, got: " ++ show result)
    case testExpr "{name, age} = person" of
      Right (JSAstExpression (JSAssignExpression (JSObjectLiteral leftBrace (JSCTLNone (JSLCons (JSLOne (JSPropertyIdentRef prop1Annot "name")) comma (JSPropertyIdentRef prop2Annot "age"))) rightBrace) (JSAssign assignAnnot) (JSIdentifier idAnnot "person")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected object destructuring assignment, got: " ++ show result)

    -- Nested destructuring assignment
    case testExpr "[a, [b, c]] = nested" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral leftBracket1 [JSArrayElement (JSIdentifier elem1Annot "a"), JSArrayComma comma1, JSArrayElement (JSArrayLiteral leftBracket2 [JSArrayElement (JSIdentifier elem2Annot "b"), JSArrayComma comma2, JSArrayElement (JSIdentifier elem3Annot "c")] rightBracket2)] rightBracket1) (JSAssign assignAnnot) (JSIdentifier idAnnot "nested")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected nested array destructuring assignment, got: " ++ show result)
    case testExpr "{a: {b}} = deep" of
      Right (JSAstExpression (JSAssignExpression (JSObjectLiteral leftBrace1 (JSCTLNone (JSLOne (JSPropertyNameandValue (JSPropertyIdent propAnnot "a") colon [JSObjectLiteral leftBrace2 (JSCTLNone (JSLOne (JSPropertyIdentRef prop2Annot "b"))) rightBrace2]))) rightBrace1) (JSAssign assignAnnot) (JSIdentifier idAnnot "deep")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected nested object destructuring assignment, got: " ++ show result)

    -- Rest pattern assignment
    case testExpr "[first, ...rest] = array" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral leftBracket [JSArrayElement (JSIdentifier elem1Annot "first"), JSArrayComma comma, JSArrayElement (JSSpreadExpression spreadAnnot (JSIdentifier elem2Annot "rest"))] rightBracket) (JSAssign assignAnnot) (JSIdentifier idAnnot "array")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected rest pattern assignment, got: " ++ show result)

    -- Sparse array assignment
    case testExpr "[, , third] = sparse" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral leftBracket [JSArrayComma comma1, JSArrayComma comma2, JSArrayElement (JSIdentifier elemAnnot "third")] rightBracket) (JSAssign assignAnnot) (JSIdentifier idAnnot "sparse")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected sparse array assignment, got: " ++ show result)

    -- Property renaming assignment
    case testExpr "{prop: newName} = obj" of
      Right (JSAstExpression (JSAssignExpression (JSObjectLiteral leftBrace (JSCTLNone (JSLOne (JSPropertyNameandValue (JSPropertyIdent propAnnot "prop") colon [JSIdentifier idAnnot "newName"]))) rightBrace) (JSAssign assignAnnot) (JSIdentifier idAnnot2 "obj")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected property renaming assignment, got: " ++ show result)

    -- Array destructuring with default values (parsed as assignment expressions)
    case testExpr "[a = 1, b = 2] = arr" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral leftBracket [JSArrayElement (JSAssignExpression (JSIdentifier elem1Annot "a") (JSAssign assign1Annot) (JSDecimal num1Annot "1")), JSArrayComma comma, JSArrayElement (JSAssignExpression (JSIdentifier elem2Annot "b") (JSAssign assign2Annot) (JSDecimal num2Annot "2"))] rightBracket) (JSAssign assignAnnot) (JSIdentifier idAnnot "arr")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected array destructuring with defaults, got: " ++ show result)
    case testExpr "[x = 'default', y] = values" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral leftBracket [JSArrayElement (JSAssignExpression (JSIdentifier elem1Annot "x") (JSAssign assign1Annot) (JSStringLiteral strAnnot "'default'")), JSArrayComma comma, JSArrayElement (JSIdentifier elem2Annot "y")] rightBracket) (JSAssign assignAnnot) (JSIdentifier idAnnot "values")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected array destructuring with default, got: " ++ show result)

    -- Mixed array destructuring with defaults and rest
    case testExpr "[first, second = 42, ...rest] = data" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral leftBracket [JSArrayElement (JSIdentifier elem1Annot "first"), JSArrayComma comma1, JSArrayElement (JSAssignExpression (JSIdentifier elem2Annot "second") (JSAssign assignAnnot) (JSDecimal numAnnot "42")), JSArrayComma comma2, JSArrayElement (JSSpreadExpression spreadAnnot (JSIdentifier elem3Annot "rest"))] rightBracket) (JSAssign assignAnnot2) (JSIdentifier idAnnot "data")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected mixed array destructuring assignment, got: " ++ show result)
    case testExpr "x^=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier idAnnot "x") (JSBwXorAssign assignAnnot) (JSDecimal numAnnot "1")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected bitwise xor assignment expression, got: " ++ show result)
    case testExpr "x|=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier idAnnot "x") (JSBwOrAssign assignAnnot) (JSDecimal numAnnot "1")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected bitwise or assignment expression, got: " ++ show result)

  it "logical assignment operators" $ do
    case testExpr "x&&=true" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier idAnnot "x") (JSLogicalAndAssign assignAnnot) (JSLiteral litAnnot "true")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected logical and assignment expression, got: " ++ show result)
    case testExpr "x||=false" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier idAnnot "x") (JSLogicalOrAssign assignAnnot) (JSLiteral litAnnot "false")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected logical or assignment expression, got: " ++ show result)
    case testExpr "x??=null" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier idAnnot "x") (JSNullishAssign assignAnnot) (JSLiteral litAnnot "null")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected nullish assignment expression, got: " ++ show result)
    case testExpr "obj.prop&&=value" of
      Right (JSAstExpression (JSAssignExpression (JSMemberDot (JSIdentifier idAnnot "obj") dot (JSIdentifier memAnnot "prop")) (JSLogicalAndAssign assignAnnot) (JSIdentifier valAnnot "value")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected member dot logical and assignment, got: " ++ show result)
    case testExpr "arr[0]||=defaultValue" of
      Right (JSAstExpression (JSAssignExpression (JSMemberSquare (JSIdentifier idAnnot "arr") leftBracket (JSDecimal numAnnot "0") rightBracket) (JSLogicalOrAssign assignAnnot) (JSIdentifier valAnnot "defaultValue")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected member square logical or assignment, got: " ++ show result)
    case testExpr "config.timeout??=5000" of
      Right (JSAstExpression (JSAssignExpression (JSMemberDot (JSIdentifier idAnnot "config") dot (JSIdentifier memAnnot "timeout")) (JSNullishAssign assignAnnot) (JSDecimal numAnnot "5000")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected member dot nullish assignment, got: " ++ show result)
    case testExpr "a&&=b&&=c" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier id1Annot "a") (JSLogicalAndAssign assign1Annot) (JSAssignExpression (JSIdentifier id2Annot "b") (JSLogicalAndAssign assign2Annot) (JSIdentifier id3Annot "c"))) astAnnot) -> pure ()
      result -> expectationFailure ("Expected nested logical and assignment, got: " ++ show result)

  it "function expression" $ do
    case testExpr "function(){}" of
      Right (JSAstExpression (JSFunctionExpression funcAnnot JSIdentNone leftParen JSLNil rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with no params, got: " ++ show result)
    case testExpr "function(a){}" of
      Right (JSAstExpression (JSFunctionExpression funcAnnot JSIdentNone leftParen (JSLOne (JSIdentifier paramAnnot "a")) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with one param, got: " ++ show result)
    case testExpr "function(a,b){}" of
      Right (JSAstExpression (JSFunctionExpression funcAnnot JSIdentNone leftParen (JSLCons (JSLOne (JSIdentifier param1Annot "a")) comma (JSIdentifier param2Annot "b")) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with two params, got: " ++ show result)
    case testExpr "function(...a){}" of
      Right (JSAstExpression (JSFunctionExpression funcAnnot JSIdentNone leftParen (JSLOne (JSSpreadExpression spreadAnnot (JSIdentifier paramAnnot "a"))) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with rest param, got: " ++ show result)
    case testExpr "function(a=1){}" of
      Right (JSAstExpression (JSFunctionExpression funcAnnot JSIdentNone leftParen (JSLOne (JSAssignExpression (JSIdentifier paramAnnot "a") (JSAssign assignAnnot) (JSDecimal numAnnot "1"))) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with default param, got: " ++ show result)
    case testExpr "function([a,b]){}" of
      Right (JSAstExpression (JSFunctionExpression funcAnnot JSIdentNone leftParen (JSLOne (JSArrayLiteral leftBracket [JSArrayElement (JSIdentifier elem1Annot "a"), JSArrayComma comma, JSArrayElement (JSIdentifier elem2Annot "b")] rightBracket)) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with array destructuring, got: " ++ show result)
    case testExpr "function([a,...b]){}" of
      Right (JSAstExpression (JSFunctionExpression funcAnnot JSIdentNone leftParen (JSLOne (JSArrayLiteral leftBracket [JSArrayElement (JSIdentifier elem1Annot "a"), JSArrayComma comma, JSArrayElement (JSSpreadExpression spreadAnnot (JSIdentifier elem2Annot "b"))] rightBracket)) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with array destructuring and rest, got: " ++ show result)
    case testExpr "function({a,b}){}" of
      Right (JSAstExpression (JSFunctionExpression funcAnnot JSIdentNone leftParen (JSLOne (JSObjectLiteral leftBrace (JSCTLNone (JSLCons (JSLOne (JSPropertyIdentRef prop1Annot "a")) comma (JSPropertyIdentRef prop2Annot "b"))) rightBrace)) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with object destructuring, got: " ++ show result)
    case testExpr "a => {}" of
      Right (JSAstExpression (JSArrowExpression (JSUnparenthesizedArrowParameter (JSIdentName paramAnnot "a")) arrow (JSConciseFunctionBody (JSBlock leftBrace [] rightBrace))) astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with single param, got: " ++ show result)
    case testExpr "(a) => { a + 2 }" of
      Right (JSAstExpression (JSArrowExpression (JSParenthesizedArrowParameterList leftParen (JSLOne (JSIdentifier paramAnnot "a")) rightParen) arrow (JSConciseFunctionBody (JSBlock leftBrace body rightBrace))) astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with paren param, got: " ++ show result)
    case testExpr "(a, b) => {}" of
      Right (JSAstExpression (JSArrowExpression (JSParenthesizedArrowParameterList leftParen (JSLCons (JSLOne (JSIdentifier param1Annot "a")) comma (JSIdentifier param2Annot "b")) rightParen) arrow (JSConciseFunctionBody (JSBlock leftBrace [] rightBrace))) astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with two params, got: " ++ show result)
    case testExpr "(a, b) => a + b" of
      Right (JSAstExpression (JSArrowExpression (JSParenthesizedArrowParameterList leftParen (JSLCons (JSLOne (JSIdentifier param1Annot "a")) comma (JSIdentifier param2Annot "b")) rightParen) arrow (JSConciseExpressionBody (JSExpressionBinary (JSIdentifier id1Annot "a") (JSBinOpPlus plusAnnot) (JSIdentifier id2Annot "b")))) astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with expression body, got: " ++ show result)
    case testExpr "() => { 42 }" of
      Right (JSAstExpression (JSArrowExpression (JSParenthesizedArrowParameterList leftParen JSLNil rightParen) arrow (JSConciseFunctionBody (JSBlock leftBrace body rightBrace))) astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with no params, got: " ++ show result)
    case testExpr "(a, ...b) => b" of
      Right (JSAstExpression (JSArrowExpression (JSParenthesizedArrowParameterList leftParen (JSLCons (JSLOne (JSIdentifier param1Annot "a")) comma (JSSpreadExpression spreadAnnot (JSIdentifier param2Annot "b"))) rightParen) arrow (JSConciseExpressionBody (JSIdentifier idAnnot "b"))) astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with rest param, got: " ++ show result)
    case testExpr "(a,b=1) => a + b" of
      Right (JSAstExpression (JSArrowExpression (JSParenthesizedArrowParameterList leftParen (JSLCons (JSLOne (JSIdentifier param1Annot "a")) comma (JSAssignExpression (JSIdentifier param2Annot "b") (JSAssign assignAnnot) (JSDecimal numAnnot "1"))) rightParen) arrow (JSConciseExpressionBody (JSExpressionBinary (JSIdentifier id1Annot "a") (JSBinOpPlus plusAnnot) (JSIdentifier id2Annot "b")))) astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with default param, got: " ++ show result)
    case testExpr "([a,b]) => a + b" of
      Right (JSAstExpression (JSArrowExpression (JSParenthesizedArrowParameterList leftParen (JSLOne (JSArrayLiteral leftBracket [JSArrayElement (JSIdentifier elem1Annot "a"), JSArrayComma comma, JSArrayElement (JSIdentifier elem2Annot "b")] rightBracket)) rightParen) arrow (JSConciseExpressionBody (JSExpressionBinary (JSIdentifier id1Annot "a") (JSBinOpPlus plusAnnot) (JSIdentifier id2Annot "b")))) astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with destructuring param, got: " ++ show result)

  it "trailing comma in function parameters" $ do
    -- Test trailing commas in function expressions
    case testExpr "function(a,){}" of
      Right (JSAstExpression (JSFunctionExpression funcAnnot JSIdentNone leftParen (JSLOne (JSIdentifier paramAnnot "a")) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with trailing comma, got: " ++ show result)
    case testExpr "function(a,b,){}" of
      Right (JSAstExpression (JSFunctionExpression funcAnnot JSIdentNone leftParen (JSLCons (JSLOne (JSIdentifier param1Annot "a")) comma (JSIdentifier param2Annot "b")) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with trailing comma, got: " ++ show result)
    -- Test named functions with trailing commas
    case testExpr "function foo(x,){}" of
      Right (JSAstExpression (JSFunctionExpression funcAnnot (JSIdentName nameAnnot "foo") leftParen (JSLOne (JSIdentifier paramAnnot "x")) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected named function expression with trailing comma, got: " ++ show result)
    -- Test generator functions with trailing commas
    case testExpr "function*(a,){}" of
      Right (JSAstExpression (JSGeneratorExpression genAnnot starAnnot JSIdentNone leftParen (JSLOne (JSIdentifier paramAnnot "a")) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected generator expression with trailing comma, got: " ++ show result)
    case testExpr "function* gen(x,y,){}" of
      Right (JSAstExpression (JSGeneratorExpression genAnnot starAnnot (JSIdentName nameAnnot "gen") leftParen (JSLCons (JSLOne (JSIdentifier param1Annot "x")) comma (JSIdentifier param2Annot "y")) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected named generator expression with trailing comma, got: " ++ show result)

  it "generator expression" $ do
    case testExpr "function*(){}" of
      Right (JSAstExpression (JSGeneratorExpression genAnnot starAnnot JSIdentNone leftParen JSLNil rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected generator expression with no params, got: " ++ show result)
    case testExpr "function*(a){}" of
      Right (JSAstExpression (JSGeneratorExpression genAnnot starAnnot JSIdentNone leftParen (JSLOne (JSIdentifier paramAnnot "a")) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected generator expression with one param, got: " ++ show result)
    case testExpr "function*(a,b){}" of
      Right (JSAstExpression (JSGeneratorExpression genAnnot starAnnot JSIdentNone leftParen (JSLCons (JSLOne (JSIdentifier param1Annot "a")) comma (JSIdentifier param2Annot "b")) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected generator expression with two params, got: " ++ show result)
    case testExpr "function*(a,...b){}" of
      Right (JSAstExpression (JSGeneratorExpression genAnnot starAnnot JSIdentNone leftParen (JSLCons (JSLOne (JSIdentifier param1Annot "a")) comma (JSSpreadExpression spreadAnnot (JSIdentifier param2Annot "b"))) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected generator expression with rest param, got: " ++ show result)
    case testExpr "function*f(){}" of
      Right (JSAstExpression (JSGeneratorExpression genAnnot starAnnot (JSIdentName nameAnnot "f") leftParen JSLNil rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected named generator expression with no params, got: " ++ show result)
    case testExpr "function*f(a){}" of
      Right (JSAstExpression (JSGeneratorExpression genAnnot starAnnot (JSIdentName nameAnnot "f") leftParen (JSLOne (JSIdentifier paramAnnot "a")) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected named generator expression with one param, got: " ++ show result)
    case testExpr "function*f(a,b){}" of
      Right (JSAstExpression (JSGeneratorExpression genAnnot starAnnot (JSIdentName nameAnnot "f") leftParen (JSLCons (JSLOne (JSIdentifier param1Annot "a")) comma (JSIdentifier param2Annot "b")) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected named generator expression with two params, got: " ++ show result)
    case testExpr "function*f(a,...b){}" of
      Right (JSAstExpression (JSGeneratorExpression genAnnot starAnnot (JSIdentName nameAnnot "f") leftParen (JSLCons (JSLOne (JSIdentifier param1Annot "a")) comma (JSSpreadExpression spreadAnnot (JSIdentifier param2Annot "b"))) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected named generator expression with rest param, got: " ++ show result)

  it "await expression" $ do
    case testExpr "await fetch('/api')" of
      Right (JSAstExpression (JSAwaitExpression awaitAnnot (JSMemberExpression (JSIdentifier idAnnot "fetch") leftParen (JSLOne (JSStringLiteral strAnnot "'/api'")) rightParen)) astAnnot) -> pure ()
      result -> expectationFailure ("Expected await expression with function call, got: " ++ show result)
    case testExpr "await Promise.resolve(42)" of
      Right (JSAstExpression (JSAwaitExpression awaitAnnot (JSMemberExpression (JSMemberDot (JSIdentifier idAnnot "Promise") dot (JSIdentifier memAnnot "resolve")) leftParen (JSLOne (JSDecimal numAnnot "42")) rightParen)) astAnnot) -> pure ()
      result -> expectationFailure ("Expected await expression with method call, got: " ++ show result)
    case testExpr "await (x + y)" of
      Right (JSAstExpression (JSAwaitExpression awaitAnnot (JSExpressionParen leftParen (JSExpressionBinary (JSIdentifier id1Annot "x") (JSBinOpPlus plusAnnot) (JSIdentifier id2Annot "y")) rightParen)) astAnnot) -> pure ()
      result -> expectationFailure ("Expected await expression with parenthesized expression, got: " ++ show result)
    case testExpr "await x.then(y => y * 2)" of
      Right (JSAstExpression (JSAwaitExpression awaitAnnot (JSMemberExpression (JSMemberDot (JSIdentifier idAnnot "x") dot (JSIdentifier memAnnot "then")) leftParen (JSLOne (JSArrowExpression (JSUnparenthesizedArrowParameter (JSIdentName paramAnnot "y")) arrow (JSConciseExpressionBody (JSExpressionBinary (JSIdentifier id1Annot "y") (JSBinOpTimes timesAnnot) (JSDecimal numAnnot "2"))))) rightParen)) astAnnot) -> pure ()
      result -> expectationFailure ("Expected await expression with method and arrow function, got: " ++ show result)
    case testExpr "await response.json()" of
      Right (JSAstExpression (JSAwaitExpression awaitAnnot (JSMemberExpression (JSMemberDot (JSIdentifier idAnnot "response") dot (JSIdentifier memAnnot "json")) leftParen JSLNil rightParen)) astAnnot) -> pure ()
      result -> expectationFailure ("Expected await expression with method call, got: " ++ show result)
    case testExpr "await new Promise(resolve => resolve(1))" of
      Right (JSAstExpression (JSAwaitExpression awaitAnnot (JSMemberNew newAnnot (JSIdentifier idAnnot "Promise") leftParen (JSLOne (JSArrowExpression (JSUnparenthesizedArrowParameter (JSIdentName paramAnnot "resolve")) arrow (JSConciseExpressionBody (JSMemberExpression (JSIdentifier callAnnot "resolve") leftParen2 (JSLOne (JSDecimal numAnnot "1")) rightParen2)))) rightParen)) astAnnot) -> pure ()
      result -> expectationFailure ("Expected await expression with constructor and arrow function, got: " ++ show result)

  it "async function expression" $ do
    case testExpr "async function foo() {}" of
      Right (JSAstExpression (JSAsyncFunctionExpression asyncAnnot funcAnnot (JSIdentName nameAnnot "foo") leftParen JSLNil rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected named async function expression with no params, got: " ++ show result)
    case testExpr "async function foo(a) {}" of
      Right (JSAstExpression (JSAsyncFunctionExpression asyncAnnot funcAnnot (JSIdentName nameAnnot "foo") leftParen (JSLOne (JSIdentifier paramAnnot "a")) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected named async function expression with one param, got: " ++ show result)
    case testExpr "async function foo(a, b) {}" of
      Right (JSAstExpression (JSAsyncFunctionExpression asyncAnnot funcAnnot (JSIdentName nameAnnot "foo") leftParen (JSLCons (JSLOne (JSIdentifier param1Annot "a")) comma (JSIdentifier param2Annot "b")) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected named async function expression with two params, got: " ++ show result)
    case testExpr "async function() {}" of
      Right (JSAstExpression (JSAsyncFunctionExpression asyncAnnot funcAnnot JSIdentNone leftParen JSLNil rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected anonymous async function expression with no params, got: " ++ show result)
    case testExpr "async function(x) { return await x; }" of
      Right (JSAstExpression (JSAsyncFunctionExpression asyncAnnot funcAnnot JSIdentNone leftParen (JSLOne (JSIdentifier paramAnnot "x")) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected anonymous async function expression with return await, got: " ++ show result)
    case testExpr "async function fetch() { return await response.json(); }" of
      Right (JSAstExpression (JSAsyncFunctionExpression asyncAnnot funcAnnot (JSIdentName nameAnnot "fetch") leftParen JSLNil rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected named async function expression with await method call, got: " ++ show result)
    case testExpr "async function handler(req, res) { const data = await db.query(); res.send(data); }" of
      Right (JSAstExpression (JSAsyncFunctionExpression asyncAnnot funcAnnot (JSIdentName nameAnnot "handler") leftParen (JSLCons (JSLOne (JSIdentifier param1Annot "req")) comma (JSIdentifier param2Annot "res")) rightParen body) astAnnot) -> pure ()
      result -> expectationFailure ("Expected named async function expression with complex body, got: " ++ show result)

  it "member expression" $ do
    case testExpr "x[y]" of
      Right (JSAstExpression (JSMemberSquare (JSIdentifier idAnnot "x") leftBracket (JSIdentifier indexAnnot "y") rightBracket) astAnnot) -> pure ()
      result -> expectationFailure ("Expected member square expression, got: " ++ show result)
    case testExpr "x[y][z]" of
      Right (JSAstExpression (JSMemberSquare (JSMemberSquare (JSIdentifier idAnnot "x") leftBracket1 (JSIdentifier index1Annot "y") rightBracket1) leftBracket2 (JSIdentifier index2Annot "z") rightBracket2) astAnnot) -> pure ()
      result -> expectationFailure ("Expected nested member square expression, got: " ++ show result)
    case testExpr "x.y" of
      Right (JSAstExpression (JSMemberDot (JSIdentifier idAnnot "x") dot (JSIdentifier memAnnot "y")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected member dot expression, got: " ++ show result)
    case testExpr "x.y.z" of
      Right (JSAstExpression (JSMemberDot (JSMemberDot (JSIdentifier idAnnot "x") dot1 (JSIdentifier mem1Annot "y")) dot2 (JSIdentifier mem2Annot "z")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected nested member dot expression, got: " ++ show result)

  it "call expression" $ do
    case testExpr "x()" of
      Right (JSAstExpression (JSMemberExpression (JSIdentifier idAnnot "x") leftParen JSLNil rightParen) astAnnot) -> pure ()
      result -> expectationFailure ("Expected member expression call, got: " ++ show result)
    case testExpr "x()()" of
      Right (JSAstExpression (JSCallExpression (JSMemberExpression (JSIdentifier idAnnot "x") leftParen1 JSLNil rightParen1) leftParen2 JSLNil rightParen2) astAnnot) -> pure ()
      result -> expectationFailure ("Expected call expression, got: " ++ show result)
    case testExpr "x()[4]" of
      Right (JSAstExpression (JSCallExpressionSquare (JSMemberExpression (JSIdentifier idAnnot "x") leftParen JSLNil rightParen) leftBracket (JSDecimal numAnnot "4") rightBracket) astAnnot) -> pure ()
      result -> expectationFailure ("Expected call expression with square access, got: " ++ show result)
    case testExpr "x().x" of
      Right (JSAstExpression (JSCallExpressionDot (JSMemberExpression (JSIdentifier idAnnot "x") leftParen JSLNil rightParen) dot (JSIdentifier memAnnot "x")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected call expression with dot access, got: " ++ show result)
    case testExpr "x(a,b=2).x" of
      Right (JSAstExpression (JSCallExpressionDot (JSMemberExpression (JSIdentifier idAnnot "x") leftParen (JSLCons (JSLOne (JSIdentifier arg1Annot "a")) comma (JSAssignExpression (JSIdentifier arg2Annot "b") (JSAssign assignAnnot) (JSDecimal numAnnot "2"))) rightParen) dot (JSIdentifier memAnnot "x")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected call expression with args and dot access, got: " ++ show result)
    case testExpr "foo (56.8379100, 60.5806664)" of
      Right (JSAstExpression (JSMemberExpression (JSIdentifier idAnnot "foo") leftParen (JSLCons (JSLOne (JSDecimal num1Annot "56.8379100")) comma (JSDecimal num2Annot "60.5806664")) rightParen) astAnnot) -> pure ()
      result -> expectationFailure ("Expected member expression with decimal args, got: " ++ show result)

  it "trailing comma in function calls" $ do
    case testExpr "f(x,)" of
      Right (JSAstExpression (JSMemberExpression (JSIdentifier idAnnot "f") leftParen (JSLOne (JSIdentifier argAnnot "x")) rightParen) astAnnot) -> pure ()
      result -> expectationFailure ("Expected member expression with trailing comma, got: " ++ show result)
    case testExpr "f(a,b,)" of
      Right (JSAstExpression (JSMemberExpression (JSIdentifier idAnnot "f") leftParen (JSLCons (JSLOne (JSIdentifier arg1Annot "a")) comma (JSIdentifier arg2Annot "b")) rightParen) astAnnot) -> pure ()
      result -> expectationFailure ("Expected member expression with multiple args and trailing comma, got: " ++ show result)
    case testExpr "Math.max(10, 20,)" of
      Right (JSAstExpression (JSMemberExpression (JSMemberDot (JSIdentifier idAnnot "Math") dot (JSIdentifier memAnnot "max")) leftParen (JSLCons (JSLOne (JSDecimal num1Annot "10")) comma (JSDecimal num2Annot "20")) rightParen) astAnnot) -> pure ()
      result -> expectationFailure ("Expected method call with trailing comma, got: " ++ show result)
    -- Chained function calls with trailing commas
    case testExpr "f(x,)(y,)" of
      Right (JSAstExpression (JSCallExpression (JSMemberExpression (JSIdentifier idAnnot "f") leftParen1 (JSLOne (JSIdentifier arg1Annot "x")) rightParen1) leftParen2 (JSLOne (JSIdentifier arg2Annot "y")) rightParen2) astAnnot) -> pure ()
      result -> expectationFailure ("Expected chained call expression with trailing commas, got: " ++ show result)
    -- Complex expressions with trailing commas
    case testExpr "obj.method(a + b, c * d,)" of
      Right (JSAstExpression (JSMemberExpression (JSMemberDot (JSIdentifier idAnnot "obj") dot (JSIdentifier memAnnot "method")) leftParen (JSLCons (JSLOne (JSExpressionBinary (JSIdentifier id1Annot "a") (JSBinOpPlus plus1Annot) (JSIdentifier id2Annot "b"))) comma (JSExpressionBinary (JSIdentifier id3Annot "c") (JSBinOpTimes timesAnnot) (JSIdentifier id4Annot "d"))) rightParen) astAnnot) -> pure ()
      result -> expectationFailure ("Expected method call with binary expressions and trailing comma, got: " ++ show result)
    -- Single argument with trailing comma
    case testExpr "console.log('hello',)" of
      Right (JSAstExpression (JSMemberExpression (JSMemberDot (JSIdentifier idAnnot "console") dot (JSIdentifier memAnnot "log")) leftParen (JSLOne (JSStringLiteral strAnnot "'hello'")) rightParen) astAnnot) -> pure ()
      result -> expectationFailure ("Expected console.log call with trailing comma, got: " ++ show result)

  it "dynamic imports (ES2020) - current parser support" $ do
    -- Note: Parser now supports dynamic import() expressions
    -- These tests verify current parsing capabilities
    parse "import('./module.js')" "test" `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
    parse "const mod = import('module')" "test" `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)  -- Assignment now works
    parse "import(moduleSpecifier)" "test" `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
    parse "import('./utils.js').then(m => m.helper())" "test" `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)  -- Complex chaining now works
    parse "await import('./async-module.js')" "test" `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)  -- Await import now works

  it "spread expression" $ do
    case testExpr "... x" of
      Right (JSAstExpression (JSSpreadExpression _ (JSIdentifier _ "x")) _) -> pure ()
      result -> expectationFailure ("Expected spread expression, got: " ++ show result)

  it "template literal" $ do
    case testExpr "``" of
      Right (JSAstExpression (JSTemplateLiteral Nothing backquote "``" []) astAnnot) -> pure ()
      result -> expectationFailure ("Expected empty template literal, got: " ++ show result)
    case testExpr "`$`" of
      Right (JSAstExpression (JSTemplateLiteral Nothing backquote "`$`" []) astAnnot) -> pure ()
      result -> expectationFailure ("Expected template literal with dollar sign, got: " ++ show result)
    case testExpr "`$\\n`" of
      Right (JSAstExpression (JSTemplateLiteral Nothing backquote "`$\\n`" []) astAnnot) -> pure ()
      result -> expectationFailure ("Expected template literal with escape sequence, got: " ++ show result)
    case testExpr "`\\${x}`" of
      Right (JSAstExpression (JSTemplateLiteral Nothing backquote "`\\${x}`" []) astAnnot) -> pure ()
      result -> expectationFailure ("Expected template literal with escaped interpolation, got: " ++ show result)
    case testExpr "`$ {x}`" of
      Right (JSAstExpression (JSTemplateLiteral Nothing backquote "`$ {x}`" []) astAnnot) -> pure ()
      result -> expectationFailure ("Expected template literal with space before brace, got: " ++ show result)
    case testExpr "`\n\n`" of
      Right (JSAstExpression (JSTemplateLiteral Nothing backquote "`\n\n`" []) astAnnot) -> pure ()
      result -> expectationFailure ("Expected template literal with newlines, got: " ++ show result)
    case testExpr "`${x+y} ${z}`" of
      Right (JSAstExpression (JSTemplateLiteral Nothing backquote "`${" [JSTemplatePart (JSExpressionBinary (JSIdentifier id1Annot "x") (JSBinOpPlus plusAnnot) (JSIdentifier id2Annot "y")) rightBrace "} ${", JSTemplatePart (JSIdentifier idAnnot "z") rightBrace2 "}`"]) astAnnot) -> pure ()
      Right other -> expectationFailure ("Expected template literal with interpolations, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for template literal, got: " ++ show result)
    case testExpr "`<${x} ${y}>`" of
      Right (JSAstExpression (JSTemplateLiteral Nothing backquote "`<${" [JSTemplatePart (JSIdentifier id1Annot "x") rightBrace1 "} ${", JSTemplatePart (JSIdentifier id2Annot "y") rightBrace2 "}>`"]) astAnnot) -> pure ()
      Right other -> expectationFailure ("Expected template literal with HTML-like interpolations, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for template literal, got: " ++ show result)
    case testExpr "tag `xyz`" of
      Right (JSAstExpression (JSTemplateLiteral (Just (JSIdentifier tagAnnot "tag")) backquote "`xyz`" []) astAnnot) -> pure ()
      result -> expectationFailure ("Expected tagged template literal, got: " ++ show result)
    case testExpr "tag()`xyz`" of
      Right (JSAstExpression (JSTemplateLiteral (Just (JSMemberExpression (JSIdentifier tagAnnot "tag") leftParen JSLNil rightParen)) backquote "`xyz`" []) astAnnot) -> pure ()
      result -> expectationFailure ("Expected template literal with function call tag, got: " ++ show result)

  it "yield" $ do
    case testExpr "yield" of
      Right (JSAstExpression (JSYieldExpression yieldAnnot Nothing) astAnnot) -> pure ()
      result -> expectationFailure ("Expected yield expression without value, got: " ++ show result)
    case testExpr "yield a + b" of
      Right (JSAstExpression (JSYieldExpression yieldAnnot (Just (JSExpressionBinary (JSIdentifier id1Annot "a") (JSBinOpPlus plusAnnot) (JSIdentifier id2Annot "b")))) astAnnot) -> pure ()
      result -> expectationFailure ("Expected yield expression with binary operation, got: " ++ show result)
    case testExpr "yield* g()" of
      Right (JSAstExpression (JSYieldFromExpression yieldAnnot starAnnot (JSMemberExpression (JSIdentifier idAnnot "g") leftParen JSLNil rightParen)) astAnnot) -> pure ()
      result -> expectationFailure ("Expected yield from expression with function call, got: " ++ show result)

  it "class expression" $ do
    case testExpr "class Foo extends Bar { a(x,y) {} *b() {} }" of
      Right (JSAstExpression (JSClassExpression classAnnot (JSIdentName nameAnnot "Foo") (JSExtends extendsAnnot (JSIdentifier heritageAnnot "Bar")) leftBrace body rightBrace) astAnnot) -> pure ()
      result -> expectationFailure ("Expected class expression with inheritance and methods, got: " ++ show result)
    case testExpr "class { static get [a]() {}; }" of
      Right (JSAstExpression (JSClassExpression classAnnot JSIdentNone JSExtendsNone leftBrace body rightBrace) astAnnot) -> pure ()
      result -> expectationFailure ("Expected anonymous class expression with static getter, got: " ++ show result)
    case testExpr "class Foo extends Bar { a(x,y) { super(x); } }" of
      Right (JSAstExpression (JSClassExpression classAnnot (JSIdentName nameAnnot "Foo") (JSExtends extendsAnnot (JSIdentifier heritageAnnot "Bar")) leftBrace body rightBrace) astAnnot) -> pure ()
      result -> expectationFailure ("Expected class expression with super call, got: " ++ show result)

  it "optional chaining" $ do
    case testExpr "obj?.prop" of
      Right (JSAstExpression (JSOptionalMemberDot (JSIdentifier idAnnot "obj") optionalDot (JSIdentifier memAnnot "prop")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected optional member dot access, got: " ++ show result)
    case testExpr "obj?.[key]" of
      Right (JSAstExpression (JSOptionalMemberSquare (JSIdentifier idAnnot "obj") optionalBracket (JSIdentifier keyAnnot "key") rightBracket) astAnnot) -> pure ()
      result -> expectationFailure ("Expected optional member square access, got: " ++ show result)
    case testExpr "obj?.method()" of
      Right (JSAstExpression (JSMemberExpression (JSOptionalMemberDot (JSIdentifier idAnnot "obj") optionalDot (JSIdentifier memAnnot "method")) leftParen JSLNil rightParen) astAnnot) -> pure ()
      result -> expectationFailure ("Expected member expression with optional method call, got: " ++ show result)
    case testExpr "obj?.prop?.deep" of
      Right (JSAstExpression (JSOptionalMemberDot (JSOptionalMemberDot (JSIdentifier idAnnot "obj") optionalDot1 (JSIdentifier mem1Annot "prop")) optionalDot2 (JSIdentifier mem2Annot "deep")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected chained optional member dot access, got: " ++ show result)
    case testExpr "obj?.method?.(args)" of
      Right (JSAstExpression (JSOptionalCallExpression (JSOptionalMemberDot (JSIdentifier idAnnot "obj") optionalDot (JSIdentifier memAnnot "method")) optionalParen (JSLOne (JSIdentifier argAnnot "args")) rightParen) astAnnot) -> pure ()
      result -> expectationFailure ("Expected optional call expression, got: " ++ show result)
    case testExpr "arr?.[0]?.value" of
      Right (JSAstExpression (JSOptionalMemberDot (JSOptionalMemberSquare (JSIdentifier idAnnot "arr") optionalBracket (JSDecimal numAnnot "0") rightBracket) optionalDot (JSIdentifier memAnnot "value")) astAnnot) -> pure ()
      result -> expectationFailure ("Expected chained optional access with square and dot, got: " ++ show result)

  it "nullish coalescing precedence" $ do
    case testExpr "x ?? y || z" of
      Right
        ( JSAstExpression
            ( JSExpressionBinary
                (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpNullishCoalescing opAnnot1) (JSIdentifier rightIdAnnot "y"))
                (JSBinOpOr opAnnot2)
                (JSIdentifier idAnnot "z")
              )
            astAnnot
          ) -> pure ()
      result -> expectationFailure ("Expected nullish coalescing with lower precedence than OR, got: " ++ show result)
    case testExpr "x || y ?? z" of
      Right
        ( JSAstExpression
            ( JSExpressionBinary
                (JSIdentifier idAnnot "x")
                (JSBinOpOr opAnnot1)
                (JSExpressionBinary (JSIdentifier leftIdAnnot "y") (JSBinOpNullishCoalescing opAnnot2) (JSIdentifier rightIdAnnot "z"))
              )
            astAnnot
          ) -> pure ()
      result -> expectationFailure ("Expected OR with higher precedence than nullish coalescing, got: " ++ show result)
    case testExpr "null ?? 'default'" of
      Right
        ( JSAstExpression
            ( JSExpressionBinary
                (JSLiteral litAnnot "null")
                (JSBinOpNullishCoalescing opAnnot)
                (JSStringLiteral strAnnot "'default'")
              )
            astAnnot
          ) -> pure ()
      result -> expectationFailure ("Expected nullish coalescing with null and string, got: " ++ show result)
    case testExpr "undefined ?? 0" of
      Right
        ( JSAstExpression
            ( JSExpressionBinary
                (JSIdentifier idAnnot "undefined")
                (JSBinOpNullishCoalescing opAnnot)
                (JSDecimal numAnnot "0")
              )
            astAnnot
          ) -> pure ()
      result -> expectationFailure ("Expected nullish coalescing with undefined and number, got: " ++ show result)
    case testExpr "x ?? y ?? z" of
      Right
        ( JSAstExpression
            ( JSExpressionBinary
                (JSExpressionBinary (JSIdentifier leftIdAnnot "x") (JSBinOpNullishCoalescing opAnnot1) (JSIdentifier rightIdAnnot "y"))
                (JSBinOpNullishCoalescing opAnnot2)
                (JSIdentifier idAnnot "z")
              )
            astAnnot
          ) -> pure ()
      result -> expectationFailure ("Expected left-associative nullish coalescing, got: " ++ show result)

  it "static class expressions (ES2015) - supported features" $ do
    -- Basic static method in class expression
    case testExpr "class { static method() {} }" of
      Right (JSAstExpression (JSClassExpression classAnnot JSIdentNone JSExtendsNone leftBrace body rightBrace) astAnnot) -> pure ()
      result -> expectationFailure ("Expected anonymous class expression with static method, got: " ++ show result)
    -- Named class expression with static methods
    case testExpr "class Calculator { static add(a, b) { return a + b; } }" of
      Right (JSAstExpression (JSClassExpression classAnnot (JSIdentName nameAnnot "Calculator") JSExtendsNone leftBrace body rightBrace) astAnnot) -> pure ()
      result -> expectationFailure ("Expected named class expression with static method, got: " ++ show result)
    -- Static getter in class expression
    case testExpr "class { static get version() { return '2.0'; } }" of
      Right (JSAstExpression (JSClassExpression classAnnot JSIdentNone JSExtendsNone leftBrace body rightBrace) astAnnot) -> pure ()
      result -> expectationFailure ("Expected anonymous class expression with static getter, got: " ++ show result)
    -- Static setter in class expression
    case testExpr "class { static set config(val) { this._config = val; } }" of
      Right (JSAstExpression (JSClassExpression classAnnot JSIdentNone JSExtendsNone leftBrace body rightBrace) astAnnot) -> pure ()
      result -> expectationFailure ("Expected anonymous class expression with static setter, got: " ++ show result)
    -- Static computed property
    case testExpr "class { static [Symbol.iterator]() {} }" of
      Right (JSAstExpression (JSClassExpression classAnnot JSIdentNone JSExtendsNone leftBrace body rightBrace) astAnnot) -> pure ()
      result -> expectationFailure ("Expected anonymous class expression with static computed property, got: " ++ show result)
    -- Multiple static features
    case testExpr "class Util { static method() {} static get prop() {} }" of
      Right (JSAstExpression (JSClassExpression classAnnot (JSIdentName nameAnnot "Util") JSExtendsNone leftBrace body rightBrace) astAnnot) -> pure ()
      result -> expectationFailure ("Expected named class expression with multiple static features, got: " ++ show result)

testExpr :: String -> Either String JSAST
testExpr str = parseUsing parseExpression str "src"
