{-# LANGUAGE OverloadedStrings #-}

module Unit.Language.Javascript.Parser.Parser.Expressions
  ( testExpressionParser,
  )
where

import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
  ( JSAccessor (..),
    JSArrayElement (..),
    JSArrowParameterList (..),
    JSClassHeritage (..),
    JSCommaList (..),
    JSCommaTrailingList (..),
    JSConciseBody (..),
    JSIdent (..),
    JSMethodDefinition (..),
    JSObjectProperty (..),
    JSPropertyName (..),
    JSTemplatePart (..),
  )
import Language.JavaScript.Parser.Parser (parseExpression, parseUsing)
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
      Right (JSAstExpression (JSExpressionBinary (JSExpressionBinary (JSDecimal _num1Annot 2) (JSBinOpPlus _plus1Annot) (JSExpressionBinary (JSDecimal _num2Annot 3) (JSBinOpTimes _timesAnnot) (JSDecimal _num3Annot 4))) (JSBinOpPlus _plus2Annot) (JSDecimal _num4Annot 5)) _astAnnot) -> pure ()
      Right other -> expectationFailure ("Expected binary expression with correct precedence for 2+3*4+5, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for 2+3*4+5, got: " ++ show result)
    case testExpr "2*3**4" of
      Right (JSAstExpression (JSExpressionBinary (JSDecimal _num1Annot 2) (JSBinOpTimes _timesAnnot) (JSExpressionBinary (JSDecimal _num2Annot 3) (JSBinOpExponentiation _expAnnot) (JSDecimal _num3Annot 4))) _astAnnot) -> pure ()
      Right other -> expectationFailure ("Expected binary expression with correct precedence for 2*3**4, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for 2*3**4, got: " ++ show result)
    case testExpr "2**3*4" of
      Right (JSAstExpression (JSExpressionBinary (JSExpressionBinary (JSDecimal _num1Annot 2) (JSBinOpExponentiation _expAnnot) (JSDecimal _num2Annot 3)) (JSBinOpTimes _timesAnnot) (JSDecimal _num3Annot 4)) _astAnnot) -> pure ()
      Right other -> expectationFailure ("Expected binary expression with correct precedence for 2**3*4, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for 2**3*4, got: " ++ show result)
  it "parentheses" $
    case testExpr "(56)" of
      Right (JSAstExpression (JSExpressionParen _ (JSDecimal _ 56) _) _) -> pure ()
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
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "x") _ [JSDecimal _ 1]))) _) _) -> pure ()
      Right other -> expectationFailure ("Expected object literal {x:1} with property x=1, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for {x:1}, got: " ++ show result)
    case testExpr "{x:1,y:2}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLCons (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "x") _ [JSDecimal _ 1])) _ (JSPropertyNameandValue (JSPropertyIdent _ "y") _ [JSDecimal _ 2]))) _) _) -> pure ()
      Right other -> expectationFailure ("Expected object literal {x:1,y:2} with properties x=1, y=2, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for {x:1,y:2}, got: " ++ show result)
    case testExpr "{x:1,}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLComma (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "x") _ [JSDecimal _ 1])) _) _) _) -> pure ()
      Right other -> expectationFailure ("Expected object literal {x:1,} with trailing comma, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for {x:1,}, got: " ++ show result)
    case testExpr "{yield:1}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "yield") _ [JSDecimal _ 1]))) _) _) -> pure ()
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
                            (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "if") _ [JSDecimal _ 1]))
                            _
                            (JSPropertyNameandValue (JSPropertyIdent _ "interface") _ [JSDecimal _ 2])
                          )
                      )
                    _
                  )
              )
            _
          ) -> pure ()
      result -> expectationFailure ("Expected assignment with object literal, got: " ++ show result)
    case testExpr "a={\n  values: 7,\n}\n" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _ "a") (JSAssign _) (JSObjectLiteral _ (JSCTLComma (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "values") _ [JSDecimal _ 7])) _) _)) _) -> pure ()
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
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLOne (JSPropertyNameandValue (JSPropertyComputed _ (JSIdentifier _ "x") _) _ [JSDecimal _ 1]))) _) _) -> pure ()
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
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLCons (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "a") _ [JSDecimal _ 1])) _ (JSObjectSpread _ (JSIdentifier _ "obj")))) _) _) -> pure ()
      result -> expectationFailure ("Expected object literal with property and spread, got: " ++ show result)
    case testExpr "{...obj, b: 2}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLCons (JSLOne (JSObjectSpread _ (JSIdentifier _ "obj"))) _ (JSPropertyNameandValue (JSPropertyIdent _ "b") _ [JSDecimal _ 2]))) _) _) -> pure ()
      result -> expectationFailure ("Expected object literal with spread and property, got: " ++ show result)
    case testExpr "{a: 1, ...obj, b: 2}" of
      Right (JSAstExpression (JSObjectLiteral _ (JSCTLNone (JSLCons (JSLCons (JSLOne (JSPropertyNameandValue (JSPropertyIdent _ "a") _ [JSDecimal _ 1])) _ (JSObjectSpread _ (JSIdentifier _ "obj"))) _ (JSPropertyNameandValue (JSPropertyIdent _ "b") _ [JSDecimal _ 2]))) _) _) -> pure ()
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
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpTypeof _opAnnot) (JSIdentifier _idAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary typeof expression, got: " ++ show result)
    case testExpr "++y" of
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpIncr _opAnnot) (JSIdentifier _idAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary increment expression, got: " ++ show result)
    case testExpr "--y" of
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpDecr _opAnnot) (JSIdentifier _idAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary decrement expression, got: " ++ show result)
    case testExpr "+y" of
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpPlus _opAnnot) (JSIdentifier _idAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary plus expression, got: " ++ show result)
    case testExpr "-y" of
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpMinus _opAnnot) (JSIdentifier _idAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary minus expression, got: " ++ show result)
    case testExpr "~y" of
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpTilde _opAnnot) (JSIdentifier _idAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary bitwise not expression, got: " ++ show result)
    case testExpr "!y" of
      Right (JSAstExpression (JSUnaryExpression (JSUnaryOpNot _opAnnot) (JSIdentifier _idAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected unary logical not expression, got: " ++ show result)
    case testExpr "y++" of
      Right (JSAstExpression (JSExpressionPostfix (JSIdentifier _idAnnot "y") (JSUnaryOpIncr _opAnnot)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected postfix increment expression, got: " ++ show result)
    case testExpr "y--" of
      Right (JSAstExpression (JSExpressionPostfix (JSIdentifier _idAnnot "y") (JSUnaryOpDecr _opAnnot)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected postfix decrement expression, got: " ++ show result)
    case testExpr "...y" of
      Right (JSAstExpression (JSSpreadExpression _ (JSIdentifier _ "y")) _) -> pure ()
      result -> expectationFailure ("Expected spread expression, got: " ++ show result)

  it "new expression" $ do
    case testExpr "new x()" of
      Right (JSAstExpression (JSMemberNew _newAnnot (JSIdentifier _idAnnot "x") _leftParen JSLNil _rightParen) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected new expression with call, got: " ++ show result)
    case testExpr "new x.y" of
      Right (JSAstExpression (JSNewExpression _newAnnot (JSMemberDot (JSIdentifier _idAnnot "x") _dot (JSIdentifier _memAnnot "y"))) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected new expression with member access, got: " ++ show result)

  it "binary expression" $ do
    case testExpr "x||y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpOr _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary logical or expression, got: " ++ show result)
    case testExpr "x&&y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpAnd _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary logical and expression, got: " ++ show result)
    case testExpr "x??y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpNullishCoalescing _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary nullish coalescing expression, got: " ++ show result)
    case testExpr "x|y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpBitOr _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary bitwise or expression, got: " ++ show result)
    case testExpr "x^y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpBitXor _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary bitwise xor expression, got: " ++ show result)
    case testExpr "x&y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpBitAnd _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary bitwise and expression, got: " ++ show result)

    case testExpr "x==y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpEq _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary equality expression, got: " ++ show result)
    case testExpr "x!=y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpNeq _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary inequality expression, got: " ++ show result)
    case testExpr "x===y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpStrictEq _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary strict equality expression, got: " ++ show result)
    case testExpr "x!==y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpStrictNeq _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary strict inequality expression, got: " ++ show result)

    case testExpr "x<y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpLt _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary less than expression, got: " ++ show result)
    case testExpr "x>y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpGt _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary greater than expression, got: " ++ show result)
    case testExpr "x<=y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpLe _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary less than or equal expression, got: " ++ show result)
    case testExpr "x>=y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpGe _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary greater than or equal expression, got: " ++ show result)

    case testExpr "x<<y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpLsh _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary left shift expression, got: " ++ show result)
    case testExpr "x>>y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpRsh _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary right shift expression, got: " ++ show result)
    case testExpr "x>>>y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpUrsh _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary unsigned right shift expression, got: " ++ show result)

    case testExpr "x+y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpPlus _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary addition expression, got: " ++ show result)
    case testExpr "x-y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpMinus _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary subtraction expression, got: " ++ show result)

    case testExpr "x*y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpTimes _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary multiplication expression, got: " ++ show result)
    case testExpr "x**y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpExponentiation _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary exponentiation expression, got: " ++ show result)
    case testExpr "x**y**z" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _idAnnot "x") (JSBinOpExponentiation _opAnnot1) (JSExpressionBinary (JSIdentifier _leftIdAnnot "y") (JSBinOpExponentiation _opAnnot2) (JSIdentifier _rightIdAnnot "z"))) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected nested exponentiation expression, got: " ++ show result)
    case testExpr "2**3**2" of
      Right (JSAstExpression (JSExpressionBinary (JSDecimal _numAnnot1 2) (JSBinOpExponentiation _opAnnot1) (JSExpressionBinary (JSDecimal _numAnnot2 3) (JSBinOpExponentiation _opAnnot2) (JSDecimal _numAnnot3 2))) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected numeric exponentiation expression, got: " ++ show result)
    case testExpr "x/y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpDivide _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary division expression, got: " ++ show result)
    case testExpr "x%y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpMod _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected binary modulo expression, got: " ++ show result)
    case testExpr "x instanceof y" of
      Right (JSAstExpression (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpInstanceOf _opAnnot) (JSIdentifier _rightIdAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected instanceof expression, got: " ++ show result)

  it "assign expression" $ do
    case testExpr "x=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _idAnnot "x") (JSAssign _assignAnnot) (JSDecimal _numAnnot 1)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected assignment expression x=1, got: " ++ show result)
    case testExpr "x*=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _idAnnot "x") (JSTimesAssign _assignAnnot) (JSDecimal _numAnnot 1)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected multiply assignment expression, got: " ++ show result)
    case testExpr "x/=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _idAnnot "x") (JSDivideAssign _assignAnnot) (JSDecimal _numAnnot 1)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected divide assignment expression, got: " ++ show result)
    case testExpr "x%=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _idAnnot "x") (JSModAssign _assignAnnot) (JSDecimal _numAnnot 1)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected modulo assignment expression, got: " ++ show result)
    case testExpr "x+=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _idAnnot "x") (JSPlusAssign _assignAnnot) (JSDecimal _numAnnot 1)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected add assignment expression, got: " ++ show result)
    case testExpr "x-=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _idAnnot "x") (JSMinusAssign _assignAnnot) (JSDecimal _numAnnot 1)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected subtract assignment expression, got: " ++ show result)
    case testExpr "x<<=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _idAnnot "x") (JSLshAssign _assignAnnot) (JSDecimal _numAnnot 1)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected left shift assignment expression, got: " ++ show result)
    case testExpr "x>>=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _idAnnot "x") (JSRshAssign _assignAnnot) (JSDecimal _numAnnot 1)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected right shift assignment expression, got: " ++ show result)
    case testExpr "x>>>=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _idAnnot "x") (JSUrshAssign _assignAnnot) (JSDecimal _numAnnot 1)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected unsigned right shift assignment expression, got: " ++ show result)
    case testExpr "x&=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _idAnnot "x") (JSBwAndAssign _assignAnnot) (JSDecimal _numAnnot 1)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected bitwise and assignment expression, got: " ++ show result)

  it "destructuring assignment expressions (ES2015) - supported features" $ do
    -- Array destructuring assignment
    case testExpr "[a, b] = arr" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral _leftBracket [JSArrayElement (JSIdentifier _elem1Annot "a"), JSArrayComma _comma, JSArrayElement (JSIdentifier _elem2Annot "b")] _rightBracket) (JSAssign _assignAnnot) (JSIdentifier _idAnnot "arr")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected array destructuring assignment, got: " ++ show result)
    case testExpr "[x, y, z] = coordinates" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral _leftBracket [JSArrayElement (JSIdentifier _elem1Annot "x"), JSArrayComma _comma1, JSArrayElement (JSIdentifier _elem2Annot "y"), JSArrayComma _comma2, JSArrayElement (JSIdentifier _elem3Annot "z")] _rightBracket) (JSAssign _assignAnnot) (JSIdentifier _idAnnot "coordinates")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected array destructuring assignment, got: " ++ show result)

    -- Object destructuring assignment
    case testExpr "{a, b} = obj" of
      Right (JSAstExpression (JSAssignExpression (JSObjectLiteral _leftBrace (JSCTLNone (JSLCons (JSLOne (JSPropertyIdentRef _prop1Annot "a")) _comma (JSPropertyIdentRef _prop2Annot "b"))) _rightBrace) (JSAssign _assignAnnot) (JSIdentifier _idAnnot "obj")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected object destructuring assignment, got: " ++ show result)
    case testExpr "{name, age} = person" of
      Right (JSAstExpression (JSAssignExpression (JSObjectLiteral _leftBrace (JSCTLNone (JSLCons (JSLOne (JSPropertyIdentRef _prop1Annot "name")) _comma (JSPropertyIdentRef _prop2Annot "age"))) _rightBrace) (JSAssign _assignAnnot) (JSIdentifier _idAnnot "person")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected object destructuring assignment, got: " ++ show result)

    -- Nested destructuring assignment
    case testExpr "[a, [b, c]] = nested" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral _leftBracket1 [JSArrayElement (JSIdentifier _elem1Annot "a"), JSArrayComma _comma1, JSArrayElement (JSArrayLiteral _leftBracket2 [JSArrayElement (JSIdentifier _elem2Annot "b"), JSArrayComma _comma2, JSArrayElement (JSIdentifier _elem3Annot "c")] _rightBracket2)] _rightBracket1) (JSAssign _assignAnnot) (JSIdentifier _idAnnot "nested")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected nested array destructuring assignment, got: " ++ show result)
    case testExpr "{a: {b}} = deep" of
      Right (JSAstExpression (JSAssignExpression (JSObjectLiteral _leftBrace1 (JSCTLNone (JSLOne (JSPropertyNameandValue (JSPropertyIdent _propAnnot "a") _colon [JSObjectLiteral _leftBrace2 (JSCTLNone (JSLOne (JSPropertyIdentRef _prop2Annot "b"))) _rightBrace2]))) _rightBrace1) (JSAssign _assignAnnot) (JSIdentifier _idAnnot "deep")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected nested object destructuring assignment, got: " ++ show result)

    -- Rest pattern assignment
    case testExpr "[first, ...rest] = array" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral _leftBracket [JSArrayElement (JSIdentifier _elem1Annot "first"), JSArrayComma _comma, JSArrayElement (JSSpreadExpression _spreadAnnot (JSIdentifier _elem2Annot "rest"))] _rightBracket) (JSAssign _assignAnnot) (JSIdentifier _idAnnot "array")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected rest pattern assignment, got: " ++ show result)

    -- Sparse array assignment
    case testExpr "[, , third] = sparse" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral _leftBracket [JSArrayComma _comma1, JSArrayComma _comma2, JSArrayElement (JSIdentifier _elemAnnot "third")] _rightBracket) (JSAssign _assignAnnot) (JSIdentifier _idAnnot "sparse")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected sparse array assignment, got: " ++ show result)

    -- Property renaming assignment
    case testExpr "{prop: newName} = obj" of
      Right (JSAstExpression (JSAssignExpression (JSObjectLiteral _leftBrace (JSCTLNone (JSLOne (JSPropertyNameandValue (JSPropertyIdent _propAnnot "prop") _colon [JSIdentifier _idAnnot "newName"]))) _rightBrace) (JSAssign _assignAnnot) (JSIdentifier _idAnnot2 "obj")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected property renaming assignment, got: " ++ show result)

    -- Array destructuring with default values (parsed as assignment expressions)
    case testExpr "[a = 1, b = 2] = arr" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral _leftBracket [JSArrayElement (JSAssignExpression (JSIdentifier _elem1Annot "a") (JSAssign _assign1Annot) (JSDecimal _num1Annot 1)), JSArrayComma _comma, JSArrayElement (JSAssignExpression (JSIdentifier _elem2Annot "b") (JSAssign _assign2Annot) (JSDecimal _num2Annot 2))] _rightBracket) (JSAssign _assignAnnot) (JSIdentifier _idAnnot "arr")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected array destructuring with defaults, got: " ++ show result)
    case testExpr "[x = 'default', y] = values" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral _leftBracket [JSArrayElement (JSAssignExpression (JSIdentifier _elem1Annot "x") (JSAssign _assign1Annot) (JSStringLiteral _strAnnot "'default'")), JSArrayComma _comma, JSArrayElement (JSIdentifier _elem2Annot "y")] _rightBracket) (JSAssign _assignAnnot) (JSIdentifier _idAnnot "values")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected array destructuring with default, got: " ++ show result)

    -- Mixed array destructuring with defaults and rest
    case testExpr "[first, second = 42, ...rest] = data" of
      Right (JSAstExpression (JSAssignExpression (JSArrayLiteral _leftBracket [JSArrayElement (JSIdentifier _elem1Annot "first"), JSArrayComma _comma1, JSArrayElement (JSAssignExpression (JSIdentifier _elem2Annot "second") (JSAssign _assignAnnot) (JSDecimal _numAnnot 42)), JSArrayComma _comma2, JSArrayElement (JSSpreadExpression _spreadAnnot (JSIdentifier _elem3Annot "rest"))] _rightBracket) (JSAssign _assignAnnot2) (JSIdentifier _idAnnot "data")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected mixed array destructuring assignment, got: " ++ show result)
    case testExpr "x^=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _idAnnot "x") (JSBwXorAssign _assignAnnot) (JSDecimal _numAnnot 1)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected bitwise xor assignment expression, got: " ++ show result)
    case testExpr "x|=1" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _idAnnot "x") (JSBwOrAssign _assignAnnot) (JSDecimal _numAnnot 1)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected bitwise or assignment expression, got: " ++ show result)

  it "logical assignment operators" $ do
    case testExpr "x&&=true" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _idAnnot "x") (JSLogicalAndAssign _assignAnnot) (JSLiteral _litAnnot "true")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected logical and assignment expression, got: " ++ show result)
    case testExpr "x||=false" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _idAnnot "x") (JSLogicalOrAssign _assignAnnot) (JSLiteral _litAnnot "false")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected logical or assignment expression, got: " ++ show result)
    case testExpr "x??=null" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _idAnnot "x") (JSNullishAssign _assignAnnot) (JSLiteral _litAnnot "null")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected nullish assignment expression, got: " ++ show result)
    case testExpr "obj.prop&&=value" of
      Right (JSAstExpression (JSAssignExpression (JSMemberDot (JSIdentifier _idAnnot "obj") _dot (JSIdentifier _memAnnot "prop")) (JSLogicalAndAssign _assignAnnot) (JSIdentifier _valAnnot "value")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected member dot logical and assignment, got: " ++ show result)
    case testExpr "arr[0]||=defaultValue" of
      Right (JSAstExpression (JSAssignExpression (JSMemberSquare (JSIdentifier _idAnnot "arr") _leftBracket (JSDecimal _numAnnot 0) _rightBracket) (JSLogicalOrAssign _assignAnnot) (JSIdentifier _valAnnot "defaultValue")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected member square logical or assignment, got: " ++ show result)
    case testExpr "config.timeout??=5000" of
      Right (JSAstExpression (JSAssignExpression (JSMemberDot (JSIdentifier _idAnnot "config") _dot (JSIdentifier _memAnnot "timeout")) (JSNullishAssign _assignAnnot) (JSDecimal _numAnnot 5000)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected member dot nullish assignment, got: " ++ show result)
    case testExpr "a&&=b&&=c" of
      Right (JSAstExpression (JSAssignExpression (JSIdentifier _id1Annot "a") (JSLogicalAndAssign _assign1Annot) (JSAssignExpression (JSIdentifier _id2Annot "b") (JSLogicalAndAssign _assign2Annot) (JSIdentifier _id3Annot "c"))) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected nested logical and assignment, got: " ++ show result)

  it "function expression" $ do
    case testExpr "function(){}" of
      Right (JSAstExpression (JSFunctionExpression _funcAnnot JSIdentNone _leftParen JSLNil _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with no params, got: " ++ show result)
    case testExpr "function(a){}" of
      Right (JSAstExpression (JSFunctionExpression _funcAnnot JSIdentNone _leftParen (JSLOne (JSIdentifier _paramAnnot "a")) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with one param, got: " ++ show result)
    case testExpr "function(a,b){}" of
      Right (JSAstExpression (JSFunctionExpression _funcAnnot JSIdentNone _leftParen (JSLCons (JSLOne (JSIdentifier _param1Annot "a")) _comma (JSIdentifier _param2Annot "b")) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with two params, got: " ++ show result)
    case testExpr "function(...a){}" of
      Right (JSAstExpression (JSFunctionExpression _funcAnnot JSIdentNone _leftParen (JSLOne (JSSpreadExpression _spreadAnnot (JSIdentifier _paramAnnot "a"))) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with rest param, got: " ++ show result)
    case testExpr "function(a=1){}" of
      Right (JSAstExpression (JSFunctionExpression _funcAnnot JSIdentNone _leftParen (JSLOne (JSAssignExpression (JSIdentifier _paramAnnot "a") (JSAssign _assignAnnot) (JSDecimal _numAnnot 1))) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with default param, got: " ++ show result)
    case testExpr "function([a,b]){}" of
      Right (JSAstExpression (JSFunctionExpression _funcAnnot JSIdentNone _leftParen (JSLOne (JSArrayLiteral _leftBracket [JSArrayElement (JSIdentifier _elem1Annot "a"), JSArrayComma _comma, JSArrayElement (JSIdentifier _elem2Annot "b")] _rightBracket)) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with array destructuring, got: " ++ show result)
    case testExpr "function([a,...b]){}" of
      Right (JSAstExpression (JSFunctionExpression _funcAnnot JSIdentNone _leftParen (JSLOne (JSArrayLiteral _leftBracket [JSArrayElement (JSIdentifier _elem1Annot "a"), JSArrayComma _comma, JSArrayElement (JSSpreadExpression _spreadAnnot (JSIdentifier _elem2Annot "b"))] _rightBracket)) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with array destructuring and rest, got: " ++ show result)
    case testExpr "function({a,b}){}" of
      Right (JSAstExpression (JSFunctionExpression _funcAnnot JSIdentNone _leftParen (JSLOne (JSObjectLiteral _leftBrace (JSCTLNone (JSLCons (JSLOne (JSPropertyIdentRef _prop1Annot "a")) _comma (JSPropertyIdentRef _prop2Annot "b"))) _rightBrace)) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with object destructuring, got: " ++ show result)
    case testExpr "a => {}" of
      Right (JSAstExpression (JSArrowExpression (JSUnparenthesizedArrowParameter (JSIdentName _paramAnnot "a")) _arrow (JSConciseFunctionBody (JSBlock _leftBrace [] _rightBrace))) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with single param, got: " ++ show result)
    case testExpr "(a) => { a + 2 }" of
      Right (JSAstExpression (JSArrowExpression (JSParenthesizedArrowParameterList _leftParen (JSLOne (JSIdentifier _paramAnnot "a")) _rightParen) _arrow (JSConciseFunctionBody (JSBlock _leftBrace _body _rightBrace))) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with paren param, got: " ++ show result)
    case testExpr "(a, b) => {}" of
      Right (JSAstExpression (JSArrowExpression (JSParenthesizedArrowParameterList _leftParen (JSLCons (JSLOne (JSIdentifier _param1Annot "a")) _comma (JSIdentifier _param2Annot "b")) _rightParen) _arrow (JSConciseFunctionBody (JSBlock _leftBrace [] _rightBrace))) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with two params, got: " ++ show result)
    case testExpr "(a, b) => a + b" of
      Right (JSAstExpression (JSArrowExpression (JSParenthesizedArrowParameterList _leftParen (JSLCons (JSLOne (JSIdentifier _param1Annot "a")) _comma (JSIdentifier _param2Annot "b")) _rightParen) _arrow (JSConciseExpressionBody (JSExpressionBinary (JSIdentifier _id1Annot "a") (JSBinOpPlus _plusAnnot) (JSIdentifier _id2Annot "b")))) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with expression body, got: " ++ show result)
    case testExpr "() => { 42 }" of
      Right (JSAstExpression (JSArrowExpression (JSParenthesizedArrowParameterList _leftParen JSLNil _rightParen) _arrow (JSConciseFunctionBody (JSBlock _leftBrace _body _rightBrace))) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with no params, got: " ++ show result)
    case testExpr "(a, ...b) => b" of
      Right (JSAstExpression (JSArrowExpression (JSParenthesizedArrowParameterList _leftParen (JSLCons (JSLOne (JSIdentifier _param1Annot "a")) _comma (JSSpreadExpression _spreadAnnot (JSIdentifier _param2Annot "b"))) _rightParen) _arrow (JSConciseExpressionBody (JSIdentifier _idAnnot "b"))) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with rest param, got: " ++ show result)
    case testExpr "(a,b=1) => a + b" of
      Right (JSAstExpression (JSArrowExpression (JSParenthesizedArrowParameterList _leftParen (JSLCons (JSLOne (JSIdentifier _param1Annot "a")) _comma (JSAssignExpression (JSIdentifier _param2Annot "b") (JSAssign _assignAnnot) (JSDecimal _numAnnot 1))) _rightParen) _arrow (JSConciseExpressionBody (JSExpressionBinary (JSIdentifier _id1Annot "a") (JSBinOpPlus _plusAnnot) (JSIdentifier _id2Annot "b")))) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with default param, got: " ++ show result)
    case testExpr "([a,b]) => a + b" of
      Right (JSAstExpression (JSArrowExpression (JSParenthesizedArrowParameterList _leftParen (JSLOne (JSArrayLiteral _leftBracket [JSArrayElement (JSIdentifier _elem1Annot "a"), JSArrayComma _comma, JSArrayElement (JSIdentifier _elem2Annot "b")] _rightBracket)) _rightParen) _arrow (JSConciseExpressionBody (JSExpressionBinary (JSIdentifier _id1Annot "a") (JSBinOpPlus _plusAnnot) (JSIdentifier _id2Annot "b")))) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected arrow expression with destructuring param, got: " ++ show result)

  it "trailing _comma in function parameters" $ do
    -- Test trailing commas in function expressions
    case testExpr "function(a,){}" of
      Right (JSAstExpression (JSFunctionExpression _funcAnnot JSIdentNone _leftParen (JSLOne (JSIdentifier _paramAnnot "a")) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with trailing comma, got: " ++ show result)
    case testExpr "function(a,b,){}" of
      Right (JSAstExpression (JSFunctionExpression _funcAnnot JSIdentNone _leftParen (JSLCons (JSLOne (JSIdentifier _param1Annot "a")) _comma (JSIdentifier _param2Annot "b")) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected function expression with trailing comma, got: " ++ show result)
    -- Test named functions with trailing commas
    case testExpr "function foo(x,){}" of
      Right (JSAstExpression (JSFunctionExpression _funcAnnot (JSIdentName _nameAnnot "foo") _leftParen (JSLOne (JSIdentifier _paramAnnot "x")) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected named function expression with trailing comma, got: " ++ show result)
    -- Test generator functions with trailing commas
    case testExpr "function*(a,){}" of
      Right (JSAstExpression (JSGeneratorExpression _genAnnot _starAnnot JSIdentNone _leftParen (JSLOne (JSIdentifier _paramAnnot "a")) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected generator expression with trailing comma, got: " ++ show result)
    case testExpr "function* gen(x,y,){}" of
      Right (JSAstExpression (JSGeneratorExpression _genAnnot _starAnnot (JSIdentName _nameAnnot "gen") _leftParen (JSLCons (JSLOne (JSIdentifier _param1Annot "x")) _comma (JSIdentifier _param2Annot "y")) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected named generator expression with trailing comma, got: " ++ show result)

  it "generator expression" $ do
    case testExpr "function*(){}" of
      Right (JSAstExpression (JSGeneratorExpression _genAnnot _starAnnot JSIdentNone _leftParen JSLNil _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected generator expression with no params, got: " ++ show result)
    case testExpr "function*(a){}" of
      Right (JSAstExpression (JSGeneratorExpression _genAnnot _starAnnot JSIdentNone _leftParen (JSLOne (JSIdentifier _paramAnnot "a")) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected generator expression with one param, got: " ++ show result)
    case testExpr "function*(a,b){}" of
      Right (JSAstExpression (JSGeneratorExpression _genAnnot _starAnnot JSIdentNone _leftParen (JSLCons (JSLOne (JSIdentifier _param1Annot "a")) _comma (JSIdentifier _param2Annot "b")) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected generator expression with two params, got: " ++ show result)
    case testExpr "function*(a,...b){}" of
      Right (JSAstExpression (JSGeneratorExpression _genAnnot _starAnnot JSIdentNone _leftParen (JSLCons (JSLOne (JSIdentifier _param1Annot "a")) _comma (JSSpreadExpression _spreadAnnot (JSIdentifier _param2Annot "b"))) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected generator expression with rest param, got: " ++ show result)
    case testExpr "function*f(){}" of
      Right (JSAstExpression (JSGeneratorExpression _genAnnot _starAnnot (JSIdentName _nameAnnot "f") _leftParen JSLNil _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected named generator expression with no params, got: " ++ show result)
    case testExpr "function*f(a){}" of
      Right (JSAstExpression (JSGeneratorExpression _genAnnot _starAnnot (JSIdentName _nameAnnot "f") _leftParen (JSLOne (JSIdentifier _paramAnnot "a")) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected named generator expression with one param, got: " ++ show result)
    case testExpr "function*f(a,b){}" of
      Right (JSAstExpression (JSGeneratorExpression _genAnnot _starAnnot (JSIdentName _nameAnnot "f") _leftParen (JSLCons (JSLOne (JSIdentifier _param1Annot "a")) _comma (JSIdentifier _param2Annot "b")) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected named generator expression with two params, got: " ++ show result)
    case testExpr "function*f(a,...b){}" of
      Right (JSAstExpression (JSGeneratorExpression _genAnnot _starAnnot (JSIdentName _nameAnnot "f") _leftParen (JSLCons (JSLOne (JSIdentifier _param1Annot "a")) _comma (JSSpreadExpression _spreadAnnot (JSIdentifier _param2Annot "b"))) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected named generator expression with rest param, got: " ++ show result)

  it "await expression" $ do
    case testExpr "await fetch('/api')" of
      Right (JSAstExpression (JSAwaitExpression _awaitAnnot (JSMemberExpression (JSIdentifier _idAnnot "fetch") _leftParen (JSLOne (JSStringLiteral _strAnnot "'/api'")) _rightParen)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected await expression with function call, got: " ++ show result)
    case testExpr "await Promise.resolve(42)" of
      Right (JSAstExpression (JSAwaitExpression _awaitAnnot (JSMemberExpression (JSMemberDot (JSIdentifier _idAnnot "Promise") _dot (JSIdentifier _memAnnot "resolve")) _leftParen (JSLOne (JSDecimal _numAnnot 42)) _rightParen)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected await expression with method call, got: " ++ show result)
    case testExpr "await (x + y)" of
      Right (JSAstExpression (JSAwaitExpression _awaitAnnot (JSExpressionParen _leftParen (JSExpressionBinary (JSIdentifier _id1Annot "x") (JSBinOpPlus _plusAnnot) (JSIdentifier _id2Annot "y")) _rightParen)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected await expression with parenthesized expression, got: " ++ show result)
    case testExpr "await x.then(y => y * 2)" of
      Right (JSAstExpression (JSAwaitExpression _awaitAnnot (JSMemberExpression (JSMemberDot (JSIdentifier _idAnnot "x") _dot (JSIdentifier _memAnnot "then")) _leftParen (JSLOne (JSArrowExpression (JSUnparenthesizedArrowParameter (JSIdentName _paramAnnot "y")) _arrow (JSConciseExpressionBody (JSExpressionBinary (JSIdentifier _id1Annot "y") (JSBinOpTimes _timesAnnot) (JSDecimal _numAnnot 2))))) _rightParen)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected await expression with method and arrow function, got: " ++ show result)
    case testExpr "await response.json()" of
      Right (JSAstExpression (JSAwaitExpression _awaitAnnot (JSMemberExpression (JSMemberDot (JSIdentifier _idAnnot "response") _dot (JSIdentifier _memAnnot "json")) _leftParen JSLNil _rightParen)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected await expression with method call, got: " ++ show result)
    case testExpr "await new Promise(resolve => resolve(1))" of
      Right (JSAstExpression (JSAwaitExpression _awaitAnnot (JSMemberNew _newAnnot (JSIdentifier _idAnnot "Promise") _leftParen (JSLOne (JSArrowExpression (JSUnparenthesizedArrowParameter (JSIdentName _paramAnnot "resolve")) _arrow (JSConciseExpressionBody (JSMemberExpression (JSIdentifier _callAnnot "resolve") _leftParen2 (JSLOne (JSDecimal _numAnnot 1)) _rightParen2)))) _rightParen)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected await expression with constructor and arrow function, got: " ++ show result)

  it "async function expression" $ do
    case testExpr "async function foo() {}" of
      Right (JSAstExpression (JSAsyncFunctionExpression _asyncAnnot _funcAnnot (JSIdentName _nameAnnot "foo") _leftParen JSLNil _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected named async function expression with no params, got: " ++ show result)
    case testExpr "async function foo(a) {}" of
      Right (JSAstExpression (JSAsyncFunctionExpression _asyncAnnot _funcAnnot (JSIdentName _nameAnnot "foo") _leftParen (JSLOne (JSIdentifier _paramAnnot "a")) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected named async function expression with one param, got: " ++ show result)
    case testExpr "async function foo(a, b) {}" of
      Right (JSAstExpression (JSAsyncFunctionExpression _asyncAnnot _funcAnnot (JSIdentName _nameAnnot "foo") _leftParen (JSLCons (JSLOne (JSIdentifier _param1Annot "a")) _comma (JSIdentifier _param2Annot "b")) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected named async function expression with two params, got: " ++ show result)
    case testExpr "async function() {}" of
      Right (JSAstExpression (JSAsyncFunctionExpression _asyncAnnot _funcAnnot JSIdentNone _leftParen JSLNil _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected anonymous async function expression with no params, got: " ++ show result)
    case testExpr "async function(x) { return await x; }" of
      Right (JSAstExpression (JSAsyncFunctionExpression _asyncAnnot _funcAnnot JSIdentNone _leftParen (JSLOne (JSIdentifier _paramAnnot "x")) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected anonymous async function expression with return await, got: " ++ show result)
    case testExpr "async function fetch() { return await response.json(); }" of
      Right (JSAstExpression (JSAsyncFunctionExpression _asyncAnnot _funcAnnot (JSIdentName _nameAnnot "fetch") _leftParen JSLNil _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected named async function expression with await method call, got: " ++ show result)
    case testExpr "async function handler(req, res) { const data = await db.query(); res.send(data); }" of
      Right (JSAstExpression (JSAsyncFunctionExpression _asyncAnnot _funcAnnot (JSIdentName _nameAnnot "handler") _leftParen (JSLCons (JSLOne (JSIdentifier _param1Annot "req")) _comma (JSIdentifier _param2Annot "res")) _rightParen _body) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected named async function expression with complex body, got: " ++ show result)

  it "member expression" $ do
    case testExpr "x[y]" of
      Right (JSAstExpression (JSMemberSquare (JSIdentifier _idAnnot "x") _leftBracket (JSIdentifier _indexAnnot "y") _rightBracket) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected member square expression, got: " ++ show result)
    case testExpr "x[y][z]" of
      Right (JSAstExpression (JSMemberSquare (JSMemberSquare (JSIdentifier _idAnnot "x") _leftBracket1 (JSIdentifier _index1Annot "y") _rightBracket1) _leftBracket2 (JSIdentifier _index2Annot "z") _rightBracket2) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected nested member square expression, got: " ++ show result)
    case testExpr "x.y" of
      Right (JSAstExpression (JSMemberDot (JSIdentifier _idAnnot "x") _dot (JSIdentifier _memAnnot "y")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected member dot expression, got: " ++ show result)
    case testExpr "x.y.z" of
      Right (JSAstExpression (JSMemberDot (JSMemberDot (JSIdentifier _idAnnot "x") _dot1 (JSIdentifier _mem1Annot "y")) _dot2 (JSIdentifier _mem2Annot "z")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected nested member dot expression, got: " ++ show result)

  it "call expression" $ do
    case testExpr "x()" of
      Right (JSAstExpression (JSMemberExpression (JSIdentifier _idAnnot "x") _leftParen JSLNil _rightParen) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected member expression call, got: " ++ show result)
    case testExpr "x()()" of
      Right (JSAstExpression (JSCallExpression (JSMemberExpression (JSIdentifier _idAnnot "x") _leftParen1 JSLNil _rightParen1) _leftParen2 JSLNil _rightParen2) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected call expression, got: " ++ show result)
    case testExpr "x()[4]" of
      Right (JSAstExpression (JSCallExpressionSquare (JSMemberExpression (JSIdentifier _idAnnot "x") _leftParen JSLNil _rightParen) _leftBracket (JSDecimal _numAnnot 4) _rightBracket) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected call expression with square access, got: " ++ show result)
    case testExpr "x().x" of
      Right (JSAstExpression (JSCallExpressionDot (JSMemberExpression (JSIdentifier _idAnnot "x") _leftParen JSLNil _rightParen) _dot (JSIdentifier _memAnnot "x")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected call expression with dot access, got: " ++ show result)
    case testExpr "x(a,b=2).x" of
      Right (JSAstExpression (JSCallExpressionDot (JSMemberExpression (JSIdentifier _idAnnot "x") _leftParen (JSLCons (JSLOne (JSIdentifier _arg1Annot "a")) _comma (JSAssignExpression (JSIdentifier _arg2Annot "b") (JSAssign _assignAnnot) (JSDecimal _numAnnot 2))) _rightParen) _dot (JSIdentifier _memAnnot "x")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected call expression with args and dot access, got: " ++ show result)
    case testExpr "foo (56.8379100, 60.5806664)" of
      Right (JSAstExpression (JSMemberExpression (JSIdentifier _idAnnot "foo") _leftParen (JSLCons (JSLOne (JSDecimal _num1Annot 56.8379100)) _comma (JSDecimal _num2Annot 60.5806664)) _rightParen) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected member expression with decimal args, got: " ++ show result)

  it "trailing _comma in function calls" $ do
    case testExpr "f(x,)" of
      Right (JSAstExpression (JSMemberExpression (JSIdentifier _idAnnot "f") _leftParen (JSLOne (JSIdentifier _argAnnot "x")) _rightParen) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected member expression with trailing comma, got: " ++ show result)
    case testExpr "f(a,b,)" of
      Right (JSAstExpression (JSMemberExpression (JSIdentifier _idAnnot "f") _leftParen (JSLCons (JSLOne (JSIdentifier _arg1Annot "a")) _comma (JSIdentifier _arg2Annot "b")) _rightParen) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected member expression with multiple args and trailing comma, got: " ++ show result)
    case testExpr "Math.max(10, 20,)" of
      Right (JSAstExpression (JSMemberExpression (JSMemberDot (JSIdentifier _idAnnot "Math") _dot (JSIdentifier _memAnnot "max")) _leftParen (JSLCons (JSLOne (JSDecimal _num1Annot 10)) _comma (JSDecimal _num2Annot 20)) _rightParen) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected method call with trailing comma, got: " ++ show result)
    -- Chained function calls with trailing commas
    case testExpr "f(x,)(y,)" of
      Right (JSAstExpression (JSCallExpression (JSMemberExpression (JSIdentifier _idAnnot "f") _leftParen1 (JSLOne (JSIdentifier _arg1Annot "x")) _rightParen1) _leftParen2 (JSLOne (JSIdentifier _arg2Annot "y")) _rightParen2) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected chained call expression with trailing commas, got: " ++ show result)
    -- Complex expressions with trailing commas
    case testExpr "obj.method(a + b, c * d,)" of
      Right (JSAstExpression (JSMemberExpression (JSMemberDot (JSIdentifier _idAnnot "obj") _dot (JSIdentifier _memAnnot "method")) _leftParen (JSLCons (JSLOne (JSExpressionBinary (JSIdentifier _id1Annot "a") (JSBinOpPlus _plus1Annot) (JSIdentifier _id2Annot "b"))) _comma (JSExpressionBinary (JSIdentifier _id3Annot "c") (JSBinOpTimes _timesAnnot) (JSIdentifier _id4Annot "d"))) _rightParen) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected method call with binary expressions and trailing comma, got: " ++ show result)
    -- Single argument with trailing _comma
    case testExpr "console.log('hello',)" of
      Right (JSAstExpression (JSMemberExpression (JSMemberDot (JSIdentifier _idAnnot "console") _dot (JSIdentifier _memAnnot "log")) _leftParen (JSLOne (JSStringLiteral _strAnnot "'hello'")) _rightParen) _astAnnot) -> pure ()
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
      Right (JSAstExpression (JSTemplateLiteral Nothing _backquote "``" []) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected empty template literal, got: " ++ show result)
    case testExpr "`$`" of
      Right (JSAstExpression (JSTemplateLiteral Nothing _backquote "`$`" []) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected template literal with dollar sign, got: " ++ show result)
    case testExpr "`$\\n`" of
      Right (JSAstExpression (JSTemplateLiteral Nothing _backquote "`$\\n`" []) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected template literal with escape sequence, got: " ++ show result)
    case testExpr "`\\${x}`" of
      Right (JSAstExpression (JSTemplateLiteral Nothing _backquote "`\\${x}`" []) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected template literal with escaped interpolation, got: " ++ show result)
    case testExpr "`$ {x}`" of
      Right (JSAstExpression (JSTemplateLiteral Nothing _backquote "`$ {x}`" []) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected template literal with space before brace, got: " ++ show result)
    case testExpr "`\n\n`" of
      Right (JSAstExpression (JSTemplateLiteral Nothing _backquote "`\n\n`" []) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected template literal with newlines, got: " ++ show result)
    case testExpr "`${x+y} ${z}`" of
      Right (JSAstExpression (JSTemplateLiteral Nothing _backquote "`${" [JSTemplatePart (JSExpressionBinary (JSIdentifier _id1Annot "x") (JSBinOpPlus _plusAnnot) (JSIdentifier _id2Annot "y")) _rightBrace "} ${", JSTemplatePart (JSIdentifier _idAnnot "z") _rightBrace2 "}`"]) _astAnnot) -> pure ()
      Right other -> expectationFailure ("Expected template literal with interpolations, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for template literal, got: " ++ show result)
    case testExpr "`<${x} ${y}>`" of
      Right (JSAstExpression (JSTemplateLiteral Nothing _backquote "`<${" [JSTemplatePart (JSIdentifier _id1Annot "x") _rightBrace1 "} ${", JSTemplatePart (JSIdentifier _id2Annot "y") _rightBrace2 "}>`"]) _astAnnot) -> pure ()
      Right other -> expectationFailure ("Expected template literal with HTML-like interpolations, got: " ++ show other)
      result -> expectationFailure ("Expected successful parse for template literal, got: " ++ show result)
    case testExpr "tag `xyz`" of
      Right (JSAstExpression (JSTemplateLiteral (Just (JSIdentifier _tagAnnot "tag")) _backquote "`xyz`" []) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected tagged template literal, got: " ++ show result)
    case testExpr "tag()`xyz`" of
      Right (JSAstExpression (JSTemplateLiteral (Just (JSMemberExpression (JSIdentifier _tagAnnot "tag") _leftParen JSLNil _rightParen)) _backquote "`xyz`" []) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected template literal with function call tag, got: " ++ show result)

  it "yield" $ do
    case testExpr "yield" of
      Right (JSAstExpression (JSYieldExpression _yieldAnnot Nothing) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected yield expression without value, got: " ++ show result)
    case testExpr "yield a + b" of
      Right (JSAstExpression (JSYieldExpression _yieldAnnot (Just (JSExpressionBinary (JSIdentifier _id1Annot "a") (JSBinOpPlus _plusAnnot) (JSIdentifier _id2Annot "b")))) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected yield expression with binary operation, got: " ++ show result)
    case testExpr "yield* g()" of
      Right (JSAstExpression (JSYieldFromExpression _yieldAnnot _starAnnot (JSMemberExpression (JSIdentifier _idAnnot "g") _leftParen JSLNil _rightParen)) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected yield from expression with function call, got: " ++ show result)

  it "class expression" $ do
    case testExpr "class Foo extends Bar { a(x,y) {} *b() {} }" of
      Right (JSAstExpression (JSClassExpression _classAnnot (JSIdentName _nameAnnot "Foo") (JSExtends _extendsAnnot (JSIdentifier _heritageAnnot "Bar")) _leftBrace _body _rightBrace) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected class expression with inheritance and methods, got: " ++ show result)
    case testExpr "class { static get [a]() {}; }" of
      Right (JSAstExpression (JSClassExpression _classAnnot JSIdentNone JSExtendsNone _leftBrace _body _rightBrace) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected anonymous class expression with static getter, got: " ++ show result)
    case testExpr "class Foo extends Bar { a(x,y) { super(x); } }" of
      Right (JSAstExpression (JSClassExpression _classAnnot (JSIdentName _nameAnnot "Foo") (JSExtends _extendsAnnot (JSIdentifier _heritageAnnot "Bar")) _leftBrace _body _rightBrace) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected class expression with super call, got: " ++ show result)

  it "optional chaining" $ do
    case testExpr "obj?.prop" of
      Right (JSAstExpression (JSOptionalMemberDot (JSIdentifier _idAnnot "obj") _optionalDot (JSIdentifier _memAnnot "prop")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected optional member dot access, got: " ++ show result)
    case testExpr "obj?.[key]" of
      Right (JSAstExpression (JSOptionalMemberSquare (JSIdentifier _idAnnot "obj") _optionalBracket (JSIdentifier _keyAnnot "key") _rightBracket) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected optional member square access, got: " ++ show result)
    case testExpr "obj?.method()" of
      Right (JSAstExpression (JSMemberExpression (JSOptionalMemberDot (JSIdentifier _idAnnot "obj") _optionalDot (JSIdentifier _memAnnot "method")) _leftParen JSLNil _rightParen) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected member expression with optional method call, got: " ++ show result)
    case testExpr "obj?.prop?.deep" of
      Right (JSAstExpression (JSOptionalMemberDot (JSOptionalMemberDot (JSIdentifier _idAnnot "obj") _optionalDot1 (JSIdentifier _mem1Annot "prop")) _optionalDot2 (JSIdentifier _mem2Annot "deep")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected chained optional member dot access, got: " ++ show result)
    case testExpr "obj?.method?.(args)" of
      Right (JSAstExpression (JSOptionalCallExpression (JSOptionalMemberDot (JSIdentifier _idAnnot "obj") _optionalDot (JSIdentifier _memAnnot "method")) _optionalParen (JSLOne (JSIdentifier _argAnnot "args")) _rightParen) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected optional call expression, got: " ++ show result)
    case testExpr "arr?.[0]?.value" of
      Right (JSAstExpression (JSOptionalMemberDot (JSOptionalMemberSquare (JSIdentifier _idAnnot "arr") _optionalBracket (JSDecimal _numAnnot 0) _rightBracket) _optionalDot (JSIdentifier _memAnnot "value")) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected chained optional access with square and dot, got: " ++ show result)

  it "nullish coalescing precedence" $ do
    case testExpr "x ?? y || z" of
      Right
        ( JSAstExpression
            ( JSExpressionBinary
                (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpNullishCoalescing _opAnnot1) (JSIdentifier _rightIdAnnot "y"))
                (JSBinOpOr _opAnnot2)
                (JSIdentifier _idAnnot "z")
              )
            _astAnnot
          ) -> pure ()
      result -> expectationFailure ("Expected nullish coalescing with lower precedence than OR, got: " ++ show result)
    case testExpr "x || y ?? z" of
      Right
        ( JSAstExpression
            ( JSExpressionBinary
                (JSIdentifier _idAnnot "x")
                (JSBinOpOr _opAnnot1)
                (JSExpressionBinary (JSIdentifier _leftIdAnnot "y") (JSBinOpNullishCoalescing _opAnnot2) (JSIdentifier _rightIdAnnot "z"))
              )
            _astAnnot
          ) -> pure ()
      result -> expectationFailure ("Expected OR with higher precedence than nullish coalescing, got: " ++ show result)
    case testExpr "null ?? 'default'" of
      Right
        ( JSAstExpression
            ( JSExpressionBinary
                (JSLiteral _litAnnot "null")
                (JSBinOpNullishCoalescing _opAnnot)
                (JSStringLiteral _strAnnot "'default'")
              )
            _astAnnot
          ) -> pure ()
      result -> expectationFailure ("Expected nullish coalescing with null and string, got: " ++ show result)
    case testExpr "undefined ?? 0" of
      Right
        ( JSAstExpression
            ( JSExpressionBinary
                (JSIdentifier _idAnnot "undefined")
                (JSBinOpNullishCoalescing _opAnnot)
                (JSDecimal _numAnnot 0)
              )
            _astAnnot
          ) -> pure ()
      result -> expectationFailure ("Expected nullish coalescing with undefined and number, got: " ++ show result)
    case testExpr "x ?? y ?? z" of
      Right
        ( JSAstExpression
            ( JSExpressionBinary
                (JSExpressionBinary (JSIdentifier _leftIdAnnot "x") (JSBinOpNullishCoalescing _opAnnot1) (JSIdentifier _rightIdAnnot "y"))
                (JSBinOpNullishCoalescing _opAnnot2)
                (JSIdentifier _idAnnot "z")
              )
            _astAnnot
          ) -> pure ()
      result -> expectationFailure ("Expected left-associative nullish coalescing, got: " ++ show result)

  it "static class expressions (ES2015) - supported features" $ do
    -- Basic static method in class expression
    case testExpr "class { static method() {} }" of
      Right (JSAstExpression (JSClassExpression _classAnnot JSIdentNone JSExtendsNone _leftBrace _body _rightBrace) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected anonymous class expression with static method, got: " ++ show result)
    -- Named class expression with static methods
    case testExpr "class Calculator { static add(a, b) { return a + b; } }" of
      Right (JSAstExpression (JSClassExpression _classAnnot (JSIdentName _nameAnnot "Calculator") JSExtendsNone _leftBrace _body _rightBrace) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected named class expression with static method, got: " ++ show result)
    -- Static getter in class expression
    case testExpr "class { static get version() { return '2.0'; } }" of
      Right (JSAstExpression (JSClassExpression _classAnnot JSIdentNone JSExtendsNone _leftBrace _body _rightBrace) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected anonymous class expression with static getter, got: " ++ show result)
    -- Static setter in class expression
    case testExpr "class { static set config(val) { this._config = val; } }" of
      Right (JSAstExpression (JSClassExpression _classAnnot JSIdentNone JSExtendsNone _leftBrace _body _rightBrace) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected anonymous class expression with static setter, got: " ++ show result)
    -- Static computed property
    case testExpr "class { static [Symbol.iterator]() {} }" of
      Right (JSAstExpression (JSClassExpression _classAnnot JSIdentNone JSExtendsNone _leftBrace _body _rightBrace) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected anonymous class expression with static computed property, got: " ++ show result)
    -- Multiple static features
    case testExpr "class Util { static method() {} static get prop() {} }" of
      Right (JSAstExpression (JSClassExpression _classAnnot (JSIdentName _nameAnnot "Util") JSExtendsNone _leftBrace _body _rightBrace) _astAnnot) -> pure ()
      result -> expectationFailure ("Expected named class expression with multiple static features, got: " ++ show result)

testExpr :: String -> Either String JSAST
testExpr str = parseUsing parseExpression str "src"
