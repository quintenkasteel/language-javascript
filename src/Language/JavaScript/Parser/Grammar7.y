{
{-# LANGUAGE BangPatterns #-}
module Language.JavaScript.Parser.Grammar7
    ( parseProgram
    , parseModule
    , parseStatement
    , parseExpression
    , parseLiteral
    ) where

import Data.Char
import Data.Functor (($>))
import Language.JavaScript.Parser.Lexer
import Language.JavaScript.Parser.ParserMonad
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token
import qualified Language.JavaScript.Parser.AST as AST

}

-- The name of the generated function to be exported from the module
%name parseProgram           Program
%name parseModule            Module
%name parseLiteral           LiteralMain
%name parseExpression        ExpressionMain
%name parseStatement         StatementMain

%tokentype { Token }
%error { parseError }
%monad { Alex } { >>= } { return }
%lexer { lexCont } { EOFToken {} }


%token

     ';'    { SemiColonToken {} }
     ','    { CommaToken {} }
     '?'    { HookToken {} }
     ':'    { ColonToken {} }
     '||'   { OrToken {} }
     '&&'   { AndToken {} }
     '??'   { NullishCoalescingToken {} }
     '?.'   { OptionalChainingToken {} }
     '|'    { BitwiseOrToken {} }
     '^'    { BitwiseXorToken {} }
     '&'    { BitwiseAndToken {} }
     '=>'   { ArrowToken {} }
     '==='  { StrictEqToken {} }
     '=='   { EqToken {} }
     '*='   { TimesAssignToken {} }
     '/='   { DivideAssignToken {} }
     '%='   { ModAssignToken {} }
     '+='   { PlusAssignToken {} }
     '-='   { MinusAssignToken {} }
     '<<='  { LshAssignToken {} }
     '>>='  { RshAssignToken {} }
     '>>>=' { UrshAssignToken {} }
     '&='   { AndAssignToken {} }
     '^='   { XorAssignToken {} }
     '|='   { OrAssignToken {} }
     '&&='  { LogicalAndAssignToken {} }
     '||='  { LogicalOrAssignToken {} }
     '??='  { NullishAssignToken {} }
     '='    { SimpleAssignToken {} }
     '!=='  { StrictNeToken {} }
     '!='   { NeToken {} }
     '<<'   { LshToken {} }
     '<='   { LeToken {} }
     '<'    { LtToken {} }
     '>>>'  { UrshToken {} }
     '>>'   { RshToken {} }
     '>='   { GeToken {} }
     '>'    { GtToken {} }
     '++'   { IncrementToken {} }
     '--'   { DecrementToken {} }
     '+'    { PlusToken {} }
     '-'    { MinusToken {} }
     '**'   { ExponentiationToken {} }
     '*'    { MulToken {} }
     '/'    { DivToken {} }
     '%'    { ModToken {} }
     '!'    { NotToken {} }
     '~'    { BitwiseNotToken {} }
     '...'  { SpreadToken {} }
     '.'    { DotToken {} }
     '['    { LeftBracketToken {} }
     ']'    { RightBracketToken {} }
     '{'    { LeftCurlyToken {} }
     '}'    { RightCurlyToken {} }
     '('    { LeftParenToken {} }
     ')'    { RightParenToken {} }

     'as'         { AsToken {} }
     'autosemi'   { AutoSemiToken {} }
     'async'      { AsyncToken {} }
     'await'      { AwaitToken {} }
     'break'      { BreakToken {} }
     'case'       { CaseToken {} }
     'catch'      { CatchToken {} }
     'class'      { ClassToken {} }
     'const'      { ConstToken {} }
     'continue'   { ContinueToken {} }
     'debugger'   { DebuggerToken {} }
     'default'    { DefaultToken {} }
     'delete'     { DeleteToken {} }
     'do'         { DoToken {} }
     'else'       { ElseToken {} }
     'enum'       { EnumToken {} }
     'export'     { ExportToken {} }
     'extends'    { ExtendsToken {} }
     'false'      { FalseToken {} }
     'finally'    { FinallyToken {} }
     'for'        { ForToken {} }
     'function'   { FunctionToken {} }
     'from'       { FromToken {} }
     'get'        { GetToken {} }
     'if'         { IfToken {} }
     'import'     { ImportToken {} }
     'in'         { InToken {} }
     'instanceof' { InstanceofToken {} }
     'let'        { LetToken {} }
     'new'        { NewToken {} }
     'null'       { NullToken {} }
     'of'         { OfToken {} }
     'return'     { ReturnToken {} }
     'set'        { SetToken {} }
     'static'     { StaticToken {} }
     'super'      { SuperToken {} }
     'switch'     { SwitchToken {} }
     'this'       { ThisToken {} }
     'throw'      { ThrowToken {} }
     'true'       { TrueToken {} }
     'try'        { TryToken {} }
     'typeof'     { TypeofToken {} }
     'var'        { VarToken {} }
     'void'       { VoidToken {} }
     'while'      { WhileToken {} }
     'with'       { WithToken {} }
     'yield'      { YieldToken {} }


     'ident'      { IdentifierToken {} }
     'private'    { PrivateNameToken {} }
     'decimal'    { DecimalToken {} }
     'hexinteger' { HexIntegerToken {} }
     'binaryinteger' { BinaryIntegerToken {} }
     'octal'      { OctalToken {} }
     'bigint'     { BigIntToken {} }
     'string'     { StringToken {} }
     'regex'      { RegExToken {} }
     'tmplnosub'  { NoSubstitutionTemplateToken {} }
     'tmplhead'   { TemplateHeadToken {} }
     'tmplmiddle' { TemplateMiddleToken {} }
     'tmpltail'   { TemplateTailToken {} }

     'future'     { FutureToken {} }

     'tail'       { TailToken {} }


%%

-- ---------------------------------------------------------------------
-- Sort out automatically inserted semi-colons.
-- A MaybeSemi is an actual semi-colon or nothing.
-- An AutoSemu is either an actual semi-colon or 'virtual' semi-colon inserted
-- by the Alex lexer or nothing.

MaybeSemi :: { AST.JSSemi }
MaybeSemi : ';' { AST.JSSemi (mkJSAnnot $1) }
         |      { AST.JSSemiAuto }

AutoSemi :: { AST.JSSemi }
AutoSemi : ';'         { AST.JSSemi (mkJSAnnot $1) }
         | 'autosemi'  { AST.JSSemiAuto }
         |             { AST.JSSemiAuto }

-- ---------------------------------------------------------------------

-- Helpers

LParen :: { AST.JSAnnot }
LParen : '(' { mkJSAnnot $1 }

RParen :: { AST.JSAnnot }
RParen : ')' { mkJSAnnot $1 }

LBrace :: { AST.JSAnnot }
LBrace : '{' { mkJSAnnot $1 }

RBrace :: { AST.JSAnnot }
RBrace : '}' { mkJSAnnot $1 }

LSquare :: { AST.JSAnnot }
LSquare : '[' { mkJSAnnot $1 }

RSquare :: { AST.JSAnnot }
RSquare : ']' { mkJSAnnot $1 }

Comma :: { AST.JSAnnot }
Comma : ',' { mkJSAnnot $1 }

Colon :: { AST.JSAnnot }
Colon : ':' { mkJSAnnot $1 }

Semi :: { AST.JSAnnot }
Semi : ';' { mkJSAnnot $1 }

Arrow :: { AST.JSAnnot }
Arrow : '=>' { mkJSAnnot $1 }

Spread :: { AST.JSAnnot }
Spread : '...' { mkJSAnnot $1 }

Dot :: { AST.JSAnnot }
Dot : '.' { mkJSAnnot $1 }

OptionalChaining :: { AST.JSAnnot }
OptionalChaining : '?.' { mkJSAnnot $1 }


As :: { AST.JSAnnot }
As : 'as' { mkJSAnnot $1 }

Increment :: { AST.JSUnaryOp }
Increment : '++' { AST.JSUnaryOpIncr (mkJSAnnot $1) }

Decrement :: { AST.JSUnaryOp }
Decrement : '--' { AST.JSUnaryOpDecr (mkJSAnnot $1) }

Delete :: { AST.JSUnaryOp }
Delete : 'delete' { AST.JSUnaryOpDelete (mkJSAnnot $1) }

Void :: { AST.JSUnaryOp }
Void : 'void' { AST.JSUnaryOpVoid (mkJSAnnot $1) }

Typeof :: { AST.JSUnaryOp }
Typeof : 'typeof' { AST.JSUnaryOpTypeof (mkJSAnnot $1) }

Plus :: { AST.JSBinOp }
Plus : '+' { AST.JSBinOpPlus (mkJSAnnot $1) }

Minus :: { AST.JSBinOp }
Minus : '-' { AST.JSBinOpMinus (mkJSAnnot $1) }

Tilde :: { AST.JSUnaryOp }
Tilde : '~' { AST.JSUnaryOpTilde (mkJSAnnot $1) }

Not :: { AST.JSUnaryOp }
Not : '!' { AST.JSUnaryOpNot (mkJSAnnot $1) }

Mul :: { AST.JSBinOp }
Mul : '*' { AST.JSBinOpTimes (mkJSAnnot $1) }

Exp :: { AST.JSBinOp }
Exp : '**' { AST.JSBinOpExponentiation (mkJSAnnot $1) }

Div :: { AST.JSBinOp }
Div : '/' { AST.JSBinOpDivide (mkJSAnnot $1) }

Mod :: { AST.JSBinOp }
Mod : '%' { AST.JSBinOpMod (mkJSAnnot $1) }

Lsh :: { AST.JSBinOp }
Lsh : '<<' { AST.JSBinOpLsh (mkJSAnnot $1) }

Rsh :: { AST.JSBinOp }
Rsh : '>>' { AST.JSBinOpRsh (mkJSAnnot $1) }

Ursh :: { AST.JSBinOp }
Ursh : '>>>' { AST.JSBinOpUrsh (mkJSAnnot $1) }

Le :: { AST.JSBinOp }
Le : '<=' { AST.JSBinOpLe (mkJSAnnot $1) }

Lt :: { AST.JSBinOp }
Lt : '<' { AST.JSBinOpLt (mkJSAnnot $1) }

Ge :: { AST.JSBinOp }
Ge : '>=' { AST.JSBinOpGe (mkJSAnnot $1) }

Gt :: { AST.JSBinOp }
Gt : '>' { AST.JSBinOpGt (mkJSAnnot $1) }

In :: { AST.JSBinOp }
In : 'in' { AST.JSBinOpIn (mkJSAnnot $1) }

Instanceof :: { AST.JSBinOp }
Instanceof : 'instanceof' { AST.JSBinOpInstanceOf (mkJSAnnot $1) }

StrictEq :: { AST.JSBinOp }
StrictEq : '===' { AST.JSBinOpStrictEq (mkJSAnnot $1) }

Equal :: { AST.JSBinOp }
Equal : '==' { AST.JSBinOpEq (mkJSAnnot $1) }

StrictNe :: { AST.JSBinOp }
StrictNe : '!==' { AST.JSBinOpStrictNeq (mkJSAnnot $1) }

Ne :: { AST.JSBinOp }
Ne : '!=' { AST.JSBinOpNeq (mkJSAnnot $1)}

Of :: { AST.JSBinOp }
Of : 'of' { AST.JSBinOpOf (mkJSAnnot $1) }

Or :: { AST.JSBinOp }
Or : '||' { AST.JSBinOpOr (mkJSAnnot $1) }

And :: { AST.JSBinOp }
And : '&&' { AST.JSBinOpAnd (mkJSAnnot $1) }

NullishCoalescing :: { AST.JSBinOp }
NullishCoalescing : '??' { AST.JSBinOpNullishCoalescing (mkJSAnnot $1) }

BitOr :: { AST.JSBinOp }
BitOr : '|' { AST.JSBinOpBitOr (mkJSAnnot $1) }

BitAnd :: { AST.JSBinOp }
BitAnd : '&' { AST.JSBinOpBitAnd (mkJSAnnot $1) }

BitXor :: { AST.JSBinOp }
BitXor : '^' { AST.JSBinOpBitXor (mkJSAnnot $1)}

Hook :: { AST.JSAnnot }
Hook : '?' { mkJSAnnot $1 }

SimpleAssign :: { AST.JSAnnot }
SimpleAssign : '=' { mkJSAnnot $1 }

OpAssign :: { AST.JSAssignOp }
OpAssign : '*='     { AST.JSTimesAssign  (mkJSAnnot $1) }
         | '/='     { AST.JSDivideAssign (mkJSAnnot $1) }
         | '%='     { AST.JSModAssign    (mkJSAnnot $1) }
         | '+='     { AST.JSPlusAssign   (mkJSAnnot $1) }
         | '-='     { AST.JSMinusAssign  (mkJSAnnot $1) }
         | '<<='    { AST.JSLshAssign    (mkJSAnnot $1) }
         | '>>='    { AST.JSRshAssign    (mkJSAnnot $1) }
         | '>>>='   { AST.JSUrshAssign   (mkJSAnnot $1) }
         | '&='     { AST.JSBwAndAssign  (mkJSAnnot $1) }
         | '^='     { AST.JSBwXorAssign  (mkJSAnnot $1) }
         | '|='     { AST.JSBwOrAssign   (mkJSAnnot $1) }
         | '&&='    { AST.JSLogicalAndAssign (mkJSAnnot $1) }
         | '||='    { AST.JSLogicalOrAssign  (mkJSAnnot $1) }
         | '??='    { AST.JSNullishAssign    (mkJSAnnot $1) }

-- IdentifierName ::                                                        See 7.6
--         IdentifierStart
--         IdentifierName IdentifierPart
-- Note: This production needs to precede the productions for all keyword
-- statements and PrimaryExpression. Contra the Happy documentation, in the
-- case of a reduce/reduce conflict, the *later* rule takes precedence, and
-- the ambiguity of, for example, `{break}` needs to resolve in favor of
-- `break` as a keyword and not as an identifier in property shorthand
-- syntax.
-- TODO: make this include any reserved word too, including future ones
IdentifierName :: { AST.JSExpression }
IdentifierName : Identifier {$1}
             | 'async'      { AST.JSIdentifier (mkJSAnnot $1) ("async") }
             | 'await'      { AST.JSIdentifier (mkJSAnnot $1) ("await") }
             | 'break'      { AST.JSIdentifier (mkJSAnnot $1) ("break") }
             | 'case'       { AST.JSIdentifier (mkJSAnnot $1) ("case") }
             | 'catch'      { AST.JSIdentifier (mkJSAnnot $1) ("catch") }
             | 'class'      { AST.JSIdentifier (mkJSAnnot $1) ("class") }
             | 'const'      { AST.JSIdentifier (mkJSAnnot $1) ("const") }
             | 'continue'   { AST.JSIdentifier (mkJSAnnot $1) ("continue") }
             | 'debugger'   { AST.JSIdentifier (mkJSAnnot $1) ("debugger") }
             | 'default'    { AST.JSIdentifier (mkJSAnnot $1) ("default") }
             | 'delete'     { AST.JSIdentifier (mkJSAnnot $1) ("delete") }
             | 'do'         { AST.JSIdentifier (mkJSAnnot $1) ("do") }
             | 'else'       { AST.JSIdentifier (mkJSAnnot $1) ("else") }
             | 'enum'       { AST.JSIdentifier (mkJSAnnot $1) ("enum") }
             | 'export'     { AST.JSIdentifier (mkJSAnnot $1) ("export") }
             | 'extends'    { AST.JSIdentifier (mkJSAnnot $1) ("extends") }
             | 'false'      { AST.JSIdentifier (mkJSAnnot $1) ("false") }
             | 'finally'    { AST.JSIdentifier (mkJSAnnot $1) ("finally") }
             | 'for'        { AST.JSIdentifier (mkJSAnnot $1) ("for") }
             | 'function'   { AST.JSIdentifier (mkJSAnnot $1) ("function") }
             | 'if'         { AST.JSIdentifier (mkJSAnnot $1) ("if") }
             | 'in'         { AST.JSIdentifier (mkJSAnnot $1) ("in") }
             | 'instanceof' { AST.JSIdentifier (mkJSAnnot $1) ("instanceof") }
             | 'let'        { AST.JSIdentifier (mkJSAnnot $1) ("let") }
             | 'new'        { AST.JSIdentifier (mkJSAnnot $1) ("new") }
             | 'null'       { AST.JSIdentifier (mkJSAnnot $1) ("null") }
             | 'of'         { AST.JSIdentifier (mkJSAnnot $1) ("of") }
             | 'return'     { AST.JSIdentifier (mkJSAnnot $1) ("return") }
             | 'static'     { AST.JSIdentifier (mkJSAnnot $1) ("static") }
             | 'super'      { AST.JSIdentifier (mkJSAnnot $1) ("super") }
             | 'switch'     { AST.JSIdentifier (mkJSAnnot $1) ("switch") }
             | 'this'       { AST.JSIdentifier (mkJSAnnot $1) ("this") }
             | 'throw'      { AST.JSIdentifier (mkJSAnnot $1) ("throw") }
             | 'true'       { AST.JSIdentifier (mkJSAnnot $1) ("true") }
             | 'try'        { AST.JSIdentifier (mkJSAnnot $1) ("try") }
             | 'typeof'     { AST.JSIdentifier (mkJSAnnot $1) ("typeof") }
             | 'var'        { AST.JSIdentifier (mkJSAnnot $1) ("var") }
             | 'void'       { AST.JSIdentifier (mkJSAnnot $1) ("void") }
             | 'while'      { AST.JSIdentifier (mkJSAnnot $1) ("while") }
             | 'with'       { AST.JSIdentifier (mkJSAnnot $1) ("with") }
             | 'future'     { AST.JSIdentifier (mkJSAnnot $1) (tokenLiteral $1) }

Var :: { AST.JSAnnot }
Var : 'var' { mkJSAnnot $1 }

Let :: { AST.JSAnnot }
Let : 'let' { mkJSAnnot $1 }

Const :: { AST.JSAnnot }
Const : 'const' { mkJSAnnot $1 }

Import :: { AST.JSAnnot }
Import : 'import' { mkJSAnnot $1 }

From :: { AST.JSAnnot }
From : 'from' { mkJSAnnot $1 }

Export :: { AST.JSAnnot }
Export : 'export' { mkJSAnnot $1 }

If :: { AST.JSAnnot }
If : 'if' { mkJSAnnot $1 }

Else :: { AST.JSAnnot }
Else : 'else' { mkJSAnnot $1 }

Do :: { AST.JSAnnot }
Do : 'do' { mkJSAnnot $1 }

While :: { AST.JSAnnot }
While : 'while' { mkJSAnnot $1 }

For :: { AST.JSAnnot }
For : 'for' { mkJSAnnot $1 }

Continue :: { AST.JSAnnot }
Continue : 'continue' { mkJSAnnot $1 }

Async :: { AST.JSAnnot }
Async : 'async' { mkJSAnnot $1 }

Await :: { AST.JSAnnot }
Await : 'await' { mkJSAnnot $1 }

Break :: { AST.JSAnnot }
Break : 'break' { mkJSAnnot $1 }

Return :: { AST.JSAnnot }
Return : 'return' { mkJSAnnot $1 }

With :: { AST.JSAnnot }
With : 'with' { mkJSAnnot $1 }

Switch :: { AST.JSAnnot }
Switch : 'switch' { mkJSAnnot $1 }

Case :: { AST.JSAnnot }
Case : 'case' { mkJSAnnot $1 }

Default :: { AST.JSAnnot }
Default : 'default' { mkJSAnnot $1 }

Throw :: { AST.JSAnnot }
Throw : 'throw' { mkJSAnnot $1 {- 'Throw' -} }

Try :: { AST.JSAnnot }
Try : 'try' { mkJSAnnot $1 }

CatchL :: { AST.JSAnnot }
CatchL : 'catch' { mkJSAnnot $1 }

FinallyL :: { AST.JSAnnot }
FinallyL : 'finally' { mkJSAnnot $1 }

Function :: { AST.JSAnnot }
Function : 'function' { mkJSAnnot $1 {- 'Function' -} }

New :: { AST.JSAnnot }
New : 'new' { mkJSAnnot $1 }

Class :: { AST.JSAnnot }
Class : 'class' { mkJSAnnot $1 }

Extends :: { AST.JSAnnot }
Extends : 'extends' { mkJSAnnot $1 }

Static :: { AST.JSAnnot }
Static : 'static' { mkJSAnnot $1 }

Super :: { AST.JSExpression }
Super : 'super' { AST.JSLiteral (mkJSAnnot $1) ("super") }


Eof :: { AST.JSAnnot }
Eof : 'tail' { mkJSAnnot $1 {- 'Eof' -} }

-- Literal ::                                                                See 7.8
--         NullLiteral
--         BooleanLiteral
--         NumericLiteral
--         StringLiteral
Literal :: { AST.JSExpression }
Literal : NullLiteral     { $1 }
        | BooleanLiteral  { $1 }
        | NumericLiteral  { $1 }
        | StringLiteral   { $1 }
        | RegularExpressionLiteral { $1 }

NullLiteral :: { AST.JSExpression }
NullLiteral : 'null' { AST.JSLiteral (mkJSAnnot $1) ("null") }

BooleanLiteral :: { AST.JSExpression }
BooleanLiteral : 'true'  { AST.JSLiteral (mkJSAnnot $1) ("true") }
               | 'false' { AST.JSLiteral (mkJSAnnot $1) ("false") }

-- <Numeric Literal> ::= DecimalLiteral
--                     | HexIntegerLiteral
--                     | OctalLiteral
NumericLiteral :: { AST.JSExpression }
NumericLiteral : 'decimal'    { AST.JSDecimal (mkJSAnnot $1) (tokenLiteral $1) }
               | 'hexinteger' { AST.JSHexInteger (mkJSAnnot $1) (tokenLiteral $1) }
               | 'binaryinteger' { AST.JSBinaryInteger (mkJSAnnot $1) (tokenLiteral $1) }
               | 'octal'      { AST.JSOctal (mkJSAnnot $1) (tokenLiteral $1) }
               | 'bigint'     { AST.JSBigIntLiteral (mkJSAnnot $1) (tokenLiteral $1) }

StringLiteral :: { AST.JSExpression }
StringLiteral : 'string'  { AST.JSStringLiteral (mkJSAnnot $1) (tokenLiteral $1) }

-- <Regular Expression Literal> ::= RegExp
RegularExpressionLiteral :: { AST.JSExpression }
RegularExpressionLiteral : 'regex' { AST.JSRegEx (mkJSAnnot $1) (tokenLiteral $1) }

-- PrimaryExpression :                                                   See 11.1
--        this
--        Identifier
--        Literal
--        ArrayLiteral
--        ObjectLiteral
--        ( Expression )
PrimaryExpression :: { AST.JSExpression }
PrimaryExpression : 'this'                   { AST.JSLiteral (mkJSAnnot $1) ("this") }
                  | Identifier               { $1 {- 'PrimaryExpression1' -} }
                  | Literal                  { $1 {- 'PrimaryExpression2' -} }
                  | ArrayLiteral             { $1 {- 'PrimaryExpression3' -} }
                  | ObjectLiteral            { $1 {- 'PrimaryExpression4' -} }
                  | ClassExpression          { $1 }
                  | GeneratorExpression      { $1 }
                  | TemplateLiteral          { mkJSTemplateLiteral Nothing $1 {- 'PrimaryExpression6' -} }
                  | LParen Expression RParen { AST.JSExpressionParen $1 $2 $3 }
                  | ImportMeta               { $1 {- 'PrimaryExpression7' -} }

-- Identifier ::                                                            See 7.6
--         IdentifierName but not ReservedWord
Identifier :: { AST.JSExpression }
Identifier : 'ident' { AST.JSIdentifier (mkJSAnnot $1) (tokenLiteral $1) }
           | 'as'    { AST.JSIdentifier (mkJSAnnot $1) ("as") }
           | 'get'   { AST.JSIdentifier (mkJSAnnot $1) ("get") }
           | 'set'   { AST.JSIdentifier (mkJSAnnot $1) ("set") }
           | 'from'  { AST.JSIdentifier (mkJSAnnot $1) ("from") }
           | 'yield' { AST.JSIdentifier (mkJSAnnot $1) ("yield") }

-- Must follow Identifier; when ambiguous, `yield` as a keyword should take
-- precedence over `yield` as an identifier name.
Yield :: { AST.JSAnnot }
Yield : 'yield' { mkJSAnnot $1 }

ImportMeta :: { AST.JSExpression }
ImportMeta : 'import' '.' 'ident' {% if tokenLiteral $3 == ("meta")
                                     then return (AST.JSImportMeta (mkJSAnnot $1) (mkJSAnnot $2))
                                     else parseError $3 }

SpreadExpression :: { AST.JSExpression }
SpreadExpression : Spread AssignmentExpression  { AST.JSSpreadExpression $1 $2 {- 'SpreadExpression' -} }

TemplateLiteral :: { JSUntaggedTemplate }
TemplateLiteral : 'tmplnosub'              { JSUntaggedTemplate (mkJSAnnot $1) (tokenLiteral $1) [] }
                | 'tmplhead' TemplateParts { JSUntaggedTemplate (mkJSAnnot $1) (tokenLiteral $1) $2 }

TemplateParts :: { [AST.JSTemplatePart] }
TemplateParts : TemplateExpression RBrace 'tmplmiddle' TemplateParts { AST.JSTemplatePart $1 $2 ('}' : (tokenLiteral $3)) : $4 }
              | TemplateExpression RBrace 'tmpltail'                 { AST.JSTemplatePart $1 $2 ('}' : (tokenLiteral $3)) : [] }

-- This production only exists to ensure that inTemplate is set to True before
-- a tmplmiddle or tmpltail token is lexed. Since the lexer is always one token
-- ahead of the parser, setInTemplate needs to be called during a reduction
-- that is *two* tokens behind tmplmiddle/tmpltail. Accordingly,
-- TemplateExpression is always followed by an RBrace, which is lexed normally.
TemplateExpression :: { AST.JSExpression }
TemplateExpression : Expression {% setInTemplate True \$> $1 }

-- ArrayLiteral :                                                        See 11.1.4
--        [ Elisionopt ]
--        [ ElementList ]
--        [ ElementList , Elisionopt ]
ArrayLiteral :: { AST.JSExpression }
ArrayLiteral : LSquare RSquare                          { AST.JSArrayLiteral $1 [] $2           {- 'ArrayLiteral11' -} }
             | LSquare Elision RSquare                  { AST.JSArrayLiteral $1 $2 $3           {- 'ArrayLiteral12' -}  }
             | LSquare ElementList RSquare              { AST.JSArrayLiteral $1 $2 $3           {- 'ArrayLiteral13' -}  }
             | LSquare ElementList Elision RSquare      { AST.JSArrayLiteral $1 ($2 ++ $3) $4   {- 'ArrayLiteral14' -} }


-- ElementList :                                                         See 11.1.4
--        Elisionopt AssignmentExpression
--        ElementList , Elisionopt AssignmentExpression
ElementList :: { [AST.JSArrayElement] }
ElementList : Elision AssignmentExpression              { $1 ++ [AST.JSArrayElement $2]             {- 'ElementList1' -} }
            | AssignmentExpression                      { [AST.JSArrayElement $1]                   {- 'ElementList2' -} }
            | ElementList Elision AssignmentExpression  { (($1)++($2 ++ [AST.JSArrayElement $3]))   {- 'ElementList3' -} }


-- Elision :                                                             See 11.1.4
--        ,
--        Elision ,
Elision :: { [AST.JSArrayElement] }
Elision : Comma             { [AST.JSArrayComma $1]     {- 'Elision1' -} }
        | Comma Elision     { (AST.JSArrayComma $1):$2  {- 'Elision2' -} }

-- ObjectLiteral :                                                       See 11.1.5
--        { }
--        { PropertyNameAndValueList }
--        { PropertyNameAndValueList , }
ObjectLiteral :: { AST.JSExpression }
ObjectLiteral : LBrace RBrace                                { AST.JSObjectLiteral $1 (AST.JSCTLNone AST.JSLNil) $2     {- 'ObjectLiteral1' -} }
              | LBrace PropertyNameandValueList RBrace       { AST.JSObjectLiteral $1 (AST.JSCTLNone $2) $3             {- 'ObjectLiteral2' -} }
              | LBrace PropertyNameandValueList Comma RBrace { AST.JSObjectLiteral $1 (AST.JSCTLComma $2 $3) $4         {- 'ObjectLiteral3' -} }

-- <Property Name and Value List> ::= <Property Name> ':' <Assignment Expression>
--                                  | <Property Name and Value List> ',' <Property Name> ':' <Assignment Expression>

-- Seems we can have function declarations in the value part too
-- PropertyNameAndValueList :                                            See 11.1.5
--        PropertyAssignment
--        PropertyNameAndValueList , PropertyAssignment
PropertyNameandValueList :: { AST.JSCommaList AST.JSObjectProperty }
PropertyNameandValueList : PropertyAssignment                                { AST.JSLOne $1        {- 'PropertyNameandValueList1' -} }
                         | PropertyNameandValueList Comma PropertyAssignment { AST.JSLCons $1 $2 $3 {- 'PropertyNameandValueList2' -} }

-- PropertyAssignment :                                                  See 11.1.5
--        PropertyName : AssignmentExpression
--        get PropertyName() { FunctionBody }
--        set PropertyName( PropertySetParameterList ) { FunctionBody }
PropertyAssignment :: { AST.JSObjectProperty }
PropertyAssignment : PropertyName Colon AssignmentExpression { AST.JSPropertyNameandValue $1 $2 [$3] }
                   | IdentifierName { identifierToProperty $1 }
                   | MethodDefinition { AST.JSObjectMethod $1 }
                   | SpreadExpression { spreadExpressionToProperty $1 }

-- TODO: not clear if get/set are keywords, or just used in a specific context. Puzzling.
MethodDefinition :: { AST.JSMethodDefinition }
MethodDefinition : PropertyName LParen RParen FunctionBody
                     { AST.JSMethodDefinition $1 $2 AST.JSLNil $3 $4 }
                 | PropertyName LParen FormalParameterList RParen FunctionBody
                     { AST.JSMethodDefinition $1 $2 $3 $4 $5 }
                 | PropertyName LParen FormalParameterList Comma RParen FunctionBody
                     { AST.JSMethodDefinition $1 $2 $3 $5 $6 }
                 | '*' PropertyName LParen RParen FunctionBody
                     { AST.JSGeneratorMethodDefinition (mkJSAnnot $1) $2 $3 AST.JSLNil $4 $5 }
                 | '*' PropertyName LParen FormalParameterList RParen FunctionBody
                     { AST.JSGeneratorMethodDefinition (mkJSAnnot $1) $2 $3 $4 $5 $6 }
                 | '*' PropertyName LParen FormalParameterList Comma RParen FunctionBody
                     { AST.JSGeneratorMethodDefinition (mkJSAnnot $1) $2 $3 $4 $6 $7 }
                 -- Should be "get" in next, but is not a Token
                 | 'get' PropertyName LParen RParen FunctionBody
                     { AST.JSPropertyAccessor (AST.JSAccessorGet (mkJSAnnot $1)) $2 $3 AST.JSLNil $4 $5 }
                 -- Should be "set" in next, but is not a Token
                 | 'set' PropertyName LParen PropertySetParameterList RParen FunctionBody
                     { AST.JSPropertyAccessor (AST.JSAccessorSet (mkJSAnnot $1)) $2 $3 (AST.JSLOne $4) $5 $6 }

-- PropertyName :                                                        See 11.1.5
--        IdentifierName
--        StringLiteral
--        NumericLiteral
PropertyName :: { AST.JSPropertyName }
PropertyName : IdentifierName { propName $1 {- 'PropertyName1' -} }
             | StringLiteral  { propName $1 {- 'PropertyName2' -} }
             | NumericLiteral { propName $1 {- 'PropertyName3' -} }
             | LSquare AssignmentExpression RSquare { AST.JSPropertyComputed $1 $2 $3 {- 'PropertyName4' -} }

-- PropertySetParameterList :                                            See 11.1.5
--        Identifier
PropertySetParameterList :: { AST.JSExpression }
PropertySetParameterList : AssignmentExpression { $1 {- 'PropertySetParameterList' -} }

-- MemberExpression :                                           See 11.2
--        PrimaryExpression
--        FunctionExpression
--        MemberExpression [ Expression ]
--        MemberExpression . IdentifierName
--        new MemberExpression Arguments
MemberExpression :: { AST.JSExpression }
MemberExpression : PrimaryExpression   { $1 {- 'MemberExpression1' -} }
                 | FunctionExpression  { $1 {- 'MemberExpression2' -} }
                 | MemberExpression LSquare Expression RSquare { AST.JSMemberSquare $1 $2 $3 $4 {- 'MemberExpression3' -} }
                 | MemberExpression Dot IdentifierName         { AST.JSMemberDot $1 $2 $3       {- 'MemberExpression4' -} }
                 | MemberExpression OptionalChaining IdentifierName { AST.JSOptionalMemberDot $1 $2 $3 {- 'MemberExpression5' -} }
                 | MemberExpression '?.' '[' Expression ']' { AST.JSOptionalMemberSquare $1 (mkJSAnnot $2) $4 (mkJSAnnot $5) {- 'MemberExpression6' -} }
                 | MemberExpression TemplateLiteral            { mkJSTemplateLiteral (Just $1) $2 }
                 | Super LSquare Expression RSquare            { AST.JSMemberSquare $1 $2 $3 $4 }
                 | Super Dot IdentifierName                    { AST.JSMemberDot $1 $2 $3 }
                 | New MemberExpression Arguments              { mkJSMemberNew $1 $2 $3         {- 'MemberExpression5' -} }

-- NewExpression :                                              See 11.2
--        MemberExpression
--        new NewExpression
NewExpression :: { AST.JSExpression }
NewExpression : MemberExpression    { $1                        {- 'NewExpression1' -} }
              | New NewExpression   { AST.JSNewExpression $1 $2 {- 'NewExpression2' -} }

AwaitExpression :: { AST.JSExpression }
AwaitExpression
  : Await Expression { AST.JSAwaitExpression $1 $2 }

-- CallExpression :                                             See 11.2
--        MemberExpression Arguments
--        CallExpression Arguments
--        CallExpression [ Expression ]
--        CallExpression . IdentifierName
CallExpression :: { AST.JSExpression }
CallExpression : MemberExpression Arguments
                    { mkJSMemberExpression $1 $2 {- 'CallExpression1' -} }
               | Super Arguments
                    { mkJSCallExpression $1 $2 }
               | CallExpression Arguments
                    { mkJSCallExpression $1 $2 {- 'CallExpression2' -} }
               | CallExpression LSquare Expression RSquare
                    { AST.JSCallExpressionSquare $1 $2 $3 $4 {- 'CallExpression3' -} }
               | CallExpression Dot IdentifierName
                    { AST.JSCallExpressionDot $1 $2 $3 {- 'CallExpression4' -} }
               | CallExpression OptionalChaining IdentifierName
                    { AST.JSOptionalMemberDot $1 $2 $3 {- 'CallExpression5' -} }
               | CallExpression '?.' '[' Expression ']'
                    { AST.JSOptionalMemberSquare $1 (mkJSAnnot $2) $4 (mkJSAnnot $5) {- 'CallExpression6' -} }
               | MemberExpression OptionalChaining Arguments
                    { mkJSOptionalCallExpression $1 $2 $3 {- 'CallExpression7' -} }
               | CallExpression OptionalChaining Arguments
                    { mkJSOptionalCallExpression $1 $2 $3 {- 'CallExpression8' -} }
               | CallExpression TemplateLiteral
                    { mkJSTemplateLiteral (Just $1) $2 {- 'CallExpression9' -} }

-- Arguments :                                                  See 11.2
--        ()
--        ( ArgumentList )
--        ( ArgumentList , )
Arguments :: { JSArguments }
Arguments : LParen RParen                      { JSArguments $1 AST.JSLNil $2  {- 'Arguments1' -} }
          | LParen ArgumentList RParen         { JSArguments $1 $2 $3			{- 'Arguments2' -} }
          | LParen ArgumentList Comma RParen   { JSArguments $1 $2 $4			{- 'Arguments3' -} }

-- ArgumentList :                                               See 11.2
--        AssignmentExpression
--        ArgumentList , AssignmentExpression
ArgumentList :: { AST.JSCommaList AST.JSExpression }
ArgumentList : AssignmentExpression                    { AST.JSLOne $1          {- 'ArgumentList1' -} }
             | ArgumentList Comma AssignmentExpression { AST.JSLCons $1 $2 $3   {- 'ArgumentList2' -} }

-- LeftHandSideExpression :                                     See 11.2
--        NewExpression
--        CallExpression
LeftHandSideExpression :: { AST.JSExpression }
LeftHandSideExpression : NewExpression  { $1 {- 'LeftHandSideExpression1' -} }
                       | CallExpression { $1 {- 'LeftHandSideExpression12' -} }
                       | AwaitExpression { $1 {- 'LeftHandSideExpression13' -} }

-- PostfixExpression :                                          See 11.3
--        LeftHandSideExpression
--                                  [no LineTerminator here]
--        LeftHandSideExpression                             ++
--                                  [no LineTerminator here]
--        LeftHandSideExpression                             --
PostfixExpression :: { AST.JSExpression }
PostfixExpression : LeftHandSideExpression { $1 {- 'PostfixExpression' -} }
                  | PostfixExpression Increment { AST.JSExpressionPostfix $1 $2 }
                  | PostfixExpression Decrement { AST.JSExpressionPostfix $1 $2 }

-- UnaryExpression :                                            See 11.4
--        PostfixExpression
--        delete UnaryExpression
--        void UnaryExpression
--        typeof UnaryExpression
--        ++ UnaryExpression
--        -- UnaryExpression
--        + UnaryExpression
--        - UnaryExpression
--        ~ UnaryExpression
--        ! UnaryExpression
UnaryExpression :: { AST.JSExpression }
UnaryExpression : PostfixExpression         { $1 {- 'UnaryExpression' -} }
                | Delete    UnaryExpression { AST.JSUnaryExpression $1 $2 }
                | Void      UnaryExpression { AST.JSUnaryExpression $1 $2 }
                | Typeof    UnaryExpression { AST.JSUnaryExpression $1 $2 }
                | Increment UnaryExpression { AST.JSUnaryExpression $1 $2 }
                | Decrement UnaryExpression { AST.JSUnaryExpression $1 $2 }
                | Plus      UnaryExpression { AST.JSUnaryExpression (mkUnary $1) $2 }
                | Minus     UnaryExpression { AST.JSUnaryExpression (mkUnary $1) $2 }
                | Tilde     UnaryExpression { AST.JSUnaryExpression $1 $2 }
                | Not       UnaryExpression { AST.JSUnaryExpression $1 $2 }

-- ExponentiationExpression :                                  See ES2016
--        UnaryExpression
--        UnaryExpression ** ExponentiationExpression
ExponentiationExpression :: { AST.JSExpression }
ExponentiationExpression : UnaryExpression                                       { $1 {- 'ExponentiationExpression' -} }
                         | UnaryExpression Exp ExponentiationExpression          { AST.JSExpressionBinary {- '**' -} $1 $2 $3 }

-- MultiplicativeExpression :                                   See 11.5
--        ExponentiationExpression
--        MultiplicativeExpression * ExponentiationExpression
--        MultiplicativeExpression / ExponentiationExpression
--        MultiplicativeExpression % ExponentiationExpression
MultiplicativeExpression :: { AST.JSExpression }
MultiplicativeExpression : ExponentiationExpression                                    { $1 {- 'MultiplicativeExpression' -} }
                         | MultiplicativeExpression Mul ExponentiationExpression       { AST.JSExpressionBinary {- '*' -} $1 $2 $3 }
                         | MultiplicativeExpression Div ExponentiationExpression       { AST.JSExpressionBinary {- '/' -} $1 $2 $3 }
                         | MultiplicativeExpression Mod ExponentiationExpression       { AST.JSExpressionBinary {- '%' -} $1 $2 $3 }

-- AdditiveExpression :                                        See 11.6
--        MultiplicativeExpression
--        AdditiveExpression + MultiplicativeExpression
--        AdditiveExpression - MultiplicativeExpression
AdditiveExpression :: { AST.JSExpression }
AdditiveExpression : AdditiveExpression Plus  MultiplicativeExpression  { AST.JSExpressionBinary {- '+' -} $1 $2 $3 }
                   | AdditiveExpression Minus MultiplicativeExpression  { AST.JSExpressionBinary {- '-' -} $1 $2 $3 }
                   | MultiplicativeExpression                           { $1 {- 'AdditiveExpression' -} }

-- ShiftExpression :                                           See 11.7
--        AdditiveExpression
--        ShiftExpression << AdditiveExpression
--        ShiftExpression >> AdditiveExpression
--        ShiftExpression >>> AdditiveExpression
ShiftExpression :: { AST.JSExpression }
ShiftExpression : ShiftExpression Lsh  AdditiveExpression   { AST.JSExpressionBinary {- '<<' -}  $1 $2 $3 }
                | ShiftExpression Rsh  AdditiveExpression   { AST.JSExpressionBinary {- '>>' -}  $1 $2 $3 }
                | ShiftExpression Ursh AdditiveExpression   { AST.JSExpressionBinary {- '>>>' -} $1 $2 $3 }
                | AdditiveExpression                        { $1 {- 'ShiftExpression' -} }

-- RelationalExpression :                                      See 11.8
--        ShiftExpression
--        RelationalExpression < ShiftExpression
--        RelationalExpression > ShiftExpression
--        RelationalExpression <= ShiftExpression
--        RelationalExpression >= ShiftExpression
--        RelationalExpression instanceof ShiftExpression
--        RelationalExpression in ShiftExpression
RelationalExpression :: { AST.JSExpression }
RelationalExpression : ShiftExpression { $1 {- 'RelationalExpression' -} }
                     | RelationalExpression Lt  ShiftExpression { AST.JSExpressionBinary {- '<' -}  $1 $2 $3 }
                     | RelationalExpression Gt  ShiftExpression { AST.JSExpressionBinary {- '>' -}  $1 $2 $3 }
                     | RelationalExpression Le  ShiftExpression { AST.JSExpressionBinary {- '<=' -} $1 $2 $3 }
                     | RelationalExpression Ge  ShiftExpression { AST.JSExpressionBinary {- '>=' -} $1 $2 $3 }
                     | RelationalExpression Instanceof ShiftExpression { AST.JSExpressionBinary {- ' instanceof' -} $1 $2 $3 }
                     | RelationalExpression In         ShiftExpression { AST.JSExpressionBinary {- ' in        ' -} $1 $2 $3 }

-- RelationalExpressionNoIn :                                  See 11.8
--        ShiftExpression
--        RelationalExpressionNoIn < ShiftExpression
--        RelationalExpressionNoIn > ShiftExpression
--        RelationalExpressionNoIn <= ShiftExpression
--        RelationalExpressionNoIn >= ShiftExpression
--        RelationalExpressionNoIn instanceof ShiftExpression
RelationalExpressionNoIn :: { AST.JSExpression }
RelationalExpressionNoIn : ShiftExpression { $1 {- 'RelationalExpressionNoIn' -} }
                     | RelationalExpressionNoIn Lt  ShiftExpression { AST.JSExpressionBinary {- '<' -}  $1 $2 $3 }
                     | RelationalExpressionNoIn Gt  ShiftExpression { AST.JSExpressionBinary {- '>' -}  $1 $2 $3 }
                     | RelationalExpressionNoIn Le  ShiftExpression { AST.JSExpressionBinary {- '<=' -} $1 $2 $3 }
                     | RelationalExpressionNoIn Ge  ShiftExpression { AST.JSExpressionBinary {- '>=' -} $1 $2 $3 }
                     | RelationalExpressionNoIn Instanceof ShiftExpression { AST.JSExpressionBinary {- ' instanceof ' -} $1 $2 $3 }

-- EqualityExpression :                                        See 11.9
--        RelationalExpression
--        EqualityExpression == RelationalExpression
--        EqualityExpression != RelationalExpression
--        EqualityExpression === RelationalExpression
--        EqualityExpression !== RelationalExpression
EqualityExpression :: { AST.JSExpression }
EqualityExpression : RelationalExpression { $1 {- 'EqualityExpression' -} }
                   | EqualityExpression Equal    RelationalExpression { AST.JSExpressionBinary {- '==' -}  $1 $2 $3 }
                   | EqualityExpression Ne       RelationalExpression { AST.JSExpressionBinary {- '!=' -}  $1 $2 $3 }
                   | EqualityExpression StrictEq RelationalExpression { AST.JSExpressionBinary {- '===' -} $1 $2 $3 }
                   | EqualityExpression StrictNe RelationalExpression { AST.JSExpressionBinary {- '!==' -} $1 $2 $3 }

-- EqualityExpressionNoIn :                                    See 11.9
--        RelationalExpressionNoIn
--        EqualityExpressionNoIn == RelationalExpressionNoIn
--        EqualityExpressionNoIn != RelationalExpressionNoIn
--        EqualityExpressionNoIn === RelationalExpressionNoIn
--        EqualityExpressionNoIn !== RelationalExpressionNoIn
EqualityExpressionNoIn :: { AST.JSExpression }
EqualityExpressionNoIn : RelationalExpressionNoIn { $1 {- 'EqualityExpressionNoIn' -} }
                       | EqualityExpressionNoIn Equal    RelationalExpression { AST.JSExpressionBinary {- '==' -}  $1 $2 $3 }
                       | EqualityExpressionNoIn Ne       RelationalExpression { AST.JSExpressionBinary {- '!=' -}  $1 $2 $3 }
                       | EqualityExpressionNoIn StrictEq RelationalExpression { AST.JSExpressionBinary {- '===' -} $1 $2 $3 }
                       | EqualityExpressionNoIn StrictNe RelationalExpression { AST.JSExpressionBinary {- '!==' -} $1 $2 $3 }

-- BitwiseANDExpression :                                      See 11.10
--        EqualityExpression
--        BitwiseANDExpression & EqualityExpression
BitwiseAndExpression :: { AST.JSExpression }
BitwiseAndExpression : EqualityExpression { $1 {- 'BitwiseAndExpression' -} }
                     | BitwiseAndExpression BitAnd EqualityExpression { AST.JSExpressionBinary {- '&' -} $1 $2 $3 }

-- BitwiseANDExpressionNoIn :                                  See 11.10
--        EqualityExpressionNoIn
--        BitwiseANDExpressionNoIn & EqualityExpressionNoIn
BitwiseAndExpressionNoIn :: { AST.JSExpression }
BitwiseAndExpressionNoIn : EqualityExpressionNoIn { $1 {- 'BitwiseAndExpression' -} }
                     | BitwiseAndExpressionNoIn BitAnd EqualityExpressionNoIn { AST.JSExpressionBinary {- '&' -} $1 $2 $3 }

-- BitwiseXORExpression :                                                                See 11.10
--        BitwiseANDExpression
--        BitwiseXORExpression ^ BitwiseANDExpression
BitwiseXOrExpression :: { AST.JSExpression }
BitwiseXOrExpression : BitwiseAndExpression { $1 {- 'BitwiseXOrExpression' -} }
                     | BitwiseXOrExpression BitXor BitwiseAndExpression { AST.JSExpressionBinary {- '^' -} $1 $2 $3 }

-- BitwiseXORExpressionNoIn :                                                            See 11.10
--        BitwiseANDExpressionNoIn
--        BitwiseXORExpressionNoIn ^ BitwiseANDExpressionNoIn
BitwiseXOrExpressionNoIn :: { AST.JSExpression }
BitwiseXOrExpressionNoIn : BitwiseAndExpressionNoIn { $1 {- 'BitwiseXOrExpression' -} }
                         | BitwiseXOrExpressionNoIn BitXor BitwiseAndExpressionNoIn { AST.JSExpressionBinary {- '^' -} $1 $2 $3 }

-- BitwiseORExpression :                                                                 See 11.10
--        BitwiseXORExpression
--        BitwiseORExpression | BitwiseXORExpression
BitwiseOrExpression :: { AST.JSExpression }
BitwiseOrExpression : BitwiseXOrExpression { $1 {- 'BitwiseOrExpression' -} }
                    | BitwiseOrExpression BitOr BitwiseXOrExpression { AST.JSExpressionBinary {- '|' -} $1 $2 $3 }

-- BitwiseORExpressionNoIn :                                                             See 11.10
--        BitwiseXORExpressionNoIn
--        BitwiseORExpressionNoIn | BitwiseXORExpressionNoIn
BitwiseOrExpressionNoIn :: { AST.JSExpression }
BitwiseOrExpressionNoIn : BitwiseXOrExpressionNoIn { $1 {- 'BitwiseOrExpression' -} }
                        | BitwiseOrExpressionNoIn BitOr BitwiseXOrExpressionNoIn { AST.JSExpressionBinary {- '|' -} $1 $2 $3 }

-- LogicalANDExpression :                                                                See 11.11
--        BitwiseORExpression
--        LogicalANDExpression && BitwiseORExpression
LogicalAndExpression :: { AST.JSExpression }
LogicalAndExpression : BitwiseOrExpression { $1 {- 'LogicalAndExpression' -} }
                     | LogicalAndExpression And BitwiseOrExpression { AST.JSExpressionBinary {- '&&' -} $1 $2 $3 }

-- LogicalANDExpressionNoIn :                                                            See 11.11
--        BitwiseORExpressionNoIn
--        LogicalANDExpressionNoIn && BitwiseORExpressionNoIn
LogicalAndExpressionNoIn :: { AST.JSExpression }
LogicalAndExpressionNoIn : BitwiseOrExpressionNoIn { $1 {- 'LogicalAndExpression' -} }
                         | LogicalAndExpressionNoIn And BitwiseOrExpressionNoIn { AST.JSExpressionBinary {- '&&' -} $1 $2 $3 }

-- NullishCoalescingExpression :                                                        See 12.5.4
--        LogicalANDExpression
--        NullishCoalescingExpression ?? LogicalANDExpression
NullishCoalescingExpression :: { AST.JSExpression }
NullishCoalescingExpression : LogicalAndExpression { $1 {- 'NullishCoalescingExpression' -} }
                            | NullishCoalescingExpression NullishCoalescing LogicalAndExpression { AST.JSExpressionBinary {- '??' -} $1 $2 $3 }

-- LogicalORExpression :                                                                 See 11.11
--        NullishCoalescingExpression
--        LogicalORExpression || NullishCoalescingExpression
LogicalOrExpression :: { AST.JSExpression }
LogicalOrExpression : NullishCoalescingExpression { $1 {- 'LogicalOrExpression' -} }
                    | LogicalOrExpression Or NullishCoalescingExpression { AST.JSExpressionBinary {- '||' -} $1 $2 $3 }

-- NullishCoalescingExpressionNoIn :                                                    See 12.5.4
--        LogicalANDExpressionNoIn
--        NullishCoalescingExpressionNoIn ?? LogicalANDExpressionNoIn
NullishCoalescingExpressionNoIn :: { AST.JSExpression }
NullishCoalescingExpressionNoIn : LogicalAndExpressionNoIn { $1 {- 'NullishCoalescingExpression' -} }
                                | NullishCoalescingExpressionNoIn NullishCoalescing LogicalAndExpressionNoIn { AST.JSExpressionBinary {- '??' -} $1 $2 $3 }

-- LogicalORExpressionNoIn :                                                             See 11.11
--        NullishCoalescingExpressionNoIn
--        LogicalORExpressionNoIn || NullishCoalescingExpressionNoIn
LogicalOrExpressionNoIn :: { AST.JSExpression }
LogicalOrExpressionNoIn : NullishCoalescingExpressionNoIn { $1 {- 'LogicalOrExpression' -} }
                        | LogicalOrExpressionNoIn Or NullishCoalescingExpressionNoIn { AST.JSExpressionBinary {- '||' -} $1 $2 $3 }

-- ConditionalExpression :                                                               See 11.12
--        LogicalORExpression
--        LogicalORExpression ? AssignmentExpression : AssignmentExpression
ConditionalExpression :: { AST.JSExpression }
ConditionalExpression : LogicalOrExpression { $1 {- 'ConditionalExpression1' -} }
                      | LogicalOrExpression Hook AssignmentExpression Colon AssignmentExpression
                        { AST.JSExpressionTernary $1 $2 $3 $4 $5 {- 'ConditionalExpression2' -}  }

-- ConditionalExpressionNoIn :                                                           See 11.12
--        LogicalORExpressionNoIn
--        LogicalORExpressionNoIn ? AssignmentExpressionNoIn : AssignmentExpressionNoIn
ConditionalExpressionNoIn :: { AST.JSExpression }
ConditionalExpressionNoIn : LogicalOrExpressionNoIn { $1 {- 'ConditionalExpressionNoIn1' -} }
                          | LogicalOrExpressionNoIn Hook AssignmentExpressionNoIn Colon AssignmentExpressionNoIn
                            { AST.JSExpressionTernary $1 $2 $3 $4 $5 {- 'ConditionalExpressionNoIn2' -} }

-- AssignmentExpression :                                                                See 11.13
--        ConditionalExpression
--        LeftHandSideExpression AssignmentOperator AssignmentExpression
AssignmentExpression :: { AST.JSExpression }
AssignmentExpression : ConditionalExpression { $1 {- 'AssignmentExpression1' -} }
                     | YieldExpression { $1 }
                     | LeftHandSideExpression AssignmentOperator AssignmentExpression
                       { AST.JSAssignExpression $1 $2 $3 {- 'AssignmentExpression2' -} }
                     | SpreadExpression { $1 }

-- AssignmentExpressionNoIn :                                                            See 11.13
--        ConditionalExpressionNoIn
--        LeftHandSideExpression AssignmentOperator AssignmentExpressionNoIn
AssignmentExpressionNoIn :: { AST.JSExpression }
AssignmentExpressionNoIn : ConditionalExpressionNoIn { $1 {- 'AssignmentExpressionNoIn1' -} }
                         | YieldExpression { $1 }
                         | LeftHandSideExpression AssignmentOperator AssignmentExpressionNoIn
                           { AST.JSAssignExpression $1 $2 $3 {- 'AssignmentExpressionNoIn1' -} }

-- AssignmentOperator : one of                                                           See 11.13
--     '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>=' | '&=' | '^=' | '|='
AssignmentOperator :: { AST.JSAssignOp }
AssignmentOperator : OpAssign     { $1 }
                   | SimpleAssign { AST.JSAssign $1 {- 'SimpleAssign' -} }

-- Expression :                                                   See 11.14
--         AssignmentExpression
--         Expression , AssignmentExpression
Expression :: { AST.JSExpression }
Expression : AssignmentExpression { $1 {- 'Expression' -} }
           | Expression Comma AssignmentExpression  { AST.JSCommaExpression $1 $2 $3    {- 'Expression2' -} }

-- ExpressionNoIn :                                               See 11.14
--         AssignmentExpressionNoIn
--         ExpressionNoIn , AssignmentExpressionNoIn
ExpressionNoIn :: { AST.JSExpression }
ExpressionNoIn : AssignmentExpressionNoIn { $1 {- 'ExpressionNoIn' -} }
               | ExpressionNoIn Comma AssignmentExpressionNoIn  { AST.JSCommaExpression $1 $2 $3 {- 'ExpressionNoIn2' -} }

-- TODO: still required?
ExpressionOpt :: { AST.JSCommaList AST.JSExpression }
ExpressionOpt : Expression { AST.JSLOne $1  {- 'ExpressionOpt1' -} }
              |            { AST.JSLNil     {- 'ExpressionOpt2' -} }

ExpressionNoInOpt :: { AST.JSCommaList AST.JSExpression }
ExpressionNoInOpt : ExpressionNoIn { AST.JSLOne $1  {- 'ExpressionOpt1' -} }
                  |                { AST.JSLNil     {- 'ExpressionOpt2' -} }


-- Statement :                                                    See clause 12
--         Block
--         VariableStatement
--         EmptyStatement
--         ExpressionStatement
--         IfStatement
--         IterationStatement
--         ContinueStatement
--         BreakStatement
--         ReturnStatement
--         WithStatement
--         LabelledStatement
--         SwitchStatement
--         ThrowStatement
--         TryStatement
--         DebuggerStatement
Statement :: { AST.JSStatement }
Statement : StatementNoEmpty   { $1 {- 'Statement1' -} }
          | EmptyStatement     { $1 {- 'Statement2' -} }

StatementNoEmpty :: { AST.JSStatement }
StatementNoEmpty
  : IfStatement             { $1 {- 'StatementNoEmpty5' -} }
  | ContinueStatement       { $1 {- 'StatementNoEmpty7' -} }
  | BreakStatement          { $1 {- 'StatementNoEmpty8' -} }
  | ReturnStatement         { $1 {- 'StatementNoEmpty9' -} }
  | WithStatement           { $1 {- 'StatementNoEmpty10' -} }
  | LabelledStatement       { $1 {- 'StatementNoEmpty11' -} }
  | SwitchStatement         { $1 {- 'StatementNoEmpty12' -} }
  | ThrowStatement          { $1 {- 'StatementNoEmpty13' -} }
  | TryStatement            { $1 {- 'StatementNoEmpty14' -} }
  | StatementBlock          { $1 {- 'StatementNoEmpty1' -} }
  | VariableStatement       { $1 {- 'StatementNoEmpty2' -} }
  | IterationStatement      { $1 {- 'StatementNoEmpty6' -} }
  | ExpressionStatement     { $1 {- 'StatementNoEmpty4' -} }
  | AsyncFunctionStatement  { $1 {- 'StatementNoEmpty15' -} }
  | DebuggerStatement       { $1 {- 'StatementNoEmpty15' -} }



StatementBlock :: { AST.JSStatement }
StatementBlock : Block MaybeSemi       { blockToStatement $1 $2 {- 'StatementBlock1' -} }


-- Block :                                                        See 12.1
--         { StatementListopt }
Block :: { AST.JSBlock }
Block : LBrace RBrace               { AST.JSBlock $1 [] $2 {- 'Block1' -} }
      | LBrace StatementList RBrace { AST.JSBlock $1 $2 $3 {- 'Block2' -} }

-- StatementList :                                                See 12.1
--         Statement
--         StatementList Statement
StatementList :: { [AST.JSStatement] }
StatementList : Statement               { [$1]       {- 'StatementList1' -} }
              | StatementList Statement { ($1++[$2]) {- 'StatementList2' -} }

-- VariableStatement :                                            See 12.2
--         var VariableDeclarationList ;
VariableStatement :: { AST.JSStatement }
VariableStatement : Var   VariableDeclarationList MaybeSemi { AST.JSVariable $1 $2 $3 {- 'VariableStatement1' -} }
                  | Let   VariableDeclarationList MaybeSemi { AST.JSLet      $1 $2 $3 {- 'VariableStatement2' -} }
                  | Const VariableDeclarationList MaybeSemi { AST.JSConstant $1 $2 $3 {- 'VariableStatement3' -} }

-- VariableDeclarationList :                                      See 12.2
--         VariableDeclaration
--         VariableDeclarationList , VariableDeclaration
VariableDeclarationList :: { AST.JSCommaList AST.JSExpression }
VariableDeclarationList : VariableDeclaration                               { AST.JSLOne $1         {- 'VariableDeclarationList1' -} }
                        | VariableDeclarationList Comma VariableDeclaration { AST.JSLCons $1 $2 $3  {- 'VariableDeclarationList2' -} }

-- VariableDeclarationListNoIn :                                  See 12.2
--         VariableDeclarationNoIn
--         VariableDeclarationListNoIn , VariableDeclarationNoIn
VariableDeclarationListNoIn :: { AST.JSCommaList AST.JSExpression }
VariableDeclarationListNoIn : VariableDeclarationNoIn                                   { AST.JSLOne $1         {- 'VariableDeclarationListNoIn1' -} }
                            | VariableDeclarationListNoIn Comma VariableDeclarationNoIn { AST.JSLCons $1 $2 $3  {- 'VariableDeclarationListNoIn2' -} }

-- VariableDeclaration :                                          See 12.2
--         Identifier Initialiseropt
VariableDeclaration :: { AST.JSExpression }
VariableDeclaration : PrimaryExpression SimpleAssign AssignmentExpression { AST.JSVarInitExpression $1 (AST.JSVarInit $2 $3) {- 'JSVarInitExpression1' -} }
                    | Identifier                                          { AST.JSVarInitExpression $1 AST.JSVarInitNone     {- 'JSVarInitExpression2' -} }

-- VariableDeclarationNoIn :                                      See 12.2
--         Identifier InitialiserNoInopt
VariableDeclarationNoIn :: { AST.JSExpression }
VariableDeclarationNoIn : PrimaryExpression SimpleAssign AssignmentExpression { AST.JSVarInitExpression $1 (AST.JSVarInit $2 $3) {- 'JSVarInitExpressionInit2' -} }
                        | Identifier                                          { AST.JSVarInitExpression $1 AST.JSVarInitNone     {- 'JSVarInitExpression2' -} }

-- EmptyStatement :                                                                         See 12.3
--         ;
EmptyStatement :: { AST.JSStatement }
EmptyStatement : Semi { AST.JSEmptyStatement $1 {- 'EmptyStatement' -} }

-- ExpressionStatement :                                                                    See 12.4
--         [lookahead not in {{, function}] Expression  ;
-- TODO: Sort out lookahead issue. Maybe by just putting production lower to set reduce/reduce conflict
--       According to http://sideshowbarker.github.com/es5-spec/#x12.4, the ambiguity is with
--       Block or FunctionDeclaration
ExpressionStatement :: { AST.JSStatement }
ExpressionStatement : Expression MaybeSemi { expressionToStatement $1 $2 {- 'ExpressionStatement' -} }


-- IfStatement :                                                                            See 12.5
--         if ( Expression ) Statement else Statement
--         if ( Expression ) Statement
IfStatement :: { AST.JSStatement } -- +++XXXX++
IfStatement : If LParen Expression RParen EmptyStatement
                  { AST.JSIf $1 $2 $3 $4 $5                            {- 'IfStatement1' -} }
            | If LParen Expression RParen StatementNoEmpty Else Statement
                  {  AST.JSIfElse $1 $2 $3 $4 $5 $6 $7                 {- 'IfStatement3' -} }
            | If LParen Expression RParen StatementNoEmpty
                  { AST.JSIf $1 $2 $3 $4 $5                            {- 'IfStatement3' -} }
            | If LParen Expression RParen EmptyStatement Else Statement
                  {  AST.JSIfElse $1 $2 $3 $4 $5 $6 $7                 {- 'IfStatement4' -} }

-- IterationStatement :                                                                     See 12.6
--         do Statement while ( Expression );
--         while ( Expression ) Statement
--         for (ExpressionNoInopt; Expressionopt ; Expressionopt ) Statement
--         for ( var VariableDeclarationListNoIn; Expressionopt ; Expressionopt ) Statement
--         for ( LeftHandSideExpression in Expression ) Statement
--         for ( var VariableDeclarationNoIn in Expression ) Statement
IterationStatement :: { AST.JSStatement }
IterationStatement : Do StatementNoEmpty While LParen Expression RParen MaybeSemi
                     { AST.JSDoWhile $1 $2 $3 $4 $5 $6 $7 {- 'IterationStatement1' -} }
                   | While LParen Expression RParen Statement
                     { AST.JSWhile $1 $2 $3 $4 $5 {- 'IterationStatement2' -} }
                   | For LParen ExpressionNoInOpt Semi ExpressionOpt Semi ExpressionOpt RParen Statement
                     { AST.JSFor $1 $2 $3 $4 $5 $6 $7 $8 $9 {- 'IterationStatement3' -} }
                   | For LParen Var VariableDeclarationListNoIn Semi ExpressionOpt Semi ExpressionOpt RParen Statement
                     { AST.JSForVar $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 {- 'IterationStatement4' -} }
                   | For LParen LeftHandSideExpression In Expression RParen Statement
                     { AST.JSForIn $1 $2 $3 $4 $5 $6 $7 {- 'IterationStatement 5' -} }
                   | For LParen Var VariableDeclarationNoIn In Expression RParen Statement
                     { AST.JSForVarIn $1 $2 $3 $4 $5 $6 $7 $8 {- 'IterationStatement6' -} }
                   | For LParen Let VariableDeclarationListNoIn Semi ExpressionOpt Semi ExpressionOpt RParen Statement
                     { AST.JSForLet $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 {- 'IterationStatement 7' -} }
                   | For LParen Let VariableDeclarationNoIn In Expression RParen Statement
                     { AST.JSForLetIn $1 $2 $3 $4 $5 $6 $7 $8 {- 'IterationStatement 8' -} }
                   | For LParen Let VariableDeclarationNoIn Of Expression RParen Statement
                     { AST.JSForLetOf $1 $2 $3 $4 $5 $6 $7 $8 {- 'IterationStatement 9' -} }
                   | For LParen LeftHandSideExpression Of Expression RParen Statement
                     { AST.JSForOf $1 $2 $3 $4 $5 $6 $7 {- 'IterationStatement 10'-} }
                   | For LParen Var VariableDeclarationNoIn Of Expression RParen Statement
                     { AST.JSForVarOf $1 $2 $3 $4 $5 $6 $7 $8 {- 'IterationStatement 11' -} }
                   | For LParen Const VariableDeclarationListNoIn Semi ExpressionOpt Semi ExpressionOpt RParen Statement
                     { AST.JSForConst $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 {- 'IterationStatement 12' -} }
                   | For LParen Const VariableDeclarationNoIn In Expression RParen Statement
                     { AST.JSForConstIn $1 $2 $3 $4 $5 $6 $7 $8 {- 'IterationStatement 13' -} }
                   | For LParen Const VariableDeclarationNoIn Of Expression RParen Statement
                     { AST.JSForConstOf $1 $2 $3 $4 $5 $6 $7 $8 {- 'IterationStatement 14' -} }

-- ContinueStatement :                                                                      See 12.7
--         continue [no LineTerminator here] Identifieropt ;
ContinueStatement :: { AST.JSStatement }
ContinueStatement : Continue AutoSemi              { AST.JSContinue $1 AST.JSIdentNone $2  {- 'ContinueStatement1' -} }
                  | Continue Identifier MaybeSemi  { AST.JSContinue $1 (identName $2) $3   {- 'ContinueStatement2' -} }

-- BreakStatement :                                                                         See 12.8
--         break [no LineTerminator here] Identifieropt ;
BreakStatement :: { AST.JSStatement }
BreakStatement : Break AutoSemi              { AST.JSBreak $1 AST.JSIdentNone $2 {- 'BreakStatement1' -} }
               | Break Identifier MaybeSemi  { AST.JSBreak $1 (identName $2) $3  {- 'BreakStatement2' -} }

-- ReturnStatement :                                                                        See 12.9
--         return [no LineTerminator here] Expressionopt ;
ReturnStatement :: { AST.JSStatement }
ReturnStatement : Return AutoSemi              { AST.JSReturn $1 Nothing $2 }
                | Return Expression MaybeSemi  { AST.JSReturn $1 (Just $2) $3 }

-- WithStatement :                                                                          See 12.10
--         with ( Expression ) Statement
WithStatement :: { AST.JSStatement }
WithStatement : With LParen Expression RParen Statement MaybeSemi  { AST.JSWith $1 $2 $3 $4 $5 $6 }

-- SwitchStatement :                                                                        See 12.11
--         switch ( Expression ) CaseBlock
SwitchStatement :: { AST.JSStatement }
SwitchStatement : Switch LParen Expression RParen LBrace CaseBlock RBrace MaybeSemi { AST.JSSwitch $1 $2 $3 $4 $5 $6 $7 $8 }

-- CaseBlock :                                                                              See 12.11
--         { CaseClausesopt }
--         { CaseClausesopt DefaultClause CaseClausesopt }
CaseBlock :: { [AST.JSSwitchParts] }
CaseBlock : CaseClausesOpt                              { $1           {- 'CaseBlock1' -} }
          | CaseClausesOpt DefaultClause CaseClausesOpt { $1++[$2]++$3 {- 'CaseBlock2' -} }

-- CaseClauses :                                                                            See 12.11
--         CaseClause
--         CaseClauses CaseClause
CaseClausesOpt :: { [AST.JSSwitchParts] }
CaseClausesOpt : CaseClause                { [$1]       {- 'CaseClausesOpt1' -} }
               | CaseClausesOpt CaseClause { ($1++[$2]) {- 'CaseClausesOpt2' -} }
               |                           { []         {- 'CaseClausesOpt3' -} }

-- CaseClause :                                                               See 12.11
--        case Expression : StatementListopt
CaseClause :: { AST.JSSwitchParts }
CaseClause : Case Expression Colon StatementList  { AST.JSCase $1 $2 $3 $4 {- 'CaseClause1' -} }
           | Case Expression Colon                { AST.JSCase $1 $2 $3 [] {- 'CaseClause2' -} }

-- DefaultClause :                                                            See 12.11
--        default : StatementListopt
DefaultClause :: { AST.JSSwitchParts }
DefaultClause : Default Colon                { AST.JSDefault $1 $2 [] {- 'DefaultClause1' -} }
              | Default Colon StatementList  { AST.JSDefault $1 $2 $3 {- 'DefaultClause2' -} }

-- LabelledStatement :                                                        See 12.12
--        Identifier : Statement
LabelledStatement :: { AST.JSStatement }
LabelledStatement : Identifier Colon Statement { AST.JSLabelled (identName $1) $2 $3 {- 'LabelledStatement' -} }

-- ThrowStatement :                                                           See 12.13
--        throw [no LineTerminator here] Expression ;
ThrowStatement :: { AST.JSStatement }
ThrowStatement : Throw Expression MaybeSemi { AST.JSThrow $1 $2 $3 {- 'ThrowStatement' -} }

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
--   i.e., 0 or more catches, then an optional finally
-- TryStatement :                                                             See 12.14
--        try Block Catch
--        try Block Finally
--        try Block Catch Finally
TryStatement :: { AST.JSStatement }
TryStatement : Try Block Catches         { AST.JSTry $1 $2 $3 AST.JSNoFinally {- 'TryStatement1' -} }
             | Try Block Finally         { AST.JSTry $1 $2 [] $3              {- 'TryStatement2' -} }
             | Try Block Catches Finally { AST.JSTry $1 $2 $3 $4              {- 'TryStatement3' -} }

Catches :: { [AST.JSTryCatch] }
Catches : Catch         { [$1]       {- 'Catches1' -} }
        | Catches Catch { ($1++[$2]) {- 'Catches2' -} }

-- Note: worked in updated syntax as per https://developer.mozilla.org/en/JavaScript/Reference/Statements/try...catch
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--   becomes
-- <Catch> ::= 'catch' '(' Identifier ')' <Block>
--           | 'catch' '(' Identifier 'if' ConditionalExpression ')' <Block>
Catch :: { AST.JSTryCatch }
Catch : CatchL LParen Identifier                          RParen Block { AST.JSCatch $1 $2 $3 $4 $5 {- 'Catch1' -} }
      | CatchL LParen Identifier If ConditionalExpression RParen Block { AST.JSCatchIf $1 $2 $3 $4 $5 $6 $7 {- 'Catch2' -} }

-- Finally :                                                                  See 12.14
--        finally Block
Finally :: { AST.JSTryFinally }
Finally : FinallyL Block { AST.JSFinally $1 $2 {- 'Finally' -} }

-- DebuggerStatement :                                                        See 12.15
--        debugger ;
DebuggerStatement :: { AST.JSStatement }
DebuggerStatement : 'debugger' MaybeSemi { AST.JSExpressionStatement (AST.JSLiteral (mkJSAnnot $1) ("debugger")) $2 {- 'DebuggerStatement' -} }

-- FunctionDeclaration :                                                      See clause 13
--        function Identifier ( FormalParameterListopt ) { FunctionBody }
FunctionDeclaration :: { AST.JSStatement }
FunctionDeclaration : NamedFunctionExpression MaybeSemi { expressionToStatement $1 $2                {- 'FunctionDeclaration1' -} }

AsyncFunctionStatement :: { AST.JSStatement }
AsyncFunctionStatement : Async NamedFunctionExpression MaybeSemi {  expressionToAsyncFunction $1 $2 $3  {- 'AsyncFunctionStatement1' -} }

-- FunctionExpression :                                                       See clause 13
--        function Identifieropt ( FormalParameterListopt ) { FunctionBody }
FunctionExpression :: { AST.JSExpression }
FunctionExpression : ArrowFunctionExpression     { $1 {- 'ArrowFunctionExpression' -} }
                   | LambdaExpression            { $1 {- 'FunctionExpression1' -} }
                   | NamedFunctionExpression     { $1 {- 'FunctionExpression2' -} }
                   | AsyncFunctionExpression     { $1 {- 'AsyncFunctionExpression' -} }

ArrowFunctionExpression :: { AST.JSExpression }
ArrowFunctionExpression : ArrowParameterList Arrow ConciseBody
                           { AST.JSArrowExpression $1 $2 $3 }

ArrowParameterList :: { AST.JSArrowParameterList }
ArrowParameterList : PrimaryExpression {%^ toArrowParameterList $1 }
                   | LParen RParen
                      { AST.JSParenthesizedArrowParameterList $1 AST.JSLNil $2 }

ConciseBody :: { AST.JSConciseBody }
ConciseBody : Block                    { AST.JSConciseFunctionBody $1 }
            | AssignmentExpression     { AST.JSConciseExpressionBody $1 }

StatementOrBlock :: { AST.JSStatement }
StatementOrBlock : Block MaybeSemi		{ blockToStatement $1 $2 }
                 | Expression MaybeSemi { expressionToStatement $1 $2 }

-- StatementListItem :
--        Statement
--        Declaration
StatementListItem :: { AST.JSStatement }
StatementListItem : Statement           { $1 }

NamedFunctionExpression :: { AST.JSExpression }
NamedFunctionExpression : Function Identifier LParen RParen FunctionBody
                            { AST.JSFunctionExpression $1 (identName $2) $3 AST.JSLNil $4 $5    {- 'NamedFunctionExpression1' -} }
                        | Function Identifier LParen FormalParameterList RParen FunctionBody
                            { AST.JSFunctionExpression $1 (identName $2) $3 $4 $5 $6            {- 'NamedFunctionExpression2' -} }
                        | Function Identifier LParen FormalParameterList Comma RParen FunctionBody
                            { AST.JSFunctionExpression $1 (identName $2) $3 $4 $6 $7            {- 'NamedFunctionExpression3' -} }

LambdaExpression :: { AST.JSExpression }
LambdaExpression : Function LParen RParen FunctionBody
                    { AST.JSFunctionExpression $1 AST.JSIdentNone $2 AST.JSLNil $3 $4	{- 'LambdaExpression1' -} }
                 | Function LParen FormalParameterList RParen FunctionBody
                    { AST.JSFunctionExpression $1 AST.JSIdentNone $2 $3 $4 $5           {- 'LambdaExpression2' -} }
                 | Function LParen FormalParameterList Comma RParen FunctionBody
                    { AST.JSFunctionExpression $1 AST.JSIdentNone $2 $3 $5 $6           {- 'LambdaExpression3' -} }

AsyncFunctionExpression :: { AST.JSExpression }
AsyncFunctionExpression : Async Function LParen RParen FunctionBody
                           { AST.JSAsyncFunctionExpression $1 $2 AST.JSIdentNone $3 AST.JSLNil $4 $5   {- 'AsyncFunctionExpression1' -} }
                        | Async Function LParen FormalParameterList RParen FunctionBody
                           { AST.JSAsyncFunctionExpression $1 $2 AST.JSIdentNone $3 $4 $5 $6           {- 'AsyncFunctionExpression2' -} }
                        | Async Function LParen FormalParameterList Comma RParen FunctionBody
                           { AST.JSAsyncFunctionExpression $1 $2 AST.JSIdentNone $3 $4 $6 $7           {- 'AsyncFunctionExpression3' -} }
                        | AsyncNamedFunctionExpression     { $1 {- 'AsyncFunctionExpression4' -} }

AsyncNamedFunctionExpression :: { AST.JSExpression }
AsyncNamedFunctionExpression : Async Function Identifier LParen RParen FunctionBody
                                { AST.JSAsyncFunctionExpression $1 $2 (identName $3) $4 AST.JSLNil $5 $6    {- 'AsyncNamedFunctionExpression1' -} }
                             | Async Function Identifier LParen FormalParameterList RParen FunctionBody
                                { AST.JSAsyncFunctionExpression $1 $2 (identName $3) $4 $5 $6 $7            {- 'AsyncNamedFunctionExpression2' -} }
                             | Async Function Identifier LParen FormalParameterList Comma RParen FunctionBody
                                { AST.JSAsyncFunctionExpression $1 $2 (identName $3) $4 $5 $7 $8            {- 'AsyncNamedFunctionExpression3' -} }

-- GeneratorDeclaration :
--         function * BindingIdentifier ( FormalParameters ) { GeneratorBody }
--         function * ( FormalParameters ) { GeneratorBody }
GeneratorDeclaration :: { AST.JSStatement }
GeneratorDeclaration : NamedGeneratorExpression MaybeSemi  { expressionToStatement $1 $2 }

-- GeneratorExpression :
--         function * BindingIdentifieropt ( FormalParameters ) { GeneratorBody }
-- GeneratorBody :
--         FunctionBody
GeneratorExpression :: { AST.JSExpression }
GeneratorExpression : NamedGeneratorExpression { $1 }
                    | Function '*' LParen RParen FunctionBody
                        { AST.JSGeneratorExpression $1 (mkJSAnnot $2) AST.JSIdentNone $3 AST.JSLNil $4 $5 }
                    | Function '*' LParen FormalParameterList RParen FunctionBody
                        { AST.JSGeneratorExpression $1 (mkJSAnnot $2) AST.JSIdentNone $3 $4 $5 $6 }
                    | Function '*' LParen FormalParameterList Comma RParen FunctionBody
                        { AST.JSGeneratorExpression $1 (mkJSAnnot $2) AST.JSIdentNone $3 $4 $6 $7 }

NamedGeneratorExpression :: { AST.JSExpression }
NamedGeneratorExpression : Function '*' Identifier LParen RParen FunctionBody
                             { AST.JSGeneratorExpression $1 (mkJSAnnot $2) (identName $3) $4 AST.JSLNil $5 $6 }
                         | Function '*' Identifier LParen FormalParameterList RParen FunctionBody
                             { AST.JSGeneratorExpression $1 (mkJSAnnot $2) (identName $3) $4 $5 $6 $7 }
                         | Function '*' Identifier LParen FormalParameterList Comma RParen FunctionBody
                             { AST.JSGeneratorExpression $1 (mkJSAnnot $2) (identName $3) $4 $5 $7 $8 }

-- YieldExpression :
--         yield
--         yield [no LineTerminator here] AssignmentExpression
--         yield [no LineTerminator here] * AssignmentExpression
YieldExpression :: { AST.JSExpression }
YieldExpression : Yield { AST.JSYieldExpression $1 Nothing }
                | Yield AssignmentExpression { AST.JSYieldExpression $1 (Just $2) }
                | Yield '*' AssignmentExpression { AST.JSYieldFromExpression $1 (mkJSAnnot $2) $3 }


IdentifierOpt :: { AST.JSIdent }
IdentifierOpt : Identifier { identName $1     {- 'IdentifierOpt1' -} }
              |            { AST.JSIdentNone  {- 'IdentifierOpt2' -} }

-- FormalParameterList :                                                      See clause 13
--        Identifier
--        FormalParameterList , Identifier
FormalParameterList :: { AST.JSCommaList AST.JSExpression }
FormalParameterList : AssignmentExpression                           { AST.JSLOne $1         {- 'FormalParameterList1' -} }
                    | FormalParameterList Comma AssignmentExpression { AST.JSLCons $1 $2 $3  {- 'FormalParameterList2' -} }

-- FunctionBody :                                                             See clause 13
--        SourceElementsopt
FunctionBody :: { AST.JSBlock }
FunctionBody : Block                    { $1    {- 'FunctionBody1' -} }

-- ClassDeclaration :
--         class BindingIdentifier ClassTail
--         class ClassTail
-- ClassExpression :
--         class BindingIdentifieropt ClassTail
-- ClassTail :
--         ClassHeritageopt { ClassBodyopt }
ClassDeclaration :: { AST.JSStatement }
ClassDeclaration : Class Identifier ClassHeritage LBrace ClassBody RBrace { AST.JSClass $1 (identName $2) $3 $4 $5 $6 AST.JSSemiAuto }

ClassExpression :: { AST.JSExpression }
ClassExpression : Class Identifier ClassHeritage LBrace ClassBody RBrace { AST.JSClassExpression $1 (identName $2)  $3 $4 $5 $6 }
                | Class            ClassHeritage LBrace ClassBody RBrace { AST.JSClassExpression $1 AST.JSIdentNone $2 $3 $4 $5 }

-- ClassHeritage :
--         extends LeftHandSideExpression
ClassHeritage :: { AST.JSClassHeritage }
ClassHeritage : Extends LeftHandSideExpression { AST.JSExtends $1 $2 }
              |                                { AST.JSExtendsNone }

-- ClassBody :
--         ClassElementList
-- ClassElementList :
--         ClassElement
--         ClassElementList ClassElement
ClassBody :: { [AST.JSClassElement] }
ClassBody :                        { [] }
          | ClassBody ClassElement { $1 ++ [$2] }

-- ClassElement :
--         MethodDefinition
--         static MethodDefinition
--         ;
ClassElement :: { AST.JSClassElement }
ClassElement : MethodDefinition              { AST.JSClassInstanceMethod $1 }
             | Static MethodDefinition       { AST.JSClassStaticMethod $1 $2 }
             | Semi                          { AST.JSClassSemi $1 }
             | PrivateField                  { $1 }
             | PrivateMethod                 { $1 }
             | PrivateAccessor               { $1 }

-- Private field declarations: #field = value; or #field;
PrivateField :: { AST.JSClassElement }
PrivateField : 'private' '=' AssignmentExpression AutoSemi { AST.JSPrivateField (mkJSAnnot $1) (extractPrivateName $1) (mkJSAnnot $2) (Just $3) $4 }
             | 'private' AutoSemi                         { AST.JSPrivateField (mkJSAnnot $1) (extractPrivateName $1) (mkJSAnnot $1) Nothing $2 }

-- Private method definitions: #method() { }
PrivateMethod :: { AST.JSClassElement }
PrivateMethod : 'private' LParen RParen FunctionBody                     { AST.JSPrivateMethod (mkJSAnnot $1) (extractPrivateName $1) $2 AST.JSLNil $3 $4 }
              | 'private' LParen FormalParameterList RParen FunctionBody { AST.JSPrivateMethod (mkJSAnnot $1) (extractPrivateName $1) $2 $3 $4 $5 }

-- Private accessor methods: get #prop() { } or set #prop(value) { }
PrivateAccessor :: { AST.JSClassElement }
PrivateAccessor : 'get' 'private' LParen RParen FunctionBody                     { AST.JSPrivateAccessor (AST.JSAccessorGet (mkJSAnnot $1)) (mkJSAnnot $2) (extractPrivateName $2) $3 AST.JSLNil $4 $5 }
                | 'set' 'private' LParen FormalParameterList RParen FunctionBody { AST.JSPrivateAccessor (AST.JSAccessorSet (mkJSAnnot $1)) (mkJSAnnot $2) (extractPrivateName $2) $3 $4 $5 $6 }

-- Program :                                                                  See clause 14
--        SourceElementsopt

Program :: { AST.JSAST }
Program : StatementList Eof     	{ AST.JSAstProgram $1 $2   	{- 'Program1' -} }
        | Eof                   	{ AST.JSAstProgram [] $1 	{- 'Program2' -} }

-- Module :                                                                   See 15.2
--        ModuleBody[opt]
--
-- ModuleBody :
--        ModuleItemList
Module :: { AST.JSAST }
Module : ModuleItemList Eof     	{ AST.JSAstModule $1 $2   	{- 'Module1' -} }
        | Eof                   	{ AST.JSAstModule [] $1 	{- 'Module2' -} }

-- ModuleItemList :
--         ModuleItem
--         ModuleItemList ModuleItem
ModuleItemList :: { [AST.JSModuleItem] }
ModuleItemList : ModuleItem                  { [$1]         {- 'ModuleItemList1' -} }
               | ModuleItemList ModuleItem   { ($1++[$2])   {- 'ModuleItemList2' -} }

-- ModuleItem :
--        ImportDeclaration
--        ExportDeclaration
--        StatementListItem
ModuleItem :: { AST.JSModuleItem }
ModuleItem : Import ImportDeclaration
                    { AST.JSModuleImportDeclaration $1 $2   {- 'ModuleItem1' -} }
           | Export ExportDeclaration
                    { AST.JSModuleExportDeclaration $1 $2   {- 'ModuleItem1' -} }
           | StatementListItem
                    { AST.JSModuleStatementListItem $1      {- 'ModuleItem2' -} }

ImportDeclaration :: { AST.JSImportDeclaration }
ImportDeclaration : ImportClause FromClause ImportAttributesOpt AutoSemi
                          { AST.JSImportDeclaration $1 $2 $3 $4 }
                  | 'string' ImportAttributesOpt AutoSemi
                          { AST.JSImportDeclarationBare (mkJSAnnot $1) (tokenLiteral $1) $2 $3 }

ImportClause :: { AST.JSImportClause }
ImportClause : IdentifierName
                     { AST.JSImportClauseDefault (identName $1) }
             | NameSpaceImport
                     { AST.JSImportClauseNameSpace $1 }
             | NamedImports
                     { AST.JSImportClauseNamed $1 }
             | IdentifierName ',' NameSpaceImport
                     { AST.JSImportClauseDefaultNameSpace (identName $1) (mkJSAnnot $2) $3 }
             | IdentifierName ',' NamedImports
                     { AST.JSImportClauseDefaultNamed (identName $1) (mkJSAnnot $2) $3 }

FromClause :: { AST.JSFromClause }
FromClause : From 'string'
                  { AST.JSFromClause $1 (mkJSAnnot $2) (tokenLiteral $2) }

NameSpaceImport :: { AST.JSImportNameSpace }
NameSpaceImport : Mul As IdentifierName
                        { AST.JSImportNameSpace $1 $2 (identName $3) }

NamedImports :: { AST.JSImportsNamed }
NamedImports : LBrace ImportsList RBrace
                      { AST.JSImportsNamed $1 $2 $3 }

ImportsList :: { AST.JSCommaList AST.JSImportSpecifier }
ImportsList : ImportSpecifier
                    { AST.JSLOne $1 }
            | ImportsList Comma ImportSpecifier
                    { AST.JSLCons $1 $2 $3 }

ImportSpecifier :: { AST.JSImportSpecifier }
ImportSpecifier : IdentifierName
                    { AST.JSImportSpecifier (identName $1) }
                | IdentifierName As IdentifierName
                    { AST.JSImportSpecifierAs (identName $1) $2 (identName $3) }

ImportAttributesOpt :: { Maybe AST.JSImportAttributes }
ImportAttributesOpt : {- empty -}                { Nothing }
                    | 'with' LBrace ImportAttributeList RBrace  { Just (AST.JSImportAttributes $2 $3 $4) }

ImportAttributeList :: { AST.JSCommaList AST.JSImportAttribute }
ImportAttributeList : ImportAttribute               { AST.JSLOne $1 }
                    | ImportAttributeList Comma ImportAttribute  { AST.JSLCons $1 $2 $3 }

ImportAttribute :: { AST.JSImportAttribute }
ImportAttribute : IdentifierName Colon 'string'
                    { AST.JSImportAttribute (identName $1) $2 (AST.JSStringLiteral (mkJSAnnot $3) (tokenLiteral $3)) }

-- ExportDeclaration :                                                        See 15.2.3
-- [ ]    export * FromClause ;
-- [x]    export ExportClause FromClause ;
-- [x]    export ExportClause ;
-- [x]    export VariableStatement
-- [ ]    export Declaration
-- [ ]    Declaration :
-- [ ]       HoistableDeclaration
-- [x]       ClassDeclaration
-- [x]       LexicalDeclaration
-- [ ]    HoistableDeclaration :
-- [x]       FunctionDeclaration
-- [x]       GeneratorDeclaration
-- [ ]       AsyncFunctionDeclaration
-- [ ]       AsyncGeneratorDeclaration
-- [ ]    export default HoistableDeclaration[Default]
-- [ ]    export default ClassDeclaration[Default]
-- [ ]    export default [lookahead ∉ { function, class }] AssignmentExpression[In] ;
ExportDeclaration :: { AST.JSExportDeclaration }
ExportDeclaration : Mul FromClause AutoSemi
                         { AST.JSExportAllFrom $1 $2 $3  {- 'ExportDeclarationStar' -} }
                  | Mul As Identifier FromClause AutoSemi
                         { AST.JSExportAllAsFrom $1 $2 (identName $3) $4 $5  {- 'ExportDeclarationStarAs' -} }
                  | ExportClause FromClause AutoSemi
                         { AST.JSExportFrom $1 $2 $3  {- 'ExportDeclaration1' -} }
                  | ExportClause AutoSemi
                         { AST.JSExportLocals $1 $2   {- 'ExportDeclaration2' -} }
                  | VariableStatement AutoSemi
                         { AST.JSExport $1 $2         {- 'ExportDeclaration3' -} }
                  | FunctionDeclaration AutoSemi
                         { AST.JSExport $1 $2         {- 'ExportDeclaration4' -} }
                  | GeneratorDeclaration AutoSemi
                         { AST.JSExport $1 $2         {- 'ExportDeclaration5' -} }
                  | ClassDeclaration AutoSemi
                         { AST.JSExport $1 $2         {- 'ExportDeclaration6' -} }

-- ExportClause :
--           { }
--           { ExportsList }
--           { ExportsList , }
ExportClause :: { AST.JSExportClause }
ExportClause : LBrace RBrace
                    { AST.JSExportClause $1 AST.JSLNil $2     {- 'ExportClause1' -} }
             | LBrace ExportsList RBrace
                    { AST.JSExportClause $1 $2 $3             {- 'ExportClause2' -} }

-- ExportsList :
--           ExportSpecifier
--           ExportsList , ExportSpecifier
ExportsList :: { AST.JSCommaList AST.JSExportSpecifier }
ExportsList : ExportSpecifier
                    { AST.JSLOne $1          {- 'ExportsList1' -} }
            | ExportsList Comma ExportSpecifier
                    { AST.JSLCons $1 $2 $3   {- 'ExportsList2' -} }

-- ExportSpecifier :
--           IdentifierName
--           IdentifierName as IdentifierName
ExportSpecifier :: { AST.JSExportSpecifier }
ExportSpecifier : IdentifierName
                    { AST.JSExportSpecifier (identName $1)                      {- 'ExportSpecifier1' -} }
                | IdentifierName As IdentifierName
                    { AST.JSExportSpecifierAs (identName $1) $2 (identName $3)  {- 'ExportSpecifier2' -} }

-- For debugging/other entry points
LiteralMain :: { AST.JSAST }
LiteralMain : Literal Eof			{ AST.JSAstLiteral $1 $2	{- 'LiteralMain' -} }

ExpressionMain :: { AST.JSAST }
ExpressionMain : Expression Eof					{ AST.JSAstExpression $1 $2 {- 'ExpressionMain' -} }

StatementMain :: { AST.JSAST }
StatementMain : StatementNoEmpty Eof	{ AST.JSAstStatement $1 $2   	{- 'StatementMain' -} }

{

-- Need this type while build the AST, but is not actually part of the AST.
data JSArguments = JSArguments AST.JSAnnot (AST.JSCommaList AST.JSExpression) AST.JSAnnot    -- lb, args, rb
data JSUntaggedTemplate = JSUntaggedTemplate !AST.JSAnnot !String ![AST.JSTemplatePart] -- lquot, head, parts

blockToStatement :: AST.JSBlock -> AST.JSSemi -> AST.JSStatement
blockToStatement (AST.JSBlock a b c) s = AST.JSStatementBlock a b c s

expressionToStatement :: AST.JSExpression -> AST.JSSemi -> AST.JSStatement
expressionToStatement (AST.JSFunctionExpression a b@(AST.JSIdentName{}) c d e f) s = AST.JSFunction a b c d e f s
expressionToStatement (AST.JSAsyncFunctionExpression a fn b@(AST.JSIdentName{}) c d e f) s = AST.JSAsyncFunction a fn b c d e f s
expressionToStatement (AST.JSGeneratorExpression a b c@(AST.JSIdentName{}) d e f g) s = AST.JSGenerator a b c d e f g s
expressionToStatement (AST.JSAssignExpression lhs op rhs) s = AST.JSAssignStatement lhs op rhs s
expressionToStatement (AST.JSMemberExpression e l a r) s = AST.JSMethodCall e l a r s
expressionToStatement (AST.JSClassExpression a b@(AST.JSIdentName{}) c d e f) s = AST.JSClass a b c d e f s
expressionToStatement exp s = AST.JSExpressionStatement exp s

expressionToAsyncFunction :: AST.JSAnnot -> AST.JSExpression -> AST.JSSemi -> AST.JSStatement
expressionToAsyncFunction aa (AST.JSFunctionExpression a b@(AST.JSIdentName{}) c d e f) s = AST.JSAsyncFunction aa a b c d e f s
expressionToAsyncFunction _aa (AST.JSAsyncFunctionExpression a fn b@(AST.JSIdentName{}) c d e f) s = AST.JSAsyncFunction a fn b c d e f s
expressionToAsyncFunction _aa _exp _s = error "Bad async function."

mkJSCallExpression :: AST.JSExpression -> JSArguments -> AST.JSExpression
mkJSCallExpression e (JSArguments l arglist r) = AST.JSCallExpression e l arglist r

mkJSMemberExpression :: AST.JSExpression -> JSArguments -> AST.JSExpression
mkJSMemberExpression e (JSArguments l arglist r) = AST.JSMemberExpression e l arglist r

mkJSMemberNew :: AST.JSAnnot -> AST.JSExpression -> JSArguments -> AST.JSExpression
mkJSMemberNew a e (JSArguments l arglist r) = AST.JSMemberNew a e l arglist r

mkJSOptionalCallExpression :: AST.JSExpression -> AST.JSAnnot -> JSArguments -> AST.JSExpression
mkJSOptionalCallExpression e annot (JSArguments l arglist r) = AST.JSOptionalCallExpression e annot arglist r


parseError :: Token -> Alex a
parseError = alexError . show

mkJSAnnot :: Token -> AST.JSAnnot
mkJSAnnot a = AST.JSAnnot (tokenSpan a) (tokenComment a)

mkJSTemplateLiteral :: Maybe AST.JSExpression -> JSUntaggedTemplate -> AST.JSExpression
mkJSTemplateLiteral tag (JSUntaggedTemplate a h ps) = AST.JSTemplateLiteral tag a h ps

-- ---------------------------------------------------------------------
-- | mkUnary : The parser detects '+' and '-' as the binary version of these
-- operator. This function converts from the binary version to the unary
-- version.
mkUnary :: AST.JSBinOp -> AST.JSUnaryOp
mkUnary (AST.JSBinOpMinus annot) = AST.JSUnaryOpMinus annot
mkUnary (AST.JSBinOpPlus  annot) = AST.JSUnaryOpPlus  annot

mkUnary x = error $ "Invalid unary op : " ++ show x

identName :: AST.JSExpression -> AST.JSIdent
identName (AST.JSIdentifier a s) = AST.JSIdentName a s
identName x = error $ "Cannot convert '" ++ show x ++ "' to a JSIdentName."

extractPrivateName :: Token -> String
extractPrivateName token = drop 1 (tokenLiteral token)  -- Remove the '#' prefix

propName :: AST.JSExpression ->  AST.JSPropertyName
propName (AST.JSIdentifier a s) = AST.JSPropertyIdent a s
propName (AST.JSDecimal a s) = AST.JSPropertyNumber a s
propName (AST.JSHexInteger a s) = AST.JSPropertyNumber a s
propName (AST.JSBinaryInteger a s) = AST.JSPropertyNumber a s
propName (AST.JSOctal a s) = AST.JSPropertyNumber a s
propName (AST.JSStringLiteral a s) = AST.JSPropertyString a s
propName x = error $ "Cannot convert '" ++ show x ++ "' to a JSPropertyName."

identifierToProperty :: AST.JSExpression -> AST.JSObjectProperty
identifierToProperty (AST.JSIdentifier a s) = AST.JSPropertyIdentRef a s
identifierToProperty x = error $ "Cannot convert '" ++ show x ++ "' to a JSObjectProperty."

spreadExpressionToProperty :: AST.JSExpression -> AST.JSObjectProperty
spreadExpressionToProperty (AST.JSSpreadExpression a expr) = AST.JSObjectSpread a expr
spreadExpressionToProperty x = error $ "Cannot convert '" ++ show x ++ "' to a JSObjectSpread."

toArrowParameterList :: AST.JSExpression -> Token -> Alex AST.JSArrowParameterList
toArrowParameterList (AST.JSIdentifier a s)          = const . return $ AST.JSUnparenthesizedArrowParameter (AST.JSIdentName a s)
toArrowParameterList (AST.JSExpressionParen lb x rb) = const . return $ AST.JSParenthesizedArrowParameterList lb (commasToCommaList x) rb
toArrowParameterList _                               = parseError

commasToCommaList :: AST.JSExpression -> AST.JSCommaList AST.JSExpression
commasToCommaList (AST.JSCommaExpression l c r) = AST.JSLCons (commasToCommaList l) c r
commasToCommaList x = AST.JSLOne x


}
