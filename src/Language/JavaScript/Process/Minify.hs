{-# LANGUAGE CPP, FlexibleInstances #-}

module Language.JavaScript.Process.Minify
    ( -- * Minify
      minifyJS
    ) where

#if ! MIN_VERSION_base(4,13,0)
import Control.Applicative ((<$>))
#endif

import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token

-- ---------------------------------------------------------------------

minifyJS :: JSAST -> JSAST
minifyJS (JSAstProgram xs _) = JSAstProgram (fixStatementList noSemi xs) emptyAnnot
minifyJS (JSAstModule xs _) = JSAstModule (map (fix emptyAnnot) xs) emptyAnnot
minifyJS (JSAstStatement (JSStatementBlock _ [s] _ _) _) = JSAstStatement (fixStmtE noSemi s) emptyAnnot
minifyJS (JSAstStatement s _) = JSAstStatement (fixStmtE noSemi s) emptyAnnot
minifyJS (JSAstExpression e _) =  JSAstExpression (fixEmpty e) emptyAnnot
minifyJS (JSAstLiteral s _)  = JSAstLiteral (fixEmpty s) emptyAnnot

-- ---------------------------------------------------------------------

class MinifyJS a where
    fix :: JSAnnot -> a -> a


fixEmpty :: MinifyJS a => a -> a
fixEmpty = fix emptyAnnot

fixSpace :: MinifyJS a => a -> a
fixSpace = fix spaceAnnot

-- -----------------------------------------------------------------------------
-- During minification, Javascript statements may need to have explicit
-- semicolons inserted between them, so that simply adding a JSStatement
-- instance for the MinifyJS typeclass would not be sufficient.

fixStmt :: JSAnnot -> JSSemi -> JSStatement -> JSStatement
fixStmt a s (JSStatementBlock _lb ss _rb _) = fixStatementBlock a s ss
fixStmt a s (JSBreak _ i _) = JSBreak a (fixSpace i) s
fixStmt a s (JSClass _ n h _ ms _ _) = JSClass a (fixSpace n) (fixSpace h) emptyAnnot (fixEmpty ms) emptyAnnot s
fixStmt a s (JSConstant _ ss _) = JSConstant a (fixVarList ss) s
fixStmt a s (JSContinue _ i _) = JSContinue a (fixSpace i) s
fixStmt a s (JSDoWhile _ st _ _ e _ _) = JSDoWhile a (mkStatementBlock noSemi st) emptyAnnot emptyAnnot (fixEmpty e) emptyAnnot s
fixStmt a s (JSFor _ _ el1 _ el2 _ el3 _ st) = JSFor a emptyAnnot (fixEmpty el1) emptyAnnot (fixEmpty el2) emptyAnnot (fixEmpty el3) emptyAnnot (fixStmtE s st)
fixStmt a s (JSForIn _ _ e1 op e2 _ st) = JSForIn a emptyAnnot (fixEmpty e1) (fixSpace op) (fixSpace e2) emptyAnnot (fixStmtE s st)
fixStmt a s (JSForVar _ _ _ el1 _ el2 _ el3 _ st) = JSForVar a emptyAnnot spaceAnnot (fixEmpty el1) emptyAnnot (fixEmpty el2) emptyAnnot (fixEmpty el3) emptyAnnot (fixStmtE s st)
fixStmt a s (JSForVarIn _ _ _ e1 op e2 _ st) = JSForVarIn a emptyAnnot spaceAnnot (fixEmpty e1) (fixSpace op) (fixSpace e2) emptyAnnot (fixStmtE s st)
fixStmt a s (JSForLet _ _ _ el1 _ el2 _ el3 _ st) = JSForLet a emptyAnnot spaceAnnot (fixEmpty el1) emptyAnnot (fixEmpty el2) emptyAnnot (fixEmpty el3) emptyAnnot (fixStmtE s st)
fixStmt a s (JSForLetIn _ _ _ e1 op e2 _ st) = JSForLetIn a emptyAnnot spaceAnnot (fixEmpty e1) (fixSpace op) (fixSpace e2) emptyAnnot (fixStmtE s st)
fixStmt a s (JSForLetOf _ _ _ e1 op e2 _ st) = JSForLetOf a emptyAnnot spaceAnnot (fixEmpty e1) (fixSpace op) (fixSpace e2) emptyAnnot (fixStmtE s st)
fixStmt a s (JSForConst _ _ _ el1 _ el2 _ el3 _ st) = JSForConst a emptyAnnot spaceAnnot (fixEmpty el1) emptyAnnot (fixEmpty el2) emptyAnnot (fixEmpty el3) emptyAnnot (fixStmtE s st)
fixStmt a s (JSForConstIn _ _ _ e1 op e2 _ st) = JSForConstIn a emptyAnnot spaceAnnot (fixEmpty e1) (fixSpace op) (fixSpace e2) emptyAnnot (fixStmtE s st)
fixStmt a s (JSForConstOf _ _ _ e1 op e2 _ st) = JSForConstOf a emptyAnnot spaceAnnot (fixEmpty e1) (fixSpace op) (fixSpace e2) emptyAnnot (fixStmtE s st)
fixStmt a s (JSForOf _ _ e1 op e2 _ st) = JSForOf a emptyAnnot (fixEmpty e1) (fixSpace op) (fixSpace e2) emptyAnnot (fixStmtE s st)
fixStmt a s (JSForVarOf _ _ _ e1 op e2 _ st) = JSForVarOf a emptyAnnot spaceAnnot (fixEmpty e1) (fixSpace op) (fixSpace e2) emptyAnnot (fixStmtE s st)
fixStmt a s (JSAsyncFunction _ _ n _ ps _ blk _) = JSAsyncFunction a spaceAnnot (fixSpace n) emptyAnnot (fixEmpty ps) emptyAnnot (fixEmpty blk) s
fixStmt a s (JSFunction _ n _ ps _ blk _) = JSFunction a (fixSpace n) emptyAnnot (fixEmpty ps) emptyAnnot (fixEmpty blk) s
fixStmt a s (JSGenerator _ _ n _ ps _ blk _) = JSGenerator a emptyAnnot (fixEmpty n) emptyAnnot (fixEmpty ps) emptyAnnot (fixEmpty blk) s
fixStmt a s (JSIf _ _ e _ st) = JSIf a emptyAnnot (fixEmpty e) emptyAnnot (fixIfElseBlock emptyAnnot s st)
fixStmt a s (JSIfElse _ _ e _ (JSEmptyStatement _) _ sf) = JSIfElse a emptyAnnot (fixEmpty e) emptyAnnot (JSEmptyStatement emptyAnnot) emptyAnnot (fixStmt spaceAnnot s sf)
fixStmt a s (JSIfElse _ _ e _ st _ sf) = JSIfElse a emptyAnnot (fixEmpty e) emptyAnnot (mkStatementBlock noSemi st) emptyAnnot (fixIfElseBlock spaceAnnot s sf)
fixStmt a s (JSLabelled e _ st) = JSLabelled (fix a e) emptyAnnot (fixStmtE s st)
fixStmt a s (JSLet _ xs _) = JSLet a (fixVarList xs) s
fixStmt _ _ (JSEmptyStatement _) = JSEmptyStatement emptyAnnot
fixStmt a s (JSExpressionStatement e _) = JSExpressionStatement (fix a e) s
fixStmt a s (JSAssignStatement lhs op rhs _) = JSAssignStatement (fix a lhs) (fixEmpty op) (fixEmpty rhs) s
fixStmt a s (JSMethodCall e _ args _ _) = JSMethodCall (fix a e) emptyAnnot (fixEmpty args) emptyAnnot s
fixStmt a s (JSReturn _ me _) = JSReturn a (fixSpace me) s
fixStmt a s (JSSwitch _ _ e _ _ sps _ _) = JSSwitch a emptyAnnot (fixEmpty e) emptyAnnot emptyAnnot (fixSwitchParts sps) emptyAnnot s
fixStmt a s (JSThrow _ e _) = JSThrow a (fixSpace e) s
fixStmt a _ (JSTry _ b tc tf) = JSTry a (fixEmpty b) (map fixEmpty tc) (fixEmpty tf)
fixStmt a s (JSVariable _ ss _) = JSVariable a (fixVarList ss) s
fixStmt a s (JSWhile _ _ e _ st) = JSWhile a emptyAnnot (fixEmpty e) emptyAnnot (fixStmt a s st)
fixStmt a s (JSWith _ _ e _ st _) = JSWith a emptyAnnot (fixEmpty e) emptyAnnot (fixStmtE noSemi st) s


fixIfElseBlock :: JSAnnot -> JSSemi -> JSStatement -> JSStatement
fixIfElseBlock _ _ (JSStatementBlock _ [] _ _) = JSEmptyStatement emptyAnnot
fixIfElseBlock a s st = fixStmt a s st

fixStmtE :: JSSemi -> JSStatement -> JSStatement
fixStmtE = fixStmt emptyAnnot

-- Turn a single JSStatement into a JSStatementBlock.
mkStatementBlock :: JSSemi -> JSStatement -> JSStatement
mkStatementBlock s (JSStatementBlock _ blk _ _) = JSStatementBlock emptyAnnot (fixStatementList noSemi blk) emptyAnnot s
mkStatementBlock s x = JSStatementBlock emptyAnnot [fixStmtE noSemi x] emptyAnnot s

-- Filter a list of JSStatment, dropping JSEmptyStatement and empty
-- JSStatementBlocks. If the resulting list contains only a single element,
-- remove the enclosing JSStatementBlock and return the inner JSStatement.
fixStatementBlock :: JSAnnot -> JSSemi -> [JSStatement] -> JSStatement
fixStatementBlock a s ss =
    case filter (not . isEmpty) ss of
        [] -> JSStatementBlock emptyAnnot [] emptyAnnot s
        [sx] -> fixStmt a s sx
        sss -> JSStatementBlock emptyAnnot (fixStatementList noSemi sss) emptyAnnot s
  where
    isEmpty (JSEmptyStatement _) = True
    isEmpty (JSStatementBlock _ [] _ _) = True
    isEmpty _ = False

-- Force semi-colons between statements, and make sure the last statement in a
-- block has no semi-colon.
fixStatementList :: JSSemi -> [JSStatement] -> [JSStatement]
fixStatementList trailingSemi =
    fixList emptyAnnot trailingSemi . filter (not . isRedundant)
  where
    isRedundant (JSStatementBlock _ [] _ _) = True
    isRedundant (JSEmptyStatement _) = True
    isRedundant _ = False

    fixList _ _ [] = []
    fixList a s [JSStatementBlock _ blk _ _] = fixList a s blk
    fixList a s [x] = [fixStmt a s x]
    fixList _ s (JSStatementBlock _ blk _ _:xs) = fixList emptyAnnot semi (filter (not . isRedundant) blk) ++ fixList emptyAnnot s xs
    fixList a s (JSConstant _ vs1 _:JSConstant _ vs2 _: xs) = fixList a s (JSConstant spaceAnnot (concatCommaList vs1 vs2) s : xs)
    fixList a s (JSVariable _ vs1 _:JSVariable _ vs2 _: xs) = fixList a s (JSVariable spaceAnnot (concatCommaList vs1 vs2) s : xs)
    fixList a s (x1@JSFunction{}:x2@JSFunction{}:xs) = fixStmt a noSemi x1 : fixList newlineAnnot s (x2:xs)
    fixList a s (x:xs) = fixStmt a semi x : fixList emptyAnnot s xs

concatCommaList :: JSCommaList a -> JSCommaList a -> JSCommaList a
concatCommaList xs JSLNil = xs
concatCommaList JSLNil ys = ys
concatCommaList xs (JSLOne y) = JSLCons xs emptyAnnot y
concatCommaList xs ys =
    let recurse (z, zs) = concatCommaList (JSLCons xs emptyAnnot z) zs
    in  maybe xs recurse $ headCommaList ys

headCommaList :: JSCommaList a -> Maybe (a, JSCommaList a)
headCommaList JSLNil = Nothing
headCommaList (JSLOne x) = Just (x, JSLNil)
headCommaList (JSLCons (JSLOne x) _ y) = Just (x, JSLOne y)
headCommaList (JSLCons xs _ y) =
    let rebuild (x, ys) = (x, JSLCons ys emptyAnnot y)
    in  rebuild <$> headCommaList xs

-- -----------------------------------------------------------------------------
-- JSExpression and the rest can use the MinifyJS typeclass.

instance MinifyJS JSExpression where
    -- Terminals
    fix a (JSIdentifier     _ s) = JSIdentifier a s
    fix a (JSDecimal        _ s) = JSDecimal a s
    fix a (JSLiteral        _ s) = JSLiteral a s
    fix a (JSHexInteger     _ s) = JSHexInteger a s
    fix a (JSBinaryInteger  _ s) = JSBinaryInteger a s
    fix a (JSOctal          _ s) = JSOctal a s
    fix _ (JSStringLiteral  _ s) = JSStringLiteral emptyAnnot s
    fix _ (JSRegEx          _ s) = JSRegEx emptyAnnot s

    -- Non-Terminals
    fix _ (JSArrayLiteral         _ xs _)             = JSArrayLiteral emptyAnnot (map fixEmpty xs) emptyAnnot
    fix a (JSArrowExpression ps _ body)               = JSArrowExpression (fix a ps) emptyAnnot (fix a body)
    fix a (JSAssignExpression     lhs op rhs)         = JSAssignExpression (fix a lhs) (fixEmpty op) (fixEmpty rhs)
    fix a (JSAwaitExpression      _ ex)               = JSAwaitExpression a (fixSpace ex)
    fix a (JSCallExpression       ex _ xs _)          = JSCallExpression (fix a ex) emptyAnnot (fixEmpty xs) emptyAnnot
    fix a (JSCallExpressionDot    ex _ xs)            = JSCallExpressionDot (fix a ex) emptyAnnot (fixEmpty xs)
    fix a (JSCallExpressionSquare ex _ xs _)          = JSCallExpressionSquare (fix a ex) emptyAnnot (fixEmpty xs) emptyAnnot
    fix a (JSClassExpression      _ n h _ ms _)       = JSClassExpression a (fixSpace n) (fixSpace h) emptyAnnot (fixEmpty ms) emptyAnnot
    fix a (JSCommaExpression      le _ re)            = JSCommaExpression (fix a le) emptyAnnot (fixEmpty re)
    fix a (JSExpressionBinary     lhs op rhs)         = fixBinOpExpression a op lhs rhs
    fix _ (JSExpressionParen      _ e _)              = JSExpressionParen emptyAnnot (fixEmpty e) emptyAnnot
    fix a (JSExpressionPostfix    e op)               = JSExpressionPostfix (fix a e) (fixEmpty op)
    fix a (JSExpressionTernary    cond _ v1 _ v2)     = JSExpressionTernary (fix a cond) emptyAnnot (fixEmpty v1) emptyAnnot (fixEmpty v2)
    fix a (JSFunctionExpression   _ n _ x2s _ x3)     = JSFunctionExpression a (fixSpace n) emptyAnnot (fixEmpty x2s) emptyAnnot (fixEmpty x3)
    fix a (JSAsyncFunctionExpression _ _ n _ x2s _ x3) = JSAsyncFunctionExpression a emptyAnnot (fixSpace n) emptyAnnot (fixEmpty x2s) emptyAnnot (fixEmpty x3)
    fix a (JSGeneratorExpression  _ _ n _ x2s _ x3)   = JSGeneratorExpression a emptyAnnot (fixEmpty n) emptyAnnot (fixEmpty x2s) emptyAnnot (fixEmpty x3)
    fix a (JSMemberDot            xs _ n)             = JSMemberDot (fix a xs) emptyAnnot (fixEmpty n)
    fix a (JSMemberExpression     e _ args _)         = JSMemberExpression (fix a e) emptyAnnot (fixEmpty args) emptyAnnot
    fix a (JSMemberNew            _ n _ s _)          = JSMemberNew a (fix spaceAnnot n) emptyAnnot (fixEmpty s) emptyAnnot
    fix a (JSMemberSquare         xs _ e _)           = JSMemberSquare (fix a xs) emptyAnnot (fixEmpty e) emptyAnnot
    fix a (JSNewExpression        _ e)                = JSNewExpression a (fixSpace e)
    fix _ (JSObjectLiteral        _ xs _)             = JSObjectLiteral emptyAnnot (fixEmpty xs) emptyAnnot
    fix a (JSTemplateLiteral      t _ s ps)           = JSTemplateLiteral (fmap (fix a) t) emptyAnnot s (map fixEmpty ps)
    fix a (JSUnaryExpression      op x)               = let (ta, fop) = fixUnaryOp a op in JSUnaryExpression fop (fix ta x)
    fix a (JSVarInitExpression    x1 x2)              = JSVarInitExpression (fix a x1) (fixEmpty x2)
    fix a (JSYieldExpression      _ x)                = JSYieldExpression a (fixSpace x)
    fix a (JSYieldFromExpression  _ _ x)              = JSYieldFromExpression a emptyAnnot (fixEmpty x)
    fix a (JSImportMeta _ _)                          = JSImportMeta a emptyAnnot
    fix a (JSSpreadExpression     _ e)                = JSSpreadExpression a (fixEmpty e)
    fix a (JSBigIntLiteral        _ s)                = JSBigIntLiteral a s
    fix a (JSOptionalMemberDot    e _ p)              = JSOptionalMemberDot (fix a e) emptyAnnot (fixEmpty p)
    fix a (JSOptionalMemberSquare e _ p _)            = JSOptionalMemberSquare (fix a e) emptyAnnot (fixEmpty p) emptyAnnot
    fix a (JSOptionalCallExpression e _ args _)       = JSOptionalCallExpression (fix a e) emptyAnnot (fixEmpty args) emptyAnnot

instance MinifyJS JSArrowParameterList where
    fix _ (JSUnparenthesizedArrowParameter p)         = JSUnparenthesizedArrowParameter (fixEmpty p)
    fix _ (JSParenthesizedArrowParameterList _ ps _)  = JSParenthesizedArrowParameterList emptyAnnot (fixEmpty ps) emptyAnnot

instance MinifyJS JSConciseBody where
    fix _ (JSConciseFunctionBody (JSBlock _ [JSExpressionStatement expr _] _)) = JSConciseExpressionBody (fixEmpty expr)
    fix _ (JSConciseFunctionBody block)      = JSConciseFunctionBody (fixEmpty block)
    fix a (JSConciseExpressionBody expr)     = JSConciseExpressionBody (fix a expr)

fixVarList :: JSCommaList JSExpression -> JSCommaList JSExpression
fixVarList (JSLCons h _ v) = JSLCons (fixVarList h) emptyAnnot (fixEmpty v)
fixVarList (JSLOne a) = JSLOne (fixSpace a)
fixVarList JSLNil = JSLNil

fixBinOpExpression :: JSAnnot -> JSBinOp -> JSExpression -> JSExpression -> JSExpression
fixBinOpExpression a (JSBinOpPlus _) lhs rhs = fixBinOpPlus a lhs rhs
fixBinOpExpression a (JSBinOpIn _) lhs rhs = JSExpressionBinary (fix a lhs) (JSBinOpIn spaceAnnot) (fix spaceAnnot rhs)
fixBinOpExpression a (JSBinOpInstanceOf _) lhs rhs = JSExpressionBinary (fix a lhs) (JSBinOpInstanceOf spaceAnnot) (fix spaceAnnot rhs)
fixBinOpExpression a op lhs rhs = JSExpressionBinary (fix a lhs) (fixEmpty op) (fixEmpty rhs)

fixBinOpPlus :: JSAnnot -> JSExpression -> JSExpression -> JSExpression
fixBinOpPlus a lhs rhs =
    case (fix a lhs, fixEmpty rhs) of
        (JSStringLiteral _ s1, JSStringLiteral _ s2) -> stringLitConcat (normalizeToSQ s1) (normalizeToSQ s2)
        (nlhs, nrhs) -> JSExpressionBinary nlhs (JSBinOpPlus emptyAnnot) nrhs

-- Concatenate two JSStringLiterals. Since the strings will include the string
-- terminators (either single or double quotes) we use whatever terminator is
-- used by the first string.
stringLitConcat :: String -> String -> JSExpression
stringLitConcat xs ys | null xs = JSStringLiteral emptyAnnot ys
stringLitConcat xs ys | null ys = JSStringLiteral emptyAnnot xs
stringLitConcat xall yall =
    case yall of
      [] -> JSStringLiteral emptyAnnot xall
      (_:yss) -> JSStringLiteral emptyAnnot (init xall ++ init yss ++ "'")

-- Normalize a String. If its single quoted, just return it and its double quoted
-- convert it to single quoted.
normalizeToSQ :: String -> String
normalizeToSQ str =
    case str of
        [] -> []
        ('\'' : _) -> str
        ('"' : xs) -> '\'' : convertSQ xs
        _ -> str -- Should not happen.
  where
    convertSQ [] = []
    convertSQ [c] = "'"
    convertSQ (c:rest) = case c of
        '\'' -> "\\'" ++ convertSQ rest
        '\\' -> case rest of
            ('"':rest') -> '"' : convertSQ rest'
            _ -> c : convertSQ rest
        _ -> c : convertSQ rest


instance MinifyJS JSBinOp where
    fix _ (JSBinOpAnd        _) = JSBinOpAnd emptyAnnot
    fix _ (JSBinOpBitAnd     _) = JSBinOpBitAnd emptyAnnot
    fix _ (JSBinOpBitOr      _) = JSBinOpBitOr emptyAnnot
    fix _ (JSBinOpBitXor     _) = JSBinOpBitXor emptyAnnot
    fix _ (JSBinOpDivide     _) = JSBinOpDivide emptyAnnot
    fix _ (JSBinOpEq         _) = JSBinOpEq emptyAnnot
    fix _ (JSBinOpExponentiation _) = JSBinOpExponentiation emptyAnnot
    fix _ (JSBinOpGe         _) = JSBinOpGe emptyAnnot
    fix _ (JSBinOpGt         _) = JSBinOpGt emptyAnnot
    fix a (JSBinOpIn         _) = JSBinOpIn a
    fix a (JSBinOpInstanceOf _) = JSBinOpInstanceOf a
    fix _ (JSBinOpLe         _) = JSBinOpLe emptyAnnot
    fix _ (JSBinOpLsh        _) = JSBinOpLsh emptyAnnot
    fix _ (JSBinOpLt         _) = JSBinOpLt emptyAnnot
    fix _ (JSBinOpMinus      _) = JSBinOpMinus emptyAnnot
    fix _ (JSBinOpMod        _) = JSBinOpMod emptyAnnot
    fix _ (JSBinOpNeq        _) = JSBinOpNeq emptyAnnot
    fix a (JSBinOpOf         _) = JSBinOpOf a
    fix _ (JSBinOpOr         _) = JSBinOpOr emptyAnnot
    fix _ (JSBinOpPlus       _) = JSBinOpPlus emptyAnnot
    fix _ (JSBinOpRsh        _) = JSBinOpRsh emptyAnnot
    fix _ (JSBinOpStrictEq   _) = JSBinOpStrictEq emptyAnnot
    fix _ (JSBinOpStrictNeq  _) = JSBinOpStrictNeq emptyAnnot
    fix _ (JSBinOpTimes      _) = JSBinOpTimes emptyAnnot
    fix _ (JSBinOpUrsh       _) = JSBinOpUrsh emptyAnnot
    fix _ (JSBinOpNullishCoalescing _) = JSBinOpNullishCoalescing emptyAnnot


instance MinifyJS JSUnaryOp where
    fix _ (JSUnaryOpDecr   _) = JSUnaryOpDecr emptyAnnot
    fix _ (JSUnaryOpDelete _) = JSUnaryOpDelete emptyAnnot
    fix _ (JSUnaryOpIncr   _) = JSUnaryOpIncr emptyAnnot
    fix _ (JSUnaryOpMinus  _) = JSUnaryOpMinus emptyAnnot
    fix _ (JSUnaryOpNot    _) = JSUnaryOpNot emptyAnnot
    fix _ (JSUnaryOpPlus   _) = JSUnaryOpPlus emptyAnnot
    fix _ (JSUnaryOpTilde  _) = JSUnaryOpTilde emptyAnnot
    fix _ (JSUnaryOpTypeof _) = JSUnaryOpTypeof emptyAnnot
    fix _ (JSUnaryOpVoid   _) = JSUnaryOpVoid emptyAnnot

fixUnaryOp :: JSAnnot -> JSUnaryOp -> (JSAnnot, JSUnaryOp)
fixUnaryOp a (JSUnaryOpDelete _) = (spaceAnnot, JSUnaryOpDelete a)
fixUnaryOp a (JSUnaryOpTypeof _) = (spaceAnnot, JSUnaryOpTypeof a)
fixUnaryOp a (JSUnaryOpVoid   _) = (spaceAnnot, JSUnaryOpVoid a)
fixUnaryOp a x = (emptyAnnot, fix a x)


instance MinifyJS JSAssignOp where
    fix a (JSAssign       _) = JSAssign a
    fix a (JSTimesAssign  _) = JSTimesAssign a
    fix a (JSDivideAssign _) = JSDivideAssign a
    fix a (JSModAssign    _) = JSModAssign a
    fix a (JSPlusAssign   _) = JSPlusAssign a
    fix a (JSMinusAssign  _) = JSMinusAssign a
    fix a (JSLshAssign    _) = JSLshAssign a
    fix a (JSRshAssign    _) = JSRshAssign a
    fix a (JSUrshAssign   _) = JSUrshAssign a
    fix a (JSBwAndAssign  _) = JSBwAndAssign a
    fix a (JSBwXorAssign  _) = JSBwXorAssign a
    fix a (JSBwOrAssign   _) = JSBwOrAssign a
    fix a (JSLogicalAndAssign _) = JSLogicalAndAssign a
    fix a (JSLogicalOrAssign  _) = JSLogicalOrAssign a
    fix a (JSNullishAssign    _) = JSNullishAssign a

instance MinifyJS JSModuleItem where
    fix _ (JSModuleImportDeclaration _ x1) = JSModuleImportDeclaration emptyAnnot (fixEmpty x1)
    fix _ (JSModuleExportDeclaration _ x1) = JSModuleExportDeclaration emptyAnnot (fixEmpty x1)
    fix a (JSModuleStatementListItem s) = JSModuleStatementListItem (fixStmt a noSemi s)

instance MinifyJS JSImportDeclaration where
    fix _ (JSImportDeclaration imps from attrs _) = JSImportDeclaration (fixEmpty imps) (fix annot from) (fixEmpty attrs) noSemi
        where
        annot = case imps of
                    JSImportClauseDefault {} -> spaceAnnot
                    JSImportClauseNameSpace {} -> spaceAnnot
                    JSImportClauseNamed {} -> emptyAnnot
                    JSImportClauseDefaultNameSpace {} -> spaceAnnot
                    JSImportClauseDefaultNamed {} -> emptyAnnot
    fix a (JSImportDeclarationBare _ m attrs _) = JSImportDeclarationBare a m (fixEmpty attrs) noSemi

instance MinifyJS JSImportClause where
    fix _ (JSImportClauseDefault n) = JSImportClauseDefault (fixSpace n)
    fix _ (JSImportClauseNameSpace ns) = JSImportClauseNameSpace (fixSpace ns)
    fix _ (JSImportClauseNamed named) = JSImportClauseNamed (fixEmpty named)
    fix _ (JSImportClauseDefaultNameSpace def _ ns) = JSImportClauseDefaultNameSpace (fixSpace def) emptyAnnot (fixEmpty ns)
    fix _ (JSImportClauseDefaultNamed def _ ns) = JSImportClauseDefaultNamed (fixSpace def) emptyAnnot (fixEmpty ns)

instance MinifyJS JSFromClause where
    fix a (JSFromClause _ _ m) = JSFromClause a emptyAnnot m

instance MinifyJS JSImportNameSpace where
    fix a (JSImportNameSpace _ _ ident) = JSImportNameSpace (JSBinOpTimes a) spaceAnnot (fixSpace ident)

instance MinifyJS JSImportsNamed where
    fix _ (JSImportsNamed _ imps _) = JSImportsNamed emptyAnnot (fixEmpty imps) emptyAnnot

instance MinifyJS JSImportSpecifier where
    fix _ (JSImportSpecifier x1) = JSImportSpecifier (fixEmpty x1)
    fix _ (JSImportSpecifierAs x1 _ x2) = JSImportSpecifierAs (fixEmpty x1) spaceAnnot (fixSpace x2)

instance MinifyJS (Maybe JSImportAttributes) where
    fix _ Nothing = Nothing
    fix _ (Just attrs) = Just (fixEmpty attrs)

instance MinifyJS JSImportAttributes where
    fix _ (JSImportAttributes _ attrs _) = JSImportAttributes emptyAnnot (fixEmpty attrs) emptyAnnot

instance MinifyJS JSImportAttribute where
    fix _ (JSImportAttribute key _ value) = JSImportAttribute (fixEmpty key) emptyAnnot (fixEmpty value)

instance MinifyJS JSExportDeclaration where
    fix a (JSExportAllFrom star from _) = JSExportAllFrom (fix a star) (fix a from) noSemi
    fix a (JSExportAllAsFrom star _as ident from _) = JSExportAllAsFrom (fix a star) emptyAnnot (fix a ident) (fix a from) noSemi
    fix a (JSExportFrom x1 from _) = JSExportFrom (fix a x1) (fix a from) noSemi
    fix _ (JSExportLocals x1 _) = JSExportLocals (fix emptyAnnot x1) noSemi
    fix _ (JSExport x1 _) = JSExport (fixStmt spaceAnnot noSemi x1) noSemi

instance MinifyJS JSExportClause where
    fix a (JSExportClause _ x1 _) = JSExportClause emptyAnnot (fixEmpty x1) a

instance MinifyJS JSExportSpecifier where
    fix _ (JSExportSpecifier x1) = JSExportSpecifier (fixEmpty x1)
    fix _ (JSExportSpecifierAs x1 _ x2) = JSExportSpecifierAs (fixEmpty x1) spaceAnnot (fixSpace x2)

instance MinifyJS JSTryCatch where
    fix a (JSCatch _ _ x1 _ x3) = JSCatch a emptyAnnot (fixEmpty x1) emptyAnnot (fixEmpty x3)
    fix a (JSCatchIf _ _ x1 _ ex _ x3) = JSCatchIf a emptyAnnot (fixEmpty x1) spaceAnnot (fixSpace ex) emptyAnnot (fixEmpty x3)


instance MinifyJS JSTryFinally where
    fix a (JSFinally _ x) = JSFinally a (fixEmpty x)
    fix _ JSNoFinally = JSNoFinally


fixSwitchParts :: [JSSwitchParts] -> [JSSwitchParts]
fixSwitchParts parts =
    case parts of
        [] -> []
        [x] -> [fixPart noSemi x]
        (x:xs) -> fixPart semi x : fixSwitchParts xs
  where
    fixPart s (JSCase _ e _ ss) = JSCase emptyAnnot (fixCase e) emptyAnnot (fixStatementList s ss)
    fixPart s (JSDefault _ _ ss) = JSDefault emptyAnnot emptyAnnot (fixStatementList s ss)

fixCase :: JSExpression -> JSExpression
fixCase (JSStringLiteral _ s) = JSStringLiteral emptyAnnot s
fixCase e = fix spaceAnnot e


instance MinifyJS JSBlock where
    fix _ (JSBlock _ ss _) = JSBlock emptyAnnot (fixStatementList noSemi ss) emptyAnnot


instance MinifyJS JSObjectProperty where
    fix a (JSPropertyNameandValue n _ vs)       = JSPropertyNameandValue (fix a n) emptyAnnot (map fixEmpty vs)
    fix a (JSPropertyIdentRef     _ s)          = JSPropertyIdentRef a s
    fix a (JSObjectMethod         m)            = JSObjectMethod (fix a m)
    fix a (JSObjectSpread         _ expr)       = JSObjectSpread a (fix emptyAnnot expr)

instance MinifyJS JSMethodDefinition where
    fix a (JSMethodDefinition          n _ ps _ b)   = JSMethodDefinition                     (fix a n)    emptyAnnot (fixEmpty ps) emptyAnnot (fixEmpty b)
    fix _ (JSGeneratorMethodDefinition _ n _ ps _ b) = JSGeneratorMethodDefinition emptyAnnot (fixEmpty n) emptyAnnot (fixEmpty ps) emptyAnnot (fixEmpty b)
    fix a (JSPropertyAccessor          s n _ ps _ b) = JSPropertyAccessor          (fix a s)  (fixSpace n) emptyAnnot (fixEmpty ps) emptyAnnot (fixEmpty b)

instance MinifyJS JSPropertyName where
    fix a (JSPropertyIdent _ s)  = JSPropertyIdent a s
    fix a (JSPropertyString _ s) = JSPropertyString a s
    fix a (JSPropertyNumber _ s) = JSPropertyNumber a s
    fix _ (JSPropertyComputed _ x _) = JSPropertyComputed emptyAnnot (fixEmpty x) emptyAnnot

instance MinifyJS JSAccessor where
    fix a (JSAccessorGet _) = JSAccessorGet a
    fix a (JSAccessorSet _) = JSAccessorSet a


instance MinifyJS JSArrayElement where
    fix _ (JSArrayElement e) = JSArrayElement (fixEmpty e)
    fix _ (JSArrayComma _)   = JSArrayComma emptyAnnot


instance MinifyJS a => MinifyJS (JSCommaList a) where
    fix _ (JSLCons xs _ x) = JSLCons (fixEmpty xs) emptyAnnot (fixEmpty x)
    fix _ (JSLOne a)       = JSLOne (fixEmpty a)
    fix _ JSLNil           = JSLNil


instance MinifyJS a => MinifyJS (JSCommaTrailingList a) where
    fix _ (JSCTLComma xs _) = JSCTLNone (fixEmpty xs)
    fix _ (JSCTLNone xs)    = JSCTLNone (fixEmpty xs)


instance MinifyJS JSIdent where
    fix a (JSIdentName _ n) = JSIdentName a n
    fix _ JSIdentNone = JSIdentNone


instance MinifyJS (Maybe JSExpression) where
    fix a me = fix a <$> me


instance MinifyJS JSVarInitializer where
    fix a (JSVarInit _ x) = JSVarInit a (fix emptyAnnot x)
    fix _ JSVarInitNone = JSVarInitNone


instance MinifyJS JSTemplatePart where
    fix _ (JSTemplatePart e _ s) = JSTemplatePart (fixEmpty e) emptyAnnot s


instance MinifyJS JSClassHeritage where
    fix _ JSExtendsNone = JSExtendsNone
    fix a (JSExtends _ e) = JSExtends a (fixSpace e)


instance MinifyJS [JSClassElement] where
    fix _ [] = []
    fix a (JSClassInstanceMethod m:t) = JSClassInstanceMethod (fix a m) : fixEmpty t
    fix a (JSClassStaticMethod _ m:t) = JSClassStaticMethod a (fixSpace m) : fixEmpty t
    fix a (JSClassSemi _:t) = fix a t
    fix a (JSPrivateField _ name _ Nothing _:t) = JSPrivateField a name a Nothing semi : fixEmpty t
    fix a (JSPrivateField _ name _ (Just initializer) _:t) = JSPrivateField a name a (Just (fixSpace initializer)) semi : fixEmpty t
    fix a (JSPrivateMethod _ name _ params _ block:t) = JSPrivateMethod a name a (fixEmpty params) a (fixSpace block) : fixEmpty t
    fix a (JSPrivateAccessor accessor _ name _ params _ block:t) = JSPrivateAccessor (fixSpace accessor) a name a (fixEmpty params) a (fixSpace block) : fixEmpty t


spaceAnnot :: JSAnnot
spaceAnnot = JSAnnot tokenPosnEmpty [WhiteSpace tokenPosnEmpty " "]

emptyAnnot :: JSAnnot
emptyAnnot = JSNoAnnot

newlineAnnot :: JSAnnot
newlineAnnot = JSAnnot tokenPosnEmpty [WhiteSpace tokenPosnEmpty "\n"]

semi :: JSSemi
semi = JSSemi emptyAnnot

noSemi :: JSSemi
noSemi = JSSemiAuto
