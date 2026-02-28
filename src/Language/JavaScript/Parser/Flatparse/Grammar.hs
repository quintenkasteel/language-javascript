{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Combined expression and statement grammar for the flatparse-based
-- JavaScript parser.
--
-- This module merges expression parsing (Pratt-style precedence climbing)
-- with statement and declaration parsing into a single module, eliminating
-- the circular dependency between expressions and statements. Expressions
-- need @statement@ for function bodies, arrow bodies, and class method
-- bodies, while statements need @expression@ for expression statements
-- and control-flow conditions.
--
-- Expression parsing features:
--   * All binary operators with correct precedence
--   * Unary prefix and postfix operators
--   * Assignment operators (15 forms)
--   * Ternary conditional
--   * Arrow functions
--   * Member access, calls, optional chaining
--   * Literals, identifiers, regex, template literals
--   * new, this, super, spread, await, yield
--   * Function, class, async, generator expressions
--
-- Statement parsing features:
--   * Basic statements (expression, block, empty)
--   * Variable declarations (var, let, const)
--   * Control flow (if\/else, while, for, do-while, switch)
--   * Function declarations (regular, async, generator)
--   * Class declarations
--   * Try\/catch\/finally
--   * Jump statements (return, break, continue, throw)
--   * Labeled statements
--   * Import\/export declarations
--   * With statement
--
-- @since 0.8.0.0
module Language.JavaScript.Parser.Flatparse.Grammar
  ( -- * Expression parsers
    expression
  , assignmentExpression
  , primaryExpression
  , callMemberExpression
  , unaryExpression
  , binaryExpression
  , binaryOperator
  , unaryOperator
  , assignmentOperator
  , callExpression
  , literalExpression
  , identifierExpression
  , listToCommaList
  , listToCommaTrailingList
  , objectLiteral
  , arrayLiteral
  , argumentList
  , sepBy
  , sepBy1
  , parseChar
  , parseString
  , fpPosToAnnot
  , defaultAnnot
  , defaultSemi
  , classElement
    -- * Statement parsers
  , statement
  , statementList
  , blockStatement
  , expressionStatement
  , emptyStatement
  , variableDeclaration
  , variableDeclarator
  , ifStatement
  , whileStatement
  , doWhileStatement
  , forStatement
  , forInit
  , switchStatement
  , caseClause
  , defaultClause
  , returnStatement
  , breakStatement
  , continueStatement
  , throwStatement
  , functionDeclaration
  , classDeclaration
  , regularMethodDefinition
  , tryStatement
  , catchClause
  , finallyClause
  , labeledStatement
  , withStatementPos
  , expectStatementEnd
  , isStatementKeyword
    -- * Module items
  , moduleItem
  , moduleItemList
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.List (foldl', filter)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified FlatParse.Basic as FP
import Control.Applicative (pure, (*>), (<*>), (<$>))

import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.SrcLocation (TokenPosn(TokenPn))

import Language.JavaScript.Parser.Flatparse.Lexer
  ( whitespace
  , numericLiteral
  , identifier
  , keyword
  , rawIdentifier
  , isKeyword
  )
import qualified Language.JavaScript.Parser.Flatparse.Pos as JSPos
import Language.JavaScript.Parser.Flatparse.Primitives

-- =====================================================================
-- Helpers
-- =====================================================================

-- | Convert FlatParse position to JSAnnot.
-- Stores the raw FP.Pos value (remaining bytes) in the TokenPn offset field.
-- The offset is converted to real line/col by 'fixPositions' after parsing.
fpPosToAnnot :: FP.Pos -> JSAnnot
fpPosToAnnot fpPos = JSAnnot (TokenPn (FP.unPos fpPos) 0 0) []

-- | Default annotation for generated nodes (no position information).
-- Uses -1 as sentinel to distinguish from real positions at offset 0.
defaultAnnot :: JSAnnot
defaultAnnot = JSAnnot (TokenPn (-1) 0 0) []

-- | Default semicolon.
defaultSemi :: JSSemi
defaultSemi = JSSemiAuto

-- | Parse specific character.
parseChar :: Char -> JSParser ()
parseChar c = do
  actual <- FP.anyChar
  if actual == c then pure () else FP.empty

-- | Parse specific character and return its annotation with position.
parseCharAnnot :: Char -> JSParser JSAnnot
parseCharAnnot c = do
  pos <- FP.getPos
  parseChar c
  pure (fpPosToAnnot pos)

-- | Parse specific string.
parseString :: String -> JSParser ()
parseString [] = pure ()
parseString (c:cs) = parseChar c *> parseString cs

-- | Parse specific string and return its annotation with position.
parseStringAnnot :: String -> JSParser JSAnnot
parseStringAnnot s = do
  pos <- FP.getPos
  parseString s
  pure (fpPosToAnnot pos)

-- | Parse a keyword and return its annotation with position.
keywordAnnot :: ByteString -> JSParser JSAnnot
keywordAnnot kw = do
  pos <- FP.getPos
  keyword kw
  pure (fpPosToAnnot pos)

-- | Parse an identifier and return it as JSIdent with position.
identName :: JSParser JSIdent
identName = do
  pos <- FP.getPos
  name <- identifier
  pure (JSIdentName (fpPosToAnnot pos) name)

-- | Parse separated list (zero or more).
sepBy :: JSParser a -> JSParser sep -> JSParser [a]
sepBy p sep = sepBy1 p sep FP.<|> pure []

-- | Parse separated list (one or more).
sepBy1 :: JSParser a -> JSParser sep -> JSParser [a]
sepBy1 p sep = do
  first <- p
  rest <- FP.many (sep *> p)
  pure (first : rest)

-- | Parse separated list with optional trailing separator.
sepByTrailing :: JSParser a -> JSParser sep -> JSParser [a]
sepByTrailing p sep = do
  items <- sepBy p sep
  _ <- FP.optional sep
  pure items

-- | Convert list to JSCommaList (no position tracking for commas).
listToCommaList :: [a] -> JSCommaList a
listToCommaList [] = JSLNil
listToCommaList [x] = JSLOne x
listToCommaList (x:xs) = go (JSLOne x) xs
  where
    go acc [] = acc
    go acc (y:ys) = go (JSLCons acc defaultAnnot y) ys

-- | Build JSCommaList from items paired with their preceding comma annotation.
listToAnnotCommaList :: a -> [(JSAnnot, a)] -> JSCommaList a
listToAnnotCommaList first rest = go (JSLOne first) rest
  where
    go acc [] = acc
    go acc ((commaA, y):ys) = go (JSLCons acc commaA y) ys

-- | Parse comma-separated list with tracked comma positions.
parseAnnotCommaList :: JSParser a -> JSParser (JSCommaList a)
parseAnnotCommaList item = do
  first <- FP.optional item
  case first of
    Nothing -> pure JSLNil
    Just x -> commaLoop x []
  where
    commaLoop firstItem acc = do
      whitespace
      mc <- FP.optional (parseCharAnnot ',')
      case mc of
        Nothing -> pure (listToAnnotCommaList firstItem (reverse acc))
        Just commaA -> do
          whitespace
          next <- item
          commaLoop firstItem ((commaA, next) : acc)

-- | Parse comma-separated list with optional trailing comma, returning
-- the list and trailing comma annotation if present.
parseAnnotCommaListTrailing :: JSParser a -> JSParser (JSCommaList a, Maybe JSAnnot)
parseAnnotCommaListTrailing item = do
  first <- FP.optional item
  case first of
    Nothing -> pure (JSLNil, Nothing)
    Just x -> commaLoop x []
  where
    commaLoop firstItem acc = do
      whitespace
      mc <- FP.optional (parseCharAnnot ',')
      case mc of
        Nothing -> pure (listToAnnotCommaList firstItem (reverse acc), Nothing)
        Just commaA -> do
          whitespace
          mNext <- FP.optional item
          case mNext of
            Nothing -> pure (listToAnnotCommaList firstItem (reverse acc), Just commaA)
            Just next -> commaLoop firstItem ((commaA, next) : acc)

-- | Parse comma-separated list with optional trailing comma, discarding
-- the trailing comma annotation. Useful for parameter lists where the AST
-- does not store a trailing comma.
parseAnnotCommaListDropTrailing :: JSParser a -> JSParser (JSCommaList a)
parseAnnotCommaListDropTrailing item = fst <$> parseAnnotCommaListTrailing item

-- | Convert list to JSCommaTrailingList.
listToCommaTrailingList :: [a] -> JSCommaTrailingList a
listToCommaTrailingList xs = JSCTLNone (listToCommaList xs)

-- | Ensure next char is NOT the given character (negative lookahead).
notFollowedBy :: Char -> JSParser ()
notFollowedBy c = do
  mc <- FP.optional (parseChar c)
  case mc of
    Nothing -> pure ()
    Just _ -> FP.empty

-- | Check if a line terminator exists before the next significant token.
-- Uses lookahead to avoid consuming input. Used for ASI-sensitive keywords
-- like @return@, @break@, @continue@, and @throw@.
hasLineTerminatorBeforeNext :: JSParser Bool
hasLineTerminatorBeforeNext = FP.lookahead scanAhead FP.<|> pure True
  where
    scanAhead = do
      skipSpacesAndTabs
      mc <- FP.optional FP.anyChar
      pure $ case mc of
        Nothing   -> True
        Just '\n' -> True
        Just '\r' -> True
        _         -> False
    skipSpacesAndTabs = FP.skipMany (FP.satisfy (\c -> c == ' ' || c == '\t'))

-- | Parse contextual keyword (not in the reserved word list).
contextualKeyword :: ByteString -> JSParser ()
contextualKeyword kw = do
  ident <- rawIdentifier
  if ident == kw then pure () else FP.empty

-- | Parse contextual keyword and return its annotation with position.
contextualKeywordAnnot :: ByteString -> JSParser JSAnnot
contextualKeywordAnnot kw = do
  pos <- FP.getPos
  contextualKeyword kw
  pure (fpPosToAnnot pos)

-- | Parse raw string literal including quotes.
-- | Parse a string literal preserving raw source bytes (zero-copy).
stringLiteralRaw :: JSParser ByteString
stringLiteralRaw = FP.byteStringOf (singleQuotedSkip FP.<|> doubleQuotedSkip)
  where
    singleQuotedSkip = do
      parseChar '\''
      FP.skipMany (escapeSkip FP.<|> normalCharSkip '\'')
      parseChar '\''
    doubleQuotedSkip = do
      parseChar '"'
      FP.skipMany (escapeSkip FP.<|> normalCharSkip '"')
      parseChar '"'

-- | Skip a normal character in a string literal.
normalCharSkip :: Char -> JSParser ()
normalCharSkip quote =
  () <$ FP.satisfy (\c -> c /= quote && c /= '\\' && c /= '\n' && c /= '\r')

-- | Skip an escape sequence in a string literal.
escapeSkip :: JSParser ()
escapeSkip = parseChar '\\' *> (() <$ FP.anyChar)

-- | Optionally consume semicolon or newline (returns unit).
expectSemiOrNewline :: JSParser ()
expectSemiOrNewline = do
  whitespace
  _ <- FP.optional (parseChar ';')
  pure ()

-- | Expect statement termination (semicolon, newline, or end of input).
-- Returns 'JSSemicolon' if an explicit semicolon was consumed,
-- 'JSSemiAuto' otherwise (ASI).
expectStatementEnd :: JSParser JSSemi
expectStatementEnd = do
  whitespace
  pos <- FP.getPos
  hasSemi <- FP.optional (parseChar ';')
  pure (maybe JSSemiAuto (const (JSSemi (fpPosToAnnot pos))) hasSemi)

-- | Parse block body: @{ stmts }@
blockBody :: JSParser JSBlock
blockBody = do
  pos <- FP.getPos
  parseChar '{'
  whitespace
  stmts <- FP.many statement
  whitespace
  rb <- parseCharAnnot '}'
  pure (JSBlock (fpPosToAnnot pos) stmts rb)

-- | Parse function parameter.
functionParam :: JSParser JSExpression
functionParam = restParam FP.<|> destructuringDefaultParam FP.<|> defaultParam FP.<|> destructuringParam FP.<|> simpleParam
  where
    restParam = do
      pos <- FP.getPos
      parseString "..."
      whitespace
      target <- destructuringParam FP.<|> simpleParam
      pure (JSSpreadExpression (fpPosToAnnot pos) target)
    destructuringDefaultParam = do
      target <- arrayLiteral FP.<|> objectLiteral
      whitespace
      eqA <- parseCharAnnot '='
      whitespace
      val <- assignmentExpression
      pure (JSAssignExpression target (JSAssign eqA) val)
    defaultParam = do
      pos <- FP.getPos
      name <- identifier
      whitespace
      eqA <- parseCharAnnot '='
      whitespace
      val <- assignmentExpression
      pure (JSAssignExpression (JSIdentifier (fpPosToAnnot pos) name) (JSAssign eqA) val)
    destructuringParam = arrayLiteral FP.<|> objectLiteral
    simpleParam = do
      pos <- FP.getPos
      name <- identifier
      pure (JSIdentifier (fpPosToAnnot pos) name)

-- | Parse property name (identifier, string, number, or computed).
propertyName :: JSParser JSPropertyName
propertyName = computedProp FP.<|> stringProp FP.<|> numericProp FP.<|> identProp
  where
    computedProp = do
      pos <- FP.getPos
      parseChar '['
      whitespace
      expr <- expression
      whitespace
      rb <- parseCharAnnot ']'
      pure (JSPropertyComputed (fpPosToAnnot pos) expr rb)
    stringProp = do
      pos <- FP.getPos
      raw <- stringLiteralRaw
      pure (JSPropertyString (fpPosToAnnot pos) raw)
    numericProp = do
      pos <- FP.getPos
      num <- numericLiteral
      pure (JSPropertyNumber (fpPosToAnnot pos) num)
    identProp = do
      pos <- FP.getPos
      name <- rawIdentifier
      pure (JSPropertyIdent (fpPosToAnnot pos) name)

-- =====================================================================
-- Expression Parsing
-- =====================================================================

-- | Parse any JavaScript expression (comma expressions included).
expression :: JSParser JSExpression
expression = commaExpression

-- | Parse comma expression: @a, b, c@
commaExpression :: JSParser JSExpression
commaExpression = do
  left <- assignmentExpression
  commaLoop left
  where
    commaLoop left = do
      whitespace
      mc <- FP.optional (parseCharAnnot ',')
      case mc of
        Nothing -> pure left
        Just commaA -> do
          whitespace
          right <- assignmentExpression
          commaLoop (JSCommaExpression left commaA right)

-- ---------------------------------------------------------------------
-- Assignment and Arrow Expressions
-- ---------------------------------------------------------------------

-- | Parse assignment expression or arrow function.
assignmentExpression :: JSParser JSExpression
assignmentExpression =
  asyncArrowFunction FP.<|> arrowFunction FP.<|> yieldExpr FP.<|> normalAssignment
  where
    normalAssignment = do
      left <- conditionalExpression
      whitespace
      maybeOp <- FP.optional assignmentOperator
      case maybeOp of
        Nothing -> pure left
        Just op -> do
          whitespace
          right <- assignmentExpression
          pure (JSAssignExpression left op right)

-- | Parse async arrow function: @async (params) => body@ or @async x => body@
asyncArrowFunction :: JSParser JSExpression
asyncArrowFunction = asyncParenArrow FP.<|> asyncSingleParamArrow

-- | Async arrow with parenthesized params: @async (a, b) => body@
asyncParenArrow :: JSParser JSExpression
asyncParenArrow = do
  asyncPos <- FP.getPos
  keyword "async"
  whitespace
  pos <- FP.getPos
  parseChar '('
  whitespace
  paramList <- parseAnnotCommaListDropTrailing arrowParam
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  arrowA <- parseStringAnnot "=>"
  whitespace
  body <- arrowBody
  pure (JSAsyncArrowExpression (fpPosToAnnot asyncPos)
    (JSParenthesizedArrowParameterList (fpPosToAnnot pos) paramList rp)
    arrowA body)

-- | Async arrow with single param: @async x => body@
asyncSingleParamArrow :: JSParser JSExpression
asyncSingleParamArrow = do
  asyncPos <- FP.getPos
  keyword "async"
  whitespace
  pos <- FP.getPos
  name <- identifier
  whitespace
  arrowA <- parseStringAnnot "=>"
  whitespace
  body <- arrowBody
  pure (JSAsyncArrowExpression (fpPosToAnnot asyncPos)
    (JSUnparenthesizedArrowParameter (JSIdentName (fpPosToAnnot pos) name))
    arrowA body)

-- | Parse arrow function: @(params) => body@ or @x => body@
arrowFunction :: JSParser JSExpression
arrowFunction = parenArrow FP.<|> singleParamArrow

-- | Arrow function with parenthesized params: @(a, b) => body@
parenArrow :: JSParser JSExpression
parenArrow = do
  pos <- FP.getPos
  parseChar '('
  whitespace
  paramList <- parseAnnotCommaListDropTrailing arrowParam
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  arrowA <- parseStringAnnot "=>"
  whitespace
  body <- arrowBody
  pure (JSArrowExpression
    (JSParenthesizedArrowParameterList (fpPosToAnnot pos) paramList rp)
    arrowA body)

-- | Arrow function with single unparenthesized param: @x => body@
singleParamArrow :: JSParser JSExpression
singleParamArrow = do
  pos <- FP.getPos
  name <- identifier
  whitespace
  arrowA <- parseStringAnnot "=>"
  whitespace
  body <- arrowBody
  pure (JSArrowExpression
    (JSUnparenthesizedArrowParameter (JSIdentName (fpPosToAnnot pos) name))
    arrowA body)

-- | Parse arrow function parameter.
-- Supports simple identifiers, destructuring patterns, default values, and rest params.
arrowParam :: JSParser JSExpression
arrowParam = spreadParam FP.<|> destructuringDefaultArrow FP.<|> defaultParamArrow FP.<|> destructuringParamArrow FP.<|> simpleParamArrow
  where
    spreadParam = do
      pos <- FP.getPos
      parseString "..."
      target <- destructuringParamArrow FP.<|> simpleParamArrow
      pure (JSSpreadExpression (fpPosToAnnot pos) target)
    destructuringDefaultArrow = do
      target <- arrayLiteral FP.<|> objectLiteral
      whitespace
      eqA <- parseCharAnnot '='
      whitespace
      val <- assignmentExpression
      pure (JSAssignExpression target (JSAssign eqA) val)
    defaultParamArrow = do
      pos <- FP.getPos
      name <- identifier
      whitespace
      eqA <- parseCharAnnot '='
      whitespace
      val <- assignmentExpression
      pure (JSAssignExpression (JSIdentifier (fpPosToAnnot pos) name) (JSAssign eqA) val)
    destructuringParamArrow = arrayLiteral FP.<|> objectLiteral
    simpleParamArrow = do
      pos <- FP.getPos
      name <- identifier
      pure (JSIdentifier (fpPosToAnnot pos) name)

-- | Parse arrow function body: block or concise expression.
arrowBody :: JSParser JSConciseBody
arrowBody = arrowBlockBody FP.<|> exprBody
  where
    arrowBlockBody = JSConciseFunctionBody <$> blockBody
    exprBody = JSConciseExpressionBody <$> assignmentExpression

-- | Parse yield expression: @yield expr@ or @yield* expr@
yieldExpr :: JSParser JSExpression
yieldExpr = yieldFrom FP.<|> yieldSimple
  where
    yieldFrom = do
      pos <- FP.getPos
      keyword "yield"
      whitespace
      starA <- parseCharAnnot '*'
      whitespace
      expr <- assignmentExpression
      pure (JSYieldFromExpression (fpPosToAnnot pos) starA expr)
    yieldSimple = do
      pos <- FP.getPos
      keyword "yield"
      whitespace
      expr <- FP.optional assignmentExpression
      pure (JSYieldExpression (fpPosToAnnot pos) expr)

-- | Parse assignment operator.
assignmentOperator :: JSParser JSAssignOp
assignmentOperator = do
  pos <- FP.getPos
  let a = fpPosToAnnot pos
  (parseString ">>>=" *> pure (JSUrshAssign a)) FP.<|>
    (parseString ">>=" *> pure (JSRshAssign a)) FP.<|>
    (parseString "<<=" *> pure (JSLshAssign a)) FP.<|>
    (parseString "**=" *> notFollowedBy '=' *> pure (JSTimesAssign a)) FP.<|>
    (parseString "&&=" *> pure (JSLogicalAndAssign a)) FP.<|>
    (parseString "||=" *> pure (JSLogicalOrAssign a)) FP.<|>
    (parseString "??=" *> pure (JSNullishAssign a)) FP.<|>
    (parseString "+=" *> pure (JSPlusAssign a)) FP.<|>
    (parseString "-=" *> pure (JSMinusAssign a)) FP.<|>
    (parseString "*=" *> pure (JSTimesAssign a)) FP.<|>
    (parseString "/=" *> pure (JSDivideAssign a)) FP.<|>
    (parseString "%=" *> pure (JSModAssign a)) FP.<|>
    (parseString "&=" *> pure (JSBwAndAssign a)) FP.<|>
    (parseString "^=" *> pure (JSBwXorAssign a)) FP.<|>
    (parseString "|=" *> pure (JSBwOrAssign a)) FP.<|>
    (parseChar '=' *> notFollowedBy '=' *> notFollowedBy '>' *> pure (JSAssign a))

-- ---------------------------------------------------------------------
-- Conditional (Ternary) Expression
-- ---------------------------------------------------------------------

-- | Parse conditional (ternary) expression: @a ? b : c@
conditionalExpression :: JSParser JSExpression
conditionalExpression = do
  left <- binaryExpression 0
  whitespace
  mq <- FP.optional (do qA <- parseCharAnnot '?'; notFollowedBy '?'; pure qA)
  case mq of
    Nothing -> pure left
    Just qA -> do
      whitespace
      trueExpr <- assignmentExpression
      whitespace
      colonA <- parseCharAnnot ':'
      whitespace
      falseExpr <- assignmentExpression
      pure (JSExpressionTernary left qA trueExpr colonA falseExpr)

-- ---------------------------------------------------------------------
-- Binary Expression (Pratt-style precedence climbing)
-- ---------------------------------------------------------------------

-- | Binary operator precedence levels (higher = tighter binding).
-- 0: || (logical or)
-- 1: ?? (nullish coalescing)
-- 2: && (logical and)
-- 3: | (bitwise or)
-- 4: ^ (bitwise xor)
-- 5: & (bitwise and)
-- 6: == != === !== (equality)
-- 7: < > <= >= in instanceof (relational)
-- 8: << >> >>> (shift)
-- 9: + - (additive)
-- 10: * / % (multiplicative)
-- 11: ** (exponentiation, right-assoc)
--
-- Note: @??@ cannot be freely mixed with @||@ and @&&@ in JavaScript
-- without parentheses, but we handle precedence to match standard behavior.

-- | Parse binary expression with precedence climbing.
binaryExpression :: Int -> JSParser JSExpression
binaryExpression minPrec = do
  left <- unaryExpression
  binaryLoop minPrec left

-- | Binary operator infix loop.
binaryLoop :: Int -> JSExpression -> JSParser JSExpression
binaryLoop minPrec left = do
  whitespace
  maybeOp <- FP.optional (binaryOperatorAtPrec minPrec)
  case maybeOp of
    Nothing -> pure left
    Just (prec, isRight, op) -> do
      whitespace
      let nextPrec = if isRight then prec else prec + 1
      right <- binaryExpression nextPrec
      binaryLoop minPrec (JSExpressionBinary left op right)

-- | Try to parse a binary operator at or above the minimum precedence.
-- Returns (precedence, is-right-associative, operator).
binaryOperatorAtPrec :: Int -> JSParser (Int, Bool, JSBinOp)
binaryOperatorAtPrec minPrec = do
  pos <- FP.getPos
  let a = fpPosToAnnot pos
  tryOps a
  where
    tryOps a =
      tryOp 11 True  (parseString "**" *> notFollowedBy '=') (JSBinOpExponentiation a) FP.<|>
      tryOp 10 False (parseChar '*' *> notFollowedBy '*' *> notFollowedBy '=') (JSBinOpTimes a) FP.<|>
      tryOp 10 False (parseChar '/' *> notFollowedBy '=') (JSBinOpDivide a) FP.<|>
      tryOp 10 False (parseChar '%' *> notFollowedBy '=') (JSBinOpMod a) FP.<|>
      tryOp 9  False (parseChar '+' *> notFollowedBy '+' *> notFollowedBy '=') (JSBinOpPlus a) FP.<|>
      tryOp 9  False (parseChar '-' *> notFollowedBy '-' *> notFollowedBy '=') (JSBinOpMinus a) FP.<|>
      tryOp 8  False (parseString ">>>" *> notFollowedBy '=') (JSBinOpUrsh a) FP.<|>
      tryOp 8  False (parseString ">>" *> notFollowedBy '>' *> notFollowedBy '=') (JSBinOpRsh a) FP.<|>
      tryOp 8  False (parseString "<<" *> notFollowedBy '=') (JSBinOpLsh a) FP.<|>
      tryOp 7  False (parseString "<=" ) (JSBinOpLe a) FP.<|>
      tryOp 7  False (parseString ">=" ) (JSBinOpGe a) FP.<|>
      tryOp 7  False (parseChar '<' *> notFollowedBy '<' *> notFollowedBy '=') (JSBinOpLt a) FP.<|>
      tryOp 7  False (parseChar '>' *> notFollowedBy '>' *> notFollowedBy '=') (JSBinOpGt a) FP.<|>
      tryOp 7  False (keyword "instanceof") (JSBinOpInstanceOf a) FP.<|>
      tryOp 7  False (keyword "in") (JSBinOpIn a) FP.<|>
      tryOp 6  False (parseString "===" ) (JSBinOpStrictEq a) FP.<|>
      tryOp 6  False (parseString "!==" ) (JSBinOpStrictNeq a) FP.<|>
      tryOp 6  False (parseString "==" *> notFollowedBy '=') (JSBinOpEq a) FP.<|>
      tryOp 6  False (parseString "!=" *> notFollowedBy '=') (JSBinOpNeq a) FP.<|>
      tryOp 5  False (parseChar '&' *> notFollowedBy '&' *> notFollowedBy '=') (JSBinOpBitAnd a) FP.<|>
      tryOp 4  False (parseChar '^' *> notFollowedBy '=') (JSBinOpBitXor a) FP.<|>
      tryOp 3  False (parseChar '|' *> notFollowedBy '|' *> notFollowedBy '=') (JSBinOpBitOr a) FP.<|>
      tryOp 2  False (parseString "&&" *> notFollowedBy '=') (JSBinOpAnd a) FP.<|>
      tryOp 1  False (parseString "??" *> notFollowedBy '=') (JSBinOpNullishCoalescing a) FP.<|>
      tryOp 0  False (parseString "||" *> notFollowedBy '=') (JSBinOpOr a)

    tryOp prec isRight parser op
      | prec >= minPrec = parser *> pure (prec, isRight, op)
      | otherwise = FP.empty

-- | Parse binary operator (for export compatibility).
binaryOperator :: JSParser JSBinOp
binaryOperator = do
  (_, _, op) <- binaryOperatorAtPrec 0
  pure op

-- ---------------------------------------------------------------------
-- Unary Expressions
-- ---------------------------------------------------------------------

-- | Parse unary expression (prefix operators).
unaryExpression :: JSParser JSExpression
unaryExpression =
  awaitExpr FP.<|>
  prefixIncDec FP.<|>
  prefixUnary FP.<|>
  keywordUnary FP.<|>
  postfixExpression

-- | Parse await expression: @await expr@
awaitExpr :: JSParser JSExpression
awaitExpr = do
  pos <- FP.getPos
  keyword "await"
  whitespace
  expr <- unaryExpression
  pure (JSAwaitExpression (fpPosToAnnot pos) expr)

-- | Parse prefix increment/decrement: @++x@ or @--x@
prefixIncDec :: JSParser JSExpression
prefixIncDec = do
  pos <- FP.getPos
  op <- (parseString "++" *> pure (JSUnaryOpIncr (fpPosToAnnot pos))) FP.<|>
        (parseString "--" *> pure (JSUnaryOpDecr (fpPosToAnnot pos)))
  whitespace
  expr <- unaryExpression
  pure (JSUnaryExpression op expr)

-- | Parse prefix unary: @!x@, @~x@, @+x@, @-x@
prefixUnary :: JSParser JSExpression
prefixUnary = do
  op <- unaryOperator
  whitespace
  expr <- unaryExpression
  pure (JSUnaryExpression op expr)

-- | Parse keyword-based unary: @typeof x@, @void x@, @delete x@
keywordUnary :: JSParser JSExpression
keywordUnary = do
  pos <- FP.getPos
  op <- (keyword "typeof" *> pure (JSUnaryOpTypeof (fpPosToAnnot pos))) FP.<|>
        (keyword "void" *> pure (JSUnaryOpVoid (fpPosToAnnot pos))) FP.<|>
        (keyword "delete" *> pure (JSUnaryOpDelete (fpPosToAnnot pos)))
  whitespace
  expr <- unaryExpression
  pure (JSUnaryExpression op expr)

-- | Parse unary operator (symbol-based only).
unaryOperator :: JSParser JSUnaryOp
unaryOperator = do
  pos <- FP.getPos
  let a = fpPosToAnnot pos
  (parseChar '!' *> notFollowedBy '=' *> pure (JSUnaryOpNot a)) FP.<|>
    (parseChar '~' *> pure (JSUnaryOpTilde a)) FP.<|>
    (parseChar '+' *> notFollowedBy '+' *> notFollowedBy '=' *> pure (JSUnaryOpPlus a)) FP.<|>
    (parseChar '-' *> notFollowedBy '-' *> notFollowedBy '=' *> pure (JSUnaryOpMinus a))

-- | Parse postfix expression: @x++@ or @x--@
postfixExpression :: JSParser JSExpression
postfixExpression = do
  expr <- callMemberExpression
  whitespace
  maybeOp <- FP.optional postfixOp
  case maybeOp of
    Nothing -> pure expr
    Just op -> pure (JSExpressionPostfix expr op)
  where
    postfixOp = do
      pos <- FP.getPos
      (parseString "++" *> pure (JSUnaryOpIncr (fpPosToAnnot pos))) FP.<|>
        (parseString "--" *> pure (JSUnaryOpDecr (fpPosToAnnot pos)))

-- ---------------------------------------------------------------------
-- Call and Member Expressions
-- ---------------------------------------------------------------------

-- | Parse call/member expression chain.
callMemberExpression :: JSParser JSExpression
callMemberExpression = do
  base <- newExpression FP.<|> primaryExpression
  memberLoop base

-- | Parse member access loop (before any call has been seen).
-- A function call here produces 'JSMemberExpression', then switches to 'callChainLoop'.
memberLoop :: JSExpression -> JSParser JSExpression
memberLoop expr = do
  whitespace
  next <- FP.optional (memberOrFirstCall expr)
  case next of
    Nothing -> pure expr
    Just (newExpr, wasCall) ->
      if wasCall then callChainLoop newExpr else memberLoop newExpr

-- | Try member access or first call. Returns (result, wasCall).
memberOrFirstCall :: JSExpression -> JSParser (JSExpression, Bool)
memberOrFirstCall expr =
  optionalChainResult expr FP.<|>
  dotAccessResult expr FP.<|>
  bracketAccessResult expr FP.<|>
  taggedTemplateResult expr FP.<|>
  firstCallResult expr

-- | Parse call chain loop (after a call has been seen).
-- Dot and bracket accesses produce CallExpressionDot/Square.
callChainLoop :: JSExpression -> JSParser JSExpression
callChainLoop expr = do
  whitespace
  next <- FP.optional (callChainAccess expr)
  case next of
    Nothing -> pure expr
    Just newExpr -> callChainLoop newExpr

-- | Access or call in a chain after initial call.
callChainAccess :: JSExpression -> JSParser JSExpression
callChainAccess expr =
  callChainDot expr FP.<|>
  callChainBracket expr FP.<|>
  chainedCall expr FP.<|>
  callChainTaggedTemplate expr FP.<|>
  optionalChainAccess expr

-- | Parse dot access: @obj.prop@ (before any call).
dotAccessResult :: JSExpression -> JSParser (JSExpression, Bool)
dotAccessResult expr = do
  pos <- FP.getPos
  parseChar '.'
  notFollowedBy '.'
  whitespace
  prop <- rawIdentifier
  pure (JSMemberDot expr (fpPosToAnnot pos) (JSIdentifier (fpPosToAnnot pos) prop), False)

-- | Parse bracket access: @obj[expr]@ (before any call).
bracketAccessResult :: JSExpression -> JSParser (JSExpression, Bool)
bracketAccessResult expr = do
  pos <- FP.getPos
  parseChar '['
  whitespace
  index <- expression
  whitespace
  rb <- parseCharAnnot ']'
  pure (JSMemberSquare expr (fpPosToAnnot pos) index rb, False)

-- | Parse first function call: @fn(args)@. Uses 'JSMemberExpression'.
firstCallResult :: JSExpression -> JSParser (JSExpression, Bool)
firstCallResult expr = do
  pos <- FP.getPos
  (argList, rp) <- argumentListAnnot
  pure (JSMemberExpression expr (fpPosToAnnot pos) argList rp, True)

-- | Parse tagged template literal: @tag\`text\`@. Returns (result, False).
taggedTemplateResult :: JSExpression -> JSParser (JSExpression, Bool)
taggedTemplateResult tag = do
  pos <- FP.getPos
  parseChar '`'
  (content, isInterp) <- templateCharsUntilEnd
  result <- buildTaggedTemplate tag pos content isInterp
  pure (result, False)

-- | Parse tagged template literal in call chain.
callChainTaggedTemplate :: JSExpression -> JSParser JSExpression
callChainTaggedTemplate tag = do
  pos <- FP.getPos
  parseChar '`'
  (content, isInterp) <- templateCharsUntilEnd
  buildTaggedTemplate tag pos content isInterp

-- | Build a tagged template literal from parsed components.
buildTaggedTemplate :: JSExpression -> FP.Pos -> ByteString -> Bool -> JSParser JSExpression
buildTaggedTemplate tag pos content isInterp =
  if isInterp
    then do
      parts <- templateParts
      pure (JSTemplateLiteral (Just tag) (fpPosToAnnot pos) ("`" <> content <> "${") parts)
    else
      pure (JSTemplateLiteral (Just tag) (fpPosToAnnot pos) ("`" <> content <> "`") [])

-- | Wrap optional chaining result with wasCall = False.
optionalChainResult :: JSExpression -> JSParser (JSExpression, Bool)
optionalChainResult expr = do
  result <- optionalChainAccess expr
  pure (result, False)

-- | Parse dot access after a call: @fn().prop@. Uses 'JSCallExpressionDot'.
callChainDot :: JSExpression -> JSParser JSExpression
callChainDot expr = do
  pos <- FP.getPos
  parseChar '.'
  notFollowedBy '.'
  whitespace
  prop <- rawIdentifier
  pure (JSCallExpressionDot expr (fpPosToAnnot pos) (JSIdentifier (fpPosToAnnot pos) prop))

-- | Parse bracket access after a call: @fn()[expr]@. Uses 'JSCallExpressionSquare'.
callChainBracket :: JSExpression -> JSParser JSExpression
callChainBracket expr = do
  pos <- FP.getPos
  parseChar '['
  whitespace
  index <- expression
  whitespace
  rb <- parseCharAnnot ']'
  pure (JSCallExpressionSquare expr (fpPosToAnnot pos) index rb)

-- | Parse chained function call: @fn(args1)(args2)@. Uses 'JSCallExpression'.
chainedCall :: JSExpression -> JSParser JSExpression
chainedCall expr = do
  pos <- FP.getPos
  (argList, rp) <- argumentListAnnot
  pure (JSCallExpression expr (fpPosToAnnot pos) argList rp)

-- | Parse optional chaining: @obj?.prop@, @obj?.[expr]@, @obj?.(args)@
optionalChainAccess :: JSExpression -> JSParser JSExpression
optionalChainAccess expr = do
  pos <- FP.getPos
  parseString "?."
  whitespace
  optDot expr pos FP.<|> optBracket expr pos FP.<|> optCall expr pos
  where
    optDot e p = do
      propPos <- FP.getPos
      prop <- rawIdentifier
      pure (JSOptionalMemberDot e (fpPosToAnnot p) (JSIdentifier (fpPosToAnnot propPos) prop))
    optBracket e p = do
      parseChar '['
      whitespace
      index <- expression
      whitespace
      rb <- parseCharAnnot ']'
      pure (JSOptionalMemberSquare e (fpPosToAnnot p) index rb)
    optCall e p = do
      parseChar '('
      whitespace
      argList <- parseAnnotCommaListDropTrailing assignmentExpression
      whitespace
      rp <- parseCharAnnot ')'
      pure (JSOptionalCallExpression e (fpPosToAnnot p) argList rp)

-- | Parse new expression: @new Foo()@ or @new Foo@
newExpression :: JSParser JSExpression
newExpression = do
  pos <- FP.getPos
  keyword "new"
  whitespace
  target <- newExpression FP.<|> memberOnlyExpression
  whitespace
  maybeArgs <- FP.optional argumentListAnnotFull
  case maybeArgs of
    Just (lp, argList, rp) -> pure (JSMemberNew (fpPosToAnnot pos) target lp argList rp)
    Nothing -> pure (JSNewExpression (fpPosToAnnot pos) target)

-- | Parse member expression without calls (for new target).
memberOnlyExpression :: JSParser JSExpression
memberOnlyExpression = do
  base <- primaryExpression
  memberOnlyLoop base
  where
    memberOnlyLoop expr = do
      whitespace
      next <- FP.optional (memberOnlyAccess expr)
      case next of
        Nothing -> pure expr
        Just newExpr -> memberOnlyLoop newExpr
    memberOnlyAccess expr = memberOnlyDot expr FP.<|> memberOnlyBracket expr
    memberOnlyDot expr = do
      pos <- FP.getPos
      parseChar '.'
      notFollowedBy '.'
      whitespace
      prop <- rawIdentifier
      pure (JSMemberDot expr (fpPosToAnnot pos) (JSIdentifier (fpPosToAnnot pos) prop))
    memberOnlyBracket expr = do
      pos <- FP.getPos
      parseChar '['
      whitespace
      index <- expression
      whitespace
      rb <- parseCharAnnot ']'
      pure (JSMemberSquare expr (fpPosToAnnot pos) index rb)

-- | Parse argument list: @(a, b, c)@
argumentList :: JSParser [JSExpression]
argumentList = do
  parseChar '('
  whitespace
  args <- sepByTrailing assignmentExpression (whitespace *> parseChar ',' *> whitespace)
  whitespace
  parseChar ')'
  pure args

-- | Parse argument list with tracked comma and closing paren positions.
argumentListAnnot :: JSParser (JSCommaList JSExpression, JSAnnot)
argumentListAnnot = do
  parseChar '('
  whitespace
  argList <- parseAnnotCommaListDropTrailing assignmentExpression
  whitespace
  rp <- parseCharAnnot ')'
  pure (argList, rp)

-- | Parse argument list with tracked opening paren, commas, and closing paren.
argumentListAnnotFull :: JSParser (JSAnnot, JSCommaList JSExpression, JSAnnot)
argumentListAnnotFull = do
  lp <- parseCharAnnot '('
  whitespace
  argList <- parseAnnotCommaListDropTrailing assignmentExpression
  whitespace
  rp <- parseCharAnnot ')'
  pure (lp, argList, rp)

-- | Alias for compatibility.
callExpression :: JSParser JSExpression
callExpression = callMemberExpression

-- ---------------------------------------------------------------------
-- Primary Expressions
-- ---------------------------------------------------------------------

-- | Parse primary expression (atoms).
primaryExpression :: JSParser JSExpression
primaryExpression =
  thisLiteral FP.<|>
  superLiteral FP.<|>
  nullLiteral FP.<|>
  booleanLiteral FP.<|>
  literalExpression FP.<|>
  regexLiteral FP.<|>
  templateLiteral FP.<|>
  spreadExpression FP.<|>
  importExpression FP.<|>
  asyncGeneratorExpr FP.<|>
  asyncFunctionExpr FP.<|>
  generatorExpression FP.<|>
  functionExpression FP.<|>
  classExpression FP.<|>
  arrayLiteral FP.<|>
  objectLiteral FP.<|>
  parenthesizedExpression FP.<|>
  identifierExpression

-- | Parse @this@ literal.
thisLiteral :: JSParser JSExpression
thisLiteral = do
  pos <- FP.getPos
  keyword "this"
  pure (JSLiteral (fpPosToAnnot pos) "this")

-- | Parse @super@ literal.
superLiteral :: JSParser JSExpression
superLiteral = do
  pos <- FP.getPos
  keyword "super"
  pure (JSLiteral (fpPosToAnnot pos) "super")

-- | Parse @null@ literal.
nullLiteral :: JSParser JSExpression
nullLiteral = do
  pos <- FP.getPos
  keyword "null"
  pure (JSLiteral (fpPosToAnnot pos) "null")

-- | Parse boolean literal.
booleanLiteral :: JSParser JSExpression
booleanLiteral = do
  pos <- FP.getPos
  val <- (keyword "true" *> pure "true") FP.<|> (keyword "false" *> pure "false")
  pure (JSLiteral (fpPosToAnnot pos) val)

-- | Parse string or numeric literal.
literalExpression :: JSParser JSExpression
literalExpression = stringLit FP.<|> numericLit

-- | Parse string literal preserving raw source form (with quotes).
stringLit :: JSParser JSExpression
stringLit = do
  pos <- FP.getPos
  raw <- stringLiteralRaw
  pure (JSStringLiteral (fpPosToAnnot pos) raw)

-- | Parse numeric literal, detecting format.
numericLit :: JSParser JSExpression
numericLit = do
  pos <- FP.getPos
  raw <- numericLiteral
  pure (classifyNumeric (fpPosToAnnot pos) raw)

-- | Classify numeric literal by format and parse into proper numeric type.
-- Decimal literals become 'Double', all integer formats become 'Integer'.
classifyNumeric :: JSAnnot -> ByteString -> JSExpression
classifyNumeric a s
  | hasBigIntSuffix = JSBigIntLiteral a (parseBigIntValue s)
  | hasPrefix "0x" || hasPrefix "0X" = JSHexInteger a (parseHexValue s)
  | hasPrefix "0b" || hasPrefix "0B" = JSBinaryInteger a (parseBinaryValue s)
  | hasOctalPrefix = JSOctal a (parseOctalValue s)
  | otherwise = JSDecimal a (parseDecimalValue s)
  where
    hasBigIntSuffix = not (BS8.null s) && BS8.last s == 'n'
    hasPrefix p = p `BS8.isPrefixOf` s
    hasOctalPrefix
      | BS8.length s >= 2
      , BS8.index s 0 == '0' =
          let c = BS8.index s 1
          in c == 'o' || c == 'O' || (c >= '0' && c <= '7')
      | otherwise = False

-- | Parse a decimal numeric string to Double, stripping numeric separators.
-- Handles leading-dot decimals like @.5@ which Haskell's 'read' rejects.
-- Returns 0 for malformed input rather than crashing.
parseDecimalValue :: ByteString -> Double
parseDecimalValue bs = fromMaybe 0 (readMaybe cleaned)
  where
    cleaned = prependZero (filter (/= '_') (BS8.unpack bs))
    prependZero ('.':rest) = '0' : '.' : rest
    prependZero s = s

-- | Parse a hex numeric string (with 0x prefix) to Integer.
-- Returns 0 for malformed input rather than crashing.
parseHexValue :: ByteString -> Integer
parseHexValue bs = fromMaybe 0 (readMaybe ("0x" <> filter (/= '_') (BS8.unpack (BS8.drop 2 bs))))

-- | Parse a binary numeric string (with 0b prefix) to Integer.
parseBinaryValue :: ByteString -> Integer
parseBinaryValue bs = foldl' (\acc c -> acc * 2 + binDigitVal c) 0 digits
  where
    digits = filter (/= '_') (BS8.unpack (BS8.drop 2 bs))
    binDigitVal '0' = 0
    binDigitVal '1' = 1
    binDigitVal _ = 0

-- | Parse an octal numeric string to Integer.
-- Returns 0 for malformed input rather than crashing.
parseOctalValue :: ByteString -> Integer
parseOctalValue bs
  | BS8.length bs >= 2, c == 'o' || c == 'O' =
      fromMaybe 0 (readMaybe ("0o" <> filter (/= '_') (BS8.unpack (BS8.drop 2 bs))))
  | otherwise =
      fromMaybe 0 (readMaybe ("0o" <> filter (/= '_') (BS8.unpack (BS8.drop 1 bs))))
  where
    c = BS8.index bs 1

-- | Parse a BigInt literal to Integer (strip trailing 'n' and classify base).
-- Handles edge cases like @123.456n@ (truncates to integer) and @123e4n@
-- (evaluates scientific notation then truncates) for parser tolerance.
-- Returns 0 for malformed input rather than crashing.
parseBigIntValue :: ByteString -> Integer
parseBigIntValue bs = classifyAndParse (BS8.init bs)
  where
    classifyAndParse s
      | "0x" `BS8.isPrefixOf` s || "0X" `BS8.isPrefixOf` s = parseHexValue s
      | "0b" `BS8.isPrefixOf` s || "0B" `BS8.isPrefixOf` s = parseBinaryValue s
      | "0o" `BS8.isPrefixOf` s || "0O" `BS8.isPrefixOf` s = parseOctalValue s
      | otherwise = parseDecimalAsInteger (filter (/= '_') (BS8.unpack s))
    parseDecimalAsInteger str
      | any (`elem` (".eE" :: String)) str =
          truncate (fromMaybe 0 (readMaybe (prependZero str) :: Maybe Double))
      | otherwise = fromMaybe 0 (readMaybe str)
    prependZero ('.':rest) = '0' : '.' : rest
    prependZero s = s

-- | Parse regex literal: @/pattern/flags@ (zero-copy).
regexLiteral :: JSParser JSExpression
regexLiteral = do
  pos <- FP.getPos
  raw <- FP.byteStringOf regexSkip
  pure (JSRegEx (fpPosToAnnot pos) raw)
  where
    regexSkip = do
      parseChar '/'
      regexBodySkip
      FP.skipMany (FP.satisfy isRegexFlag)
    isRegexFlag c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

-- | Skip regex body up to the closing @/@.
regexBodySkip :: JSParser ()
regexBodySkip = goNormal
  where
    goNormal = do
      c <- FP.anyChar
      handleNormal c
    handleNormal '/' = pure ()
    handleNormal '\\' = FP.anyChar *> goNormal
    handleNormal '[' = goClass
    handleNormal '\n' = FP.empty
    handleNormal '\r' = FP.empty
    handleNormal _ = goNormal
    goClass = do
      c <- FP.anyChar
      handleClass c
    handleClass ']' = goNormal
    handleClass '\\' = FP.anyChar >>= handleClassEscape
    handleClass '\n' = FP.empty
    handleClass '\r' = FP.empty
    handleClass _ = goClass
    handleClassEscape ']' = goNormal
    handleClassEscape _ = goClass

-- | Parse template literal: @\`hello ${name}\`@
templateLiteral :: JSParser JSExpression
templateLiteral = do
  pos <- FP.getPos
  parseChar '`'
  (content, isInterp) <- templateCharsUntilEnd
  if isInterp
    then do
      parts <- templateParts
      pure (JSTemplateLiteral Nothing (fpPosToAnnot pos) ("`" <> content <> "${") parts)
    else
      pure (JSTemplateLiteral Nothing (fpPosToAnnot pos) ("`" <> content <> "`") [])

-- | Parse template literal body segments.
-- Each segment reads an expression between @${...}@, then collects
-- the suffix text up to the next @${@ or closing backtick.
templateParts :: JSParser [JSTemplatePart]
templateParts = do
  whitespace
  expr <- expression
  whitespace
  rbA <- parseCharAnnot '}'
  (content, isInterp) <- templateCharsUntilEnd
  if isInterp
    then do
      rest <- templateParts
      pure (JSTemplatePart expr rbA ("}" <> content <> "${") : rest)
    else
      pure [JSTemplatePart expr rbA ("}" <> content <> "`")]

-- | Parse template chars until closing backtick or interpolation start.
-- Returns (content, True) if @${@ was found, (content, False) if backtick closes.
templateCharsUntilEnd :: JSParser (ByteString, Bool)
templateCharsUntilEnd = go []
  where
    go acc = do
      c <- FP.anyChar
      handleChar c acc
    handleChar '`' acc = pure (BS8.pack (reverse acc), False)
    handleChar '\\' acc = do
      c2 <- FP.anyChar
      go (c2 : '\\' : acc)
    handleChar '$' acc = do
      mc <- FP.optional (parseChar '{')
      maybe (go ('$' : acc)) (const (pure (BS8.pack (reverse acc), True))) mc
    handleChar c acc = go (c : acc)

-- | Parse spread expression: @...expr@
spreadExpression :: JSParser JSExpression
spreadExpression = do
  pos <- FP.getPos
  parseString "..."
  whitespace
  expr <- assignmentExpression
  pure (JSSpreadExpression (fpPosToAnnot pos) expr)

-- | Parse import expression: @import.meta@ or @import(source)@
importExpression :: JSParser JSExpression
importExpression = importMeta FP.<|> importCall
  where
    importMeta = do
      pos <- FP.getPos
      keyword "import"
      dotA <- parseCharAnnot '.'
      keyword "meta"
      pure (JSImportMeta (fpPosToAnnot pos) dotA)
    importCall = do
      pos <- FP.getPos
      keyword "import"
      lp <- parseCharAnnot '('
      whitespace
      expr <- assignmentExpression
      whitespace
      rp <- parseCharAnnot ')'
      pure (JSImportCall (fpPosToAnnot pos) lp expr rp)

-- | Parse function expression: @function name(params) { body }@
functionExpression :: JSParser JSExpression
functionExpression = do
  pos <- FP.getPos
  keyword "function"
  whitespace
  name <- FP.optional identName
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  paramList <- parseAnnotCommaListDropTrailing functionParam
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- blockBody
  let ident = maybe JSIdentNone id name
  pure (JSFunctionExpression (fpPosToAnnot pos) ident lp paramList rp body)

-- | Parse async function expression: @async function name(params) { body }@
-- | Parse async generator expression: @async function* name(params) { body }@
asyncGeneratorExpr :: JSParser JSExpression
asyncGeneratorExpr = do
  pos <- FP.getPos
  keyword "async"
  whitespace
  funcAnnot <- keywordAnnot "function"
  whitespace
  star <- parseCharAnnot '*'
  whitespace
  name <- FP.optional identName
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  paramList <- parseAnnotCommaListDropTrailing functionParam
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- blockBody
  let ident = maybe JSIdentNone id name
  pure (JSAsyncGeneratorExpression (fpPosToAnnot pos) funcAnnot star ident lp paramList rp body)

asyncFunctionExpr :: JSParser JSExpression
asyncFunctionExpr = do
  pos <- FP.getPos
  keyword "async"
  whitespace
  funcAnnot <- keywordAnnot "function"
  whitespace
  name <- FP.optional identName
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  paramList <- parseAnnotCommaListDropTrailing functionParam
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- blockBody
  let ident = maybe JSIdentNone id name
  pure (JSAsyncFunctionExpression (fpPosToAnnot pos) funcAnnot ident lp paramList rp body)

-- | Parse generator expression: @function* name(params) { body }@
generatorExpression :: JSParser JSExpression
generatorExpression = do
  pos <- FP.getPos
  keyword "function"
  whitespace
  star <- parseCharAnnot '*'
  whitespace
  name <- FP.optional identName
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  paramList <- parseAnnotCommaListDropTrailing functionParam
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- blockBody
  let ident = maybe JSIdentNone id name
  pure (JSGeneratorExpression (fpPosToAnnot pos) star ident lp (paramList) rp body)

-- | Parse class expression: @class Name extends Super { body }@
classExpression :: JSParser JSExpression
classExpression = do
  pos <- FP.getPos
  keyword "class"
  whitespace
  name <- FP.optional identName
  whitespace
  heritage <- FP.optional exprExtendsClause
  whitespace
  lb <- parseCharAnnot '{'
  whitespace
  elements <- FP.many classElement
  whitespace
  rb <- parseCharAnnot '}'
  let ident = maybe JSIdentNone id name
      ext = maybe JSExtendsNone (\(ea, e) -> JSExtends ea e) heritage
  pure (JSClassExpression (fpPosToAnnot pos) ident ext lb elements rb)
  where
    exprExtendsClause = do
      ea <- keywordAnnot "extends"
      whitespace
      expr <- assignmentExpression
      pure (ea, expr)

-- | Parse identifier expression.
identifierExpression :: JSParser JSExpression
identifierExpression = do
  pos <- FP.getPos
  ident <- identifier
  pure (JSIdentifier (fpPosToAnnot pos) ident)

-- | Parse parenthesized expression: @(expr)@
parenthesizedExpression :: JSParser JSExpression
parenthesizedExpression = do
  pos <- FP.getPos
  parseChar '('
  whitespace
  expr <- expression
  whitespace
  rp <- parseCharAnnot ')'
  pure (JSExpressionParen (fpPosToAnnot pos) expr rp)

-- ---------------------------------------------------------------------
-- Array Literals
-- ---------------------------------------------------------------------

-- | Parse array literal: @[elem1, elem2, ...spread]@
-- Handles elisions (commas without values).
arrayLiteral :: JSParser JSExpression
arrayLiteral = do
  pos <- FP.getPos
  parseChar '['
  whitespace
  elements <- arrayElements
  whitespace
  rb <- parseCharAnnot ']'
  pure (JSArrayLiteral (fpPosToAnnot pos) elements rb)

-- | Parse array elements including elisions.
-- Commas between elements produce 'JSArrayComma' nodes in the AST,
-- preserving the comma structure for round-trip fidelity.
arrayElements :: JSParser [JSArrayElement]
arrayElements = FP.many arrayElementOrComma
  where
    arrayElementOrComma = elision FP.<|> spreadElement FP.<|> normalElement
    elision = do
      pos <- FP.getPos
      parseChar ','
      whitespace
      pure (JSArrayComma (fpPosToAnnot pos))
    spreadElement = do
      pos <- FP.getPos
      parseString "..."
      expr <- assignmentExpression
      pure (JSArrayElement (JSSpreadExpression (fpPosToAnnot pos) expr))
    normalElement = do
      expr <- assignmentExpression
      pure (JSArrayElement expr)

-- ---------------------------------------------------------------------
-- Object Literals
-- ---------------------------------------------------------------------

-- | Parse object literal: @{ key: value, ...spread }@
-- Handles trailing commas and shorthand properties.
objectLiteral :: JSParser JSExpression
objectLiteral = do
  pos <- FP.getPos
  parseChar '{'
  whitespace
  (propList, trailingComma) <- parseAnnotCommaListTrailing objectProperty
  whitespace
  rb <- parseCharAnnot '}'
  let trailingList = maybe (JSCTLNone propList) (\ca -> JSCTLComma propList ca) trailingComma
  pure (JSObjectLiteral (fpPosToAnnot pos) trailingList rb)

-- | Parse single object property.
objectProperty :: JSParser JSObjectProperty
objectProperty =
  spreadProp FP.<|>
  methodProp FP.<|>
  accessorProp FP.<|>
  computedProp FP.<|>
  keyValueProp FP.<|>
  shorthandProp

-- | Parse spread property: @...expr@
spreadProp :: JSParser JSObjectProperty
spreadProp = do
  pos <- FP.getPos
  parseString "..."
  whitespace
  expr <- assignmentExpression
  pure (JSObjectSpread (fpPosToAnnot pos) expr)

-- | Parse method property: @name(params) { body }@
methodProp :: JSParser JSObjectProperty
methodProp = generatorMethod FP.<|> asyncMethod FP.<|> regularMethod
  where
    generatorMethod = do
      pos <- FP.getPos
      parseChar '*'
      whitespace
      name <- propertyName
      whitespace
      lp <- parseCharAnnot '('
      whitespace
      paramList <- parseAnnotCommaListDropTrailing functionParam
      whitespace
      rp <- parseCharAnnot ')'
      whitespace
      body <- blockBody
      pure (JSObjectMethod (JSGeneratorMethodDefinition (fpPosToAnnot pos) name lp paramList rp body))
    asyncMethod = do
      pos <- FP.getPos
      keyword "async"
      whitespace
      name <- propertyName
      whitespace
      lp <- parseCharAnnot '('
      whitespace
      paramList <- parseAnnotCommaListDropTrailing functionParam
      whitespace
      rp <- parseCharAnnot ')'
      whitespace
      body <- blockBody
      pure (JSObjectMethod (JSAsyncMethodDefinition (fpPosToAnnot pos) name lp paramList rp body))
    regularMethod = do
      name <- propertyName
      whitespace
      lp <- parseCharAnnot '('
      whitespace
      paramList <- parseAnnotCommaListDropTrailing functionParam
      whitespace
      rp <- parseCharAnnot ')'
      whitespace
      body <- blockBody
      pure (JSObjectMethod (JSMethodDefinition name lp paramList rp body))

-- | Parse accessor property: @get name() { body }@ or @set name(val) { body }@
accessorProp :: JSParser JSObjectProperty
accessorProp = do
  pos <- FP.getPos
  accessor <- (keyword "get" *> pure (JSAccessorGet (fpPosToAnnot pos))) FP.<|>
              (keyword "set" *> pure (JSAccessorSet (fpPosToAnnot pos)))
  whitespace
  name <- propertyName
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  paramList <- parseAnnotCommaListDropTrailing functionParam
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- blockBody
  pure (JSObjectMethod (JSPropertyAccessor accessor name lp paramList rp body))

-- | Parse computed property: @[expr]: value@
computedProp :: JSParser JSObjectProperty
computedProp = do
  pos <- FP.getPos
  parseChar '['
  whitespace
  expr <- expression
  whitespace
  rb <- parseCharAnnot ']'
  whitespace
  colonA <- parseCharAnnot ':'
  whitespace
  value <- assignmentExpression
  pure (JSPropertyNameandValue (JSPropertyComputed (fpPosToAnnot pos) expr rb) colonA [value])

-- | Parse key:value property.
keyValueProp :: JSParser JSObjectProperty
keyValueProp = do
  name <- propertyName
  whitespace
  colonA <- parseCharAnnot ':'
  whitespace
  value <- assignmentExpression
  pure (JSPropertyNameandValue name colonA [value])

-- | Parse shorthand property: @x@ (equivalent to @x: x@)
shorthandProp :: JSParser JSObjectProperty
shorthandProp = do
  pos <- FP.getPos
  name <- rawIdentifier
  pure (JSPropertyIdentRef (fpPosToAnnot pos) name)

-- ---------------------------------------------------------------------
-- Class Elements
-- ---------------------------------------------------------------------

-- | Parse class element (method, field, static, private, etc.)
classElement :: JSParser JSClassElement
classElement = do
  whitespace
  classSemi FP.<|>
    staticElement FP.<|>
    privateElement FP.<|>
    instanceElement
  where
    classSemi = do
      pos <- FP.getPos
      parseChar ';'
      whitespace
      pure (JSClassSemi (fpPosToAnnot pos))
    staticElement = do
      pos <- FP.getPos
      keyword "static"
      whitespace
      staticBlock pos FP.<|> FP.try (staticMethod pos) FP.<|> staticField pos
    staticBlock pos = do
      body <- blockBody
      pure (JSClassStaticBlock (fpPosToAnnot pos) body)
    staticField pos = do
      name <- propertyName
      whitespace
      (eq, initExpr, semi) <- fieldInitializer
      pure (JSClassStaticField (fpPosToAnnot pos) name eq initExpr semi)
    staticMethod pos = do
      method <- classMethodDef
      pure (JSClassStaticMethod (fpPosToAnnot pos) method)
    privateElement = FP.try privateMethod FP.<|> privateAccessor FP.<|> privateField
    privateField = do
      pos <- FP.getPos
      parseChar '#'
      name <- rawIdentifier
      whitespace
      initExpr <- FP.optional (parseCharAnnot '=' *> whitespace *> assignmentExpression)
      expectSemiOrNewline
      pure (JSPrivateField (fpPosToAnnot pos) name (fpPosToAnnot pos) initExpr defaultSemi)
    privateMethod = do
      pos <- FP.getPos
      parseChar '#'
      name <- rawIdentifier
      whitespace
      lp <- parseCharAnnot '('
      whitespace
      paramList <- parseAnnotCommaListDropTrailing functionParam
      whitespace
      rp <- parseCharAnnot ')'
      whitespace
      body <- blockBody
      pure (JSPrivateMethod (fpPosToAnnot pos) name lp paramList rp body)
    privateAccessor = privateGetter FP.<|> privateSetter
    privateGetter = do
      pos <- FP.getPos
      keyword "get"
      whitespace
      parseChar '#'
      name <- rawIdentifier
      whitespace
      lp <- parseCharAnnot '('
      whitespace
      rp <- parseCharAnnot ')'
      whitespace
      body <- blockBody
      pure (JSPrivateAccessor (JSAccessorGet (fpPosToAnnot pos)) (fpPosToAnnot pos) name lp JSLNil rp body)
    privateSetter = do
      pos <- FP.getPos
      keyword "set"
      whitespace
      parseChar '#'
      name <- rawIdentifier
      whitespace
      lp <- parseCharAnnot '('
      whitespace
      paramList <- parseAnnotCommaListDropTrailing functionParam
      whitespace
      rp <- parseCharAnnot ')'
      whitespace
      body <- blockBody
      pure (JSPrivateAccessor (JSAccessorSet (fpPosToAnnot pos)) (fpPosToAnnot pos) name lp paramList rp body)
    instanceElement = asyncGeneratorMethod FP.<|> FP.try instanceMethod FP.<|> publicField
    asyncGeneratorMethod = do
      pos <- FP.getPos
      keyword "async"
      whitespace
      star <- parseCharAnnot '*'
      whitespace
      name <- propertyName
      whitespace
      lp <- parseCharAnnot '('
      whitespace
      paramList <- parseAnnotCommaListDropTrailing functionParam
      whitespace
      rp <- parseCharAnnot ')'
      whitespace
      body <- blockBody
      pure (JSAsyncGeneratorMethodDefinition (fpPosToAnnot pos) star name lp paramList rp body)
    publicField = do
      name <- propertyName
      whitespace
      (eq, initExpr, semi) <- fieldInitializer
      pure (JSClassField name eq initExpr semi)
    instanceMethod = do
      method <- classMethodDef
      pure (JSClassInstanceMethod method)
    fieldInitializer =
      fieldWithInit FP.<|> fieldWithoutInit
    fieldWithInit = do
      eq <- parseCharAnnot '='
      whitespace
      val <- assignmentExpression
      semi <- expectStatementEnd
      pure (eq, Just val, semi)
    fieldWithoutInit = do
      semi <- expectStatementEnd
      pure (defaultAnnot, Nothing, semi)

-- | Parse class method definition.
classMethodDef :: JSParser JSMethodDefinition
classMethodDef =
  generatorMethodDef FP.<|>
  asyncMethodDef FP.<|>
  accessorMethodDef FP.<|>
  regularMethodDef

-- | Parse regular method: @name(params) { body }@
regularMethodDef :: JSParser JSMethodDefinition
regularMethodDef = do
  name <- propertyName
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  paramList <- parseAnnotCommaListDropTrailing functionParam
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- blockBody
  pure (JSMethodDefinition name lp paramList rp body)

-- | Parse regular method definition (exported name for compatibility).
regularMethodDefinition :: JSParser JSMethodDefinition
regularMethodDefinition = regularMethodDef

-- | Parse generator method: @*name(params) { body }@
generatorMethodDef :: JSParser JSMethodDefinition
generatorMethodDef = do
  pos <- FP.getPos
  parseChar '*'
  whitespace
  name <- propertyName
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  paramList <- parseAnnotCommaListDropTrailing functionParam
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- blockBody
  pure (JSGeneratorMethodDefinition (fpPosToAnnot pos) name lp paramList rp body)

-- | Parse async method: @async name(params) { body }@
asyncMethodDef :: JSParser JSMethodDefinition
asyncMethodDef = do
  pos <- FP.getPos
  keyword "async"
  whitespace
  name <- propertyName
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  paramList <- parseAnnotCommaListDropTrailing functionParam
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- blockBody
  pure (JSAsyncMethodDefinition (fpPosToAnnot pos) name lp paramList rp body)

-- | Parse accessor method: @get name() { body }@ or @set name(val) { body }@
accessorMethodDef :: JSParser JSMethodDefinition
accessorMethodDef = do
  pos <- FP.getPos
  accessor <- (keyword "get" *> pure (JSAccessorGet (fpPosToAnnot pos))) FP.<|>
              (keyword "set" *> pure (JSAccessorSet (fpPosToAnnot pos)))
  whitespace
  name <- propertyName
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  paramList <- parseAnnotCommaListDropTrailing functionParam
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- blockBody
  pure (JSPropertyAccessor accessor name lp paramList rp body)

-- =====================================================================
-- Statement Parsing
-- =====================================================================

-- | Parse any JavaScript statement.
statement :: JSParser JSStatement
statement = do
  whitespace
  blockStatement FP.<|>
    emptyStatement FP.<|>
    variableDeclaration FP.<|>
    asyncGeneratorDecl FP.<|>
    asyncFunctionDecl FP.<|>
    generatorDecl FP.<|>
    functionDeclaration FP.<|>
    classDeclaration FP.<|>
    ifStatement FP.<|>
    whileStatement FP.<|>
    doWhileStatement FP.<|>
    forStatement FP.<|>
    switchStatement FP.<|>
    tryStatement FP.<|>
    withStatement FP.<|>
    returnStatement FP.<|>
    breakStatement FP.<|>
    continueStatement FP.<|>
    throwStatement FP.<|>
    debuggerStatement FP.<|>
    labeledStatement FP.<|>
    expressionStatement

-- | Parse a list of statements.
statementList :: JSParser [JSStatement]
statementList = FP.many statement

-- | Parse a module item (import, export, or regular statement).
moduleItem :: JSParser JSModuleItem
moduleItem = do
  whitespace
  importModuleItem FP.<|>
    exportModuleItem FP.<|>
    statementModuleItem

-- | Parse a list of module items.
moduleItemList :: JSParser [JSModuleItem]
moduleItemList = FP.many moduleItem

-- | Wrap a statement as a module item.
statementModuleItem :: JSParser JSModuleItem
statementModuleItem = JSModuleStatementListItem <$> statement

-- ---------------------------------------------------------------------
-- Basic Statements
-- ---------------------------------------------------------------------

-- | Parse block statement: @{ ... }@
blockStatement :: JSParser JSStatement
blockStatement = do
  pos <- FP.getPos
  parseChar '{'
  whitespace
  stmts <- FP.many statement
  whitespace
  rb <- parseCharAnnot '}'
  pure (JSStatementBlock (fpPosToAnnot pos) stmts rb defaultSemi)

-- | Parse expression statement with semicolon.
-- Detects assignment statements and method calls to produce the
-- correct AST constructor for backward compatibility.
expressionStatement :: JSParser JSStatement
expressionStatement = do
  expr <- expression
  semi <- expectStatementEnd
  pure (classifyExprStatement expr semi)

-- | Classify expression statement by its top-level form.
-- Assignment expressions become 'JSAssignStatement', call expressions
-- become 'JSMethodCall', and all others become 'JSExpressionStatement'.
classifyExprStatement :: JSExpression -> JSSemi -> JSStatement
classifyExprStatement (JSAssignExpression lhs op rhs) semi =
  JSAssignStatement lhs op rhs semi
classifyExprStatement (JSMemberExpression fn lp args rp) semi =
  JSMethodCall fn lp args rp semi
classifyExprStatement expr semi =
  JSExpressionStatement expr semi

-- | Parse empty statement: @;@
emptyStatement :: JSParser JSStatement
emptyStatement = do
  pos <- FP.getPos
  parseChar ';'
  pure (JSEmptyStatement (fpPosToAnnot pos))

-- ---------------------------------------------------------------------
-- Variable Declarations
-- ---------------------------------------------------------------------

-- | Parse variable declaration: @var x = 1, y = 2;@
variableDeclaration :: JSParser JSStatement
variableDeclaration =
  varDecl FP.<|> letDecl FP.<|> constDecl
  where
    varDecl = do
      pos <- FP.getPos
      keyword "var"
      whitespace
      decls <- sepBy1 variableDeclarator (whitespace *> parseChar ',' *> whitespace)
      semi <- expectStatementEnd
      pure (JSVariable (fpPosToAnnot pos) (listToCommaList decls) semi)
    letDecl = do
      pos <- FP.getPos
      keyword "let"
      whitespace
      decls <- sepBy1 variableDeclarator (whitespace *> parseChar ',' *> whitespace)
      semi <- expectStatementEnd
      pure (JSLet (fpPosToAnnot pos) (listToCommaList decls) semi)
    constDecl = do
      pos <- FP.getPos
      keyword "const"
      whitespace
      decls <- sepBy1 variableDeclarator (whitespace *> parseChar ',' *> whitespace)
      semi <- expectStatementEnd
      pure (JSConstant (fpPosToAnnot pos) (listToCommaList decls) semi)

-- | Parse single variable declarator: @name = initializer@
-- Supports simple identifiers, array destructuring, and object destructuring.
variableDeclarator :: JSParser JSExpression
variableDeclarator = do
  lhs <- destructuringTarget FP.<|> identTarget
  whitespace
  initializer <- FP.optional initExpr
  let initNode = maybe JSVarInitNone id initializer
  pure (JSVarInitExpression lhs initNode)
  where
    identTarget = do
      pos <- FP.getPos
      name <- identifier
      pure (JSIdentifier (fpPosToAnnot pos) name)
    destructuringTarget = arrayLiteral FP.<|> objectLiteral
    initExpr = do
      eqA <- parseCharAnnot '='
      whitespace
      expr <- assignmentExpression
      pure (JSVarInit eqA expr)

-- ---------------------------------------------------------------------
-- Control Flow
-- ---------------------------------------------------------------------

-- | Parse if statement: @if (test) consequent [else alternate]@
ifStatement :: JSParser JSStatement
ifStatement = do
  pos <- FP.getPos
  keyword "if"
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  test <- expression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  consequent <- statement
  elseClause <- FP.optional (do whitespace; ea <- keywordAnnot "else"; whitespace; alt <- statement; pure (ea, alt))
  maybe (pure (JSIf (fpPosToAnnot pos) lp test rp consequent))
        (\(ea, alt) -> pure (JSIfElse (fpPosToAnnot pos) lp test rp consequent ea alt))
        elseClause

-- | Parse while statement: @while (test) body@
whileStatement :: JSParser JSStatement
whileStatement = do
  pos <- FP.getPos
  keyword "while"
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  test <- expression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  pure (JSWhile (fpPosToAnnot pos) lp test rp body)

-- | Parse do-while statement: @do body while (test);@
doWhileStatement :: JSParser JSStatement
doWhileStatement = do
  pos <- FP.getPos
  keyword "do"
  whitespace
  body <- statement
  whitespace
  wa <- keywordAnnot "while"
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  test <- expression
  whitespace
  rp <- parseCharAnnot ')'
  semi <- expectStatementEnd
  pure (JSDoWhile (fpPosToAnnot pos) body wa lp test rp semi)

-- | Parse for statement (all variants).
-- Handles: for, for-in, for-of, for-var, for-let, for-const variants.
forStatement :: JSParser JSStatement
forStatement = forAwaitStatement FP.<|> forRegularStatement

-- | Parse for-await statement: @for await (... of ...) body@
forAwaitStatement :: JSParser JSStatement
forAwaitStatement = do
  pos <- FP.getPos
  keyword "for"
  whitespace
  awaitA <- keywordAnnot "await"
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  forAwaitBody pos awaitA lp

-- | Parse regular for statement (no await).
forRegularStatement :: JSParser JSStatement
forRegularStatement = do
  pos <- FP.getPos
  keyword "for"
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  forNormalBody pos lp

-- | Parse for-await-of variants: @for await (const x of iter) body@
forAwaitBody :: FP.Pos -> JSAnnot -> JSAnnot -> JSParser JSStatement
forAwaitBody pos awaitA lp =
  forAwaitVarOf pos awaitA lp FP.<|>
  forAwaitLetOf pos awaitA lp FP.<|>
  forAwaitConstOf pos awaitA lp FP.<|>
  forAwaitOf pos awaitA lp

-- | Parse regular for loop variants (no await).
forNormalBody :: FP.Pos -> JSAnnot -> JSParser JSStatement
forNormalBody pos lp =
  forVarIn pos lp FP.<|>
    forVarOf pos lp FP.<|>
    forVar pos lp FP.<|>
    forLetIn pos lp FP.<|>
    forLetOf pos lp FP.<|>
    forLet pos lp FP.<|>
    forConstIn pos lp FP.<|>
    forConstOf pos lp FP.<|>
    forConst pos lp FP.<|>
    forIn pos lp FP.<|>
    forOf pos lp FP.<|>
    forStandard pos lp

-- | Standard for loop: @for (init; test; update) body@
forStandard :: FP.Pos -> JSAnnot -> JSParser JSStatement
forStandard pos lp = do
  initExpr <- forInit
  whitespace
  s1 <- parseCharAnnot ';'
  whitespace
  test <- FP.optional expression
  whitespace
  s2 <- parseCharAnnot ';'
  whitespace
  update <- FP.optional expression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  let testList = maybe JSLNil JSLOne test
      updateList = maybe JSLNil JSLOne update
  pure (JSFor (fpPosToAnnot pos) lp initExpr s1 testList s2 updateList rp body)

-- | For-in loop: @for (expr in obj) body@
forIn :: FP.Pos -> JSAnnot -> JSParser JSStatement
forIn pos lp = do
  expr <- callMemberExpression
  whitespace
  inA <- keywordAnnot "in"
  whitespace
  obj <- expression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  pure (JSForIn (fpPosToAnnot pos) lp expr (JSBinOpIn inA) obj rp body)

-- | For-of loop: @for (expr of iter) body@
forOf :: FP.Pos -> JSAnnot -> JSParser JSStatement
forOf pos lp = do
  expr <- callMemberExpression
  whitespace
  ofA <- contextualKeywordAnnot "of"
  whitespace
  iter <- assignmentExpression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  pure (JSForOf (fpPosToAnnot pos) lp expr (JSBinOpOf ofA) iter rp body)

-- | For-await-of: @for await (expr of iter) body@
forAwaitOf :: FP.Pos -> JSAnnot -> JSAnnot -> JSParser JSStatement
forAwaitOf pos awaitA lp = do
  expr <- callMemberExpression
  whitespace
  ofA <- contextualKeywordAnnot "of"
  whitespace
  iter <- assignmentExpression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  pure (JSForAwaitOf (fpPosToAnnot pos) awaitA lp expr (JSBinOpOf ofA) iter rp body)

-- | For-await-var-of: @for await (var x of iter) body@
forAwaitVarOf :: FP.Pos -> JSAnnot -> JSAnnot -> JSParser JSStatement
forAwaitVarOf pos awaitA lp = do
  varA <- keywordAnnot "var"
  whitespace
  expr <- callMemberExpression
  whitespace
  ofA <- contextualKeywordAnnot "of"
  whitespace
  iter <- assignmentExpression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  pure (JSForAwaitVarOf (fpPosToAnnot pos) awaitA lp varA expr (JSBinOpOf ofA) iter rp body)

-- | For-await-let-of: @for await (let x of iter) body@
forAwaitLetOf :: FP.Pos -> JSAnnot -> JSAnnot -> JSParser JSStatement
forAwaitLetOf pos awaitA lp = do
  letA <- keywordAnnot "let"
  whitespace
  expr <- callMemberExpression
  whitespace
  ofA <- contextualKeywordAnnot "of"
  whitespace
  iter <- assignmentExpression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  pure (JSForAwaitLetOf (fpPosToAnnot pos) awaitA lp letA expr (JSBinOpOf ofA) iter rp body)

-- | For-await-const-of: @for await (const x of iter) body@
forAwaitConstOf :: FP.Pos -> JSAnnot -> JSAnnot -> JSParser JSStatement
forAwaitConstOf pos awaitA lp = do
  constA <- keywordAnnot "const"
  whitespace
  expr <- callMemberExpression
  whitespace
  ofA <- contextualKeywordAnnot "of"
  whitespace
  iter <- assignmentExpression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  pure (JSForAwaitConstOf (fpPosToAnnot pos) awaitA lp constA expr (JSBinOpOf ofA) iter rp body)

-- | For-var standard: @for (var decls; test; update) body@
forVar :: FP.Pos -> JSAnnot -> JSParser JSStatement
forVar pos lp = do
  varPos <- FP.getPos
  keyword "var"
  whitespace
  decls <- sepBy1 variableDeclarator (whitespace *> parseChar ',' *> whitespace)
  whitespace
  s1 <- parseCharAnnot ';'
  whitespace
  test <- FP.optional expression
  whitespace
  s2 <- parseCharAnnot ';'
  whitespace
  update <- FP.optional expression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  let testList = maybe JSLNil JSLOne test
      updateList = maybe JSLNil JSLOne update
  pure (JSForVar (fpPosToAnnot pos) lp (fpPosToAnnot varPos) (listToCommaList decls) s1 testList s2 updateList rp body)

-- | For-var-in: @for (var decl in obj) body@
forVarIn :: FP.Pos -> JSAnnot -> JSParser JSStatement
forVarIn pos lp = do
  varPos <- FP.getPos
  keyword "var"
  whitespace
  decl <- variableDeclarator
  whitespace
  inA <- keywordAnnot "in"
  whitespace
  obj <- expression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  pure (JSForVarIn (fpPosToAnnot pos) lp (fpPosToAnnot varPos) decl (JSBinOpIn inA) obj rp body)

-- | For-var-of: @for (var decl of iter) body@
forVarOf :: FP.Pos -> JSAnnot -> JSParser JSStatement
forVarOf pos lp = do
  varPos <- FP.getPos
  keyword "var"
  whitespace
  decl <- variableDeclarator
  whitespace
  ofA <- contextualKeywordAnnot "of"
  whitespace
  iter <- assignmentExpression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  pure (JSForVarOf (fpPosToAnnot pos) lp (fpPosToAnnot varPos) decl (JSBinOpOf ofA) iter rp body)

-- | For-let standard: @for (let decls; test; update) body@
forLet :: FP.Pos -> JSAnnot -> JSParser JSStatement
forLet pos lp = do
  letPos <- FP.getPos
  keyword "let"
  whitespace
  decls <- sepBy1 variableDeclarator (whitespace *> parseChar ',' *> whitespace)
  whitespace
  s1 <- parseCharAnnot ';'
  whitespace
  test <- FP.optional expression
  whitespace
  s2 <- parseCharAnnot ';'
  whitespace
  update <- FP.optional expression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  let testList = maybe JSLNil JSLOne test
      updateList = maybe JSLNil JSLOne update
  pure (JSForLet (fpPosToAnnot pos) lp (fpPosToAnnot letPos) (listToCommaList decls) s1 testList s2 updateList rp body)

-- | For-let-in: @for (let decl in obj) body@
forLetIn :: FP.Pos -> JSAnnot -> JSParser JSStatement
forLetIn pos lp = do
  letPos <- FP.getPos
  keyword "let"
  whitespace
  decl <- variableDeclarator
  whitespace
  inA <- keywordAnnot "in"
  whitespace
  obj <- expression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  pure (JSForLetIn (fpPosToAnnot pos) lp (fpPosToAnnot letPos) decl (JSBinOpIn inA) obj rp body)

-- | For-let-of: @for (let decl of iter) body@
forLetOf :: FP.Pos -> JSAnnot -> JSParser JSStatement
forLetOf pos lp = do
  letPos <- FP.getPos
  keyword "let"
  whitespace
  decl <- variableDeclarator
  whitespace
  ofA <- contextualKeywordAnnot "of"
  whitespace
  iter <- assignmentExpression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  pure (JSForLetOf (fpPosToAnnot pos) lp (fpPosToAnnot letPos) decl (JSBinOpOf ofA) iter rp body)

-- | For-const standard: @for (const decls; test; update) body@
forConst :: FP.Pos -> JSAnnot -> JSParser JSStatement
forConst pos lp = do
  constPos <- FP.getPos
  keyword "const"
  whitespace
  decls <- sepBy1 variableDeclarator (whitespace *> parseChar ',' *> whitespace)
  whitespace
  s1 <- parseCharAnnot ';'
  whitespace
  test <- FP.optional expression
  whitespace
  s2 <- parseCharAnnot ';'
  whitespace
  update <- FP.optional expression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  let testList = maybe JSLNil JSLOne test
      updateList = maybe JSLNil JSLOne update
  pure (JSForConst (fpPosToAnnot pos) lp (fpPosToAnnot constPos) (listToCommaList decls) s1 testList s2 updateList rp body)

-- | For-const-in: @for (const decl in obj) body@
forConstIn :: FP.Pos -> JSAnnot -> JSParser JSStatement
forConstIn pos lp = do
  constPos <- FP.getPos
  keyword "const"
  whitespace
  decl <- variableDeclarator
  whitespace
  inA <- keywordAnnot "in"
  whitespace
  obj <- expression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  pure (JSForConstIn (fpPosToAnnot pos) lp (fpPosToAnnot constPos) decl (JSBinOpIn inA) obj rp body)

-- | For-const-of: @for (const decl of iter) body@
forConstOf :: FP.Pos -> JSAnnot -> JSParser JSStatement
forConstOf pos lp = do
  constPos <- FP.getPos
  keyword "const"
  whitespace
  decl <- variableDeclarator
  whitespace
  ofA <- contextualKeywordAnnot "of"
  whitespace
  iter <- assignmentExpression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  pure (JSForConstOf (fpPosToAnnot pos) lp (fpPosToAnnot constPos) decl (JSBinOpOf ofA) iter rp body)

-- | Parse for loop init expression.
forInit :: JSParser (JSCommaList JSExpression)
forInit = do
  exprs <- sepBy assignmentExpression (whitespace *> parseChar ',' *> whitespace)
  pure (listToCommaList exprs)

-- | Parse switch statement: @switch (expr) { cases }@
switchStatement :: JSParser JSStatement
switchStatement = do
  pos <- FP.getPos
  keyword "switch"
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  discriminant <- expression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  lb <- parseCharAnnot '{'
  whitespace
  cases <- FP.many (whitespace *> (caseClause FP.<|> defaultClause))
  whitespace
  rb <- parseCharAnnot '}'
  pure (JSSwitch (fpPosToAnnot pos) lp discriminant rp lb cases rb defaultSemi)

-- | Parse case clause: @case value: statements@
caseClause :: JSParser JSSwitchParts
caseClause = do
  pos <- FP.getPos
  keyword "case"
  whitespace
  value <- expression
  whitespace
  colon <- parseCharAnnot ':'
  whitespace
  stmts <- caseBodyStatements
  pure (JSCase (fpPosToAnnot pos) value colon stmts)

-- | Parse default clause: @default: statements@
defaultClause :: JSParser JSSwitchParts
defaultClause = do
  pos <- FP.getPos
  keyword "default"
  whitespace
  colon <- parseCharAnnot ':'
  whitespace
  stmts <- caseBodyStatements
  pure (JSDefault (fpPosToAnnot pos) colon stmts)

-- | Parse statements inside case/default, stopping before next @case@, @default@, or @}@.
caseBodyStatements :: JSParser [JSStatement]
caseBodyStatements = FP.many caseBodyStatement
  where
    caseBodyStatement = do
      whitespace
      notAtCaseBoundary
      statement
    notAtCaseBoundary = do
      mc <- FP.optional (FP.lookahead (keyword "case" FP.<|> keyword "default" FP.<|> parseChar '}'))
      case mc of
        Just _ -> FP.empty
        Nothing -> pure ()

-- | Parse with statement: @with (expr) stmt@
withStatement :: JSParser JSStatement
withStatement = do
  pos <- FP.getPos
  keyword "with"
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  expr <- expression
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- statement
  pure (JSWith (fpPosToAnnot pos) lp expr rp body defaultSemi)

-- ---------------------------------------------------------------------
-- Jump Statements
-- ---------------------------------------------------------------------

-- | Parse return statement: @return expr;@
returnStatement :: JSParser JSStatement
returnStatement = do
  pos <- FP.getPos
  keyword "return"
  hasNewline <- hasLineTerminatorBeforeNext
  whitespace
  expr <- if hasNewline then pure Nothing else FP.optional expression
  semi <- expectStatementEnd
  pure (JSReturn (fpPosToAnnot pos) expr semi)

-- | Parse break statement: @break label;@
breakStatement :: JSParser JSStatement
breakStatement = do
  pos <- FP.getPos
  keyword "break"
  hasNewline <- hasLineTerminatorBeforeNext
  whitespace
  label <- if hasNewline then pure Nothing else FP.optional identName
  semi <- expectStatementEnd
  pure (JSBreak (fpPosToAnnot pos) (maybe JSIdentNone id label) semi)

-- | Parse continue statement: @continue label;@
continueStatement :: JSParser JSStatement
continueStatement = do
  pos <- FP.getPos
  keyword "continue"
  hasNewline <- hasLineTerminatorBeforeNext
  whitespace
  label <- if hasNewline then pure Nothing else FP.optional identName
  semi <- expectStatementEnd
  pure (JSContinue (fpPosToAnnot pos) (maybe JSIdentNone id label) semi)

-- | Parse throw statement: @throw expr;@
throwStatement :: JSParser JSStatement
throwStatement = do
  pos <- FP.getPos
  keyword "throw"
  whitespace
  expr <- expression
  semi <- expectStatementEnd
  pure (JSThrow (fpPosToAnnot pos) expr semi)

-- | Parse debugger statement: @debugger;@
debuggerStatement :: JSParser JSStatement
debuggerStatement = do
  pos <- FP.getPos
  keyword "debugger"
  semi <- expectStatementEnd
  pure (JSDebugger (fpPosToAnnot pos) semi)

-- ---------------------------------------------------------------------
-- Function and Class Declarations
-- ---------------------------------------------------------------------

-- | Parse function declaration: @function name(params) { body }@
functionDeclaration :: JSParser JSStatement
functionDeclaration = do
  pos <- FP.getPos
  keyword "function"
  whitespace
  name <- identName
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  paramList <- parseAnnotCommaListDropTrailing functionParam
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- blockBody
  semi <- expectStatementEnd
  pure (JSFunction (fpPosToAnnot pos) name lp (paramList) rp body semi)

-- | Parse async generator declaration: @async function* name(params) { body }@
asyncGeneratorDecl :: JSParser JSStatement
asyncGeneratorDecl = do
  pos <- FP.getPos
  keyword "async"
  whitespace
  funcAnnot <- keywordAnnot "function"
  whitespace
  star <- parseCharAnnot '*'
  whitespace
  name <- identName
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  paramList <- parseAnnotCommaListDropTrailing functionParam
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- blockBody
  pure (JSAsyncGenerator (fpPosToAnnot pos) funcAnnot star name lp paramList rp body defaultSemi)

-- | Parse async function declaration: @async function name(params) { body }@
asyncFunctionDecl :: JSParser JSStatement
asyncFunctionDecl = do
  pos <- FP.getPos
  keyword "async"
  whitespace
  funcAnnot <- keywordAnnot "function"
  whitespace
  name <- identName
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  paramList <- parseAnnotCommaListDropTrailing functionParam
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- blockBody
  pure (JSAsyncFunction (fpPosToAnnot pos) funcAnnot name lp (paramList) rp body defaultSemi)

-- | Parse generator declaration: @function* name(params) { body }@
generatorDecl :: JSParser JSStatement
generatorDecl = do
  pos <- FP.getPos
  keyword "function"
  whitespace
  star <- parseCharAnnot '*'
  whitespace
  name <- identName
  whitespace
  lp <- parseCharAnnot '('
  whitespace
  paramList <- parseAnnotCommaListDropTrailing functionParam
  whitespace
  rp <- parseCharAnnot ')'
  whitespace
  body <- blockBody
  pure (JSGenerator (fpPosToAnnot pos) star name lp (paramList) rp body defaultSemi)

-- | Parse class declaration: @class Name extends Super { body }@
classDeclaration :: JSParser JSStatement
classDeclaration = do
  pos <- FP.getPos
  keyword "class"
  whitespace
  name <- identName
  whitespace
  heritage <- FP.optional extendsClause
  whitespace
  lb <- parseCharAnnot '{'
  whitespace
  elements <- FP.many classElement
  whitespace
  rb <- parseCharAnnot '}'
  let ext = maybe JSExtendsNone (\(ea, e) -> JSExtends ea e) heritage
  pure (JSClass (fpPosToAnnot pos) name ext lb elements rb defaultSemi)

-- | Parse extends clause for class declarations.
extendsClause :: JSParser (JSAnnot, JSExpression)
extendsClause = do
  ea <- keywordAnnot "extends"
  whitespace
  expr <- assignmentExpression
  pure (ea, expr)

-- ---------------------------------------------------------------------
-- Exception Handling
-- ---------------------------------------------------------------------

-- | Parse try statement: @try { ... } catch (e) { ... } finally { ... }@
tryStatement :: JSParser JSStatement
tryStatement = do
  pos <- FP.getPos
  keyword "try"
  whitespace
  tryBlock <- blockBody
  whitespace
  catches <- FP.many (whitespace *> catchClause)
  whitespace
  finallyPart <- FP.optional finallyClause
  let finally = maybe JSNoFinally (\(fa, f) -> JSFinally fa f) finallyPart
  pure (JSTry (fpPosToAnnot pos) tryBlock catches finally)

-- | Parse catch clause: @catch (param) { body }@ or @catch (param if guard) { body }@
catchClause :: JSParser JSTryCatch
catchClause = do
  ca <- keywordAnnot "catch"
  whitespace
  catchWithGuard ca FP.<|> catchSimple ca FP.<|> catchNoParam ca
  where
    catchWithGuard ca = do
      lp <- parseCharAnnot '('
      whitespace
      pPos <- FP.getPos
      p <- identifier
      whitespace
      ifA <- keywordAnnot "if"
      whitespace
      guard <- expression
      whitespace
      rp <- parseCharAnnot ')'
      whitespace
      body <- blockBody
      pure (JSCatchIf ca lp (JSIdentifier (fpPosToAnnot pPos) p) ifA guard rp body)
    catchSimple ca = do
      lp <- parseCharAnnot '('
      whitespace
      param <- catchParam
      whitespace
      rp <- parseCharAnnot ')'
      whitespace
      body <- blockBody
      pure (JSCatch ca lp param rp body)
    catchParam =
      arrayLiteral FP.<|> objectLiteral FP.<|> catchIdentifier
    catchIdentifier = do
      pPos <- FP.getPos
      p <- identifier
      pure (JSIdentifier (fpPosToAnnot pPos) p)
    catchNoParam ca = do
      body <- blockBody
      pure (JSCatch ca defaultAnnot (JSIdentifier defaultAnnot "e") defaultAnnot body)

-- | Parse finally clause: @finally { body }@
finallyClause :: JSParser (JSAnnot, JSBlock)
finallyClause = do
  fa <- keywordAnnot "finally"
  whitespace
  body <- blockBody
  pure (fa, body)

-- ---------------------------------------------------------------------
-- Labeled Statements
-- ---------------------------------------------------------------------

-- | Parse labeled statement: @label: statement@
labeledStatement :: JSParser JSStatement
labeledStatement = do
  pos <- FP.getPos
  label <- rawIdentifier
  whitespace
  colon <- parseCharAnnot ':'
  whitespace
  stmt <- statement
  pure (JSLabelled (JSIdentName (fpPosToAnnot pos) label) colon stmt)

-- ---------------------------------------------------------------------
-- Import/Export Statements
-- ---------------------------------------------------------------------

-- | Parse import as a module item.
importModuleItem :: JSParser JSModuleItem
importModuleItem = do
  pos <- FP.getPos
  keyword "import"
  whitespace
  decl <- importDecl
  pure (JSModuleImportDeclaration (fpPosToAnnot pos) decl)

-- | Parse import declaration body.
importDecl :: JSParser JSImportDeclaration
importDecl =
  sideEffectImport FP.<|>
  namedImport FP.<|>
  namespaceImport FP.<|>
  defaultImport
  where
    sideEffectImport = do
      modPos <- FP.getPos
      moduleName <- stringLiteralRaw
      attrs <- importAttributes
      semi <- expectStatementEnd
      pure (JSImportDeclarationBare (fpPosToAnnot modPos) moduleName attrs semi)

    namedImport = do
      lb <- parseCharAnnot '{'
      whitespace
      specs <- sepBy importSpec (whitespace *> parseChar ',' *> whitespace)
      whitespace
      rb <- parseCharAnnot '}'
      whitespace
      from <- fromClause
      attrs <- importAttributes
      semi <- expectStatementEnd
      let named = JSImportsNamed lb (listToCommaList specs) rb
      pure (JSImportDeclaration (JSImportClauseNamed named) from attrs semi)

    namespaceImport = do
      starPos <- FP.getPos
      parseChar '*'
      whitespace
      asAnnot <- contextualKeywordAnnot "as"
      whitespace
      name <- identName
      whitespace
      from <- fromClause
      attrs <- importAttributes
      semi <- expectStatementEnd
      let ns = JSImportNameSpace (JSBinOpTimes (fpPosToAnnot starPos)) asAnnot name
      pure (JSImportDeclaration (JSImportClauseNameSpace ns) from attrs semi)

    defaultImport = do
      name <- identName
      whitespace
      rest <- FP.optional (do ca <- parseCharAnnot ','; whitespace; r <- defaultImportRest; pure (ca, r))
      whitespace
      from <- fromClause
      attrs <- importAttributes
      semi <- expectStatementEnd
      buildDefaultImport name rest from attrs semi

    buildDefaultImport name Nothing from attrs semi =
      pure (JSImportDeclaration (JSImportClauseDefault name) from attrs semi)
    buildDefaultImport name (Just (ca, Left named)) from attrs semi =
      pure (JSImportDeclaration (JSImportClauseDefaultNamed name ca named) from attrs semi)
    buildDefaultImport name (Just (ca, Right ns)) from attrs semi =
      pure (JSImportDeclaration (JSImportClauseDefaultNameSpace name ca ns) from attrs semi)

    defaultImportRest = namedImportsPart' FP.<|> namespaceImportPart'

    namedImportsPart' = do
      named <- namedImportsPart
      pure (Left named)

    namespaceImportPart' = do
      starPos <- FP.getPos
      parseChar '*'
      whitespace
      asAnnot <- contextualKeywordAnnot "as"
      whitespace
      name <- identName
      let ns = JSImportNameSpace (JSBinOpTimes (fpPosToAnnot starPos)) asAnnot name
      pure (Right ns)

    namedImportsPart = do
      lb <- parseCharAnnot '{'
      whitespace
      specs <- sepBy importSpec (whitespace *> parseChar ',' *> whitespace)
      whitespace
      rb <- parseCharAnnot '}'
      pure (JSImportsNamed lb (listToCommaList specs) rb)

-- | Parse import specifier: @name@ or @name as alias@
importSpec :: JSParser JSImportSpecifier
importSpec = do
  namePos <- FP.getPos
  name <- rawIdentifier
  whitespace
  alias <- FP.optional (do asA <- contextualKeywordAnnot "as"; whitespace; a <- identName; pure (asA, a))
  let ident = JSIdentName (fpPosToAnnot namePos) name
  maybe (pure (JSImportSpecifier ident))
        (\(asA, a) -> pure (JSImportSpecifierAs ident asA a))
        alias

-- | Parse from clause: @from "module"@
fromClause :: JSParser JSFromClause
fromClause = do
  fa <- contextualKeywordAnnot "from"
  whitespace
  modPos <- FP.getPos
  moduleName <- stringLiteralRaw
  pure (JSFromClause fa (fpPosToAnnot modPos) moduleName)

-- | Parse import attributes: @with { type: 'json' }@
importAttributes :: JSParser (Maybe JSImportAttributes)
importAttributes = FP.optional parseAttrs
  where
    parseAttrs = do
      whitespace
      withPos <- FP.getPos
      contextualKeyword "with"
      whitespace
      lbA <- parseCharAnnot '{'
      whitespace
      attrList <- parseAnnotCommaList importAttr
      whitespace
      rbA <- parseCharAnnot '}'
      pure (JSImportAttributes lbA attrList rbA)
    importAttr = do
      keyPos <- FP.getPos
      key <- identifier
      whitespace
      colonA <- parseCharAnnot ':'
      whitespace
      val <- assignmentExpression
      pure (JSImportAttribute (JSIdentName (fpPosToAnnot keyPos) key) colonA val)

-- | Parse export as a module item.
exportModuleItem :: JSParser JSModuleItem
exportModuleItem = do
  pos <- FP.getPos
  keyword "export"
  whitespace
  decl <- exportDecl
  pure (JSModuleExportDeclaration (fpPosToAnnot pos) decl)

-- | Parse export declaration body.
exportDecl :: JSParser JSExportDeclaration
exportDecl =
  exportDefault FP.<|>
  exportAllAsFrom FP.<|>
  exportAllFrom FP.<|>
  exportNamedFrom FP.<|>
  exportNamedLocals FP.<|>
  exportDeclarationStmt
  where
    exportDefault = do
      defA <- keywordAnnot "default"
      whitespace
      stmt <- statement
      pure (JSExportDefault defA stmt JSSemiAuto)

    exportAllAsFrom = do
      starA <- parseCharAnnot '*'
      whitespace
      asA <- do { p <- FP.getPos; contextualKeyword "as"; pure (fpPosToAnnot p) }
      whitespace
      namePos <- FP.getPos
      name <- identifier
      whitespace
      from <- fromClause
      semi <- expectStatementEnd
      let star = JSBinOpTimes starA
          ident = JSIdentName (fpPosToAnnot namePos) name
      pure (JSExportAllAsFrom star asA ident from semi)

    exportAllFrom = do
      starA <- parseCharAnnot '*'
      whitespace
      from <- fromClause
      semi <- expectStatementEnd
      pure (JSExportAllFrom (JSBinOpTimes starA) from semi)

    exportNamedFrom = do
      clause <- exportClause
      whitespace
      from <- fromClause
      semi <- expectStatementEnd
      pure (JSExportFrom clause from semi)

    exportNamedLocals = do
      clause <- exportClause
      semi <- expectStatementEnd
      pure (JSExportLocals clause semi)

    exportDeclarationStmt = do
      stmt <- variableDeclaration FP.<|> asyncFunctionDecl FP.<|> generatorDecl FP.<|> functionDeclaration FP.<|> classDeclaration
      semi <- expectStatementEnd
      pure (JSExport stmt semi)

-- | Parse export clause: @{ name1, name2 as alias2 }@
exportClause :: JSParser JSExportClause
exportClause = do
  lbA <- parseCharAnnot '{'
  whitespace
  specList <- parseAnnotCommaList exportSpec
  whitespace
  rbA <- parseCharAnnot '}'
  pure (JSExportClause lbA specList rbA)

-- | Parse export specifier: @name@ or @name as alias@
exportSpec :: JSParser JSExportSpecifier
exportSpec = do
  namePos <- FP.getPos
  name <- rawIdentifier
  whitespace
  alias <- FP.optional (do asA <- do { p <- FP.getPos; contextualKeyword "as"; pure (fpPosToAnnot p) }; whitespace; aPos <- FP.getPos; a <- rawIdentifier; pure (asA, aPos, a))
  let ident = JSIdentName (fpPosToAnnot namePos) name
  case alias of
    Nothing -> pure (JSExportSpecifier ident)
    Just (asA, aPos, a) -> pure (JSExportSpecifierAs ident asA (JSIdentName (fpPosToAnnot aPos) a))

-- ---------------------------------------------------------------------
-- Utilities
-- ---------------------------------------------------------------------

-- | Parse with statement position tracking.
withStatementPos :: JSParser a -> JSParser (FP.Pos, a)
withStatementPos parser = do
  pos <- FP.getPos
  result <- parser
  pure (pos, result)

-- | Check if a ByteString is a statement keyword.
isStatementKeyword :: ByteString -> Bool
isStatementKeyword kw = kw `elem` kws
  where
    kws :: [ByteString]
    kws =
      [ "var", "let", "const"
      , "function", "class"
      , "if", "else", "while", "do", "for", "switch", "case", "default"
      , "return", "break", "continue", "throw"
      , "try", "catch", "finally"
      , "import", "export"
      , "with", "debugger"
      ]
