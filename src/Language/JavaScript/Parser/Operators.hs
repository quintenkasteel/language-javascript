{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 #-}

-- | Parsing utilities and operator combinators for JavaScript parsing.
--
-- This module provides two groups of functionality:
--
-- 1. Core parsing utilities used throughout the grammar (position helpers,
--    character/string matching, separator combinators, comma-list builders).
--    These are the foundation that all other parser modules build on.
--
-- 2. Operator parsers (assignment, binary, unary) and the precedence table.
--    These are self-contained — they don't depend on expression or statement
--    parsers and can be compiled independently.
--
-- Extracted from "Language.JavaScript.Parser.Grammar" to improve
-- incremental compilation: changes to operators or utilities don't
-- trigger recompilation of the 2000+ line grammar module.
--
-- @since 0.8.0.0
module Language.JavaScript.Parser.Operators
  ( -- * Position and annotation helpers
    fpPosToAnnot
  , defaultAnnot
  , defaultSemi
    -- * Character and string matching
  , parseChar
  , parseCharAnnot
  , parseString
  , parseStringAnnot
  , keywordAnnot
  , identName
    -- * Separator combinators
  , sepBy
  , sepBy1
  , sepByTrailing
    -- * Comma-list builders
  , listToCommaList
  , listToAnnotCommaList
  , parseAnnotCommaList
  , parseAnnotCommaListTrailing
  , parseAnnotCommaListDropTrailing
  , listToCommaTrailingList
    -- * Lookahead and checks
  , notFollowedBy
  , hasLineTerminatorBeforeNext
  , contextualKeyword
  , contextualKeywordAnnot
    -- * String literal scanning
  , stringLiteralRaw
  , normalCharSkip
  , escapeSkip
    -- * Statement termination
  , expectSemiOrNewline
  , expectStatementEnd
    -- * Assignment operators
  , assignmentOperator
    -- * Binary operators and precedence
  , binaryOperatorAtPrec
  , binaryOperator
    -- * Unary operators
  , unaryOperator
  ) where

import Data.ByteString (ByteString)
import qualified FlatParse.Basic as FP

import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.SrcLocation (TokenPosn(TokenPn))

import Language.JavaScript.Parser.Lexer
  ( whitespace
  , identifier
  , keyword
  , rawIdentifier
  )
import Language.JavaScript.Parser.Primitives

-- =====================================================================
-- Position and Annotation Helpers
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

-- =====================================================================
-- Character and String Matching
-- =====================================================================

-- | Parse specific ASCII character using native byte comparison.
-- Avoids UTF-8 decode overhead for the common case of matching ASCII punctuation.
{-# INLINE parseChar #-}
parseChar :: Char -> JSParser ()
parseChar c = FP.word8 (fromIntegral (fromEnum c))

-- | Parse specific character and return its annotation with position.
parseCharAnnot :: Char -> JSParser JSAnnot
parseCharAnnot c = do
  pos <- FP.getPos
  parseChar c
  pure (fpPosToAnnot pos)

-- | Parse specific ASCII string using native memcmp.
-- Takes 'ByteString' directly to avoid runtime 'pack' overhead;
-- with @OverloadedStrings@, string literals become compile-time constants.
{-# INLINE parseString #-}
parseString :: ByteString -> JSParser ()
parseString = FP.byteString

-- | Parse specific string and return its annotation with position.
parseStringAnnot :: ByteString -> JSParser JSAnnot
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

-- =====================================================================
-- Separator Combinators
-- =====================================================================

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

-- =====================================================================
-- Comma-List Builders
-- =====================================================================

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

-- =====================================================================
-- Lookahead and Checks
-- =====================================================================

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

-- =====================================================================
-- String Literal Scanning
-- =====================================================================

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

-- =====================================================================
-- Statement Termination
-- =====================================================================

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

-- =====================================================================
-- Assignment Operators
-- =====================================================================

-- | Parse assignment operator.
assignmentOperator :: JSParser JSAssignOp
assignmentOperator = do
  pos <- FP.getPos
  let a = fpPosToAnnot pos
  (parseString ">>>=" *> pure (JSUrshAssign a)) FP.<|>
    (parseString ">>=" *> pure (JSRshAssign a)) FP.<|>
    (parseString "<<=" *> pure (JSLshAssign a)) FP.<|>
    (parseString "**=" *> notFollowedBy '=' *> pure (JSExponentiationAssign a)) FP.<|>
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

-- =====================================================================
-- Binary Operators and Precedence
-- =====================================================================

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

-- | Try to parse a binary operator at or above the minimum precedence.
-- Returns (precedence, is-right-associative, operator).
--
-- Uses first-character dispatch to avoid trying all 25 operator alternatives
-- sequentially. Peeks at the first byte and dispatches to a small group
-- of 1-4 alternatives, reducing average comparisons from ~12 to ~2.
binaryOperatorAtPrec :: Int -> JSParser (Int, Bool, JSBinOp)
binaryOperatorAtPrec minPrec = do
  pos <- FP.getPos
  let a = fpPosToAnnot pos
  c <- FP.lookahead FP.anyChar
  dispatchOp a c
  where
    dispatchOp a '*' = starOps a
    dispatchOp a '/' = tryOp 10 False (parseChar '/' *> notFollowedBy '=') (JSBinOpDivide a)
    dispatchOp a '%' = tryOp 10 False (parseChar '%' *> notFollowedBy '=') (JSBinOpMod a)
    dispatchOp a '+' = tryOp 9  False (parseChar '+' *> notFollowedBy '+' *> notFollowedBy '=') (JSBinOpPlus a)
    dispatchOp a '-' = tryOp 9  False (parseChar '-' *> notFollowedBy '-' *> notFollowedBy '=') (JSBinOpMinus a)
    dispatchOp a '>' = gtOps a
    dispatchOp a '<' = ltOps a
    dispatchOp a '=' = eqOps a
    dispatchOp a '!' = neqOps a
    dispatchOp a '&' = ampOps a
    dispatchOp a '^' = tryOp 4  False (parseChar '^' *> notFollowedBy '=') (JSBinOpBitXor a)
    dispatchOp a '|' = pipeOps a
    dispatchOp a '?' = tryOp 1  False (parseString "??" *> notFollowedBy '=') (JSBinOpNullishCoalescing a)
    dispatchOp a 'i' = keywordOps a
    dispatchOp _ _   = FP.empty

    starOps a =
      tryOp 11 True  (parseString "**" *> notFollowedBy '=') (JSBinOpExponentiation a) FP.<|>
      tryOp 10 False (parseChar '*' *> notFollowedBy '*' *> notFollowedBy '=') (JSBinOpTimes a)

    gtOps a =
      tryOp 8  False (parseString ">>>" *> notFollowedBy '=') (JSBinOpUrsh a) FP.<|>
      tryOp 8  False (parseString ">>" *> notFollowedBy '>' *> notFollowedBy '=') (JSBinOpRsh a) FP.<|>
      tryOp 7  False (parseString ">=") (JSBinOpGe a) FP.<|>
      tryOp 7  False (parseChar '>' *> notFollowedBy '>' *> notFollowedBy '=') (JSBinOpGt a)

    ltOps a =
      tryOp 8  False (parseString "<<" *> notFollowedBy '=') (JSBinOpLsh a) FP.<|>
      tryOp 7  False (parseString "<=") (JSBinOpLe a) FP.<|>
      tryOp 7  False (parseChar '<' *> notFollowedBy '<' *> notFollowedBy '=') (JSBinOpLt a)

    eqOps a =
      tryOp 6  False (parseString "===") (JSBinOpStrictEq a) FP.<|>
      tryOp 6  False (parseString "==" *> notFollowedBy '=') (JSBinOpEq a)

    neqOps a =
      tryOp 6  False (parseString "!==") (JSBinOpStrictNeq a) FP.<|>
      tryOp 6  False (parseString "!=" *> notFollowedBy '=') (JSBinOpNeq a)

    ampOps a =
      tryOp 2  False (parseString "&&" *> notFollowedBy '=') (JSBinOpAnd a) FP.<|>
      tryOp 5  False (parseChar '&' *> notFollowedBy '&' *> notFollowedBy '=') (JSBinOpBitAnd a)

    pipeOps a =
      tryOp 0  False (parseString "||" *> notFollowedBy '=') (JSBinOpOr a) FP.<|>
      tryOp 3  False (parseChar '|' *> notFollowedBy '|' *> notFollowedBy '=') (JSBinOpBitOr a)

    keywordOps a =
      tryOp 7  False (keyword "instanceof") (JSBinOpInstanceOf a) FP.<|>
      tryOp 7  False (keyword "in") (JSBinOpIn a)

    tryOp prec isRight parser op
      | prec >= minPrec = parser *> pure (prec, isRight, op)
      | otherwise = FP.empty

-- | Parse binary operator (for export compatibility).
binaryOperator :: JSParser JSBinOp
binaryOperator = do
  (_, _, op) <- binaryOperatorAtPrec 0
  pure op

-- =====================================================================
-- Unary Operators
-- =====================================================================

-- | Parse unary operator (symbol-based only).
unaryOperator :: JSParser JSUnaryOp
unaryOperator = do
  pos <- FP.getPos
  let a = fpPosToAnnot pos
  (parseChar '!' *> notFollowedBy '=' *> pure (JSUnaryOpNot a)) FP.<|>
    (parseChar '~' *> pure (JSUnaryOpTilde a)) FP.<|>
    (parseChar '+' *> notFollowedBy '+' *> notFollowedBy '=' *> pure (JSUnaryOpPlus a)) FP.<|>
    (parseChar '-' *> notFollowedBy '-' *> notFollowedBy '=' *> pure (JSUnaryOpMinus a))
