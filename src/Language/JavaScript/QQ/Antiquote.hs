{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | JavaScript quasi-quoter with Haskell antiquotation support.
--
-- The @jsx@ quasi-quoter extends @jsast@ with the ability to splice
-- Haskell expressions into JavaScript source code using @${expr}@ syntax.
-- Each antiquoted expression must have type 'JSExpression' and will be
-- substituted into the corresponding position in the parsed AST.
--
-- ==== Examples
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import Language.JavaScript.QQ (jsx)
-- import Language.JavaScript.Parser.AST (JSAST, JSExpression(..))
--
-- buildGreeting :: JSExpression -> JSAST
-- buildGreeting nameExpr = [jsx| console.log("Hello " + ${nameExpr}); |]
-- @
--
-- @since 0.8.0.0
module Language.JavaScript.QQ.Antiquote
  ( -- * Quasi-quoter
    jsx,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (evaluate)
import Data.ByteString (ByteString)
import qualified Data.Data
import Data.Typeable (cast)
import qualified Data.ByteString.Char8 as BS8
import Language.Haskell.Meta.Parse (parseExp)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (dataToExpQ, liftTyped)
import qualified Data.Map.Strict as Map
import qualified Language.Haskell.TH as TH
import qualified Language.JavaScript.Parser.AST as AST
import qualified Language.JavaScript.Parser.Parser as Parser

-- | Quasi-quoter that parses JavaScript with Haskell antiquotation.
--
-- Supports @${haskellExpr}@ syntax for splicing Haskell expressions
-- into the JavaScript AST. Each spliced expression must have type
-- 'JSExpression'.
--
-- The antiquotation process:
--
--   1. Scans for @${...}@ markers in the input
--   2. Replaces each with a unique placeholder identifier
--   3. Parses the resulting JavaScript
--   4. Substitutes placeholders with the corresponding Haskell expressions
--
-- ==== Usage
--
-- @
-- [jsx| var x = ${myExpr}; |]
-- @
--
-- @since 0.8.0.0
jsx :: QuasiQuoter
jsx =
  QuasiQuoter
    { quoteExp = antiquoteJS,
      quotePat = unsupported "pattern",
      quoteType = unsupported "type",
      quoteDec = unsupported "declaration"
    }
  where
    unsupported ctx _ = fail ("jsx quasi-quoter cannot be used in a " <> ctx <> " context")

-- | Process JavaScript with antiquotation and embed the resulting AST.
--
-- Uses 'TH.runIO' to force parsing through compiled code rather than
-- the GHC bytecode interpreter, which is necessary because the flatparse
-- backend uses low-level operations incompatible with bytecode evaluation.
antiquoteJS :: String -> Q Exp
antiquoteJS input = do
  loc <- TH.location
  let (processed, spliceMap) = extractSplices input
  result <- TH.runIO (evaluate (Parser.parse processed (TH.loc_filename loc)))
  case result of
    Left err -> fail (formatError loc err)
    Right ast -> astToExpWithSplices spliceMap ast

-- | Extract @${...}@ splice markers from input, replacing each with a
-- unique placeholder identifier and building a map from placeholder
-- names to the original Haskell expression strings.
extractSplices :: String -> (String, Map.Map String String)
extractSplices = go (0 :: Int) [] Map.empty
  where
    go _ acc smap [] = (reverse acc, smap)
    go n acc smap ('$' : '{' : rest) =
      let (expr, remaining) = extractBraced 0 [] rest
          placeholder = "__qq_splice_" <> show n <> "__"
          newAcc = reverse placeholder <> acc
       in go (n + 1) newAcc (Map.insert placeholder expr smap) remaining
    go n acc smap (c : rest) = go n (c : acc) smap rest

    extractBraced :: Int -> String -> String -> (String, String)
    extractBraced _ acc [] = (reverse acc, [])
    extractBraced 0 acc ('}' : rest) = (reverse acc, rest)
    extractBraced depth acc ('{' : rest) = extractBraced (depth + 1) ('{' : acc) rest
    extractBraced depth acc ('}' : rest) = extractBraced (depth - 1) ('}' : acc) rest
    extractBraced depth acc (c : rest) = extractBraced depth (c : acc) rest

-- | Convert a parsed AST to a TH expression, replacing placeholder
-- identifiers with their corresponding Haskell expression splices.
astToExpWithSplices :: Map.Map String String -> AST.JSAST -> Q Exp
astToExpWithSplices spliceMap = dataToExpQ handler
  where
    handler :: forall a. Data.Data.Data a => a -> Maybe (Q Exp)
    handler a = (cast a >>= handleExpr) <|> (cast a >>= handleBS)

    -- | Intercept ByteString values to avoid ByteString's broken 'Data'
    -- instance (which throws in @toConstr@). Uses the 'Lift' instance
    -- from @bytestring >= 0.11.2.0@ instead.
    handleBS :: ByteString -> Maybe (Q Exp)
    handleBS bs = Just (TH.unTypeCode (liftTyped bs))

    handleExpr :: AST.JSExpression -> Maybe (Q Exp)
    handleExpr (AST.JSIdentifier _ name) =
      Map.lookup (BS8.unpack name) spliceMap >>= parseSplice
    handleExpr _ = Nothing

    parseSplice :: String -> Maybe (Q Exp)
    parseSplice exprStr =
      case parseExp exprStr of
        Left _ -> Nothing
        Right expr -> Just (pure expr)

-- | Format a parse error with Haskell source location context.
formatError :: TH.Loc -> String -> String
formatError loc err =
  "jsx: JavaScript parse error in "
    <> TH.loc_filename loc
    <> ": "
    <> err
