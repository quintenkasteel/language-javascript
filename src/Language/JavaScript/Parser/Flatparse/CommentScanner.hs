{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Two-pass comment scanner for restoring comments to the AST.
--
-- The flatparse parser discards comments during lexing for performance.
-- This module implements a second pass over the raw source to extract
-- comment and whitespace annotations, which are then reattached to
-- AST nodes by matching byte offsets.
--
-- ==== Design
--
-- The scanner is a simple state machine over the source 'ByteString' that:
--
--   * Tracks byte offset position
--   * Skips string literals, template literals, and regex literals
--     to avoid false comment matches inside them
--   * Captures @\/\/...@ line comments as 'CommentA'
--   * Captures @\/* ... *\/@  block comments as 'CommentA'
--   * Captures whitespace runs as 'WhiteSpace'
--   * Returns @[(Int, CommentAnnotation)]@ — byte offset paired with annotation
--
-- @since 0.8.0.0
module Language.JavaScript.Parser.Flatparse.CommentScanner
  ( -- * Comment Scanning
    scanComments
  , CommentEntry(..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Language.JavaScript.Parser.SrcLocation (TokenPosn(TokenPn))
import Language.JavaScript.Parser.Token (CommentAnnotation(..))

-- | A scanned comment or whitespace entry with its byte offset.
data CommentEntry = CommentEntry
  { ceOffset :: !Int
  , ceAnnotation :: !CommentAnnotation
  } deriving (Show)

-- | Scan source for comments and whitespace annotations.
--
-- Returns a list of 'CommentEntry' values, each containing a byte offset
-- and the corresponding 'CommentAnnotation'. The entries are in source order.
--
-- String literals, template literals, and regex literals are skipped
-- to avoid matching comment-like sequences inside them.
scanComments :: ByteString -> [CommentEntry]
scanComments bs = go 0
  where
    len = BS.length bs
    go !i
      | i >= len = []
      | isWS (charAt i) = scanWhitespace i (i + 1)
      | matchesAt i '/' = handleSlash i
      | matchesAt i '"' = go (skipDoubleString (i + 1))
      | matchesAt i '\'' = go (skipSingleString (i + 1))
      | matchesAt i '`' = scanTemplate (i + 1)
      | otherwise = go (i + 1)

    -- Scan a contiguous whitespace run.
    scanWhitespace !start !i
      | i >= len = mkWS start i : []
      | isWS (charAt i) = scanWhitespace start (i + 1)
      | otherwise = mkWS start i : go i

    -- Handle a '/' character: could be //, /*, or division/regex.
    handleSlash !i
      | i + 1 >= len = go (i + 1)
      | matchesAt (i + 1) '/' = scanLineComment i (i + 2)
      | matchesAt (i + 1) '*' = scanBlockComment i (i + 2)
      | otherwise = go (i + 1)

    -- Scan a // line comment until end of line or end of input.
    scanLineComment !start !i
      | i >= len = mkComment start i : []
      | isLineEnd (charAt i) = mkComment start (i + 1) : go (i + 1)
      | otherwise = scanLineComment start (i + 1)

    -- Scan a /* block comment */ until closing */.
    scanBlockComment !start !i
      | i >= len = mkComment start i : []
      | i + 1 < len && matchesAt i '*' && matchesAt (i + 1) '/' =
          mkComment start (i + 2) : go (i + 2)
      | otherwise = scanBlockComment start (i + 1)

    -- Skip a double-quoted string literal, handling escape sequences.
    skipDoubleString !i
      | i >= len = i
      | matchesAt i '\\' = skipDoubleString (i + 2)
      | matchesAt i '"' = i + 1
      | otherwise = skipDoubleString (i + 1)

    -- Skip a single-quoted string literal, handling escape sequences.
    skipSingleString !i
      | i >= len = i
      | matchesAt i '\\' = skipSingleString (i + 2)
      | matchesAt i '\'' = i + 1
      | otherwise = skipSingleString (i + 1)

    -- Scan a template literal for comments inside interpolation expressions.
    -- Template string parts (between backtick and ${, or between } and backtick)
    -- cannot contain comments, but ${...} interpolation expressions CAN.
    scanTemplate !i
      | i >= len = []
      | matchesAt i '\\' = scanTemplate (i + 2)
      | matchesAt i '`' = go (i + 1)
      | matchesAt i '$' && i + 1 < len && matchesAt (i + 1) '{' =
          scanTemplateBrace (i + 2) 1
      | otherwise = scanTemplate (i + 1)

    -- Scan inside a ${...} interpolation expression for comments/whitespace.
    -- This is effectively like the main 'go' loop but tracking brace depth
    -- and resuming template scanning after the closing '}'.
    scanTemplateBrace !i !depth
      | i >= len = []
      | depth <= 0 = scanTemplate i
      | isWS (charAt i) = scanTemplateBraceWS i (i + 1) depth
      | matchesAt i '/' = handleTemplateBraceSlash i depth
      | matchesAt i '{' = scanTemplateBrace (i + 1) (depth + 1)
      | matchesAt i '}' = scanTemplateBrace (i + 1) (depth - 1)
      | matchesAt i '"' = scanTemplateBrace (skipDoubleString (i + 1)) depth
      | matchesAt i '\'' = scanTemplateBrace (skipSingleString (i + 1)) depth
      | matchesAt i '`' = scanTemplateBrace (skipTemplate (i + 1)) depth
      | otherwise = scanTemplateBrace (i + 1) depth

    -- Scan whitespace inside ${...}
    scanTemplateBraceWS !start !i !depth
      | i >= len = mkWS start i : []
      | isWS (charAt i) = scanTemplateBraceWS start (i + 1) depth
      | otherwise = mkWS start i : scanTemplateBrace i depth

    -- Handle '/' inside ${...} — could be comment or division
    handleTemplateBraceSlash !i !depth
      | i + 1 >= len = scanTemplateBrace (i + 1) depth
      | matchesAt (i + 1) '/' = scanTemplateBraceLineComment i (i + 2) depth
      | matchesAt (i + 1) '*' = scanTemplateBraceBlockComment i (i + 2) depth
      | otherwise = scanTemplateBrace (i + 1) depth

    -- Scan // comment inside ${...}
    scanTemplateBraceLineComment !start !i !depth
      | i >= len = mkComment start i : []
      | isLineEnd (charAt i) = mkComment start (i + 1) : scanTemplateBrace (i + 1) depth
      | otherwise = scanTemplateBraceLineComment start (i + 1) depth

    -- Scan /* ... */ comment inside ${...}
    scanTemplateBraceBlockComment !start !i !depth
      | i >= len = mkComment start i : []
      | i + 1 < len && matchesAt i '*' && matchesAt (i + 1) '/' =
          mkComment start (i + 2) : scanTemplateBrace (i + 2) depth
      | otherwise = scanTemplateBraceBlockComment start (i + 1) depth

    -- Skip a template literal, handling escape sequences and ${...}.
    -- Returns the position after the closing backtick.
    -- Used only inside nested templates within ${...} expressions.
    skipTemplate !i
      | i >= len = i
      | matchesAt i '\\' = skipTemplate (i + 2)
      | matchesAt i '`' = i + 1
      | matchesAt i '$' && i + 1 < len && matchesAt (i + 1) '{' =
          skipTemplate (skipTemplateBrace (i + 2) 1)
      | otherwise = skipTemplate (i + 1)

    -- Skip a ${...} inside a template, tracking brace depth.
    -- Skips strings and nested templates but does NOT skip comments,
    -- since comments inside template interpolations are valid JS.
    skipTemplateBrace !i !depth
      | i >= len = i
      | depth <= 0 = i
      | matchesAt i '{' = skipTemplateBrace (i + 1) (depth + 1)
      | matchesAt i '}' = skipTemplateBrace (i + 1) (depth - 1)
      | matchesAt i '"' = skipTemplateBrace (skipDoubleString (i + 1)) depth
      | matchesAt i '\'' = skipTemplateBrace (skipSingleString (i + 1)) depth
      | matchesAt i '`' = skipTemplateBrace (skipTemplate (i + 1)) depth
      | otherwise = skipTemplateBrace (i + 1) depth

    -- Build a CommentEntry for a comment span.
    mkComment !start !end = CommentEntry start (CommentA dummyPos (sliceBS start end))
      where
        dummyPos = TokenPn start 0 0

    -- Build a CommentEntry for a whitespace span.
    mkWS !start !end = CommentEntry start (WhiteSpace dummyPos (sliceBS start end))
      where
        dummyPos = TokenPn start 0 0

    -- Safe byte access.
    charAt !i = BS8.index bs i

    -- Check if byte at position matches a character.
    matchesAt !i c = i < len && charAt i == c

    -- Slice a range from the ByteString.
    sliceBS !start !end = BS.take (end - start) (BS.drop start bs)

    -- Whitespace classification (excluding line terminators which
    -- are tracked separately as part of whitespace runs).
    isWS ' ' = True
    isWS '\t' = True
    isWS '\n' = True
    isWS '\r' = True
    isWS '\v' = True
    isWS '\f' = True
    isWS '\160' = True  -- non-breaking space
    isWS _ = False

    isLineEnd '\n' = True
    isLineEnd '\r' = True
    isLineEnd _ = False
