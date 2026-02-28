{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Compact position encoding for JavaScript source locations.
--
-- This module provides an efficient representation for source positions using
-- a single Word64 to encode both line and column information. This approach
-- significantly reduces memory overhead compared to the traditional TokenPosn
-- representation while maintaining full precision for typical JavaScript files.
--
-- ==== Position Encoding
--
-- Positions are encoded as follows:
--   * Upper 32 bits: Line number (1-based)
--   * Lower 32 bits: Column number (1-based)
--
-- This encoding supports:
--   * Lines: 1 to 4,294,967,295 (over 4 billion lines)
--   * Columns: 1 to 4,294,967,295 (over 4 billion columns per line)
--
-- ==== Examples
--
-- >>> let pos = mkPos 42 17
-- >>> posLine pos
-- 42
-- >>> posColumn pos
-- 17
--
-- >>> showPos (mkPos 1 1)
-- "1:1"
--
-- @since 0.8.0.0
module Language.JavaScript.Parser.Pos
  ( Pos (..),
    mkPos,
    posLine,
    posColumn,
    showPos,
    advanceColumn,
    advanceLine,
    noPos,
    -- * Conversion to Original AST
    posToAnnot,
    posToTokenPosn,
  )
where

import Control.DeepSeq (NFData)
import Data.Bits
import Data.Data (Data, Typeable)
import Data.Word (Word64)
import Foreign.Storable (Storable)
import GHC.Generics (Generic)

-- Imports for conversion to original AST
import Language.JavaScript.Parser.AST (JSAnnot(..))
import Language.JavaScript.Parser.SrcLocation (TokenPosn(..))
-- | Compact source position representation.
--
-- Encodes line and column information in a single Word64 value for
-- memory efficiency. Use 'mkPos' to construct and 'posLine'/'posColumn'
-- to extract components.
newtype Pos = Pos Word64
  deriving (Eq, Ord, Storable, Generic, Data, Typeable, NFData)

instance Show Pos where
  show = showPos

-- | Construct a position from line and column numbers.
--
-- Both line and column are 1-based. Values exceeding 32-bit range
-- will be truncated.
--
-- >>> mkPos 42 17
-- 42:17
--
-- >>> mkPos 0 0  -- Will be normalized to 1:1
-- 1:1
mkPos :: Int -> Int -> Pos
mkPos line col =
  let !safeeLine = max 1 line
      !safeCol = max 1 col
      !encoded = fromIntegral safeeLine `shiftL` 32 .|. fromIntegral safeCol
  in Pos encoded

-- | Extract line number from position.
--
-- >>> posLine (mkPos 42 17)
-- 42
posLine :: Pos -> Int
posLine (Pos w) = fromIntegral (w `shiftR` 32)

-- | Extract column number from position.
--
-- >>> posColumn (mkPos 42 17)
-- 17
posColumn :: Pos -> Int
posColumn (Pos w) = fromIntegral (w .&. 0xFFFFFFFF)

-- | Format position as "line:column" string.
--
-- >>> showPos (mkPos 42 17)
-- "42:17"
showPos :: Pos -> String
showPos pos = show (posLine pos) ++ ":" ++ show (posColumn pos)

-- | Advance column position by given amount.
--
-- >>> advanceColumn 5 (mkPos 10 15)
-- 10:20
advanceColumn :: Int -> Pos -> Pos
advanceColumn delta pos = mkPos (posLine pos) (posColumn pos + delta)

-- | Advance to next line, resetting column to 1.
--
-- >>> advanceLine (mkPos 10 15)
-- 11:1
advanceLine :: Pos -> Pos
advanceLine pos = mkPos (posLine pos + 1) 1

-- | No position marker for generated or unknown locations.
--
-- >>> noPos
-- 0:0
noPos :: Pos
noPos = Pos 0

-- ---------------------------------------------------------------------
-- Conversion to Original AST Types
-- ---------------------------------------------------------------------

-- | Convert a compact Pos to TokenPosn for compatibility with original AST.
--
-- Assumes character address equals (line - 1) * average_line_length + (column - 1).
-- This is an approximation since we don't track actual character addresses in Pos.
--
-- >>> posToTokenPosn (mkPos 5 10)
-- TokenPn 49 4 9
posToTokenPosn :: Pos -> TokenPosn
posToTokenPosn pos =
  let line = posLine pos
      col = posColumn pos
      -- Convert to 0-based line/column for TokenPosn
      zeroBasedLine = max 0 (line - 1)
      zeroBasedCol = max 0 (col - 1)
      -- Estimate character address (this is approximate)
      -- Using 50 chars per line as average estimate
      estimatedAddress = zeroBasedLine * 50 + zeroBasedCol
  in TokenPn estimatedAddress zeroBasedLine zeroBasedCol

-- | Convert a compact Pos to JSAnnot for direct use in original AST.
--
-- Creates a JSAnnot with the converted position and empty comment list.
-- This provides seamless integration between flatparse and original AST.
--
-- >>> posToAnnot (mkPos 5 10)
-- JSAnnot (TokenPn 49 4 9) []
posToAnnot :: Pos -> JSAnnot
posToAnnot pos = JSAnnot (posToTokenPosn pos) []