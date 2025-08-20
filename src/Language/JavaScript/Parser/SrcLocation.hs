{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}
-- | Source location tracking for JavaScript parsing.
--
-- This module provides comprehensive position tracking functionality for
-- the JavaScript parser, including position manipulation, validation,
-- and formatting operations.
--
-- ==== Position Representation
--
-- JavaScript source positions are represented by 'TokenPosn' which tracks:
--   * Address: Character offset from start of input
--   * Line: Line number (0-based)
--   * Column: Column number (0-based, 8-character tab stops)
--
-- ==== Usage Examples
--
-- > -- Create and manipulate positions
-- > let pos = TokenPn 100 5 10
-- > getLineNumber pos                 -- 5
-- > getColumn pos                     -- 10
-- > formatPositionForError pos        -- "line 5, column 10"
-- >
-- > -- Position arithmetic
-- > let advanced = advancePosition pos 5
-- > let tabPos = advanceTab pos
-- > positionOffset pos advanced       -- 5
--
-- @since 0.7.1.0
module Language.JavaScript.Parser.SrcLocation (
  -- * Position Types
    TokenPosn(..)
  , tokenPosnEmpty
  -- * Position Accessors
  , getAddress
  , getLineNumber
  , getColumn
  -- * Position Arithmetic
  , advancePosition
  , advanceTab
  , advanceToNewline
  , positionOffset
  -- * Position Utilities
  , makePosition
  , normalizePosition
  , isValidPosition
  , isStartOfLine
  , isEmptyPosition
  -- * Position Formatting
  , formatPosition
  , formatPositionForError
  -- * Position Comparison
  , compareByAddress
  , comparePositionsOnLine
  -- * Position Validation
  , isConsistentPosition
  -- * Safe Position Operations
  , safeAdvancePosition
  , safePositionOffset
  ) where

import Control.DeepSeq (NFData)
import Data.Data
import GHC.Generics (Generic)

-- | `TokenPosn' records the location of a token in the input text.  It has three
-- fields: the address (number of characters preceding the token), line number
-- and column of a token within the file.
-- Note: The lexer assumes the usual eight character tab stops.

data TokenPosn = TokenPn !Int -- address (number of characters preceding the token)
                         !Int -- line number
                         !Int -- column
        deriving (Eq, Generic, NFData, Show, Read, Data, Typeable)

-- | Empty position at the start of input.
--
-- Represents the beginning of the source file with zero address,
-- line, and column values.
--
-- >>> tokenPosnEmpty
-- TokenPn 0 0 0
--
-- @since 0.7.1.0
tokenPosnEmpty :: TokenPosn
tokenPosnEmpty = TokenPn 0 0 0

-- | Extract the character address from a position.
--
-- The address represents the number of characters from the start
-- of the input to this position.
--
-- >>> getAddress (TokenPn 100 5 10)
-- 100
--
-- @since 0.7.1.0
getAddress :: TokenPosn -> Int
getAddress (TokenPn addr _ _) = addr

-- | Extract the line number from a position.
--
-- Line numbers are 0-based, where the first line is line 0.
--
-- >>> getLineNumber (TokenPn 100 5 10)
-- 5
--
-- @since 0.7.1.0
getLineNumber :: TokenPosn -> Int
getLineNumber (TokenPn _ line _) = line

-- | Extract the column number from a position.
--
-- Column numbers are 0-based, where the first column is column 0.
-- Tab characters advance to 8-character boundaries.
--
-- >>> getColumn (TokenPn 100 5 10)
-- 10
--
-- @since 0.7.1.0
getColumn :: TokenPosn -> Int
getColumn (TokenPn _ _ col) = col

-- | Advance position by n characters on the same line.
--
-- Updates both the address and column by the specified amount,
-- while keeping the line number unchanged.
--
-- >>> advancePosition (TokenPn 10 2 5) 3
-- TokenPn 13 2 8
--
-- @since 0.7.1.0
advancePosition :: TokenPosn -> Int -> TokenPosn
advancePosition (TokenPn addr line col) n = TokenPn (addr + n) line (col + n)

-- | Advance position for a tab character.
--
-- Moves to the next 8-character tab stop and increments the address.
-- Uses standard 8-character tab stops as assumed by the lexer.
--
-- >>> advanceTab (TokenPn 0 1 3)
-- TokenPn 1 1 8
--
-- @since 0.7.1.0
advanceTab :: TokenPosn -> TokenPosn
advanceTab (TokenPn addr line col) = 
  let newCol = ((col `div` 8) + 1) * 8
  in TokenPn (addr + 1) line newCol

-- | Advance to a new line.
--
-- Sets the column to 0, updates to the specified line number,
-- and increments the address by 1 for the newline character.
--
-- >>> advanceToNewline (TokenPn 10 2 5) 3
-- TokenPn 11 3 0
--
-- @since 0.7.1.0
advanceToNewline :: TokenPosn -> Int -> TokenPosn
advanceToNewline (TokenPn addr _ _) newLine = TokenPn (addr + 1) newLine 0

-- | Calculate the character offset between two positions.
--
-- Returns the difference in addresses between the two positions.
-- Positive values indicate the second position is later.
--
-- >>> positionOffset (TokenPn 10 1 1) (TokenPn 20 2 1)
-- 10
--
-- @since 0.7.1.0
positionOffset :: TokenPosn -> TokenPosn -> Int
positionOffset (TokenPn addr1 _ _) (TokenPn addr2 _ _) = addr2 - addr1

-- | Create a position from line and column numbers.
--
-- The address defaults to 0. This is useful for creating positions
-- when only line and column information is available.
--
-- >>> makePosition 5 10
-- TokenPn 0 5 10
--
-- @since 0.7.1.0
makePosition :: Int -> Int -> TokenPosn
makePosition line col = TokenPn 0 line col

-- | Normalize a position to ensure non-negative values.
--
-- Clamps all components to be at least 0, useful for handling
-- invalid or corrupted position data.
--
-- >>> normalizePosition (TokenPn (-1) (-1) (-1))
-- TokenPn 0 0 0
--
-- @since 0.7.1.0
normalizePosition :: TokenPosn -> TokenPosn
normalizePosition (TokenPn addr line col) = 
  TokenPn (max 0 addr) (max 0 line) (max 0 col)

-- | Check if a position has valid (non-negative) components.
--
-- Returns 'True' if all address, line, and column values are
-- non-negative, 'False' otherwise.
--
-- >>> isValidPosition (TokenPn 100 5 10)
-- True
-- >>> isValidPosition (TokenPn (-1) 5 10)
-- False
--
-- @since 0.7.1.0
isValidPosition :: TokenPosn -> Bool
isValidPosition (TokenPn addr line col) = 
  addr >= 0 && line >= 0 && col >= 0

-- | Check if position is at the start of a line.
--
-- Returns 'True' if the column is 0, indicating the position
-- is at the beginning of a line.
--
-- >>> isStartOfLine (TokenPn 100 5 0)
-- True
-- >>> isStartOfLine (TokenPn 100 5 10)
-- False
--
-- @since 0.7.1.0
isStartOfLine :: TokenPosn -> Bool
isStartOfLine (TokenPn _ _ col) = col == 0

-- | Check if position represents the empty/initial position.
--
-- Returns 'True' if the position equals 'tokenPosnEmpty'.
--
-- >>> isEmptyPosition tokenPosnEmpty
-- True
-- >>> isEmptyPosition (TokenPn 1 0 0)
-- False
--
-- @since 0.7.1.0
isEmptyPosition :: TokenPosn -> Bool
isEmptyPosition pos = pos == tokenPosnEmpty

-- | Format position for detailed display.
--
-- Returns a human-readable string containing address, line,
-- and column information.
--
-- >>> formatPosition (TokenPn 100 5 10)
-- "address 100, line 5, column 10"
--
-- @since 0.7.1.0
formatPosition :: TokenPosn -> String
formatPosition (TokenPn addr line col) = 
  "address " ++ show addr ++ ", line " ++ show line ++ ", column " ++ show col

-- | Format position for error messages.
--
-- Returns a concise string suitable for error reporting,
-- containing only line and column information.
--
-- >>> formatPositionForError (TokenPn 100 5 10)
-- "line 5, column 10"
--
-- @since 0.7.1.0
formatPositionForError :: TokenPosn -> String
formatPositionForError (TokenPn _ line col) = 
  "line " ++ show line ++ ", column " ++ show col

-- | Compare positions by address.
--
-- Since 'TokenPosn' does not derive 'Ord', this function provides
-- address-based comparison for ordering positions.
--
-- >>> compareByAddress (TokenPn 10 1 1) (TokenPn 20 1 1)
-- LT
--
-- @since 0.7.1.0
compareByAddress :: TokenPosn -> TokenPosn -> Ordering
compareByAddress (TokenPn addr1 _ _) (TokenPn addr2 _ _) = compare addr1 addr2

-- | Compare positions within the same line by column.
--
-- Useful for ordering positions that are known to be on the same line.
-- Only compares column numbers, ignoring address and line.
--
-- >>> comparePositionsOnLine (TokenPn 100 5 10) (TokenPn 105 5 15)
-- LT
--
-- @since 0.7.1.0
comparePositionsOnLine :: TokenPosn -> TokenPosn -> Ordering
comparePositionsOnLine (TokenPn _ _ col1) (TokenPn _ _ col2) = compare col1 col2

-- | Check if position is consistent with parsing rules.
--
-- Returns 'True' if the position follows expected conventions:
-- line 0 should have column 0 for consistency with empty position.
--
-- >>> isConsistentPosition (TokenPn 0 0 0)
-- True
-- >>> isConsistentPosition (TokenPn 0 0 5)
-- False
--
-- @since 0.7.1.0
isConsistentPosition :: TokenPosn -> Bool
isConsistentPosition (TokenPn _ 0 col) = col == 0
isConsistentPosition _ = True

-- | Safely advance position by n characters, preventing overflow.
--
-- Like 'advancePosition' but guards against integer overflow by
-- clamping the address to 'maxBound' if overflow would occur.
--
-- >>> safeAdvancePosition (TokenPn (maxBound - 10) 1000 100) 5
-- TokenPn (maxBound - 5) 1000 105
--
-- @since 0.7.1.0
safeAdvancePosition :: TokenPosn -> Int -> TokenPosn
safeAdvancePosition (TokenPn addr line col) n
  | addr > maxBound - n = TokenPn maxBound line (col + n)
  | otherwise = TokenPn (addr + n) line (col + n)

-- | Safely calculate position offset, preventing negative results.
--
-- Like 'positionOffset' but ensures the result is non-negative,
-- useful when a guaranteed positive offset is required.
--
-- >>> safePositionOffset (TokenPn 20 1 1) (TokenPn 10 1 1)
-- 0
--
-- @since 0.7.1.0
safePositionOffset :: TokenPosn -> TokenPosn -> Int
safePositionOffset pos1 pos2 = max 0 (positionOffset pos1 pos2)

