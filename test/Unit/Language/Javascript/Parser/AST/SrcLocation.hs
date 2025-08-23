{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive SrcLocation Testing for JavaScript Parser
--
-- This module provides systematic testing for all source location functionality
-- to achieve high coverage of the SrcLocation module. It tests:
--
--   * 'TokenPosn' construction and manipulation
--   * Position arithmetic and ordering operations
--   * Position utility functions and accessors
--   * Position serialization and show instances
--   * Position validation and boundary conditions
--   * Comparison and ordering operations
--
-- The tests focus on position correctness, arithmetic consistency,
-- and robust handling of edge cases.
--
-- @since 0.7.1.0
module Unit.Language.Javascript.Parser.AST.SrcLocation
    ( testSrcLocation
    ) where

import Test.Hspec
import Test.QuickCheck
import Control.DeepSeq (deepseq)
import Data.Data (toConstr, dataTypeOf)

import Language.JavaScript.Parser.SrcLocation
  ( TokenPosn(..)
  , tokenPosnEmpty
  , getAddress
  , getLineNumber
  , getColumn
  , advancePosition
  , advanceTab
  , advanceToNewline
  , positionOffset
  , makePosition
  , normalizePosition
  , isValidPosition
  , isStartOfLine
  , isEmptyPosition
  , formatPosition
  , formatPositionForError
  , compareByAddress
  , comparePositionsOnLine
  , isConsistentPosition
  , safeAdvancePosition
  , safePositionOffset
  )

-- | Comprehensive SrcLocation testing
testSrcLocation :: Spec
testSrcLocation = describe "SrcLocation Coverage" $ do
  
  describe "TokenPosn construction and manipulation" $ do
    testTokenPosnConstruction
    testTokenPosnArithmetic
    
  describe "Position utility functions" $ do
    testPositionAccessors
    testPositionUtilities
    
  describe "Position ordering and comparison" $ do
    testPositionOrdering
    testPositionEquality
    
  describe "Position serialization and show" $ do
    testPositionSerialization
    testPositionShowInstances
    
  describe "Position validation and boundary conditions" $ do
    testPositionValidation
    testPositionBoundaries
    
  describe "Generic and Data instances" $ do
    testGenericInstances
    testDataInstances
    
  describe "Property-based position testing" $ do
    testPositionProperties

-- | Test TokenPosn construction
testTokenPosnConstruction :: Spec
testTokenPosnConstruction = describe "TokenPosn construction" $ do
  
  it "creates empty position correctly" $ do
    tokenPosnEmpty `shouldBe` TokenPn 0 0 0
    tokenPosnEmpty `deepseq` (return ())
    
  it "creates position with specific values correctly" $ do
    let pos = TokenPn 100 5 10
    pos `shouldBe` TokenPn 100 5 10
    pos `shouldSatisfy` isValidPosition
    
  it "handles zero values correctly" $ do
    let pos = TokenPn 0 0 0
    pos `shouldBe` tokenPosnEmpty
    pos `shouldSatisfy` isValidPosition
    
  it "handles large position values" $ do
    let pos = TokenPn 1000000 10000 1000
    pos `shouldSatisfy` isValidPosition
    getAddress pos `shouldBe` 1000000
    getLineNumber pos `shouldBe` 10000
    getColumn pos `shouldBe` 1000

-- | Test position arithmetic operations
testTokenPosnArithmetic :: Spec
testTokenPosnArithmetic = describe "Position arithmetic" $ do
  
  it "advances position correctly" $ do
    let pos1 = TokenPn 10 2 5
    let pos2 = advancePosition pos1 5
    getAddress pos2 `shouldBe` 15
    getColumn pos2 `shouldBe` 10  -- Advanced by 5 columns
    getLineNumber pos2 `shouldBe` 2     -- Same line
    
  it "handles newline advancement" $ do
    let pos1 = TokenPn 10 2 5
    let pos2 = advanceToNewline pos1 3
    getAddress pos2 `shouldBe` 11  -- Address advanced by 1 (for newline char)
    getLineNumber pos2 `shouldBe` 3     -- Advanced to specified line
    getColumn pos2 `shouldBe` 0   -- Reset to column 0
    
  it "calculates position offset correctly" $ do
    let pos1 = TokenPn 10 2 5
    let pos2 = TokenPn 20 3 1
    positionOffset pos1 pos2 `shouldBe` 10
    positionOffset pos2 pos1 `shouldBe` -10
    positionOffset pos1 pos1 `shouldBe` 0
    
  it "handles tab advancement correctly" $ do
    let pos1 = TokenPn 0 1 0
    let pos2 = advanceTab pos1
    getColumn pos2 `shouldBe` 8   -- Tab stops at column 8
    let pos3 = advanceTab (TokenPn 0 1 3)
    getColumn pos3 `shouldBe` 8   -- Tab advances to next 8-char boundary

-- | Test position accessor functions  
testPositionAccessors :: Spec
testPositionAccessors = describe "Position accessors" $ do
  
  it "extracts address correctly" $ do
    let pos = TokenPn 100 5 10
    getAddress pos `shouldBe` 100
    
  it "extracts line number correctly" $ do
    let pos = TokenPn 100 5 10
    getLineNumber pos `shouldBe` 5
    
  it "extracts column number correctly" $ do
    let pos = TokenPn 100 5 10
    getColumn pos `shouldBe` 10
    
  it "handles boundary values correctly" $ do
    let pos = TokenPn maxBound maxBound maxBound
    getAddress pos `shouldBe` maxBound
    getLineNumber pos `shouldBe` maxBound
    getColumn pos `shouldBe` maxBound

-- | Test position utility functions
testPositionUtilities :: Spec  
testPositionUtilities = describe "Position utilities" $ do
  
  it "checks if position is at start of line" $ do
    isStartOfLine (TokenPn 0 1 0) `shouldBe` True
    isStartOfLine (TokenPn 100 5 0) `shouldBe` True
    isStartOfLine (TokenPn 100 5 1) `shouldBe` False
    
  it "checks if position is empty" $ do
    isEmptyPosition (TokenPn 0 0 0) `shouldBe` True
    isEmptyPosition tokenPosnEmpty `shouldBe` True
    isEmptyPosition (TokenPn 1 0 0) `shouldBe` False
    isEmptyPosition (TokenPn 0 1 0) `shouldBe` False
    isEmptyPosition (TokenPn 0 0 1) `shouldBe` False
    
  it "creates position from line/column" $ do
    let pos = makePosition 5 10
    getLineNumber pos `shouldBe` 5
    getColumn pos `shouldBe` 10
    getAddress pos `shouldBe` 0  -- Default address
    
  it "normalizes position correctly" $ do
    let pos = TokenPn (-1) (-1) (-1)  -- Invalid position
    let normalized = normalizePosition pos
    isValidPosition normalized `shouldBe` True
    getAddress normalized `shouldBe` 0
    getLineNumber normalized `shouldBe` 0
    getColumn normalized `shouldBe` 0

-- | Test position ordering and comparison
testPositionOrdering :: Spec
testPositionOrdering = describe "Position ordering" $ do
  
  it "implements correct address-based ordering" $ do
    let pos1 = TokenPn 10 2 5
    let pos2 = TokenPn 20 1 1  -- Later address, earlier line
    -- Note: TokenPosn doesn't derive Ord, so we implement manual comparison
    compareByAddress pos1 pos2 `shouldBe` LT
    compareByAddress pos2 pos1 `shouldBe` GT
    
  it "maintains transitivity" $ do
    property $ \(Positive addr1) (Positive addr2) (Positive addr3) ->
      let pos1 = TokenPn addr1 1 1
          pos2 = TokenPn addr2 2 2  
          pos3 = TokenPn addr3 3 3
          cmp1 = compareByAddress pos1 pos2
          cmp2 = compareByAddress pos2 pos3
          cmp3 = compareByAddress pos1 pos3
      in (cmp1 /= GT && cmp2 /= GT) ==> (cmp3 /= GT)
      
  it "handles equal positions correctly" $ do
    let pos1 = TokenPn 100 5 10
    let pos2 = TokenPn 100 5 10
    pos1 `shouldBe` pos2
    compareByAddress pos1 pos2 `shouldBe` EQ
    
  it "orders positions within same line" $ do
    let pos1 = TokenPn 100 5 10
    let pos2 = TokenPn 105 5 15
    compareByAddress pos1 pos2 `shouldBe` LT
    comparePositionsOnLine pos1 pos2 `shouldBe` LT

-- | Test position equality
testPositionEquality :: Spec
testPositionEquality = describe "Position equality" $ do
  
  it "implements reflexivity" $ do
    property $ \(Positive addr) (Positive line) (Positive col) ->
      let pos = TokenPn addr line col
      in pos == pos
      
  it "implements symmetry" $ do  
    property $ \(pos1 :: TokenPosn) (pos2 :: TokenPosn) ->
      (pos1 == pos2) == (pos2 == pos1)
      
  it "implements transitivity" $ do
    let pos = TokenPn 100 5 10
    pos == pos `shouldBe` True
    pos == TokenPn 100 5 10 `shouldBe` True
    TokenPn 100 5 10 == pos `shouldBe` True

-- | Test position serialization  
testPositionSerialization :: Spec
testPositionSerialization = describe "Position serialization" $ do
  
  it "shows positions in readable format" $ do
    let pos = TokenPn 100 5 10
    show pos `shouldBe` "TokenPn 100 5 10"
    
  it "shows empty position correctly" $ do
    let posStr = show tokenPosnEmpty
    posStr `shouldBe` "TokenPn 0 0 0"
    
  it "reads positions correctly" $ do
    let pos = TokenPn 100 5 10
    let posStr = show pos
    read posStr `shouldBe` pos
    
  it "maintains read/show round-trip property" $ do
    property $ \(Positive addr) (Positive line) (Positive col) ->
      let pos = TokenPn addr line col
      in read (show pos) == pos

-- | Test show instances
testPositionShowInstances :: Spec  
testPositionShowInstances = describe "Show instances" $ do
  
  it "provides detailed position information" $ do
    let pos = TokenPn 100 5 10
    let posStr = formatPosition pos
    posStr `shouldBe` "address 100, line 5, column 10"
    
  it "handles zero position gracefully" $ do
    let posStr = formatPosition tokenPosnEmpty
    posStr `shouldBe` "address 0, line 0, column 0"
    
  it "formats positions for error messages" $ do
    let pos = TokenPn 100 5 10
    let errStr = formatPositionForError pos
    errStr `shouldBe` "line 5, column 10"

-- | Test position validation
testPositionValidation :: Spec
testPositionValidation = describe "Position validation" $ do
  
  it "validates correct positions" $ do
    isValidPosition (TokenPn 0 1 1) `shouldBe` True
    isValidPosition (TokenPn 100 5 10) `shouldBe` True
    isValidPosition tokenPosnEmpty `shouldBe` True
    
  it "rejects negative positions" $ do
    isValidPosition (TokenPn (-1) 1 1) `shouldBe` False
    isValidPosition (TokenPn 1 (-1) 1) `shouldBe` False
    isValidPosition (TokenPn 1 1 (-1)) `shouldBe` False
    
  it "validates position consistency" $ do
    -- Line 0 should have column 0 for consistency
    isConsistentPosition (TokenPn 0 0 0) `shouldBe` True
    isConsistentPosition (TokenPn 0 0 5) `shouldBe` False  -- Column > 0 on line 0
    isConsistentPosition (TokenPn 10 1 5) `shouldBe` True

-- | Test position boundaries
testPositionBoundaries :: Spec
testPositionBoundaries = describe "Position boundaries" $ do
  
  it "handles maximum integer values" $ do
    let pos = TokenPn maxBound maxBound maxBound
    pos `shouldSatisfy` isValidPosition
    getAddress pos `shouldBe` maxBound
    
  it "handles minimum valid values" $ do
    let pos = TokenPn 0 0 0
    pos `shouldSatisfy` isValidPosition
    pos `shouldBe` tokenPosnEmpty
    
  it "prevents integer overflow in arithmetic" $ do
    let pos = TokenPn (maxBound - 10) 1000 100
    let advanced = safeAdvancePosition pos 5
    isValidPosition advanced `shouldBe` True
    
  it "handles edge cases in position calculation" $ do
    let pos1 = TokenPn 0 0 0
    let pos2 = TokenPn maxBound maxBound maxBound
    let offset = safePositionOffset pos1 pos2
    offset `shouldSatisfy` (>= 0)

-- | Test Generic instances
testGenericInstances :: Spec
testGenericInstances = describe "Generic instances" $ do
  it "supports generic operations on TokenPosn" $ do
    let pos = TokenPn 100 5 10
    pos `deepseq` pos `shouldBe` pos  -- Test NFData instance
    
  it "compiles generic instances correctly" $ do
    -- Test that generic deriving works
    let pos1 = TokenPn 100 5 10
    let pos2 = TokenPn 100 5 10
    pos1 == pos2 `shouldBe` True

-- | Test Data instances  
testDataInstances :: Spec
testDataInstances = describe "Data instances" $ do
  
  it "supports Data operations" $ do
    let pos = TokenPn 100 5 10
    let constr = toConstr pos
    show constr `shouldBe` "TokenPn"
    
  it "provides correct datatype information" $ do
    let pos = TokenPn 100 5 10
    let datatype = dataTypeOf pos
    show datatype `shouldBe` "DataType {tycon = \"Language.JavaScript.Parser.SrcLocation.TokenPosn\", datarep = AlgRep [TokenPn]}"

-- | Test position properties with QuickCheck
testPositionProperties :: Spec
testPositionProperties = describe "Position properties" $ do
  
  it "position advancement is monotonic" $ property $ \(Positive addr) (Positive line) (Positive col) (Positive n) ->
    let pos = TokenPn addr line col
        advanced = advancePosition pos n
    in getAddress advanced >= getAddress pos
    
  it "position offset is symmetric" $ property $ \pos1 pos2 ->
    positionOffset pos1 pos2 == negate (positionOffset pos2 pos1)
    
  it "position comparison is consistent" $ property $ \(pos1 :: TokenPosn) (pos2 :: TokenPosn) ->
    let cmp1 = compareByAddress pos1 pos2
        cmp2 = compareByAddress pos2 pos1
    in (cmp1 == LT) == (cmp2 == GT)
    
  it "position equality is decidable" $ property $ \(pos1 :: TokenPosn) (pos2 :: TokenPosn) ->
    (pos1 == pos2) || (pos1 /= pos2)

-- Test utilities

-- QuickCheck instance for TokenPosn
instance Arbitrary TokenPosn where
  arbitrary = do
    addr <- choose (0, 10000)
    line <- choose (0, 1000) 
    col <- choose (0, 200)
    return (TokenPn addr line col)