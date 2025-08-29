{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Advanced JavaScript input generators for comprehensive fuzzing.
--
-- This module provides sophisticated input generation strategies designed
-- to discover parser edge cases, trigger crashes, and explore uncommon
-- code paths through systematic input mutation and generation:
--
--   * __Malformed Input Generation__: Syntax-breaking mutations
--     Creates inputs with deliberate syntax errors, incomplete constructs,
--     invalid character sequences, and boundary condition violations.
--
--   * __Edge Case Generation__: Boundary condition testing
--     Generates JavaScript at language limits - deeply nested structures,
--     extremely long identifiers, complex escape sequences, and Unicode edge cases.
--
--   * __Mutation-Based Fuzzing__: Seed input transformation
--     Applies various mutation strategies to valid JavaScript inputs
--     including character substitution, deletion, insertion, and reordering.
--
--   * __Grammar-Based Generation__: Structured random JavaScript
--     Generates syntactically valid but semantically unusual JavaScript
--     programs to test parser behavior with unusual but legal constructs.
--
-- All generators include resource limits and early termination conditions
-- to prevent resource exhaustion during fuzzing campaigns.
--
-- ==== Examples
--
-- Generating malformed inputs:
--
-- >>> malformedInputs <- generateMalformedJS 100
-- >>> length malformedInputs
-- 100
--
-- Mutating seed inputs:
--
-- >>> mutated <- mutateFuzzInput "var x = 42;"
-- >>> putStrLn (Text.unpack mutated)
-- var x = 42;;;;;
--
-- @since 0.7.1.0
module Properties.Language.Javascript.Parser.Fuzz.FuzzGenerators
  ( -- * Malformed Input Generation
    generateMalformedJS,
    generateSyntaxErrors,
    generateIncompleteConstructs,
    generateInvalidCharacters,

    -- * Edge Case Generation
    generateEdgeCaseJS,
    generateDeepNesting,
    generateLongIdentifiers,
    generateUnicodeEdgeCases,
    generateLargeNumbers,

    -- * Mutation-Based Fuzzing
    mutateFuzzInput,
    applyRandomMutations,
    applyCharacterMutations,
    applyStructuralMutations,

    -- * Grammar-Based Generation
    generateRandomJS,
    generateValidPrograms,
    generateExpressionChains,
    generateControlFlowNesting,

    -- * Mutation Strategies
    MutationStrategy (..),
    applyMutationStrategy,
    combineMutationStrategies,
  )
where

import Control.Monad (forM, replicateM)
import Data.Char (chr, isAscii, isPrint, ord)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Random (randomIO, randomRIO)

-- ---------------------------------------------------------------------
-- Malformed Input Generation
-- ---------------------------------------------------------------------

-- | Generate collection of malformed JavaScript inputs
generateMalformedJS :: Int -> IO [Text.Text]
generateMalformedJS count = do
  let strategies =
        [ generateSyntaxErrors,
          generateIncompleteConstructs,
          generateInvalidCharacters,
          generateBrokenTokens
        ]
  let perStrategy = count `div` length strategies
  results <- forM strategies $ \strategy -> strategy perStrategy
  return $ concat results

-- | Generate inputs with deliberate syntax errors
generateSyntaxErrors :: Int -> IO [Text.Text]
generateSyntaxErrors count = replicateM count generateSingleSyntaxError

-- | Generate single syntax error input
generateSingleSyntaxError :: IO Text.Text
generateSingleSyntaxError = do
  base <- chooseBase
  errorType <- randomRIO (1, 8)
  case errorType of
    1 -> return $ base <> "(((((" -- Unmatched parentheses
    2 -> return $ base <> "{{{{{" -- Unmatched braces
    3 -> return $ base <> "if (" -- Incomplete condition
    4 -> return $ base <> "var ;" -- Missing identifier
    5 -> return $ base <> "function" -- Incomplete function
    6 -> return $ base <> "return;" -- Missing return value context
    7 -> return $ base <> "+++" -- Invalid operator sequence
    _ -> return $ base <> "var x = ," -- Missing expression
  where
    chooseBase = do
      bases <- return ["", "var x = 1; ", "function f() {", "("]
      idx <- randomRIO (0, length bases - 1)
      return $ Text.pack (bases !! idx)

-- | Generate inputs with incomplete language constructs
generateIncompleteConstructs :: Int -> IO [Text.Text]
generateIncompleteConstructs count = replicateM count generateIncompleteConstruct

-- | Generate single incomplete construct
generateIncompleteConstruct :: IO Text.Text
generateIncompleteConstruct = do
  constructType <- randomRIO (1, 10)
  case constructType of
    1 -> return "function f(" -- Incomplete parameter list
    2 -> return "if (true" -- Incomplete condition
    3 -> return "for (var i = 0" -- Incomplete for loop
    4 -> return "switch (x) {" -- Incomplete switch
    5 -> return "try {" -- Incomplete try block
    6 -> return "var x = {" -- Incomplete object literal
    7 -> return "var arr = [" -- Incomplete array literal
    8 -> return "x." -- Incomplete member access
    9 -> return "new " -- Incomplete constructor call
    _ -> return "/^" -- Incomplete regex

-- | Generate inputs with invalid character sequences
generateInvalidCharacters :: Int -> IO [Text.Text]
generateInvalidCharacters count = replicateM count generateInvalidCharInput

-- | Generate input with problematic characters
generateInvalidCharInput :: IO Text.Text
generateInvalidCharInput = do
  strategy <- randomRIO (1, 6)
  case strategy of
    1 -> generateNullBytes
    2 -> generateControlChars
    3 -> generateInvalidUnicode
    4 -> generateSurrogatePairs
    5 -> generateLongLines
    _ -> generateBinaryData

-- | Generate inputs with broken token sequences
generateBrokenTokens :: Int -> IO [Text.Text]
generateBrokenTokens count = replicateM count generateBrokenTokenInput

-- | Generate single broken token input
generateBrokenTokenInput :: IO Text.Text
generateBrokenTokenInput = do
  tokenType <- randomRIO (1, 8)
  case tokenType of
    1 -> return "\"unclosed string" -- Unclosed string
    2 -> return "/* unclosed comment" -- Unclosed comment
    3 -> return "0x" -- Incomplete hex number
    4 -> return "1e" -- Incomplete scientific notation
    5 -> return "\\u" -- Incomplete unicode escape
    6 -> return "var 123abc" -- Invalid identifier
    7 -> return "'\\x" -- Incomplete hex escape
    _ -> return "//\n\r\n" -- Mixed line endings

-- ---------------------------------------------------------------------
-- Edge Case Generation
-- ---------------------------------------------------------------------

-- | Generate JavaScript edge cases testing parser limits
generateEdgeCaseJS :: Int -> IO [Text.Text]
generateEdgeCaseJS count = do
  let strategies =
        [ generateDeepNesting,
          generateLongIdentifiers,
          generateUnicodeEdgeCases,
          generateLargeNumbers,
          generateComplexRegex,
          generateEscapeSequences
        ]
  let perStrategy = count `div` length strategies
  results <- forM strategies $ \strategy -> strategy perStrategy
  return $ concat results

-- | Generate deeply nested structures
generateDeepNesting :: Int -> IO [Text.Text]
generateDeepNesting count = replicateM count generateSingleDeepNesting

-- | Generate single deeply nested structure
generateSingleDeepNesting :: IO Text.Text
generateSingleDeepNesting = do
  depth <- randomRIO (50, 200)
  nestingType <- randomRIO (1, 5)
  case nestingType of
    1 -> return $ generateNestedParens depth
    2 -> return $ generateNestedBraces depth
    3 -> return $ generateNestedArrays depth
    4 -> return $ generateNestedObjects depth
    _ -> return $ generateNestedFunctions depth

-- | Generate extremely long identifiers
generateLongIdentifiers :: Int -> IO [Text.Text]
generateLongIdentifiers count = replicateM count generateLongIdentifier

-- | Generate single long identifier
generateLongIdentifier :: IO Text.Text
generateLongIdentifier = do
  length' <- randomRIO (1000, 10000)
  chars <- replicateM length' (randomRIO ('a', 'z'))
  return $ "var " <> Text.pack chars <> " = 1;"

-- | Generate Unicode edge cases
generateUnicodeEdgeCases :: Int -> IO [Text.Text]
generateUnicodeEdgeCases count = replicateM count generateUnicodeEdgeCase

-- | Generate single Unicode edge case
generateUnicodeEdgeCase :: IO Text.Text
generateUnicodeEdgeCase = do
  caseType <- randomRIO (1, 6)
  case caseType of
    1 -> return $ generateBidiOverride
    2 -> return $ generateZeroWidthChars
    3 -> return $ generateSurrogateChars
    4 -> return $ generateCombiningChars
    5 -> return $ generateRtlChars
    _ -> return $ generatePrivateUseChars

-- | Generate large number literals
generateLargeNumbers :: Int -> IO [Text.Text]
generateLargeNumbers count = replicateM count generateLargeNumber

-- | Generate single large number
generateLargeNumber :: IO Text.Text
generateLargeNumber = do
  numberType <- randomRIO (1, 4)
  case numberType of
    1 -> do
      -- Very large integer
      digits <- randomRIO (100, 1000)
      digitString <- replicateM digits (randomRIO ('0', '9'))
      return $ "var x = " <> Text.pack digitString <> ";"
    2 -> do
      -- Number with many decimal places
      decimals <- randomRIO (100, 500)
      decimalString <- replicateM decimals (randomRIO ('0', '9'))
      return $ "var x = 1." <> Text.pack decimalString <> ";"
    3 -> do
      -- Scientific notation with large exponent
      exp' <- randomRIO (100, 308)
      return $ "var x = 1e" <> Text.pack (show exp') <> ";"
    _ -> do
      -- Hex number with many digits
      hexDigits <- randomRIO (50, 100)
      hexString <- replicateM hexDigits generateHexChar
      return $ "var x = 0x" <> Text.pack hexString <> ";"

-- | Generate complex regular expressions
generateComplexRegex :: Int -> IO [Text.Text]
generateComplexRegex count = replicateM count generateComplexRegexSingle

-- | Generate single complex regex
generateComplexRegexSingle :: IO Text.Text
generateComplexRegexSingle = do
  complexity <- randomRIO (1, 5)
  case complexity of
    1 -> return "/(.{0,1000}){50}/g" -- Exponential backtracking
    2 -> return "/[\\u0000-\\uFFFF]{1000}/u" -- Large Unicode range
    3 -> return "/(a+)+b/" -- Nested quantifiers
    4 -> return "/(?=.*){100}/m" -- Many lookaheads
    _ -> return "/\\x00\\x01\\xFF/g" -- Hex escapes

-- | Generate complex escape sequences
generateEscapeSequences :: Int -> IO [Text.Text]
generateEscapeSequences count = replicateM count generateEscapeSequence

-- | Generate single escape sequence test
generateEscapeSequence :: IO Text.Text
generateEscapeSequence = do
  escapeType <- randomRIO (1, 6)
  case escapeType of
    1 -> return "\"\\u{10FFFF}\"" -- Max Unicode code point
    2 -> return "\"\\x00\\xFF\"" -- Null and max byte
    3 -> return "\"\\0\\1\\2\"" -- Octal escapes
    4 -> return "\"\\\\\\/\"" -- Escaped backslashes
    5 -> return "\"\\r\\n\\t\"" -- Control characters
    _ -> return "\"\\uD800\\uDC00\"" -- Surrogate pair

-- ---------------------------------------------------------------------
-- Mutation-Based Fuzzing
-- ---------------------------------------------------------------------

-- | Mutation strategies for input transformation
data MutationStrategy
  = -- | Replace random characters
    CharacterSubstitution
  | -- | Insert random characters
    CharacterInsertion
  | -- | Delete random characters
    CharacterDeletion
  | -- | Reorder language tokens
    TokenReordering
  | -- | Modify AST structure
    StructuralMutation
  | -- | Corrupt Unicode sequences
    UnicodeCorruption
  deriving (Eq, Show, Enum)

-- | Apply random mutations to input text
mutateFuzzInput :: Text.Text -> IO Text.Text
mutateFuzzInput input = do
  numMutations <- randomRIO (1, 5)
  applyRandomMutations numMutations input

-- | Apply specified number of random mutations
applyRandomMutations :: Int -> Text.Text -> IO Text.Text
applyRandomMutations 0 input = return input
applyRandomMutations n input = do
  strategy <- randomRIO (1, 6) >>= \i -> return $ toEnum (i - 1)
  mutated <- applyMutationStrategy strategy input
  applyRandomMutations (n - 1) mutated

-- | Apply specific mutation strategy
applyMutationStrategy :: MutationStrategy -> Text.Text -> IO Text.Text
applyMutationStrategy CharacterSubstitution input =
  applyCharacterMutations input substituteRandomChar
applyMutationStrategy CharacterInsertion input =
  applyCharacterMutations input insertRandomChar
applyMutationStrategy CharacterDeletion input =
  applyCharacterMutations input deleteRandomChar
applyMutationStrategy TokenReordering input =
  applyTokenReordering input
applyMutationStrategy StructuralMutation input =
  applyStructuralMutations input
applyMutationStrategy UnicodeCorruption input =
  applyUnicodeCorruption input

-- | Apply character-level mutations
applyCharacterMutations :: Text.Text -> (Text.Text -> IO Text.Text) -> IO Text.Text
applyCharacterMutations input mutationFunc
  | Text.null input = return input
  | otherwise = mutationFunc input

-- | Apply structural mutations to code
applyStructuralMutations :: Text.Text -> IO Text.Text
applyStructuralMutations input = do
  mutationType <- randomRIO (1, 5)
  case mutationType of
    1 -> return $ input <> " {" -- Add unmatched brace
    2 -> return $ "(" <> input -- Add unmatched paren
    3 -> return $ input <> ";" -- Add extra semicolon
    4 -> return $ "/*" <> input <> "*/" -- Wrap in comment
    _ -> return $ input <> input -- Duplicate content

-- | Combine multiple mutation strategies
combineMutationStrategies :: [MutationStrategy] -> Text.Text -> IO Text.Text
combineMutationStrategies [] input = return input
combineMutationStrategies (s : ss) input = do
  mutated <- applyMutationStrategy s input
  combineMutationStrategies ss mutated

-- ---------------------------------------------------------------------
-- Grammar-Based Generation
-- ---------------------------------------------------------------------

-- | Generate random valid JavaScript programs
generateRandomJS :: Int -> IO [Text.Text]
generateRandomJS count = replicateM count generateSingleRandomJS

-- | Generate single random JavaScript program
generateSingleRandomJS :: IO Text.Text
generateSingleRandomJS = do
  programType <- randomRIO (1, 5)
  case programType of
    1 -> generateValidPrograms 1 >>= return . head
    2 -> generateExpressionChains 1 >>= return . head
    3 -> generateControlFlowNesting 1 >>= return . head
    4 -> generateRandomFunction
    _ -> generateRandomClass

-- | Generate valid JavaScript programs
generateValidPrograms :: Int -> IO [Text.Text]
generateValidPrograms count = replicateM count generateValidProgram

-- | Generate single valid program
generateValidProgram :: IO Text.Text
generateValidProgram = do
  statements <- randomRIO (1, 10)
  stmtList <- replicateM statements generateRandomStatement
  return $ Text.intercalate "\n" stmtList

-- | Generate expression chains
generateExpressionChains :: Int -> IO [Text.Text]
generateExpressionChains count = replicateM count generateExpressionChain

-- | Generate single expression chain
generateExpressionChain :: IO Text.Text
generateExpressionChain = do
  chainLength <- randomRIO (5, 20)
  expressions <- replicateM chainLength generateRandomExpression
  return $ Text.intercalate " + " expressions

-- | Generate control flow nesting
generateControlFlowNesting :: Int -> IO [Text.Text]
generateControlFlowNesting count = replicateM count generateNestedControlFlow

-- | Generate nested control flow
generateNestedControlFlow :: IO Text.Text
generateNestedControlFlow = do
  depth <- randomRIO (3, 8)
  generateNestedControlFlow' depth

-- ---------------------------------------------------------------------
-- Helper Functions
-- ---------------------------------------------------------------------

-- | Generate nested parentheses
generateNestedParens :: Int -> Text.Text
generateNestedParens depth =
  Text.replicate depth "(" <> "x" <> Text.replicate depth ")"

-- | Generate nested braces
generateNestedBraces :: Int -> Text.Text
generateNestedBraces depth =
  Text.replicate depth "{" <> Text.replicate depth "}"

-- | Generate nested arrays
generateNestedArrays :: Int -> Text.Text
generateNestedArrays depth =
  Text.replicate depth "[" <> "1" <> Text.replicate depth "]"

-- | Generate nested objects
generateNestedObjects :: Int -> Text.Text
generateNestedObjects depth =
  Text.replicate depth "{x:" <> "1" <> Text.replicate depth "}"

-- | Generate nested functions
generateNestedFunctions :: Int -> Text.Text
generateNestedFunctions depth =
  let prefix = Text.replicate depth "function f(){"
      suffix = Text.replicate depth "}"
   in prefix <> "return 1;" <> suffix

-- | Generate bidirectional override characters
generateBidiOverride :: Text.Text
generateBidiOverride = "\\u202E var x = 1; \\u202C"

-- | Generate zero-width characters
generateZeroWidthChars :: Text.Text
generateZeroWidthChars = "var\\u200Bx\\u200C=\\u200D1\\uFEFF;"

-- | Generate surrogate characters
generateSurrogateChars :: Text.Text
generateSurrogateChars = "var x = \"\\uD800\\uDC00\";"

-- | Generate combining characters
generateCombiningChars :: Text.Text
generateCombiningChars = "var x\\u0301\\u0308 = 1;"

-- | Generate right-to-left characters
generateRtlChars :: Text.Text
generateRtlChars = "var \\u05D0\\u05D1 = 1;"

-- | Generate private use characters
generatePrivateUseChars :: Text.Text
generatePrivateUseChars = "var \\uE000\\uF8FF = 1;"

-- | Generate hex character
generateHexChar :: IO Char
generateHexChar = do
  choice <- randomRIO (1, 2)
  if choice == 1
    then randomRIO ('0', '9')
    else randomRIO ('A', 'F')

-- | Substitute random character
substituteRandomChar :: Text.Text -> IO Text.Text
substituteRandomChar input
  | Text.null input = return input
  | otherwise = do
    pos <- randomRIO (0, Text.length input - 1)
    newChar <- randomRIO ('\0', '\127')
    let (prefix, suffix) = Text.splitAt pos input
        remaining = Text.drop 1 suffix
    return $ prefix <> Text.singleton newChar <> remaining

-- | Insert random character
insertRandomChar :: Text.Text -> IO Text.Text
insertRandomChar input = do
  pos <- randomRIO (0, Text.length input)
  newChar <- randomRIO ('\0', '\127')
  let (prefix, suffix) = Text.splitAt pos input
  return $ prefix <> Text.singleton newChar <> suffix

-- | Delete random character
deleteRandomChar :: Text.Text -> IO Text.Text
deleteRandomChar input
  | Text.null input = return input
  | otherwise = do
    pos <- randomRIO (0, Text.length input - 1)
    let (prefix, suffix) = Text.splitAt pos input
        remaining = Text.drop 1 suffix
    return $ prefix <> remaining

-- | Apply token reordering
applyTokenReordering :: Text.Text -> IO Text.Text
applyTokenReordering input = do
  let tokens = Text.words input
  if length tokens < 2
    then return input
    else do
      i <- randomRIO (0, length tokens - 1)
      j <- randomRIO (0, length tokens - 1)
      let swapped = swapElements i j tokens
      return $ Text.unwords swapped

-- | Apply Unicode corruption
applyUnicodeCorruption :: Text.Text -> IO Text.Text
applyUnicodeCorruption input
  | Text.null input = return input
  | otherwise = do
    pos <- randomRIO (0, Text.length input - 1)
    let (prefix, suffix) = Text.splitAt pos input
        corrupted = case Text.uncons suffix of
          Nothing -> suffix
          Just (c, rest) ->
            let corruptedChar = chr ((ord c + 1) `mod` 0x10000)
             in Text.cons corruptedChar rest
    return $ prefix <> corrupted

-- | Generate random statement
generateRandomStatement :: IO Text.Text
generateRandomStatement = do
  stmtType <- randomRIO (1, 6)
  case stmtType of
    1 -> return "var x = 1;"
    2 -> return "if (true) {}"
    3 -> return "for (var i = 0; i < 10; i++) {}"
    4 -> return "function f() { return 1; }"
    5 -> return "try {} catch (e) {}"
    _ -> return "switch (x) { case 1: break; }"

-- | Generate random expression
generateRandomExpression :: IO Text.Text
generateRandomExpression = do
  exprType <- randomRIO (1, 8)
  case exprType of
    1 -> return "x"
    2 -> return "42"
    3 -> return "true"
    4 -> return "\"hello\""
    5 -> return "(x + 1)"
    6 -> return "f()"
    7 -> return "obj.prop"
    _ -> return "[1, 2, 3]"

-- | Generate random function
generateRandomFunction :: IO Text.Text
generateRandomFunction = do
  paramCount <- randomRIO (0, 5)
  params <- replicateM paramCount generateRandomParam
  let paramList = Text.intercalate ", " params
  return $ "function f(" <> paramList <> ") { return 1; }"

-- | Generate random class
generateRandomClass :: IO Text.Text
generateRandomClass = return "class C { constructor() {} method() { return 1; } }"

-- | Generate random parameter
generateRandomParam :: IO Text.Text
generateRandomParam = do
  paramType <- randomRIO (1, 3)
  case paramType of
    1 -> return "x"
    2 -> return "x = 1"
    _ -> return "...args"

-- | Generate nested control flow recursively
generateNestedControlFlow' :: Int -> IO Text.Text
generateNestedControlFlow' 0 = return "return 1;"
generateNestedControlFlow' depth = do
  controlType <- randomRIO (1, 4)
  inner <- generateNestedControlFlow' (depth - 1)
  case controlType of
    1 -> return $ "if (true) { " <> inner <> " }"
    2 -> return $ "for (var i = 0; i < 10; i++) { " <> inner <> " }"
    3 -> return $ "while (true) { " <> inner <> " break; }"
    _ -> return $ "try { " <> inner <> " } catch (e) {}"

-- | Swap elements in list
swapElements :: Int -> Int -> [a] -> [a]
swapElements i j xs
  | i == j = xs
  | i >= 0 && j >= 0 && i < length xs && j < length xs =
    let elemI = xs !! i
        elemJ = xs !! j
        swapped =
          zipWith
            ( \idx x ->
                if idx == i
                  then elemJ
                  else
                    if idx == j
                      then elemI
                      else x
            )
            [0 ..]
            xs
     in swapped
  | otherwise = xs

-- | Generate null bytes in string
generateNullBytes :: IO Text.Text
generateNullBytes = return "var x = \"\0\0\0\";"

-- | Generate control characters
generateControlChars :: IO Text.Text
generateControlChars = return "var x = \"\1\2\3\127\";"

-- | Generate invalid Unicode sequences
generateInvalidUnicode :: IO Text.Text
generateInvalidUnicode = return "var x = \"\\uD800\\uD800\";" -- Invalid surrogate pair

-- | Generate surrogate pairs
generateSurrogatePairs :: IO Text.Text
generateSurrogatePairs = return "var x = \"\\uD83D\\uDE00\";" -- Valid emoji

-- | Generate very long lines
generateLongLines :: IO Text.Text
generateLongLines = do
  length' <- randomRIO (1000, 10000)
  content <- replicateM length' (return 'x')
  return $ "var " <> Text.pack content <> " = 1;"

-- | Generate binary data
generateBinaryData :: IO Text.Text
generateBinaryData = do
  bytes <- replicateM 100 (randomRIO (0, 255))
  let chars = map (chr . fromIntegral) bytes
  return $ Text.pack chars
