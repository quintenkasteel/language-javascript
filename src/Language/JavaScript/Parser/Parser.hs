module Language.JavaScript.Parser.Parser
  ( -- * Parsing
    parse,
    parseModule,
    readJs,
    readJsModule,
    -- , readJsKeepComments
    parseFile,
    parseFileUtf8,

    -- * Parsing expressions

    -- parseExpr
    parseUsing,
    showStripped,
    showStrippedMaybe,
    showStrippedString,
    showStrippedMaybeString,
  )
where

import qualified Language.JavaScript.Parser.AST as AST
import qualified Language.JavaScript.Parser.Grammar7 as P
import Language.JavaScript.Parser.Lexer
import System.IO

-- | Parse JavaScript Program (Script)
-- Parse one compound statement, or a sequence of simple statements.
-- Generally used for interactive input, such as from the command line of an interpreter.
-- Return comments in addition to the parsed statements.
parse ::
  -- | The input stream (Javascript source code).
  String ->
  -- | The name of the Javascript source (filename or input device).
  String ->
  -- | An error or maybe the abstract syntax tree (AST) of zero
  -- or more Javascript statements, plus comments.
  Either String AST.JSAST
parse = parseUsing P.parseProgram

-- | Parse JavaScript module
parseModule ::
  -- | The input stream (JavaScript source code).
  String ->
  -- | The name of the JavaScript source (filename or input device).
  String ->
  -- | An error or maybe the abstract syntax tree (AST) of zero
  -- or more JavaScript statements, plus comments.
  Either String AST.JSAST
parseModule = parseUsing P.parseModule

readJsWith ::
  (String -> String -> Either String AST.JSAST) ->
  String ->
  AST.JSAST
readJsWith f input =
  case f input "src" of
    Left msg -> error (show msg)
    Right p -> p

readJs :: String -> AST.JSAST
readJs = readJsWith parse

readJsModule :: String -> AST.JSAST
readJsModule = readJsWith parseModule

-- | Parse the given file.
-- For UTF-8 support, make sure your locale is set such that
-- "System.IO.localeEncoding" returns "utf8"
parseFile :: FilePath -> IO AST.JSAST
parseFile filename =
  do
    x <- readFile filename
    return $ readJs x

-- | Parse the given file, explicitly setting the encoding to UTF8
-- when reading it
parseFileUtf8 :: FilePath -> IO AST.JSAST
parseFileUtf8 filename =
  do
    h <- openFile filename ReadMode
    hSetEncoding h utf8
    x <- hGetContents h
    return $ readJs x

showStripped :: AST.JSAST -> String
showStripped = AST.showStripped

showStrippedMaybe :: Show a => Either a AST.JSAST -> String
showStrippedMaybe maybeAst =
  case maybeAst of
    Left msg -> "Left (" ++ show msg ++ ")"
    Right p -> "Right (" ++ AST.showStripped p ++ ")"

-- | Backward-compatible String version of showStripped
showStrippedString :: AST.JSAST -> String
showStrippedString = AST.showStripped

-- | Backward-compatible String version of showStrippedMaybe
showStrippedMaybeString :: Show a => Either a AST.JSAST -> String
showStrippedMaybeString = showStrippedMaybe

-- | Parse one compound statement, or a sequence of simple statements.
-- Generally used for interactive input, such as from the command line of an interpreter.
-- Return comments in addition to the parsed statements.
parseUsing ::
  -- | The parser to be used
  Alex AST.JSAST ->
  -- | The input stream (Javascript source code).
  String ->
  -- | The name of the Javascript source (filename or input device).
  String ->
  -- | An error or maybe the abstract syntax tree (AST) of zero
  -- or more Javascript statements, plus comments.
  Either String AST.JSAST
parseUsing p input _srcName = runAlex input p
