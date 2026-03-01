-- | Main entry point for the language-javascript parser library.
--
-- Re-exports all public parsing functions, AST types, pretty printing, and
-- serialization utilities. Most users should import only this module.
--
-- Three API tiers are available, from highest to lowest performance:
--
--   * 'parseByteString' / 'parseModuleByteString' — zero-copy 'ByteString' input
--   * 'parseText' / 'parseModuleText' — 'Text' input (UTF-8 encoded internally)
--   * 'parse' / 'parseModule' — 'String' input (backward compatible)
--
-- @since 0.5.0.0
module Language.JavaScript.Parser
  ( -- * String-based Parsing
    PA.parse,
    PA.parseModule,

    -- * ByteString Parsing (zero-copy, highest performance)
    PA.parseByteString,
    PA.parseModuleByteString,

    -- * Text Parsing (convenience for Text-based applications)
    PA.parseText,
    PA.parseModuleText,

    -- * File Parsing (safe — does not throw on parse error)
    PA.parseFileSafe,
    PA.parseFileUtf8Safe,

    -- * Structured Parsing (rich error types via "Language.JavaScript.Parser.Core")
    Core.parseProgramByteString,
    Core.parseModuleProgramByteString,
    Core.parseExpressionByteString,
    Core.ParseResult (..),
    Core.ParseSuccess (..),
    Core.ParseFailure (..),
    Core.ParseError (..),
    Core.formatParseError,
    Core.parseErrorPosition,

    -- * Input Validation
    PA.maxInputSize,

    -- * Display Utilities
    PA.showStripped,
    PA.showStrippedMaybe,

    -- * Deprecated (kept for backward compatibility)
    PA.parseBS,
    PA.parseModuleBS,
    PA.readJsSafe,
    PA.readJsModuleSafe,
    PA.parseFile,
    PA.parseFileUtf8,

    -- * AST Elements
    JSExpression (..),
    JSAnnot (..),
    JSBinOp (..),
    JSBlock (..),
    JSUnaryOp (..),
    JSSemi (..),
    JSAssignOp (..),
    JSTryCatch (..),
    JSTryFinally (..),
    JSStatement (..),
    JSSwitchParts (..),
    JSAST (..),
    CommentAnnotation (..),

    -- * Source Locations
    TokenPosn (..),
    tokenPosnEmpty,

    -- * Pretty Printing
    renderJS,
    renderToString,
    renderToText,

    -- * Quasi-quoters
    js,
    jsast,
    jsx,
  )
where

import Language.JavaScript.Parser.AST
import qualified Language.JavaScript.Parser.Core as Core
import qualified Language.JavaScript.Parser.Parser as PA
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token
import Language.JavaScript.Pretty.Printer
import Language.JavaScript.QQ (js, jsast, jsx)

-- EOF
