-- | Main entry point for the language-javascript parser library.
--
-- Re-exports all public parsing functions, AST types, pretty printing, and
-- serialization utilities. Most users should import only this module.
--
-- Three API tiers are available, from highest to lowest performance:
--
--   * 'parseBS' / 'parseModuleBS' — zero-copy 'ByteString' input
--   * 'parseText' / 'parseModuleText' — 'Text' input (UTF-8 encoded internally)
--   * 'parse' / 'readJsSafe' — 'String' input (backward compatible)
--
-- @since 0.5.0.0
module Language.JavaScript.Parser
  ( -- * String-based Parsing (backward compatible)
    PA.parse,
    PA.parseModule,
    PA.readJs,
    PA.readJsModule,
    PA.readJsSafe,
    PA.readJsModuleSafe,
    PA.parseFile,
    PA.parseFileUtf8,

    -- * ByteString Parsing (zero-copy, highest performance)
    PA.parseBS,
    PA.parseModuleBS,
    PA.parseSafeBS,
    PA.parseModuleSafeBS,

    -- * Text Parsing (convenience for Text-based applications)
    PA.parseText,
    PA.parseModuleText,
    PA.parseSafeText,
    PA.parseModuleSafeText,

    -- * Display Utilities
    PA.showStripped,
    PA.showStrippedMaybe,

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

    -- * XML Serialization
    renderToXML,

    -- * S-Expression Serialization
    renderToSExpr,

    -- * Quasi-quoters
    js,
    jsast,
    jsx,
  )
where

import Language.JavaScript.Parser.AST
import qualified Language.JavaScript.Parser.Parser as PA
import Language.JavaScript.Parser.SrcLocation
import Language.JavaScript.Parser.Token
import Language.JavaScript.Pretty.Printer
import Language.JavaScript.Pretty.SExpr (renderToSExpr)
import Language.JavaScript.Pretty.XML (renderToXML)
import Language.JavaScript.QQ (js, jsast, jsx)

-- EOF
