module Language.JavaScript.Parser
  ( PA.parse,
    PA.parseModule,
    PA.readJs,
    PA.readJsModule,
    PA.parseFile,
    PA.parseFileUtf8,
    PA.showStripped,
    PA.showStrippedMaybe,

    -- * AST elements
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
    -- , ParseError(..)
    -- Source locations
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
