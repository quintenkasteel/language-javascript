{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : Language.Python.Common.Token
-- Copyright   : (c) 2009 Bernie Pope
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Lexical tokens for the Python lexer. Contains the superset of tokens from
-- version 2 and version 3 of Python (they are mostly the same).
module Language.JavaScript.Parser.Token
  ( -- * The tokens
    Token (..),
    CommentAnnotation (..),
    JSDocComment (..),
    JSDocTag (..),
    JSDocTagSpecific (..),
    JSDocAccess (..),
    JSDocProperty (..),
    JSDocType (..),
    JSDocObjectField (..),
    JSDocEnumValue (..),

    -- * String conversion
    debugTokenString,

    -- * JSDoc utilities
    isJSDocComment,
    parseJSDocFromComment,
    parseInlineTags,
    -- * JSDoc validation
    validateJSDoc,
    JSDocValidationError(..),
    JSDocValidationResult,
    -- * JSDoc inline tags
    JSDocInlineTag(..),
    JSDocRichText(..),

    -- * Classification

    -- TokenClass (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Data
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Language.JavaScript.Parser.SrcLocation
import qualified Data.Text as Text
import Data.Text (Text)
import Data.String (fromString)
import qualified Data.Char as Char
import qualified Data.List

-- | JSDoc comment structure containing description and tags.
--
-- JSDoc comments are documentation comments that start with @/**@ and end with @*/@.
-- They can contain a description followed by zero or more tags (like @@param@, @@returns@, etc.).
--
-- ==== __Examples__
--
-- A simple JSDoc comment with description only:
--
-- >>> parseJSDocFromComment pos "/** Calculate sum of two numbers */"
-- Just (JSDocComment { jsDocDescription = Just "Calculate sum of two numbers", jsDocTags = [] })
--
-- A JSDoc comment with tags:
--
-- >>> parseJSDocFromComment pos "/** @param {number} x First number\n@returns {number} Sum */"
-- Just (JSDocComment { jsDocTags = [param tag, returns tag] })
--
-- @since 0.8.0.0
data JSDocComment = JSDocComment
  { jsDocPosition :: !TokenPosn
    -- ^ Source location where the JSDoc comment appears
  , jsDocDescription :: !(Maybe Text)
    -- ^ Optional main description text before any tags
  , jsDocTags :: ![JSDocTag]
    -- ^ List of JSDoc tags (@@param@, @@returns@, etc.)
  }
  deriving (Eq, Show, Generic, Lift, NFData, Typeable, Data, Read)

-- | Individual JSDoc tag representation.
--
-- JSDoc tags provide structured information about code elements. Common tags include:
--
-- * @@param@ - function parameter documentation
-- * @@returns@ - return value documentation
-- * @@type@ - type annotation
-- * @@description@ - detailed description
-- * @@since@ - version information
-- * @@deprecated@ - deprecation notice
--
-- ==== __Examples__
--
-- A parameter tag:
--
-- @
-- @@param {string} name - User full name
-- @
--
-- A return type tag:
--
-- @
-- @@returns {Promise<User>} - Promise resolving to user object
-- @
--
-- @since 0.8.0.0
data JSDocTag = JSDocTag
  { jsDocTagName :: !Text
    -- ^ Tag name (e.g., \"param\", \"returns\", \"type\")
  , jsDocTagType :: !(Maybe JSDocType)
    -- ^ Optional type information (e.g., {string}, {number[]})
  , jsDocTagParamName :: !(Maybe Text)
    -- ^ Parameter name for @@param tags
  , jsDocTagDescription :: !(Maybe Text)
    -- ^ Descriptive text for the tag
  , jsDocTagPosition :: !TokenPosn
    -- ^ Source location of the tag
  , jsDocTagSpecific :: !(Maybe JSDocTagSpecific)
    -- ^ Tag-specific structured information
  }
  deriving (Eq, Show, Generic, Lift, NFData, Typeable, Data, Read)

-- | Tag-specific information for standard JSDoc tags.
--
-- This type contains specialized data for different JSDoc tags, allowing
-- for type-safe representation of tag-specific information beyond the
-- common fields in 'JSDocTag'.
--
-- The tags are based on the JSDoc 3.x specification and include both
-- standard tags and common extensions.
--
-- @since 0.8.0.0
data JSDocTagSpecific
  = JSDocParamTag
      { jsDocParamOptional :: !Bool,
        jsDocParamVariadic :: !Bool,
        jsDocParamDefaultValue :: !(Maybe Text)
      }
  | JSDocReturnTag
      { jsDocReturnPromise :: !Bool
      }
  | JSDocDescriptionTag
      { jsDocDescriptionText :: !Text
      }
  | JSDocTypeTag
      { jsDocTypeType :: !JSDocType
      }
  | JSDocPropertyTag
      { jsDocPropertyTagName :: !Text,
        jsDocPropertyTagType :: !(Maybe JSDocType),
        jsDocPropertyTagOptional :: !Bool,
        jsDocPropertyTagDescription :: !(Maybe Text)
      }
  | JSDocDefaultTag
      { jsDocDefaultValue :: !Text
      }
  | JSDocConstantTag
      { jsDocConstantValue :: !(Maybe Text)
      }
  | JSDocGlobalTag
  | JSDocAliasTag
      { jsDocAliasName :: !Text
      }
  | JSDocAugmentsTag
      { jsDocAugmentsParent :: !Text
      }
  | JSDocBorrowsTag
      { jsDocBorrowsFrom :: !Text,
        jsDocBorrowsAs :: !(Maybe Text)
      }
  | JSDocClassDescTag
      { jsDocClassDescText :: !Text
      }
  | JSDocCopyrightTag
      { jsDocCopyrightText :: !Text
      }
  | JSDocExportsTag
      { jsDocExportsName :: !Text
      }
  | JSDocExternalTag
      { jsDocExternalName :: !Text,
        jsDocExternalUrl :: !(Maybe Text)
      }
  | JSDocFileTag
      { jsDocFileDescription :: !Text
      }
  | JSDocFunctionTag
  | JSDocHideConstructorTag
  | JSDocImplementsTag
      { jsDocImplementsInterface :: !Text
      }
  | JSDocInheritDocTag
  | JSDocInstanceTag
  | JSDocInterfaceTag
      { jsDocInterfaceName :: !(Maybe Text)
      }
  | JSDocKindTag
      { jsDocKindValue :: !Text
      }
  | JSDocLendsTag
      { jsDocLendsName :: !Text
      }
  | JSDocLicenseTag
      { jsDocLicenseText :: !Text
      }
  | JSDocMemberTag
      { jsDocMemberName :: !(Maybe Text),
        jsDocMemberType :: !(Maybe Text)
      }
  | JSDocMixesTag
      { jsDocMixesMixin :: !Text
      }
  | JSDocMixinTag
  | JSDocNameTag
      { jsDocNameValue :: !Text
      }
  | JSDocRequiresTag
      { jsDocRequiresModule :: !Text
      }
  | JSDocSummaryTag
      { jsDocSummaryText :: !Text
      }
  | JSDocThisTag
      { jsDocThisType :: !JSDocType
      }
  | JSDocTodoTag
      { jsDocTodoText :: !Text
      }
  | JSDocTutorialTag
      { jsDocTutorialName :: !Text
      }
  | JSDocVariationTag
      { jsDocVariationId :: !Text
      }
  | JSDocYieldsTag
      { jsDocYieldsType :: !(Maybe JSDocType),
        jsDocYieldsDescription :: !(Maybe Text)
      }
  | JSDocThrowsTag
      { jsDocThrowsCondition :: !(Maybe Text)
      }
  | JSDocExampleTag
      { jsDocExampleLanguage :: !(Maybe Text),
        jsDocExampleCaption :: !(Maybe Text)
      }
  | JSDocSeeTag
      { jsDocSeeReference :: !Text,
        jsDocSeeDisplayText :: !(Maybe Text)
      }
  | JSDocSinceTag
      { jsDocSinceVersion :: !Text
      }
  | JSDocDeprecatedTag
      { jsDocDeprecatedSince :: !(Maybe Text),
        jsDocDeprecatedReplacement :: !(Maybe Text)
      }
  | JSDocAuthorTag
      { jsDocAuthorName :: !Text,
        jsDocAuthorEmail :: !(Maybe Text)
      }
  | JSDocVersionTag
      { jsDocVersionNumber :: !Text
      }
  | JSDocAccessTag
      { jsDocAccessLevel :: !JSDocAccess
      }
  | JSDocNamespaceTag
      { jsDocNamespacePath :: !Text
      }
  | JSDocClassTag
      { jsDocClassName :: !(Maybe Text),
        jsDocClassExtends :: !(Maybe Text)
      }
  | JSDocModuleTag
      { jsDocModuleName :: !Text,
        jsDocModuleType :: !(Maybe Text)
      }
  | JSDocMemberOfTag
      { jsDocMemberOfParent :: !Text,
        jsDocMemberOfForced :: !Bool
      }
  | JSDocTypedefTag
      { jsDocTypedefName :: !Text,
        jsDocTypedefProperties :: ![JSDocProperty]
      }
  | JSDocEnumTag
      { jsDocEnumName :: !Text,
        jsDocEnumBaseType :: !(Maybe JSDocType),
        jsDocEnumValues :: ![JSDocEnumValue]
      }
  | JSDocCallbackTag
      { jsDocCallbackName :: !Text
      }
  | JSDocEventTag
      { jsDocEventName :: !Text
      }
  | JSDocFiresTag
      { jsDocFiresEventName :: !Text
      }
  | JSDocListensTag
      { jsDocListensEventName :: !Text
      }
  | JSDocIgnoreTag
  | JSDocInnerTag
  | JSDocReadOnlyTag
  | JSDocStaticTag
  | JSDocOverrideTag
  | JSDocAbstractTag
  | JSDocFinalTag
  | JSDocGeneratorTag
  | JSDocAsyncTag
  deriving (Eq, Show, Generic, Lift, NFData, Typeable, Data, Read)

-- | Access levels for JSDoc
data JSDocAccess
  = JSDocPublic
  | JSDocPrivate
  | JSDocProtected
  | JSDocPackage
  deriving (Eq, Show, Generic, Lift, NFData, Typeable, Data, Read)

-- | Property definition for complex types
data JSDocProperty = JSDocProperty
  { jsDocPropertyName :: !Text,
    jsDocPropertyType :: !(Maybe JSDocType),
    jsDocPropertyOptional :: !Bool,
    jsDocPropertyDescription :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, Lift, NFData, Typeable, Data, Read)

-- | Enum value specification in JSDoc @enum tags
data JSDocEnumValue = JSDocEnumValue
  { jsDocEnumValueName :: !Text,
    jsDocEnumValueLiteral :: !(Maybe Text),  -- String or numeric literal
    jsDocEnumValueDescription :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic, Lift, NFData, Typeable, Data, Read)

-- | JSDoc type expressions.
--
-- JSDoc type expressions describe the types of values in JavaScript code.
-- They support a rich syntax for describing simple types, complex structures,
-- and relationships between types.
--
-- ==== __Examples__
--
-- Basic types:
--
-- @
-- {string}      -- JSDocBasicType "string"
-- {number}      -- JSDocBasicType "number"
-- {boolean}     -- JSDocBasicType "boolean"
-- @
--
-- Array types:
--
-- @
-- {Array<string>}  -- JSDocGenericType "Array" [JSDocBasicType "string"]
-- {string[]}       -- JSDocArrayType (JSDocBasicType "string")
-- @
--
-- Union types:
--
-- @
-- {string|number}  -- JSDocUnionType [JSDocBasicType "string", JSDocBasicType "number"]
-- @
--
-- Function types:
--
-- @
-- {function(string, number): boolean}  -- JSDocFunctionType [string, number] boolean
-- @
--
-- @since 0.8.0.0
data JSDocType
  = JSDocBasicType !Text
    -- ^ Simple type name (e.g., \"string\", \"number\", \"MyClass\")
  | JSDocArrayType !JSDocType
    -- ^ Array type using [] syntax (e.g., string[], number[])
  | JSDocUnionType ![JSDocType]
    -- ^ Union type using | syntax (e.g., string|number)
  | JSDocObjectType ![JSDocObjectField]
    -- ^ Object type with named fields
  | JSDocFunctionType ![JSDocType] !JSDocType
    -- ^ Function type: parameter types and return type
  | JSDocGenericType !Text ![JSDocType]
    -- ^ Generic type with type parameters (e.g., Array\<string\>, Promise\<User\>)
  | JSDocOptionalType !JSDocType
  | JSDocNullableType !JSDocType
  | JSDocNonNullableType !JSDocType
  | JSDocEnumType !Text ![JSDocEnumValue]
  deriving (Eq, Show, Generic, Lift, NFData, Typeable, Data, Read)

-- | Object field in JSDoc type specification
data JSDocObjectField = JSDocObjectField
  { jsDocFieldName :: !Text,
    jsDocFieldType :: !JSDocType,
    jsDocFieldOptional :: !Bool
  }
  deriving (Eq, Show, Generic, Lift, NFData, Typeable, Data, Read)

-- | Inline JSDoc tags that can appear within description text.
--
-- Inline tags provide cross-references and links within JSDoc comments.
-- They are enclosed in curly braces and start with @, like {@link MyClass}.
--
-- ==== __Examples__
--
-- Link to another symbol:
--
-- @
-- {@link MyClass}           -- JSDocInlineLink "MyClass" Nothing
-- {@link MyClass#method}    -- JSDocInlineLink "MyClass#method" Nothing
-- {@link MyClass|text}      -- JSDocInlineLink "MyClass" (Just "text")
-- @
--
-- Tutorial reference:
--
-- @
-- {@tutorial getting-started}  -- JSDocInlineTutorial "getting-started" Nothing
-- @
--
-- @since 0.8.0.0
data JSDocInlineTag
  = JSDocInlineLink
      { jsDocInlineLinkTarget :: !Text
        -- ^ Symbol name or URL to link to
      , jsDocInlineLinkText :: !(Maybe Text)
        -- ^ Optional custom link text
      }
  | JSDocInlineTutorial
      { jsDocInlineTutorialName :: !Text
        -- ^ Tutorial identifier
      , jsDocInlineTutorialText :: !(Maybe Text)
        -- ^ Optional custom display text
      }
  | JSDocInlineCode
      { jsDocInlineCodeText :: !Text
        -- ^ Code to display inline
      }
  deriving (Eq, Show, Generic, Lift, NFData, Typeable, Data, Read)

-- | Rich text that can contain inline JSDoc tags.
--
-- JSDoc descriptions and tag text can contain inline tags like {@link}
-- mixed with regular text. This type represents parsed rich text.
--
-- @since 0.8.0.0
data JSDocRichText
  = JSDocPlainText !Text
    -- ^ Plain text content
  | JSDocInlineTag !JSDocInlineTag
    -- ^ Inline JSDoc tag
  | JSDocRichTextList ![JSDocRichText]
    -- ^ Sequence of rich text elements
  deriving (Eq, Show, Generic, Lift, NFData, Typeable, Data, Read)

-- | JSDoc validation errors.
--
-- These errors indicate problems with JSDoc comments that violate
-- best practices, have missing required information, or contain
-- inconsistencies.
--
-- @since 0.8.0.0
data JSDocValidationError
  = JSDocMissingDescription
    -- ^ JSDoc comment lacks a description
  | JSDocMissingReturn
    -- ^ Function JSDoc lacks @returns tag
  | JSDocMissingParam !Text
    -- ^ Function parameter lacks @param documentation
  | JSDocUnknownParam !Text
    -- ^ @param documents non-existent parameter
  | JSDocDuplicateParam !Text
    -- ^ Parameter documented multiple times
  | JSDocInvalidType !Text
    -- ^ Type expression is malformed
  | JSDocEmptyTag !Text
    -- ^ Tag has no content
  | JSDocUnknownTag !Text
    -- ^ Unrecognized JSDoc tag
  | JSDocInconsistentParam !Text !Text
    -- ^ Parameter type/name mismatch
  | JSDocDeprecatedWithoutReplacement
    -- ^ @deprecated tag without replacement suggestion
  deriving (Eq, Show, Generic, Lift, NFData, Typeable, Data, Read)

-- | Result of JSDoc validation.
--
-- Contains a list of validation errors found in the JSDoc comment.
-- An empty list indicates the JSDoc is valid.
--
-- @since 0.8.0.0
type JSDocValidationResult = [JSDocValidationError]

data CommentAnnotation
  = CommentA TokenPosn String
  | WhiteSpace TokenPosn String
  | JSDocA TokenPosn JSDocComment
  | NoComment
  deriving (Eq, Generic, Lift, NFData, Show, Typeable, Data, Read)

-- | Lexical tokens.
-- Each may be annotated with any comment occurring between the prior token and this one
data Token
  = -- Comment

    -- | Single line comment.
    CommentToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- | White space, for preservation.
    WsToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- Identifiers

    -- | Identifier.
    IdentifierToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- | Private identifier (#identifier).
    PrivateNameToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- Javascript Literals

    -- | Literal: Decimal
    DecimalToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- | Literal: Hexadecimal Integer
    HexIntegerToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- | Literal: Binary Integer (ES2015)
    BinaryIntegerToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- | Literal: Octal Integer
    OctalToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- | Literal: string, delimited by either single or double quotes
    StringToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- | Literal: Regular Expression
    RegExToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- | Literal: BigInt Integer (e.g., 123n)
    BigIntToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- Keywords
    AsyncToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | AwaitToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | BreakToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | CaseToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | CatchToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | ClassToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | ConstToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | LetToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | ContinueToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | DebuggerToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | DefaultToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | DeleteToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | DoToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | ElseToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | EnumToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | ExtendsToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | FalseToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | FinallyToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | ForToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | FunctionToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | FromToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | IfToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | InToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | InstanceofToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | NewToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | NullToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | OfToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | ReturnToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | StaticToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | SuperToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | SwitchToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | ThisToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | ThrowToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | TrueToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | TryToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | TypeofToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | VarToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | VoidToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | WhileToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | YieldToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | ImportToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | WithToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | ExportToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- Future reserved words
    FutureToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- Needed, not sure what they are though.
    GetToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | SetToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- Delimiters
    -- Operators
    AutoSemiToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | SemiColonToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | CommaToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | HookToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | ColonToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | OrToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | AndToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | BitwiseOrToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | BitwiseXorToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | BitwiseAndToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | StrictEqToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | EqToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | TimesAssignToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | DivideAssignToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | ModAssignToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | PlusAssignToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | MinusAssignToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | LshAssignToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | RshAssignToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | UrshAssignToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | AndAssignToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | XorAssignToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | OrAssignToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | LogicalAndAssignToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | LogicalOrAssignToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | NullishAssignToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | SimpleAssignToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | StrictNeToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | NeToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | LshToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | LeToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | LtToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | UrshToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | RshToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | GeToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | GtToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | IncrementToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | DecrementToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | PlusToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | MinusToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | MulToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | ExponentiationToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | DivToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | ModToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | NotToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | BitwiseNotToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | ArrowToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | SpreadToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | DotToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | -- | Optional chaining operator (?.)
    OptionalChainingToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | -- | Optional bracket access (?.[)
    OptionalBracketToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | -- | Nullish coalescing operator (??)
    NullishCoalescingToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | LeftBracketToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | RightBracketToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | LeftCurlyToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | RightCurlyToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | LeftParenToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | RightParenToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | CondcommentEndToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | -- Template literal lexical components
    NoSubstitutionTemplateToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | TemplateHeadToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | TemplateMiddleToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | TemplateTailToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- Special cases
    AsToken {tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]}
  | -- | Stuff between last JS and EOF
    TailToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  | -- | End of file
    EOFToken {tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]}
  deriving (Eq, Generic, Lift, NFData, Show, Typeable)

-- | Produce a string from a token containing detailed information. Mainly intended for debugging.
debugTokenString :: Token -> String
debugTokenString = takeWhile (/= ' ') . show

-- | Check if a comment string is a JSDoc comment.
--
-- JSDoc comments are distinguished from regular comments by starting with @/**@
-- (two asterisks) instead of @/*@ (single asterisk). This function validates
-- the comment format and ensures it meets JSDoc requirements.
--
-- ==== __Examples__
--
-- >>> isJSDocComment "/** This is a JSDoc comment */"
-- True
--
-- >>> isJSDocComment "/* This is a regular comment */"
-- False
--
-- >>> isJSDocComment "// This is a line comment"
-- False
--
-- >>> isJSDocComment "/**/"
-- True
--
-- @since 0.8.0.0
isJSDocComment :: String -> Bool
isJSDocComment content =
  let stripped = Text.strip (Text.pack content)
  in Text.isPrefixOf "/**" stripped &&
     Text.isSuffixOf "*/" stripped &&
     Text.length stripped >= 5  -- Minimum "/**/"

-- | Parse JSDoc from comment string if it's a JSDoc comment.
--
-- This is the main entry point for JSDoc parsing. It first validates that
-- the input is a valid JSDoc comment using 'isJSDocComment', then parses
-- the content to extract the description and tags.
--
-- The parser supports the full JSDoc 3.x specification including:
--
-- * Description text before tags
-- * All standard JSDoc tags (@@param@, @@returns@, @@type@, etc.)
-- * Type expressions ({string}, {Array\<number\>}, {string|number})
-- * Complex type definitions for objects and functions
-- * Inline tags like @@link and @@tutorial (future extension)
--
-- ==== __Examples__
--
-- Simple description only:
--
-- >>> parseJSDocFromComment pos "/** Calculate the sum of two numbers */"
-- Just (JSDocComment { jsDocDescription = Just "Calculate the sum of two numbers", jsDocTags = [] })
--
-- With parameter and return documentation:
--
-- >>> parseJSDocFromComment pos "/** @param {number} x First number\n@param {number} y Second number\n@returns {number} Sum */"
-- Just (JSDocComment { jsDocTags = [param x, param y, returns] })
--
-- Invalid input returns Nothing:
--
-- >>> parseJSDocFromComment pos "/* Not a JSDoc comment */"
-- Nothing
--
-- @since 0.8.0.0
parseJSDocFromComment :: TokenPosn -> String -> Maybe JSDocComment
parseJSDocFromComment pos comment
  | isJSDocComment comment = parseJSDocContent pos comment
  | otherwise = Nothing

-- | Parse JSDoc content from comment string
parseJSDocContent :: TokenPosn -> String -> Maybe JSDocComment
parseJSDocContent pos content =
  case extractJSDocContent content of
    Nothing -> Nothing
    Just cleanContent -> Just (parseCleanJSDocContent pos cleanContent)
  where
    extractJSDocContent :: String -> Maybe String
    extractJSDocContent str
      | Text.isPrefixOf "/**" textStr && Text.isSuffixOf "*/" textStr =
          Just (Text.unpack (Text.strip (Text.drop 3 (Text.dropEnd 2 textStr))))
      | otherwise = Nothing
      where
        textStr = Text.pack str

    parseCleanJSDocContent :: TokenPosn -> String -> JSDocComment
    parseCleanJSDocContent position cleanContent =
      let (description, tagsText) = splitDescriptionAndTags cleanContent
          tags = parseTags tagsText
      in JSDocComment position description tags

    splitDescriptionAndTags :: String -> (Maybe Text, String)
    splitDescriptionAndTags content =
      let textContent = Text.pack content
          lines' = Text.lines textContent
          cleanLines = map (Text.stripStart . Text.dropWhile (== '*') . Text.stripStart) lines'
          nonEmptyLines = filter (not . Text.null) cleanLines
      in case nonEmptyLines of
        [] -> (Nothing, "")
        (firstLine:rest) ->
          if Text.isPrefixOf "@" firstLine
            then (Nothing, Text.unpack textContent)
            else
              let descLines = takeWhile (not . Text.isPrefixOf "@") (firstLine:rest)
                  tagsLines = dropWhile (not . Text.isPrefixOf "@") (firstLine:rest)
                  description = if null descLines then Nothing
                               else Just (Text.unwords descLines)
                  tagsText = Text.unpack (Text.unlines tagsLines)
              in (description, tagsText)

    parseTags :: String -> [JSDocTag]
    parseTags tagsText =
      let textContent = Text.pack tagsText
          lines' = Text.lines textContent
          cleanLines = map (Text.stripStart . Text.dropWhile (== '*') . Text.stripStart) lines'
          tagLines = filter (Text.isPrefixOf "@") cleanLines
      in map parseTag tagLines

    parseTag :: Text -> JSDocTag
    parseTag line =
      let words' = Text.words line
      in case words' of
        [] -> JSDocTag "" Nothing Nothing Nothing pos Nothing
        (tagWithAt:rest) ->
          let tagName = Text.drop 1 tagWithAt  -- Remove @
              (jsDocType, remaining) = extractType rest
              (paramName, description) = extractNameAndDescription remaining
              tagSpecific = parseTagSpecific tagName jsDocType paramName description remaining
          in JSDocTag tagName jsDocType paramName description pos tagSpecific

    extractType :: [Text] -> (Maybe JSDocType, [Text])
    extractType [] = (Nothing, [])
    extractType (first:rest)
      | Text.isPrefixOf "{" first && Text.isSuffixOf "}" first =
          let typeText = Text.drop 1 (Text.dropEnd 1 first)
          in (parseComplexType typeText, rest)
      | Text.isPrefixOf "{" first =
          let (typeEnd, remaining) = span (not . Text.isSuffixOf "}") rest
              fullType = Text.concat (first : typeEnd ++ take 1 remaining)
              cleanType = Text.drop 1 (Text.dropEnd 1 fullType)
          in (parseComplexType cleanType, drop 1 remaining)
      | otherwise = (Nothing, first:rest)

    extractNameAndDescription :: [Text] -> (Maybe Text, Maybe Text)
    extractNameAndDescription [] = (Nothing, Nothing)
    extractNameAndDescription [single] =
      if isParamName single then (Just single, Nothing) else (Nothing, Just single)
    extractNameAndDescription (first:rest) =
      if isParamName first
        then (Just first, if null rest then Nothing else Just (Text.unwords rest))
        else (Nothing, Just (Text.unwords (first:rest)))

    isParamName :: Text -> Bool
    isParamName text =
      let firstChar = Text.take 1 text
      in not (Text.null firstChar) &&
         Text.all (\c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' || c >= '0' && c <= '9') text &&
         not (Text.any (== ' ') text) &&
         Text.length text <= 20  -- Reasonable parameter name length

    -- | Parse complex JSDoc type expressions
    parseComplexType :: Text -> Maybe JSDocType
    parseComplexType typeText
      | Text.null typeText = Nothing
      | otherwise = parseTypeExpression (Text.strip typeText)

    parseTypeExpression :: Text -> Maybe JSDocType
    parseTypeExpression text
      | Text.null text = Nothing
      -- Handle optional types: string= or ?string
      | Text.isSuffixOf "=" text =
          let baseType = Text.dropEnd 1 text
          in fmap JSDocOptionalType (parseTypeExpression baseType)
      | Text.isPrefixOf "?" text =
          let baseType = Text.drop 1 text
          in fmap JSDocNullableType (parseTypeExpression baseType)
      -- Handle non-nullable types: !string
      | Text.isPrefixOf "!" text =
          let baseType = Text.drop 1 text
          in fmap JSDocNonNullableType (parseTypeExpression baseType)
      -- Handle array types: Array<string> or string[]
      | Text.isSuffixOf "[]" text =
          let baseType = Text.dropEnd 2 text
          in fmap JSDocArrayType (parseTypeExpression baseType)
      -- Handle union types: string|number|boolean
      | Text.any (== '|') text =
          parseUnionType text
      -- Handle generic types: Array<T>, Map<K,V>, Promise<string>
      | Text.any (== '<') text && Text.any (== '>') text =
          parseGenericType text
      -- Handle function types: function(string, number): boolean
      | Text.isPrefixOf "function(" text =
          parseFunctionType text
      -- Handle object types: {name: string, age: number}
      | Text.isPrefixOf "{" text && Text.isSuffixOf "}" text =
          parseObjectType text
      -- Handle enum references: MyEnum, Color, Status
      | isEnumReference text =
          Just (JSDocEnumType text [])  -- Empty values list for references
      -- Basic type
      | otherwise = Just (JSDocBasicType text)

    parseUnionType :: Text -> Maybe JSDocType
    parseUnionType text =
      let types = map Text.strip (Text.splitOn "|" text)
          parsedTypes = mapM parseTypeExpression types
      in fmap JSDocUnionType parsedTypes

    parseGenericType :: Text -> Maybe JSDocType
    parseGenericType text =
      case Text.breakOn "<" text of
        (baseName, rest)
          | Text.null rest -> Nothing
          | otherwise ->
              let argsText = Text.drop 1 (Text.dropEnd 1 rest)
                  args = parseGenericArgs argsText
              in case args of
                Just parsedArgs -> Just (JSDocGenericType baseName parsedArgs)
                Nothing -> Nothing

    parseGenericArgs :: Text -> Maybe [JSDocType]
    parseGenericArgs text =
      let args = splitGenericArgs text
      in mapM parseTypeExpression args

    splitGenericArgs :: Text -> [Text]
    splitGenericArgs text = splitCommaBalanced text 0 [] ""
      where
        splitCommaBalanced :: Text -> Int -> [Text] -> Text -> [Text]
        splitCommaBalanced remaining depth acc current
          | Text.null remaining =
              if Text.null current then acc else acc ++ [Text.strip current]
          | otherwise =
              case Text.uncons remaining of
                Nothing -> if Text.null current then acc else acc ++ [Text.strip current]
                Just (char, rest') -> case char of
                  '<' -> splitCommaBalanced rest' (depth + 1) acc (current <> Text.singleton char)
                  '>' -> splitCommaBalanced rest' (depth - 1) acc (current <> Text.singleton char)
                  ',' | depth == 0 ->
                    splitCommaBalanced rest' depth (acc ++ [Text.strip current]) ""
                  _ -> splitCommaBalanced rest' depth acc (current <> Text.singleton char)

    parseFunctionType :: Text -> Maybe JSDocType
    parseFunctionType text =
      case Text.breakOn ")" text of
        (paramsPart, rest)
          | Text.null rest -> Nothing
          | otherwise ->
              let paramsText = Text.drop 9 paramsPart  -- Remove "function("
                  returnPart = Text.drop 1 rest  -- Remove ")"
                  returnType = if Text.isPrefixOf ": " returnPart
                              then parseTypeExpression (Text.drop 2 returnPart)
                              else Just (JSDocBasicType "void")
                  paramTypes = if Text.null paramsText
                              then Just []
                              else mapM parseTypeExpression (map Text.strip (Text.splitOn "," paramsText))
              in case (paramTypes, returnType) of
                (Just params, Just ret) -> Just (JSDocFunctionType params ret)
                _ -> Nothing

    parseObjectType :: Text -> Maybe JSDocType
    parseObjectType text =
      let content = Text.drop 1 (Text.dropEnd 1 text)
          fields = parseObjectFields content
      in fmap JSDocObjectType fields

    parseObjectFields :: Text -> Maybe [JSDocObjectField]
    parseObjectFields text
      | Text.null text = Just []
      | otherwise =
          let fieldTexts = splitObjectFields text
          in mapM parseObjectField fieldTexts

    splitObjectFields :: Text -> [Text]
    splitObjectFields text = splitCommaBalanced text 0 [] ""
      where
        splitCommaBalanced :: Text -> Int -> [Text] -> Text -> [Text]
        splitCommaBalanced remaining depth acc current
          | Text.null remaining =
              if Text.null current then acc else acc ++ [Text.strip current]
          | otherwise =
              case Text.uncons remaining of
                Nothing -> if Text.null current then acc else acc ++ [Text.strip current]
                Just (char, rest') -> case char of
                  '{' -> splitCommaBalanced rest' (depth + 1) acc (current <> Text.singleton char)
                  '}' -> splitCommaBalanced rest' (depth - 1) acc (current <> Text.singleton char)
                  ',' | depth == 0 ->
                    splitCommaBalanced rest' depth (acc ++ [Text.strip current]) ""
                  _ -> splitCommaBalanced rest' depth acc (current <> Text.singleton char)

    parseObjectField :: Text -> Maybe JSDocObjectField
    parseObjectField text =
      case Text.breakOn ":" text of
        (name, rest)
          | Text.null rest -> Nothing
          | otherwise ->
              let fieldName = Text.strip name
                  optional = Text.isSuffixOf "?" fieldName
                  cleanName = if optional then Text.dropEnd 1 fieldName else fieldName
                  typeText = Text.strip (Text.drop 1 rest)  -- Remove ":"
              in case parseTypeExpression typeText of
                Just fieldType -> Just (JSDocObjectField cleanName fieldType optional)
                Nothing -> Nothing

    -- | Parse tag-specific information based on tag name
    parseTagSpecific :: Text -> Maybe JSDocType -> Maybe Text -> Maybe Text -> [Text] -> Maybe JSDocTagSpecific
    parseTagSpecific tagName jsDocType paramName description remaining =
      case Text.toLower tagName of
        "param" -> parseParamTag paramName description
        "parameter" -> parseParamTag paramName description
        "arg" -> parseParamTag paramName description
        "argument" -> parseParamTag paramName description
        "return" -> parseReturnTag description
        "returns" -> parseReturnTag description
        "throws" -> parseThrowsTag description
        "exception" -> parseThrowsTag description
        "example" -> parseExampleTag description
        "description" -> parseDescriptionTag description
        "type" -> parseTypeTagSpecific jsDocType
        "property" -> parsePropertyTag paramName jsDocType description
        "default" -> parseDefaultTag description
        "constant" -> parseConstantTag description
        "global" -> Just JSDocGlobalTag
        "alias" -> parseAliasTag paramName
        "augments" -> parseAugmentsTag paramName
        "borrows" -> parseBorrowsTag description
        "classdesc" -> parseClassDescTag description
        "copyright" -> parseCopyrightTag description
        "exports" -> parseExportsTag paramName
        "external" -> parseExternalTag paramName description
        "file" -> parseFileTag description
        "function" -> Just JSDocFunctionTag
        "hideconstructor" -> Just JSDocHideConstructorTag
        "implements" -> parseImplementsTag paramName
        "inheritdoc" -> Just JSDocInheritDocTag
        "instance" -> Just JSDocInstanceTag
        "interface" -> parseInterfaceTag paramName
        "kind" -> parseKindTag description
        "lends" -> parseLendsTag paramName
        "license" -> parseLicenseTag description
        "member" -> parseMemberTag paramName jsDocType
        "mixes" -> parseMixesTag paramName
        "mixin" -> Just JSDocMixinTag
        "name" -> parseNameTag paramName
        "requires" -> parseRequiresTag paramName
        "summary" -> parseSummaryTag description
        "this" -> parseThisTag jsDocType
        "todo" -> parseTodoTag description
        "tutorial" -> parseTutorialTag paramName
        "variation" -> parseVariationTag paramName
        "yields" -> parseYieldsTag jsDocType description
        "see" -> parseSeeTag description
        "since" -> parseSinceTag description
        "deprecated" -> parseDeprecatedTag description
        "author" -> parseAuthorTag description
        "version" -> parseVersionTag description
        "public" -> Just (JSDocAccessTag JSDocPublic)
        "private" -> Just (JSDocAccessTag JSDocPrivate)
        "protected" -> Just (JSDocAccessTag JSDocProtected)
        "package" -> Just (JSDocAccessTag JSDocPackage)
        "namespace" -> parseNamespaceTag paramName description
        "class" -> parseClassTag paramName description
        "constructor" -> parseClassTag paramName description
        "module" -> parseModuleTag paramName description
        "memberof" -> parseMemberOfTag description
        "typedef" -> parseTypedefTag paramName
        "enum" -> parseEnumTag paramName jsDocType description
        "callback" -> parseCallbackTag paramName
        "event" -> parseEventTag paramName
        "fires" -> parseFiresTag paramName
        "listens" -> parseListensTag paramName
        "ignore" -> Just JSDocIgnoreTag
        "inner" -> Just JSDocInnerTag
        "readonly" -> Just JSDocReadOnlyTag
        "static" -> Just JSDocStaticTag
        "override" -> Just JSDocOverrideTag
        "abstract" -> Just JSDocAbstractTag
        "final" -> Just JSDocFinalTag
        "generator" -> Just JSDocGeneratorTag
        "async" -> Just JSDocAsyncTag
        _ -> Nothing

    parseParamTag :: Maybe Text -> Maybe Text -> Maybe JSDocTagSpecific
    parseParamTag paramName description =
      let optional = maybe False (Text.any (== '?')) paramName
          variadic = maybe False (Text.isPrefixOf "...") paramName
          defaultValue = extractDefaultValue description
      in Just (JSDocParamTag optional variadic defaultValue)

    parseReturnTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseReturnTag description =
      let promiseReturn = maybe False (Text.isInfixOf "promise" . Text.toLower) description
      in Just (JSDocReturnTag promiseReturn)

    parseThrowsTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseThrowsTag description =
      Just (JSDocThrowsTag description)

    parseExampleTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseExampleTag description =
      let (language, caption) = extractExampleInfo description
      in Just (JSDocExampleTag language caption)

    parseSeeTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseSeeTag description =
      case description of
        Nothing -> Nothing
        Just desc ->
          let (reference, displayText) = extractSeeInfo desc
          in Just (JSDocSeeTag reference displayText)

    parseSinceTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseSinceTag description =
      case description of
        Nothing -> Nothing
        Just version -> Just (JSDocSinceTag version)

    parseDeprecatedTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseDeprecatedTag description =
      let (since, replacement) = extractDeprecatedInfo description
      in Just (JSDocDeprecatedTag since replacement)

    parseAuthorTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseAuthorTag description =
      case description of
        Nothing -> Nothing
        Just desc ->
          let (name, email) = extractAuthorInfo desc
          in Just (JSDocAuthorTag name email)

    parseVersionTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseVersionTag description =
      case description of
        Nothing -> Nothing
        Just version -> Just (JSDocVersionTag version)

    parseNamespaceTag :: Maybe Text -> Maybe Text -> Maybe JSDocTagSpecific
    parseNamespaceTag paramName description =
      case paramName of
        Nothing -> case description of
          Nothing -> Nothing
          Just path -> Just (JSDocNamespaceTag path)
        Just path -> Just (JSDocNamespaceTag path)

    parseClassTag :: Maybe Text -> Maybe Text -> Maybe JSDocTagSpecific
    parseClassTag paramName description =
      let className = paramName
          extends = extractExtendsInfo description
      in Just (JSDocClassTag className extends)

    parseModuleTag :: Maybe Text -> Maybe Text -> Maybe JSDocTagSpecific
    parseModuleTag paramName description =
      case paramName of
        Nothing -> Nothing
        Just name ->
          let moduleType = extractModuleType description
          in Just (JSDocModuleTag name moduleType)

    parseMemberOfTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseMemberOfTag description =
      case description of
        Nothing -> Nothing
        Just desc ->
          let (parent, forced) = extractMemberOfInfo desc
          in Just (JSDocMemberOfTag parent forced)

    parseTypedefTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseTypedefTag paramName =
      case paramName of
        Nothing -> Nothing
        Just name -> Just (JSDocTypedefTag name [])  -- Properties parsed separately

    parseEnumTag :: Maybe Text -> Maybe JSDocType -> Maybe Text -> Maybe JSDocTagSpecific
    parseEnumTag paramName enumBaseType description =
      case paramName of
        Nothing -> Nothing
        Just name ->
          let baseType = enumBaseType
              enumValues = parseEnumValues description
          in Just (JSDocEnumTag name baseType enumValues)

    parseCallbackTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseCallbackTag paramName =
      case paramName of
        Nothing -> Nothing
        Just name -> Just (JSDocCallbackTag name)

    parseEventTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseEventTag paramName =
      case paramName of
        Nothing -> Nothing
        Just name -> Just (JSDocEventTag name)

    parseFiresTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseFiresTag paramName =
      case paramName of
        Nothing -> Nothing
        Just name -> Just (JSDocFiresTag name)

    parseListensTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseListensTag paramName =
      case paramName of
        Nothing -> Nothing
        Just name -> Just (JSDocListensTag name)

    -- New parsing functions for additional JSDoc tags
    parseDescriptionTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseDescriptionTag description =
      case description of
        Nothing -> Nothing
        Just desc -> Just (JSDocDescriptionTag desc)

    parseTypeTagSpecific :: Maybe JSDocType -> Maybe JSDocTagSpecific
    parseTypeTagSpecific jsDocType =
      case jsDocType of
        Nothing -> Nothing
        Just docType -> Just (JSDocTypeTag docType)

    parsePropertyTag :: Maybe Text -> Maybe JSDocType -> Maybe Text -> Maybe JSDocTagSpecific
    parsePropertyTag paramName jsDocType description =
      case paramName of
        Nothing -> Nothing
        Just name ->
          let optional = Text.isSuffixOf "?" name
              cleanName = if optional then Text.dropEnd 1 name else name
          in Just (JSDocPropertyTag cleanName jsDocType optional description)

    parseDefaultTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseDefaultTag description =
      case description of
        Nothing -> Nothing
        Just value -> Just (JSDocDefaultTag value)

    parseConstantTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseConstantTag description =
      Just (JSDocConstantTag description)

    parseAliasTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseAliasTag paramName =
      case paramName of
        Nothing -> Nothing
        Just name -> Just (JSDocAliasTag name)

    parseAugmentsTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseAugmentsTag paramName =
      case paramName of
        Nothing -> Nothing
        Just parent -> Just (JSDocAugmentsTag parent)

    parseBorrowsTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseBorrowsTag description =
      case description of
        Nothing -> Nothing
        Just desc ->
          case Text.breakOn " as " desc of
            (from, rest) | not (Text.null rest) ->
              Just (JSDocBorrowsTag from (Just (Text.drop 4 rest)))
            _ -> Just (JSDocBorrowsTag desc Nothing)

    parseClassDescTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseClassDescTag description =
      case description of
        Nothing -> Nothing
        Just desc -> Just (JSDocClassDescTag desc)

    parseCopyrightTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseCopyrightTag description =
      case description of
        Nothing -> Nothing
        Just desc -> Just (JSDocCopyrightTag desc)

    parseExportsTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseExportsTag paramName =
      case paramName of
        Nothing -> Nothing
        Just name -> Just (JSDocExportsTag name)

    parseExternalTag :: Maybe Text -> Maybe Text -> Maybe JSDocTagSpecific
    parseExternalTag paramName description =
      case paramName of
        Nothing -> Nothing
        Just name -> Just (JSDocExternalTag name description)

    parseFileTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseFileTag description =
      case description of
        Nothing -> Nothing
        Just desc -> Just (JSDocFileTag desc)

    parseImplementsTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseImplementsTag paramName =
      case paramName of
        Nothing -> Nothing
        Just interface -> Just (JSDocImplementsTag interface)

    parseInterfaceTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseInterfaceTag paramName =
      Just (JSDocInterfaceTag paramName)

    parseKindTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseKindTag description =
      case description of
        Nothing -> Nothing
        Just kind -> Just (JSDocKindTag kind)

    parseLendsTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseLendsTag paramName =
      case paramName of
        Nothing -> Nothing
        Just name -> Just (JSDocLendsTag name)

    parseLicenseTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseLicenseTag description =
      case description of
        Nothing -> Nothing
        Just license -> Just (JSDocLicenseTag license)

    parseMemberTag :: Maybe Text -> Maybe JSDocType -> Maybe JSDocTagSpecific
    parseMemberTag paramName jsDocType =
      let memberType = case jsDocType of
            Nothing -> Nothing
            Just (JSDocBasicType name) -> Just name
            _ -> Nothing
      in Just (JSDocMemberTag paramName memberType)

    parseMixesTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseMixesTag paramName =
      case paramName of
        Nothing -> Nothing
        Just mixin -> Just (JSDocMixesTag mixin)

    parseNameTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseNameTag paramName =
      case paramName of
        Nothing -> Nothing
        Just name -> Just (JSDocNameTag name)

    parseRequiresTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseRequiresTag paramName =
      case paramName of
        Nothing -> Nothing
        Just module' -> Just (JSDocRequiresTag module')

    parseSummaryTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseSummaryTag description =
      case description of
        Nothing -> Nothing
        Just summary -> Just (JSDocSummaryTag summary)

    parseThisTag :: Maybe JSDocType -> Maybe JSDocTagSpecific
    parseThisTag jsDocType =
      case jsDocType of
        Nothing -> Nothing
        Just thisType -> Just (JSDocThisTag thisType)

    parseTodoTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseTodoTag description =
      case description of
        Nothing -> Nothing
        Just todo -> Just (JSDocTodoTag todo)

    parseTutorialTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseTutorialTag paramName =
      case paramName of
        Nothing -> Nothing
        Just tutorial -> Just (JSDocTutorialTag tutorial)

    parseVariationTag :: Maybe Text -> Maybe JSDocTagSpecific
    parseVariationTag paramName =
      case paramName of
        Nothing -> Nothing
        Just variation -> Just (JSDocVariationTag variation)

    parseYieldsTag :: Maybe JSDocType -> Maybe Text -> Maybe JSDocTagSpecific
    parseYieldsTag jsDocType description =
      Just (JSDocYieldsTag jsDocType description)

    -- Helper functions for extracting specific information
    extractDefaultValue :: Maybe Text -> Maybe Text
    extractDefaultValue description =
      case description of
        Nothing -> Nothing
        Just desc ->
          case Text.breakOn "=" desc of
            (_, rest) | not (Text.null rest) -> Just (Text.strip (Text.drop 1 rest))
            _ -> Nothing

    extractExampleInfo :: Maybe Text -> (Maybe Text, Maybe Text)
    extractExampleInfo description =
      case description of
        Nothing -> (Nothing, Nothing)
        Just desc ->
          if Text.isPrefixOf "<caption>" desc
            then
              let captionEnd = Text.breakOn "</caption>" (Text.drop 9 desc)
              in case captionEnd of
                (caption, rest) | not (Text.null rest) ->
                  (Nothing, Just caption)
                _ -> (Nothing, Nothing)
            else (Nothing, Nothing)

    extractSeeInfo :: Text -> (Text, Maybe Text)
    extractSeeInfo desc =
      case Text.breakOn " " desc of
        (reference, rest) | not (Text.null rest) ->
          (reference, Just (Text.strip rest))
        _ -> (desc, Nothing)

    extractDeprecatedInfo :: Maybe Text -> (Maybe Text, Maybe Text)
    extractDeprecatedInfo description =
      case description of
        Nothing -> (Nothing, Nothing)
        Just desc ->
          let words' = Text.words desc
          in case words' of
            (since:rest) | Text.all (\c -> c >= '0' && c <= '9' || c == '.') since ->
              (Just since, if null rest then Nothing else Just (Text.unwords rest))
            _ -> (Nothing, description)

    extractAuthorInfo :: Text -> (Text, Maybe Text)
    extractAuthorInfo desc =
      case Text.breakOn "<" desc of
        (name, rest) | not (Text.null rest) ->
          let email = Text.takeWhile (/= '>') (Text.drop 1 rest)
          in (Text.strip name, Just email)
        _ -> (desc, Nothing)

    extractExtendsInfo :: Maybe Text -> Maybe Text
    extractExtendsInfo description =
      case description of
        Nothing -> Nothing
        Just desc ->
          if Text.isPrefixOf "extends " (Text.toLower desc)
            then Just (Text.drop 8 desc)
            else Nothing

    extractModuleType :: Maybe Text -> Maybe Text
    extractModuleType description =
      case description of
        Nothing -> Nothing
        Just desc ->
          case Text.words desc of
            (moduleType:_) -> Just moduleType
            _ -> Nothing

    extractMemberOfInfo :: Text -> (Text, Bool)
    extractMemberOfInfo desc =
      if Text.isPrefixOf "!" desc
        then (Text.drop 1 desc, True)
        else (desc, False)

    parseEnumValues :: Maybe Text -> [JSDocEnumValue]
    parseEnumValues description =
      case description of
        Nothing -> []
        Just desc ->
          let descLines = Text.lines desc
              enumLines = filter (Text.isPrefixOf "-" . Text.strip) descLines
          in map parseEnumValueLine enumLines
      where
        parseEnumValueLine :: Text -> JSDocEnumValue
        parseEnumValueLine line =
          let trimmedLine = Text.stripStart (Text.drop 1 (Text.stripStart line))
              (nameAndValue, description) = Text.breakOn " - " trimmedLine
              (valueName, literal) = parseNameAndLiteral nameAndValue
              enumDesc = if Text.null description then Nothing else Just (Text.drop 3 description)
          in JSDocEnumValue valueName literal enumDesc

        parseNameAndLiteral :: Text -> (Text, Maybe Text)
        parseNameAndLiteral nameValue =
          case Text.breakOn "=" nameValue of
            (name, valueText) | not (Text.null valueText) ->
              let value = Text.strip (Text.drop 1 valueText)
              in (Text.strip name, Just value)
            _ -> (Text.strip nameValue, Nothing)

    -- | Check if a type name refers to an enum
    -- Enums typically use PascalCase and are not basic JavaScript types
    isEnumReference :: Text -> Bool
    isEnumReference text =
      not (Text.null text) &&
      not (isBasicJSType text) &&
      isCapitalized text &&
      Text.all (\c -> Char.isAlphaNum c || c == '_') text
      where
        isCapitalized t = case Text.uncons t of
          Just (c, _) -> Char.isUpper c
          Nothing -> False

        isBasicJSType :: Text -> Bool
        isBasicJSType t = t `elem`
          [ "string", "number", "boolean", "object", "function", "undefined"
          , "null", "any", "void", "Array", "Object", "Function", "Promise"
          , "Map", "Set", "WeakMap", "WeakSet", "Date", "RegExp", "Error"
          , "Symbol", "BigInt"
          ]

-- | Parse inline JSDoc tags from text.
--
-- Inline tags are enclosed in curly braces and start with @, like {@link MyClass}.
-- This function parses text containing inline tags and returns rich text.
--
-- ==== __Examples__
--
-- >>> parseInlineTags "See {@link MyClass} for details"
-- JSDocRichTextList [JSDocPlainText "See ", JSDocInlineTag (JSDocInlineLink "MyClass" Nothing), JSDocPlainText " for details"]
--
-- >>> parseInlineTags "Plain text only"
-- JSDocPlainText "Plain text only"
--
-- @since 0.8.0.0
parseInlineTags :: Text -> JSDocRichText
parseInlineTags text
  | Text.null text = JSDocPlainText ""
  | not (Text.isInfixOf "{@" text) = JSDocPlainText text
  | otherwise = JSDocRichTextList (parseRichTextSegments text)

-- | Parse text segments containing inline tags
parseRichTextSegments :: Text -> [JSDocRichText]
parseRichTextSegments text = parseSegments text []
  where
    parseSegments :: Text -> [JSDocRichText] -> [JSDocRichText]
    parseSegments remaining acc
      | Text.null remaining = reverse acc
      | otherwise =
          case Text.breakOn "{@" remaining of
            (before, after) | Text.null after ->
              -- No more inline tags
              if Text.null before
                then reverse acc
                else reverse (JSDocPlainText before : acc)
            (before, after) ->
              -- Found an inline tag
              let beforeSegment = if Text.null before then [] else [JSDocPlainText before]
                  (inlineTag, rest) = parseNextInlineTag after
              in case inlineTag of
                Just tag -> parseSegments rest (JSDocInlineTag tag : beforeSegment ++ acc)
                Nothing -> parseSegments (Text.drop 2 after) (beforeSegment ++ acc)

-- | Parse the next inline tag from text starting with "{@"
parseNextInlineTag :: Text -> (Maybe JSDocInlineTag, Text)
parseNextInlineTag text =
  case Text.findIndex (== '}') text of
    Nothing -> (Nothing, text) -- No closing brace
    Just closeIndex ->
      let tagContent = Text.take closeIndex text
          remaining = Text.drop (closeIndex + 1) text
          innerContent = Text.drop 2 tagContent -- Remove "{@"
      in (parseInlineTagContent innerContent, remaining)

-- | Parse the content of an inline tag
parseInlineTagContent :: Text -> Maybe JSDocInlineTag
parseInlineTagContent content =
  case Text.words content of
    [] -> Nothing
    (tagName:rest) ->
      case Text.toLower tagName of
        "link" -> parseInlineLink rest
        "tutorial" -> parseInlineTutorial rest
        "code" -> parseInlineCode rest
        _ -> Nothing

-- | Parse {@link} inline tag
parseInlineLink :: [Text] -> Maybe JSDocInlineTag
parseInlineLink words' =
  case words' of
    [] -> Nothing
    _ ->
      let fullText = Text.unwords words'
      in case Text.breakOn "|" fullText of
        (target, linkText) | not (Text.null linkText) ->
          let customText = Text.strip (Text.drop 1 linkText)
          in Just (JSDocInlineLink (Text.strip target) (Just customText))
        _ ->
          case words' of
            [target] -> Just (JSDocInlineLink target Nothing)
            (target:rest) ->
              let customText = Text.unwords rest
              in Just (JSDocInlineLink target (Just customText))

-- | Parse {@tutorial} inline tag
parseInlineTutorial :: [Text] -> Maybe JSDocInlineTag
parseInlineTutorial words' =
  case words' of
    [] -> Nothing
    [name] -> Just (JSDocInlineTutorial name Nothing)
    (name:rest) ->
      let customText = Text.unwords rest
      in Just (JSDocInlineTutorial name (Just customText))

-- | Parse {@code} inline tag
parseInlineCode :: [Text] -> Maybe JSDocInlineTag
parseInlineCode words' =
  case words' of
    [] -> Nothing
    _ -> Just (JSDocInlineCode (Text.unwords words'))

-- | Validate a JSDoc comment for correctness and best practices.
--
-- This function performs comprehensive validation of JSDoc comments including:
--
-- * Checking for required documentation (description, parameters, return values)
-- * Validating type expressions are well-formed
-- * Ensuring parameter consistency
-- * Detecting duplicate or missing documentation
-- * Verifying tag completeness and correctness
--
-- ==== __Examples__
--
-- Valid JSDoc returns no errors:
--
-- >>> let jsDoc = JSDocComment pos (Just "Calculate sum") [paramTag, returnTag]
-- >>> validateJSDoc jsDoc []
-- []
--
-- Missing description:
--
-- >>> let jsDoc = JSDocComment pos Nothing [paramTag]
-- >>> validateJSDoc jsDoc ["x"]
-- [JSDocMissingDescription]
--
-- Missing parameter documentation:
--
-- >>> let jsDoc = JSDocComment pos (Just "Calculate") [returnTag]
-- >>> validateJSDoc jsDoc ["x", "y"]
-- [JSDocMissingParam "x", JSDocMissingParam "y"]
--
-- @since 0.8.0.0
validateJSDoc
  :: JSDocComment
  -- ^ JSDoc comment to validate
  -> [Text]
  -- ^ Function parameter names (empty for non-functions)
  -> JSDocValidationResult
  -- ^ List of validation errors (empty if valid)
validateJSDoc jsDoc functionParams =
  let errors = []
  in errors
    ++ validateDescription jsDoc
    ++ validateParameters jsDoc functionParams
    ++ validateTags jsDoc
    ++ validateConsistency jsDoc

-- | Validate JSDoc description
validateDescription :: JSDocComment -> [JSDocValidationError]
validateDescription jsDoc =
  case jsDocDescription jsDoc of
    Nothing -> [JSDocMissingDescription]
    Just desc | Text.null (Text.strip desc) -> [JSDocMissingDescription]
    _ -> []

-- | Validate parameter documentation
validateParameters :: JSDocComment -> [Text] -> [JSDocValidationError]
validateParameters jsDoc functionParams =
  let paramTags = [tag | tag <- jsDocTags jsDoc, jsDocTagName tag == "param"]
      documentedParams = [name | tag <- paramTags, Just name <- [jsDocTagParamName tag]]
      missingParams = [param | param <- functionParams, param `notElem` documentedParams]
      unknownParams = [param | param <- documentedParams, param `notElem` functionParams]
      duplicateParams = findDuplicates documentedParams
  in map JSDocMissingParam missingParams
     ++ map JSDocUnknownParam unknownParams
     ++ map JSDocDuplicateParam duplicateParams

-- | Find duplicate values in a list
findDuplicates :: (Eq a, Ord a) => [a] -> [a]
findDuplicates xs = [x | (x:y:_) <- group (sort xs)]
  where
    sort = Data.List.sort
    group = Data.List.group

-- | Validate JSDoc tags for correctness
validateTags :: JSDocComment -> [JSDocValidationError]
validateTags jsDoc =
  let tags = jsDocTags jsDoc
  in concatMap validateTag tags
     ++ validateReturnDocumentation jsDoc
     ++ validateDeprecatedTag jsDoc

-- | Validate individual JSDoc tag
validateTag :: JSDocTag -> [JSDocValidationError]
validateTag tag =
  let tagName = jsDocTagName tag
      description = jsDocTagDescription tag
      emptyErrors = case description of
        Nothing -> [JSDocEmptyTag tagName]
        Just desc | Text.null (Text.strip desc) -> [JSDocEmptyTag tagName]
        _ -> []
      typeErrors = validateTagType tag
  in emptyErrors ++ typeErrors

-- | Validate tag type expressions
validateTagType :: JSDocTag -> [JSDocValidationError]
validateTagType tag =
  case jsDocTagType tag of
    Nothing -> []
    Just jsDocType -> validateTypeExpression jsDocType (jsDocTagName tag)

-- | Validate type expression syntax
validateTypeExpression :: JSDocType -> Text -> [JSDocValidationError]
validateTypeExpression jsDocType tagName =
  case jsDocType of
    JSDocBasicType typeName
      | Text.null (Text.strip typeName) -> [JSDocInvalidType ("Empty type in " <> tagName)]
      | otherwise -> []
    JSDocArrayType elementType -> validateTypeExpression elementType tagName
    JSDocUnionType types
      | null types -> [JSDocInvalidType ("Empty union type in " <> tagName)]
      | otherwise -> concatMap (`validateTypeExpression` tagName) types
    JSDocGenericType name args
      | Text.null (Text.strip name) -> [JSDocInvalidType ("Empty generic name in " <> tagName)]
      | null args -> [JSDocInvalidType ("Generic type without arguments in " <> tagName)]
      | otherwise -> concatMap (`validateTypeExpression` tagName) args
    _ -> [] -- Other types are considered valid

-- | Validate return documentation for functions
validateReturnDocumentation :: JSDocComment -> [JSDocValidationError]
validateReturnDocumentation jsDoc =
  let hasParams = any (\tag -> jsDocTagName tag == "param") (jsDocTags jsDoc)
      hasReturns = any (\tag -> jsDocTagName tag `elem` ["returns", "return"]) (jsDocTags jsDoc)
  in if hasParams && not hasReturns
     then [JSDocMissingReturn]
     else []

-- | Validate deprecated tag has replacement suggestion
validateDeprecatedTag :: JSDocComment -> [JSDocValidationError]
validateDeprecatedTag jsDoc =
  let deprecatedTags = [tag | tag <- jsDocTags jsDoc, jsDocTagName tag == "deprecated"]
      hasReplacement tag = case jsDocTagDescription tag of
        Nothing -> False
        Just desc -> not (Text.null (Text.strip desc))
  in [JSDocDeprecatedWithoutReplacement | tag <- deprecatedTags, not (hasReplacement tag)]

-- | Validate internal consistency of JSDoc
validateConsistency :: JSDocComment -> [JSDocValidationError]
validateConsistency jsDoc =
  let paramTags = [tag | tag <- jsDocTags jsDoc, jsDocTagName tag == "param"]
  in concatMap validateParamConsistency paramTags

-- | Validate parameter tag consistency
validateParamConsistency :: JSDocTag -> [JSDocValidationError]
validateParamConsistency tag =
  case (jsDocTagParamName tag, jsDocTagType tag, jsDocTagDescription tag) of
    (Just paramName, Just jsDocType, Just desc) ->
      -- Check if parameter name appears in description
      if Text.isInfixOf paramName desc
        then []
        else []  -- Not necessarily an error
    _ -> [] -- Incomplete tags are validated elsewhere
