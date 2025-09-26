{-# LANGUAGE OverloadedStrings #-}

module Unit.Language.Javascript.Parser.Parser.Modules
  ( testModuleParser,
  )
where

import Data.List (isInfixOf)
import Language.JavaScript.Parser (parse, parseModule)
import Language.JavaScript.Parser.AST
  ( JSAST (..),
    JSAnnot,
    JSBlock (..),
    JSCommaList (..),
    JSExportClause (..),
    JSExportDeclaration (..),
    JSExportSpecifier (..),
    JSExpression (..),
    JSFromClause (..),
    JSIdent (..),
    JSImportClause (..),
    JSImportDeclaration (..),
    JSImportNameSpace (..),
    JSImportSpecifier (..),
    JSImportsNamed (..),
    JSModuleItem (..),
    JSSemi,
    JSStatement (..),
    JSVarInitializer (..),
  )
import Test.Hspec

testModuleParser :: Spec
testModuleParser = describe "Parse modules:" $ do
  it "as" $
    case parseModule "as" "test" of
      Right (JSAstModule [JSModuleStatementListItem (JSExpressionStatement (JSIdentifier _ "as") _)] _) -> pure ()
      result -> expectationFailure ("Expected JSIdentifier 'as', got: " ++ show result)

  it "import" $ do
    -- Not yet supported
    -- test "import 'a';"            `shouldBe` ""

    -- Default import with single quotes - preserve 'def' identifier and 'mod' module
    case parseModule "import def from 'mod';" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseDefault (JSIdentName _ "def")) (JSFromClause _ _ "'mod'") Nothing _)] _) -> pure ()
      result -> expectationFailure ("Expected JSImportDeclaration with def from 'mod', got: " ++ show result)
    -- Default import with double quotes - preserve 'def' identifier and 'mod' module
    case parseModule "import def from \"mod\";" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseDefault (JSIdentName _ "def")) (JSFromClause _ _ "\"mod\"") Nothing _)] _) -> pure ()
      result -> expectationFailure ("Expected JSImportDeclaration with def from \"mod\", got: " ++ show result)
    -- Namespace import - preserve 'thing' identifier and 'mod' module
    case parseModule "import * as thing from 'mod';" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseNameSpace (JSImportNameSpace _ _ (JSIdentName _ "thing"))) (JSFromClause _ _ "'mod'") Nothing _)] _) -> pure ()
      result -> expectationFailure ("Expected JSImportNameSpace with thing from 'mod', got: " ++ show result)
    -- Named imports with 'as' renaming - preserve 'foo', 'bar', 'baz', 'quux' identifiers and 'mod' module
    case parseModule "import { foo, bar, baz as quux } from 'mod';" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseNamed (JSImportsNamed _ (JSLCons (JSLCons (JSLOne (JSImportSpecifier (JSIdentName _ "foo"))) _ (JSImportSpecifier (JSIdentName _ "bar"))) _ (JSImportSpecifierAs (JSIdentName _ "baz") _ (JSIdentName _ "quux"))) _)) (JSFromClause _ _ "'mod'") Nothing _)] _) -> pure ()
      result -> expectationFailure ("Expected JSImportsNamed with foo,bar,baz as quux from 'mod', got: " ++ show result)
    -- Mixed default and namespace import - preserve 'def' default, 'thing' namespace, 'mod' module
    case parseModule "import def, * as thing from 'mod';" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseDefaultNameSpace (JSIdentName _ "def") _ (JSImportNameSpace _ _ (JSIdentName _ "thing"))) (JSFromClause _ _ "'mod'") Nothing _)] _) -> pure ()
      result -> expectationFailure ("Expected JSImportClauseDefaultNameSpace with def, thing from 'mod', got: " ++ show result)
    -- Mixed default and named imports - preserve 'def', 'foo', 'bar', 'baz', 'quux' identifiers and 'mod' module
    case parseModule "import def, { foo, bar, baz as quux } from 'mod';" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseDefaultNamed (JSIdentName _ "def") _ (JSImportsNamed _ (JSLCons (JSLCons (JSLOne (JSImportSpecifier (JSIdentName _ "foo"))) _ (JSImportSpecifier (JSIdentName _ "bar"))) _ (JSImportSpecifierAs (JSIdentName _ "baz") _ (JSIdentName _ "quux"))) _)) (JSFromClause _ _ "'mod'") Nothing _)] _) -> pure ()
      result -> expectationFailure ("Expected JSImportClauseDefaultNamed with def, foo,bar,baz as quux from 'mod', got: " ++ show result)

  it "export" $ do
    -- Empty export declarations
    case parseModule "export {}" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExportLocals (JSExportClause _ JSLNil _) _)] _) -> pure ()
      result -> expectationFailure ("Expected JSExportLocals with empty clause, got: " ++ show result)
    case parseModule "export {};" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExportLocals (JSExportClause _ JSLNil _) _)] _) -> pure ()
      result -> expectationFailure ("Expected JSExportLocals with empty clause and semicolon, got: " ++ show result)
    -- Export const declaration - preserve 'a' variable name
    case parseModule "export const a = 1;" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExport (JSConstant _ (JSLOne (JSVarInitExpression (JSIdentifier _ "a") (JSVarInit _ (JSDecimal _ "1")))) _) _)] _) -> pure ()
      result -> expectationFailure ("Expected JSExport with const 'a', got: " ++ show result)
    -- Export function declaration - preserve 'f' function name
    case parseModule "export function f() {};" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExport (JSFunction _ (JSIdentName _ "f") _ JSLNil _ (JSBlock _ [] _) _) _)] _) -> pure ()
      result -> expectationFailure ("Expected JSExport with function 'f', got: " ++ show result)
    -- Export named specifier - preserve 'a' identifier
    case parseModule "export { a };" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExportLocals (JSExportClause _ (JSLOne (JSExportSpecifier (JSIdentName _ "a"))) _) _)] _) -> pure ()
      result -> expectationFailure ("Expected JSExportLocals with specifier 'a', got: " ++ show result)
    -- Export named specifier with 'as' renaming - preserve 'a', 'b' identifiers
    case parseModule "export { a as b };" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExportLocals (JSExportClause _ (JSLOne (JSExportSpecifierAs (JSIdentName _ "a") _ (JSIdentName _ "b"))) _) _)] _) -> pure ()
      result -> expectationFailure ("Expected JSExportSpecifierAs with 'a' as 'b', got: " ++ show result)
    -- Re-export empty from module - preserve 'mod' module name
    case parseModule "export {} from 'mod'" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExportFrom (JSExportClause _ JSLNil _) (JSFromClause _ _ "'mod'") _)] _) -> pure ()
      result -> expectationFailure ("Expected JSExportFrom with empty clause from 'mod', got: " ++ show result)
    -- Re-export all from module - preserve 'mod' module name
    case parseModule "export * from 'mod'" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExportAllFrom _ (JSFromClause _ _ "'mod'") _)] _) -> pure ()
      result -> expectationFailure ("Expected JSExportAllFrom with 'mod', got: " ++ show result)
    case parseModule "export * from 'mod';" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExportAllFrom _ (JSFromClause _ _ "'mod'") _)] _) -> pure ()
      result -> expectationFailure ("Expected JSExportAllFrom with 'mod' and semicolon, got: " ++ show result)
    -- Re-export all with double quotes - preserve "module" module name
    case parseModule "export * from \"module\"" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExportAllFrom _ (JSFromClause _ _ "\"module\"") _)] _) -> pure ()
      result -> expectationFailure ("Expected JSExportAllFrom with \"module\", got: " ++ show result)
    -- Re-export all with relative path - preserve './relative/path' module name
    case parseModule "export * from './relative/path'" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExportAllFrom _ (JSFromClause _ _ "'./relative/path'") _)] _) -> pure ()
      result -> expectationFailure ("Expected JSExportAllFrom with './relative/path', got: " ++ show result)
    -- Re-export all with parent path - preserve '../parent/module' module name
    case parseModule "export * from '../parent/module'" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExportAllFrom _ (JSFromClause _ _ "'../parent/module'") _)] _) -> pure ()
      result -> expectationFailure ("Expected JSExportAllFrom with '../parent/module', got: " ++ show result)

  it "advanced module features (ES2020+) - supported features" $ do
    -- Mixed default and namespace imports - preserve 'def', 'ns', 'module' names
    case parseModule "import def, * as ns from 'module';" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseDefaultNameSpace (JSIdentName _ "def") _ (JSImportNameSpace _ _ (JSIdentName _ "ns"))) (JSFromClause _ _ "'module'") Nothing _)] _) -> pure ()
      result -> expectationFailure ("Expected JSImportClauseDefaultNameSpace with def, ns from 'module', got: " ++ show result)

    -- Mixed default and named imports - preserve 'def', 'named1', 'named2', 'module' names
    case parseModule "import def, { named1, named2 } from 'module';" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseDefaultNamed (JSIdentName _ "def") _ (JSImportsNamed _ (JSLCons (JSLOne (JSImportSpecifier (JSIdentName _ "named1"))) _ (JSImportSpecifier (JSIdentName _ "named2"))) _)) (JSFromClause _ _ "'module'") Nothing _)] _) -> pure ()
      result -> expectationFailure ("Expected JSImportClauseDefaultNamed with def, named1, named2 from 'module', got: " ++ show result)

    -- Export default as named from another module - preserve 'default', 'named', 'module' names
    case parseModule "export { default as named } from 'module';" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExportFrom (JSExportClause _ (JSLOne (JSExportSpecifierAs (JSIdentName _ "default") _ (JSIdentName _ "named"))) _) (JSFromClause _ _ "'module'") _)] _) -> pure ()
      result -> expectationFailure ("Expected JSExportSpecifierAs with default as named from 'module', got: " ++ show result)

    -- Re-export with renaming - preserve 'original', 'renamed', 'other', './utils' names
    case parseModule "export { original as renamed, other } from './utils';" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExportFrom (JSExportClause _ (JSLCons (JSLOne (JSExportSpecifierAs (JSIdentName _ "original") _ (JSIdentName _ "renamed"))) _ (JSExportSpecifier (JSIdentName _ "other"))) _) (JSFromClause _ _ "'./utils'") _)] _) -> pure ()
      result -> expectationFailure ("Expected JSExportFrom with original as renamed, other from './utils', got: " ++ show result)

    -- Complex namespace imports - preserve 'utilities', '@scope/package' names
    case parseModule "import * as utilities from '@scope/package';" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseNameSpace (JSImportNameSpace _ _ (JSIdentName _ "utilities"))) (JSFromClause _ _ "'@scope/package'") Nothing _)] _) -> pure ()
      result -> expectationFailure ("Expected JSImportNameSpace with utilities from '@scope/package', got: " ++ show result)

    -- Multiple named exports from different modules - preserve 'func1', 'func2', 'alias', './module1' names
    case parseModule "export { func1, func2 as alias } from './module1';" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExportFrom (JSExportClause _ (JSLCons (JSLOne (JSExportSpecifier (JSIdentName _ "func1"))) _ (JSExportSpecifierAs (JSIdentName _ "func2") _ (JSIdentName _ "alias"))) _) (JSFromClause _ _ "'./module1'") _)] _) -> pure ()
      result -> expectationFailure ("Expected JSExportFrom with func1, func2 as alias from './module1', got: " ++ show result)

  it "advanced module features (ES2020+) - supported and limitations" $ do
    -- Export * as namespace is now supported (ES2020)
    case parseModule "export * as ns from 'module';" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExportAllAsFrom _ _ (JSIdentName _ "ns") (JSFromClause _ _ "'module'") _)] _) -> pure ()
      Left err -> expectationFailure ("Should parse export * as: " ++ show err)
    case parseModule "export * as namespace from './utils';" "test" of
      Right (JSAstModule [JSModuleExportDeclaration _ (JSExportAllAsFrom _ _ (JSIdentName _ "namespace") (JSFromClause _ _ "'./utils'") _)] _) -> pure ()
      Left err -> expectationFailure ("Should parse export * as: " ++ show err)

    -- Note: import.meta is now supported for property access
    case parse "import.meta.url" "test" of
      Right (JSAstProgram [JSExpressionStatement (JSMemberDot (JSImportMeta _ _) _ (JSIdentifier _ "url")) _] _) -> pure ()
      Left err -> expectationFailure ("Should parse import.meta.url: " ++ show err)
    case parse "import.meta.resolve('./module')" "test" of
      Right (JSAstProgram [JSMethodCall (JSMemberDot (JSImportMeta _ _) _ (JSIdentifier _ "resolve")) _ (JSLOne (JSStringLiteral _ "'./module'")) _ _] _) -> pure ()
      Left err -> expectationFailure ("Should parse import.meta.resolve: " ++ show err)
    case parse "console.log(import.meta)" "test" of
      Right (JSAstProgram [JSMethodCall (JSMemberDot (JSIdentifier _ "console") _ (JSIdentifier _ "log")) _ (JSLOne (JSImportMeta _ _)) _ _] _) -> pure ()
      Left err -> expectationFailure ("Should parse console.log(import.meta): " ++ show err)

    -- Note: Dynamic import() expressions are now supported
    case parse "import('./module.js')" "test" of
      Right (JSAstProgram [JSExpressionStatement (JSImportCall {}) _] _) -> pure ()  -- Now expects success
      Left err -> expectationFailure ("Dynamic import should parse successfully, got error: " ++ show err)
      Right result -> expectationFailure ("Expected import call expression, got: " ++ show result)

    -- Note: Import assertions may be parsed as objects but not semantically supported
    case parse "import('./data.json', { assert: { type: 'json' } })" "test" of
      Left err -> err `shouldSatisfy` (\msg -> "parse error" `isInfixOf` msg || "LeftParenToken" `isInfixOf` msg)
      Right _ -> pure ()  -- Parse may succeed syntactically but semantic support is separate

  it "import.meta expressions (ES2020)" $ do
    -- Basic import.meta access
    case parse "import.meta;" "test" of
      Right (JSAstProgram [JSExpressionStatement (JSImportMeta _ _) _] _) -> pure ()
      Left err -> expectationFailure ("Should parse import.meta: " ++ show err)

    -- import.meta.url property access
    case parseModule "const url = import.meta.url;" "test" of
      Right (JSAstModule [JSModuleStatementListItem (JSConstant _ (JSLOne (JSVarInitExpression (JSIdentifier _ "url") (JSVarInit _ (JSMemberDot (JSImportMeta _ _) _ (JSIdentifier _ "url"))))) _)] _) -> pure ()
      Left err -> expectationFailure ("Should parse const url = import.meta.url: " ++ show err)

    -- import.meta.resolve() method calls
    case parseModule "const resolved = import.meta.resolve('./module.js');" "test" of
      Right (JSAstModule [JSModuleStatementListItem (JSConstant _ (JSLOne (JSVarInitExpression (JSIdentifier _ "resolved") (JSVarInit _ (JSMemberExpression (JSMemberDot (JSImportMeta _ _) _ (JSIdentifier _ "resolve")) _ (JSLOne (JSStringLiteral _ "'./module.js'")) _)))) _)] _) -> pure ()
      Left err -> expectationFailure ("Should parse const resolved = import.meta.resolve: " ++ show err)

    -- import.meta in function calls - preserve console, log, url, import.meta identifiers
    case parseModule "console.log(import.meta.url, import.meta);" "test" of
      Right (JSAstModule [JSModuleStatementListItem (JSMethodCall (JSMemberDot (JSIdentifier _ "console") _ (JSIdentifier _ "log")) _ (JSLCons (JSLOne (JSMemberDot (JSImportMeta _ _) _ (JSIdentifier _ "url"))) _ (JSImportMeta _ _)) _ _)] _) -> pure ()
      Left err -> expectationFailure ("Should parse console.log(import.meta.url, import.meta): " ++ show err)

    -- import.meta in conditional expressions - preserve hasUrl variable, url property
    case parseModule "const hasUrl = import.meta.url ? true : false;" "test" of
      Right (JSAstModule [JSModuleStatementListItem (JSConstant _ (JSLOne (JSVarInitExpression (JSIdentifier _ "hasUrl") (JSVarInit _ (JSExpressionTernary (JSMemberDot (JSImportMeta _ _) _ (JSIdentifier _ "url")) _ (JSLiteral _ "true") _ (JSLiteral _ "false"))))) _)] _) -> pure ()
      Left err -> expectationFailure ("Should parse conditional with import.meta.url: " ++ show err)

    -- import.meta property access variations - preserve env property
    case parseModule "import.meta.env;" "test" of
      Right (JSAstModule [JSModuleStatementListItem (JSExpressionStatement (JSMemberDot (JSImportMeta _ _) _ (JSIdentifier _ "env")) _)] _) -> pure ()
      Left err -> expectationFailure ("Should parse import.meta.env: " ++ show err)

    -- Basic import.meta access statement
    case parseModule "import.meta;" "test" of
      Right (JSAstModule [JSModuleStatementListItem (JSExpressionStatement (JSImportMeta _ _) _)] _) -> pure ()
      result -> expectationFailure ("Expected JSImportMeta statement, got: " ++ show result)

  it "import attributes with 'with' clause (ES2021+)" $ do
    -- JSON imports with type attribute - preserve data, './data.json', type, json identifiers
    case parseModule "import data from './data.json' with { type: 'json' };" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseDefault (JSIdentName _ "data")) (JSFromClause _ _ "'./data.json'") (Just _) _)] _) -> pure ()
      Left err -> expectationFailure ("Should parse import with type attribute: " ++ show err)

    -- Test that various import attributes parse successfully - preserve styles, css identifiers
    case parseModule "import * as styles from './styles.css' with { type: 'css' };" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseNameSpace (JSImportNameSpace _ _ (JSIdentName _ "styles"))) (JSFromClause _ _ "'./styles.css'") (Just _) _)] _) -> pure ()
      Left err -> expectationFailure ("Should parse namespace import with type attribute: " ++ show err)

    case parseModule "import { config, settings } from './config.json' with { type: 'json' };" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseNamed (JSImportsNamed _ (JSLCons (JSLOne (JSImportSpecifier (JSIdentName _ "config"))) _ (JSImportSpecifier (JSIdentName _ "settings"))) _)) (JSFromClause _ _ "'./config.json'") (Just _) _)] _) -> pure ()
      Left err -> expectationFailure ("Should parse named import with type attribute: " ++ show err)

    case parseModule "import secure from './secure.json' with { type: 'json', integrity: 'sha256-abc123' };" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseDefault (JSIdentName _ "secure")) (JSFromClause _ _ "'./secure.json'") (Just _) _)] _) -> pure ()
      Left err -> expectationFailure ("Should parse import with multiple attributes: " ++ show err)

    case parseModule "import defaultExport, { namedExport } from './module.js' with { type: 'module' };" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseDefaultNamed (JSIdentName _ "defaultExport") _ (JSImportsNamed _ (JSLOne (JSImportSpecifier (JSIdentName _ "namedExport"))) _)) (JSFromClause _ _ "'./module.js'") (Just _) _)] _) -> pure ()
      Left err -> expectationFailure ("Should parse mixed import with type attribute: " ++ show err)

    case parseModule "import './polyfill.js' with { type: 'module' };" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclarationBare _ "'./polyfill.js'" (Just _) _)] _) -> pure ()
      Left err -> expectationFailure ("Should parse side-effect import with type attribute: " ++ show err)

    -- Import without attributes (backwards compatibility) - preserve regular identifier
    case parseModule "import regular from './regular.js';" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseDefault (JSIdentName _ "regular")) (JSFromClause _ _ "'./regular.js'") Nothing _)] _) -> pure ()
      Left err -> expectationFailure ("Should parse regular import without attributes: " ++ show err)

    -- Multiple attributes with various attribute types - preserve wasm identifier
    case parseModule "import wasm from './module.wasm' with { type: 'webassembly', encoding: 'binary' };" "test" of
      Right (JSAstModule [JSModuleImportDeclaration _ (JSImportDeclaration (JSImportClauseDefault (JSIdentName _ "wasm")) (JSFromClause _ _ "'./module.wasm'") (Just _) _)] _) -> pure ()
      Left err -> expectationFailure ("Should parse import with webassembly attributes: " ++ show err)
