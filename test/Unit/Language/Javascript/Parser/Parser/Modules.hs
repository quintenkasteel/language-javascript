module Unit.Language.Javascript.Parser.Parser.Modules
    ( testModuleParser
    ) where

import Test.Hspec

import Language.JavaScript.Parser


testModuleParser :: Spec
testModuleParser = describe "Parse modules:" $ do
    it "as" $
        test "as"
            `shouldBe`
            "Right (JSAstModule [JSModuleStatementListItem (JSIdentifier 'as')])"

    it "import" $ do
        -- Not yet supported
        -- test "import 'a';"            `shouldBe` ""

        test "import def from 'mod';"
            `shouldBe`
            "Right (JSAstModule [JSModuleImportDeclaration (JSImportDeclaration (JSImportClauseDefault (JSIdentifier 'def'),JSFromClause ''mod''))])"
        test "import def from \"mod\";"
            `shouldBe`
            "Right (JSAstModule [JSModuleImportDeclaration (JSImportDeclaration (JSImportClauseDefault (JSIdentifier 'def'),JSFromClause '\"mod\"'))])"
        test "import * as thing from 'mod';"
            `shouldBe`
            "Right (JSAstModule [JSModuleImportDeclaration (JSImportDeclaration (JSImportClauseNameSpace (JSImportNameSpace (JSIdentifier 'thing')),JSFromClause ''mod''))])"
        test "import { foo, bar, baz as quux } from 'mod';"
            `shouldBe`
            "Right (JSAstModule [JSModuleImportDeclaration (JSImportDeclaration (JSImportClauseNameSpace (JSImportsNamed ((JSImportSpecifier (JSIdentifier 'foo'),JSImportSpecifier (JSIdentifier 'bar'),JSImportSpecifierAs (JSIdentifier 'baz',JSIdentifier 'quux')))),JSFromClause ''mod''))])"
        test "import def, * as thing from 'mod';"
            `shouldBe`
            "Right (JSAstModule [JSModuleImportDeclaration (JSImportDeclaration (JSImportClauseDefaultNameSpace (JSIdentifier 'def',JSImportNameSpace (JSIdentifier 'thing')),JSFromClause ''mod''))])"
        test "import def, { foo, bar, baz as quux } from 'mod';"
            `shouldBe`
            "Right (JSAstModule [JSModuleImportDeclaration (JSImportDeclaration (JSImportClauseDefaultNamed (JSIdentifier 'def',JSImportsNamed ((JSImportSpecifier (JSIdentifier 'foo'),JSImportSpecifier (JSIdentifier 'bar'),JSImportSpecifierAs (JSIdentifier 'baz',JSIdentifier 'quux')))),JSFromClause ''mod''))])"

    it "export" $ do
        test "export {}"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportLocals (JSExportClause (())))])"
        test "export {};"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportLocals (JSExportClause (())))])"
        test "export const a = 1;"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExport (JSConstant (JSVarInitExpression (JSIdentifier 'a') [JSDecimal '1'])))])"
        test "export function f() {};"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExport (JSFunction 'f' () (JSBlock [])))])"
        test "export { a };"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportLocals (JSExportClause ((JSExportSpecifier (JSIdentifier 'a')))))])"
        test "export { a as b };"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportLocals (JSExportClause ((JSExportSpecifierAs (JSIdentifier 'a',JSIdentifier 'b')))))])"
        test "export {} from 'mod'"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportFrom (JSExportClause (()),JSFromClause ''mod''))])"
        test "export * from 'mod'"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportAllFrom ('*',JSFromClause ''mod''))])"
        test "export * from 'mod';"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportAllFrom ('*',JSFromClause ''mod''))])"
        test "export * from \"module\""
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportAllFrom ('*',JSFromClause '\"module\"'))])"
        test "export * from './relative/path'"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportAllFrom ('*',JSFromClause ''./relative/path''))])"
        test "export * from '../parent/module'"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportAllFrom ('*',JSFromClause ''../parent/module''))])"

    it "advanced module features (ES2020+) - supported features" $ do
        -- Mixed default and namespace imports
        test "import def, * as ns from 'module';"
            `shouldBe`
            "Right (JSAstModule [JSModuleImportDeclaration (JSImportDeclaration (JSImportClauseDefaultNameSpace (JSIdentifier 'def',JSImportNameSpace (JSIdentifier 'ns')),JSFromClause ''module''))])"
        
        -- Mixed default and named imports  
        test "import def, { named1, named2 } from 'module';"
            `shouldBe`
            "Right (JSAstModule [JSModuleImportDeclaration (JSImportDeclaration (JSImportClauseDefaultNamed (JSIdentifier 'def',JSImportsNamed ((JSImportSpecifier (JSIdentifier 'named1'),JSImportSpecifier (JSIdentifier 'named2')))),JSFromClause ''module''))])"
        
        -- Export default as named from another module
        test "export { default as named } from 'module';"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportFrom (JSExportClause ((JSExportSpecifierAs (JSIdentifier 'default',JSIdentifier 'named'))),JSFromClause ''module''))])"
        
        -- Re-export with renaming
        test "export { original as renamed, other } from './utils';"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportFrom (JSExportClause ((JSExportSpecifierAs (JSIdentifier 'original',JSIdentifier 'renamed'),JSExportSpecifier (JSIdentifier 'other'))),JSFromClause ''./utils''))])"
        
        -- Complex namespace imports
        test "import * as utilities from '@scope/package';"
            `shouldBe`
            "Right (JSAstModule [JSModuleImportDeclaration (JSImportDeclaration (JSImportClauseNameSpace (JSImportNameSpace (JSIdentifier 'utilities')),JSFromClause ''@scope/package''))])"
        
        -- Multiple named exports from different modules
        test "export { func1, func2 as alias } from './module1';"
            `shouldBe`
            "Right (JSAstModule [JSModuleExportDeclaration (JSExportFrom (JSExportClause ((JSExportSpecifier (JSIdentifier 'func1'),JSExportSpecifierAs (JSIdentifier 'func2',JSIdentifier 'alias'))),JSFromClause ''./module1''))])"

    it "advanced module features (ES2020+) - supported and limitations" $ do
        -- Export * as namespace is now supported (ES2020)
        case parseModule "export * as ns from 'module';" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Should parse export * as: " ++ show err)
        case parseModule "export * as namespace from './utils';" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Should parse export * as: " ++ show err)
        
        -- Note: import.meta is now supported for property access
        case parse "import.meta.url" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Should parse import.meta.url: " ++ show err)
        case parse "import.meta.resolve('./module')" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Should parse import.meta.resolve: " ++ show err)
        case parse "console.log(import.meta)" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Should parse console.log(import.meta): " ++ show err)
        
        -- Note: Dynamic import() expressions are not supported as expressions (already tested elsewhere)
        case parse "import('./module.js')" "test" of
            Left _ -> pure ()  -- Expected to fail
            Right _ -> expectationFailure "Dynamic import() should not parse as expression"
        
        -- Note: Import assertions are not yet supported for dynamic imports
        case parse "import('./data.json', { assert: { type: 'json' } })" "test" of
            Left _ -> pure ()  -- Expected to fail
            Right _ -> expectationFailure "Import assertions should not yet be supported"

    it "import.meta expressions (ES2020)" $ do
        -- Basic import.meta access
        case parse "import.meta;" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Should parse import.meta: " ++ show err)
        
        -- import.meta.url property access
        case parseModule "const url = import.meta.url;" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Should parse import.meta.url: " ++ show err)
        
        -- import.meta.resolve() method calls
        case parseModule "const resolved = import.meta.resolve('./module.js');" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Should parse import.meta.resolve: " ++ show err)
        
        -- import.meta in function calls
        case parseModule "console.log(import.meta.url, import.meta);" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Should parse import.meta in function calls: " ++ show err)
        
        -- import.meta in conditional expressions
        case parseModule "const hasUrl = import.meta.url ? true : false;" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Parse should succeed: " ++ show err)
        
        -- import.meta property access variations
        case parseModule "import.meta.env;" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Parse should succeed: " ++ show err)
        
        -- Verify one basic exact string match for import.meta
        test "import.meta;"
            `shouldBe`
            "Right (JSAstModule [JSModuleStatementListItem (JSImportMeta,JSSemicolon)])"

    it "import attributes with 'with' clause (ES2021+)" $ do
        -- JSON imports with type attribute (functional test)
        case parseModule "import data from './data.json' with { type: 'json' };" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Parse should succeed: " ++ show err)
        
        -- Test that various import attributes parse successfully (functional tests)
        case parseModule "import * as styles from './styles.css' with { type: 'css' };" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Parse should succeed: " ++ show err)
        
        case parseModule "import { config, settings } from './config.json' with { type: 'json' };" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Parse should succeed: " ++ show err)
        
        case parseModule "import secure from './secure.json' with { type: 'json', integrity: 'sha256-abc123' };" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Parse should succeed: " ++ show err)
        
        case parseModule "import defaultExport, { namedExport } from './module.js' with { type: 'module' };" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Parse should succeed: " ++ show err)
        
        case parseModule "import './polyfill.js' with { type: 'module' };" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Parse should succeed: " ++ show err)
        
        -- Import without attributes (backwards compatibility) 
        case parseModule "import regular from './regular.js';" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Parse should succeed: " ++ show err)
        
        -- Multiple attributes with various attribute types
        case parseModule "import wasm from './module.wasm' with { type: 'webassembly', encoding: 'binary' };" "test" of
            Right _ -> pure ()
            Left err -> expectationFailure ("Parse should succeed: " ++ show err)


test :: String -> String
test str = showStrippedMaybe (parseModule str "src")
