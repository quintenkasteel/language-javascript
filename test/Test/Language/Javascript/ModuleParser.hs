module Test.Language.Javascript.ModuleParser
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
        parseModule "export * as ns from 'module';" "test" `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        parseModule "export * as namespace from './utils';" "test" `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        
        -- Note: import.meta is now supported for property access
        parse "import.meta.url" "test" `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        parse "import.meta.resolve('./module')" "test" `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        parse "console.log(import.meta)" "test" `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        
        -- Note: Dynamic import() expressions are not supported as expressions (already tested elsewhere)
        parse "import('./module.js')" "test" `shouldSatisfy` (\result -> case result of Left _ -> True; Right _ -> False)
        
        -- Note: Import assertions are not yet supported for dynamic imports
        parse "import('./data.json', { assert: { type: 'json' } })" "test" `shouldSatisfy` (\result -> case result of Left _ -> True; Right _ -> False)

    it "import.meta expressions (ES2020)" $ do
        -- Basic import.meta access
        parse "import.meta;" "test"
            `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        
        -- import.meta.url property access
        parseModule "const url = import.meta.url;" "test"
            `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        
        -- import.meta.resolve() method calls
        parseModule "const resolved = import.meta.resolve('./module.js');" "test"
            `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        
        -- import.meta in function calls
        parseModule "console.log(import.meta.url, import.meta);" "test"
            `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        
        -- import.meta in conditional expressions
        parseModule "const hasUrl = import.meta.url ? true : false;" "test"
            `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        
        -- import.meta property access variations
        parseModule "import.meta.env;" "test"
            `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        
        -- Verify one basic exact string match for import.meta
        test "import.meta;"
            `shouldBe`
            "Right (JSAstModule [JSModuleStatementListItem (JSImportMeta,JSSemicolon)])"

    it "import attributes with 'with' clause (ES2021+)" $ do
        -- JSON imports with type attribute (functional test)
        parseModule "import data from './data.json' with { type: 'json' };" "test"
            `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        
        -- Test that various import attributes parse successfully (functional tests)
        parseModule "import * as styles from './styles.css' with { type: 'css' };" "test" 
            `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        
        parseModule "import { config, settings } from './config.json' with { type: 'json' };" "test"
            `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        
        parseModule "import secure from './secure.json' with { type: 'json', integrity: 'sha256-abc123' };" "test"
            `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        
        parseModule "import defaultExport, { namedExport } from './module.js' with { type: 'module' };" "test"
            `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        
        parseModule "import './polyfill.js' with { type: 'module' };" "test"
            `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        
        -- Import without attributes (backwards compatibility) 
        parseModule "import regular from './regular.js';" "test"
            `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)
        
        -- Multiple attributes with various attribute types
        parseModule "import wasm from './module.wasm' with { type: 'webassembly', encoding: 'binary' };" "test"
            `shouldSatisfy` (\result -> case result of Right _ -> True; Left _ -> False)


test :: String -> String
test str = showStrippedMaybe (parseModule str "src")
