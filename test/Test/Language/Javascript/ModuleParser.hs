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


test :: String -> String
test str = showStrippedMaybe (parseModule str "src")
