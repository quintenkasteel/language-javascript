{-# LANGUAGE OverloadedStrings #-}

module Unit.Language.Javascript.Parser.Parser.ExportStar
  ( testExportStar,
  )
where

import Language.JavaScript.Parser
import qualified Language.JavaScript.Parser.AST as AST
import Test.Hspec

-- | Comprehensive test suite for export * from 'module' syntax
testExportStar :: Spec
testExportStar = describe "Export Star Syntax Tests" $ do
  describe "basic export * parsing" $ do
    it "parses export * from 'module'" $ do
      case parseModule "export * from 'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "parses export * from double quotes" $ do
      case parseModule "export * from \"module\";" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "parses export * without semicolon" $ do
      case parseModule "export * from 'module'" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

  describe "module specifier variations" $ do
    it "parses relative paths" $ do
      case parseModule "export * from './utils';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "parses parent directory paths" $ do
      case parseModule "export * from '../parent';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "parses scoped packages" $ do
      case parseModule "export * from '@scope/package';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "parses file extensions" $ do
      case parseModule "export * from './file.js';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

  describe "whitespace handling" $ do
    it "handles extra whitespace" $ do
      case parseModule "export   *   from   'module'   ;" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "handles newlines" $ do
      case parseModule "export\n*\nfrom\n'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "handles tabs" $ do
      case parseModule "export\t*\tfrom\t'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

  describe "comment handling" $ do
    it "handles comments before *" $ do
      case parseModule "export /* comment */ * from 'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "handles comments after *" $ do
      case parseModule "export * /* comment */ from 'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "handles comments before from" $ do
      case parseModule "export * from /* comment */ 'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

  describe "multiple export statements" $ do
    it "parses multiple export * statements" $ do
      let input =
            unlines
              [ "export * from 'module1';",
                "export * from 'module2';",
                "export * from 'module3';"
              ]
      case parseModule input "test" of
        Right (AST.JSAstModule stmts _) -> do
          length stmts `shouldBe` 3
          -- Verify all are export declarations
          let isExportStar (AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)) = True
              isExportStar _ = False
          all isExportStar stmts `shouldBe` True
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "parses mixed export types" $ do
      let input =
            unlines
              [ "export * from 'all';",
                "export { specific } from 'named';",
                "export const local = 42;"
              ]
      case parseModule input "test" of
        Right (AST.JSAstModule stmts _) -> do
          length stmts `shouldBe` 3
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

  describe "complex module names" $ do
    it "handles Unicode in module names" $ do
      case parseModule "export * from './файл';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "handles special characters" $ do
      case parseModule "export * from './file-with-dashes_and_underscores.module.js';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "handles empty string (edge case)" $ do
      case parseModule "export * from '';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

  describe "error conditions" $ do
    it "rejects missing 'from' keyword" $ do
      case parseModule "export * 'module';" "test" of
        Left _ -> pure () -- Should fail
        Right _ -> expectationFailure "Expected parse error for missing 'from'"

    it "rejects missing module specifier" $ do
      case parseModule "export * from;" "test" of
        Left _ -> pure () -- Should fail
        Right _ -> expectationFailure "Expected parse error for missing module specifier"

    it "rejects non-string module specifier" $ do
      case parseModule "export * from identifier;" "test" of
        Left _ -> pure () -- Should fail
        Right _ -> expectationFailure "Expected parse error for non-string module specifier"

    it "rejects numeric module specifier" $ do
      case parseModule "export * from 123;" "test" of
        Left _ -> pure () -- Should fail
        Right _ -> expectationFailure "Expected parse error for numeric module specifier"

  describe "export * as namespace parsing" $ do
    it "parses export * as ns from 'module'" $ do
      case parseModule "export * as ns from 'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "parses export * as namespace from double quotes" $ do
      case parseModule "export * as namespace from \"module\";" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "parses export * as identifier without semicolon" $ do
      case parseModule "export * as myNamespace from 'module'" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "validates correct namespace identifier extraction" $ do
      case parseModule "export * as testNamespace from 'test-module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ (AST.JSIdentName _ name) _ _)] _) ->
          name `shouldBe` "testNamespace"
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

  describe "export * as namespace whitespace handling" $ do
    it "handles extra whitespace" $ do
      case parseModule "export   *   as   namespace   from   'module'   ;" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "handles newlines" $ do
      case parseModule "export\n*\nas\nnamespace\nfrom\n'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "handles tabs" $ do
      case parseModule "export\t*\tas\tnamespace\tfrom\t'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

  describe "export * as namespace comment handling" $ do
    it "handles comments before as" $ do
      case parseModule "export * /* comment */ as namespace from 'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "handles comments after as" $ do
      case parseModule "export * as /* comment */ namespace from 'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "handles comments before from" $ do
      case parseModule "export * as namespace /* comment */ from 'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

  describe "export * as namespace with various module specifiers" $ do
    it "parses relative paths" $ do
      case parseModule "export * as utils from './utils';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "parses scoped packages" $ do
      case parseModule "export * as scopedPkg from '@scope/package';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "parses file extensions" $ do
      case parseModule "export * as fileNS from './file.js';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

  describe "export * as namespace identifier variations" $ do
    it "accepts camelCase identifiers" $ do
      case parseModule "export * as camelCaseNamespace from 'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "accepts PascalCase identifiers" $ do
      case parseModule "export * as PascalCaseNamespace from 'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "accepts underscore identifiers" $ do
      case parseModule "export * as underscore_namespace from 'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "accepts dollar sign identifiers" $ do
      case parseModule "export * as $namespace from 'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

    it "accepts single letter identifiers" $ do
      case parseModule "export * as a from 'module';" "test" of
        Right (AST.JSAstModule [AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)] _) ->
          pure ()
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)

  describe "export * as namespace error conditions" $ do
    it "rejects missing 'as' keyword" $ do
      case parseModule "export * namespace from 'module';" "test" of
        Left _ -> pure () -- Should fail
        Right _ -> expectationFailure "Expected parse error for missing 'as'"

    it "rejects missing namespace identifier" $ do
      case parseModule "export * as from 'module';" "test" of
        Left _ -> pure () -- Should fail
        Right _ -> expectationFailure "Expected parse error for missing namespace identifier"

    it "rejects missing 'from' keyword" $ do
      case parseModule "export * as namespace 'module';" "test" of
        Left _ -> pure () -- Should fail
        Right _ -> expectationFailure "Expected parse error for missing 'from'"

    it "rejects reserved words as namespace" $ do
      case parseModule "export * as function from 'module';" "test" of
        Left _ -> pure () -- Should fail
        Right _ -> expectationFailure "Expected parse error for reserved word as namespace"

    it "rejects invalid identifier start" $ do
      case parseModule "export * as 123namespace from 'module';" "test" of
        Left _ -> pure () -- Should fail
        Right _ -> expectationFailure "Expected parse error for invalid identifier start"

  describe "mixed export * variations" $ do
    it "parses both export * and export * as in same module" $ do
      let input =
            unlines
              [ "export * from 'module1';",
                "export * as ns2 from 'module2';",
                "export * from 'module3';",
                "export * as ns4 from 'module4';"
              ]
      case parseModule input "test" of
        Right (AST.JSAstModule stmts _) -> do
          length stmts `shouldBe` 4
          -- Verify correct types
          let isExportStar (AST.JSModuleExportDeclaration _ (AST.JSExportAllFrom _ _ _)) = True
              isExportStar _ = False
          let isExportStarAs (AST.JSModuleExportDeclaration _ (AST.JSExportAllAsFrom _ _ _ _ _)) = True
              isExportStarAs _ = False
          let exportStarCount = length (filter isExportStar stmts)
          let exportStarAsCount = length (filter isExportStarAs stmts)
          exportStarCount `shouldBe` 2
          exportStarAsCount `shouldBe` 2
        Right other -> expectationFailure ("Unexpected AST: " ++ show other)
        Left err -> expectationFailure ("Parse failed: " ++ show err)
