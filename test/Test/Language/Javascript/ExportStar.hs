{-# LANGUAGE OverloadedStrings #-}
module Test.Language.Javascript.ExportStar
    ( testExportStar
    ) where

import Test.Hspec
import Language.JavaScript.Parser
import qualified Language.JavaScript.Parser.AST as AST

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
            let input = unlines 
                    [ "export * from 'module1';"
                    , "export * from 'module2';"
                    , "export * from 'module3';"
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
            let input = unlines
                    [ "export * from 'all';"
                    , "export { specific } from 'named';"
                    , "export const local = 42;"
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
                Left _ -> pure ()  -- Should fail
                Right _ -> expectationFailure "Expected parse error for missing 'from'"
        
        it "rejects missing module specifier" $ do
            case parseModule "export * from;" "test" of
                Left _ -> pure ()  -- Should fail
                Right _ -> expectationFailure "Expected parse error for missing module specifier"
        
        it "rejects non-string module specifier" $ do
            case parseModule "export * from identifier;" "test" of
                Left _ -> pure ()  -- Should fail
                Right _ -> expectationFailure "Expected parse error for non-string module specifier"
        
        it "rejects numeric module specifier" $ do
            case parseModule "export * from 123;" "test" of
                Left _ -> pure ()  -- Should fail
                Right _ -> expectationFailure "Expected parse error for numeric module specifier"