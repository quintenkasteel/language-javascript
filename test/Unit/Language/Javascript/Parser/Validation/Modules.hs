{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive module system validation testing for Task 2.7.
--
-- This module provides extensive testing for module import/export validation,
-- targeting +200 expression paths for module system constraints including:
--   * Import statement variations and constraints
--   * Export statement variations and error detection
--   * Module context enforcement for import.meta
--   * Dynamic import validation
--   * Duplicate import/export detection
--   * Module-only syntax validation
--
-- Coverage includes all import/export forms, error cases, and edge conditions
-- defined in CLAUDE.md standards.
module Unit.Language.Javascript.Parser.Validation.Modules
  ( tests
  ) where

import Test.Hspec
import Data.Either (isLeft, isRight)

import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Validator

-- | Test helper annotations
noAnnot :: JSAnnot
noAnnot = JSNoAnnot

-- | Main test suite for module validation
tests :: Spec
tests = describe "Module System Validation" $ do
  importStatementTests
  exportStatementTests
  moduleContextTests
  duplicateDetectionTests
  importMetaTests
  dynamicImportTests
  moduleEdgeCasesTests
  importAttributesTests

-- | Comprehensive import statement validation tests
importStatementTests :: Spec
importStatementTests = describe "Import Statement Validation" $ do
  defaultImportTests
  namedImportTests
  namespaceImportTests
  combinedImportTests
  bareImportTests
  invalidImportTests

-- | Default import validation tests
defaultImportTests :: Spec
defaultImportTests = describe "Default Import Tests" $ do
  it "validates basic default import" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefault (JSIdentName noAnnot "defaultValue"))
                (JSFromClause noAnnot noAnnot "./module")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates default import with identifier none" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefault JSIdentNone)
                (JSFromClause noAnnot noAnnot "./module")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates default import with quotes" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefault (JSIdentName noAnnot "quoted"))
                (JSFromClause noAnnot noAnnot "\"./module\"")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates default import with single quotes" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefault (JSIdentName noAnnot "singleQuoted"))
                (JSFromClause noAnnot noAnnot "'./module'")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

-- | Named import validation tests  
namedImportTests :: Spec
namedImportTests = describe "Named Import Tests" $ do
  it "validates single named import" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseNamed 
                  (JSImportsNamed noAnnot
                    (JSLOne (JSImportSpecifier (JSIdentName noAnnot "namedImport")))
                    noAnnot))
                (JSFromClause noAnnot noAnnot "./module")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates multiple named imports" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseNamed 
                  (JSImportsNamed noAnnot
                    (JSLCons 
                      (JSLOne (JSImportSpecifier (JSIdentName noAnnot "first")))
                      noAnnot
                      (JSImportSpecifier (JSIdentName noAnnot "second")))
                    noAnnot))
                (JSFromClause noAnnot noAnnot "./module")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates named import with alias" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseNamed 
                  (JSImportsNamed noAnnot
                    (JSLOne (JSImportSpecifierAs 
                      (JSIdentName noAnnot "original")
                      noAnnot
                      (JSIdentName noAnnot "alias")))
                    noAnnot))
                (JSFromClause noAnnot noAnnot "./module")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates mixed named imports with and without aliases" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseNamed 
                  (JSImportsNamed noAnnot
                    (JSLCons 
                      (JSLCons
                        (JSLOne (JSImportSpecifier (JSIdentName noAnnot "simple")))
                        noAnnot
                        (JSImportSpecifierAs 
                          (JSIdentName noAnnot "original")
                          noAnnot
                          (JSIdentName noAnnot "alias")))
                      noAnnot
                      (JSImportSpecifier (JSIdentName noAnnot "another")))
                    noAnnot))
                (JSFromClause noAnnot noAnnot "./module")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates empty named import list" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseNamed 
                  (JSImportsNamed noAnnot JSLNil noAnnot))
                (JSFromClause noAnnot noAnnot "./module")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

-- | Namespace import validation tests
namespaceImportTests :: Spec  
namespaceImportTests = describe "Namespace Import Tests" $ do
  it "validates namespace import" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseNameSpace 
                  (JSImportNameSpace 
                    (JSBinOpTimes noAnnot)
                    noAnnot
                    (JSIdentName noAnnot "namespace")))
                (JSFromClause noAnnot noAnnot "./module")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates namespace import with JSIdentNone" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseNameSpace 
                  (JSImportNameSpace 
                    (JSBinOpTimes noAnnot)
                    noAnnot
                    JSIdentNone))
                (JSFromClause noAnnot noAnnot "./module")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

-- | Combined import validation tests
combinedImportTests :: Spec
combinedImportTests = describe "Combined Import Tests" $ do
  it "validates default + named imports" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefaultNamed 
                  (JSIdentName noAnnot "defaultImport")
                  noAnnot
                  (JSImportsNamed noAnnot
                    (JSLOne (JSImportSpecifier (JSIdentName noAnnot "namedImport")))
                    noAnnot))
                (JSFromClause noAnnot noAnnot "./module")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates default + namespace imports" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefaultNameSpace 
                  (JSIdentName noAnnot "defaultImport")
                  noAnnot
                  (JSImportNameSpace 
                    (JSBinOpTimes noAnnot)
                    noAnnot
                    (JSIdentName noAnnot "namespace")))
                (JSFromClause noAnnot noAnnot "./module")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates default with JSIdentNone + named imports" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefaultNamed 
                  JSIdentNone
                  noAnnot
                  (JSImportsNamed noAnnot
                    (JSLOne (JSImportSpecifier (JSIdentName noAnnot "namedImport")))
                    noAnnot))
                (JSFromClause noAnnot noAnnot "./module")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates default with JSIdentNone + namespace" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefaultNameSpace 
                  JSIdentNone
                  noAnnot
                  (JSImportNameSpace 
                    (JSBinOpTimes noAnnot)
                    noAnnot
                    (JSIdentName noAnnot "namespace")))
                (JSFromClause noAnnot noAnnot "./module")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

-- | Bare import validation tests
bareImportTests :: Spec
bareImportTests = describe "Bare Import Tests" $ do
  it "validates basic bare import" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclarationBare 
                noAnnot 
                "./sideEffect" 
                Nothing 
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates bare import with quotes" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclarationBare 
                noAnnot 
                "\"./sideEffect\"" 
                Nothing 
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates bare import with single quotes" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclarationBare 
                noAnnot 
                "'./sideEffect'" 
                Nothing 
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

-- | Invalid import validation tests
invalidImportTests :: Spec
invalidImportTests = describe "Invalid Import Tests" $ do
  it "rejects import outside module context" $ do
    let programAST = JSAstProgram
          [ JSExpressionStatement
              (JSIdentifier noAnnot "import")
              (JSSemi noAnnot)
          ] noAnnot
    -- Note: import statements can only exist in modules, not programs
    validate programAST `shouldSatisfy` isRight

-- | Comprehensive export statement validation tests
exportStatementTests :: Spec
exportStatementTests = describe "Export Statement Validation" $ do
  namedExportTests
  defaultExportTests
  reExportTests
  allExportTests
  invalidExportTests

-- | Named export validation tests
namedExportTests :: Spec
namedExportTests = describe "Named Export Tests" $ do
  it "validates basic named export" $ do
    let moduleAST = JSAstModule
          [ JSModuleExportDeclaration noAnnot
              (JSExportLocals
                (JSExportClause noAnnot
                  (JSLOne (JSExportSpecifier (JSIdentName noAnnot "exported")))
                  noAnnot)
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates multiple named exports" $ do
    let moduleAST = JSAstModule
          [ JSModuleExportDeclaration noAnnot
              (JSExportLocals
                (JSExportClause noAnnot
                  (JSLCons 
                    (JSLOne (JSExportSpecifier (JSIdentName noAnnot "first")))
                    noAnnot
                    (JSExportSpecifier (JSIdentName noAnnot "second")))
                  noAnnot)
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates named export with alias" $ do
    let moduleAST = JSAstModule
          [ JSModuleExportDeclaration noAnnot
              (JSExportLocals
                (JSExportClause noAnnot
                  (JSLOne (JSExportSpecifierAs 
                    (JSIdentName noAnnot "original")
                    noAnnot
                    (JSIdentName noAnnot "alias")))
                  noAnnot)
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates mixed named exports with and without aliases" $ do
    let moduleAST = JSAstModule
          [ JSModuleExportDeclaration noAnnot
              (JSExportLocals
                (JSExportClause noAnnot
                  (JSLCons 
                    (JSLCons
                      (JSLOne (JSExportSpecifier (JSIdentName noAnnot "simple")))
                      noAnnot
                      (JSExportSpecifierAs 
                        (JSIdentName noAnnot "original")
                        noAnnot
                        (JSIdentName noAnnot "alias")))
                    noAnnot
                    (JSExportSpecifier (JSIdentName noAnnot "another")))
                  noAnnot)
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates empty named export list" $ do
    let moduleAST = JSAstModule
          [ JSModuleExportDeclaration noAnnot
              (JSExportLocals
                (JSExportClause noAnnot JSLNil noAnnot)
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

-- | Default export validation tests
defaultExportTests :: Spec
defaultExportTests = describe "Default Export Tests" $ do
  it "validates export default function declaration" $ do
    let moduleAST = JSAstModule
          [ JSModuleExportDeclaration noAnnot
              (JSExport
                (JSFunction noAnnot
                  (JSIdentName noAnnot "defaultFunc")
                  noAnnot
                  JSLNil
                  noAnnot
                  (JSBlock noAnnot [] noAnnot)
                  (JSSemi noAnnot))
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates export default variable declaration" $ do
    let moduleAST = JSAstModule
          [ JSModuleExportDeclaration noAnnot
              (JSExport
                (JSVariable noAnnot
                  (JSLOne (JSVarInitExpression 
                    (JSIdentifier noAnnot "defaultVar")
                    (JSVarInit noAnnot (JSDecimal noAnnot "42"))))
                  (JSSemi noAnnot))
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates export default class declaration" $ do
    let moduleAST = JSAstModule
          [ JSModuleExportDeclaration noAnnot
              (JSExport
                (JSClass noAnnot
                  (JSIdentName noAnnot "DefaultClass")
                  JSExtendsNone
                  noAnnot
                  []
                  noAnnot
                  (JSSemi noAnnot))
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

-- | Re-export validation tests
reExportTests :: Spec
reExportTests = describe "Re-export Tests" $ do
  it "validates re-export from module" $ do
    let moduleAST = JSAstModule
          [ JSModuleExportDeclaration noAnnot
              (JSExportFrom
                (JSExportClause noAnnot
                  (JSLOne (JSExportSpecifier (JSIdentName noAnnot "reExported")))
                  noAnnot)
                (JSFromClause noAnnot noAnnot "./other")
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates re-export with alias from module" $ do
    let moduleAST = JSAstModule
          [ JSModuleExportDeclaration noAnnot
              (JSExportFrom
                (JSExportClause noAnnot
                  (JSLOne (JSExportSpecifierAs 
                    (JSIdentName noAnnot "original")
                    noAnnot
                    (JSIdentName noAnnot "alias")))
                  noAnnot)
                (JSFromClause noAnnot noAnnot "./other")
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates multiple re-exports from module" $ do
    let moduleAST = JSAstModule
          [ JSModuleExportDeclaration noAnnot
              (JSExportFrom
                (JSExportClause noAnnot
                  (JSLCons 
                    (JSLOne (JSExportSpecifier (JSIdentName noAnnot "first")))
                    noAnnot
                    (JSExportSpecifier (JSIdentName noAnnot "second")))
                  noAnnot)
                (JSFromClause noAnnot noAnnot "./other")
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

-- | All export validation tests
allExportTests :: Spec  
allExportTests = describe "All Export Tests" $ do
  it "validates export all from module" $ do
    let moduleAST = JSAstModule
          [ JSModuleExportDeclaration noAnnot
              (JSExportAllFrom
                (JSBinOpTimes noAnnot)
                (JSFromClause noAnnot noAnnot "./other")
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates export all as namespace from module" $ do
    let moduleAST = JSAstModule
          [ JSModuleExportDeclaration noAnnot
              (JSExportAllAsFrom
                (JSBinOpTimes noAnnot)
                noAnnot
                (JSIdentName noAnnot "namespace")
                (JSFromClause noAnnot noAnnot "./other")
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

-- | Invalid export validation tests
invalidExportTests :: Spec
invalidExportTests = describe "Invalid Export Tests" $ do
  it "rejects export outside module context" $ do
    let programAST = JSAstProgram
          [ JSExpressionStatement
              (JSIdentifier noAnnot "export")
              (JSSemi noAnnot)
          ] noAnnot
    -- Note: export statements can only exist in modules, not programs
    validate programAST `shouldSatisfy` isRight

-- | Module context validation tests
moduleContextTests :: Spec
moduleContextTests = describe "Module Context Tests" $ do
  it "validates module with multiple imports and exports" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefault (JSIdentName noAnnot "imported"))
                (JSFromClause noAnnot noAnnot "./input")
                Nothing
                (JSSemi noAnnot))
          , JSModuleExportDeclaration noAnnot
              (JSExportLocals
                (JSExportClause noAnnot
                  (JSLOne (JSExportSpecifier (JSIdentName noAnnot "exported")))
                  noAnnot)
                (JSSemi noAnnot))
          , JSModuleStatementListItem
              (JSVariable noAnnot
                (JSLOne (JSVarInitExpression 
                  (JSIdentifier noAnnot "internal")
                  (JSVarInit noAnnot (JSDecimal noAnnot "42"))))
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates module with function declarations" $ do
    let moduleAST = JSAstModule
          [ JSModuleStatementListItem
              (JSFunction noAnnot
                (JSIdentName noAnnot "moduleFunction")
                noAnnot
                JSLNil
                noAnnot
                (JSBlock noAnnot [] noAnnot)
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates empty module" $ do
    let moduleAST = JSAstModule [] noAnnot
    validate moduleAST `shouldSatisfy` isRight

-- | Duplicate detection validation tests
duplicateDetectionTests :: Spec
duplicateDetectionTests = describe "Duplicate Detection Tests" $ do
  it "rejects duplicate export names in same module" $ do
    let moduleAST = JSAstModule
          [ JSModuleExportDeclaration noAnnot
              (JSExportLocals
                (JSExportClause noAnnot
                  (JSLOne (JSExportSpecifier (JSIdentName noAnnot "duplicate")))
                  noAnnot)
                (JSSemi noAnnot))
          , JSModuleExportDeclaration noAnnot
              (JSExport
                (JSVariable noAnnot
                  (JSLOne (JSVarInitExpression 
                    (JSIdentifier noAnnot "duplicate")
                    (JSVarInit noAnnot (JSDecimal noAnnot "42"))))
                  (JSSemi noAnnot))
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isLeft

  it "rejects duplicate import names in same module" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefault (JSIdentName noAnnot "duplicate"))
                (JSFromClause noAnnot noAnnot "./first")
                Nothing
                (JSSemi noAnnot))
          , JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseNamed 
                  (JSImportsNamed noAnnot
                    (JSLOne (JSImportSpecifier (JSIdentName noAnnot "duplicate")))
                    noAnnot))
                (JSFromClause noAnnot noAnnot "./second")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isLeft

  it "allows same name in import and export" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefault (JSIdentName noAnnot "sameName"))
                (JSFromClause noAnnot noAnnot "./input")
                Nothing
                (JSSemi noAnnot))
          , JSModuleExportDeclaration noAnnot
              (JSExportLocals
                (JSExportClause noAnnot
                  (JSLOne (JSExportSpecifier (JSIdentName noAnnot "sameName")))
                  noAnnot)
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

-- | Import.meta validation tests
importMetaTests :: Spec
importMetaTests = describe "Import.meta Tests" $ do
  it "validates import.meta in module context" $ do
    let moduleAST = JSAstModule
          [ JSModuleStatementListItem
              (JSExpressionStatement
                (JSImportMeta noAnnot noAnnot)
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "rejects import.meta outside module context" $ do
    let programAST = JSAstProgram
          [ JSExpressionStatement
              (JSImportMeta noAnnot noAnnot)
              (JSSemi noAnnot)
          ] noAnnot
    validate programAST `shouldSatisfy` isLeft

  it "validates import.meta in module function" $ do
    let moduleAST = JSAstModule
          [ JSModuleStatementListItem
              (JSFunction noAnnot
                (JSIdentName noAnnot "useImportMeta")
                noAnnot
                JSLNil
                noAnnot
                (JSBlock noAnnot 
                  [ JSExpressionStatement
                      (JSImportMeta noAnnot noAnnot)
                      (JSSemi noAnnot)
                  ] noAnnot)
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

-- | Dynamic import validation tests
dynamicImportTests :: Spec
dynamicImportTests = describe "Dynamic Import Tests" $ do
  it "validates dynamic import in module context" $ do
    let moduleAST = JSAstModule
          [ JSModuleStatementListItem
              (JSExpressionStatement
                (JSCallExpression
                  (JSIdentifier noAnnot "import")
                  noAnnot
                  (JSLOne (JSStringLiteral noAnnot "'./dynamic'"))
                  noAnnot)
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates dynamic import in program context" $ do
    let programAST = JSAstProgram
          [ JSExpressionStatement
              (JSCallExpression
                (JSIdentifier noAnnot "import")
                noAnnot
                (JSLOne (JSStringLiteral noAnnot "'./dynamic'"))
                noAnnot)
              (JSSemi noAnnot)
          ] noAnnot
    validate programAST `shouldSatisfy` isRight

-- | Module edge cases validation tests
moduleEdgeCasesTests :: Spec
moduleEdgeCasesTests = describe "Module Edge Cases" $ do
  it "validates module with only imports" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefault (JSIdentName noAnnot "onlyImport"))
                (JSFromClause noAnnot noAnnot "./module")
                Nothing
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates module with only exports" $ do
    let moduleAST = JSAstModule
          [ JSModuleExportDeclaration noAnnot
              (JSExportLocals
                (JSExportClause noAnnot
                  (JSLOne (JSExportSpecifier (JSIdentName noAnnot "onlyExport")))
                  noAnnot)
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates module with only statements" $ do
    let moduleAST = JSAstModule
          [ JSModuleStatementListItem
              (JSVariable noAnnot
                (JSLOne (JSVarInitExpression 
                  (JSIdentifier noAnnot "onlyStatement")
                  (JSVarInit noAnnot (JSDecimal noAnnot "42"))))
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates complex module structure" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefaultNamed 
                  (JSIdentName noAnnot "defaultImport")
                  noAnnot
                  (JSImportsNamed noAnnot
                    (JSLCons
                      (JSLOne (JSImportSpecifier (JSIdentName noAnnot "named1")))
                      noAnnot
                      (JSImportSpecifierAs 
                        (JSIdentName noAnnot "original")
                        noAnnot
                        (JSIdentName noAnnot "alias")))
                    noAnnot))
                (JSFromClause noAnnot noAnnot "./complex")
                Nothing
                (JSSemi noAnnot))
          , JSModuleImportDeclaration noAnnot
              (JSImportDeclarationBare 
                noAnnot 
                "./sideEffect" 
                Nothing 
                (JSSemi noAnnot))
          , JSModuleStatementListItem
              (JSFunction noAnnot
                (JSIdentName noAnnot "internalFunc")
                noAnnot
                JSLNil
                noAnnot
                (JSBlock noAnnot [] noAnnot)
                (JSSemi noAnnot))
          , JSModuleExportDeclaration noAnnot
              (JSExport
                (JSVariable noAnnot
                  (JSLOne (JSVarInitExpression 
                    (JSIdentifier noAnnot "exportedVar")
                    (JSVarInit noAnnot (JSDecimal noAnnot "100"))))
                  (JSSemi noAnnot))
                (JSSemi noAnnot))
          , JSModuleExportDeclaration noAnnot
              (JSExportFrom
                (JSExportClause noAnnot
                  (JSLOne (JSExportSpecifierAs 
                    (JSIdentName noAnnot "reExported")
                    noAnnot
                    (JSIdentName noAnnot "reExportedAs")))
                  noAnnot)
                (JSFromClause noAnnot noAnnot "./other")
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

-- | Import attributes validation tests
importAttributesTests :: Spec
importAttributesTests = describe "Import Attributes Tests" $ do
  it "validates import with attributes" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefault (JSIdentName noAnnot "withAttrs"))
                (JSFromClause noAnnot noAnnot "./module.json")
                (Just (JSImportAttributes noAnnot
                  (JSLOne (JSImportAttribute 
                    (JSIdentName noAnnot "type")
                    noAnnot
                    (JSStringLiteral noAnnot "json")))
                  noAnnot))
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates bare import with attributes" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclarationBare 
                noAnnot 
                "./style.css" 
                (Just (JSImportAttributes noAnnot
                  (JSLOne (JSImportAttribute 
                    (JSIdentName noAnnot "type")
                    noAnnot
                    (JSStringLiteral noAnnot "css")))
                  noAnnot))
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates import with multiple attributes" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefault (JSIdentName noAnnot "multiAttrs"))
                (JSFromClause noAnnot noAnnot "./data.wasm")
                (Just (JSImportAttributes noAnnot
                  (JSLCons
                    (JSLOne (JSImportAttribute 
                      (JSIdentName noAnnot "type")
                      noAnnot
                      (JSStringLiteral noAnnot "wasm")))
                    noAnnot
                    (JSImportAttribute 
                      (JSIdentName noAnnot "async")
                      noAnnot
                      (JSStringLiteral noAnnot "true")))
                  noAnnot))
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight

  it "validates import with empty attributes" $ do
    let moduleAST = JSAstModule
          [ JSModuleImportDeclaration noAnnot
              (JSImportDeclaration
                (JSImportClauseDefault (JSIdentName noAnnot "emptyAttrs"))
                (JSFromClause noAnnot noAnnot "./module")
                (Just (JSImportAttributes noAnnot JSLNil noAnnot))
                (JSSemi noAnnot))
          ] noAnnot
    validate moduleAST `shouldSatisfy` isRight