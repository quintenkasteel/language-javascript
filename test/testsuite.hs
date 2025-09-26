-- Unit Tests - Lexer

-- Unit Tests - Parser

-- Unit Tests - AST

-- Unit Tests - Pretty Printing

-- Unit Tests - Validation

-- Unit Tests - Error

-- Integration Tests

-- import Integration.Language.Javascript.Parser.AdvancedFeatures  -- Temporarily disabled due to constructor issues

-- Golden Tests

-- Property Tests

-- Benchmark Tests

import Benchmarks.Language.Javascript.Parser.ErrorRecovery
import Benchmarks.Language.Javascript.Parser.Memory
import Benchmarks.Language.Javascript.Parser.Performance
import Control.Monad (when)
import Golden.Language.Javascript.Parser.GoldenTests
import Integration.Language.Javascript.Parser.Compatibility
import Integration.Language.Javascript.Parser.Minification
import Integration.Language.Javascript.Parser.RoundTrip
import Properties.Language.Javascript.Parser.CoreProperties
import Properties.Language.Javascript.Parser.Fuzzing
import Properties.Language.Javascript.Parser.GeneratorsTest
import System.Exit
import Test.Hspec
import Test.Hspec.Runner
import Unit.Language.Javascript.Parser.AST.Construction
import Unit.Language.Javascript.Parser.AST.Generic
import Unit.Language.Javascript.Parser.AST.SrcLocation
import Unit.Language.Javascript.Parser.Error.AdvancedRecovery
import Unit.Language.Javascript.Parser.Error.Negative
import Unit.Language.Javascript.Parser.Error.Quality
import Unit.Language.Javascript.Parser.Error.Recovery
import Unit.Language.Javascript.Parser.Lexer.ASIHandling
import Unit.Language.Javascript.Parser.Lexer.AdvancedLexer
import Unit.Language.Javascript.Parser.Lexer.BasicLexer
import Unit.Language.Javascript.Parser.Lexer.NumericLiterals
import Unit.Language.Javascript.Parser.Lexer.StringLiterals
import Unit.Language.Javascript.Parser.Lexer.UnicodeSupport
import Unit.Language.Javascript.Parser.Parser.ExportStar
import Unit.Language.Javascript.Parser.Parser.Expressions
import Unit.Language.Javascript.Parser.Parser.Literals
import Unit.Language.Javascript.Parser.Parser.Modules
import Unit.Language.Javascript.Parser.Parser.Programs
import Unit.Language.Javascript.Parser.Parser.Statements
import Unit.Language.Javascript.Parser.Pretty.JSONTest
import Unit.Language.Javascript.Parser.Pretty.SExprTest
import Unit.Language.Javascript.Parser.Pretty.XMLTest
import Unit.Language.Javascript.Parser.Validation.ControlFlow
import Unit.Language.Javascript.Parser.Validation.Core
import Unit.Language.Javascript.Parser.Validation.ES6Features
import Unit.Language.Javascript.Parser.Validation.Modules
import Unit.Language.Javascript.Parser.Validation.StrictMode
import Unit.Language.Javascript.Process.TreeShake.Core
import Unit.Language.Javascript.Process.TreeShake.Advanced
import Unit.Language.Javascript.Process.TreeShake.Stress
import Unit.Language.Javascript.Process.TreeShake.FrameworkPatterns
import Unit.Language.Javascript.Process.TreeShake.AdvancedJSEdgeCases
import Unit.Language.Javascript.Process.TreeShake.LibraryPatterns
import Unit.Language.Javascript.Process.TreeShake.IntegrationScenarios
import Unit.Language.Javascript.Process.TreeShake.EnterpriseScale
-- import Unit.Language.Javascript.Process.TreeShake.Usage
-- import Unit.Language.Javascript.Process.TreeShake.Elimination
import Integration.Language.Javascript.Process.TreeShake
import Test.Language.Javascript.JSDocTest
import Unit.Language.Javascript.Runtime.ValidatorTest

main :: IO ()
main = do
  summary <- hspecWithResult defaultConfig testAll
  when (summaryFailures summary == 0)
    exitSuccess
  exitFailure

testAll :: Spec
testAll = do
  -- Unit Tests - Lexer
  Unit.Language.Javascript.Parser.Lexer.BasicLexer.testLexer
  Unit.Language.Javascript.Parser.Lexer.AdvancedLexer.testAdvancedLexer
  Unit.Language.Javascript.Parser.Lexer.UnicodeSupport.testUnicode
  Unit.Language.Javascript.Parser.Lexer.StringLiterals.testStringLiteralComplexity
  Unit.Language.Javascript.Parser.Lexer.NumericLiterals.testNumericLiteralEdgeCases
  Unit.Language.Javascript.Parser.Lexer.ASIHandling.testASIEdgeCases

  -- Unit Tests - Parser
  Unit.Language.Javascript.Parser.Parser.Expressions.testExpressionParser
  Unit.Language.Javascript.Parser.Parser.Statements.testStatementParser
  Unit.Language.Javascript.Parser.Parser.Programs.testProgramParser
  Unit.Language.Javascript.Parser.Parser.Modules.testModuleParser
  Unit.Language.Javascript.Parser.Parser.ExportStar.testExportStar
  Unit.Language.Javascript.Parser.Parser.Literals.testLiteralParser

  -- Unit Tests - AST
  Unit.Language.Javascript.Parser.AST.Construction.testASTConstructors
  Unit.Language.Javascript.Parser.AST.Generic.testGenericNFData
  Unit.Language.Javascript.Parser.AST.SrcLocation.testSrcLocation

  -- Unit Tests - Pretty Printing
  Unit.Language.Javascript.Parser.Pretty.JSONTest.testJSONSerialization
  Unit.Language.Javascript.Parser.Pretty.XMLTest.testXMLSerialization
  Unit.Language.Javascript.Parser.Pretty.SExprTest.testSExprSerialization

  -- Unit Tests - Validation
  Unit.Language.Javascript.Parser.Validation.Core.testValidator
  Unit.Language.Javascript.Parser.Validation.ES6Features.testES6ValidationSimple
  Unit.Language.Javascript.Parser.Validation.StrictMode.tests
  Unit.Language.Javascript.Parser.Validation.Modules.tests
  Unit.Language.Javascript.Parser.Validation.ControlFlow.testControlFlowValidation

  -- Unit Tests - Error
  Unit.Language.Javascript.Parser.Error.Recovery.testErrorRecovery
  Unit.Language.Javascript.Parser.Error.AdvancedRecovery.testAdvancedErrorRecovery
  Unit.Language.Javascript.Parser.Error.Quality.testErrorQuality
  Unit.Language.Javascript.Parser.Error.Negative.testNegativeCases

  -- Unit Tests - TreeShake
  Unit.Language.Javascript.Process.TreeShake.Core.testTreeShakeCore
  Unit.Language.Javascript.Process.TreeShake.Advanced.testTreeShakeAdvanced
  Unit.Language.Javascript.Process.TreeShake.Stress.testTreeShakeStress
  Unit.Language.Javascript.Process.TreeShake.FrameworkPatterns.frameworkPatternsTests
  Unit.Language.Javascript.Process.TreeShake.AdvancedJSEdgeCases.advancedJSEdgeCasesTests
  Unit.Language.Javascript.Process.TreeShake.LibraryPatterns.libraryPatternsTests
  Unit.Language.Javascript.Process.TreeShake.IntegrationScenarios.integrationScenariosTests
  Unit.Language.Javascript.Process.TreeShake.EnterpriseScale.enterpriseScaleTests
  -- Unit.Language.Javascript.Process.TreeShake.Usage.testUsageAnalysis
  -- Unit.Language.Javascript.Process.TreeShake.Elimination.testEliminationCore

  -- Unit Tests - JSDoc
  Test.Language.Javascript.JSDocTest.tests

  -- Unit Tests - Runtime Validation
  Unit.Language.Javascript.Runtime.ValidatorTest.validatorTests

  -- Integration Tests
  Integration.Language.Javascript.Parser.RoundTrip.testRoundTrip
  Integration.Language.Javascript.Parser.RoundTrip.testES6RoundTrip
  -- Integration.Language.Javascript.Parser.AdvancedFeatures.testAdvancedJavaScriptFeatures  -- Temporarily disabled
  Integration.Language.Javascript.Parser.Minification.testMinifyExpr
  Integration.Language.Javascript.Parser.Minification.testMinifyStmt
  Integration.Language.Javascript.Parser.Minification.testMinifyProg
  Integration.Language.Javascript.Parser.Minification.testMinifyModule
  Integration.Language.Javascript.Parser.Compatibility.testRealWorldCompatibility
  Integration.Language.Javascript.Process.TreeShake.testTreeShakeIntegration

  -- Golden Tests
  Golden.Language.Javascript.Parser.GoldenTests.goldenTests

  -- Property Tests
  Properties.Language.Javascript.Parser.CoreProperties.testPropertyInvariants
  Properties.Language.Javascript.Parser.Fuzzing.testFuzzingSuite
  Properties.Language.Javascript.Parser.GeneratorsTest.testGenerators

  -- Benchmark Tests
  Benchmarks.Language.Javascript.Parser.Performance.performanceTests
  Benchmarks.Language.Javascript.Parser.Memory.memoryTests
  Benchmarks.Language.Javascript.Parser.ErrorRecovery.benchmarkErrorRecovery
