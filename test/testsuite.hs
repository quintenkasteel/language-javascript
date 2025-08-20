
import Control.Monad (when)
import System.Exit
import Test.Hspec
import Test.Hspec.Runner


import Test.Language.Javascript.AdvancedJavaScriptFeatureTest
import Test.Language.Javascript.AdvancedLexerTest
import Test.Language.Javascript.ASIEdgeCases
import Test.Language.Javascript.ASTConstructorTest
import Test.Language.Javascript.ErrorRecoveryTest
import Test.Language.Javascript.ErrorRecoveryAdvancedTest
import Test.Language.Javascript.ErrorQualityTest
import Test.Language.Javascript.ErrorRecoveryBench
import Test.Language.Javascript.ES6ValidationSimpleTest
import Test.Language.Javascript.ExpressionParser
import Test.Language.Javascript.ExportStar
import Test.Language.Javascript.Generic
-- import Test.Language.Javascript.GoldenTest
import Test.Language.Javascript.Lexer
import Test.Language.Javascript.LiteralParser
import Test.Language.Javascript.Minify
import Test.Language.Javascript.NumericLiteralEdgeCases
import Test.Language.Javascript.ModuleParser
import Test.Language.Javascript.ProgramParser
import Test.Language.Javascript.RoundTrip
import Test.Language.Javascript.SrcLocationTest
import Test.Language.Javascript.StatementParser
import Test.Language.Javascript.StringLiteralComplexity
import Test.Language.Javascript.UnicodeTest
import Test.Language.Javascript.Validator
import Test.Language.Javascript.PropertyTest
-- import Test.Language.Javascript.GeneratorsTest
import qualified Test.Language.Javascript.StrictModeValidationTest as StrictModeValidationTest
import qualified Test.Language.Javascript.ModuleValidationTest as ModuleValidationTest
import qualified Test.Language.Javascript.ControlFlowValidationTest as ControlFlowValidationTest
import qualified Test.Language.Javascript.PerformanceTest as PerformanceTest
import qualified Test.Language.Javascript.MemoryTest as MemoryTest
import qualified Test.Language.Javascript.FuzzingSuite as FuzzingSuite
import qualified Test.Language.Javascript.CompatibilityTest as CompatibilityTest
-- import qualified Test.Language.Javascript.PerformanceAdvancedTest as PerformanceAdvancedTest


main :: IO ()
main = do
    summary <- hspecWithResult defaultConfig testAll
    when (summaryFailures summary == 0)
        exitSuccess
    exitFailure


testAll :: Spec
testAll = do
    testLexer
    testAdvancedLexer
    testUnicode
    testASIEdgeCases
    testLiteralParser
    testStringLiteralComplexity
    testNumericLiteralEdgeCases
    testExpressionParser
    testStatementParser
    testProgramParser
    testModuleParser
    testExportStar
    testRoundTrip
    testMinifyExpr
    testMinifyStmt
    testMinifyProg
    testMinifyModule
    testGenericNFData
    testValidator
    testES6ValidationSimple
    testAdvancedJavaScriptFeatures
    testASTConstructors
    testSrcLocation
    testErrorRecovery
    testAdvancedErrorRecovery
    testErrorQuality
    benchmarkErrorRecovery
    testPropertyInvariants
    -- testGenerators
    StrictModeValidationTest.tests
    ModuleValidationTest.tests
    ControlFlowValidationTest.testControlFlowValidation
    PerformanceTest.performanceTests
    MemoryTest.memoryTests
    -- PerformanceAdvancedTest.advancedPerformanceTests
    FuzzingSuite.testFuzzingSuite
    CompatibilityTest.testRealWorldCompatibility
    -- goldenTests
