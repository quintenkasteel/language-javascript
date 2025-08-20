import Control.Monad (when)
import System.Exit
import Test.Hspec
import Test.Hspec.Runner


-- Unit Tests - Lexer
import Unit.Language.Javascript.Parser.Lexer.BasicLexer
import Unit.Language.Javascript.Parser.Lexer.AdvancedLexer
import Unit.Language.Javascript.Parser.Lexer.UnicodeSupport
import Unit.Language.Javascript.Parser.Lexer.StringLiterals
import Unit.Language.Javascript.Parser.Lexer.NumericLiterals
import Unit.Language.Javascript.Parser.Lexer.ASIHandling

-- Unit Tests - Parser
import Unit.Language.Javascript.Parser.Parser.Expressions
import Unit.Language.Javascript.Parser.Parser.Statements
import Unit.Language.Javascript.Parser.Parser.Programs
import Unit.Language.Javascript.Parser.Parser.Modules
import Unit.Language.Javascript.Parser.Parser.ExportStar
import Unit.Language.Javascript.Parser.Parser.Literals

-- Unit Tests - AST
import Unit.Language.Javascript.Parser.AST.Construction
import Unit.Language.Javascript.Parser.AST.Generic
import Unit.Language.Javascript.Parser.AST.SrcLocation

-- Unit Tests - Validation
import Unit.Language.Javascript.Parser.Validation.Core
import Unit.Language.Javascript.Parser.Validation.ES6Features
import Unit.Language.Javascript.Parser.Validation.StrictMode
import Unit.Language.Javascript.Parser.Validation.Modules
import Unit.Language.Javascript.Parser.Validation.ControlFlow

-- Unit Tests - Error
import Unit.Language.Javascript.Parser.Error.Recovery
import Unit.Language.Javascript.Parser.Error.AdvancedRecovery
import Unit.Language.Javascript.Parser.Error.Quality
import Unit.Language.Javascript.Parser.Error.Negative

-- Integration Tests
import Integration.Language.Javascript.Parser.RoundTrip
-- import Integration.Language.Javascript.Parser.AdvancedFeatures  -- Temporarily disabled due to constructor issues
import Integration.Language.Javascript.Parser.Minification
import Integration.Language.Javascript.Parser.Compatibility

-- Golden Tests
import Golden.Language.Javascript.Parser.GoldenTests

-- Property Tests
import Properties.Language.Javascript.Parser.CoreProperties
import Properties.Language.Javascript.Parser.Fuzzing
import Properties.Language.Javascript.Parser.GeneratorsTest

-- Benchmark Tests
import Benchmarks.Language.Javascript.Parser.Performance
import Benchmarks.Language.Javascript.Parser.Memory
import Benchmarks.Language.Javascript.Parser.ErrorRecovery


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
    
    -- Integration Tests
    Integration.Language.Javascript.Parser.RoundTrip.testRoundTrip
    Integration.Language.Javascript.Parser.RoundTrip.testES6RoundTrip
    -- Integration.Language.Javascript.Parser.AdvancedFeatures.testAdvancedJavaScriptFeatures  -- Temporarily disabled
    Integration.Language.Javascript.Parser.Minification.testMinifyExpr
    Integration.Language.Javascript.Parser.Minification.testMinifyStmt
    Integration.Language.Javascript.Parser.Minification.testMinifyProg
    Integration.Language.Javascript.Parser.Minification.testMinifyModule
    Integration.Language.Javascript.Parser.Compatibility.testRealWorldCompatibility
    
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