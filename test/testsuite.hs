
import Control.Monad (when)
import System.Exit
import Test.Hspec
import Test.Hspec.Runner


import Test.Language.Javascript.AdvancedLexerTest
import Test.Language.Javascript.ASIEdgeCases
import Test.Language.Javascript.ASTConstructorTest
import Test.Language.Javascript.ErrorRecoveryTest
import Test.Language.Javascript.ErrorQualityTest
import Test.Language.Javascript.ErrorRecoveryBench
import Test.Language.Javascript.ExpressionParser
import Test.Language.Javascript.ExportStar
import Test.Language.Javascript.Generic
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
    testASTConstructors
    testSrcLocation
    testErrorRecovery
    testErrorQuality
    benchmarkErrorRecovery
