import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Parser.AST
import Language.JavaScript.Process.TreeShake.Types
import qualified Language.JavaScript.Process.TreeShake.Types as Types
import qualified Data.Text as Text
import Control.Lens ((^.))

main :: IO ()
main = do
  let source = "var friends = new WeakSet();"
  case parse source "test" of
    Right (JSAstProgram (JSStatementList stmts)) ->
      case stmts of
        [JSVariable _ decls _] ->
          case fromCommaList decls of
            [JSVarInitExpression (JSIdentifier _ name) initializer] -> do
              putStrLn $ "=== VAR DECLARATION DEBUG ==="
              putStrLn $ "Variable name: " ++ name
              putStrLn $ "Initializer: " ++ show initializer

              -- Now analyze usage
              let ast = JSAstProgram (JSStatementList stmts)
              let analysis = analyzeUsage ast
              let usage = analysis ^. usageMap

              putStrLn $ "Is friends used: " ++ show (Types.isIdentifierUsed (Text.pack "friends") usage)

            _ -> putStrLn "Unexpected variable declaration structure"
        _ -> putStrLn "Unexpected statement structure"
    _ -> putStrLn "Parse failed or unexpected AST structure"