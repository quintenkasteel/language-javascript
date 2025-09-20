import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST

main :: IO ()
main = do
  let source = "var x = new WeakSet();"
  case parse source "test" of
    Right ast -> do
      putStrLn "Parsed AST:"
      print ast
    Left err -> putStrLn $ "Parse failed: " ++ err