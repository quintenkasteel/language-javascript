import Language.JavaScript.Parser

main :: IO ()
main = do
  let source = "new WeakSet()"
  case parse source "test" of
    Right ast -> do
      putStrLn "AST structure for 'new WeakSet()':"
      print ast
    Left err -> putStrLn $ "Parse failed: " ++ err