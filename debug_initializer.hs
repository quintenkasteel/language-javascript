import Language.JavaScript.Parser

main :: IO ()
main = do
  let source = "var unused = new WeakSet();"
  case parse source "test" of
    Right ast -> do
      putStrLn "Full AST for var unused = new WeakSet();"
      print ast
    Left err -> putStrLn $ "Parse failed: " ++ err