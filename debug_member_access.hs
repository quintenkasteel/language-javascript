import Language.JavaScript.Parser
import Language.JavaScript.Process.TreeShake

main :: IO ()
main = do
  putStrLn "=== MEMBER ACCESS DEBUG ==="

  -- Test 1: Simple identifier usage
  let source1 = "var x = 42; console.log(x);"
  case parse source1 "test" of
    Right ast -> do
      putStrLn "Test 1 (simple identifier):"
      putStrLn $ "Source: " ++ source1
      let optimized = treeShake defaultOptions ast
      let astString = show optimized
      if "\"x\"" `elem` words astString || " x " `elem` [astString]
      then putStrLn "Result: x preserved"
      else putStrLn "Result: x eliminated"
    Left err -> putStrLn $ "Parse failed: " ++ err

  putStrLn ""

  -- Test 2: Member access usage
  let source2 = "var obj = {}; console.log(obj.prop);"
  case parse source2 "test" of
    Right ast -> do
      putStrLn "Test 2 (member access):"
      putStrLn $ "Source: " ++ source2
      let optimized = treeShake defaultOptions ast
      let astString = show optimized
      if "obj" `elem` words astString
      then putStrLn "Result: obj preserved"
      else putStrLn "Result: obj eliminated"
    Left err -> putStrLn $ "Parse failed: " ++ err

  putStrLn ""

  -- Test 3: Method call usage
  let source3 = "var obj = {}; obj.method();"
  case parse source3 "test" of
    Right ast -> do
      putStrLn "Test 3 (method call):"
      putStrLn $ "Source: " ++ source3
      let optimized = treeShake defaultOptions ast
      let astString = show optimized
      if "obj" `elem` words astString
      then putStrLn "Result: obj preserved"
      else putStrLn "Result: obj eliminated"
    Left err -> putStrLn $ "Parse failed: " ++ err