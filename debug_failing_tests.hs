import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Parser.AST
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import qualified Data.Text as Text

main :: IO ()
main = do
  putStrLn "=== Deep Analysis of Failing Tests ==="
  
  -- Test 1: Multi-variable declaration
  putStrLn "\n1. MULTI-VARIABLE TEST:"
  let test1 = "var used = 1, unused = 2; console.log(used);"
  case parse test1 "test1" of
    Right ast -> do
      let optimized = treeShake defaultOptions ast
      putStrLn $ "Source: " ++ test1
      putStrLn "Expected: eliminate 'unused', preserve 'used'"
      putStrLn "Analysis needed: Check variable filtering logic"
      
  -- Test 2: Nested scopes  
  putStrLn "\n2. NESTED SCOPE TEST:"
  let test2 = "function outer() { var used = 1; var unused = 2; return used; } outer();"
  case parse test2 "test2" of
    Right ast -> do
      let optimized = treeShake defaultOptions ast
      putStrLn $ "Source: " ++ test2  
      putStrLn "Expected: eliminate nested 'unused', preserve 'used'"
      putStrLn "Analysis needed: Check function body elimination"
      
  -- Test 3: Closures
  putStrLn "\n3. CLOSURE TEST:"
  let test3 = "function outer() { var captured = 1; return function() { return captured; }; }"
  case parse test3 "test3" of
    Right ast -> do
      let optimized = treeShake defaultOptions ast
      putStrLn $ "Source: " ++ test3
      putStrLn "Expected: preserve 'captured' (used in closure)"
      putStrLn "Analysis needed: Check closure variable analysis"
      
  -- Test 4: Optimization levels
  putStrLn "\n4. OPTIMIZATION LEVELS TEST:"
  let test4 = "var x = 1; function unused() { return x; }"
  case parse test4 "test4" of
    Right ast -> do
      let conservativeResult = treeShake (defaultOptions & optimizationLevel .~ Conservative) ast
      let aggressiveResult = treeShake (defaultOptions & optimizationLevel .~ Aggressive) ast
      putStrLn $ "Source: " ++ test4
      putStrLn "Expected: Conservative != Aggressive results" 
      putStrLn "Analysis needed: Check optimization level implementation"