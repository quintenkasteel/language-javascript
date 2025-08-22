#!/usr/bin/env runhaskell

import Language.JavaScript.Parser
import System.CPUTime
import Text.Printf

testCode :: String
testCode = "const x = 42; function hello() { return 'world'; }"

main :: IO ()
main = do
  start <- getCPUTime
  case parse testCode "test" of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right ast -> putStrLn $ "Parse success: " ++ take 50 (show ast) ++ "..."
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  printf "Computation time: %0.3f sec\n" (diff :: Double)