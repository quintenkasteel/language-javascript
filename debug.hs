{-# LANGUAGE OverloadedStrings #-}
module Main where
import Language.JavaScript.Parser.Parser (parse, parseExpression)

main :: IO ()
main = do
  putStrLn "=== parse empty ==="
  putStrLn $ show $ parse "" "src"
  
  putStrLn "=== parse '42;' ==="
  putStrLn $ show $ parse "42;" "src"
  
  putStrLn "=== parseExpression '42' ==="
  putStrLn $ show $ parseExpression "42" "src"
  
  putStrLn "=== parse 'var x = 42;' ==="
  putStrLn $ show $ parse "var x = 42;" "src"
  
  putStrLn "=== parseExpression 'x' ==="
  putStrLn $ show $ parseExpression "x" "src"
