{-# LANGUAGE OverloadedStrings #-}

-- |
-- Runtime Validation Demo
--
-- Demonstrates the JSDoc runtime validation system with concrete examples
-- showing how JavaScript functions can be validated at runtime using their
-- JSDoc type annotations.

module Main where

import Language.JavaScript.Runtime.Integration

main :: IO ()
main = do
  putStrLn "🚀 JSDoc Runtime Validation Enhancement Demo"
  putStrLn "=============================================="
  putStrLn ""

  -- Show the complete workflow
  showValidationWorkflow

  putStrLn ""
  putStrLn "=== Interactive Demo ==="

  -- Run the demonstration
  demonstrateRuntimeValidation

  putStrLn ""
  putStrLn "🎉 Runtime validation enhancement complete!"
  putStrLn ""
  putStrLn "Key Benefits:"
  putStrLn "• ✅ JSDoc types are now executable validation rules"
  putStrLn "• ✅ Function parameters validated against JSDoc @param types"
  putStrLn "• ✅ Return values validated against JSDoc @returns types"
  putStrLn "• ✅ Complex types supported (objects, arrays, unions)"
  putStrLn "• ✅ Rich error messages with type information"
  putStrLn "• ✅ Configurable validation (development/production modes)"
  putStrLn ""
  putStrLn "This enhancement transforms JSDoc from documentation"
  putStrLn "into a runtime type safety system for JavaScript!"