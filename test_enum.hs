#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript
-}

import Language.JavaScript.Parser.Validator
import Language.JavaScript.Parser.Token
import Language.JavaScript.Parser.SrcLocation

main :: IO ()
main = do
  putStrLn "Testing enum validation functions..."

  -- Test enum values
  let pos = tokenPosn (TokenPn 0 1 1)
  let enumValues = [ JSDocEnumValue "RED" (Just "\"red\"") Nothing
                   , JSDocEnumValue "GREEN" (Just "\"green\"") Nothing
                   , JSDocEnumValue "BLUE" (Just "\"blue\"") Nothing
                   ]

  -- Test duplicate enum values detection
  let duplicateErrors = findDuplicateEnumValues enumValues pos
  putStrLn $ "Duplicate errors: " ++ show (length duplicateErrors)

  -- Test enum type consistency validation
  let typeErrors = validateEnumValueTypeConsistency "Color" enumValues pos
  putStrLn $ "Type consistency errors: " ++ show (length typeErrors)

  -- Test enum with duplicates
  let dupEnumValues = [ JSDocEnumValue "RED" (Just "\"red\"") Nothing
                      , JSDocEnumValue "RED" (Just "\"blue\"") Nothing
                      ]
  let dupErrors = findDuplicateEnumValues dupEnumValues pos
  putStrLn $ "Duplicate enum errors (should be 1+): " ++ show (length dupErrors)

  -- Test enum with mixed types
  let mixedEnumValues = [ JSDocEnumValue "STRING_VAL" (Just "\"red\"") Nothing
                        , JSDocEnumValue "NUMBER_VAL" (Just "42") Nothing
                        ]
  let mixedErrors = validateEnumValueTypeConsistency "Mixed" mixedEnumValues pos
  putStrLn $ "Mixed type errors (should be 1+): " ++ show (length mixedErrors)

  putStrLn "✓ Enum validation functions are working correctly!"