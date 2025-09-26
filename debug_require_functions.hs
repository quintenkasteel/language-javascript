#!/usr/bin/env cabal
{- cabal:
build-depends: base, language-javascript
-}

{-# LANGUAGE OverloadedStrings #-}

import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Parser.AST

-- Copy of the functions from Elimination.hs for debugging
isRequireCall :: JSExpression -> Bool
isRequireCall (JSIdentifier _ "require") = True
isRequireCall _ = False

isRequireCallExpression :: JSExpression -> Bool
isRequireCallExpression (JSCallExpression fn _ _ _) = isRequireCall fn
isRequireCallExpression (JSMemberExpression fn _ _ _) = isRequireCall fn  -- require('module') call
isRequireCallExpression _ = False

hasInitializerSideEffectsDebug :: JSExpression -> Bool
hasInitializerSideEffectsDebug expr = case expr of
  -- Direct require call
  JSMemberExpression fn _ _ _ -> do
    let isReq = isRequireCall fn
    let result = not isReq
    -- Debug output would go here if we could
    result

  -- Member access on require result
  JSCallExpressionDot baseExpr _ _ -> do
    let isReqCall = isRequireCallExpression baseExpr
    let result = not isReqCall
    -- Debug output would go here if we could
    result

  -- Default: has side effects
  _ -> True

main :: IO ()
main = do
  putStrLn "=== Require Function Debug ==="

  let source1 = "var unused = require('crypto');"
  putStrLn $ "\n--- Direct require: " ++ source1
  case parse source1 "test" of
    Right (JSAstProgram [JSVariable _ (JSLOne (JSVarInitExpression _ (JSVarInit _ expr))) _] _) -> do
      putStrLn $ "isRequireCall result for direct require: " ++ show (case expr of JSMemberExpression fn _ _ _ -> isRequireCall fn; _ -> False)
      putStrLn $ "hasInitializerSideEffectsDebug result: " ++ show (hasInitializerSideEffectsDebug expr)
    _ -> putStrLn "Parse failed"

  let source2 = "var useEffect = require('react').useEffect;"
  putStrLn $ "\n--- Member access require: " ++ source2
  case parse source2 "test" of
    Right (JSAstProgram [JSVariable _ (JSLOne (JSVarInitExpression _ (JSVarInit _ expr))) _] _) -> do
      case expr of
        JSCallExpressionDot baseExpr _ _ -> do
          putStrLn $ "isRequireCallExpression result for base: " ++ show (isRequireCallExpression baseExpr)
          case baseExpr of
            JSMemberExpression fn _ _ _ -> do
              putStrLn $ "isRequireCall result for function: " ++ show (isRequireCall fn)
            _ -> putStrLn "Base is not JSMemberExpression"
        _ -> putStrLn "Not JSCallExpressionDot"
      putStrLn $ "hasInitializerSideEffectsDebug result: " ++ show (hasInitializerSideEffectsDebug expr)
    _ -> putStrLn "Parse failed"