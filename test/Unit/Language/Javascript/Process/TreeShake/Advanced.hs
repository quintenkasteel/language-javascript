{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | Advanced comprehensive tests for JavaScript tree shaking functionality.
--
-- This module provides comprehensive testing of complex real-world scenarios
-- for the tree shaking implementation, covering edge cases, performance,
-- and advanced JavaScript patterns that go beyond basic elimination.
--
-- Test categories covered:
--   * Complex dependency chains and transitive references
--   * Advanced JavaScript patterns (closures, hoisting, prototypes)
--   * Modern JavaScript features (async/await, generators, classes)
--   * Error recovery and malformed input handling
--   * Performance and scalability testing
--   * Cross-module dependency optimization
--   * Dynamic code patterns and runtime behaviors
--
-- All tests follow CLAUDE.md standards with real functionality testing
-- and comprehensive coverage of complex scenarios.
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Process.TreeShake.Advanced
  ( testTreeShakeAdvanced,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Lens.Micro ((.~), (&))
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Parser (parse, parseModule)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
import Test.Hspec

-- | Main test suite for advanced tree shaking scenarios.
testTreeShakeAdvanced :: Spec
testTreeShakeAdvanced = describe "TreeShake Advanced Tests" $ do
  testComplexDependencyChains
  testAdvancedJavaScriptPatterns
  testModernJavaScriptFeatures
  testCrossModuleDependencies
  testDynamicCodePatterns
  testErrorRecoveryScenarios
  testPerformanceScenarios
  testRealWorldCodeBases
  testAdvancedEdgeCasePatterns

-- | Test complex dependency chains and transitive references.
testComplexDependencyChains :: Spec
testComplexDependencyChains = describe "Complex Dependency Chains" $ do
  it "handles deep transitive dependencies" $ do
    let source = unlines
          [ "function a() { return b() + 1; }"
          , "function b() { return c() * 2; }"
          , "function c() { return d() - 3; }"
          , "function d() { return getValue(); }"
          , "function getValue() { return 42; }"
          , "function unused1() { return unused2(); }"
          , "function unused2() { return 0; }"
          , "console.log(a());"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve entire chain a->b->c->d->getValue
        astShouldContainIdentifier optimized "a"
        astShouldContainIdentifier optimized "b"
        astShouldContainIdentifier optimized "c"
        astShouldContainIdentifier optimized "d"
        astShouldContainIdentifier optimized "getValue"
        -- Should eliminate unused chain
        astShouldNotContainIdentifier optimized "unused1"
        astShouldNotContainIdentifier optimized "unused2"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles circular dependencies" $ do
    let source = unlines
          [ "function circularA() { return circularB() + 1; }"
          , "function circularB() { return circularC() + 1; }"
          , "function circularC() { return circularA() + 1; }"
          , "var result = circularA();"
          , "function unused() { return 0; }"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve circular dependency chain
        astShouldContainIdentifier optimized "circularA"
        astShouldContainIdentifier optimized "circularB"
        astShouldContainIdentifier optimized "circularC"
        astShouldContainIdentifier optimized "result"
        -- Should eliminate unused function
        astShouldNotContainIdentifier optimized "unused"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles conditional dependencies" $ do
    let source = unlines
          [ "var condition = true;"
          , "function conditionalMain() {"
          , "  if (condition) {"
          , "    return branchA();"
          , "  } else {"
          , "    return branchB();"
          , "  }"
          , "}"
          , "function branchA() { return helperA(); }"
          , "function branchB() { return helperB(); }"
          , "function helperA() { return 'A'; }"
          , "function helperB() { return 'B'; }"
          , "function totallyUnused() { return 'unused'; }"
          , "console.log(conditionalMain());"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve all reachable functions (both branches)
        astShouldContainIdentifier optimized "conditionalMain"
        astShouldContainIdentifier optimized "branchA"
        astShouldContainIdentifier optimized "branchB"
        astShouldContainIdentifier optimized "helperA"
        astShouldContainIdentifier optimized "helperB"
        astShouldContainIdentifier optimized "condition"
        -- Should eliminate unreachable function
        astShouldNotContainIdentifier optimized "totallyUnused"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test advanced JavaScript patterns (closures, hoisting, prototypes).
testAdvancedJavaScriptPatterns :: Spec
testAdvancedJavaScriptPatterns = describe "Advanced JavaScript Patterns" $ do
  it "handles closures and captured variables" $ do
    let source = unlines
          [ "function outerFunction(param) {"
          , "  var capturedVar = param + 1;"
          , "  var unusedVar = 'unused';"
          , "  return function innerFunction() {"
          , "    return capturedVar * 2;"
          , "  };"
          , "}"
          , "var closure = outerFunction(5);"
          , "var result = closure();"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve closure and captured variables
        astShouldContainIdentifier optimized "outerFunction"
        astShouldContainIdentifier optimized "capturedVar"
        astShouldContainIdentifier optimized "closure"
        astShouldContainIdentifier optimized "result"
        -- Should eliminate unused variables in closure scope
        astShouldNotContainIdentifier optimized "unusedVar"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles function hoisting scenarios" $ do
    let source = unlines
          [ "console.log(hoistedFunction());"  -- Called before declaration
          , "var x = regularVar;"  -- Used before declaration
          , "function hoistedFunction() { return 'hoisted'; }"
          , "var regularVar = 42;"
          , "function unusedHoisted() { return 'unused'; }"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve hoisted function and variable
        astShouldContainIdentifier optimized "hoistedFunction"
        astShouldContainIdentifier optimized "regularVar"
        astShouldContainIdentifier optimized "x"
        -- Should eliminate unused hoisted function
        astShouldNotContainIdentifier optimized "unusedHoisted"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles prototype chain manipulation" $ do
    let source = unlines
          [ "function Constructor() {"
          , "  this.property = 'value';"
          , "}"
          , "Constructor.prototype.method = function() {"
          , "  return this.property;"
          , "};"
          , "Constructor.prototype.unusedMethod = function() {"
          , "  return 'unused';"
          , "};"
          , "var instance = new Constructor();"
          , "console.log(instance.method());"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve constructor and used prototype method
        astShouldContainIdentifier optimized "Constructor"
        astShouldContainIdentifier optimized "instance"
        -- Note: Prototype analysis is complex, may preserve more than ideal
        astShouldContainIdentifier optimized "method"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test modern JavaScript features (async/await, generators, classes).
testModernJavaScriptFeatures :: Spec
testModernJavaScriptFeatures = describe "Modern JavaScript Features" $ do
  it "handles async/await patterns" $ do
    let source = unlines
          [ "async function fetchData() {"
          , "  const response = await fetch('/api/data');"
          , "  return response.json();"
          , "}"
          , "async function processData() {"
          , "  const data = await fetchData();"
          , "  return data.processed;"
          , "}"
          , "async function unusedAsync() {"
          , "  await fetch('/unused');"
          , "}"
          , "processData().then(console.log);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve async function chain
        astShouldContainIdentifier optimized "fetchData"
        astShouldContainIdentifier optimized "processData"
        -- Should eliminate unused async function
        astShouldNotContainIdentifier optimized "unusedAsync"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles generator functions" $ do
    let source = unlines
          [ "function* usedGenerator() {"
          , "  yield 1;"
          , "  yield 2;"
          , "  return 3;"
          , "}"
          , "function* unusedGenerator() {"
          , "  yield 'unused';"
          , "}"
          , "const iterator = usedGenerator();"
          , "console.log(iterator.next().value);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve used generator
        astShouldContainIdentifier optimized "usedGenerator"
        astShouldContainIdentifier optimized "iterator"
        -- Should eliminate unused generator
        astShouldNotContainIdentifier optimized "unusedGenerator"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles ES6 classes with inheritance" $ do
    let source = unlines
          [ "class BaseClass {"
          , "  constructor(value) {"
          , "    this.value = value;"
          , "  }"
          , "  getValue() {"
          , "    return this.value;"
          , "  }"
          , "  unusedMethod() {"
          , "    return 'unused';"
          , "  }"
          , "}"
          , "class ExtendedClass extends BaseClass {"
          , "  constructor(value, extra) {"
          , "    super(value);"
          , "    this.extra = extra;"
          , "  }"
          , "  getTotal() {"
          , "    return this.getValue() + this.extra;"
          , "  }"
          , "}"
          , "class UnusedClass {"
          , "  method() { return 'unused'; }"
          , "}"
          , "const instance = new ExtendedClass(10, 5);"
          , "console.log(instance.getTotal());"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve class inheritance chain
        astShouldContainIdentifier optimized "BaseClass"
        astShouldContainIdentifier optimized "ExtendedClass"
        astShouldContainIdentifier optimized "instance"
        -- Should eliminate unused class
        astShouldNotContainIdentifier optimized "UnusedClass"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test cross-module dependency optimization.
testCrossModuleDependencies :: Spec
testCrossModuleDependencies = describe "Cross-Module Dependencies" $ do
  it "handles complex ES6 module imports" $ do
    let source = unlines
          [ "import { usedFunction, unusedFunction } from 'utils';"
          , "import defaultExport from 'helpers';"
          , "import * as namespace from 'tools';"
          , "import 'side-effects-only';"
          , "import { alias as renamedImport } from 'renamed';"
          , ""
          , "console.log(usedFunction());"
          , "console.log(defaultExport());"
          , "console.log(namespace.method());"
          , "console.log(renamedImport());"
          ]
    case parseModule source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve used imports
        astShouldContainIdentifier optimized "usedFunction"
        astShouldContainIdentifier optimized "defaultExport"
        astShouldContainIdentifier optimized "namespace"
        astShouldContainIdentifier optimized "renamedImport"
        -- Should eliminate unused named import
        astShouldNotContainIdentifier optimized "unusedFunction"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles re-export patterns" $ do
    let source = unlines
          [ "export { usedExport, unusedExport } from 'source';"
          , "export { default as renamedDefault } from 'other';"
          , "export * from 'everything';"
          , ""
          , "import { usedExport } from './this-module';"
          , "console.log(usedExport());"
          ]
    case parseModule source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve used re-exports
        astShouldContainIdentifier optimized "usedExport"
        -- Export * should be preserved (side effects)
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test dynamic code patterns and runtime behaviors.
testDynamicCodePatterns :: Spec
testDynamicCodePatterns = describe "Dynamic Code Patterns" $ do
  it "handles dynamic property access" $ do
    let source = unlines
          [ "var obj = {"
          , "  usedProp: 'used',"
          , "  unusedProp: 'unused'"
          , "};"
          , "var key = 'usedProp';"
          , "var dynamicKey = 'computed' + 'Key';"
          , "var unused = 'unused';"
          , ""
          , "console.log(obj[key]);"
          , "console.log(obj[dynamicKey]);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve object and dynamic access variables
        astShouldContainIdentifier optimized "obj"
        astShouldContainIdentifier optimized "key"
        astShouldContainIdentifier optimized "dynamicKey"
        -- Should eliminate unused variable
        astShouldNotContainIdentifier optimized "unused"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles function construction and eval variants" $ do
    let source = unlines
          [ "var usedInEval = 'will be used in eval';"
          , "var usedInFunction = 'will be used in Function';"
          , "var unused = 'completely unused';"
          , ""
          , "eval('console.log(usedInEval);');"
          , "var dynamicFunc = new Function('return usedInFunction;');"
          , "console.log(dynamicFunc());"
          ]
    case parse source "test" of
      Right ast -> do
        let opts = defaultOptions & aggressiveShaking .~ False  -- Conservative mode
        let optimized = treeShake opts ast
        -- Should preserve variables in conservative mode due to eval/Function
        astShouldContainIdentifier optimized "usedInEval"
        astShouldContainIdentifier optimized "usedInFunction"
        astShouldContainIdentifier optimized "dynamicFunc"
        -- In conservative mode, unused should still be eliminated
        astShouldNotContainIdentifier optimized "unused"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test error recovery scenarios.
testErrorRecoveryScenarios :: Spec
testErrorRecoveryScenarios = describe "Error Recovery Scenarios" $ do
  it "handles malformed but parseable code gracefully" $ do
    let source = unlines
          [ "var x = 1  // missing semicolon"
          , "var y = 2;"
          , "console.log(x + y)  // missing semicolon"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve used variables despite missing semicolons
        astShouldContainIdentifier optimized "x"
        astShouldContainIdentifier optimized "y"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles complex nested structures" $ do
    let source = unlines
          [ "var config = {"
          , "  nested: {"
          , "    deep: {"
          , "      value: {"
          , "        used: 'important',"
          , "        unused: 'not needed'"
          , "      }"
          , "    }"
          , "  },"
          , "  other: 'unused branch'"
          , "};"
          , "console.log(config.nested.deep.value.used);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve config object (conservative for object property access)
        astShouldContainIdentifier optimized "config"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test performance scenarios.
testPerformanceScenarios :: Spec
testPerformanceScenarios = describe "Performance Scenarios" $ do
  it "handles large numbers of variables efficiently" $ do
    let generateVars n = unlines $
          [ "var used1 = 1;" ] ++
          [ "var unused" ++ show i ++ " = " ++ show i ++ ";" | i <- [2..n] ] ++
          [ "console.log(used1);" ]
    let source = generateVars 100  -- 100 variables, only 1 used
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve only the used variable
        astShouldContainIdentifier optimized "used1"
        -- Should eliminate many unused variables (spot check a few)
        astShouldNotContainIdentifier optimized "unused2"
        astShouldNotContainIdentifier optimized "unused50"
        astShouldNotContainIdentifier optimized "unused100"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles deeply nested function calls" $ do
    let generateNestedCalls depth =
          let functions = [ "function func" ++ show i ++ "() { return func" ++ show (i+1) ++ "(); }"
                          | i <- [1..depth] ]
              lastFunction = "function func" ++ show (depth + 1) ++ "() { return 42; }"
              call = "console.log(func1());"
          in unlines (functions ++ [lastFunction, call])
    let source = generateNestedCalls 50  -- 50 levels deep
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve entire call chain
        astShouldContainIdentifier optimized "func1"
        astShouldContainIdentifier optimized "func25"
        astShouldContainIdentifier optimized "func51"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test real-world codebase patterns.
testRealWorldCodeBases :: Spec
testRealWorldCodeBases = describe "Real-World CodeBase Patterns" $ do
  it "handles React-like component patterns" $ do
    let source = unlines
          [ "function useState(initial) {"
          , "  return [initial, function(newValue) { return newValue; }];"
          , "}"
          , "function useEffect(callback, deps) {"
          , "  callback();"
          , "}"
          , "function unusedHook() { return 'unused'; }"
          , ""
          , "function MyComponent() {"
          , "  const [state, setState] = useState(0);"
          , "  useEffect(function() { console.log('mounted'); }, []);"
          , "  return { render: function() { return state; } };"
          , "}"
          , ""
          , "var app = MyComponent();"
          , "console.log(app.render());"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve component and used hooks
        astShouldContainIdentifier optimized "MyComponent"
        astShouldContainIdentifier optimized "useState"
        astShouldContainIdentifier optimized "useEffect"
        astShouldContainIdentifier optimized "app"
        -- Should eliminate unused hook
        astShouldNotContainIdentifier optimized "unusedHook"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles Node.js-like module patterns" $ do
    let source = unlines
          [ "var fs = require('fs');"
          , "var path = require('path');"
          , "var unused = require('unused-module');"
          , ""
          , "function readConfig() {"
          , "  return fs.readFileSync(path.join(__dirname, 'config.json'));"
          , "}"
          , ""
          , "function writeLog(message) {"
          , "  fs.writeFileSync('log.txt', message);"
          , "}"
          , ""
          , "function unusedFunction() {"
          , "  return 'unused';"
          , "}"
          , ""
          , "var config = readConfig();"
          , "writeLog('Application started');"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve used modules and functions
        astShouldContainIdentifier optimized "fs"
        astShouldContainIdentifier optimized "path"
        astShouldContainIdentifier optimized "readConfig"
        astShouldContainIdentifier optimized "writeLog"
        astShouldContainIdentifier optimized "config"
        -- Should eliminate unused module and function
        astShouldNotContainIdentifier optimized "unused"
        astShouldNotContainIdentifier optimized "unusedFunction"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles utility library patterns" $ do
    let source = unlines
          [ "var utils = {"
          , "  debounce: function(func, delay) {"
          , "    var timeout;"
          , "    return function() {"
          , "      clearTimeout(timeout);"
          , "      timeout = setTimeout(func, delay);"
          , "    };"
          , "  },"
          , "  throttle: function(func, limit) {"
          , "    var inThrottle;"
          , "    return function() {"
          , "      if (!inThrottle) {"
          , "        func.apply(this, arguments);"
          , "        inThrottle = true;"
          , "        setTimeout(function() { inThrottle = false; }, limit);"
          , "      }"
          , "    };"
          , "  },"
          , "  unusedUtil: function() {"
          , "    return 'unused';"
          , "  }"
          , "};"
          , ""
          , "var debouncedLog = utils.debounce(function() {"
          , "  console.log('debounced');"
          , "}, 100);"
          , ""
          , "debouncedLog();"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve utils object and used methods
        astShouldContainIdentifier optimized "utils"
        astShouldContainIdentifier optimized "debouncedLog"
        -- Note: Object method analysis is conservative, may preserve more
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test advanced edge case patterns that could cause production issues.
testAdvancedEdgeCasePatterns :: Spec
testAdvancedEdgeCasePatterns = describe "Advanced Edge Case Patterns" $ do
  testDynamicPropertyAccessPatterns
  testEventDrivenCodePatterns
  testPolyfillAndShimPatterns
  testWeakMapPrivateStatePatterns
  testProxyReflectAPIPatterns
  testFrameworkLifecyclePatterns

-- | Test complex dynamic property access patterns.
testDynamicPropertyAccessPatterns :: Spec
testDynamicPropertyAccessPatterns = describe "Dynamic Property Access Patterns" $ do
  it "preserves all methods in objects with dynamic property access" $ do
    let source = unlines
          [ "var handlers = {"
          , "  method1: function() { return 'handler1'; },"
          , "  method2: function() { return 'handler2'; },"
          , "  unusedMethod: function() { return 'unused'; }"
          , "};"
          , ""
          , "var methodName = 'method' + (Math.random() > 0.5 ? '1' : '2');"
          , "var result = handlers[methodName]();"
          , "console.log(result);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve handlers object and all methods due to dynamic access
        astShouldContainIdentifier optimized "handlers"
        astShouldContainIdentifier optimized "method1"
        astShouldContainIdentifier optimized "method2"
        -- In conservative mode, unusedMethod might be preserved due to dynamic access
        astShouldContainIdentifier optimized "methodName"
        astShouldContainIdentifier optimized "result"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles computed property access with complex expressions" $ do
    let source = unlines
          [ "var api = {"
          , "  getUser: function(id) { return 'user' + id; },"
          , "  deleteUser: function(id) { return 'deleted' + id; },"
          , "  updateUser: function(id) { return 'updated' + id; },"
          , "  unusedMethod: function() { return 'unused'; }"
          , "};"
          , ""
          , "var action = 'get';"
          , "var entity = 'User';"
          , "var method = action + entity;"
          , "var result = api[method](123);"
          , "console.log(result);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve api object and methods due to computed access
        astShouldContainIdentifier optimized "api"
        astShouldContainIdentifier optimized "getUser"
        astShouldContainIdentifier optimized "action"
        astShouldContainIdentifier optimized "entity"
        astShouldContainIdentifier optimized "method"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves methods accessed via bracket notation with variables" $ do
    let source = unlines
          [ "var config = {"
          , "  development: { debug: true },"
          , "  production: { debug: false },"
          , "  test: { debug: true }"
          , "};"
          , ""
          , "var env = process.env.NODE_ENV || 'development';"
          , "var settings = config[env];"
          , "console.log(settings.debug);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve config object and all environment configs
        astShouldContainIdentifier optimized "config"
        astShouldContainIdentifier optimized "development"
        astShouldContainIdentifier optimized "production"
        astShouldContainIdentifier optimized "test"
        astShouldContainIdentifier optimized "env"
        astShouldContainIdentifier optimized "settings"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test event-driven code preservation patterns.
testEventDrivenCodePatterns :: Spec
testEventDrivenCodePatterns = describe "Event-Driven Code Patterns" $ do
  it "preserves event handler functions even when they appear unused" $ do
    let source = unlines
          [ "function handleClick() {"
          , "  console.log('clicked');"
          , "}"
          , "function handleSubmit() {"
          , "  console.log('submitted');"
          , "}"
          , "function unusedHandler() {"
          , "  console.log('never used');"
          , "}"
          , ""
          , "element.addEventListener('click', handleClick);"
          , "form.addEventListener('submit', handleSubmit);"
          , "console.log('Event listeners registered');"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve event handlers due to addEventListener calls
        astShouldContainIdentifier optimized "handleClick"
        astShouldContainIdentifier optimized "handleSubmit"
        -- Should eliminate truly unused handler
        astShouldNotContainIdentifier optimized "unusedHandler"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves callback functions registered with APIs" $ do
    let source = unlines
          [ "function onReady() {"
          , "  console.log('app ready');"
          , "}"
          , "function onError(error) {"
          , "  console.log('error:', error);"
          , "}"
          , "function unusedCallback() {"
          , "  console.log('unused');"
          , "}"
          , ""
          , "api.onReady(onReady);"
          , "api.onError(onError);"
          , "api.init();"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve callback functions passed to API
        astShouldContainIdentifier optimized "onReady"
        astShouldContainIdentifier optimized "onError"
        -- Should eliminate unused callback
        astShouldNotContainIdentifier optimized "unusedCallback"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test polyfill and shim preservation patterns.
testPolyfillAndShimPatterns :: Spec
testPolyfillAndShimPatterns = describe "Polyfill and Shim Patterns" $ do
  it "preserves polyfills that modify global prototypes" $ do
    let source = unlines
          [ "// Polyfill for Array.includes"
          , "if (!Array.prototype.includes) {"
          , "  Array.prototype.includes = function(searchElement) {"
          , "    return this.indexOf(searchElement) !== -1;"
          , "  };"
          , "}"
          , ""
          , "// Usage of polyfilled method"
          , "var arr = [1, 2, 3];"
          , "var hasTwo = arr.includes(2);"
          , "console.log(hasTwo);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve polyfill and usage
        astShouldContainIdentifier optimized "includes"
        astShouldContainIdentifier optimized "searchElement"
        astShouldContainIdentifier optimized "arr"
        astShouldContainIdentifier optimized "hasTwo"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves side-effect imports that don't export anything" $ do
    let source = unlines
          [ "// Side effect import simulation"
          , "var coreJsStable = function() {"
          , "  // Patches global objects"
          , "  if (!Object.assign) {"
          , "    Object.assign = function() { /* polyfill */ };"
          , "  }"
          , "};"
          , ""
          , "// Execute side effect"
          , "coreJsStable();"
          , ""
          , "// Use polyfilled functionality"
          , "var merged = Object.assign({}, {a: 1}, {b: 2});"
          , "console.log(merged);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve side effect function and usage
        astShouldContainIdentifier optimized "coreJsStable"
        astShouldContainIdentifier optimized "assign"
        astShouldContainIdentifier optimized "merged"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test WeakMap/WeakSet private state patterns.
testWeakMapPrivateStatePatterns :: Spec
testWeakMapPrivateStatePatterns = describe "WeakMap/WeakSet Private State Patterns" $ do
  it "preserves WeakMap-based private fields" $ do
    let source = unlines
          [ "var privateData = new WeakMap();"
          , ""
          , "function MyClass(value) {"
          , "  privateData.set(this, {"
          , "    secret: value,"
          , "    helper: function() { return 'helper'; }"
          , "  });"
          , "}"
          , ""
          , "MyClass.prototype.getSecret = function() {"
          , "  return privateData.get(this).secret;"
          , "};"
          , ""
          , "MyClass.prototype.callHelper = function() {"
          , "  return privateData.get(this).helper();"
          , "};"
          , ""
          , "var instance = new MyClass('secret value');"
          , "console.log(instance.getSecret());"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve WeakMap and private state access
        astShouldContainIdentifier optimized "privateData"
        astShouldContainIdentifier optimized "MyClass"
        astShouldContainIdentifier optimized "secret"
        astShouldContainIdentifier optimized "helper"
        astShouldContainIdentifier optimized "getSecret"
        astShouldContainIdentifier optimized "instance"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves WeakSet membership patterns" $ do
    let source = unlines
          [ "var friends = new WeakSet();"
          , "var enemies = new WeakSet();"
          , ""
          , "function Person(name) {"
          , "  this.name = name;"
          , "}"
          , ""
          , "function addFriend(person) {"
          , "  friends.add(person);"
          , "}"
          , ""
          , "function isFriend(person) {"
          , "  return friends.has(person);"
          , "}"
          , ""
          , "var alice = new Person('Alice');"
          , "addFriend(alice);"
          , "console.log(isFriend(alice));"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve WeakSet and membership functions
        astShouldContainIdentifier optimized "friends"
        astShouldContainIdentifier optimized "Person"
        astShouldContainIdentifier optimized "addFriend"
        astShouldContainIdentifier optimized "isFriend"
        astShouldContainIdentifier optimized "alice"
        -- Should eliminate unused WeakSet
        astShouldNotContainIdentifier optimized "enemies"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test Proxy/Reflect API interaction patterns.
testProxyReflectAPIPatterns :: Spec
testProxyReflectAPIPatterns = describe "Proxy/Reflect API Patterns" $ do
  it "preserves proxy trap handlers" $ do
    let source = unlines
          [ "var target = {"
          , "  value: 42,"
          , "  hiddenValue: 100"
          , "};"
          , ""
          , "var handler = {"
          , "  get: function(obj, prop) {"
          , "    if (prop === 'value') {"
          , "      return obj[prop];"
          , "    }"
          , "    return undefined;"
          , "  },"
          , "  unusedTrap: function() {"
          , "    return 'unused';"
          , "  }"
          , "};"
          , ""
          , "var proxy = new Proxy(target, handler);"
          , "console.log(proxy.value);"
          , "console.log(proxy.hiddenValue);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve proxy components
        astShouldContainIdentifier optimized "target"
        astShouldContainIdentifier optimized "handler"
        astShouldContainIdentifier optimized "get"
        astShouldContainIdentifier optimized "proxy"
        -- May preserve all trap handlers due to dynamic nature
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves Reflect API usage patterns" $ do
    let source = unlines
          [ "var metaObject = {"
          , "  getValue: function() { return this.value; },"
          , "  setValue: function(val) { this.value = val; },"
          , "  unusedMethod: function() { return 'unused'; }"
          , "};"
          , ""
          , "var obj = { value: 10 };"
          , ""
          , "// Dynamic method invocation using Reflect"
          , "var methodName = 'getValue';"
          , "var result = Reflect.apply(metaObject[methodName], obj, []);"
          , "console.log(result);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve Reflect.apply usage and target methods
        astShouldContainIdentifier optimized "metaObject"
        astShouldContainIdentifier optimized "getValue"
        astShouldContainIdentifier optimized "obj"
        astShouldContainIdentifier optimized "methodName"
        astShouldContainIdentifier optimized "result"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test framework-specific lifecycle method patterns.
testFrameworkLifecyclePatterns :: Spec
testFrameworkLifecyclePatterns = describe "Framework Lifecycle Patterns" $ do
  it "preserves React-like lifecycle methods" $ do
    let source = unlines
          [ "function Component() {"
          , "  this.componentDidMount = function() {"
          , "    console.log('mounted');"
          , "  };"
          , "  this.componentWillUnmount = function() {"
          , "    console.log('unmounting');"
          , "  };"
          , "  this.unusedLifecycle = function() {"
          , "    console.log('unused');"
          , "  };"
          , "  this.render = function() {"
          , "    return 'rendered';"
          , "  };"
          , "}"
          , ""
          , "var instance = new Component();"
          , "// Lifecycle methods called by framework"
          , "instance.componentDidMount();"
          , "console.log(instance.render());"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve explicitly called lifecycle methods
        astShouldContainIdentifier optimized "Component"
        astShouldContainIdentifier optimized "componentDidMount"
        astShouldContainIdentifier optimized "render"
        astShouldContainIdentifier optimized "instance"
        -- componentWillUnmount is not called, might be eliminated
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves Vue-like computed property patterns" $ do
    let source = unlines
          [ "var component = {"
          , "  data: {"
          , "    firstName: 'John',"
          , "    lastName: 'Doe',"
          , "    unused: 'unused'"
          , "  },"
          , "  computed: {"
          , "    fullName: function() {"
          , "      return this.data.firstName + ' ' + this.data.lastName;"
          , "    },"
          , "    unusedComputed: function() {"
          , "      return 'unused';"
          , "    }"
          , "  }"
          , "};"
          , ""
          , "// Framework would call computed properties"
          , "var name = component.computed.fullName.call(component);"
          , "console.log(name);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Should preserve used computed properties
        astShouldContainIdentifier optimized "component"
        astShouldContainIdentifier optimized "data"
        astShouldContainIdentifier optimized "firstName"
        astShouldContainIdentifier optimized "lastName"
        astShouldContainIdentifier optimized "computed"
        astShouldContainIdentifier optimized "fullName"
        astShouldContainIdentifier optimized "name"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- Helper functions (reuse from Core.hs)

-- | Check if AST contains specific identifier in its structure.
astShouldContainIdentifier :: JSAST -> ByteString -> Expectation
astShouldContainIdentifier ast identifier =
  if astContainsIdentifier ast identifier
  then pure ()
  else expectationFailure $ "Identifier not found in AST: " ++ BS8.unpack identifier

-- | Check if AST does not contain specific identifier in its structure.
astShouldNotContainIdentifier :: JSAST -> ByteString -> Expectation
astShouldNotContainIdentifier ast identifier =
  if astContainsIdentifier ast identifier
  then expectationFailure $ "Identifier should not be in AST: " ++ BS8.unpack identifier
  else pure ()

-- | Check if AST contains specific identifier anywhere in its structure.
astContainsIdentifier :: JSAST -> ByteString -> Bool
astContainsIdentifier ast identifier = case ast of
  JSAstProgram statements _ ->
    any (statementContainsIdentifier identifier) statements
  JSAstModule items _ ->
    any (moduleItemContainsIdentifier identifier) items
  JSAstStatement stmt _ ->
    statementContainsIdentifier identifier stmt
  JSAstExpression expr _ ->
    expressionContainsIdentifier identifier expr
  JSAstLiteral expr _ ->
    expressionContainsIdentifier identifier expr

-- | Check if statement contains identifier.
statementContainsIdentifier :: ByteString -> JSStatement -> Bool
statementContainsIdentifier identifier stmt = case stmt of
  JSFunction _ ident _ _ _ body _ ->
    identifierMatches identifier ident || blockContainsIdentifier identifier body
  JSAsyncFunction _ _ ident _ _ _ body _ ->
    identifierMatches identifier ident || blockContainsIdentifier identifier body
  JSGenerator _ _ ident _ _ _ body _ ->
    identifierMatches identifier ident || blockContainsIdentifier identifier body
  JSVariable _ decls _ ->
    any (expressionContainsIdentifier identifier) (fromCommaList decls)
  JSLet _ decls _ ->
    any (expressionContainsIdentifier identifier) (fromCommaList decls)
  JSConstant _ decls _ ->
    any (expressionContainsIdentifier identifier) (fromCommaList decls)
  JSClass _ ident _ _ _ _ _ ->
    identifierMatches identifier ident
  JSExpressionStatement expr _ ->
    expressionContainsIdentifier identifier expr
  JSAssignStatement lhs _ rhs _ ->
    expressionContainsIdentifier identifier lhs ||
    expressionContainsIdentifier identifier rhs
  JSStatementBlock _ stmts _ _ ->
    any (statementContainsIdentifier identifier) stmts
  JSReturn _ (Just expr) _ ->
    expressionContainsIdentifier identifier expr
  JSIf _ _ test _ thenStmt ->
    expressionContainsIdentifier identifier test ||
    statementContainsIdentifier identifier thenStmt
  JSIfElse _ _ test _ thenStmt _ elseStmt ->
    expressionContainsIdentifier identifier test ||
    statementContainsIdentifier identifier thenStmt ||
    statementContainsIdentifier identifier elseStmt
  _ -> False

-- | Check if expression contains identifier.
expressionContainsIdentifier :: ByteString -> JSExpression -> Bool
expressionContainsIdentifier identifier expr = case expr of
  JSIdentifier _ name -> name == identifier
  JSVarInitExpression lhs rhs ->
    expressionContainsIdentifier identifier lhs ||
    case rhs of
      JSVarInit _ rhsExpr -> expressionContainsIdentifier identifier rhsExpr
      JSVarInitNone -> False
  JSCallExpression func _ args _ ->
    expressionContainsIdentifier identifier func ||
    any (expressionContainsIdentifier identifier) (fromCommaList args)
  JSCallExpressionDot func _ prop ->
    expressionContainsIdentifier identifier func ||
    expressionContainsIdentifier identifier prop
  JSCallExpressionSquare func _ prop _ ->
    expressionContainsIdentifier identifier func ||
    expressionContainsIdentifier identifier prop
  JSMemberDot obj _ prop ->
    expressionContainsIdentifier identifier obj ||
    expressionContainsIdentifier identifier prop
  JSMemberSquare obj _ prop _ ->
    expressionContainsIdentifier identifier obj ||
    expressionContainsIdentifier identifier prop
  JSAssignExpression lhs _ rhs ->
    expressionContainsIdentifier identifier lhs ||
    expressionContainsIdentifier identifier rhs
  JSExpressionBinary lhs _ rhs ->
    expressionContainsIdentifier identifier lhs ||
    expressionContainsIdentifier identifier rhs
  JSExpressionParen _ innerExpr _ ->
    expressionContainsIdentifier identifier innerExpr
  JSArrayLiteral _ elements _ ->
    any (arrayElementContainsIdentifier identifier) elements
  JSObjectLiteral _ props _ ->
    objectPropertyListContainsIdentifier identifier props
  JSFunctionExpression _ ident _ params _ body ->
    identifierMatches identifier ident ||
    any (expressionContainsIdentifier identifier) (fromCommaList params) ||
    blockContainsIdentifier identifier body
  _ -> False

-- Helper functions for complex expressions
arrayElementContainsIdentifier :: ByteString -> JSArrayElement -> Bool
arrayElementContainsIdentifier identifier element = case element of
  JSArrayElement expr -> expressionContainsIdentifier identifier expr
  JSArrayComma _ -> False

objectPropertyListContainsIdentifier :: ByteString -> JSObjectPropertyList -> Bool
objectPropertyListContainsIdentifier identifier propList = case propList of
  JSCTLComma props _ -> any (objectPropertyContainsIdentifier identifier) (fromCommaList props)
  JSCTLNone props -> any (objectPropertyContainsIdentifier identifier) (fromCommaList props)

objectPropertyContainsIdentifier :: ByteString -> JSObjectProperty -> Bool
objectPropertyContainsIdentifier identifier prop = case prop of
  JSPropertyNameandValue propName _ values ->
    propertyNameContainsIdentifier identifier propName ||
    any (expressionContainsIdentifier identifier) values
  JSPropertyIdentRef _ name -> name == identifier
  JSObjectMethod (JSMethodDefinition propName _ params _ body) ->
    propertyNameContainsIdentifier identifier propName ||
    any (expressionContainsIdentifier identifier) (fromCommaList params) ||
    blockContainsIdentifier identifier body
  JSObjectMethod (JSGeneratorMethodDefinition _ propName _ params _ body) ->
    propertyNameContainsIdentifier identifier propName ||
    any (expressionContainsIdentifier identifier) (fromCommaList params) ||
    blockContainsIdentifier identifier body
  JSObjectMethod (JSPropertyAccessor _ propName _ params _ body) ->
    propertyNameContainsIdentifier identifier propName ||
    any (expressionContainsIdentifier identifier) (fromCommaList params) ||
    blockContainsIdentifier identifier body
  JSObjectMethod (JSAsyncMethodDefinition _ propName _ params _ body) ->
    propertyNameContainsIdentifier identifier propName ||
    any (expressionContainsIdentifier identifier) (fromCommaList params) ||
    blockContainsIdentifier identifier body
  JSObjectSpread _ expr -> expressionContainsIdentifier identifier expr

-- | Check if property name contains identifier.
propertyNameContainsIdentifier :: ByteString -> JSPropertyName -> Bool
propertyNameContainsIdentifier identifier propName = case propName of
  JSPropertyIdent _ name -> name == identifier
  JSPropertyString _ str -> str == identifier
  JSPropertyNumber _ num -> num == identifier
  JSPropertyComputed _ expr _ -> expressionContainsIdentifier identifier expr

-- | Check if block contains identifier.
blockContainsIdentifier :: ByteString -> JSBlock -> Bool
blockContainsIdentifier identifier (JSBlock _ stmts _) =
  any (statementContainsIdentifier identifier) stmts

-- | Check if module item contains identifier.
moduleItemContainsIdentifier :: ByteString -> JSModuleItem -> Bool
moduleItemContainsIdentifier identifier item = case item of
  JSModuleStatementListItem stmt -> statementContainsIdentifier identifier stmt
  JSModuleImportDeclaration _ importDecl -> importContainsIdentifier identifier importDecl
  JSModuleExportDeclaration _ exportDecl -> exportContainsIdentifier identifier exportDecl

-- | Check if import declaration contains identifier.
importContainsIdentifier :: ByteString -> JSImportDeclaration -> Bool
importContainsIdentifier identifier importDecl = case importDecl of
  JSImportDeclaration importClause _ _ _ ->
    case importClause of
      JSImportClauseDefault ident -> identifierMatches identifier ident
      JSImportClauseNameSpace (JSImportNameSpace _ _ nsIdent) -> identifierMatches identifier nsIdent
      JSImportClauseNamed (JSImportsNamed _ specs _) ->
        any (importSpecContainsIdentifier identifier) (fromCommaList specs)
      JSImportClauseDefaultNameSpace ident _ (JSImportNameSpace _ _ nsIdent) ->
        identifierMatches identifier ident || identifierMatches identifier nsIdent
      JSImportClauseDefaultNamed ident _ (JSImportsNamed _ specs _) ->
        identifierMatches identifier ident ||
        any (importSpecContainsIdentifier identifier) (fromCommaList specs)
  _ -> False

-- | Check if import spec contains identifier.
importSpecContainsIdentifier :: ByteString -> JSImportSpecifier -> Bool
importSpecContainsIdentifier identifier spec = case spec of
  JSImportSpecifier ident -> identifierMatches identifier ident
  JSImportSpecifierAs _ _ localIdent -> identifierMatches identifier localIdent

-- | Check if export declaration contains identifier.
exportContainsIdentifier :: ByteString -> JSExportDeclaration -> Bool
exportContainsIdentifier identifier exportDecl = case exportDecl of
  JSExportFrom exportClause _ _ ->
    exportClauseContainsIdentifier identifier exportClause
  JSExportLocals exportClause _ ->
    exportClauseContainsIdentifier identifier exportClause
  _ -> False

-- | Check if export clause contains identifier.
exportClauseContainsIdentifier :: ByteString -> JSExportClause -> Bool
exportClauseContainsIdentifier identifier exportClause = case exportClause of
  JSExportClause _ specs _ ->
    any (exportSpecContainsIdentifier identifier) (fromCommaList specs)

-- | Check if export spec contains identifier.
exportSpecContainsIdentifier :: ByteString -> JSExportSpecifier -> Bool
exportSpecContainsIdentifier identifier spec = case spec of
  JSExportSpecifier ident -> identifierMatches identifier ident
  JSExportSpecifierAs ident _ _ -> identifierMatches identifier ident

-- | Check if JSIdent matches identifier.
identifierMatches :: ByteString -> JSIdent -> Bool
identifierMatches identifier (JSIdentName _ name) = name == identifier
identifierMatches _ JSIdentNone = False

