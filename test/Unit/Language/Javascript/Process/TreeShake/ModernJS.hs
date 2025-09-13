{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive unit tests for modern JavaScript patterns in tree shaking.
--
-- This module tests advanced ES2015+ features including destructuring,
-- spread operators, async/await, classes, template literals, dynamic imports,
-- and other modern JavaScript patterns that require sophisticated analysis.
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Process.TreeShake.ModernJS
  ( testModernJavaScriptPatterns,
  )
where

import Control.Lens ((^.), (.~), (&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Parser (parse)
import Language.JavaScript.Parser.SrcLocation (TokenPosn (..))
import Language.JavaScript.Process.TreeShake
import Test.Hspec

-- | Main test suite for modern JavaScript tree shaking patterns.
testModernJavaScriptPatterns :: Spec
testModernJavaScriptPatterns = describe "Modern JavaScript Tree Shaking" $ do
  testDestructuringPatterns
  testSpreadOperators
  testAsyncAwaitPatterns
  testClassAndPrototypePatterns
  testTemplateLiterals
  testDynamicImports
  testComputedProperties
  testArrowFunctions
  testGeneratorsAndIterators
  testSymbolsAndBigInt
  testModulePatterns
  testComplexRealWorldScenarios

-- | Test destructuring assignment patterns.
testDestructuringPatterns :: Spec
testDestructuringPatterns = describe "Destructuring Patterns" $ do
  it "eliminates unused destructured variables" $ do
    let source = unlines
          [ "const obj = {a: 1, b: 2, c: 3};"
          , "const {a, b, c} = obj;"
          , "console.log(a, b);"  -- c is unused
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "a"
        astShouldContainIdentifier optimized "b"
        astShouldNotContainIdentifier optimized "c"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves nested destructuring dependencies" $ do
    let source = unlines
          [ "const nested = {x: {y: {z: 'value'}}};"
          , "const {x: {y: {z}}} = nested;"
          , "console.log(z);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "nested"
        astShouldContainIdentifier optimized "z"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles array destructuring with unused elements" $ do
    let source = unlines
          [ "const arr = [1, 2, 3, 4];"
          , "const [first, , third] = arr;"  -- second element unused
          , "console.log(first, third);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "arr"
        astShouldContainIdentifier optimized "first"
        astShouldContainIdentifier optimized "third"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "eliminates unused rest parameters" $ do
    let source = unlines
          [ "const arr = [1, 2, 3, 4, 5];"
          , "const [first, second, ...rest] = arr;"
          , "console.log(first, second);"  -- rest is unused
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "first"
        astShouldContainIdentifier optimized "second"
        astShouldNotContainIdentifier optimized "rest"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test spread operator patterns.
testSpreadOperators :: Spec
testSpreadOperators = describe "Spread Operators" $ do
  it "preserves spread in function calls" $ do
    let source = unlines
          [ "const args = [1, 2, 3];"
          , "function sum(a, b, c) { return a + b + c; }"
          , "const result = sum(...args);"
          , "console.log(result);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "args"
        astShouldContainIdentifier optimized "sum"
        astShouldContainIdentifier optimized "result"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "eliminates unused spread operations" $ do
    let source = unlines
          [ "const arr1 = [1, 2];"
          , "const arr2 = [3, 4];"
          , "const combined = [...arr1, ...arr2];"
          , "const unused = [...arr1];"  -- unused spread
          , "console.log(combined);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "combined"
        astShouldNotContainIdentifier optimized "unused"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test async/await patterns.
testAsyncAwaitPatterns :: Spec
testAsyncAwaitPatterns = describe "Async/Await Patterns" $ do
  it "preserves async function dependencies" $ do
    let source = unlines
          [ "async function fetchData() {"
          , "  const response = await fetch('/api/data');"
          , "  return response.json();"
          , "}"
          , "async function processData() {"
          , "  const data = await fetchData();"
          , "  return data.processed;"
          , "}"
          , "processData().then(console.log);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "fetchData"
        astShouldContainIdentifier optimized "processData"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "eliminates unused async functions" $ do
    let source = unlines
          [ "async function used() { return 'used'; }"
          , "async function unused() { return 'unused'; }"
          , "used().then(console.log);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "used"
        astShouldNotContainIdentifier optimized "unused"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles Promise chains correctly" $ do
    let source = unlines
          [ "function step1() { return Promise.resolve(1); }"
          , "function step2(x) { return Promise.resolve(x + 1); }"
          , "function step3(x) { return Promise.resolve(x + 1); }"
          , "function unused() { return Promise.resolve('unused'); }"
          , "step1().then(step2).then(step3).then(console.log);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "step1"
        astShouldContainIdentifier optimized "step2"
        astShouldContainIdentifier optimized "step3"
        astShouldNotContainIdentifier optimized "unused"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test class and prototype patterns.
testClassAndPrototypePatterns :: Spec
testClassAndPrototypePatterns = describe "Class and Prototype Patterns" $ do
  it "preserves class inheritance chains" $ do
    let source = unlines
          [ "class Base {"
          , "  constructor() { this.base = true; }"
          , "  baseMethod() { return 'base'; }"
          , "}"
          , "class Derived extends Base {"
          , "  constructor() { super(); this.derived = true; }"
          , "  derivedMethod() { return 'derived'; }"
          , "}"
          , "const instance = new Derived();"
          , "console.log(instance.baseMethod());"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "Base"
        astShouldContainIdentifier optimized "Derived"
        astShouldContainIdentifier optimized "instance"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "eliminates unused class methods" $ do
    let source = unlines
          [ "class MyClass {"
          , "  usedMethod() { return 'used'; }"
          , "  unusedMethod() { return 'unused'; }"
          , "}"
          , "const obj = new MyClass();"
          , "console.log(obj.usedMethod());"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "MyClass"
        astShouldContainIdentifier optimized "usedMethod"
        astShouldNotContainIdentifier optimized "unusedMethod"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles static methods correctly" $ do
    let source = unlines
          [ "class Utils {"
          , "  static usedStatic() { return 'used'; }"
          , "  static unusedStatic() { return 'unused'; }"
          , "  instanceMethod() { return 'instance'; }"
          , "}"
          , "console.log(Utils.usedStatic());"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "Utils"
        astShouldContainIdentifier optimized "usedStatic"
        astShouldNotContainIdentifier optimized "unusedStatic"
        -- Instance method should also be eliminated if not used
        astShouldNotContainIdentifier optimized "instanceMethod"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test template literal patterns.
testTemplateLiterals :: Spec
testTemplateLiterals = describe "Template Literals" $ do
  it "preserves variables used in template literals" $ do
    let source = unlines
          [ "const name = 'World';"
          , "const greeting = `Hello, ${name}!`;"
          , "const unused = 'unused';"
          , "console.log(greeting);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "name"
        astShouldContainIdentifier optimized "greeting"
        astShouldNotContainIdentifier optimized "unused"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles tagged template literals" $ do
    let source = unlines
          [ "function tag(strings, ...values) {"
          , "  return strings.reduce((result, string, i) => {"
          , "    return result + string + (values[i] || '');"
          , "  }, '');"
          , "}"
          , "const value = 'test';"
          , "const result = tag`Template with ${value}`;"
          , "console.log(result);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "tag"
        astShouldContainIdentifier optimized "value"
        astShouldContainIdentifier optimized "result"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test dynamic import patterns.
testDynamicImports :: Spec
testDynamicImports = describe "Dynamic Imports" $ do
  it "preserves dynamic import expressions" $ do
    let source = unlines
          [ "async function loadModule() {"
          , "  const module = await import('./module.js');"
          , "  return module.default;"
          , "}"
          , "loadModule().then(console.log);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "loadModule"
        -- Dynamic imports should be preserved as they have side effects
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles conditional dynamic imports" $ do
    let source = unlines
          [ "async function conditionalLoad(condition) {"
          , "  if (condition) {"
          , "    const module = await import('./conditional.js');"
          , "    return module.feature();"
          , "  }"
          , "  return null;"
          , "}"
          , "conditionalLoad(true).then(console.log);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "conditionalLoad"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test computed property patterns.
testComputedProperties :: Spec
testComputedProperties = describe "Computed Properties" $ do
  it "preserves variables used in computed properties" $ do
    let source = unlines
          [ "const key = 'dynamicKey';"
          , "const obj = { [key]: 'value' };"
          , "const unused = 'unused';"
          , "console.log(obj);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "key"
        astShouldContainIdentifier optimized "obj"
        astShouldNotContainIdentifier optimized "unused"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles complex computed property expressions" $ do
    let source = unlines
          [ "const prefix = 'get';"
          , "const suffix = 'Value';"
          , "const obj = {"
          , "  [prefix + suffix]: function() { return 'computed'; }"
          , "};"
          , "console.log(obj.getValue());"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "prefix"
        astShouldContainIdentifier optimized "suffix"
        astShouldContainIdentifier optimized "obj"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test arrow function patterns.
testArrowFunctions :: Spec
testArrowFunctions = describe "Arrow Functions" $ do
  it "eliminates unused arrow functions" $ do
    let source = unlines
          [ "const usedArrow = () => 'used';"
          , "const unusedArrow = () => 'unused';"
          , "console.log(usedArrow());"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "usedArrow"
        astShouldNotContainIdentifier optimized "unusedArrow"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves arrow function closures" $ do
    let source = unlines
          [ "function createCounter() {"
          , "  let count = 0;"
          , "  return () => ++count;"
          , "}"
          , "const counter = createCounter();"
          , "console.log(counter());"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "createCounter"
        astShouldContainIdentifier optimized "counter"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test generator and iterator patterns.
testGeneratorsAndIterators :: Spec
testGeneratorsAndIterators = describe "Generators and Iterators" $ do
  it "preserves generator function dependencies" $ do
    let source = unlines
          [ "function* numberGenerator() {"
          , "  let i = 0;"
          , "  while (true) {"
          , "    yield i++;"
          , "  }"
          , "}"
          , "const gen = numberGenerator();"
          , "console.log(gen.next().value);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "numberGenerator"
        astShouldContainIdentifier optimized "gen"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "eliminates unused generators" $ do
    let source = unlines
          [ "function* usedGen() { yield 1; }"
          , "function* unusedGen() { yield 2; }"
          , "const iter = usedGen();"
          , "console.log(iter.next());"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "usedGen"
        astShouldNotContainIdentifier optimized "unusedGen"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test Symbol and BigInt patterns.
testSymbolsAndBigInt :: Spec
testSymbolsAndBigInt = describe "Symbols and BigInt" $ do
  it "handles Symbol usage correctly" $ do
    let source = unlines
          [ "const sym = Symbol('unique');"
          , "const obj = { [sym]: 'value' };"
          , "const unused = Symbol('unused');"
          , "console.log(obj[sym]);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "sym"
        astShouldContainIdentifier optimized "obj"
        astShouldNotContainIdentifier optimized "unused"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles BigInt literals" $ do
    let source = unlines
          [ "const bigNum = 123456789012345678901234567890n;"
          , "const unused = 987654321098765432109876543210n;"
          , "console.log(bigNum);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "bigNum"
        astShouldNotContainIdentifier optimized "unused"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test complex module patterns.
testModulePatterns :: Spec
testModulePatterns = describe "Module Patterns" $ do
  it "handles re-exports correctly" $ do
    let source = unlines
          [ "import { feature } from './feature.js';"
          , "export { feature };"
          , "export const local = 'local';"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        -- Both re-exported and local exports should be preserved
        astShouldContainIdentifier optimized "feature"
        astShouldContainIdentifier optimized "local"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "eliminates unused namespace imports" $ do
    let source = unlines
          [ "import * as utils from './utils.js';"
          , "import * as unused from './unused.js';"
          , "console.log(utils.helper());"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "utils"
        astShouldNotContainIdentifier optimized "unused"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test complex real-world scenarios.
testComplexRealWorldScenarios :: Spec
testComplexRealWorldScenarios = describe "Complex Real-World Scenarios" $ do
  it "handles React-like component patterns" $ do
    let source = unlines
          [ "function useState(initial) { return [initial, () => {}]; }"
          , "function useEffect(fn, deps) { fn(); }"
          , ""
          , "function UsedComponent() {"
          , "  const [state, setState] = useState(0);"
          , "  useEffect(() => { console.log('effect'); }, []);"
          , "  return state;"
          , "}"
          , ""
          , "function UnusedComponent() {"
          , "  return 'unused';"
          , "}"
          , ""
          , "const app = UsedComponent();"
          , "console.log(app);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "UsedComponent"
        astShouldContainIdentifier optimized "useState"
        astShouldContainIdentifier optimized "useEffect"
        astShouldNotContainIdentifier optimized "UnusedComponent"
      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles webpack-style conditional requires" $ do
    let source = unlines
          [ "function loadFeature(name) {"
          , "  if (name === 'feature1') {"
          , "    return require('./feature1.js');"
          , "  } else if (name === 'feature2') {"
          , "    return require('./feature2.js');"
          , "  }"
          , "  return null;"
          , "}"
          , ""
          , "const feature = loadFeature('feature1');"
          , "console.log(feature);"
          ]
    case parse source "test" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        astShouldContainIdentifier optimized "loadFeature"
        astShouldContainIdentifier optimized "feature"
      Left err -> expectationFailure $ "Parse failed: " ++ err

-- Helper functions for test assertions

-- | Check if AST contains identifier.
astShouldContainIdentifier :: JSAST -> String -> Expectation
astShouldContainIdentifier ast identifier =
  hasIdentifierUsage (Text.pack identifier) ast `shouldBe` True

-- | Check if AST does not contain identifier.
astShouldNotContainIdentifier :: JSAST -> String -> Expectation
astShouldNotContainIdentifier ast identifier =
  hasIdentifierUsage (Text.pack identifier) ast `shouldBe` False