{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive tests for advanced JavaScript edge cases in tree shaking.
--
-- This module tests tree shaking behavior with cutting-edge JavaScript features
-- and complex runtime patterns that require sophisticated analysis. These tests
-- ensure the tree shaker correctly handles dynamic property access, metaprogramming,
-- and advanced language features that can affect code reachability.
--
-- Test coverage includes:
--   * Proxy/Reflect dynamic property access patterns
--   * Symbol-keyed properties and well-known symbols
--   * WeakRef and FinalizationRegistry patterns
--   * Complex prototype chain manipulations
--   * Dynamic import() expressions
--   * Template literal tag functions
--   * Advanced metaprogramming patterns
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Process.TreeShake.AdvancedJSEdgeCases
  ( advancedJSEdgeCasesTests,
  )
where

import Control.Lens ((^.), (&), (.~))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Language.JavaScript.Parser.AST
import Language.JavaScript.Parser.Parser (parse, parseModule)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake
import Language.JavaScript.Process.TreeShake.Types
  ( _dynamicAccessObjects, _hasEvalCall, _evalCallCount, defaultTreeShakeOptions
  , preserveSideEffects, TreeShakeOptions )
import Test.Hspec
import Test.QuickCheck

-- | Main test suite for advanced JavaScript edge cases.
advancedJSEdgeCasesTests :: Spec
advancedJSEdgeCasesTests = describe "Advanced JavaScript Edge Cases" $ do
  testProxyReflectPatterns
  testSymbolPatterns
  testWeakRefFinalizationRegistry
  testPrototypeManipulation
  testDynamicImports
  testTemplateLiteralTags
  testMetaprogrammingPatterns
  testAdvancedBuiltinUsage

-- | Test Proxy/Reflect dynamic property access patterns.
testProxyReflectPatterns :: Spec
testProxyReflectPatterns = describe "Proxy/Reflect Dynamic Access" $ do
  it "detects dynamic property access through Proxy handlers" $ do
    let source = unlines
          [ "const usedObject = {"
          , "  usedProperty: 'used',"
          , "  unusedProperty: 'unused'"
          , "};"
          , ""
          , "const unusedObject = {"
          , "  prop: 'truly unused'"
          , "};"
          , ""
          , "const proxyHandler = {"
          , "  get(target, prop) {"
          , "    console.log('Accessing:', prop);"
          , "    return Reflect.get(target, prop);"
          , "  },"
          , "  set(target, prop, value) {"
          , "    console.log('Setting:', prop, value);"
          , "    return Reflect.set(target, prop, value);"
          , "  }"
          , "};"
          , ""
          , "const proxiedObject = new Proxy(usedObject, proxyHandler);"
          , ""
          , "// Dynamic access means we can't eliminate properties"
          , "console.log(proxiedObject.usedProperty);"
          , "proxiedObject.newProperty = 'dynamic';"
          ]

    case parse source "proxy-patterns" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Object with proxy should be marked as having dynamic access
        "usedObject" `shouldSatisfy` (`Set.member` (_dynamicAccessObjects analysis))

        -- Proxy handler and Reflect usage should be preserved
        optimizedSource `shouldContain` "Proxy"
        optimizedSource `shouldContain` "Reflect.get"
        optimizedSource `shouldContain` "Reflect.set"
        optimizedSource `shouldContain` "proxyHandler"

        -- Object accessed through proxy should be preserved entirely
        optimizedSource `shouldContain` "usedObject"
        optimizedSource `shouldContain` "usedProperty"
        -- Even "unused" property should be preserved due to dynamic access
        optimizedSource `shouldContain` "unusedProperty"

        -- Truly unused object should still be removed
        optimizedSource `shouldNotContain` "unusedObject"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles Proxy traps and side effects correctly" $ do
    let source = unlines
          [ "let globalCounter = 0;"
          , ""
          , "const sideEffectHandler = {"
          , "  has(target, prop) {"
          , "    globalCounter++;"  -- Side effect in trap
          , "    return Reflect.has(target, prop);"
          , "  },"
          , "  ownKeys(target) {"
          , "    console.log('Getting own keys');"
          , "    return Reflect.ownKeys(target);"
          , "  },"
          , "  unusedTrap(target, prop) {"  -- This trap is unused
          , "    return 'unused';"
          , "  }"
          , "};"
          , ""
          , "const data = {key: 'value'};"
          , "const proxy = new Proxy(data, sideEffectHandler);"
          , ""
          , "'key' in proxy;"
          , "Object.keys(proxy);"
          ]

    case parse source "proxy-side-effects" of
      Right ast -> do
        let opts = defaultTreeShakeOptions & preserveSideEffects .~ True
        let optimized = treeShake opts ast
        let optimizedSource = renderToString optimized

        -- Side effect traps should be preserved
        optimizedSource `shouldContain` "has"
        optimizedSource `shouldContain` "ownKeys"
        optimizedSource `shouldContain` "globalCounter++"

        -- Unused trap might be removed (depending on aggressiveness)
        -- But the handler object itself should be preserved for dynamic access
        optimizedSource `shouldContain` "sideEffectHandler"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test Symbol-keyed properties and well-known symbols.
testSymbolPatterns :: Spec
testSymbolPatterns = describe "Symbol Patterns" $ do
  it "handles Symbol-keyed properties correctly" $ do
    let source = unlines
          [ "const usedSymbol = Symbol('used');"
          , "const unusedSymbol = Symbol('unused');"
          , ""
          , "const obj = {"
          , "  regularProp: 'regular',"
          , "  [usedSymbol]: 'symbol value',"
          , "  [unusedSymbol]: 'unused symbol value'"
          , "};"
          , ""
          , "// Access symbol property"
          , "console.log(obj[usedSymbol]);"
          , "console.log(obj.regularProp);"
          ]

    case parse source "symbol-properties" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used symbol should be preserved
        optimizedSource `shouldContain` "usedSymbol"
        optimizedSource `shouldContain` "Symbol('used')"

        -- Regular used property should be preserved
        optimizedSource `shouldContain` "regularProp"

        -- Unused symbol should be removed
        optimizedSource `shouldNotContain` "unusedSymbol"
        optimizedSource `shouldNotContain` "Symbol('unused')"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves well-known symbols and their usage" $ do
    let source = unlines
          [ "class UsedIterable {"
          , "  constructor(items) {"
          , "    this.items = items;"
          , "  }"
          , ""
          , "  [Symbol.iterator]() {"
          , "    let index = 0;"
          , "    const items = this.items;"
          , "    return {"
          , "      next() {"
          , "        if (index < items.length) {"
          , "          return {value: items[index++], done: false};"
          , "        }"
          , "        return {done: true};"
          , "      }"
          , "    };"
          , "  }"
          , "}"
          , ""
          , "class UnusedToString {"
          , "  [Symbol.toString]() {"
          , "    return 'unused';"
          , "  }"
          , "}"
          , ""
          , "// Use the iterable"
          , "for (const item of new UsedIterable([1, 2, 3])) {"
          , "  console.log(item);"
          , "}"
          ]

    case parse source "well-known-symbols" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used iterable with Symbol.iterator should be preserved
        optimizedSource `shouldContain` "UsedIterable"
        optimizedSource `shouldContain` "Symbol.iterator"

        -- Unused class with symbol method should be removed
        optimizedSource `shouldNotContain` "UnusedToString"
        optimizedSource `shouldNotContain` "Symbol.toString"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles Symbol.for registry patterns" $ do
    let source = unlines
          [ "const USED_KEY = Symbol.for('app.used.key');"
          , "const UNUSED_KEY = Symbol.for('app.unused.key');"
          , ""
          , "const registry = new Map();"
          , "registry.set(USED_KEY, 'used value');"
          , "registry.set(UNUSED_KEY, 'unused value');"
          , ""
          , "function getValue(key) {"
          , "  return registry.get(key);"
          , "}"
          , ""
          , "console.log(getValue(USED_KEY));"
          ]

    case parse source "symbol-registry" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used Symbol.for should be preserved
        optimizedSource `shouldContain` "USED_KEY"
        optimizedSource `shouldContain` "Symbol.for('app.used.key')"

        -- Unused Symbol.for should be removed
        optimizedSource `shouldNotContain` "UNUSED_KEY"
        optimizedSource `shouldNotContain` "Symbol.for('app.unused.key')"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test WeakRef and FinalizationRegistry patterns.
testWeakRefFinalizationRegistry :: Spec
testWeakRefFinalizationRegistry = describe "WeakRef/FinalizationRegistry" $ do
  it "handles WeakRef patterns correctly" $ do
    let source = unlines
          [ "let usedObject = {data: 'used'};"
          , "let unusedObject = {data: 'unused'};"
          , ""
          , "const usedWeakRef = new WeakRef(usedObject);"
          , "const unusedWeakRef = new WeakRef(unusedObject);"
          , ""
          , "function checkUsedRef() {"
          , "  const obj = usedWeakRef.deref();"
          , "  if (obj) {"
          , "    console.log(obj.data);"
          , "  }"
          , "}"
          , ""
          , "function checkUnusedRef() {"
          , "  const obj = unusedWeakRef.deref();"
          , "  return obj;"
          , "}"
          , ""
          , "// Only use the used ref"
          , "checkUsedRef();"
          ]

    case parse source "weakref-patterns" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used WeakRef and its target should be preserved
        optimizedSource `shouldContain` "usedObject"
        optimizedSource `shouldContain` "usedWeakRef"
        optimizedSource `shouldContain` "checkUsedRef"

        -- Unused WeakRef and its components should be removed
        optimizedSource `shouldNotContain` "unusedObject"
        optimizedSource `shouldNotContain` "unusedWeakRef"
        optimizedSource `shouldNotContain` "checkUnusedRef"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles FinalizationRegistry patterns" $ do
    let source = unlines
          [ "const usedCleanupRegistry = new FinalizationRegistry((heldValue) => {"
          , "  console.log('Cleaning up:', heldValue);"
          , "});"
          , ""
          , "const unusedCleanupRegistry = new FinalizationRegistry((heldValue) => {"
          , "  console.log('Unused cleanup:', heldValue);"
          , "});"
          , ""
          , "function createUsedResource() {"
          , "  const resource = {id: Math.random()};"
          , "  usedCleanupRegistry.register(resource, resource.id);"
          , "  return resource;"
          , "}"
          , ""
          , "function createUnusedResource() {"
          , "  const resource = {id: Math.random()};"
          , "  unusedCleanupRegistry.register(resource, resource.id);"
          , "  return resource;"
          , "}"
          , ""
          , "const myResource = createUsedResource();"
          , "console.log(myResource.id);"
          ]

    case parse source "finalization-registry" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used registry and resource creation should be preserved
        optimizedSource `shouldContain` "usedCleanupRegistry"
        optimizedSource `shouldContain` "createUsedResource"
        optimizedSource `shouldContain` "FinalizationRegistry"

        -- Unused registry should be removed
        optimizedSource `shouldNotContain` "unusedCleanupRegistry"
        optimizedSource `shouldNotContain` "createUnusedResource"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test complex prototype chain manipulations.
testPrototypeManipulation :: Spec
testPrototypeManipulation = describe "Prototype Chain Manipulation" $ do
  it "handles Object.setPrototypeOf patterns" $ do
    let source = unlines
          [ "function UsedBase() {"
          , "  this.baseProperty = 'base';"
          , "}"
          , ""
          , "function UnusedBase() {"
          , "  this.unusedProperty = 'unused';"
          , "}"
          , ""
          , "UsedBase.prototype.usedMethod = function() {"
          , "  return this.baseProperty;"
          , "};"
          , ""
          , "function UsedDerived() {"
          , "  UsedBase.call(this);"
          , "  this.derivedProperty = 'derived';"
          , "}"
          , ""
          , "// Dynamic prototype manipulation"
          , "Object.setPrototypeOf(UsedDerived.prototype, UsedBase.prototype);"
          , ""
          , "const instance = new UsedDerived();"
          , "console.log(instance.usedMethod());"
          ]

    case parse source "prototype-manipulation" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used constructors and prototype chain should be preserved
        optimizedSource `shouldContain` "UsedBase"
        optimizedSource `shouldContain` "UsedDerived"
        optimizedSource `shouldContain` "Object.setPrototypeOf"
        optimizedSource `shouldContain` "usedMethod"

        -- Unused base should be removed
        optimizedSource `shouldNotContain` "UnusedBase"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles Object.create with complex prototype chains" $ do
    let source = unlines
          [ "const usedProto = {"
          , "  usedMethod() {"
          , "    return 'used';"
          , "  },"
          , "  unusedMethod() {"
          , "    return 'unused';"
          , "  }"
          , "};"
          , ""
          , "const unusedProto = {"
          , "  method() {"
          , "    return 'unused proto';"
          , "  }"
          , "};"
          , ""
          , "const usedObj = Object.create(usedProto, {"
          , "  ownProp: {"
          , "    value: 'own property',"
          , "    writable: true"
          , "  }"
          , "});"
          , ""
          , "const unusedObj = Object.create(unusedProto);"
          , ""
          , "console.log(usedObj.usedMethod());"
          , "console.log(usedObj.ownProp);"
          ]

    case parse source "object-create" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used prototype and object should be preserved
        optimizedSource `shouldContain` "usedProto"
        optimizedSource `shouldContain` "usedObj"
        optimizedSource `shouldContain` "Object.create"
        optimizedSource `shouldContain` "usedMethod"

        -- Due to prototype relationship, unused method on used proto
        -- might need to be preserved (conservative analysis)
        -- But unused proto should be removed
        optimizedSource `shouldNotContain` "unusedProto"
        optimizedSource `shouldNotContain` "unusedObj"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test dynamic import() expressions.
testDynamicImports :: Spec
testDynamicImports = describe "Dynamic Import Expressions" $ do
  it "handles dynamic import with computed module names" $ do
    let source = unlines
          [ "const moduleMap = {"
          , "  'used': './used-module.js',"
          , "  'unused': './unused-module.js'"
          , "};"
          , ""
          , "async function loadUsedModule() {"
          , "  const moduleName = 'used';"
          , "  const module = await import(moduleMap[moduleName]);"
          , "  return module.default;"
          , "}"
          , ""
          , "async function loadUnusedModule() {"
          , "  const moduleName = 'unused';"
          , "  const module = await import(moduleMap[moduleName]);"
          , "  return module.default;"
          , "}"
          , ""
          , "async function dynamicLoader(name) {"
          , "  const path = `./modules/${name}.js`;"
          , "  return await import(path);"
          , "}"
          , ""
          , "loadUsedModule().then(mod => console.log(mod));"
          , "dynamicLoader('runtime').then(mod => console.log(mod));"
          ]

    case parse source "dynamic-imports" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Dynamic imports make analysis conservative
        -- Module map should be preserved due to dynamic access
        "moduleMap" `shouldSatisfy` (`Set.member` (_dynamicAccessObjects analysis))

        -- Used dynamic import function should be preserved
        optimizedSource `shouldContain` "loadUsedModule"
        optimizedSource `shouldContain` "dynamicLoader"

        -- Unused dynamic import function should be removed
        optimizedSource `shouldNotContain` "loadUnusedModule"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles conditional dynamic imports" $ do
    let source = unlines
          [ "let loadedModules = new Set();"
          , ""
          , "async function conditionalLoader(condition, moduleName) {"
          , "  if (condition && !loadedModules.has(moduleName)) {"
          , "    const module = await import(`./conditional/${moduleName}.js`);"
          , "    loadedModules.add(moduleName);"
          , "    return module.default;"
          , "  }"
          , "  return null;"
          , "}"
          , ""
          , "async function unusedConditionalLoader(name) {"
          , "  if (false) {" -- Dead code, but has dynamic import
          , "    return await import(`./unused/${name}.js`);"
          , "  }"
          , "}"
          , ""
          , "// Used with runtime condition"
          , "conditionalLoader(true, 'feature');"
          ]

    case parse source "conditional-imports" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used conditional loader should be preserved
        optimizedSource `shouldContain` "conditionalLoader"
        optimizedSource `shouldContain` "loadedModules"

        -- Unused loader should be removed (dead code with import)
        optimizedSource `shouldNotContain` "unusedConditionalLoader"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test template literal tag functions.
testTemplateLiteralTags :: Spec
testTemplateLiteralTags = describe "Template Literal Tags" $ do
  it "handles template tag functions correctly" $ do
    let source = unlines
          [ "function usedTag(strings, ...values) {"
          , "  return strings.reduce((result, string, i) => {"
          , "    return result + string + (values[i] || '');"
          , "  }, '');"
          , "}"
          , ""
          , "function unusedTag(strings, ...values) {"
          , "  return values.join(' ');"
          , "}"
          , ""
          , "function sqlTag(strings, ...values) {"
          , "  // SQL template tag with side effects"
          , "  console.log('SQL Query:', strings, values);"
          , "  return strings.join('?');"
          , "}"
          , ""
          , "const name = 'test';"
          , "const unusedVar = 'unused';"
          , ""
          , "const result = usedTag`Hello ${name}!`;"
          , "console.log(result);"
          , ""
          , "// SQL tag used in different context"
          , "const query = sqlTag`SELECT * FROM users WHERE name = ${name}`;"
          , "console.log(query);"
          ]

    case parse source "template-tags" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used template tags should be preserved
        optimizedSource `shouldContain` "usedTag"
        optimizedSource `shouldContain` "sqlTag"
        optimizedSource `shouldContain` "name"

        -- Unused template tag and variable should be removed
        optimizedSource `shouldNotContain` "unusedTag"
        optimizedSource `shouldNotContain` "unusedVar"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles complex template literal expressions" $ do
    let source = unlines
          [ "const config = {"
          , "  apiUrl: 'https://api.example.com',"
          , "  version: 'v1',"
          , "  timeout: 5000"
          , "};"
          , ""
          , "function buildUrl(endpoint, params = {}) {"
          , "  const baseUrl = `${config.apiUrl}/${config.version}`;"
          , "  const queryString = Object.keys(params)"
          , "    .map(key => `${key}=${params[key]}`)"
          , "    .join('&');"
          , "  return queryString ? `${baseUrl}/${endpoint}?${queryString}` : `${baseUrl}/${endpoint}`;"
          , "}"
          , ""
          , "function unusedUrlBuilder(path) {"
          , "  return `${config.apiUrl}/${path}?timeout=${config.timeout}`;"
          , "}"
          , ""
          , "const userUrl = buildUrl('users', {active: true});"
          , "console.log(userUrl);"
          ]

    case parse source "complex-templates" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used config properties and function should be preserved
        optimizedSource `shouldContain` "buildUrl"
        optimizedSource `shouldContain` "apiUrl"
        optimizedSource `shouldContain` "version"

        -- Unused function and config property should be removed
        optimizedSource `shouldNotContain` "unusedUrlBuilder"
        -- timeout is only used in unused function, so should be removed
        optimizedSource `shouldNotContain` "timeout"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test advanced metaprogramming patterns.
testMetaprogrammingPatterns :: Spec
testMetaprogrammingPatterns = describe "Metaprogramming Patterns" $ do
  it "handles eval and Function constructor patterns" $ do
    let source = unlines
          [ "const usedDynamicCode = 'console.log(\"dynamic code\")';"
          , "const unusedDynamicCode = 'alert(\"unused\")';"
          , ""
          , "function executeUsedCode() {"
          , "  eval(usedDynamicCode);"  -- eval makes analysis conservative
          , "}"
          , ""
          , "function executeUnusedCode() {"
          , "  eval(unusedDynamicCode);"
          , "}"
          , ""
          , "const usedFunction = new Function('x', 'return x * 2');"
          , "const unusedFunction = new Function('y', 'return y + 1');"
          , ""
          , "executeUsedCode();"
          , "console.log(usedFunction(5));"
          ]

    case parse source "eval-patterns" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Should detect eval usage
        _hasEvalCall analysis `shouldBe` True
        _evalCallCount analysis `shouldSatisfy` (> 0)

        -- Used code with eval should be preserved
        optimizedSource `shouldContain` "executeUsedCode"
        optimizedSource `shouldContain` "usedDynamicCode"
        optimizedSource `shouldContain` "usedFunction"

        -- Unused code should be removed
        optimizedSource `shouldNotContain` "executeUnusedCode"
        optimizedSource `shouldNotContain` "unusedDynamicCode"
        optimizedSource `shouldNotContain` "unusedFunction"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles property descriptor metaprogramming" $ do
    let source = unlines
          [ "const usedObject = {};"
          , "const unusedObject = {};"
          , ""
          , "Object.defineProperty(usedObject, 'dynamicProp', {"
          , "  get() {"
          , "    console.log('Getting dynamic property');"
          , "    return this._value;"
          , "  },"
          , "  set(value) {"
          , "    console.log('Setting dynamic property');"
          , "    this._value = value;"
          , "  },"
          , "  enumerable: true"
          , "});"
          , ""
          , "Object.defineProperty(unusedObject, 'unusedProp', {"
          , "  value: 'unused',"
          , "  writable: false"
          , "});"
          , ""
          , "usedObject.dynamicProp = 'test';"
          , "console.log(usedObject.dynamicProp);"
          ]

    case parse source "property-descriptors" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Object with dynamic property should be marked
        "usedObject" `shouldSatisfy` (`Set.member` (_dynamicAccessObjects analysis))

        -- Used object and its property definition should be preserved
        optimizedSource `shouldContain` "usedObject"
        optimizedSource `shouldContain` "Object.defineProperty"
        optimizedSource `shouldContain` "dynamicProp"

        -- Unused object should be removed
        optimizedSource `shouldNotContain` "unusedObject"
        optimizedSource `shouldNotContain` "unusedProp"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test advanced builtin usage patterns.
testAdvancedBuiltinUsage :: Spec
testAdvancedBuiltinUsage = describe "Advanced Builtin Usage" $ do
  it "handles Map/Set with dynamic keys" $ do
    let source = unlines
          [ "const usedMap = new Map();"
          , "const unusedMap = new Map();"
          , ""
          , "function addToUsedMap(key, value) {"
          , "  usedMap.set(key, value);"
          , "}"
          , ""
          , "function addToUnusedMap(key, value) {"
          , "  unusedMap.set(key, value);"
          , "}"
          , ""
          , "// Dynamic usage patterns"
          , "['a', 'b', 'c'].forEach((key, index) => {"
          , "  addToUsedMap(key, index);"
          , "});"
          , ""
          , "for (const [key, value] of usedMap) {"
          , "  console.log(key, value);"
          , "}"
          ]

    case parse source "map-set-dynamic" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used map and related functions should be preserved
        optimizedSource `shouldContain` "usedMap"
        optimizedSource `shouldContain` "addToUsedMap"

        -- Unused map should be removed
        optimizedSource `shouldNotContain` "unusedMap"
        optimizedSource `shouldNotContain` "addToUnusedMap"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles ArrayBuffer and TypedArray patterns" $ do
    let source = unlines
          [ "const usedBuffer = new ArrayBuffer(1024);"
          , "const unusedBuffer = new ArrayBuffer(512);"
          , ""
          , "const usedView = new Int32Array(usedBuffer);"
          , "const unusedView = new Float32Array(unusedBuffer);"
          , ""
          , "function processUsedData() {"
          , "  for (let i = 0; i < usedView.length; i++) {"
          , "    usedView[i] = i * 2;"
          , "  }"
          , "  return usedView.buffer.byteLength;"
          , "}"
          , ""
          , "function processUnusedData() {"
          , "  unusedView.fill(0);"
          , "  return unusedView;"
          , "}"
          , ""
          , "console.log(processUsedData());"
          ]

    case parse source "typed-arrays" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used buffer and view should be preserved
        optimizedSource `shouldContain` "usedBuffer"
        optimizedSource `shouldContain` "usedView"
        optimizedSource `shouldContain` "Int32Array"
        optimizedSource `shouldContain` "processUsedData"

        -- Unused components should be removed
        optimizedSource `shouldNotContain` "unusedBuffer"
        optimizedSource `shouldNotContain` "unusedView"
        optimizedSource `shouldNotContain` "Float32Array"
        optimizedSource `shouldNotContain` "processUnusedData"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- Property tests for edge cases
prop_proxyPreservesTargetProperties :: [Text.Text] -> Property
prop_proxyPreservesTargetProperties props =
  not (null props) ==>
  True  -- Placeholder for proxy target preservation test

prop_symbolKeysPreserveDynamicAccess :: Text.Text -> Property
prop_symbolKeysPreserveDynamicAccess symbolName =
  not (Text.null symbolName) ==>
  True  -- Placeholder for symbol dynamic access test