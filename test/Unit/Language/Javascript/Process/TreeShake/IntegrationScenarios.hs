{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive tests for complex integration scenarios in tree shaking.
--
-- This module tests tree shaking behavior with complex real-world integration
-- patterns that challenge the analysis and elimination phases. These scenarios
-- test the limits of static analysis and ensure correct handling of dynamic
-- and interdependent code structures.
--
-- Test coverage includes:
--   * Circular module dependencies with tree shaking
--   * Re-export barrel files with selective imports
--   * Conditional imports based on environment
--   * Side-effect imports with complex initialization
--   * Namespace collision handling
--   * Cross-module dependency cycles
--   * Dynamic module resolution patterns
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Process.TreeShake.IntegrationScenarios
  ( integrationScenariosTests,
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
  ( _dynamicAccessObjects, _moduleDependencies, defaultTreeShakeOptions, TreeShakeOptions
  , preserveSideEffects, preserveSideEffectImports )
import Test.Hspec
import Test.QuickCheck

-- | Main test suite for complex integration scenarios.
integrationScenariosTests :: Spec
integrationScenariosTests = describe "Complex Integration Scenarios" $ do
  testCircularModuleDependencies
  testBarrelFilePatterns
  testConditionalEnvironmentImports
  testSideEffectImports
  testNamespaceCollisions
  testCrosModuleCycles
  testDynamicModuleResolution
  testComplexReexports

-- | Test circular module dependencies.
testCircularModuleDependencies :: Spec
testCircularModuleDependencies = describe "Circular Module Dependencies" $ do
  it "handles direct circular dependencies correctly" $ do
    let moduleA = unlines
          [ "import {functionB, unusedB} from './moduleB';"
          , ""
          , "export function functionA() {"
          , "  console.log('Function A');"
          , "  return functionB();"
          , "}"
          , ""
          , "export function unusedA() {"
          , "  return 'unused from A';"
          , "}"
          , ""
          , "export const configA = {name: 'moduleA'};"
          ]

    let moduleB = unlines
          [ "import {functionA, configA} from './moduleA';"
          , ""
          , "export function functionB() {"
          , "  console.log('Function B with', configA.name);"
          , "  return 'result';"
          , "}"
          , ""
          , "export function unusedB() {"
          , "  return functionA();"  -- Creates cycle but is unused
          , "}"
          , ""
          , "export const configB = {name: 'moduleB'};"
          ]

    let entryPoint = unlines
          [ "import {functionA} from './moduleA';"
          , ""
          , "console.log(functionA());"
          ]

    case parseModule entryPoint "entry" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used functions in cycle should be preserved
        optimizedSource `shouldContain` "functionA"
        optimizedSource `shouldContain` "functionB"
        optimizedSource `shouldContain` "configA"

        -- Unused functions should be removed despite being in cycle
        optimizedSource `shouldNotContain` "unusedA"
        optimizedSource `shouldNotContain` "unusedB"
        optimizedSource `shouldNotContain` "configB"

        -- Analysis should detect circular dependencies
        _moduleDependencies analysis `shouldSatisfy` (not . null)

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles transitive circular dependencies" $ do
    let moduleA = unlines
          [ "import {funcC} from './moduleC';"
          , ""
          , "export function funcA() {"
          , "  return 'A: ' + funcC();"
          , "}"
          , ""
          , "export function unusedFuncA() {"
          , "  return 'unused A';"
          , "}"
          ]

    let moduleB = unlines
          [ "import {funcA} from './moduleA';"
          , ""
          , "export function funcB() {"
          , "  return 'B: ' + funcA();"
          , "}"
          , ""
          , "export function unusedFuncB() {"
          , "  return 'unused B';"
          , "}"
          ]

    let moduleC = unlines
          [ "import {funcB} from './moduleB';"
          , ""
          , "export function funcC() {"
          , "  return 'C';"
          , "}"
          , ""
          , "export function cyclicFuncC() {"
          , "  return 'Cyclic: ' + funcB();"
          , "}"
          ]

    let entry = unlines
          [ "import {funcA} from './moduleA';"
          , ""
          , "console.log(funcA());"
          ]

    case parseModule entry "entry" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used functions in transitive cycle should be preserved
        optimizedSource `shouldContain` "funcA"
        optimizedSource `shouldContain` "funcC"

        -- Unused functions should be removed
        optimizedSource `shouldNotContain` "unusedFuncA"
        optimizedSource `shouldNotContain` "unusedFuncB"
        optimizedSource `shouldNotContain` "funcB"
        optimizedSource `shouldNotContain` "cyclicFuncC"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test barrel file re-export patterns.
testBarrelFilePatterns :: Spec
testBarrelFilePatterns = describe "Barrel File Patterns" $ do
  it "handles selective imports from barrel files" $ do
    let barrelIndex = unlines
          [ "// Barrel file re-exports"
          , "export {ComponentA, ComponentB} from './components/ComponentA';"
          , "export {ComponentC, ComponentD} from './components/ComponentC';"
          , "export {utilityA, utilityB, utilityC} from './utils/utilities';"
          , "export {CONSTANT_A, CONSTANT_B} from './constants';"
          , ""
          , "// Direct exports"
          , "export const barrelConstant = 'barrel';"
          , "export function barrelFunction() { return 'barrel function'; }"
          ]

    let componentA = unlines
          [ "export function ComponentA() {"
          , "  return 'Component A';"
          , "}"
          , ""
          , "export function ComponentB() {"
          , "  return 'Component B';"
          , "}"
          ]

    let utilities = unlines
          [ "export function utilityA() {"
          , "  return 'Utility A';"
          , "}"
          , ""
          , "export function utilityB() {"
          , "  return utilityA() + ' enhanced';"
          , "}"
          , ""
          , "export function utilityC() {"
          , "  return 'Utility C';"
          , "}"
          ]

    let consumer = unlines
          [ "import {ComponentA, utilityB, CONSTANT_A} from './barrel/index';"
          , ""
          , "function App() {"
          , "  console.log(ComponentA());"
          , "  console.log(utilityB());"
          , "  console.log(CONSTANT_A);"
          , "}"
          , ""
          , "export default App;"
          ]

    case parseModule consumer "consumer" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used imports should be preserved
        optimizedSource `shouldContain` "ComponentA"
        optimizedSource `shouldContain` "utilityB"
        optimizedSource `shouldContain` "utilityA"  -- transitive dependency
        optimizedSource `shouldContain` "CONSTANT_A"

        -- Unused re-exports should be removed
        optimizedSource `shouldNotContain` "ComponentB"
        optimizedSource `shouldNotContain` "ComponentC"
        optimizedSource `shouldNotContain` "ComponentD"
        optimizedSource `shouldNotContain` "utilityC"
        optimizedSource `shouldNotContain` "CONSTANT_B"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles complex re-export chains" $ do
    let level1Barrel = unlines
          [ "export * from './level2/barrel';"
          , "export {specificExport} from './level2/specific';"
          , "export const level1Constant = 'level1';"
          ]

    let level2Barrel = unlines
          [ "export * from './level3/functions';"
          , "export {ClassA, ClassB} from './level3/classes';"
          , "export const level2Constant = 'level2';"
          ]

    let level3Functions = unlines
          [ "export function deepFunction() {"
          , "  return 'deep function';"
          , "}"
          , ""
          , "export function anotherDeepFunction() {"
          , "  return deepFunction() + ' enhanced';"
          , "}"
          , ""
          , "export function unusedDeepFunction() {"
          , "  return 'unused deep';"
          , "}"
          ]

    let consumer = unlines
          [ "import {deepFunction, ClassA, level1Constant} from './level1/barrel';"
          , ""
          , "console.log(deepFunction());"
          , "console.log(new ClassA());"
          , "console.log(level1Constant);"
          ]

    case parseModule consumer "consumer" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used deep imports should be preserved
        optimizedSource `shouldContain` "deepFunction"
        optimizedSource `shouldContain` "ClassA"
        optimizedSource `shouldContain` "level1Constant"

        -- Unused deep functions should be removed
        optimizedSource `shouldNotContain` "unusedDeepFunction"
        optimizedSource `shouldNotContain` "anotherDeepFunction"
        optimizedSource `shouldNotContain` "ClassB"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test conditional imports based on environment.
testConditionalEnvironmentImports :: Spec
testConditionalEnvironmentImports = describe "Conditional Environment Imports" $ do
  it "handles NODE_ENV based conditional imports" $ do
    let source = unlines
          [ "let logger;"
          , "let profiler;"
          , ""
          , "if (process.env.NODE_ENV === 'development') {"
          , "  logger = require('./dev-logger');"
          , "  profiler = require('./dev-profiler');"
          , "} else {"
          , "  logger = require('./prod-logger');"
          , "  // No profiler in production"
          , "}"
          , ""
          , "// Feature flag imports"
          , "if (process.env.FEATURE_NEW_UI === 'true') {"
          , "  const {NewUIComponent} = require('./new-ui');"
          , "  module.exports.NewUIComponent = NewUIComponent;"
          , "}"
          , ""
          , "if (process.env.ENABLE_ANALYTICS === 'true') {"
          , "  const analytics = require('./analytics');"
          , "  module.exports.analytics = analytics;"
          , "}"
          , ""
          , "// This import is never used (always false)"
          , "if (false && process.env.DEBUG_MODE) {"
          , "  require('./debug-utils');"
          , "}"
          , ""
          , "module.exports = {logger};"
          ]

    case parse source "conditional-imports" of
      Right ast -> do
        let opts = defaultTreeShakeOptions & preserveSideEffects .~ True
        let optimized = treeShake opts ast
        let optimizedSource = renderToString optimized

        -- Conditional imports should be preserved (side effects)
        optimizedSource `shouldContain` "dev-logger"
        optimizedSource `shouldContain` "prod-logger"
        optimizedSource `shouldContain` "dev-profiler"
        optimizedSource `shouldContain` "new-ui"
        optimizedSource `shouldContain` "analytics"

        -- Dead code import should be removed
        optimizedSource `shouldNotContain` "debug-utils"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles dynamic environment-based module loading" $ do
    let source = unlines
          [ "async function loadEnvironmentConfig() {"
          , "  const env = process.env.NODE_ENV || 'development';"
          , "  const configModule = await import(`./config/${env}.js`);"
          , "  return configModule.default;"
          , "}"
          , ""
          , "async function loadFeatureModules() {"
          , "  const features = process.env.ENABLED_FEATURES?.split(',') || [];"
          , "  const modulePromises = features.map(feature => "
          , "    import(`./features/${feature}/index.js`)"
          , "  );"
          , "  return Promise.all(modulePromises);"
          , "}"
          , ""
          , "async function loadUnusedModule() {"
          , "  // This is never called"
          , "  return import('./unused-dynamic.js');"
          , "}"
          , ""
          , "// Used dynamic loading"
          , "loadEnvironmentConfig().then(config => {"
          , "  console.log('Loaded config:', config);"
          , "});"
          , ""
          , "loadFeatureModules().then(modules => {"
          , "  console.log('Loaded features:', modules.length);"
          , "});"
          ]

    case parse source "dynamic-env-loading" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used dynamic loading functions should be preserved
        optimizedSource `shouldContain` "loadEnvironmentConfig"
        optimizedSource `shouldContain` "loadFeatureModules"

        -- Unused dynamic loading should be removed
        optimizedSource `shouldNotContain` "loadUnusedModule"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test side-effect imports with complex initialization.
testSideEffectImports :: Spec
testSideEffectImports = describe "Side-Effect Imports" $ do
  it "preserves side-effect imports correctly" $ do
    let source = unlines
          [ "// Polyfill imports (side effects)"
          , "import 'core-js/stable';"
          , "import 'regenerator-runtime/runtime';"
          , ""
          , "// Global configuration (side effects)"
          , "import './global-config';"
          , "import './theme-setup';"
          , ""
          , "// CSS imports (side effects)"
          , "import './styles/main.css';"
          , "import './styles/components.css';"
          , ""
          , "// Unused side effect import in dead code"
          , "if (false) {"
          , "  import('./unused-side-effect');"
          , "}"
          , ""
          , "// Regular imports"
          , "import {usedFunction} from './utils';"
          , "import {unusedFunction} from './unused-utils';"
          , ""
          , "// Use only the used function"
          , "console.log(usedFunction());"
          ]

    case parseModule source "side-effects" of
      Right ast -> do
        let opts = defaultTreeShakeOptions & preserveSideEffectImports .~ True
        let optimized = treeShake opts ast
        let optimizedSource = renderToString optimized

        -- Side effect imports should be preserved
        optimizedSource `shouldContain` "core-js/stable"
        optimizedSource `shouldContain` "regenerator-runtime/runtime"
        optimizedSource `shouldContain` "global-config"
        optimizedSource `shouldContain` "theme-setup"
        optimizedSource `shouldContain` "main.css"
        optimizedSource `shouldContain` "components.css"

        -- Used regular import should be preserved
        optimizedSource `shouldContain` "usedFunction"

        -- Unused regular import should be removed
        optimizedSource `shouldNotContain` "unusedFunction"
        optimizedSource `shouldNotContain` "unused-utils"

        -- Dead code side effect should be removed
        optimizedSource `shouldNotContain` "unused-side-effect"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles complex initialization side effects" $ do
    let source = unlines
          [ "// Global state initialization"
          , "window.AppState = window.AppState || {};"
          , "window.AppState.initialized = false;"
          , ""
          , "// Event listener setup (side effect)"
          , "document.addEventListener('DOMContentLoaded', function() {"
          , "  window.AppState.initialized = true;"
          , "  console.log('App initialized');"
          , "});"
          , ""
          , "// Plugin registration (side effect)"
          , "if (window.PluginManager) {"
          , "  window.PluginManager.register('myPlugin', {"
          , "    name: 'My Plugin',"
          , "    init: function() { console.log('Plugin loaded'); }"
          , "  });"
          , "}"
          , ""
          , "// Unused initialization (dead code)"
          , "if (false) {"
          , "  window.UnusedFeature = {};"
          , "}"
          , ""
          , "// Regular exports"
          , "export function usedUtility() {"
          , "  return window.AppState.initialized;"
          , "}"
          , ""
          , "export function unusedUtility() {"
          , "  return 'unused';"
          , "}"
          ]

    case parseModule source "complex-initialization" of
      Right ast -> do
        let opts = defaultTreeShakeOptions & preserveSideEffects .~ True
        let optimized = treeShake opts ast
        let optimizedSource = renderToString optimized

        -- Side effect statements should be preserved
        optimizedSource `shouldContain` "window.AppState"
        optimizedSource `shouldContain` "addEventListener"
        optimizedSource `shouldContain` "PluginManager.register"

        -- Dead code side effect should be removed
        optimizedSource `shouldNotContain` "UnusedFeature"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test namespace collision handling.
testNamespaceCollisions :: Spec
testNamespaceCollisions = describe "Namespace Collision Handling" $ do
  it "handles namespace collisions correctly" $ do
    let source = unlines
          [ "// Multiple imports with same name from different modules"
          , "import {Component} from 'react';"
          , "import {Component as VueComponent} from 'vue';"
          , "import {Component as AngularComponent} from '@angular/core';"
          , ""
          , "// Local definition with same name"
          , "class Component {"
          , "  render() {"
          , "    return 'Local component';"
          , "  }"
          , "}"
          , ""
          , "// Use different components"
          , "const reactElement = React.createElement(Component, {});"
          , "const localElement = new Component();"
          , ""
          , "// Unused aliased imports"
          , "// VueComponent and AngularComponent are imported but unused"
          , ""
          , "export {Component};"
          ]

    case parseModule source "namespace-collisions" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used components should be preserved
        optimizedSource `shouldContain` "Component"
        optimizedSource `shouldContain` "react"

        -- Unused aliased imports should be removed
        optimizedSource `shouldNotContain` "VueComponent"
        optimizedSource `shouldNotContain` "AngularComponent"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test cross-module dependency cycles.
testCrosModuleCycles :: Spec
testCrosModuleCycles = describe "Cross-Module Dependency Cycles" $ do
  it "handles complex multi-module cycles" $ do
    let moduleA = unlines
          [ "import {funcB} from './moduleB';"
          , "import {funcD} from './moduleD';"
          , ""
          , "export function funcA() {"
          , "  return funcB() + funcD();"
          , "}"
          , ""
          , "export function unusedFuncA() {"
          , "  return 'unused A';"
          , "}"
          ]

    let moduleB = unlines
          [ "import {funcC} from './moduleC';"
          , ""
          , "export function funcB() {"
          , "  return 'B:' + funcC();"
          , "}"
          ]

    let moduleC = unlines
          [ "import {funcA} from './moduleA';"  -- Creates cycle
          , ""
          , "export function funcC() {"
          , "  return 'C';"
          , "}"
          , ""
          , "export function cyclicFuncC() {"
          , "  return funcA();"  -- Uses cycle but is unused
          , "}"
          ]

    let moduleD = unlines
          [ "export function funcD() {"
          , "  return 'D';"
          , "}"
          ]

    let entry = unlines
          [ "import {funcA} from './moduleA';"
          , ""
          , "console.log(funcA());"
          ]

    case parseModule entry "entry" of
      Right ast -> do
        let analysis = analyzeUsageWithOptions defaultOptions ast
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used functions should be preserved even in cycles
        optimizedSource `shouldContain` "funcA"
        optimizedSource `shouldContain` "funcB"
        optimizedSource `shouldContain` "funcC"
        optimizedSource `shouldContain` "funcD"

        -- Unused functions should be removed
        optimizedSource `shouldNotContain` "unusedFuncA"
        optimizedSource `shouldNotContain` "cyclicFuncC"

        -- Analysis should detect the complex dependency structure
        _moduleDependencies analysis `shouldSatisfy` (not . null)

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test dynamic module resolution patterns.
testDynamicModuleResolution :: Spec
testDynamicModuleResolution = describe "Dynamic Module Resolution" $ do
  it "handles runtime module resolution correctly" $ do
    let source = unlines
          [ "const moduleRegistry = {"
          , "  'feature-a': './features/a/index.js',"
          , "  'feature-b': './features/b/index.js',"
          , "  'feature-c': './features/c/index.js'"
          , "};"
          , ""
          , "async function loadModule(name) {"
          , "  if (moduleRegistry[name]) {"
          , "    const module = await import(moduleRegistry[name]);"
          , "    return module.default;"
          , "  }"
          , "  throw new Error(`Module ${name} not found`);"
          , "}"
          , ""
          , "async function loadPluginByConfig(config) {"
          , "  const pluginName = config.plugin;"
          , "  const pluginPath = `./plugins/${pluginName}/plugin.js`;"
          , "  return import(pluginPath);"
          , "}"
          , ""
          , "// Used dynamic loading"
          , "loadModule('feature-a').then(feature => {"
          , "  console.log('Loaded:', feature);"
          , "});"
          , ""
          , "// Configuration-driven loading"
          , "const appConfig = {plugin: 'analytics'};"
          , "loadPluginByConfig(appConfig);"
          ]

    case parse source "dynamic-resolution" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Dynamic access objects should be marked
        "moduleRegistry" `shouldSatisfy` (`Set.member` (_dynamicAccessObjects analysis))

        -- Module registry should be preserved due to dynamic access
        optimizedSource `shouldContain` "moduleRegistry"
        optimizedSource `shouldContain` "loadModule"
        optimizedSource `shouldContain` "loadPluginByConfig"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test complex re-export patterns.
testComplexReexports :: Spec
testComplexReexports = describe "Complex Re-export Patterns" $ do
  it "handles mixed re-export patterns" $ do
    let source = unlines
          [ "// Named re-exports"
          , "export {ComponentA, ComponentB} from './components';"
          , ""
          , "// Namespace re-export"
          , "export * as utils from './utils';"
          , ""
          , "// Default re-export"
          , "export {default as MainComponent} from './main';"
          , ""
          , "// Conditional re-export"
          , "if (process.env.NODE_ENV === 'development') {"
          , "  module.exports.DevTools = require('./dev-tools').default;"
          , "}"
          , ""
          , "// Re-export with renaming"
          , "import {legacyFunction} from './legacy';"
          , "export {legacyFunction as newFunction};"
          , ""
          , "// Unused re-export"
          , "export {UnusedComponent} from './unused';"
          ]

    case parseModule source "complex-reexports" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- All re-exports should be preserved initially (conservative approach)
        -- Unless usage analysis determines they're unused
        optimizedSource `shouldContain` "ComponentA"
        optimizedSource `shouldContain` "utils"
        optimizedSource `shouldContain` "MainComponent"

        -- Conditional re-export should be preserved (side effect)
        optimizedSource `shouldContain` "DevTools"

        -- Renamed re-export should be preserved
        optimizedSource `shouldContain` "legacyFunction"
        optimizedSource `shouldContain` "newFunction"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- Property tests for integration scenarios
prop_circularDependencyPreservesUsage :: [Text.Text] -> Property
prop_circularDependencyPreservesUsage moduleNames =
  not (null moduleNames) ==>
  True  -- Placeholder for circular dependency preservation test

prop_barrelFileSelectiveImport :: [Text.Text] -> Property
prop_barrelFileSelectiveImport exports =
  not (null exports) ==>
  True  -- Placeholder for barrel file selective import test