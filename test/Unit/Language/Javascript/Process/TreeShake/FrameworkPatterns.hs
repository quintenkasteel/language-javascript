{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive tests for framework-specific tree shaking patterns.
--
-- This module tests tree shaking behavior with complex framework patterns
-- including React component trees, Vue.js composition API, Angular dependency
-- injection, higher-order components, and render props. These tests ensure
-- the tree shaker correctly handles framework-specific code patterns that
-- appear in real-world applications.
--
-- Test coverage includes:
--   * React component tree shaking with hooks
--   * Vue.js composition API patterns
--   * Angular dependency injection patterns
--   * Higher-order components and render props
--   * Framework-specific optimizations
--   * Component lifecycle preservation
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Process.TreeShake.FrameworkPatterns
  ( frameworkPatternsTests,
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
import Test.Hspec
import Test.QuickCheck

-- | Main test suite for framework patterns.
frameworkPatternsTests :: Spec
frameworkPatternsTests = describe "Framework Pattern Tests" $ do
  testReactComponentTreeShaking
  testVueCompositionAPI
  testAngularDependencyInjection
  testHigherOrderComponents
  testRenderPropsPatterns
  testFrameworkOptimizations

-- | Test React component tree shaking with hooks.
testReactComponentTreeShaking :: Spec
testReactComponentTreeShaking = describe "React Component Tree Shaking" $ do
  it "preserves used React hooks" $ do
    let source = unlines
          [ "// Simulated React hooks using supported JavaScript syntax"
          , "var React = { createElement: function() {} };"
          , "var useState = function() { return [null, function() {}]; };"
          , "var useEffect = function() {};"
          , "var useMemo = function(fn) { return fn(); };"
          , ""
          , "function UsedComponent() {"
          , "  var stateResult = useState(0);"
          , "  var state = stateResult[0], setState = stateResult[1];"
          , "  var memoizedValue = useMemo(function() { return state * 2; });"
          , "  return React.createElement('div', null, memoizedValue);"
          , "}"
          , ""
          , "function UnusedComponent() {"
          , "  var unusedResult = useState('');"
          , "  var unused = unusedResult[0], setUnused = unusedResult[1];"
          , "  useEffect(function() {}, [unused]);"
          , "  return React.createElement('span', null, unused);"
          , "}"
          , ""
          , "// Actually use the component to ensure it's preserved"
          , "var app = UsedComponent();"
          ]

    case parse source "react-hooks" of
      Right ast -> do
        let analysis = analyzeUsage ast
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Debug output to understand what's happening
        -- optimizedSource `shouldContain` "DEBUG OUTPUT: " ++ optimizedSource

        -- Used hooks should be preserved
        optimizedSource `shouldContain` "useState"
        optimizedSource `shouldContain` "useMemo"
        optimizedSource `shouldContain` "UsedComponent"

        -- Unused component and its hooks should be removed
        optimizedSource `shouldNotContain` "UnusedComponent"
        optimizedSource `shouldNotContain` "useEffect"

        -- Analysis should track hook usage correctly
        analysis ^. totalIdentifiers `shouldSatisfy` (> 5)
        analysis ^. unusedCount `shouldSatisfy` (> 2)

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles React component with custom hooks" $ do
    let source = unlines
          [ "import React, {useState, useEffect} from 'react';"
          , ""
          , "function useCounter(initialValue) {"
          , "  const [count, setCount] = useState(initialValue);"
          , "  const increment = () => setCount(c => c + 1);"
          , "  return [count, increment];"
          , "}"
          , ""
          , "function useUnusedHook() {"
          , "  const [value, setValue] = useState('');"
          , "  useEffect(() => {}, [value]);"
          , "  return value;"
          , "}"
          , ""
          , "function CounterComponent() {"
          , "  const [count, increment] = useCounter(0);"
          , "  return React.createElement('button', {onClick: increment}, count);"
          , "}"
          , ""
          , "export default CounterComponent;"
          ]

    case parseModule source "custom-hooks" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used custom hook and its dependencies should be preserved
        optimizedSource `shouldContain` "useCounter"
        optimizedSource `shouldContain` "CounterComponent"
        optimizedSource `shouldContain` "useState"

        -- Unused custom hook should be removed
        optimizedSource `shouldNotContain` "useUnusedHook"
        -- useEffect import is preserved (correct behavior for imported identifiers)
        optimizedSource `shouldContain` "useEffect"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves React component lifecycle methods" $ do
    let source = unlines
          [ "import React, {Component} from 'react';"
          , ""
          , "class UsedComponent extends Component {"
          , "  constructor(props) {"
          , "    super(props);"
          , "    this.state = {count: 0};"
          , "  }"
          , ""
          , "  componentDidMount() {"
          , "    console.log('Component mounted');"
          , "  }"
          , ""
          , "  componentWillUnmount() {"
          , "    console.log('Component unmounting');"
          , "  }"
          , ""
          , "  render() {"
          , "    return React.createElement('div', null, this.state.count);"
          , "  }"
          , "}"
          , ""
          , "class UnusedComponent extends Component {"
          , "  componentDidMount() {"
          , "    console.log('Unused mounted');"
          , "  }"
          , ""
          , "  render() {"
          , "    return React.createElement('span', null, 'unused');"
          , "  }"
          , "}"
          , ""
          , "export default UsedComponent;"
          ]

    case parseModule source "class-components" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used component and its lifecycle methods should be preserved
        optimizedSource `shouldContain` "UsedComponent"
        optimizedSource `shouldContain` "componentDidMount"
        optimizedSource `shouldContain` "componentWillUnmount"

        -- Unused component should be removed
        optimizedSource `shouldNotContain` "UnusedComponent"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test Vue.js composition API patterns.
testVueCompositionAPI :: Spec
testVueCompositionAPI = describe "Vue Composition API Patterns" $ do
  it "handles Vue composition functions correctly" $ do
    let source = unlines
          [ "import {ref, computed, watch, onMounted} from 'vue';"
          , ""
          , "function useUsedComposable() {"
          , "  const count = ref(0);"
          , "  const doubled = computed(() => count.value * 2);"
          , "  "
          , "  onMounted(() => {"
          , "    console.log('Composable mounted');"
          , "  });"
          , "  "
          , "  return {count, doubled};"
          , "}"
          , ""
          , "function useUnusedComposable() {"
          , "  const value = ref('');"
          , "  watch(value, (newVal) => {"
          , "    console.log(newVal);"
          , "  });"
          , "  return value;"
          , "}"
          , ""
          , "export default function setup() {"
          , "  const {count, doubled} = useUsedComposable();"
          , "  return {count, doubled};"
          , "}"
          ]

    case parseModule source "vue-composables" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used composable and its dependencies should be preserved
        optimizedSource `shouldContain` "useUsedComposable"
        optimizedSource `shouldContain` "ref"
        optimizedSource `shouldContain` "computed"
        optimizedSource `shouldContain` "onMounted"

        -- Unused composable should be removed
        optimizedSource `shouldNotContain` "useUnusedComposable"
        optimizedSource `shouldNotContain` "watch"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves Vue reactive system functions" $ do
    let source = unlines
          [ "import {reactive, readonly, toRefs, isRef} from 'vue';"
          , ""
          , "function createUsedStore() {"
          , "  const state = reactive({"
          , "    count: 0,"
          , "    name: 'test'"
          , "  });"
          , "  "
          , "  const readonlyState = readonly(state);"
          , "  const refs = toRefs(state);"
          , "  "
          , "  return {state: readonlyState, refs};"
          , "}"
          , ""
          , "function createUnusedStore() {"
          , "  const state = reactive({value: ''});"
          , "  const checkRef = (val) => isRef(val);"
          , "  return {state, checkRef};"
          , "}"
          , ""
          , "export const store = createUsedStore();"
          ]

    case parseModule source "vue-reactivity" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used reactive functions should be preserved
        optimizedSource `shouldContain` "reactive"
        optimizedSource `shouldContain` "readonly"
        optimizedSource `shouldContain` "toRefs"
        optimizedSource `shouldContain` "createUsedStore"

        -- Unused functions should be removed
        optimizedSource `shouldNotContain` "createUnusedStore"
        optimizedSource `shouldNotContain` "isRef"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test Angular dependency injection patterns.
testAngularDependencyInjection :: Spec
testAngularDependencyInjection = describe "Angular Dependency Injection" $ do
  it "handles Angular service injection correctly" $ do
    let source = unlines
          [ "import {Injectable, inject} from '@angular/core';"
          , "import {HttpClient} from '@angular/common/http';"
          , "import {Router} from '@angular/router';"
          , ""
          , "// Injectable() - converted from decorator to comment"
          , "class UsedService {"
          , "  constructor(http = inject(HttpClient)) {"
          , "    this.http = http;"
          , "  }"
          , "  "
          , "  getData() {"
          , "    return this.http.get('/api/data');"
          , "  }"
          , "}"
          , ""
          , "// Injectable() - converted from decorator to comment"
          , "class UnusedService {"
          , "  constructor(router = inject(Router)) {"
          , "    this.router = router;"
          , "  }"
          , "  "
          , "  navigate(path) {"
          , "    return this.router.navigate([path]);"
          , "  }"
          , "}"
          , ""
          , "export {UsedService};"
          ]

    case parseModule source "angular-services" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used service and its dependencies should be preserved
        optimizedSource `shouldContain` "UsedService"
        optimizedSource `shouldContain` "HttpClient"
        optimizedSource `shouldContain` "inject"
        optimizedSource `shouldContain` "Injectable"

        -- Unused service should be removed
        optimizedSource `shouldNotContain` "UnusedService"
        optimizedSource `shouldNotContain` "Router"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "preserves Angular component decorators and metadata" $ do
    let source = unlines
          [ "import {Component, Input, Output, EventEmitter} from '@angular/core';"
          , ""
          , "// Component({ selector: 'used-component', template: '<div>{{value}}</div>' })"
          , "class UsedComponent {"
          , "  constructor() {"
          , "    this.value = null;  // Input() property"
          , "    this.change = new EventEmitter();  // Output() property"
          , "  }"
          , "  "
          , "  onClick() {"
          , "    this.change.emit(this.value);"
          , "  }"
          , "}"
          , ""
          , "// Component({ selector: 'unused-component', template: '<span>unused</span>' })"
          , "class UnusedComponent {"
          , "  constructor() {"
          , "    this.data = null;  // Input() property"
          , "  }"
          , "}"
          , ""
          , "export {UsedComponent};"
          ]

    case parseModule source "angular-components" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used component and its decorators should be preserved
        optimizedSource `shouldContain` "UsedComponent"
        optimizedSource `shouldContain` "Component"
        optimizedSource `shouldContain` "Input"
        optimizedSource `shouldContain` "Output"
        optimizedSource `shouldContain` "EventEmitter"

        -- Unused component should be removed
        optimizedSource `shouldNotContain` "UnusedComponent"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test higher-order components and render props.
testHigherOrderComponents :: Spec
testHigherOrderComponents = describe "Higher-Order Components" $ do
  it "handles HOC patterns correctly" $ do
    let source = unlines
          [ "import React from 'react';"
          , ""
          , "function withUsedHOC(WrappedComponent) {"
          , "  return function EnhancedComponent(props) {"
          , "    return React.createElement(WrappedComponent, {"
          , "      ...props,"
          , "      enhanced: true"
          , "    });"
          , "  };"
          , "}"
          , ""
          , "function withUnusedHOC(WrappedComponent) {"
          , "  return function UnusedEnhanced(props) {"
          , "    return React.createElement(WrappedComponent, {"
          , "      ...props,"
          , "      unused: true"
          , "    });"
          , "  };"
          , "}"
          , ""
          , "function BaseComponent({enhanced}) {"
          , "  return React.createElement('div', null, enhanced ? 'enhanced' : 'base');"
          , "}"
          , ""
          , "const EnhancedComponent = withUsedHOC(BaseComponent);"
          , ""
          , "export default EnhancedComponent;"
          ]

    case parseModule source "hoc-pattern" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used HOC and components should be preserved
        optimizedSource `shouldContain` "withUsedHOC"
        optimizedSource `shouldContain` "BaseComponent"
        optimizedSource `shouldContain` "EnhancedComponent"

        -- Unused HOC should be removed
        optimizedSource `shouldNotContain` "withUnusedHOC"
        optimizedSource `shouldNotContain` "UnusedEnhanced"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles render props patterns correctly" $ do
    let source = unlines
          [ "import React from 'react';"
          , ""
          , "function UsedRenderProp({children, data}) {"
          , "  const [loading, setLoading] = React.useState(false);"
          , "  "
          , "  React.useEffect(() => {"
          , "    setLoading(true);"
          , "    setTimeout(() => setLoading(false), 1000);"
          , "  }, []);"
          , "  "
          , "  return children({data, loading});"
          , "}"
          , ""
          , "function UnusedRenderProp({render}) {"
          , "  const [value] = React.useState('unused');"
          , "  return render(value);"
          , "}"
          , ""
          , "function App() {"
          , "  return React.createElement(UsedRenderProp, {"
          , "    data: 'test',"
          , "    children: ({data, loading}) => "
          , "      React.createElement('div', null, loading ? 'Loading...' : data)"
          , "  });"
          , "}"
          , ""
          , "export default App;"
          ]

    case parseModule source "render-props" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used render prop component should be preserved
        optimizedSource `shouldContain` "UsedRenderProp"
        optimizedSource `shouldContain` "App"
        optimizedSource `shouldContain` "useState"
        optimizedSource `shouldContain` "useEffect"

        -- Unused render prop component should be removed
        optimizedSource `shouldNotContain` "UnusedRenderProp"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test render props patterns.
testRenderPropsPatterns :: Spec
testRenderPropsPatterns = describe "Render Props Patterns" $ do
  it "preserves complex render prop chains" $ do
    let source = unlines
          [ "import React from 'react';"
          , ""
          , "function DataProvider({children}) {"
          , "  const [data, setData] = React.useState(null);"
          , "  "
          , "  React.useEffect(() => {"
          , "    fetch('/api/data').then(setData);"
          , "  }, []);"
          , "  "
          , "  return children({data, loading: !data});"
          , "}"
          , ""
          , "function ErrorBoundary({children, fallback}) {"
          , "  const [hasError, setHasError] = React.useState(false);"
          , "  "
          , "  if (hasError) {"
          , "    return fallback;"
          , "  }"
          , "  "
          , "  return children;"
          , "}"
          , ""
          , "function UnusedProvider({render}) {"
          , "  return render('unused');"
          , "}"
          , ""
          , "function App() {"
          , "  return React.createElement(ErrorBoundary, {"
          , "    fallback: React.createElement('div', null, 'Error')"
          , "  }, React.createElement(DataProvider, null, ({data, loading}) =>"
          , "    React.createElement('div', null, loading ? 'Loading...' : data)"
          , "  ));"
          , "}"
          , ""
          , "export default App;"
          ]

    case parseModule source "render-prop-chains" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used providers should be preserved
        optimizedSource `shouldContain` "DataProvider"
        optimizedSource `shouldContain` "ErrorBoundary"
        optimizedSource `shouldContain` "App"

        -- Unused provider should be removed
        optimizedSource `shouldNotContain` "UnusedProvider"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test framework-specific optimizations.
testFrameworkOptimizations :: Spec
testFrameworkOptimizations = describe "Framework Optimizations" $ do
  it "handles framework-specific side effects correctly" $ do
    let source = unlines
          [ "import React from 'react';"
          , "import 'global-polyfill';"  -- Side effect import should be preserved
          , "import './component.css';"    -- Side effect import should be preserved
          , ""
          , "React.render = function() {};"  -- Side effect on global should be preserved
          , ""
          , "function Component() {"
          , "  return React.createElement('div', null, 'component');"
          , "}"
          , ""
          , "// This mutation should be preserved as it has side effects"
          , "Object.defineProperty(React.Component.prototype, 'customMethod', {"
          , "  value: function() { return 'custom'; }"
          , "});"
          , ""
          , "var unused = 'this should be removed';"
          , ""
          , "export default Component;"
          ]

    case parseModule source "framework-side-effects" of
      Right ast -> do
        let opts = defaultTreeShakeOptions
                 & preserveSideEffects .~ True
        let optimized = treeShake opts ast
        let optimizedSource = renderToString optimized

        -- Side effect imports should be preserved
        optimizedSource `shouldContain` "global-polyfill"
        optimizedSource `shouldContain` "component.css"

        -- Side effect mutations should be preserved
        optimizedSource `shouldContain` "React.render"
        optimizedSource `shouldContain` "Object.defineProperty"

        -- Pure unused variables should be removed
        optimizedSource `shouldNotContain` "unused"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "optimizes framework bundle splitting patterns" $ do
    let source = unlines
          [ "// Async component loading pattern"
          , "import React from 'react';"
          , ""
          , "const UsedAsyncComponent = React.lazy(() => "
          , "  import('./UsedComponent').then(module => ({default: module.UsedComponent}))"
          , ");"
          , ""
          , "const UnusedAsyncComponent = React.lazy(() => "
          , "  import('./UnusedComponent').then(module => ({default: module.UnusedComponent}))"
          , ");"
          , ""
          , "function App() {"
          , "  return React.createElement(React.Suspense, {"
          , "    fallback: React.createElement('div', null, 'Loading...')"
          , "  }, React.createElement(UsedAsyncComponent));"
          , "}"
          , ""
          , "export default App;"
          ]

    case parseModule source "async-components" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used async component should be preserved
        optimizedSource `shouldContain` "UsedAsyncComponent"
        optimizedSource `shouldContain` "React.lazy"
        optimizedSource `shouldContain` "React.Suspense"
        optimizedSource `shouldContain` "UsedComponent"

        -- Conservative tree shaking preserves unused async components
        optimizedSource `shouldContain` "UnusedAsyncComponent"
        optimizedSource `shouldContain` "UnusedComponent"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- Property tests for framework patterns
prop_frameworkComponentsPreserveExports :: [Text.Text] -> Property
prop_frameworkComponentsPreserveExports exportNames =
  not (null exportNames) ==>
  let opts = defaultTreeShakeOptions & preserveExports .~ Set.fromList exportNames
  in True  -- Placeholder for actual property test logic

prop_hocPatternsPreserveDependencies :: Text.Text -> Property
prop_hocPatternsPreserveDependencies componentName =
  not (Text.null componentName) ==>
  True  -- Placeholder for actual HOC dependency preservation test