{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Comprehensive tests for real-world library patterns in tree shaking.
--
-- This module tests tree shaking behavior with popular JavaScript libraries
-- and their specific usage patterns. These tests ensure the tree shaker
-- correctly handles functional programming patterns, observable chains,
-- state management, middleware systems, and polyfill libraries.
--
-- Test coverage includes:
--   * Lodash/Ramda functional programming patterns
--   * RxJS observable chains and operators
--   * Redux state management patterns
--   * Express.js middleware chains
--   * Polyfill library patterns
--   * Utility library tree shaking
--   * Plugin architecture patterns
--
-- @since 0.8.0.0
module Unit.Language.Javascript.Process.TreeShake.LibraryPatterns
  ( libraryPatternsTests,
  )
where

import Language.JavaScript.Parser.Parser (parse, parseModule)
import Language.JavaScript.Pretty.Printer (renderToString)
import Language.JavaScript.Process.TreeShake
import Test.Hspec
import Test.QuickCheck

-- | Main test suite for library patterns.
libraryPatternsTests :: Spec
libraryPatternsTests = describe "Real-World Library Patterns" $ do
  testLodashFunctionalPatterns
  testRamداFunctionalPatterns
  testRxJSObservableChains
  testReduxStateManagement
  testExpressMiddleware
  testPolyfillLibraries
  testUtilityLibraries
  testPluginArchitectures

-- | Test Lodash functional programming patterns.
testLodashFunctionalPatterns :: Spec
testLodashFunctionalPatterns = describe "Lodash Functional Patterns" $ do
  it "handles Lodash chain operations correctly" $ do
    let source = unlines
          [ "import _ from 'lodash';"
          , "import {map, filter, reduce} from 'lodash';"
          , "import {sortBy, uniq, flatten} from 'lodash';  // Some unused"
          , ""
          , "const data = ["
          , "  {name: 'Alice', age: 30, active: true},"
          , "  {name: 'Bob', age: 25, active: false},"
          , "  {name: 'Charlie', age: 35, active: true}"
          , "];"
          , ""
          , "// Used Lodash functions"
          , "const activeUsers = filter(data, 'active');"
          , "const names = map(activeUsers, 'name');"
          , "const summary = reduce(names, (acc, name) => `${acc}, ${name}`, '');"
          , ""
          , "// Chain operation"
          , "const result = _.chain(data)"
          , "  .filter('active')"
          , "  .map('name')"
          , "  .value();"
          , ""
          , "console.log(summary, result);"
          ]

    case parseModule source "lodash-chains" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used Lodash functions should be preserved
        optimizedSource `shouldContain` "filter"
        optimizedSource `shouldContain` "map"
        optimizedSource `shouldContain` "reduce"
        optimizedSource `shouldContain` "_.chain"

        -- Unused functions should be removed
        optimizedSource `shouldNotContain` "sortBy"
        optimizedSource `shouldNotContain` "uniq"
        optimizedSource `shouldNotContain` "flatten"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles Lodash functional composition patterns" $ do
    let source = unlines
          [ "import {flow, compose, curry, partial} from 'lodash';"
          , "import {memoize, debounce, throttle, once} from 'lodash';  // Some unused"
          , ""
          , "// Function composition"
          , "const processData = flow(["
          , "  data => data.filter(x => x > 0),"
          , "  data => data.map(x => x * 2),"
          , "  data => data.reduce((a, b) => a + b, 0)"
          , "]);"
          , ""
          , "// Currying and partial application"
          , "const add = curry((a, b) => a + b);"
          , "const add10 = add(10);"
          , "const multiply = (a, b) => a * b;"
          , "const multiplyBy2 = partial(multiply, 2);"
          , ""
          , "// Memoization for expensive operations"
          , "const expensiveCalculation = memoize((n) => {"
          , "  console.log('Computing for', n);"
          , "  return n * n * n;"
          , "});"
          , ""
          , "// Used functions"
          , "const result = processData([1, -2, 3, -4, 5]);"
          , "const computed = add10(5);"
          , "const doubled = multiplyBy2(21);"
          , "const memoized = expensiveCalculation(10);"
          , ""
          , "console.log(result, computed, doubled, memoized);"
          ]

    case parseModule source "lodash-composition" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used functional programming utilities should be preserved
        optimizedSource `shouldContain` "flow"
        optimizedSource `shouldContain` "curry"
        optimizedSource `shouldContain` "partial"
        optimizedSource `shouldContain` "memoize"

        -- Unused utilities should be removed
        optimizedSource `shouldNotContain` "compose"
        optimizedSource `shouldNotContain` "debounce"
        optimizedSource `shouldNotContain` "throttle"
        optimizedSource `shouldNotContain` "once"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test Ramda functional programming patterns.
testRamداFunctionalPatterns :: Spec
testRamداFunctionalPatterns = describe "Ramda Functional Patterns" $ do
  it "handles Ramda curried function patterns" $ do
    let source = unlines
          [ "import * as R from 'ramda';"
          , ""
          , "const data = [1, 2, 3, 4, 5];"
          , ""
          , "// Used Ramda functions"
          , "const usedPipe = R.pipe("
          , "  R.filter(R.gt(R.__, 2)),"  -- greater than 2
          , "  R.map(R.multiply(2)),"      -- multiply by 2
          , "  R.reduce(R.add, 0)"         -- sum
          , ");"
          , ""
          , "const unusedCompose = R.compose("
          , "  R.join(', '),"
          , "  R.map(R.toString),"
          , "  R.sort(R.subtract)"
          , ");"
          , ""
          , "// Point-free style"
          , "const isEven = R.pipe(R.modulo(R.__, 2), R.equals(0));"
          , "const sumOfEvens = R.pipe("
          , "  R.filter(isEven),"
          , "  R.sum"
          , ");"
          , ""
          , "const result = usedPipe(data);"
          , "const evenSum = sumOfEvens(data);"
          , "console.log(result, evenSum);"
          ]

    case parseModule source "ramda-patterns" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used Ramda functions should be preserved
        optimizedSource `shouldContain` "R.pipe"
        optimizedSource `shouldContain` "R.filter"
        optimizedSource `shouldContain` "R.map"
        optimizedSource `shouldContain` "R.reduce"
        optimizedSource `shouldContain` "R.add"
        optimizedSource `shouldContain` "R.multiply"
        optimizedSource `shouldContain` "R.sum"

        -- Unused functions should be removed
        optimizedSource `shouldNotContain` "unusedCompose"
        optimizedSource `shouldNotContain` "R.join"
        optimizedSource `shouldNotContain` "R.sort"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test RxJS observable chains and operators.
testRxJSObservableChains :: Spec
testRxJSObservableChains = describe "RxJS Observable Chains" $ do
  it "handles RxJS operator chains correctly" $ do
    let source = unlines
          [ "import {Observable, of, from, interval} from 'rxjs';"
          , "import {map, filter, switchMap, mergeMap} from 'rxjs/operators';"
          , "import {debounceTime, distinctUntilChanged, take} from 'rxjs/operators';"
          , "import {catchError, retry, finalize} from 'rxjs/operators';  // Some unused"
          , ""
          , "// Used observable pipeline"
          , "const usedStream$ = of(1, 2, 3, 4, 5).pipe("
          , "  filter(x => x > 2),"
          , "  map(x => x * 2),"
          , "  take(2)"
          , ");"
          , ""
          , "// Search functionality with debouncing"
          , "const searchInput$ = new Observable(subscriber => {"
          , "  const input = document.getElementById('search');"
          , "  input.addEventListener('input', e => subscriber.next(e.target.value));"
          , "});"
          , ""
          , "const searchResults$ = searchInput$.pipe("
          , "  debounceTime(300),"
          , "  distinctUntilChanged(),"
          , "  switchMap(query => from(fetch(`/search?q=${query}`)))"
          , ");"
          , ""
          , "// Unused observable"
          , "const unusedStream$ = interval(1000).pipe("
          , "  mergeMap(() => of('unused')),"
          , "  retry(3),"
          , "  catchError(() => of('error'))"
          , ");"
          , ""
          , "// Subscribe to used streams"
          , "usedStream$.subscribe(console.log);"
          , "searchResults$.subscribe(results => console.log(results));"
          ]

    case parseModule source "rxjs-operators" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used RxJS operators should be preserved
        optimizedSource `shouldContain` "filter"
        optimizedSource `shouldContain` "map"
        optimizedSource `shouldContain` "take"
        optimizedSource `shouldContain` "debounceTime"
        optimizedSource `shouldContain` "distinctUntilChanged"
        optimizedSource `shouldContain` "switchMap"

        -- Unused operators and streams should be removed
        optimizedSource `shouldNotContain` "unusedStream$"
        optimizedSource `shouldNotContain` "mergeMap"
        optimizedSource `shouldNotContain` "retry"
        optimizedSource `shouldNotContain` "catchError"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles RxJS subject and multicasting patterns" $ do
    let source = unlines
          [ "import {Subject, BehaviorSubject, ReplaySubject} from 'rxjs';"
          , "import {share, shareReplay, publish, connect} from 'rxjs/operators';"
          , "import {multicast, refCount} from 'rxjs/operators';  // Some unused"
          , ""
          , "// Used subjects"
          , "const eventBus$ = new Subject();"
          , "const stateSubject$ = new BehaviorSubject({count: 0});"
          , ""
          , "// Unused subject"
          , "const unusedReplay$ = new ReplaySubject(5);"
          , ""
          , "// Shared observable"
          , "const sharedData$ = eventBus$.pipe("
          , "  share(),"
          , "  shareReplay(1)"
          , ");"
          , ""
          , "// Event emitters"
          , "function emitEvent(event) {"
          , "  eventBus$.next(event);"
          , "}"
          , ""
          , "function updateState(newState) {"
          , "  stateSubject$.next(newState);"
          , "}"
          , ""
          , "// Subscriptions"
          , "sharedData$.subscribe(data => console.log('Shared:', data));"
          , "stateSubject$.subscribe(state => console.log('State:', state));"
          , ""
          , "emitEvent({type: 'USER_CLICK'});"
          , "updateState({count: 1});"
          ]

    case parseModule source "rxjs-subjects" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used subjects and operators should be preserved
        optimizedSource `shouldContain` "Subject"
        optimizedSource `shouldContain` "BehaviorSubject"
        optimizedSource `shouldContain` "eventBus$"
        optimizedSource `shouldContain` "stateSubject$"
        optimizedSource `shouldContain` "share"
        optimizedSource `shouldContain` "shareReplay"

        -- Unused subject and operators should be removed
        optimizedSource `shouldNotContain` "ReplaySubject"
        optimizedSource `shouldNotContain` "unusedReplay$"
        optimizedSource `shouldNotContain` "multicast"
        optimizedSource `shouldNotContain` "refCount"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test Redux state management patterns.
testReduxStateManagement :: Spec
testReduxStateManagement = describe "Redux State Management" $ do
  it "handles Redux store and action patterns" $ do
    let source = unlines
          [ "import {createStore, combineReducers, applyMiddleware} from 'redux';"
          , "import {connect} from 'react-redux';"
          , "import thunk from 'redux-thunk';"
          , "import logger from 'redux-logger';  // Unused middleware"
          , ""
          , "// Action creators"
          , "const increment = () => ({type: 'INCREMENT'});"
          , "const decrement = () => ({type: 'DECREMENT'});"
          , "const reset = () => ({type: 'RESET'});  // Unused action"
          , ""
          , "// Async action creator"
          , "const fetchUser = (userId) => (dispatch, getState) => {"
          , "  return fetch(`/api/users/${userId}`)"
          , "    .then(response => response.json())"
          , "    .then(user => dispatch({type: 'SET_USER', payload: user}));"
          , "};"
          , ""
          , "const unusedAsyncAction = () => (dispatch) => {"
          , "  dispatch({type: 'UNUSED'});"
          , "};"
          , ""
          , "// Reducers"
          , "const counterReducer = (state = 0, action) => {"
          , "  switch (action.type) {"
          , "    case 'INCREMENT': return state + 1;"
          , "    case 'DECREMENT': return state - 1;"
          , "    case 'RESET': return 0;"
          , "    default: return state;"
          , "  }"
          , "};"
          , ""
          , "const userReducer = (state = null, action) => {"
          , "  switch (action.type) {"
          , "    case 'SET_USER': return action.payload;"
          , "    default: return state;"
          , "  }"
          , "};"
          , ""
          , "// Root reducer"
          , "const rootReducer = combineReducers({"
          , "  counter: counterReducer,"
          , "  user: userReducer"
          , "});"
          , ""
          , "// Store with thunk middleware"
          , "const store = createStore("
          , "  rootReducer,"
          , "  applyMiddleware(thunk)"
          , ");"
          , ""
          , "// Use the store"
          , "store.dispatch(increment());"
          , "store.dispatch(fetchUser(123));"
          , "console.log(store.getState());"
          ]

    case parseModule source "redux-patterns" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used Redux functions and actions should be preserved
        optimizedSource `shouldContain` "createStore"
        optimizedSource `shouldContain` "combineReducers"
        optimizedSource `shouldContain` "applyMiddleware"
        optimizedSource `shouldContain` "increment"
        optimizedSource `shouldContain` "decrement"
        optimizedSource `shouldContain` "fetchUser"
        optimizedSource `shouldContain` "thunk"

        -- Unused actions and middleware should be removed
        optimizedSource `shouldNotContain` "reset"
        optimizedSource `shouldNotContain` "unusedAsyncAction"
        optimizedSource `shouldNotContain` "logger"

      Left err -> expectationFailure $ "Parse failed: " ++ err

  it "handles Redux selector and reselect patterns" $ do
    let source = unlines
          [ "import {createSelector} from 'reselect';"
          , ""
          , "const getCounter = state => state.counter;"
          , "const getUser = state => state.user;"
          , "const getSettings = state => state.settings;  // Unused selector"
          , ""
          , "// Memoized selectors"
          , "const getCounterDoubled = createSelector("
          , "  [getCounter],"
          , "  counter => counter * 2"
          , ");"
          , ""
          , "const getUserWithCounter = createSelector("
          , "  [getUser, getCounter],"
          , "  (user, counter) => ({...user, visitCount: counter})"
          , ");"
          , ""
          , "const getUnusedData = createSelector("
          , "  [getSettings],"
          , "  settings => settings.theme"
          , ");"
          , ""
          , "// Component using selectors"
          , "function UserDisplay({state}) {"
          , "  const doubled = getCounterDoubled(state);"
          , "  const userWithCount = getUserWithCounter(state);"
          , "  "
          , "  return {"
          , "    doubled,"
          , "    user: userWithCount"
          , "  };"
          , "}"
          , ""
          , "export default UserDisplay;"
          ]

    case parseModule source "redux-selectors" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used selectors should be preserved
        optimizedSource `shouldContain` "getCounter"
        optimizedSource `shouldContain` "getUser"
        optimizedSource `shouldContain` "getCounterDoubled"
        optimizedSource `shouldContain` "getUserWithCounter"
        optimizedSource `shouldContain` "createSelector"

        -- Unused selectors should be removed
        optimizedSource `shouldNotContain` "getSettings"
        optimizedSource `shouldNotContain` "getUnusedData"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test Express.js middleware patterns.
testExpressMiddleware :: Spec
testExpressMiddleware = describe "Express Middleware Patterns" $ do
  it "handles Express middleware chains correctly" $ do
    let source = unlines
          [ "const express = require('express');"
          , "const cors = require('cors');"
          , "const helmet = require('helmet');"
          , "const rateLimit = require('express-rate-limit');"
          , "const compression = require('compression');  // Unused middleware"
          , ""
          , "const app = express();"
          , ""
          , "// Used middleware"
          , "app.use(cors());"
          , "app.use(helmet());"
          , "app.use(express.json());"
          , ""
          , "// Rate limiting middleware"
          , "const limiter = rateLimit({"
          , "  windowMs: 15 * 60 * 1000,"
          , "  max: 100"
          , "});"
          , ""
          , "app.use('/api/', limiter);"
          , ""
          , "// Custom middleware"
          , "const authMiddleware = (req, res, next) => {"
          , "  const token = req.headers.authorization;"
          , "  if (!token) {"
          , "    return res.status(401).json({error: 'No token'});"
          , "  }"
          , "  next();"
          , "};"
          , ""
          , "const unusedMiddleware = (req, res, next) => {"
          , "  req.timestamp = Date.now();"
          , "  next();"
          , "};"
          , ""
          , "// Routes with middleware"
          , "app.get('/api/users', authMiddleware, (req, res) => {"
          , "  res.json([{id: 1, name: 'User'}]);"
          , "});"
          , ""
          , "app.listen(3000, () => {"
          , "  console.log('Server running on port 3000');"
          , "});"
          ]

    case parse source "express-middleware" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used middleware should be preserved
        optimizedSource `shouldContain` "cors"
        optimizedSource `shouldContain` "helmet"
        optimizedSource `shouldContain` "rateLimit"
        optimizedSource `shouldContain` "authMiddleware"

        -- Unused middleware should be removed
        optimizedSource `shouldNotContain` "compression"
        optimizedSource `shouldNotContain` "unusedMiddleware"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test polyfill library patterns.
testPolyfillLibraries :: Spec
testPolyfillLibraries = describe "Polyfill Library Patterns" $ do
  it "handles conditional polyfill loading" $ do
    let source = unlines
          [ "// Feature detection and conditional loading"
          , "if (!Array.prototype.includes) {"
          , "  require('core-js/features/array/includes');"
          , "}"
          , ""
          , "if (!Promise.prototype.finally) {"
          , "  require('core-js/features/promise/finally');"
          , "}"
          , ""
          , "if (!Object.entries) {"
          , "  require('core-js/features/object/entries');"
          , "}"
          , ""
          , "// This polyfill is never needed (always false condition)"
          , "if (false && !String.prototype.padStart) {"
          , "  require('core-js/features/string/pad-start');"
          , "}"
          , ""
          , "// Use the polyfilled features"
          , "const array = [1, 2, 3];"
          , "console.log(array.includes(2));"
          , ""
          , "Promise.resolve('test')"
          , "  .then(val => val.toUpperCase())"
          , "  .finally(() => console.log('Done'));"
          , ""
          , "const obj = {a: 1, b: 2};"
          , "console.log(Object.entries(obj));"
          ]

    case parse source "polyfill-patterns" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Conditional polyfill loads should be preserved (side effects)
        optimizedSource `shouldContain` "core-js/features/array/includes"
        optimizedSource `shouldContain` "core-js/features/promise/finally"
        optimizedSource `shouldContain` "core-js/features/object/entries"

        -- Dead code polyfill should be removed
        optimizedSource `shouldNotContain` "core-js/features/string/pad-start"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test utility library patterns.
testUtilityLibraries :: Spec
testUtilityLibraries = describe "Utility Library Patterns" $ do
  it "handles utility function tree shaking" $ do
    let source = unlines
          [ "import {debounce, throttle, once} from './utils';"
          , "import {formatDate, parseDate, isValidDate} from './date-utils';"
          , "import {validateEmail, validatePhone} from './validators';  // Unused"
          , ""
          , "// Create debounced function"
          , "const debouncedSave = debounce((data) => {"
          , "  console.log('Saving:', data);"
          , "}, 500);"
          , ""
          , "// One-time initialization"
          , "const initApp = once(() => {"
          , "  console.log('App initialized');"
          , "});"
          , ""
          , "// Date utilities"
          , "function displayDate(timestamp) {"
          , "  if (isValidDate(timestamp)) {"
          , "    return formatDate(timestamp);"
          , "  }"
          , "  return 'Invalid date';"
          , "}"
          , ""
          , "// Use the utilities"
          , "debouncedSave({id: 1, name: 'Test'});"
          , "initApp();"
          , "console.log(displayDate(Date.now()));"
          ]

    case parseModule source "utility-patterns" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used utilities should be preserved
        optimizedSource `shouldContain` "debounce"
        optimizedSource `shouldContain` "once"
        optimizedSource `shouldContain` "formatDate"
        optimizedSource `shouldContain` "isValidDate"

        -- Unused utilities should be removed
        optimizedSource `shouldNotContain` "throttle"
        optimizedSource `shouldNotContain` "parseDate"
        optimizedSource `shouldNotContain` "validateEmail"
        optimizedSource `shouldNotContain` "validatePhone"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- | Test plugin architecture patterns.
testPluginArchitectures :: Spec
testPluginArchitectures = describe "Plugin Architecture Patterns" $ do
  it "handles dynamic plugin loading patterns" $ do
    let source = unlines
          [ "class PluginManager {"
          , "  constructor() {"
          , "    this.plugins = new Map();"
          , "    this.hooks = new Map();"
          , "  }"
          , ""
          , "  registerPlugin(name, plugin) {"
          , "    this.plugins.set(name, plugin);"
          , "    if (plugin.hooks) {"
          , "      for (const [hook, handler] of Object.entries(plugin.hooks)) {"
          , "        if (!this.hooks.has(hook)) {"
          , "          this.hooks.set(hook, []);"
          , "        }"
          , "        this.hooks.get(hook).push(handler);"
          , "      }"
          , "    }"
          , "  }"
          , ""
          , "  async executeHook(hookName, ...args) {"
          , "    const handlers = this.hooks.get(hookName) || [];"
          , "    for (const handler of handlers) {"
          , "      await handler(...args);"
          , "    }"
          , "  }"
          , "}"
          , ""
          , "// Used plugins"
          , "const loggerPlugin = {"
          , "  name: 'logger',"
          , "  hooks: {"
          , "    beforeAction: (action) => console.log('Before:', action),"
          , "    afterAction: (action) => console.log('After:', action)"
          , "  }"
          , "};"
          , ""
          , "const metricsPlugin = {"
          , "  name: 'metrics',"
          , "  hooks: {"
          , "    beforeAction: (action) => performance.mark(`${action}-start`),"
          , "    afterAction: (action) => {"
          , "      performance.mark(`${action}-end`);"
          , "      performance.measure(action, `${action}-start`, `${action}-end`);"
          , "    }"
          , "  }"
          , "};"
          , ""
          , "// Unused plugin"
          , "const debugPlugin = {"
          , "  name: 'debug',"
          , "  hooks: {"
          , "    beforeAction: (action) => debugger,"
          , "  }"
          , "};"
          , ""
          , "// Initialize plugin manager"
          , "const manager = new PluginManager();"
          , "manager.registerPlugin('logger', loggerPlugin);"
          , "manager.registerPlugin('metrics', metricsPlugin);"
          , ""
          , "// Use the system"
          , "manager.executeHook('beforeAction', 'user-login');"
          ]

    case parse source "plugin-architecture" of
      Right ast -> do
        let optimized = treeShake defaultOptions ast
        let optimizedSource = renderToString optimized

        -- Used plugin system components should be preserved
        optimizedSource `shouldContain` "PluginManager"
        optimizedSource `shouldContain` "loggerPlugin"
        optimizedSource `shouldContain` "metricsPlugin"
        optimizedSource `shouldContain` "registerPlugin"
        optimizedSource `shouldContain` "executeHook"

        -- Unused plugin should be removed
        optimizedSource `shouldNotContain` "debugPlugin"

      Left err -> expectationFailure $ "Parse failed: " ++ err

-- Property tests for library patterns
prop_lodashChainPreservesDependencies :: [String] -> Property
prop_lodashChainPreservesDependencies operations =
  not (null operations) ==>
  True  -- Placeholder for Lodash chain dependency preservation test

prop_rxjsOperatorChainOptimization :: [String] -> Property
prop_rxjsOperatorChainOptimization operators =
  not (null operators) ==>
  True  -- Placeholder for RxJS operator chain optimization test