-- Understanding JavaScript constructor side effects

{-
Constructors that have side effects when called:
1. Date() - modifies global state (not really, but implementation dependent)
2. Promise() - starts async operations
3. XMLHttpRequest() - can have side effects
4. WebSocket() - network connection
5. Worker() - creates threads
6. Custom constructors - unknown side effects

Constructors that are generally safe to eliminate when unused:
1. Array() - just creates array
2. Object() - just creates object
3. Map() - just creates map
4. Set() - just creates set
5. WeakMap() - just creates weak map
6. WeakSet() - just creates weak set
7. RegExp() - just creates regex
8. String() - just creates string
9. Number() - just creates number
10. Boolean() - just creates boolean

The current logic seems inverted - it treats "safe" constructors as eliminable
and "unsafe" constructors as having side effects.

But the real issue might be that MOST constructors should be eliminable,
and only a few specific ones should be considered to have side effects.
-}