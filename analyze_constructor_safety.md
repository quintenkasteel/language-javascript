# Constructor Safety Analysis

## Safe Constructors (No Observable Side Effects When Unused)

These constructors only create objects and don't have observable side effects:

### Currently in safe list:
- `Array()` - Creates array
- `Object()` - Creates object
- `Map()` - Creates map
- `Set()` - Creates set
- `WeakMap()` - Creates weak map

### Added by fix:
- `WeakSet()` - Creates weak set (identical to WeakMap behavior)

### Could be added in future:
- `RegExp()` - Creates regex object
- `String()` - Creates string wrapper (when used as constructor)
- `Number()` - Creates number wrapper (when used as constructor)
- `Boolean()` - Creates boolean wrapper (when used as constructor)
- `Date()` - Creates date object (mostly safe, some edge cases)

## Unsafe Constructors (Have Observable Side Effects)

These should NOT be eliminated even when unused:

- `Promise()` - Executes function immediately
- `XMLHttpRequest()` - May trigger network activity
- `WebSocket()` - Establishes network connection
- `Worker()` - Creates worker thread
- `SharedArrayBuffer()` - May have memory effects
- Custom constructors - Unknown behavior

## Conclusion

The WeakSet fix is correct and minimal. WeakSet behaves identically to WeakMap and should be treated the same way. The fix properly addresses the inconsistency without over-engineering.