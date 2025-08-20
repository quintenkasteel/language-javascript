// ES2023+ Advanced Features Test Fixture
// This file contains cutting-edge JavaScript features for comprehensive testing

// Array.prototype.findLast() and findLastIndex() - ES2023
const numbers = [1, 2, 3, 4, 5, 4, 3, 2, 1];
const lastGreaterThanThree = numbers.findLast(x => x > 3);
const lastIndexGreaterThanThree = numbers.findLastIndex(x => x > 3);

// Hashbang comment support - ES2023
#!/usr/bin/env node

// Import attributes (formerly import assertions) - ES2023  
import data from './data.json' with { type: 'json' };
import wasmModule from './module.wasm' with { 
  type: 'webassembly',
  integrity: 'sha384-...' 
};

// Array.prototype.toReversed(), toSorted(), toSpliced(), with() - ES2023
const originalArray = [3, 1, 4, 1, 5];
const reversedCopy = originalArray.toReversed();
const sortedCopy = originalArray.toSorted();
const splicedCopy = originalArray.toSpliced(1, 2, 'new', 'elements');
const modifiedCopy = originalArray.with(2, 'changed');

// Array.prototype.findLastIndex() with complex predicate
const users = [
  { id: 1, name: 'Alice', active: true },
  { id: 2, name: 'Bob', active: false },
  { id: 3, name: 'Charlie', active: true },
  { id: 4, name: 'David', active: false }
];

const lastActiveUserIndex = users.findLastIndex(user => user.active);
const lastInactiveUser = users.findLast(user => !user.active);

// Array groupBy (proposal) - Future ES features
const groupedByStatus = users.groupBy(user => user.active ? 'active' : 'inactive');

// Temporal API (proposal) - Future ES features  
const now = Temporal.Now.instant();
const date = Temporal.PlainDate.from('2023-12-01');
const time = Temporal.PlainTime.from('14:30:00');

// Record and Tuple (proposal) - Future ES features
const record = #{
  name: "John",
  age: 30,
  address: #{
    street: "123 Main St",
    city: "Anytown"
  }
};

const tuple = #[1, 2, 3, "hello", #{ nested: "object" }];

// Pattern matching (proposal) - Future ES features
function processValue(value) {
  return match (value) {
    when Number if (value > 0) => `Positive: ${value}`,
    when Number => `Non-positive: ${value}`,
    when String if (value.length > 5) => `Long string: ${value}`,
    when String => `Short string: ${value}`,
    when Array => `Array with ${value.length} elements`,
    when Object => `Object with keys: ${Object.keys(value).join(', ')}`,
    else => 'Unknown type'
  };
}

// Do expressions (proposal) - Future ES features
const result = do {
  const x = 5;
  const y = 10;
  if (x > y) {
    x * 2;
  } else {
    y * 2;
  }
};

// Pipeline operator (proposal) - Future ES features  
const processedValue = value
  |> x => x.toString()
  |> x => x.toUpperCase()
  |> x => x.split('')
  |> x => x.reverse()
  |> x => x.join('');

// Partial application (proposal) - Future ES features
const add = (a, b, c) => a + b + c;
const addFive = add(5, ?, ?);
const addFiveAndTwo = addFive(2, ?);
const result2 = addFiveAndTwo(3); // 10

// Enhanced error cause chaining - ES2022/2023
try {
  throw new Error('Primary error');
} catch (originalError) {
  throw new Error('Secondary error', { cause: originalError });
}

// Export with enhanced syntax
export { data, wasmModule, processedValue };
export * as utilities from './utils.js';