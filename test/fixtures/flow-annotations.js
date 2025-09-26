// Flow Type Annotation Patterns Test Fixture
// This file simulates Flow-annotated JavaScript with type annotations stripped

// Basic type annotations on functions
function add(a, b) {
  // Flow: function add(a: number, b: number): number
  return a + b;
}

function greet(name, age) {
  // Flow: function greet(name: string, age?: number): string
  if (age !== undefined) {
    return `Hello ${name}, you are ${age} years old`;
  }
  return `Hello ${name}`;
}

// Arrow functions with Flow annotations
const multiply = (x, y) => {
  // Flow: const multiply = (x: number, y: number): number => x * y;
  return x * y;
};

const processUser = (user) => {
  // Flow: const processUser = (user: User): Promise<ProcessedUser> => 
  return new Promise((resolve) => {
    setTimeout(() => {
      resolve({
        ...user,
        processed: true,
        timestamp: Date.now()
      });
    }, 100);
  });
};

// Object type annotations
const user = {
  // Flow: const user: {name: string, age: number, email?: string} = {
  name: "John Doe",
  age: 30,
  email: "john@example.com"
};

const config = {
  // Flow: const config: {|apiUrl: string, timeout: number, debug: boolean|} = {
  apiUrl: "https://api.example.com",
  timeout: 5000,
  debug: false
};

// Array type annotations
const numbers = [1, 2, 3, 4, 5];
// Flow: const numbers: Array<number> = [1, 2, 3, 4, 5];

const users = [
  // Flow: const users: Array<User> = [
  { id: 1, name: "Alice", active: true },
  { id: 2, name: "Bob", active: false }
];

// Generic function types
function identity(x) {
  // Flow: function identity<T>(x: T): T
  return x;
}

function map(array, fn) {
  // Flow: function map<T, U>(array: Array<T>, fn: T => U): Array<U>
  return array.map(fn);
}

function createContainer(initialValue) {
  // Flow: function createContainer<T>(initialValue: T): Container<T>
  return {
    value: initialValue,
    get() {
      return this.value;
    },
    set(newValue) {
      this.value = newValue;
    }
  };
}

// Class with Flow annotations
class Rectangle {
  // Flow: class Rectangle {
  //   width: number;
  //   height: number;
  
  constructor(width, height) {
    // Flow: constructor(width: number, height: number)
    this.width = width;
    this.height = height;
  }
  
  area() {
    // Flow: area(): number
    return this.width * this.height;
  }
  
  scale(factor) {
    // Flow: scale(factor: number): Rectangle
    return new Rectangle(this.width * factor, this.height * factor);
  }
  
  static square(size) {
    // Flow: static square(size: number): Rectangle
    return new Rectangle(size, size);
  }
}

// Generic class
class Container {
  // Flow: class Container<T> {
  //   value: T;
  
  constructor(value) {
    // Flow: constructor(value: T)
    this.value = value;
  }
  
  get() {
    // Flow: get(): T
    return this.value;
  }
  
  set(newValue) {
    // Flow: set(newValue: T): void
    this.value = newValue;
  }
  
  map(fn) {
    // Flow: map<U>(fn: T => U): Container<U>
    return new Container(fn(this.value));
  }
}

// Union types (simulated with runtime checks)
function processStringOrNumber(value) {
  // Flow: function processStringOrNumber(value: string | number): string
  if (typeof value === "string") {
    return value.toUpperCase();
  } else if (typeof value === "number") {
    return value.toString();
  }
  throw new Error("Invalid type");
}

// Intersection types (simulated with object spread)
function combineObjects(obj1, obj2) {
  // Flow: function combineObjects<A, B>(obj1: A, obj2: B): A & B
  return { ...obj1, ...obj2 };
}

// Optional and nullable types
function processOptionalString(str) {
  // Flow: function processOptionalString(str?: ?string): string
  if (str == null) {
    return "default";
  }
  return str.trim();
}

// Function types
const calculator = {
  // Flow: const calculator: {
  //   add: (a: number, b: number) => number,
  //   subtract: (a: number, b: number) => number,
  //   operation: ?(a: number, b: number) => number
  // } = {
  add: (a, b) => a + b,
  subtract: (a, b) => a - b,
  operation: null
};

// Higher-order functions
function createValidator(predicate) {
  // Flow: function createValidator<T>(predicate: T => boolean): T => boolean
  return (value) => {
    try {
      return predicate(value);
    } catch {
      return false;
    }
  };
}

const isValidUser = createValidator((user) => {
  return typeof user.name === "string" && 
         typeof user.age === "number" && 
         user.age > 0;
});

// Async functions with Flow types
async function fetchUserData(userId) {
  // Flow: async function fetchUserData(userId: string): Promise<User>
  const response = await fetch(`/api/users/${userId}`);
  
  if (!response.ok) {
    throw new Error(`Failed to fetch user: ${response.status}`);
  }
  
  const userData = await response.json();
  return userData;
}

async function* generateNumbers(max) {
  // Flow: async function* generateNumbers(max: number): AsyncGenerator<number, void, void>
  for (let i = 0; i < max; i++) {
    await new Promise(resolve => setTimeout(resolve, 100));
    yield i;
  }
}

// Type guards (runtime type checking)
function isString(value) {
  // Flow: function isString(value: mixed): boolean %checks
  return typeof value === "string";
}

function isUser(obj) {
  // Flow: function isUser(obj: mixed): boolean %checks
  return obj != null &&
         typeof obj === "object" &&
         typeof obj.name === "string" &&
         typeof obj.age === "number";
}

// Exact object types (simulated with Object.freeze)
const exactConfig = Object.freeze({
  // Flow: const exactConfig: {|apiUrl: string, timeout: number|} = {
  apiUrl: "https://api.example.com",
  timeout: 5000
});

// Disjoint union types (tagged unions)
function processShape(shape) {
  // Flow: function processShape(shape: Circle | Rectangle | Triangle): number
  switch (shape.type) {
    case "circle":
      return Math.PI * shape.radius * shape.radius;
    case "rectangle":
      return shape.width * shape.height;
    case "triangle":
      return 0.5 * shape.base * shape.height;
    default:
      throw new Error(`Unknown shape type: ${shape.type}`);
  }
}

const circle = { type: "circle", radius: 5 };
const rectangle = { type: "rectangle", width: 10, height: 20 };
const triangle = { type: "triangle", base: 8, height: 12 };

// Mapped types (simulated with utility functions)
function makePartial(obj) {
  // Flow: function makePartial<T: {}>(obj: T): $Shape<T>
  const partial = {};
  for (const key in obj) {
    if (Math.random() > 0.5) {
      partial[key] = obj[key];
    }
  }
  return partial;
}

function makeReadonly(obj) {
  // Flow: function makeReadonly<T: {}>(obj: T): $ReadOnly<T>
  return Object.freeze({ ...obj });
}

// Utility types
function pick(obj, keys) {
  // Flow: function pick<T: {}, K: $Keys<T>>(obj: T, keys: Array<K>): $Pick<T, K>
  const result = {};
  keys.forEach(key => {
    if (key in obj) {
      result[key] = obj[key];
    }
  });
  return result;
}

function omit(obj, keys) {
  // Flow: function omit<T: {}, K: $Keys<T>>(obj: T, keys: Array<K>): $Diff<T, {[K]: any}>
  const result = { ...obj };
  keys.forEach(key => {
    delete result[key];
  });
  return result;
}

// Export patterns with Flow types
export {
  // Flow: export {
  //   add,
  //   greet,
  //   multiply,
  //   processUser,
  //   Rectangle,
  //   Container,
  //   calculator,
  //   fetchUserData,
  //   isString,
  //   isUser,
  //   processShape
  // };
  add,
  greet,
  multiply,
  processUser,
  Rectangle,
  Container,
  calculator,
  fetchUserData,
  isString,
  isUser,
  processShape
};

export default Rectangle;
// Flow: export default Rectangle;