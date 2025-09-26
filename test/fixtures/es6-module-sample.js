// ES6 Module syntax sample
import defaultExport from './module.js';
import * as name from './module.js';
import { export1 } from './module.js';
import { export1 as alias1 } from './module.js';
import { export1, export2 } from './module.js';
import { foo, bar } from './module.js';
import defaultExport, { export1, export2 } from './module.js';
import defaultExport, * as name from './module.js';
import './module.js';

// Dynamic imports
const modulePromise = import('./module.js');
const { export1: dynamicExport } = await import('./dynamic-module.js');

// Re-exports
export { default } from './module.js';
export * from './module.js';
export { export1 } from './module.js';
export { export1 as alias } from './module.js';

// Named exports
export const namedConstant = 42;
export let namedVariable = 'test';
export function namedFunction() {
  return 'function export';
}
export class NamedClass {
  constructor(value) {
    this.value = value;
  }
}

// Default export variations
export default function() {
  return 'default function';
}

export default class {
  constructor(name) {
    this.name = name;
  }
}

const value = 100;
export default value;

// Complex exports
const obj = {
  method1() { return 1; },
  method2() { return 2; }
};
export const { method1, method2 } = obj;

export const [first, second, ...rest] = [1, 2, 3, 4, 5];