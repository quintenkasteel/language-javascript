// ECMAScript statement examples
// Variable declarations
var x = 1;
let y = 2;
const z = 3;

// Function declarations
function add(a, b) {
    return a + b;
}

// Control flow statements
if (condition) {
    statement1();
} else if (other) {
    statement2();
} else {
    statement3();
}

switch (value) {
    case 1:
        break;
    case 2:
        continue;
    default:
        throw new Error("Invalid");
}

// Loop statements
for (let i = 0; i < 10; i++) {
    console.log(i);
}

for (const item of array) {
    process(item);
}

for (const key in object) {
    handle(key, object[key]);
}

while (condition) {
    doWork();
}

do {
    work();
} while (condition);

// Try-catch statements
try {
    riskyOperation();
} catch (error) {
    handleError(error);
} finally {
    cleanup();
}

// Class declarations
class MyClass extends BaseClass {
    constructor(value) {
        super();
        this.value = value;
    }
    
    method() {
        return this.value;
    }
    
    static staticMethod() {
        return "static";
    }
}

// Import/export statements
import { named } from './module.js';
import * as namespace from './module.js';
import defaultExport from './module.js';

export const exportedVar = 42;
export function exportedFunc() {}
export default class {}
export { named as renamed };