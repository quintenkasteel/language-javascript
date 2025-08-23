// ECMAScript edge cases and complex constructs
// ASI (Automatic Semicolon Insertion) cases
a = b
(c + d).print()

a = b + c
(d + e).print()

// Complex destructuring
const {a, b: {c, d = defaultValue}} = object;
const [first, , third, ...rest] = array;

// Complex template literals
`Hello ${name}, you have ${
    messages.length > 0 ? messages.length : 'no'
} new messages`;

// Complex arrow functions
const complex = (a, b = defaultValue, ...rest) => ({
    result: a + b + rest.reduce((sum, x) => sum + x, 0)
});

// Generators and async
function* generator() {
    yield 1;
    yield* otherGenerator();
    return 'done';
}

async function asyncFunc() {
    const result = await promise;
    return result;
}

// Complex object literals
const obj = {
    prop: value,
    [computed]: 'dynamic',
    method() { return this.prop; },
    async asyncMethod() { return await promise; },
    *generator() { yield 1; },
    get getter() { return this._value; },
    set setter(value) { this._value = value; }
};

// Unicode identifiers and strings
const café = "unicode";
const π = 3.14159;
const 日本語 = "Japanese";

// Complex regular expressions
/(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)/g;

// Exotic features
const {[Symbol.iterator]: iter} = iterable;
new.target;
import.meta;