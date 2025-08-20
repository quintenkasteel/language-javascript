// TypeScript Declaration File Patterns Test Fixture
// This file simulates TypeScript .d.ts file constructs emitted as JavaScript

// Ambient declarations - declare keyword patterns
declare function getElementById(id: string): HTMLElement | null;
declare const process: NodeJS.Process;
declare var global: NodeJS.Global;
declare let Buffer: BufferConstructor;

// Namespace declarations
declare namespace NodeJS {
  interface Process {
    env: ProcessEnv;
    exit(code?: number): never;
    cwd(): string;
  }
  
  interface ProcessEnv {
    [key: string]: string | undefined;
  }
}

// Module declarations  
declare module "fs" {
  export function readFile(path: string, callback: Function): void;
  export function writeFile(path: string, data: string, callback: Function): void;
  export const constants: {
    F_OK: number;
    R_OK: number;
    W_OK: number;
    X_OK: number;
  };
}

declare module "*.json" {
  const value: any;
  export default value;
}

declare module "*.css" {
  const classes: { [key: string]: string };
  export default classes;
}

// Global augmentation
declare global {
  interface Window {
    myCustomProperty: string;
    myCustomMethod(): void;
  }
  
  var myGlobalFunction: (input: string) => void;
}

// Interface declarations (translated to object patterns)
const UserInterface = {
  // interface User {
  //   id: number;
  //   name: string;
  //   email?: string;
  //   readonly createdAt: Date;
  // }
  
  // Simulated interface as object schema
  schema: {
    id: "number",
    name: "string", 
    email: "string?",
    createdAt: "Date"
  }
};

// Type alias patterns (as runtime constructs)
const StringOrNumber = ["string", "number"];
const UserArray = Array;

// Generic type parameters (simulated with functions)
function identity<T>(arg: T): T {
  return arg;
}

function createArray<T>(...items: T[]): T[] {
  return items;
}

// Class with access modifiers
class ExampleClass {
  private _privateField: string;
  protected _protectedField: number;
  public publicField: boolean;
  readonly readonlyField: string;
  static staticField: number = 42;
  
  constructor(
    private _constructorParam: string,
    public publicParam: number
  ) {
    this._privateField = _constructorParam;
    this._protectedField = publicParam;
    this.publicField = true;
    this.readonlyField = "readonly";
  }
  
  private _privateMethod(): void {
    console.log("Private method");
  }
  
  protected _protectedMethod(): void {
    console.log("Protected method");  
  }
  
  public publicMethod(): void {
    console.log("Public method");
  }
  
  static staticMethod(): void {
    console.log("Static method");
  }
}

// Abstract class pattern
abstract class AbstractBase {
  abstract abstractMethod(): void;
  
  concreteMethod(): void {
    console.log("Concrete implementation");
  }
}

// Interface implementation pattern
class ConcreteImplementation extends AbstractBase implements UserInterface {
  id: number = 1;
  name: string = "John";
  email?: string;
  readonly createdAt: Date = new Date();
  
  abstractMethod(): void {
    console.log("Abstract method implementation");
  }
}

// Enum declarations
enum Color {
  Red = "red",
  Green = "green", 
  Blue = "blue"
}

enum Direction {
  Up = 1,
  Down,
  Left,
  Right
}

// Const assertions and as const
const config = {
  apiUrl: "https://api.example.com",
  timeout: 5000,
  retries: 3
} as const;

const statusCodes = [200, 404, 500] as const;

// Utility types (simulated)
function Partial<T>(obj: T): Partial<T> {
  return { ...obj };
}

function Required<T>(obj: T): Required<T> {
  return { ...obj };
}

function Pick<T, K extends keyof T>(obj: T, keys: K[]): Pick<T, K> {
  const result = {} as Pick<T, K>;
  keys.forEach(key => {
    result[key] = obj[key];
  });
  return result;
}

// Conditional types (simulated with functions)
function isString(value: any): value is string {
  return typeof value === "string";
}

// Mapped types (simulated)
function makeOptional<T>(obj: T): { [K in keyof T]?: T[K] } {
  return { ...obj };
}

// Template literal types (simulated)
const createRoute = (base: string, path: string): `${string}/${string}` => {
  return `${base}/${path}`;
};

// Export patterns
export {
  UserInterface,
  ExampleClass,
  AbstractBase,
  ConcreteImplementation,
  Color,
  Direction,
  config,
  identity,
  createArray
};

export default ExampleClass;
export type { UserInterface as User };
export * from "./other-declarations";