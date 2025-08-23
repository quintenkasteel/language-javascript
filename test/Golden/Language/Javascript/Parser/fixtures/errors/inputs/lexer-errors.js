// Lexer error examples
// Invalid escape sequences
var str = "\x";
var str2 = "\u";
var str3 = "\u123";

// Invalid numeric literals
var num1 = 0x;
var num2 = 0b;
var num3 = 0o;
var num4 = 1e;
var num5 = 1e+;

// Invalid unicode identifiers
var \u0000invalid = "null character";

// Unterminated template literal
var template = `unterminated

// Invalid regular expression
var regex = /*/;