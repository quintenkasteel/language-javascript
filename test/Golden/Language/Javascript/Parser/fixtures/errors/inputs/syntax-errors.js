// Syntax error examples for testing error messages
// Unclosed string
var x = "unclosed string

// Unclosed comment
/* unclosed comment

// Invalid tokens
var 123invalid = "starts with number";

// Missing semicolon before statement
var a = 1
var b = 2

// Mismatched brackets
function test() {
    if (condition {
        return;
    }
}

// Invalid assignment target
1 = 2;
func() = value;

// Missing closing brace
function incomplete() {
    var x = 1;
    
// Unexpected token
var x = 1 2 3;

// Invalid regex
var regex = /[/;