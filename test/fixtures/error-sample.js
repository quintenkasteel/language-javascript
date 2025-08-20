// Error sample with intentional syntax errors
var x = ;
function f( {
  return;
}

// Unclosed string
var s = "unclosed string

// Invalid regex
var r = /[/;

// Missing closing paren
if (condition {
  console.log("test");
}