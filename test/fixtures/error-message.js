// Error message quality testing
function test() {
  // Various syntax errors to test error message quality
  if (condition {
    console.log("missing closing paren");
  }
  
  var obj = {
    prop1: "value1"
    prop2: "value2"  // Missing comma
  };
  
  return 
    42;  // Automatic semicolon insertion issue
}