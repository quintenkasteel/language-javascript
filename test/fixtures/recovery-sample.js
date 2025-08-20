// Error recovery test sample
function recoveryTest() {
  var x = 5;
  // Intentional syntax error for recovery testing
  var y = ;
  
  // Parser should recover and continue
  function anotherFunction() {
    return "recovered";
  }
  
  return x;
}