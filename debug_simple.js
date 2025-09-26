var handlers = {
  method1: function() { return 'handler1'; }
};
var methodName = 'method1';
var result = handlers[methodName]();
console.log(result);