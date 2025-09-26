// Throughput test sample with complex JavaScript patterns
(function(global) {
  "use strict";
  
  var Utils = {
    processArray: function(arr, callback) {
      var results = [];
      for (var i = 0; i < arr.length; i++) {
        results.push(callback(arr[i], i));
      }
      return results;
    },
    
    debounce: function(func, wait) {
      var timeout;
      return function executedFunction() {
        var context = this;
        var args = arguments;
        var later = function() {
          timeout = null;
          func.apply(context, args);
        };
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
      };
    }
  };
  
  global.Utils = Utils;
})(typeof window !== 'undefined' ? window : global);