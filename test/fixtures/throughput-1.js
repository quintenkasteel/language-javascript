// Throughput benchmark sample 1
function fibonacci(n) {
  if (n <= 1) return n;
  return fibonacci(n - 1) + fibonacci(n - 2);
}

function quickSort(arr) {
  if (arr.length <= 1) return arr;
  
  var pivot = arr[Math.floor(arr.length / 2)];
  var left = arr.filter(function(x) { return x < pivot; });
  var middle = arr.filter(function(x) { return x === pivot; });
  var right = arr.filter(function(x) { return x > pivot; });
  
  return quickSort(left).concat(middle).concat(quickSort(right));
}

var testData = [64, 34, 25, 12, 22, 11, 90];
var sorted = quickSort(testData);
var fib10 = fibonacci(10);