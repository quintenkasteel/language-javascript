// Lodash-style utility functions sample
(function() {
  'use strict';
  
  function isArray(value) {
    return Array.isArray(value);
  }
  
  function map(array, iteratee) {
    const result = [];
    for (let i = 0; i < array.length; i++) {
      result.push(iteratee(array[i], i, array));
    }
    return result;
  }
  
  function filter(array, predicate) {
    const result = [];
    for (let i = 0; i < array.length; i++) {
      if (predicate(array[i], i, array)) {
        result.push(array[i]);
      }
    }
    return result;
  }
  
  const _ = {
    isArray: isArray,
    map: map,
    filter: filter
  };
  
  if (typeof module !== 'undefined' && module.exports) {
    module.exports = _;
  } else if (typeof window !== 'undefined') {
    window._ = _;
  }
}());