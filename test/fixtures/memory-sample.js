// Memory usage test sample
function createMemoryIntensiveObject() {
  var obj = {
    data: new Array(1000).fill(0).map(function(_, i) {
      return {
        id: i,
        payload: "x".repeat(100)
      };
    }),
    
    process: function() {
      return this.data.reduce(function(acc, item) {
        acc[item.id] = item.payload.length;
        return acc;
      }, {});
    },
    
    cleanup: function() {
      this.data = null;
    }
  };
  
  return obj;
}

var memoryTest = createMemoryIntensiveObject();
var result = memoryTest.process();
memoryTest.cleanup();