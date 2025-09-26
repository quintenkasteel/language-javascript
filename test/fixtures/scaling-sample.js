// Large scaling sample for performance testing
function createLargeDataStructure(size) {
  var data = [];
  for (var i = 0; i < size; i++) {
    data.push({
      id: i,
      name: "Item " + i,
      children: []
    });
  }
  return data;
}

var largeArray = createLargeDataStructure(1000);
var processedData = largeArray.map(function(item) {
  return {
    processedId: item.id * 2,
    processedName: item.name.toUpperCase(),
    metadata: {
      created: new Date().toISOString(),
      processed: true
    }
  };
});

console.log("Processed " + processedData.length + " items");