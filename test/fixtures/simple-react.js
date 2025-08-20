// Simple React-style component without JSX or imports
function MyComponent(props) {
  var title = props.title || "Default Title";
  var items = props.items || [];
  var count = 0;
  var loading = false;
  
  function handleClick(e) {
    e.preventDefault();
    loading = true;
    
    try {
      count = count + 1;
    } catch (error) {
      console.error('Failed:', error);
    } finally {
      loading = false;
    }
  }
  
  function renderItems() {
    var result = [];
    for (var i = 0; i < items.length; i++) {
      var item = items[i];
      result.push(item.name || "Item " + (i + 1));
    }
    return result;
  }
  
  return {
    title: title,
    count: count,
    handleClick: handleClick,
    renderItems: renderItems,
    loading: loading
  };
}

if (typeof module !== 'undefined' && module.exports) {
  module.exports = MyComponent;
}