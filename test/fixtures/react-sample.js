// React-style component sample with modern JavaScript features
import React, { useState, useEffect } from 'react';

const MyComponent = ({ title = "Default Title", items = [] }) => {
  const [count, setCount] = useState(0);
  const [loading, setLoading] = useState(false);
  
  useEffect(() => {
    const timer = setTimeout(() => {
      setLoading(false);
    }, 1000);
    
    return () => clearTimeout(timer);
  }, []);
  
  const handleClick = async (e) => {
    e.preventDefault();
    setLoading(true);
    
    try {
      const response = await fetch('/api/data');
      const data = await response.json();
      setCount(prevCount => prevCount + data.increment);
    } catch (error) {
      console.error('Failed to fetch:', error);
    } finally {
      setLoading(false);
    }
  };
  
  const renderItems = () => {
    return items.map((item, index) => (
      <li key={item.id || index}>
        {item.name || `Item ${index + 1}`}
      </li>
    ));
  };
  
  return (
    <div className="component">
      <h1>{title}</h1>
      <p>Count: {count}</p>
      <button onClick={handleClick} disabled={loading}>
        {loading ? 'Loading...' : 'Increment'}
      </button>
      <ul>
        {renderItems()}
      </ul>
    </div>
  );
};

export default MyComponent;