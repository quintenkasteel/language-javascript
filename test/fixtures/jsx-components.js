// JSX Component Patterns Test Fixture  
// This file contains React JSX patterns compiled to JavaScript

import React, { useState, useEffect, useContext, useMemo, useCallback } from 'react';

// Simple functional component with JSX
const SimpleComponent = () => {
  return React.createElement("div", null, "Hello World");
};

// Component with props
const ComponentWithProps = (props) => {
  return React.createElement(
    "div", 
    { className: props.className, id: props.id },
    React.createElement("h1", null, props.title),
    React.createElement("p", null, props.content)
  );
};

// Component with destructured props
const ComponentWithDestructuredProps = ({ title, content, className = "default" }) => {
  return React.createElement(
    "article",
    { className },
    React.createElement("h2", null, title),
    React.createElement("div", { dangerouslySetInnerHTML: { __html: content } })
  );
};

// Component with state hooks
const StatefulComponent = () => {
  const [count, setCount] = useState(0);
  const [loading, setLoading] = useState(false);
  const [user, setUser] = useState(null);
  
  useEffect(() => {
    setLoading(true);
    fetchUser()
      .then(userData => {
        setUser(userData);
        setLoading(false);
      })
      .catch(error => {
        console.error('Failed to fetch user:', error);
        setLoading(false);
      });
  }, []);
  
  const handleIncrement = useCallback(() => {
    setCount(prevCount => prevCount + 1);
  }, []);
  
  const userDisplay = useMemo(() => {
    if (!user) return "No user";
    return `${user.name} (${user.email})`;
  }, [user]);
  
  if (loading) {
    return React.createElement("div", { className: "loading" }, "Loading...");
  }
  
  return React.createElement(
    "div",
    { className: "stateful-component" },
    React.createElement("h3", null, "User: ", userDisplay),
    React.createElement("p", null, "Count: ", count),
    React.createElement(
      "button", 
      { onClick: handleIncrement, disabled: loading },
      "Increment"
    )
  );
};

// Component with children and render props
const ContainerComponent = ({ children, render }) => {
  const [data, setData] = useState([]);
  
  useEffect(() => {
    fetchData().then(setData);
  }, []);
  
  return React.createElement(
    "div",
    { className: "container" },
    React.createElement("header", null, "Data Container"),
    children,
    render && render(data),
    React.createElement(
      "footer", 
      null, 
      `Total items: ${data.length}`
    )
  );
};

// Higher-order component pattern
const withAuth = (WrappedComponent) => {
  return (props) => {
    const [isAuthenticated, setIsAuthenticated] = useState(false);
    
    useEffect(() => {
      checkAuth().then(setIsAuthenticated);
    }, []);
    
    if (!isAuthenticated) {
      return React.createElement(
        "div", 
        { className: "auth-required" },
        "Please log in to continue"
      );
    }
    
    return React.createElement(WrappedComponent, props);
  };
};

// Context provider pattern
const ThemeContext = React.createContext();

const ThemeProvider = ({ children, theme = "light" }) => {
  const [currentTheme, setCurrentTheme] = useState(theme);
  
  const toggleTheme = useCallback(() => {
    setCurrentTheme(prev => prev === "light" ? "dark" : "light");
  }, []);
  
  const value = useMemo(() => ({
    theme: currentTheme,
    toggleTheme
  }), [currentTheme, toggleTheme]);
  
  return React.createElement(
    ThemeContext.Provider,
    { value },
    children
  );
};

// Component using context
const ThemedComponent = () => {
  const { theme, toggleTheme } = useContext(ThemeContext);
  
  return React.createElement(
    "div",
    { 
      className: `themed-component theme-${theme}`,
      style: { 
        backgroundColor: theme === "light" ? "#ffffff" : "#333333",
        color: theme === "light" ? "#333333" : "#ffffff"
      }
    },
    React.createElement("h4", null, `Current theme: ${theme}`),
    React.createElement(
      "button",
      { onClick: toggleTheme },
      "Toggle Theme"
    )
  );
};

// Fragment patterns
const ComponentWithFragments = () => {
  return React.createElement(
    React.Fragment,
    null,
    React.createElement("h1", null, "Title"),
    React.createElement("p", null, "Description"),
    React.createElement("p", null, "More content")
  );
};

// Short fragment syntax (React 16.2+)
const ComponentWithShortFragments = () => {
  return React.createElement(
    React.Fragment,
    null,
    React.createElement("span", null, "First"),
    React.createElement("span", null, "Second"),
    React.createElement("span", null, "Third")
  );
};

// Complex component with multiple JSX patterns
const ComplexComponent = ({ items = [], onItemClick, headerComponent: HeaderComponent }) => {
  const [filter, setFilter] = useState("");
  const [sortOrder, setSortOrder] = useState("asc");
  
  const filteredItems = useMemo(() => {
    return items
      .filter(item => 
        item.name.toLowerCase().includes(filter.toLowerCase())
      )
      .sort((a, b) => {
        const modifier = sortOrder === "asc" ? 1 : -1;
        return a.name.localeCompare(b.name) * modifier;
      });
  }, [items, filter, sortOrder]);
  
  return React.createElement(
    "div",
    { className: "complex-component" },
    HeaderComponent && React.createElement(HeaderComponent, { 
      title: "Item List",
      itemCount: filteredItems.length 
    }),
    React.createElement(
      "div",
      { className: "controls" },
      React.createElement("input", {
        type: "text",
        placeholder: "Filter items...",
        value: filter,
        onChange: (e) => setFilter(e.target.value)
      }),
      React.createElement(
        "select",
        {
          value: sortOrder,
          onChange: (e) => setSortOrder(e.target.value)
        },
        React.createElement("option", { value: "asc" }, "A-Z"),
        React.createElement("option", { value: "desc" }, "Z-A")
      )
    ),
    React.createElement(
      "ul",
      { className: "item-list" },
      filteredItems.map((item, index) =>
        React.createElement(
          "li",
          { 
            key: item.id || index,
            className: `item ${item.active ? 'active' : 'inactive'}`,
            onClick: () => onItemClick && onItemClick(item)
          },
          React.createElement("span", { className: "item-name" }, item.name),
          item.description && React.createElement(
            "small", 
            { className: "item-description" }, 
            item.description
          )
        )
      )
    ),
    filteredItems.length === 0 && React.createElement(
      "div",
      { className: "empty-state" },
      "No items found"
    )
  );
};

// Class component pattern (legacy but still used)
class ClassComponent extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      value: '',
      isEditing: false
    };
    
    this.handleChange = this.handleChange.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
    this.toggleEdit = this.toggleEdit.bind(this);
  }
  
  componentDidMount() {
    console.log('Component mounted');
  }
  
  componentDidUpdate(prevProps, prevState) {
    if (prevState.value !== this.state.value) {
      console.log('Value changed:', this.state.value);
    }
  }
  
  componentWillUnmount() {
    console.log('Component will unmount');
  }
  
  handleChange(event) {
    this.setState({ value: event.target.value });
  }
  
  handleSubmit(event) {
    event.preventDefault();
    this.props.onSubmit && this.props.onSubmit(this.state.value);
    this.setState({ isEditing: false });
  }
  
  toggleEdit() {
    this.setState(prevState => ({ isEditing: !prevState.isEditing }));
  }
  
  render() {
    const { isEditing, value } = this.state;
    
    if (isEditing) {
      return React.createElement(
        "form",
        { onSubmit: this.handleSubmit },
        React.createElement("input", {
          type: "text",
          value: value,
          onChange: this.handleChange,
          autoFocus: true
        }),
        React.createElement("button", { type: "submit" }, "Save"),
        React.createElement("button", { 
          type: "button", 
          onClick: this.toggleEdit 
        }, "Cancel")
      );
    }
    
    return React.createElement(
      "div",
      { onClick: this.toggleEdit },
      React.createElement("span", null, value || "Click to edit")
    );
  }
}

// Utility functions for async data fetching
async function fetchUser() {
  const response = await fetch('/api/user');
  if (!response.ok) {
    throw new Error('Failed to fetch user');
  }
  return response.json();
}

async function fetchData() {
  const response = await fetch('/api/data');
  if (!response.ok) {
    throw new Error('Failed to fetch data');
  }
  return response.json();
}

async function checkAuth() {
  try {
    const response = await fetch('/api/auth/check');
    return response.ok;
  } catch {
    return false;
  }
}

// Export all components
export {
  SimpleComponent,
  ComponentWithProps,
  ComponentWithDestructuredProps,
  StatefulComponent,
  ContainerComponent,
  withAuth,
  ThemeProvider,
  ThemeContext,
  ThemedComponent,
  ComponentWithFragments,
  ComponentWithShortFragments,
  ComplexComponent,
  ClassComponent
};

export default ComplexComponent;