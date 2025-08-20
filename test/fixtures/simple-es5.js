// Simple ES5-compatible JavaScript without modern features
(function(global) {
  'use strict';
  
  // Configuration object
  var CONFIG = {
    apiEndpoints: {
      users: '/api/v1/users',
      posts: '/api/v1/posts',
      comments: '/api/v1/comments'
    },
    
    httpMethods: ['GET', 'POST', 'PUT', 'DELETE'],
    
    statusCodes: {
      OK: 200,
      CREATED: 201,
      BAD_REQUEST: 400,
      NOT_FOUND: 404,
      INTERNAL_ERROR: 500
    }
  };
  
  // Constructor function
  function DataProcessor(config) {
    this.config = config || CONFIG;
    this.cache = {};
    this.stats = {
      processed: 0,
      errors: 0,
      startTime: new Date().getTime()
    };
  }
  
  // Method: Filter data by category
  DataProcessor.prototype.filterByCategory = function(data, category) {
    var result = [];
    for (var i = 0; i < data.length; i++) {
      if (data[i].category === category) {
        result.push(data[i]);
      }
    }
    return result;
  };
  
  // Method: Sort data by price
  DataProcessor.prototype.sortByPrice = function(data, ascending) {
    var sortedData = data.slice(); // Copy array
    sortedData.sort(function(a, b) {
      if (ascending) {
        return a.price - b.price;
      } else {
        return b.price - a.price;
      }
    });
    return sortedData;
  };
  
  // Method: Group data by category
  DataProcessor.prototype.groupByCategory = function(data) {
    var groups = {};
    for (var i = 0; i < data.length; i++) {
      var item = data[i];
      var category = item.category;
      if (!groups[category]) {
        groups[category] = [];
      }
      groups[category].push(item);
    }
    return groups;
  };
  
  // Method: Calculate statistics
  DataProcessor.prototype.calculateStats = function(data) {
    var stats = {
      total: data.length,
      totalValue: 0,
      averagePrice: 0,
      inStockCount: 0,
      categoryCounts: {}
    };
    
    for (var i = 0; i < data.length; i++) {
      var item = data[i];
      stats.totalValue += item.price;
      if (item.inStock) {
        stats.inStockCount++;
      }
      
      if (!stats.categoryCounts[item.category]) {
        stats.categoryCounts[item.category] = 0;
      }
      stats.categoryCounts[item.category]++;
    }
    
    if (stats.total > 0) {
      stats.averagePrice = stats.totalValue / stats.total;
    }
    
    return stats;
  };
  
  // Method: Search functionality
  DataProcessor.prototype.search = function(data, query, fields) {
    fields = fields || ['name', 'description'];
    var lowerQuery = query.toLowerCase();
    var results = [];
    
    for (var i = 0; i < data.length; i++) {
      var item = data[i];
      var found = false;
      
      for (var j = 0; j < fields.length; j++) {
        var field = fields[j];
        if (item[field] && item[field].toLowerCase().indexOf(lowerQuery) !== -1) {
          found = true;
          break;
        }
      }
      
      if (found) {
        results.push(item);
      }
    }
    
    return results;
  };
  
  // Method: Validate item
  DataProcessor.prototype.validateItem = function(item) {
    var errors = [];
    
    if (!item.id || typeof item.id !== 'number') {
      errors.push('Invalid or missing ID');
    }
    
    if (!item.name || typeof item.name !== 'string') {
      errors.push('Invalid or missing name');
    }
    
    if (typeof item.price !== 'number' || item.price < 0) {
      errors.push('Invalid price');
    }
    
    return {
      valid: errors.length === 0,
      errors: errors
    };
  };
  
  // Method: Cache management
  DataProcessor.prototype.cacheResult = function(key, data, ttl) {
    ttl = ttl || 300000; // 5 minutes default
    this.cache[key] = {
      data: data,
      expires: new Date().getTime() + ttl
    };
  };
  
  DataProcessor.prototype.getCachedResult = function(key) {
    var cached = this.cache[key];
    if (cached && cached.expires > new Date().getTime()) {
      return cached.data;
    }
    delete this.cache[key];
    return null;
  };
  
  // Utility object
  var Utils = {
    capitalizeWords: function(str) {
      return str.replace(/\b\w/g, function(letter) {
        return letter.toUpperCase();
      });
    },
    
    chunk: function(arr, size) {
      var chunks = [];
      for (var i = 0; i < arr.length; i += size) {
        chunks.push(arr.slice(i, i + size));
      }
      return chunks;
    },
    
    unique: function(arr) {
      var result = [];
      for (var i = 0; i < arr.length; i++) {
        if (result.indexOf(arr[i]) === -1) {
          result.push(arr[i]);
        }
      }
      return result;
    },
    
    random: function(min, max) {
      return Math.random() * (max - min) + min;
    },
    
    round: function(num, decimals) {
      var factor = Math.pow(10, decimals);
      return Math.round(num * factor) / factor;
    }
  };
  
  // Export to global
  global.DataProcessor = DataProcessor;
  global.CONFIG = CONFIG;
  global.Utils = Utils;
  
})(typeof window !== 'undefined' ? window : this);