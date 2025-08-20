// Large JavaScript file sample for performance testing
(function(global) {
  'use strict';
  
  // Large object with many properties for stress testing
  const LARGE_CONFIG = {
    apiEndpoints: {
      users: '/api/v1/users',
      posts: '/api/v1/posts',
      comments: '/api/v1/comments',
      categories: '/api/v1/categories',
      tags: '/api/v1/tags',
      media: '/api/v1/media',
      analytics: '/api/v1/analytics',
      settings: '/api/v1/settings',
      notifications: '/api/v1/notifications',
      payments: '/api/v1/payments'
    },
    
    httpMethods: ['GET', 'POST', 'PUT', 'DELETE', 'PATCH', 'OPTIONS', 'HEAD'],
    
    statusCodes: {
      OK: 200,
      CREATED: 201,
      NO_CONTENT: 204,
      BAD_REQUEST: 400,
      UNAUTHORIZED: 401,
      FORBIDDEN: 403,
      NOT_FOUND: 404,
      METHOD_NOT_ALLOWED: 405,
      CONFLICT: 409,
      INTERNAL_SERVER_ERROR: 500,
      BAD_GATEWAY: 502,
      SERVICE_UNAVAILABLE: 503
    },
    
    // Generate large array of objects
    sampleData: Array.from({ length: 1000 }, (_, i) => ({
      id: i + 1,
      name: `Item ${i + 1}`,
      description: `This is a description for item number ${i + 1}`,
      category: `Category ${(i % 10) + 1}`,
      price: Math.random() * 1000,
      inStock: Math.random() > 0.3,
      tags: [`tag${i % 5}`, `tag${(i + 1) % 5}`, `tag${(i + 2) % 5}`],
      metadata: {
        createdAt: new Date(Date.now() - Math.random() * 86400000 * 365).toISOString(),
        updatedAt: new Date().toISOString(),
        version: Math.floor(Math.random() * 10) + 1,
        author: `user${i % 100}`,
        permissions: {
          read: true,
          write: Math.random() > 0.5,
          delete: Math.random() > 0.8
        }
      }
    }))
  };
  
  // Large class with many methods
  class DataProcessor {
    constructor(config = LARGE_CONFIG) {
      this.config = config;
      this.cache = new Map();
      this.eventListeners = new Map();
      this.processingQueue = [];
      this.stats = {
        processed: 0,
        errors: 0,
        startTime: Date.now()
      };
    }
    
    // Method 1: Data filtering
    filterByCategory(data, category) {
      return data.filter(item => item.category === category);
    }
    
    // Method 2: Data sorting
    sortByPrice(data, ascending = true) {
      return [...data].sort((a, b) => 
        ascending ? a.price - b.price : b.price - a.price
      );
    }
    
    // Method 3: Data grouping
    groupByCategory(data) {
      return data.reduce((groups, item) => {
        const category = item.category;
        if (!groups[category]) {
          groups[category] = [];
        }
        groups[category].push(item);
        return groups;
      }, {});
    }
    
    // Method 4: Data aggregation
    calculateStats(data) {
      const stats = {
        total: data.length,
        totalValue: 0,
        averagePrice: 0,
        inStockCount: 0,
        categoryCounts: {},
        priceRanges: {
          low: 0,     // < 100
          medium: 0,  // 100-500
          high: 0     // > 500
        }
      };
      
      data.forEach(item => {
        stats.totalValue += item.price;
        if (item.inStock) stats.inStockCount++;
        
        stats.categoryCounts[item.category] = 
          (stats.categoryCounts[item.category] || 0) + 1;
        
        if (item.price < 100) stats.priceRanges.low++;
        else if (item.price <= 500) stats.priceRanges.medium++;
        else stats.priceRanges.high++;
      });
      
      stats.averagePrice = stats.totalValue / stats.total;
      return stats;
    }
    
    // Method 5: Async data processing
    async processDataAsync(data, processor) {
      const results = [];
      for (const item of data) {
        try {
          const result = await processor(item);
          results.push(result);
          this.stats.processed++;
        } catch (error) {
          this.stats.errors++;
          console.error(`Error processing item ${item.id}:`, error);
        }
      }
      return results;
    }
    
    // Method 6: Batch processing
    processBatch(data, batchSize = 100) {
      const batches = [];
      for (let i = 0; i < data.length; i += batchSize) {
        batches.push(data.slice(i, i + batchSize));
      }
      
      return batches.map((batch, index) => ({
        batchNumber: index + 1,
        items: batch,
        stats: this.calculateStats(batch)
      }));
    }
    
    // Method 7: Search functionality
    search(data, query, fields = ['name', 'description']) {
      const lowerQuery = query.toLowerCase();
      return data.filter(item => 
        fields.some(field => 
          item[field] && item[field].toLowerCase().includes(lowerQuery)
        )
      );
    }
    
    // Method 8: Data validation
    validateItem(item) {
      const errors = [];
      
      if (!item.id || typeof item.id !== 'number') {
        errors.push('Invalid or missing ID');
      }
      
      if (!item.name || typeof item.name !== 'string') {
        errors.push('Invalid or missing name');
      }
      
      if (typeof item.price !== 'number' || item.price < 0) {
        errors.push('Invalid price');
      }
      
      if (typeof item.inStock !== 'boolean') {
        errors.push('Invalid inStock value');
      }
      
      return {
        valid: errors.length === 0,
        errors: errors
      };
    }
    
    // Method 9: Data transformation
    transformToApiFormat(data) {
      return data.map(item => ({
        id: item.id,
        name: item.name,
        description: item.description,
        category_id: this.getCategoryId(item.category),
        price_cents: Math.round(item.price * 100),
        available: item.inStock,
        tags: item.tags.join(','),
        created_at: item.metadata.createdAt,
        updated_at: item.metadata.updatedAt
      }));
    }
    
    // Method 10: Cache management
    cacheResult(key, data, ttl = 300000) { // 5 minutes default
      this.cache.set(key, {
        data: data,
        expires: Date.now() + ttl
      });
    }
    
    getCachedResult(key) {
      const cached = this.cache.get(key);
      if (cached && cached.expires > Date.now()) {
        return cached.data;
      }
      this.cache.delete(key);
      return null;
    }
    
    // Helper methods (11-20)
    getCategoryId(categoryName) {
      const categoryMap = {
        'Category 1': 1, 'Category 2': 2, 'Category 3': 3,
        'Category 4': 4, 'Category 5': 5, 'Category 6': 6,
        'Category 7': 7, 'Category 8': 8, 'Category 9': 9,
        'Category 10': 10
      };
      return categoryMap[categoryName] || 0;
    }
    
    formatPrice(price, currency = 'USD') {
      const formatter = new Intl.NumberFormat('en-US', {
        style: 'currency',
        currency: currency
      });
      return formatter.format(price);
    }
    
    generateReport(data) {
      const stats = this.calculateStats(data);
      const grouped = this.groupByCategory(data);
      
      return {
        summary: stats,
        categories: Object.keys(grouped).map(category => ({
          name: category,
          itemCount: grouped[category].length,
          totalValue: grouped[category].reduce((sum, item) => sum + item.price, 0),
          averagePrice: grouped[category].reduce((sum, item) => sum + item.price, 0) / grouped[category].length
        })),
        topItems: this.sortByPrice(data, false).slice(0, 10),
        bottomItems: this.sortByPrice(data, true).slice(0, 10)
      };
    }
    
    exportToCSV(data) {
      const headers = ['ID', 'Name', 'Description', 'Category', 'Price', 'In Stock'];
      const csvContent = [
        headers.join(','),
        ...data.map(item => [
          item.id,
          `"${item.name}"`,
          `"${item.description}"`,
          `"${item.category}"`,
          item.price,
          item.inStock
        ].join(','))
      ].join('\n');
      
      return csvContent;
    }
    
    // Event system methods (21-25)
    addEventListener(event, callback) {
      if (!this.eventListeners.has(event)) {
        this.eventListeners.set(event, []);
      }
      this.eventListeners.get(event).push(callback);
    }
    
    removeEventListener(event, callback) {
      if (this.eventListeners.has(event)) {
        const callbacks = this.eventListeners.get(event);
        const index = callbacks.indexOf(callback);
        if (index > -1) {
          callbacks.splice(index, 1);
        }
      }
    }
    
    emit(event, data) {
      if (this.eventListeners.has(event)) {
        this.eventListeners.get(event).forEach(callback => {
          try {
            callback(data);
          } catch (error) {
            console.error(`Error in event listener for ${event}:`, error);
          }
        });
      }
    }
    
    getProcessingStats() {
      return {
        ...this.stats,
        uptime: Date.now() - this.stats.startTime,
        successRate: this.stats.processed / (this.stats.processed + this.stats.errors) * 100
      };
    }
    
    reset() {
      this.cache.clear();
      this.eventListeners.clear();
      this.processingQueue.length = 0;
      this.stats = {
        processed: 0,
        errors: 0,
        startTime: Date.now()
      };
    }
  }
  
  // Large utility object with many functions
  const Utils = {
    // String utilities
    capitalizeWords: str => str.replace(/\b\w/g, l => l.toUpperCase()),
    slugify: str => str.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, ''),
    truncate: (str, length) => str.length > length ? str.slice(0, length) + '...' : str,
    
    // Array utilities
    chunk: (arr, size) => Array.from({ length: Math.ceil(arr.length / size) }, (_, i) =>
      arr.slice(i * size, i * size + size)
    ),
    unique: arr => [...new Set(arr)],
    flatten: arr => arr.reduce((flat, item) => flat.concat(Array.isArray(item) ? Utils.flatten(item) : item), []),
    
    // Object utilities
    deepClone: obj => JSON.parse(JSON.stringify(obj)),
    merge: (target, ...sources) => Object.assign({}, target, ...sources),
    pick: (obj, keys) => keys.reduce((result, key) => {
      if (key in obj) result[key] = obj[key];
      return result;
    }, {}),
    
    // Number utilities
    random: (min, max) => Math.random() * (max - min) + min,
    round: (num, decimals) => Math.round(num * Math.pow(10, decimals)) / Math.pow(10, decimals),
    clamp: (num, min, max) => Math.min(Math.max(num, min), max),
    
    // Date utilities
    formatDate: date => new Intl.DateTimeFormat('en-US').format(date),
    addDays: (date, days) => new Date(date.getTime() + days * 24 * 60 * 60 * 1000),
    diffDays: (date1, date2) => Math.floor((date2 - date1) / (24 * 60 * 60 * 1000)),
    
    // Validation utilities
    isEmail: email => /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email),
    isUrl: url => /^https?:\/\/[^\s$.?#].[^\s]*$/i.test(url),
    isUUID: uuid => /^[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(uuid)
  };
  
  // Export everything
  global.DataProcessor = DataProcessor;
  global.LARGE_CONFIG = LARGE_CONFIG;
  global.Utils = Utils;
  
})(typeof window !== 'undefined' ? window : global);