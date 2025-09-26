// Test fixture for throughput benchmarking
// This file contains realistic JavaScript patterns for performance testing

// Function declarations with various parameter patterns
function complexFunction(param1, param2, options = {}, ...rest) {
    const {
        timeout = 5000,
        retries = 3,
        debug = false,
        callback = null
    } = options;
    
    if (debug) {
        console.log(`Processing with timeout: ${timeout}, retries: ${retries}`);
    }
    
    const result = new Promise((resolve, reject) => {
        let attempts = 0;
        
        const attemptOperation = () => {
            attempts++;
            
            try {
                const data = processData(param1, param2, ...rest);
                
                if (callback && typeof callback === 'function') {
                    callback(null, data);
                }
                
                resolve(data);
            } catch (error) {
                if (attempts < retries) {
                    setTimeout(attemptOperation, timeout / retries);
                } else {
                    if (callback) {
                        callback(error);
                    }
                    reject(error);
                }
            }
        };
        
        attemptOperation();
    });
    
    return result;
}

// Class definition with modern JavaScript features
class DataProcessor {
    #privateField = new WeakMap();
    
    constructor(config = {}) {
        this.config = {
            batchSize: 1000,
            parallel: true,
            validation: true,
            ...config
        };
        
        this.#privateField.set(this, {
            statistics: {
                processed: 0,
                errors: 0,
                startTime: Date.now()
            }
        });
    }
    
    async processLargeDataset(dataset) {
        const stats = this.#privateField.get(this);
        const { batchSize, parallel, validation } = this.config;
        
        if (validation && !Array.isArray(dataset)) {
            throw new TypeError('Dataset must be an array');
        }
        
        const batches = [];
        for (let i = 0; i < dataset.length; i += batchSize) {
            batches.push(dataset.slice(i, i + batchSize));
        }
        
        const processResults = parallel
            ? await Promise.all(batches.map(batch => this.processBatch(batch)))
            : await this.processSequentially(batches);
        
        stats.processed += dataset.length;
        return processResults.flat();
    }
    
    async processBatch(batch) {
        return batch.map(item => {
            try {
                return this.transformItem(item);
            } catch (error) {
                const stats = this.#privateField.get(this);
                stats.errors++;
                return { error: error.message, item };
            }
        });
    }
    
    async processSequentially(batches) {
        const results = [];
        for (const batch of batches) {
            const batchResult = await this.processBatch(batch);
            results.push(...batchResult);
            
            // Yield control to event loop
            await new Promise(resolve => setImmediate(resolve));
        }
        return results;
    }
    
    transformItem(item) {
        if (typeof item === 'object' && item !== null) {
            return {
                ...item,
                processed: true,
                timestamp: Date.now(),
                hash: this.generateHash(JSON.stringify(item))
            };
        }
        
        return {
            value: item,
            processed: true,
            timestamp: Date.now(),
            hash: this.generateHash(String(item))
        };
    }
    
    generateHash(input) {
        let hash = 0;
        for (let i = 0; i < input.length; i++) {
            const char = input.charCodeAt(i);
            hash = ((hash << 5) - hash) + char;
            hash = hash & hash; // Convert to 32-bit integer
        }
        return hash;
    }
    
    getStatistics() {
        const stats = this.#privateField.get(this);
        return {
            ...stats,
            duration: Date.now() - stats.startTime,
            throughput: stats.processed / ((Date.now() - stats.startTime) / 1000)
        };
    }
}

// Module exports and complex object patterns
const Utils = {
    async fetchWithRetry(url, options = {}) {
        const {
            retries = 3,
            delay = 1000,
            timeout = 5000,
            ...fetchOptions
        } = options;
        
        for (let attempt = 1; attempt <= retries; attempt++) {
            try {
                const controller = new AbortController();
                const timeoutId = setTimeout(() => controller.abort(), timeout);
                
                const response = await fetch(url, {
                    ...fetchOptions,
                    signal: controller.signal
                });
                
                clearTimeout(timeoutId);
                
                if (!response.ok) {
                    throw new Error(`HTTP ${response.status}: ${response.statusText}`);
                }
                
                return await response.json();
            } catch (error) {
                if (attempt === retries) {
                    throw error;
                }
                
                await new Promise(resolve => setTimeout(resolve, delay * attempt));
            }
        }
    },
    
    debounce(func, wait, immediate = false) {
        let timeout;
        return function executedFunction(...args) {
            const later = () => {
                timeout = null;
                if (!immediate) func.apply(this, args);
            };
            
            const callNow = immediate && !timeout;
            clearTimeout(timeout);
            timeout = setTimeout(later, wait);
            
            if (callNow) func.apply(this, args);
        };
    },
    
    throttle(func, limit) {
        let inThrottle;
        return function(...args) {
            if (!inThrottle) {
                func.apply(this, args);
                inThrottle = true;
                setTimeout(() => inThrottle = false, limit);
            }
        };
    }
};

// Complex regex patterns and string processing
const ValidationPatterns = {
    email: /^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/,
    phone: /^\+?[\d\s\-\(\)]+$/,
    url: /^https?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()@:%_\+.~#?&//=]*)$/,
    
    validate(type, value) {
        const pattern = this[type];
        if (!pattern) {
            throw new Error(`Unknown validation type: ${type}`);
        }
        
        return pattern.test(String(value));
    },
    
    sanitize(input, options = {}) {
        const {
            allowHtml = false,
            maxLength = 1000,
            trim = true
        } = options;
        
        let sanitized = String(input);
        
        if (trim) {
            sanitized = sanitized.trim();
        }
        
        if (sanitized.length > maxLength) {
            sanitized = sanitized.substring(0, maxLength);
        }
        
        if (!allowHtml) {
            sanitized = sanitized
                .replace(/&/g, '&amp;')
                .replace(/</g, '&lt;')
                .replace(/>/g, '&gt;')
                .replace(/"/g, '&quot;')
                .replace(/'/g, '&#39;');
        }
        
        return sanitized;
    }
};

// Event system with complex patterns
class EventEmitter {
    constructor() {
        this.events = new Map();
        this.maxListeners = 10;
    }
    
    on(event, listener) {
        if (typeof listener !== 'function') {
            throw new TypeError('Listener must be a function');
        }
        
        if (!this.events.has(event)) {
            this.events.set(event, []);
        }
        
        const listeners = this.events.get(event);
        if (listeners.length >= this.maxListeners) {
            console.warn(`MaxListenersExceededWarning: ${listeners.length + 1} listeners added to event ${event}`);
        }
        
        listeners.push(listener);
        return this;
    }
    
    once(event, listener) {
        const onceWrapper = (...args) => {
            this.off(event, onceWrapper);
            listener.apply(this, args);
        };
        
        return this.on(event, onceWrapper);
    }
    
    off(event, listener) {
        if (!this.events.has(event)) {
            return this;
        }
        
        const listeners = this.events.get(event);
        const index = listeners.indexOf(listener);
        
        if (index !== -1) {
            listeners.splice(index, 1);
        }
        
        if (listeners.length === 0) {
            this.events.delete(event);
        }
        
        return this;
    }
    
    emit(event, ...args) {
        if (!this.events.has(event)) {
            return false;
        }
        
        const listeners = [...this.events.get(event)];
        
        for (const listener of listeners) {
            try {
                listener.apply(this, args);
            } catch (error) {
                console.error(`Error in event listener for ${event}:`, error);
            }
        }
        
        return true;
    }
    
    removeAllListeners(event) {
        if (event) {
            this.events.delete(event);
        } else {
            this.events.clear();
        }
        
        return this;
    }
    
    listenerCount(event) {
        return this.events.has(event) ? this.events.get(event).length : 0;
    }
}

// Export patterns for module compatibility testing
if (typeof module !== 'undefined' && module.exports) {
    module.exports = {
        complexFunction,
        DataProcessor,
        Utils,
        ValidationPatterns,
        EventEmitter
    };
} else if (typeof window !== 'undefined') {
    window.ThroughputTest = {
        complexFunction,
        DataProcessor,
        Utils,
        ValidationPatterns,
        EventEmitter
    };
}