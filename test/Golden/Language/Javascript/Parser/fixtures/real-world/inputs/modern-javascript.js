// Modern JavaScript features example
import { debounce, throttle } from 'lodash';
import fetch from 'node-fetch';

// Modern class with private fields
class APIClient {
    #baseUrl;
    #timeout;
    #retryCount;

    constructor(baseUrl, options = {}) {
        this.#baseUrl = baseUrl;
        this.#timeout = options.timeout ?? 5000;
        this.#retryCount = options.retryCount ?? 3;
    }

    async #makeRequest(url, options) {
        const controller = new AbortController();
        const timeoutId = setTimeout(() => controller.abort(), this.#timeout);

        try {
            const response = await fetch(url, {
                ...options,
                signal: controller.signal
            });

            clearTimeout(timeoutId);

            if (!response.ok) {
                throw new Error(`HTTP ${response.status}: ${response.statusText}`);
            }

            return response;
        } catch (error) {
            clearTimeout(timeoutId);
            throw error;
        }
    }

    async get(endpoint, params = {}) {
        const url = new URL(endpoint, this.#baseUrl);
        Object.entries(params).forEach(([key, value]) => {
            url.searchParams.append(key, value);
        });

        return this.#retryRequest(() => this.#makeRequest(url.toString()));
    }

    async post(endpoint, data) {
        const url = new URL(endpoint, this.#baseUrl);
        return this.#retryRequest(() => 
            this.#makeRequest(url.toString(), {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(data)
            })
        );
    }

    async #retryRequest(requestFn, attempt = 1) {
        try {
            return await requestFn();
        } catch (error) {
            if (attempt >= this.#retryCount) {
                throw error;
            }

            const delay = Math.pow(2, attempt) * 1000;
            await new Promise(resolve => setTimeout(resolve, delay));
            
            return this.#retryRequest(requestFn, attempt + 1);
        }
    }
}

// Modern async patterns
const asyncPipeline = async function* (iterable, ...transforms) {
    for await (const item of iterable) {
        let result = item;
        for (const transform of transforms) {
            result = await transform(result);
        }
        yield result;
    }
};

// Decorator pattern with modern syntax
const memoize = (target, propertyKey, descriptor) => {
    const originalMethod = descriptor.value;
    const cache = new Map();

    descriptor.value = function(...args) {
        const key = JSON.stringify(args);
        if (cache.has(key)) {
            return cache.get(key);
        }
        
        const result = originalMethod.apply(this, args);
        cache.set(key, result);
        return result;
    };

    return descriptor;
};

class Calculator {
    @memoize
    fibonacci(n) {
        if (n <= 1) return n;
        return this.fibonacci(n - 1) + this.fibonacci(n - 2);
    }
}

// Modern error handling with custom error classes
class ValidationError extends Error {
    constructor(field, value, message) {
        super(message);
        this.name = 'ValidationError';
        this.field = field;
        this.value = value;
    }
}

// Advanced destructuring and pattern matching
const processUserData = ({
    name,
    email,
    preferences: {
        theme = 'light',
        notifications: { email: emailNotifs = true, push: pushNotifs = false } = {}
    } = {},
    ...rest
}) => {
    return {
        displayName: name?.trim() || 'Anonymous',
        contactEmail: email?.toLowerCase(),
        settings: {
            theme,
            notifications: { email: emailNotifs, push: pushNotifs }
        },
        metadata: rest
    };
};

// Modern module patterns
export {
    APIClient,
    asyncPipeline,
    memoize,
    Calculator,
    ValidationError,
    processUserData
};

export default APIClient;