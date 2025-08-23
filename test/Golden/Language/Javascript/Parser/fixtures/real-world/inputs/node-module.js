// Real-world Node.js module example
const fs = require('fs').promises;
const path = require('path');
const crypto = require('crypto');

class FileCache {
    constructor(cacheDir = './cache', maxAge = 3600000) {
        this.cacheDir = cacheDir;
        this.maxAge = maxAge;
        this.ensureCacheDir();
    }

    async ensureCacheDir() {
        try {
            await fs.mkdir(this.cacheDir, { recursive: true });
        } catch (error) {
            if (error.code !== 'EEXIST') {
                throw error;
            }
        }
    }

    generateKey(input) {
        return crypto
            .createHash('sha256')
            .update(JSON.stringify(input))
            .digest('hex');
    }

    getCachePath(key) {
        return path.join(this.cacheDir, `${key}.json`);
    }

    async get(key) {
        const cacheKey = this.generateKey(key);
        const cachePath = this.getCachePath(cacheKey);

        try {
            const stats = await fs.stat(cachePath);
            const age = Date.now() - stats.mtime.getTime();

            if (age > this.maxAge) {
                await this.delete(key);
                return null;
            }

            const data = await fs.readFile(cachePath, 'utf8');
            return JSON.parse(data);
        } catch (error) {
            if (error.code === 'ENOENT') {
                return null;
            }
            throw error;
        }
    }

    async set(key, value) {
        const cacheKey = this.generateKey(key);
        const cachePath = this.getCachePath(cacheKey);
        
        const data = JSON.stringify(value, null, 2);
        await fs.writeFile(cachePath, data, 'utf8');
    }

    async delete(key) {
        const cacheKey = this.generateKey(key);
        const cachePath = this.getCachePath(cacheKey);

        try {
            await fs.unlink(cachePath);
            return true;
        } catch (error) {
            if (error.code === 'ENOENT') {
                return false;
            }
            throw error;
        }
    }

    async clear() {
        const files = await fs.readdir(this.cacheDir);
        const deletePromises = files
            .filter(file => file.endsWith('.json'))
            .map(file => fs.unlink(path.join(this.cacheDir, file)));
        
        await Promise.all(deletePromises);
    }

    async keys() {
        const files = await fs.readdir(this.cacheDir);
        return files
            .filter(file => file.endsWith('.json'))
            .map(file => path.basename(file, '.json'));
    }
}

module.exports = FileCache;

// Usage example
if (require.main === module) {
    const cache = new FileCache();
    
    (async () => {
        await cache.set('user:123', { name: 'John', age: 30 });
        const user = await cache.get('user:123');
        console.log('Cached user:', user);
    })().catch(console.error);
}