// CommonJS module syntax sample
const fs = require('fs');
const path = require('path');
const { promisify } = require('util');
const EventEmitter = require('events');

// Destructuring requires
const { 
  readFile, 
  writeFile, 
  stat 
} = require('fs').promises;

// Conditional requires
let logger;
if (process.env.NODE_ENV === 'development') {
  logger = require('./dev-logger');
} else {
  logger = require('./prod-logger');
}

// Dynamic requires
const moduleName = process.env.MODULE || 'default';
const dynamicModule = require(`./modules/${moduleName}`);

// Module creation
class FileManager extends EventEmitter {
  constructor(basePath) {
    super();
    this.basePath = basePath;
  }
  
  async readFileContent(filename) {
    try {
      const fullPath = path.join(this.basePath, filename);
      const content = await readFile(fullPath, 'utf8');
      this.emit('fileRead', filename, content.length);
      return content;
    } catch (error) {
      this.emit('error', error);
      throw error;
    }
  }
  
  async writeFileContent(filename, content) {
    try {
      const fullPath = path.join(this.basePath, filename);
      await writeFile(fullPath, content, 'utf8');
      this.emit('fileWritten', filename, content.length);
    } catch (error) {
      this.emit('error', error);
      throw error;
    }
  }
}

// Utility functions
function createManager(basePath) {
  return new FileManager(basePath);
}

function validatePath(filePath) {
  if (!filePath || typeof filePath !== 'string') {
    throw new Error('Invalid file path');
  }
  return path.normalize(filePath);
}

// Module exports - various patterns
module.exports = FileManager;

module.exports.FileManager = FileManager;
module.exports.createManager = createManager;
module.exports.validatePath = validatePath;

// Alternative export pattern
exports.FileManager = FileManager;
exports.createManager = createManager;
exports.validatePath = validatePath;

// Conditional exports
if (process.env.INCLUDE_HELPERS === 'true') {
  module.exports.helpers = {
    isFile: async (path) => {
      try {
        const stats = await stat(path);
        return stats.isFile();
      } catch {
        return false;
      }
    },
    isDirectory: async (path) => {
      try {
        const stats = await stat(path);
        return stats.isDirectory();
      } catch {
        return false;
      }
    }
  };
}

// Module metadata
module.exports.version = '1.0.0';
module.exports.name = 'file-manager';