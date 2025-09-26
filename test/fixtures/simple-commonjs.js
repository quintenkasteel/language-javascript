// Simple CommonJS module without destructuring
var fs = {};  // Simulated fs module
var path = {};  // Simulated path module

function FileManager(basePath) {
  this.basePath = basePath;
  this.events = {};
}

FileManager.prototype.on = function(event, callback) {
  if (!this.events[event]) {
    this.events[event] = [];
  }
  this.events[event].push(callback);
};

FileManager.prototype.emit = function(event, data) {
  if (this.events[event]) {
    for (var i = 0; i < this.events[event].length; i++) {
      this.events[event][i](data);
    }
  }
};

FileManager.prototype.readFile = function(filename, callback) {
  try {
    var content = "simulated file content";
    this.emit('fileRead', { filename: filename, size: content.length });
    callback(null, content);
  } catch (error) {
    this.emit('error', error);
    callback(error);
  }
};

FileManager.prototype.writeFile = function(filename, content, callback) {
  try {
    this.emit('fileWritten', { filename: filename, size: content.length });
    callback(null);
  } catch (error) {
    this.emit('error', error);
    callback(error);
  }
};

function createManager(basePath) {
  return new FileManager(basePath);
}

function validatePath(filePath) {
  if (!filePath || typeof filePath !== 'string') {
    throw new Error('Invalid file path');
  }
  return filePath;
}

// CommonJS exports
module.exports = FileManager;
module.exports.FileManager = FileManager;
module.exports.createManager = createManager;
module.exports.validatePath = validatePath;
module.exports.version = '1.0.0';
module.exports.name = 'file-manager';