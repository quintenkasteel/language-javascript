// Commander.js-style CLI parsing sample (ES5 compatible)
(function() {
  'use strict';
  
  // Simple command parser
  function Command() {
    this.commands = {};
    this.options = {};
    this.arguments = [];
    this._name = '';
    this._description = '';
    this._version = '';
  }
  
  Command.prototype.name = function(name) {
    this._name = name;
    return this;
  };
  
  Command.prototype.description = function(desc) {
    this._description = desc;
    return this;
  };
  
  Command.prototype.version = function(version) {
    this._version = version;
    return this;
  };
  
  Command.prototype.option = function(flags, description, defaultValue) {
    this.options[flags] = {
      description: description,
      defaultValue: defaultValue
    };
    return this;
  };
  
  Command.prototype.argument = function(name, description, defaultValue) {
    this.arguments.push({
      name: name,
      description: description,
      defaultValue: defaultValue
    });
    return this;
  };
  
  Command.prototype.action = function(callback) {
    this.actionCallback = callback;
    return this;
  };
  
  Command.prototype.command = function(name) {
    var subCommand = new Command();
    subCommand.name(name);
    this.commands[name] = subCommand;
    return subCommand;
  };
  
  Command.prototype.alias = function(aliasName) {
    this.aliasName = aliasName;
    return this;
  };
  
  Command.prototype.parse = function(argv) {
    argv = argv || ['node', 'script.js'];
    
    // Simple parsing logic
    var args = argv.slice(2);
    var parsedOptions = {};
    var parsedArgs = [];
    
    for (var i = 0; i < args.length; i++) {
      var arg = args[i];
      if (arg.indexOf('--') === 0) {
        var optionName = arg.slice(2);
        parsedOptions[optionName] = true;
        if (i + 1 < args.length && args[i + 1].indexOf('--') !== 0) {
          parsedOptions[optionName] = args[i + 1];
          i++;
        }
      } else if (arg.indexOf('-') === 0) {
        var shortOption = arg.slice(1);
        parsedOptions[shortOption] = true;
      } else {
        parsedArgs.push(arg);
      }
    }
    
    if (this.actionCallback) {
      this.actionCallback.apply(null, parsedArgs.concat([parsedOptions]));
    }
  };
  
  Command.prototype.addHelpText = function(position, text) {
    this.helpText = text;
    return this;
  };
  
  // Usage example
  var program = new Command();
  
  program
    .name('file-tool')
    .description('CLI tool for file operations')
    .version('1.0.0');
  
  var listCommand = program
    .command('list')
    .description('List files in directory')
    .option('-a, --all', 'show hidden files')
    .option('-l, --long', 'use long listing format')
    .argument('[directory]', 'directory to list', '.')
    .action(function(directory, options) {
      console.log('Listing files in: ' + (directory || '.'));
      if (options.all) console.log('Including hidden files');
      if (options.long) console.log('Using long format');
      
      var files = ['file1.txt', 'file2.js', '.hidden'];
      for (var i = 0; i < files.length; i++) {
        var file = files[i];
        if (!options.all && file.charAt(0) === '.') {
          continue;
        }
        
        if (options.long) {
          console.log('- ' + file + ' (100 bytes)');
        } else {
          console.log(file);
        }
      }
    });
  
  listCommand.alias('ls');
  
  var copyCommand = program
    .command('copy')
    .description('Copy files')
    .argument('<source>', 'source file')
    .argument('<destination>', 'destination file')
    .option('-f, --force', 'force overwrite')
    .action(function(source, destination, options) {
      console.log('Copying ' + source + ' to ' + destination);
      
      if (!options.force) {
        console.log('Use --force to overwrite existing files');
      }
      
      console.log('Copy completed successfully');
    });
  
  copyCommand.alias('cp');
  
  program
    .option('-v, --verbose', 'verbose output')
    .option('-q, --quiet', 'quiet mode');
  
  program.addHelpText('after', '\nExamples:\n  $ file-tool list --all\n  $ file-tool copy source.txt dest.txt --force\n');
  
  // Export for module systems
  if (typeof module !== 'undefined' && module.exports) {
    module.exports = Command;
  } else if (typeof window !== 'undefined') {
    window.Command = Command;
  }
  
  return program;
})();