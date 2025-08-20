// Commander.js-style CLI parsing sample
#!/usr/bin/env node

const { Command } = require('commander');
const fs = require('fs');
const path = require('path');

const program = new Command();

program
  .name('file-tool')
  .description('CLI tool for file operations')
  .version('1.0.0');

program
  .command('list')
  .alias('ls')
  .description('List files in directory')
  .option('-a, --all', 'show hidden files')
  .option('-l, --long', 'use long listing format')
  .argument('[directory]', 'directory to list', '.')
  .action((directory, options) => {
    console.log(`Listing files in: ${directory}`);
    if (options.all) console.log('Including hidden files');
    if (options.long) console.log('Using long format');
    
    try {
      const files = fs.readdirSync(directory);
      files.forEach(file => {
        if (!options.all && file.startsWith('.')) return;
        
        if (options.long) {
          const stats = fs.statSync(path.join(directory, file));
          console.log(`${stats.isDirectory() ? 'd' : '-'} ${file} (${stats.size} bytes)`);
        } else {
          console.log(file);
        }
      });
    } catch (error) {
      console.error(`Error: ${error.message}`);
      process.exit(1);
    }
  });

program
  .command('copy')
  .alias('cp')
  .description('Copy files')
  .argument('<source>', 'source file')
  .argument('<destination>', 'destination file')
  .option('-f, --force', 'force overwrite')
  .action((source, destination, options) => {
    console.log(`Copying ${source} to ${destination}`);
    
    try {
      if (!options.force && fs.existsSync(destination)) {
        console.error('Error: Destination exists (use --force to overwrite)');
        process.exit(1);
      }
      
      fs.copyFileSync(source, destination);
      console.log('Copy completed successfully');
    } catch (error) {
      console.error(`Error: ${error.message}`);
      process.exit(1);
    }
  });

program
  .command('delete')
  .alias('rm')
  .description('Delete files')
  .argument('<files...>', 'files to delete')
  .option('-r, --recursive', 'delete directories recursively')
  .option('--dry-run', 'show what would be deleted without actually deleting')
  .action((files, options) => {
    files.forEach(file => {
      if (options.dryRun) {
        console.log(`Would delete: ${file}`);
        return;
      }
      
      try {
        const stats = fs.statSync(file);
        if (stats.isDirectory()) {
          if (options.recursive) {
            fs.rmSync(file, { recursive: true });
            console.log(`Deleted directory: ${file}`);
          } else {
            console.error(`Error: ${file} is a directory (use --recursive)`);
          }
        } else {
          fs.unlinkSync(file);
          console.log(`Deleted file: ${file}`);
        }
      } catch (error) {
        console.error(`Error deleting ${file}: ${error.message}`);
      }
    });
  });

// Global options
program
  .option('-v, --verbose', 'verbose output')
  .option('-q, --quiet', 'quiet mode');

// Custom help
program.addHelpText('after', `
Examples:
  $ file-tool list --all
  $ file-tool copy source.txt dest.txt --force
  $ file-tool delete file1.txt file2.txt --dry-run
`);

program.parse();

module.exports = program;