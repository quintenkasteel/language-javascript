// Chalk-style terminal coloring sample
const chalk = require('chalk');

// Basic colors
console.log(chalk.blue('Hello world!'));
console.log(chalk.red.bold('Error: Something went wrong'));
console.log(chalk.green('✓ Success'));

// Chained styles
console.log(chalk.blue.bgRed.bold('Blue text on red background'));
console.log(chalk.white.bgBlue(' INFO '));

// Template literals
const name = 'John';
const age = 30;
console.log(chalk`
  Hello {bold ${name}}, you are {red ${age}} years old.
  Your account has {green $${100.50}} remaining.
`);

// Custom themes
const error = chalk.bold.red;
const warning = chalk.keyword('orange');
const info = chalk.blue;

console.log(error('Error: File not found'));
console.log(warning('Warning: Deprecated API'));
console.log(info('Info: Process completed'));

// Complex combinations
const log = {
  error: (msg) => console.log(chalk.red.bold(`[ERROR] ${msg}`)),
  warn: (msg) => console.log(chalk.yellow.bold(`[WARN] ${msg}`)),
  info: (msg) => console.log(chalk.blue(`[INFO] ${msg}`)),
  success: (msg) => console.log(chalk.green.bold(`[SUCCESS] ${msg}`))
};

log.error('Database connection failed');
log.warn('Using deprecated configuration');
log.info('Starting server...');
log.success('Server started successfully');

// Export for use in other modules
module.exports = {
  error,
  warning,
  info,
  log
};