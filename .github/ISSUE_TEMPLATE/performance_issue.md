---
name: Performance Issue
about: Report performance problems or memory issues
title: '[PERFORMANCE] '
labels: 'performance'
assignees: ''
---

## Performance Issue Description

A clear description of the performance problem you're experiencing.

## JavaScript Code

Please provide the JavaScript code that demonstrates the performance issue:

```javascript
// Paste your JavaScript code here
// Include file size if it's a large file
```

## Performance Metrics

**File size:** [e.g., 2.5MB]
**Parse time:** [e.g., 45 seconds]
**Memory usage:** [e.g., 1.2GB peak memory]

## Expected Performance

What performance would you expect for this input?

## Environment

- **language-javascript version:** [e.g., 0.8.0.0]
- **GHC version:** [e.g., 9.8.2]
- **Operating System:** [e.g., Ubuntu 22.04]
- **Available RAM:** [e.g., 16GB]
- **CPU:** [e.g., Intel i7-12700K]

## Reproduction Steps

1. Create a file with the JavaScript code
2. Time the parsing: `time cabal exec language-javascript < largefile.js`
3. Monitor memory usage with: `top` or `htop`

## Profiling Information

If you've done any profiling, please share the results:

```
Profiling output here
```

## Comparison with Other Tools

How does performance compare with other JavaScript parsers?

| Parser | Parse Time | Memory Usage |
|--------|------------|--------------|
| language-javascript | ? | ? |
| Babel | ? | ? |
| Acorn | ? | ? |

## Type of Performance Issue

- [ ] Slow parsing speed
- [ ] High memory usage
- [ ] Memory leaks
- [ ] Exponential time complexity
- [ ] Stack overflow
- [ ] Other: ___________

## JavaScript Characteristics

What characteristics does your JavaScript code have?

- [ ] Deeply nested structures
- [ ] Very long identifier names
- [ ] Many string literals
- [ ] Complex regular expressions
- [ ] Large number of functions
- [ ] Heavy use of ES6+ features
- [ ] Minified code
- [ ] Generated/transpiled code

## Impact

- [ ] Blocks usage entirely
- [ ] Significantly slows down workflow
- [ ] Minor inconvenience
- [ ] Academic interest

## Suggested Solutions

Do you have any ideas for improving performance?

## Additional Context

Any additional information about the performance issue.