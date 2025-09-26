---
name: Bug Report
about: Create a report to help us improve the JavaScript parser
title: '[BUG] '
labels: 'bug'
assignees: ''
---

## Bug Description

A clear and concise description of what the bug is.

## JavaScript Code

Please provide the JavaScript code that triggers the bug:

```javascript
// Paste your JavaScript code here
```

## Expected Behavior

A clear and concise description of what you expected to happen.

## Actual Behavior

A clear and concise description of what actually happened.

## Parser Output/Error

```
Paste the parser output or error message here
```

## Environment

- **language-javascript version:** [e.g., 0.8.0.0]
- **GHC version:** [e.g., 9.8.2]
- **Operating System:** [e.g., Ubuntu 22.04, macOS 13, Windows 11]
- **Cabal version:** [e.g., 3.10.2.1]

## Reproduction Steps

Steps to reproduce the behavior:

1. Create a file with the JavaScript code above
2. Run the parser with: `cabal exec language-javascript < file.js`
3. Observe the error/unexpected behavior

## Additional Context

- Does this work with other JavaScript parsers? (e.g., Babel, Acorn, etc.)
- Is this valid JavaScript according to the ECMAScript specification?
- Any additional context or screenshots

## Minimal Example

If possible, provide the smallest JavaScript code that reproduces the issue:

```javascript
// Minimal reproduction case
```

## Parser Mode

- [ ] Parsing entire programs
- [ ] Parsing expressions only
- [ ] Parsing statements only  
- [ ] Using pretty printer output
- [ ] Using JSON serialization
- [ ] Using XML serialization

## JavaScript Language Version

Which JavaScript/ECMAScript version should this code work with?

- [ ] ES3
- [ ] ES5
- [ ] ES6/ES2015
- [ ] ES2016
- [ ] ES2017
- [ ] ES2018
- [ ] ES2019
- [ ] ES2020
- [ ] ES2021
- [ ] ES2022+

## Related Issues

<!-- Link any related issues here -->