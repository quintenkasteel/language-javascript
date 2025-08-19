# JavaScript Parser Enhancement TODO List

This document tracks remaining implementation tasks for enhancing the language-javascript parser with comprehensive test coverage and modern JavaScript feature support.

## 🔥 HIGH PRIORITY (Critical for Parser Robustness)

### Task 14: Parser Error Condition Tests ⚠️
**Status**: In Progress  
**Priority**: HIGH - Critical for robust error handling

Implement comprehensive error condition tests for invalid syntax combinations:

- **Await outside async context**:
  - `await expression` in non-async functions
  - `await` in global scope (non-module)
  - `await` in sync generator functions

- **Private fields outside class context**:
  - `#field` access outside class methods
  - Private field declarations outside class bodies
  - Private method calls outside class scope

- **Malformed syntax recovery**:
  - Incomplete function declarations
  - Unclosed brackets and parentheses
  - Invalid destructuring patterns
  - Malformed template literals

**Files to Update**:
- `test/Test/Language/Javascript/Validator.hs`
- Add new error test cases to existing validation test suite

### Task 15: ES6+ Feature Constraint Tests
**Status**: Pending  
**Priority**: HIGH - Modern JavaScript compliance

Implement tests for ES6+ features with proper context validation:

- **Super usage validation**:
  - `super()` calls only in constructor
  - `super.method()` only in class methods
  - `super` outside class context errors

- **new.target validation**:
  - `new.target` only in function/constructor context
  - Arrow functions cannot access `new.target`
  - Global scope `new.target` errors

- **Rest parameters validation**:
  - Rest parameter must be last
  - Only one rest parameter per function
  - Rest parameter in destructuring

**Implementation Notes**:
- Add `NewTargetOutsideFunction` error type
- Add `SuperOutsideClass` error type
- Extend validation context with constructor/method flags

### Task 16: Module System Error Tests
**Status**: Pending  
**Priority**: HIGH - ES6 modules compliance

Implement comprehensive module system validation:

- **Import/export outside modules**:
  - Import statements in scripts (non-modules)
  - Export statements in scripts
  - Dynamic import restrictions

- **Module dependency validation**:
  - Circular import detection
  - Missing module specifier validation
  - Invalid module specifier formats

- **Module context validation**:
  - Top-level await only in modules
  - Module-only syntax in scripts

**Error Types to Add**:
- `TopLevelAwaitOutsideModule`
- `CircularImportDependency`
- `InvalidModuleSpecifier`

## 🎯 MEDIUM PRIORITY (Important for Code Quality)

### Task 17: Syntax Validation Tests
**Status**: Pending  
**Priority**: MEDIUM - Language specification compliance

Implement comprehensive syntax validation tests:

- **Label validation**:
  - Label shadowing in nested scopes
  - Label conflicts with variable names
  - Break/continue to non-existent labels

- **Reserved word validation**:
  - Context-sensitive reserved words
  - Future reserved words in strict mode
  - Reserved words as property names (allowed vs forbidden)

- **Multiple default validation**:
  - Switch statements with multiple defaults
  - Function parameter defaults
  - Destructuring defaults

**Test Categories**:
- Lexical scoping edge cases
- Context-sensitive parsing
- Strict mode vs sloppy mode differences

### Task 18: Literal Validation Tests
**Status**: Pending  
**Priority**: MEDIUM - Input validation robustness

Implement comprehensive literal validation and parsing tests:

- **Escape sequence validation**:
  - Valid Unicode escapes (`\u{1F600}`)
  - Invalid escape sequences
  - Hex escapes (`\x41`)
  - Octal escapes (strict mode restrictions)

- **Regex pattern validation**:
  - Valid regex flags combinations
  - Invalid regex patterns
  - Unicode regex patterns
  - Regex literal edge cases

- **String literal edge cases**:
  - Template literal validation
  - Tagged template literal parsing
  - Multi-line string handling

**Implementation Focus**:
- Input sanitization
- Error recovery for malformed literals
- Cross-browser compatibility

### Task 19: Duplicate Detection Tests
**Status**: Pending  
**Priority**: MEDIUM - 15 test cases identified

Implement systematic duplicate detection validation:

- **Function scope duplicates**:
  - Duplicate function declarations
  - Function/variable name conflicts
  - Parameter name duplicates

- **Block scope duplicates**:
  - Let/const redeclaration
  - Function/let conflicts
  - Import name conflicts

- **Object/class duplicates**:
  - Duplicate object property names
  - Duplicate class method names
  - Getter/setter conflicts

**Test Cases** (15 total):
1. Duplicate function declarations in same scope
2. Function redeclaring variable
3. Let redeclaring let/const/var
4. Const redeclaring any binding
5. Parameter duplicate names
6. Object duplicate property names (strict mode)
7. Class duplicate method names
8. Getter/setter same property conflicts
9. Import specifier name conflicts
10. Export specifier name conflicts
11. Label name conflicts
12. Switch case label duplicates
13. Catch parameter shadowing
14. For loop variable conflicts
15. Nested scope shadowing edge cases

### Task 20: Getter/Setter Validation Tests
**Status**: Pending  
**Priority**: MEDIUM - Property accessor compliance

Implement comprehensive getter/setter validation:

- **Parameter validation**:
  - Getters with parameters (forbidden)
  - Setters without parameters (forbidden)
  - Setters with multiple parameters (forbidden)
  - Valid getter/setter pairs

- **Context validation**:
  - Static getters/setters
  - Private getters/setters
  - Computed property getters/setters

- **Object literal validation**:
  - Duplicate accessor names
  - Data property + accessor conflicts
  - Accessor + method conflicts

**Implementation Requirements**:
- Extend validation context for property types
- Add accessor-specific error types
- Test object literal vs class accessor differences

## 🔍 LOW PRIORITY (Nice to Have)

### Task 21: Integration Tests for Complex Scenarios
**Status**: Pending  
**Priority**: LOW - Real-world scenario coverage

Implement integration tests for complex, nested validation scenarios:

- **Multi-level nesting validation**:
  - Async generators with complex destructuring
  - Class expressions with private fields in modules
  - Template literals with embedded expressions

- **Cross-feature interaction tests**:
  - Import assertions with destructuring
  - Private fields with static class blocks
  - Async/await with generator delegation

- **Real-world code patterns**:
  - React component patterns
  - Node.js module patterns
  - Framework-specific constructs

### Task 22: Test Coverage Verification
**Status**: Pending  
**Priority**: LOW - Quality assurance

Verify and achieve 95%+ test coverage for validation functions:

- **Coverage analysis**:
  - Line coverage measurement
  - Branch coverage analysis
  - Function coverage verification

- **Gap identification**:
  - Uncovered error paths
  - Missing edge case tests
  - Untested validation branches

- **Coverage reporting**:
  - Generate coverage reports
  - Identify coverage gaps
  - Prioritize gap filling

**Target**: 95%+ coverage of all validation functions

## 📋 Implementation Guidelines

### Code Quality Standards
All implementations must follow the CLAUDE.md standards:
- Functions ≤15 lines, ≤4 parameters, ≤4 branches
- Use lenses for record access/updates
- Qualified imports pattern
- Comprehensive Haddock documentation
- 85%+ test coverage per module

### Testing Strategy
- **Unit tests**: Every validation function
- **Property tests**: Parser invariants
- **Golden tests**: Error message consistency
- **Integration tests**: Real-world scenarios
- **NO MOCK FUNCTIONS**: Test actual functionality

### Error Handling Requirements
- Rich error types with position information
- Helpful error messages with suggestions
- Validation context tracking
- Total functions where possible

### Performance Considerations
- Efficient validation algorithms
- Minimal memory allocation
- Stream processing for large files
- Profile-driven optimization

## 🎯 Success Criteria

### For Each Task:
- [ ] All tests pass
- [ ] No compilation warnings
- [ ] Documentation updated
- [ ] Code follows CLAUDE.md standards
- [ ] Coverage requirements met
- [ ] Performance impact assessed

### Overall Goals:
- [ ] 95%+ validation test coverage
- [ ] Comprehensive error condition handling
- [ ] Full ES2015+ specification compliance
- [ ] Robust malformed input handling
- [ ] Production-ready parser validation

## 📝 Notes

This TODO list represents the remaining work to achieve comprehensive JavaScript parser validation and testing. Priority should be given to HIGH priority tasks that improve parser robustness and error handling.

The implementation approach should be systematic, completing one task category at a time while maintaining all existing functionality and test coverage.

Each task should include both positive tests (valid syntax should parse) and negative tests (invalid syntax should produce appropriate errors) to ensure comprehensive validation coverage.