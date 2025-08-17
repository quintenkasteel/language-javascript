# JavaScript Parser Extension Roadmap

## Deep Analysis Complete: Comprehensive Implementation Roadmap

Based on thorough analysis of the codebase, here's the detailed, specific todo list for adding ES2020+ JavaScript features and multiple output formats with 85%+ test coverage.

## Phase 1: ES2020+ JavaScript Features (Tasks 1-17)

### Lexical Analysis Layer

- [ ] **Task 1**: Add BigInt token support to Token.hs (BigIntToken constructor)
- [ ] **Task 2**: Add optional chaining tokens to Token.hs (?. and ?.[)
- [ ] **Task 3**: Add nullish coalescing token to Token.hs (??)
- [ ] **Task 4**: Add BigInt literal lexer rules to Lexer.x
- [ ] **Task 5**: Add optional chaining lexer rules to Lexer.x
- [ ] **Task 6**: Add nullish coalescing lexer rules to Lexer.x

**Key Finding**: Current lexer uses Alex with comprehensive token definitions - extension pattern is well-established

### AST Layer

- [ ] **Task 7**: Extend JSBinOp in AST.hs with JSBinOpNullishCoalescing
- [ ] **Task 8**: Extend JSExpression in AST.hs with JSBigIntLiteral constructor
- [ ] **Task 9**: Extend JSExpression in AST.hs with JSOptionalMemberExpression constructor
- [ ] **Task 10**: Extend JSExpression in AST.hs with JSOptionalCallExpression constructor

**Key Finding**: JSExpression has 35+ constructors already - adding 4 more follows existing patterns. JSBinOp has 21 operators - adding nullish coalescing fits the established structure.

### Grammar Layer

- [ ] **Task 11**: Add BigInt grammar rules to Grammar7.y
- [ ] **Task 12**: Add optional chaining grammar rules to Grammar7.y
- [ ] **Task 13**: Add nullish coalescing grammar rules to Grammar7.y

**Key Finding**: Grammar7.y uses precedence-based parsing - new operators need proper precedence levels

### Pretty Printing

- [ ] **Task 14**: Update pretty printer with new BigInt rendering in Printer.hs
- [ ] **Task 15**: Update pretty printer with optional chaining rendering in Printer.hs
- [ ] **Task 16**: Update pretty printer with nullish coalescing rendering in Printer.hs
- [ ] **Task 17**: Update minifier with new AST constructors in Minify.hs

**Key Finding**: Printer.hs uses `|>` operator pattern - extending is straightforward

## Phase 2: Multiple Output Formats (Tasks 18-25)

### New Output Modules

- [ ] **Task 18**: Create Language.JavaScript.Pretty.JSON module
- [ ] **Task 19**: Add JSON serialization for all AST constructors
- [ ] **Task 20**: Create Language.JavaScript.Pretty.XML module
- [ ] **Task 21**: Add XML serialization for all AST constructors
- [ ] **Task 22**: Create Language.JavaScript.Pretty.SExpr module
- [ ] **Task 23**: Add S-expression serialization for all AST constructors
- [ ] **Task 24**: Enhance Language.JavaScript.Pretty.Minified module
- [ ] **Task 25**: Add output format selector to main Parser module

**Key Finding**: Current architecture separates parsing from pretty printing - new formats can follow same pattern. TODO.txt:23 explicitly mentions "Export AST as JSON or XML" - this is a planned feature.

## Phase 3: Comprehensive Testing (Tasks 26-41)

### Feature Tests

- [ ] **Task 26**: Add BigInt literal tests to ExpressionParser.hs
- [ ] **Task 27**: Add optional chaining tests to ExpressionParser.hs
- [ ] **Task 28**: Add nullish coalescing tests to ExpressionParser.hs

**Key Finding**: Test suite uses HSpec with comprehensive round-trip testing

### Output Format Tests

- [ ] **Task 29**: Add JSON output round-trip tests
- [ ] **Task 30**: Add XML output round-trip tests
- [ ] **Task 31**: Add S-expression output round-trip tests
- [ ] **Task 32**: Add comprehensive property-based tests using QuickCheck

**Key Finding**: No existing QuickCheck usage - adding property-based testing will significantly boost coverage. Existing RoundTrip.hs shows the testing pattern.

### Coverage Infrastructure

- [ ] **Task 33**: Configure HPC code coverage in cabal file
- [ ] **Task 34**: Add coverage flags to test suite configuration
- [ ] **Task 35**: Create coverage report generation script

**Key Finding**: No existing coverage setup - this is new infrastructure

### Quality Assurance

- [ ] **Task 36**: Add edge case tests for new JS features
- [ ] **Task 37**: Update ShowStripped instances for new AST constructors
- [ ] **Task 38**: Add Data/Typeable derives for new AST constructors
- [ ] **Task 39**: Update binOpEq function for nullish coalescing
- [ ] **Task 40**: Add error handling tests for malformed new syntax
- [ ] **Task 41**: Validate 85%+ test coverage target achievement

## Implementation Complexity Assessment

### Low Risk Areas
- Token additions (Tasks 1-3): Well-defined pattern
- Pretty printer updates (Tasks 14-16): Consistent `|>` operator usage
- Basic test additions (Tasks 26-28): Established HSpec patterns

### Medium Risk Areas
- Grammar modifications (Tasks 11-13): Require Happy parser expertise
- New output format modules (Tasks 18-24): Substantial new code
- Coverage infrastructure (Tasks 33-35): New build system integration

### High Value Areas
- Property-based testing (Task 32): Major coverage boost
- JSON/XML output (Tasks 18-21): High user value (mentioned in TODO.txt)
- BigInt support (Tasks 1,4,8,11,14,17): Modern JavaScript requirement

## Key Technical Insights

1. **Architecture Strengths**: Clean separation between lexer, parser, AST, and pretty printer makes extensions straightforward
2. **Testing Foundation**: Robust HSpec suite with round-trip testing provides excellent foundation for 85%+ coverage
3. **Extension Points**: AST constructors use consistent patterns - new features fit naturally
4. **Build System**: Cabal configuration supports multiple test suites - coverage integration is feasible

## Implementation Timeline

**Estimated Timeline**: 6-8 weeks with this specific roadmap achieving the 85%+ test coverage target.

### Suggested Implementation Order

1. **Week 1-2**: Core ES2020+ features (Tasks 1-17)
2. **Week 3-4**: JSON/XML output formats (Tasks 18-21, 25)
3. **Week 5**: S-expression output and enhanced minification (Tasks 22-24)
4. **Week 6-7**: Comprehensive testing suite (Tasks 26-32, 36-40)
5. **Week 8**: Coverage infrastructure and validation (Tasks 33-35, 41)

### Dependencies

- **Tasks 1-6** must be completed before **Tasks 11-13**
- **Tasks 7-10** must be completed before **Tasks 14-17**
- **Tasks 18-24** can be implemented in parallel
- **Tasks 26-28** depend on **Tasks 1-17**
- **Tasks 29-31** depend on **Tasks 18-23**
- **Task 41** depends on all previous tasks

### Success Criteria

- ✅ All ES2020+ features parsing correctly
- ✅ All output formats producing valid, round-trip compatible results
- ✅ Test coverage ≥ 85% as measured by HPC
- ✅ No regression in existing functionality
- ✅ Performance impact < 10% for existing use cases