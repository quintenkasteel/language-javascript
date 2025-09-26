# Comprehensive Refactoring Command Suite

**Task:** Execute systematic code refactoring and quality improvement with coordinated agent deployment for the language-javascript parser project.

- **Scope**: Complete codebase refactoring with style, structure, and quality improvements
- **Standards**: 100% CLAUDE.md compliance with systematic agent coordination  
- **Process**: Analysis → Refactoring → Validation → Quality Gates → Final Verification
- **Enforcement**: Zero tolerance - agents must achieve comprehensive quality improvements

---

## 🚀 REFACTORING ORCHESTRATION OVERVIEW

### Mission Statement

Transform the language-javascript parser codebase to achieve comprehensive quality improvements through systematic analysis, coordinated refactoring, and mandatory validation. All agents must work in harmony to ensure CLAUDE.md compliance and parsing excellence.

### Core Principles

- **Zero Tolerance**: No quality violations allowed
- **Systematic Coordination**: Agents work in proper sequence
- **Complete Coverage**: All code must meet standards
- **Parser Focus**: JavaScript parsing accuracy preserved
- **Incremental Validation**: Continuous verification throughout process

---

## 🔍 PHASE 1: COMPREHENSIVE ANALYSIS

### Analysis Agent Deployment:

```bash
# ANALYSIS PHASE: Deep codebase analysis
echo "=== PHASE 1: Comprehensive Analysis ==="

# Architecture Analysis
Task(analyze-architecture, "Analyze module structure and dependencies", "analyze-architecture")

# Test Quality Analysis  
Task(analyze-tests, "Analyze test coverage and quality patterns", "analyze-tests")

# Performance Analysis
Task(analyze-performance, "Analyze parsing performance patterns", "analyze-performance")
```

**Requirements**:
- Complete architecture evaluation with dependency mapping
- Test coverage analysis with anti-pattern detection
- Performance bottleneck identification
- Quality metrics establishment

---

## 🔧 PHASE 2: FOUNDATIONAL REFACTORING

### Core Refactoring Agent Sequence:

```bash
# FOUNDATIONAL REFACTORING: Core style improvements
echo "=== PHASE 2: Foundational Refactoring ==="

# Import Standardization
Task(validate-imports, "Standardize all import patterns to CLAUDE.md compliance", "validate-imports")

# Variable Naming Consistency
Task(variable-naming-refactor, "Standardize variable naming conventions", "variable-naming-refactor")

# Operator Style Refactoring
Task(operator-refactor, "Convert $ operators to parentheses throughout codebase", "operator-refactor")

# Let to Where Conversion
Task(let-to-where-refactor, "Convert let expressions to where clauses", "let-to-where-refactor")
```

**Validation Requirements**:
- All agents must complete successfully before proceeding
- Build must pass after each refactoring step
- Tests must continue to pass
- No regressions in parsing functionality

---

## 🏗️ PHASE 3: STRUCTURAL IMPROVEMENTS

### Structure and Pattern Agent Deployment:

```bash
# STRUCTURAL IMPROVEMENTS: Advanced refactoring patterns
echo "=== PHASE 3: Structural Improvements ==="

# Lens Implementation
Task(validate-lenses, "Implement comprehensive lens usage for record operations", "validate-lenses")

# Function Quality Enforcement
Task(validate-functions, "Ensure all functions meet size and complexity limits", "validate-functions")

# Module Structure Optimization
Task(module-structure-auditor, "Optimize module organization and dependencies", "module-structure-auditor")

# Parser Logic Validation
Task(validate-parsing, "Validate parsing logic and grammar completeness", "validate-parsing")
```

**Quality Gates**:
- Function size ≤ 15 lines
- Function parameters ≤ 4  
- Branching complexity ≤ 4
- Lens usage for all record operations
- Optimal module organization

---

## ⚡ PHASE 4: PARSER-SPECIFIC OPTIMIZATION

### Parser Enhancement Agent Coordination:

```bash
# PARSER OPTIMIZATION: JavaScript parser-specific improvements
echo "=== PHASE 4: Parser-Specific Optimization ==="

# AST Transformation Validation
Task(validate-ast-transformation, "Validate AST manipulation patterns", "validate-ast-transformation")

# Code Generation Optimization
Task(validate-code-generation, "Optimize pretty printer and code generation", "validate-code-generation")

# Compiler Pattern Validation
Task(validate-compiler-patterns, "Validate compiler design patterns", "validate-compiler-patterns")

# Security Validation
Task(validate-security, "Ensure secure JavaScript input handling", "validate-security")
```

**Parser Requirements**:
- All JavaScript constructs properly parsed
- Error handling covers all edge cases
- AST transformations preserve semantics
- Performance optimized for large files

---

## 🧪 PHASE 5: TEST QUALITY ENFORCEMENT

### Test Quality Agent Deployment:

```bash
# TEST QUALITY: Comprehensive test improvement
echo "=== PHASE 5: Test Quality Enforcement ==="

# Test Analysis and Gap Identification
Task(analyze-tests, "Comprehensive test analysis with gap identification", "analyze-tests")

# Test Creation for Coverage Gaps
Task(validate-test-creation, "Create comprehensive tests for all gaps", "validate-test-creation")

# Test Execution and Validation
Task(validate-tests, "Execute all tests and validate results", "validate-tests")

# Golden File Validation
Task(validate-golden-files, "Validate and update golden test files", "validate-golden-files")
```

**Test Requirements**:
- 85%+ code coverage achieved
- Zero anti-pattern violations
- All parser functionality tested
- Error conditions comprehensively covered

---

## 🎯 PHASE 6: FINAL VALIDATION AND QUALITY ASSURANCE

### Comprehensive Quality Validation:

```bash
# FINAL VALIDATION: Complete quality assurance
echo "=== PHASE 6: Final Validation ==="

# Format and Lint Validation
Task(validate-format, "Apply final formatting and lint checks", "validate-format")

# Build System Validation
Task(validate-build, "Ensure complete build system success", "validate-build")

# Documentation Validation
Task(validate-documentation, "Validate and improve documentation", "validate-documentation")

# Final Style Enforcement
Task(code-style-enforcer, "Comprehensive final style validation", "code-style-enforcer")
```

**Final Quality Gates**:
- Build: 100% success with 0 warnings
- Tests: 100% pass rate with 85%+ coverage
- Format: 100% ormolu and hlint compliance
- Style: 100% CLAUDE.md compliance
- Documentation: Complete and accurate

---

## 📊 MANDATORY VALIDATION CHECKPOINTS

### Inter-Phase Validation:

```bash
# VALIDATION CHECKPOINTS: Between each phase
validate_phase() {
    local phase_name="$1"
    echo "🔍 Validating $phase_name completion..."
    
    # Build must pass
    if ! cabal build; then
        echo "❌ Build failed after $phase_name"
        exit 1
    fi
    
    # Tests must pass
    if ! cabal test; then
        echo "❌ Tests failed after $phase_name"
        exit 1
    fi
    
    # Style compliance check
    if ! .claude/commands/test-quality-audit test/; then
        echo "❌ Style violations detected after $phase_name"
        exit 1
    fi
    
    echo "✅ $phase_name validation passed"
}

# Call after each phase:
validate_phase "Analysis"
validate_phase "Foundational Refactoring"
validate_phase "Structural Improvements"
validate_phase "Parser Optimization"
validate_phase "Test Quality"
validate_phase "Final Validation"
```

---

## 🔄 ITERATIVE IMPROVEMENT PROTOCOL

### Agent Coordination Requirements:

1. **Sequential Execution**: Agents must run in proper dependency order
2. **Validation Gates**: Each phase must pass validation before proceeding
3. **Cross-Agent Communication**: Agents coordinate through shared validation scripts
4. **Incremental Progress**: Progress tracked and reported at each step
5. **Rollback Capability**: Ability to rollback if critical issues detected

### Error Handling and Recovery:

```bash
# ERROR HANDLING: Systematic error recovery
handle_refactoring_error() {
    local failed_agent="$1"
    local error_type="$2"
    
    echo "🚨 Refactoring error in $failed_agent: $error_type"
    
    case "$error_type" in
        "build_failure")
            echo "Build failed - investigating compilation errors"
            Task(validate-build, "Analyze and fix build issues", "validate-build")
            ;;
        "test_failure")
            echo "Tests failed - analyzing test issues"
            Task(validate-tests, "Analyze and fix test failures", "validate-tests")
            ;;
        "style_violation")
            echo "Style violations - enforcing compliance"
            Task(code-style-enforcer, "Fix style violations", "code-style-enforcer")
            ;;
    esac
}
```

---

## 📋 SUCCESS CRITERIA AND METRICS

### Comprehensive Success Metrics:

```bash
# SUCCESS METRICS: Measure refactoring success
generate_refactoring_report() {
    echo "📊 REFACTORING SUCCESS REPORT"
    echo "=============================="
    
    # Code Quality Metrics
    echo "Code Quality Improvements:"
    echo "- CLAUDE.md Compliance: $(check_claude_compliance)%"
    echo "- Function Size Compliance: $(check_function_sizes)%"
    echo "- Import Organization: $(check_import_organization)%"
    echo "- Lens Usage: $(check_lens_usage)%"
    
    # Test Quality Metrics
    echo "Test Quality Improvements:"
    echo "- Test Coverage: $(measure_coverage)%"
    echo "- Anti-Pattern Violations: $(count_antipatterns)"
    echo "- Meaningful Test Ratio: $(calculate_meaningful_tests)%"
    
    # Parser Quality Metrics
    echo "Parser Quality Improvements:"
    echo "- Grammar Coverage: $(measure_grammar_coverage)%"
    echo "- Error Handling Coverage: $(measure_error_coverage)%"
    echo "- Performance Optimization: $(measure_performance_improvement)%"
}
```

### Final Validation Requirements:

- **Code Quality**: 100% CLAUDE.md compliance
- **Test Quality**: 85%+ coverage with 0 anti-patterns
- **Parser Quality**: All JavaScript constructs supported
- **Build Quality**: 0 warnings, 0 errors
- **Documentation**: Complete and accurate
- **Performance**: Optimized for production use

---

This comprehensive refactoring command orchestrates systematic code quality improvement for the language-javascript parser project through coordinated agent deployment, ensuring excellence in all aspects of code quality while preserving parsing functionality and achieving CLAUDE.md compliance.