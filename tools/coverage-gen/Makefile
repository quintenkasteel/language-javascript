# Makefile for coverage-driven test generation tools
# 
# This Makefile provides convenient targets for building, testing,
# and running the coverage-driven test generation infrastructure.

# Default target
.PHONY: all
all: build test

# Build the coverage generation tool
.PHONY: build
build:
	@echo "Building coverage generation tool..."
	cabal build coverage-gen

# Run tests for coverage generation system
.PHONY: test
test:
	@echo "Running coverage generation tests..."
	cabal test coverage-gen-test

# Generate coverage report
.PHONY: coverage
coverage:
	@echo "Generating coverage report..."
	cabal test --enable-coverage
	@echo "Coverage report available in dist/hpc/"

# Run the coverage generation tool
.PHONY: run
run:
	@echo "Running coverage generation..."
	cabal run coverage-gen -- --hpc dist/hpc/tix/testsuite/testsuite.tix --target 0.95

# Run with corpus analysis
.PHONY: run-corpus
run-corpus:
	@echo "Running with corpus analysis..."
	cabal run coverage-gen -- \
		--hpc dist/hpc/tix/testsuite/testsuite.tix \
		--corpus test/fixtures/ \
		--target 0.95 \
		--verbose

# Clean build artifacts
.PHONY: clean
clean:
	@echo "Cleaning build artifacts..."
	cabal clean
	rm -rf dist-newstyle/

# Install the tool globally
.PHONY: install
install:
	@echo "Installing coverage-gen tool..."
	cabal install coverage-gen

# Run performance benchmarks
.PHONY: bench
bench:
	@echo "Running performance benchmarks..."
	cabal bench coverage-gen-bench

# Generate documentation
.PHONY: docs
docs:
	@echo "Generating documentation..."
	cabal haddock coverage-gen

# Run style checks
.PHONY: lint
lint:
	@echo "Running style checks..."
	hlint Coverage/
	@echo "Running ormolu format check..."
	ormolu --mode check Coverage/**/*.hs Main.hs

# Format code
.PHONY: format
format:
	@echo "Formatting code..."
	ormolu --mode inplace Coverage/**/*.hs Main.hs

# Run comprehensive validation
.PHONY: validate
validate: build test lint coverage
	@echo "All validation checks passed!"

# Create sample corpus
.PHONY: sample-corpus
sample-corpus:
	@echo "Creating sample corpus..."
	mkdir -p corpus/samples/
	cp ../../test/fixtures/*.js corpus/samples/ || true
	cp ../../test/k.js corpus/samples/ || true
	@echo "Sample corpus created in corpus/samples/"

# Generate test report
.PHONY: report
report:
	@echo "Generating comprehensive test report..."
	cabal test --enable-coverage --test-show-details=always > test-report.txt
	@echo "Test report saved to test-report.txt"

# Help target
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  all          - Build and test (default)"
	@echo "  build        - Build the coverage generation tool"
	@echo "  test         - Run tests"
	@echo "  coverage     - Generate coverage report"
	@echo "  run          - Run the tool with default settings"
	@echo "  run-corpus   - Run with corpus analysis"
	@echo "  clean        - Clean build artifacts"
	@echo "  install      - Install tool globally"
	@echo "  bench        - Run performance benchmarks"
	@echo "  docs         - Generate documentation"
	@echo "  lint         - Run style checks"
	@echo "  format       - Format code"
	@echo "  validate     - Run all checks"
	@echo "  sample-corpus - Create sample corpus"
	@echo "  report       - Generate test report"
	@echo "  help         - Show this help"