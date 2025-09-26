# Makefile for language-javascript - JavaScript Parser for Haskell
# This Makefile provides comprehensive commands for building, testing, and publishing
#
# Usage:
#   make              - Build the library
#   make test         - Run all tests
#   make clean        - Clean build artifacts
#   make publish      - Publish to Hackage (requires auth)
#   make docs         - Generate documentation
#   make format       - Format code with ormolu
#   make lint         - Run hlint on source code

# =============================================================================
# Configuration
# =============================================================================

CABAL := cabal
PACKAGE_NAME := language-javascript
VERSION := $(shell grep '^Version:' $(PACKAGE_NAME).cabal | sed 's/Version: *//')

# Directories
SRC_DIR := src
TEST_DIR := test
DIST_DIR := dist-newstyle
DOCS_DIR := docs

# File patterns
HASKELL_FILES := $(shell find $(SRC_DIR) -name "*.hs" -o -name "*.lhs")
TEST_FILES := $(shell find $(TEST_DIR) -name "*.hs" -o -name "*.lhs")
GENERATED_FILES := src/Language/JavaScript/Parser/Lexer.hs src/Language/JavaScript/Parser/Grammar7.hs

# Tools
HLINT := hlint
ORMOLU := ormolu
HADDOCK := haddock

# Colors for output
RED := \033[31m
GREEN := \033[32m
YELLOW := \033[33m
BLUE := \033[34m
RESET := \033[0m

# =============================================================================
# Main Targets
# =============================================================================

.PHONY: all build test clean install docs format lint fix-lint help
.DEFAULT_GOAL := help

help: ## Show this help message
	@echo "$(BLUE)language-javascript Makefile$(RESET)"
	@echo "Version: $(VERSION)"
	@echo ""
	@echo "$(YELLOW)Available targets:$(RESET)"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(GREEN)%-20s$(RESET) %s\n", $$1, $$2}'

all: build ## Build the library (alias for 'build')

build: ## Build the library
	@echo "$(BLUE)Building $(PACKAGE_NAME)...$(RESET)"
	$(CABAL) build --enable-tests --enable-benchmarks
	@echo "$(GREEN)Build completed successfully!$(RESET)"

# =============================================================================
# Testing
# =============================================================================

test: ## Run all tests
	@echo "$(BLUE)Running all tests...$(RESET)"
	$(CABAL) test --test-show-details=streaming --enable-tests
	@echo "$(GREEN)All tests completed!$(RESET)"

test-quick: ## Run tests without building (faster)
	@echo "$(BLUE)Running quick tests...$(RESET)"
	$(CABAL) test --test-show-details=streaming

test-coverage: ## Run tests with coverage report
	@echo "$(BLUE)Running tests with coverage...$(RESET)"
	$(CABAL) test --enable-coverage --test-show-details=streaming
	@echo "$(YELLOW)Coverage report generated in:$(RESET) $(DIST_DIR)/build/.../coverage"

test-match: ## Run specific test pattern (usage: make test-match PATTERN="unicode")
	@echo "$(BLUE)Running tests matching pattern: $(PATTERN)$(RESET)"
	$(CABAL) test --test-show-details=streaming --test-options="--match $(PATTERN)"

# Fuzzing targets
test-fuzz-basic: ## Run basic fuzzing tests
	@echo "$(BLUE)Running basic fuzzing tests...$(RESET)"
	FUZZ_TEST_ENV=ci $(CABAL) test testsuite

test-fuzz-comprehensive: ## Run comprehensive fuzzing tests
	@echo "$(BLUE)Running comprehensive fuzzing tests...$(RESET)"
	FUZZ_TEST_ENV=development $(CABAL) test testsuite

test-fuzz-regression: ## Run fuzzing regression tests
	@echo "$(BLUE)Running fuzzing regression tests...$(RESET)"
	FUZZ_TEST_ENV=regression $(CABAL) test testsuite

# =============================================================================
# Code Quality
# =============================================================================

format: ## Format all Haskell source files with ormolu
	@echo "$(BLUE)Formatting Haskell source files...$(RESET)"
	@if command -v $(ORMOLU) > /dev/null 2>&1; then \
		$(ORMOLU) --mode inplace $(HASKELL_FILES) $(TEST_FILES); \
		echo "$(GREEN)Code formatting completed!$(RESET)"; \
	else \
		echo "$(RED)Error: $(ORMOLU) not found. Install with: cabal install ormolu$(RESET)"; \
		exit 1; \
	fi

format-check: ## Check if code is properly formatted
	@echo "$(BLUE)Checking code formatting...$(RESET)"
	@if command -v $(ORMOLU) > /dev/null 2>&1; then \
		$(ORMOLU) --mode check $(HASKELL_FILES) $(TEST_FILES) && \
		echo "$(GREEN)All files are properly formatted!$(RESET)" || \
		(echo "$(RED)Some files need formatting. Run 'make format'$(RESET)" && exit 1); \
	else \
		echo "$(RED)Error: $(ORMOLU) not found. Install with: cabal install ormolu$(RESET)"; \
		exit 1; \
	fi

lint: ## Run hlint on all source files
	@echo "$(BLUE)Running hlint on source files...$(RESET)"
	@if command -v $(HLINT) > /dev/null 2>&1; then \
		$(HLINT) $(SRC_DIR) $(TEST_DIR) \
		  --ignore="Parse error" \
		  --ignore="Use camelCase" \
		  --ignore="Reduce duplication" \
		  --report=$(DIST_DIR)/hlint-report.html && \
		echo "$(GREEN)Linting completed! Report: $(DIST_DIR)/hlint-report.html$(RESET)"; \
	else \
		echo "$(RED)Error: $(HLINT) not found. Install with: cabal install hlint$(RESET)"; \
		exit 1; \
	fi

lint-ci: ## Run hlint with CI-friendly output (fails on warnings)
	@echo "$(BLUE)Running hlint for CI...$(RESET)"
	@if command -v $(HLINT) > /dev/null 2>&1; then \
		$(HLINT) $(SRC_DIR) $(TEST_DIR) \
		  --ignore="Parse error" \
		  --ignore="Use camelCase" \
		  --ignore="Reduce duplication"; \
	else \
		echo "$(RED)Error: $(HLINT) not found. Install with: cabal install hlint$(RESET)"; \
		exit 1; \
	fi

fix-lint: ## Automatically fix hlint suggestions and format code
	@echo "$(BLUE)Auto-fixing hlint suggestions...$(RESET)"
	@if command -v $(HLINT) > /dev/null 2>&1; then \
		for file in $$(find $(SRC_DIR) $(TEST_DIR) -name "*.hs" -o -name "*.lhs"); do \
			$(HLINT) "$$file" \
			  --ignore="Parse error" \
			  --ignore="Use camelCase" \
			  --ignore="Reduce duplication" \
			  --refactor --refactor-options="--inplace" -j &>/dev/null || true; \
		done; \
		echo "$(YELLOW)Running format after hlint fixes...$(RESET)"; \
		$(MAKE) format; \
		echo "$(GREEN)Auto-fix completed!$(RESET)"; \
	else \
		echo "$(RED)Error: $(HLINT) not found. Install with: cabal install hlint$(RESET)"; \
		exit 1; \
	fi

# =============================================================================
# Documentation
# =============================================================================

docs: ## Generate Haddock documentation
	@echo "$(BLUE)Generating documentation...$(RESET)"
	$(CABAL) haddock --enable-doc-index --hyperlink-source
	@echo "$(GREEN)Documentation generated!$(RESET)"
	@echo "$(YELLOW)View at:$(RESET) $(DIST_DIR)/build/.../doc/html/$(PACKAGE_NAME)/index.html"

docs-open: docs ## Generate and open documentation in browser
	@echo "$(BLUE)Opening documentation...$(RESET)"
	@find $(DIST_DIR) -name "index.html" -path "*/$(PACKAGE_NAME)/index.html" -exec open {} \; 2>/dev/null || \
	 find $(DIST_DIR) -name "index.html" -path "*/$(PACKAGE_NAME)/index.html" -exec xdg-open {} \; 2>/dev/null || \
	 echo "$(YELLOW)Please manually open the documentation file$(RESET)"

# =============================================================================
# Building and Installing
# =============================================================================

configure: ## Configure the package
	@echo "$(BLUE)Configuring $(PACKAGE_NAME)...$(RESET)"
	$(CABAL) configure --enable-tests --enable-benchmarks

install: ## Install the package locally
	@echo "$(BLUE)Installing $(PACKAGE_NAME)...$(RESET)"
	$(CABAL) install --overwrite-policy=always
	@echo "$(GREEN)Installation completed!$(RESET)"

install-deps: ## Install all dependencies
	@echo "$(BLUE)Installing dependencies...$(RESET)"
	$(CABAL) build --dependencies-only --enable-tests --enable-benchmarks
	@echo "$(GREEN)Dependencies installed!$(RESET)"

# =============================================================================
# Cleaning
# =============================================================================

clean: ## Clean build artifacts
	@echo "$(BLUE)Cleaning build artifacts...$(RESET)"
	$(CABAL) clean
	rm -rf $(DIST_DIR)
	find . -name "*.hi" -delete
	find . -name "*.o" -delete
	find . -name "*.dyn_hi" -delete
	find . -name "*.dyn_o" -delete
	find . -name "*.p_hi" -delete
	find . -name "*.p_o" -delete
	rm -f testsuite.exe
	rm -f $(GENERATED_FILES)
	@echo "$(GREEN)Cleanup completed!$(RESET)"

clean-docs: ## Clean documentation
	@echo "$(BLUE)Cleaning documentation...$(RESET)"
	find $(DIST_DIR) -path "*/doc" -type d -exec rm -rf {} + 2>/dev/null || true
	@echo "$(GREEN)Documentation cleanup completed!$(RESET)"

distclean: clean clean-docs ## Complete cleanup including cabal files
	@echo "$(BLUE)Performing complete cleanup...$(RESET)"
	rm -f cabal.project.local
	rm -f .ghc.environment.*
	@echo "$(GREEN)Complete cleanup finished!$(RESET)"

# =============================================================================
# Version Management and Publishing
# =============================================================================

version: ## Show current version
	@echo "$(BLUE)Current version:$(RESET) $(VERSION)"

version-check: ## Verify version consistency across files
	@echo "$(BLUE)Checking version consistency...$(RESET)"
	@cabal_version=$$(grep '^Version:' $(PACKAGE_NAME).cabal | sed 's/Version: *//'); \
	changelog_version=$$(grep '^##' ChangeLog.md | head -1 | sed 's/## *//'); \
	if [ "$$cabal_version" = "$$changelog_version" ]; then \
		echo "$(GREEN)Version consistency check passed: $$cabal_version$(RESET)"; \
	else \
		echo "$(RED)Version mismatch!$(RESET)"; \
		echo "  Cabal file: $$cabal_version"; \
		echo "  ChangeLog:  $$changelog_version"; \
		exit 1; \
	fi

check-uploadable: ## Check if package is ready for upload
	@echo "$(BLUE)Checking if package is uploadable...$(RESET)"
	$(CABAL) check
	$(CABAL) sdist
	@echo "$(GREEN)Package check completed!$(RESET)"

sdist: ## Create source distribution
	@echo "$(BLUE)Creating source distribution...$(RESET)"
	$(CABAL) sdist
	@echo "$(GREEN)Source distribution created in $(DIST_DIR)/sdist/$(RESET)"

publish-check: version-check check-uploadable ## Comprehensive pre-publish checks
	@echo "$(BLUE)Running comprehensive pre-publish checks...$(RESET)"
	$(MAKE) test
	$(MAKE) lint-ci
	$(MAKE) format-check
	@echo "$(GREEN)All pre-publish checks passed!$(RESET)"

upload-candidate: publish-check ## Upload package candidate to Hackage
	@echo "$(BLUE)Uploading package candidate to Hackage...$(RESET)"
	@echo "$(YELLOW)This will upload a candidate that others can test$(RESET)"
	@read -p "Continue? (y/N): " confirm && [ "$$confirm" = "y" ] || exit 1
	$(CABAL) upload --candidate $(DIST_DIR)/sdist/$(PACKAGE_NAME)-$(VERSION).tar.gz
	@echo "$(GREEN)Candidate uploaded successfully!$(RESET)"

publish: publish-check ## Publish package to Hackage (CAUTION: Irreversible!)
	@echo "$(RED)WARNING: This will publish $(PACKAGE_NAME) v$(VERSION) to Hackage!$(RESET)"
	@echo "$(RED)This action is IRREVERSIBLE!$(RESET)"
	@echo ""
	@read -p "Are you absolutely sure you want to publish? Type 'PUBLISH' to confirm: " confirm; \
	if [ "$$confirm" = "PUBLISH" ]; then \
		echo "$(BLUE)Publishing to Hackage...$(RESET)"; \
		$(CABAL) upload $(DIST_DIR)/sdist/$(PACKAGE_NAME)-$(VERSION).tar.gz; \
		echo "$(GREEN)Package published successfully!$(RESET)"; \
	else \
		echo "$(YELLOW)Publish cancelled$(RESET)"; \
		exit 1; \
	fi

# =============================================================================
# Development Utilities
# =============================================================================

ghci: ## Start GHCi with the project loaded
	@echo "$(BLUE)Starting GHCi...$(RESET)"
	$(CABAL) repl

benchmark: ## Run benchmarks
	@echo "$(BLUE)Running benchmarks...$(RESET)"
	$(CABAL) bench --benchmark-options='+RTS -T'

profile: ## Build with profiling enabled
	@echo "$(BLUE)Building with profiling...$(RESET)"
	$(CABAL) build --enable-profiling

watch: ## Watch files and rebuild on changes (requires entr)
	@echo "$(BLUE)Watching for changes...$(RESET)"
	@if command -v entr > /dev/null 2>&1; then \
		find $(SRC_DIR) $(TEST_DIR) -name "*.hs" | entr -c make build; \
	else \
		echo "$(RED)Error: entr not found. Install with your package manager$(RESET)"; \
		exit 1; \
	fi

# =============================================================================
# Generated Files
# =============================================================================

generate: ## Generate Lexer and Grammar files
	@echo "$(BLUE)Generating lexer and parser files...$(RESET)"
	$(CABAL) build

# =============================================================================
# CI/CD Helpers
# =============================================================================

ci-build: ## CI build (build + test + lint + format check)
	@echo "$(BLUE)Running CI build...$(RESET)"
	$(MAKE) build
	$(MAKE) test
	$(MAKE) lint-ci
	$(MAKE) format-check
	@echo "$(GREEN)CI build completed successfully!$(RESET)"

ci-quick: ## Quick CI check (build + quick test)
	@echo "$(BLUE)Running quick CI check...$(RESET)"
	$(MAKE) build
	$(MAKE) test-quick
	@echo "$(GREEN)Quick CI check completed!$(RESET)"

# =============================================================================
# Information
# =============================================================================

info: ## Show project information
	@echo "$(BLUE)Project Information:$(RESET)"
	@echo "  Package: $(PACKAGE_NAME)"
	@echo "  Version: $(VERSION)"
	@echo "  Cabal:   $$($(CABAL) --version | head -1)"
	@echo "  GHC:     $$($(CABAL) exec -- ghc --version)"
	@echo ""
	@echo "$(BLUE)Directories:$(RESET)"
	@echo "  Source:  $(SRC_DIR)/"
	@echo "  Tests:   $(TEST_DIR)/"
	@echo "  Build:   $(DIST_DIR)/"
	@echo ""
	@echo "$(BLUE)Files:$(RESET)"
	@echo "  Haskell: $$(find $(SRC_DIR) -name "*.hs" | wc -l) source files"
	@echo "  Tests:   $$(find $(TEST_DIR) -name "*.hs" | wc -l) test files"