# Adapted from: http://www.greghendershott.com/2017/04/racket-makefiles.html
PACKAGE-NAME=relation

DEPS-FLAGS=--check-pkg-deps --unused-pkg-deps

help:
	@echo "build - Compile libraries"
	@echo "build-docs - Build docs"
	@echo "check-deps - Check dependencies"
	@echo "build-all - Compile libraries, build docs, and check dependencies"
	@echo "clean - Remove all build artifacts"
	@echo "install - Install package along with dependencies"
	@echo "remove - Remove package"
	@echo "test - Run tests"
	@echo "docs - View docs in a browser"
	@echo "profile - Run benchmarks to gauge relative performance against built-in interfaces"

# Primarily for use by CI.
# Installs dependencies as well as linking this as a package.
install:
	raco pkg install --deps search-auto

remove:
	raco pkg remove $(PACKAGE-NAME)

# Primarily for day-to-day dev.
# Build libraries from source.
build:
	raco setup --no-docs --tidy --pkgs $(PACKAGE-NAME)

# Primarily for day-to-day dev.
# Build docs (if any).
build-docs:
	raco setup --no-launcher --no-foreign-libs --no-info-domain --no-pkg-deps \
	--no-install --no-post-install --tidy --pkgs $(PACKAGE-NAME)

# Primarily for day-to-day dev.
# Build libraries from source, build docs (if any), and check dependencies.
build-all:
	raco setup --tidy $(DEPS-FLAGS) --pkgs $(PACKAGE-NAME)

# Primarily for use by CI, after make install -- since that already
# does the equivalent of make setup, this tries to do as little as
# possible except checking deps.
check-deps:
	raco setup --no-docs $(DEPS-FLAGS) $(PACKAGE-NAME)

# Note: Each collection's info.rkt can say what to clean, for example
# (define clean '("compiled" "doc" "doc/<collect>")) to clean
# generated docs, too.
clean:
	raco setup --fast-clean --pkgs $(PACKAGE-NAME)

# Suitable for both day-to-day dev and CI
test:
	raco test -x -p $(PACKAGE-NAME)

docs:
	raco docs $(PACKAGE-NAME)

profile:
	echo "Profiling order and equivalence relations..."
	raco profile dev/profile/comparable/builtin.rkt | grep "Total cpu time"
	raco profile dev/profile/comparable/relation.rkt | grep "Total cpu time"
	echo "Profiling type transformers..."
	raco profile dev/profile/transform/builtin.rkt | grep "Total cpu time"
	raco profile dev/profile/transform/relation.rkt | grep "Total cpu time"
	echo "Profiling algebraic operators..."
	raco profile dev/profile/algebraic/builtin.rkt | grep "Total cpu time"
	raco profile dev/profile/algebraic/relation.rkt | grep "Total cpu time"

.PHONY:	help install remove build build-docs build-all check-deps clean test docs profile
