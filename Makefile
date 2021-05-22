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
	@echo "test-with-errortrace - Run tests with error tracing"
	@echo "errortrace - Alias for test-with-errortrace"
	@echo "cover - Run test coverage checker and view report"
	@echo "cover-coveralls - Run test coverage and upload to Coveralls"
	@echo "coverage-check - Run test coverage checker"
	@echo "coverage-report - View test coverage report"
	@echo "docs - View docs in a browser"
	@echo "profile - Run benchmarks to gauge relative performance against built-in interfaces"
	@echo "test-<module> - Run tests for <module>"
	@echo "errortrace-<module> - Run tests for <module> with error tracing"
	@echo "profile-<module> - Run benchmarks for <module>"
	@echo "Modules:"
	@echo "  logic"
	@echo "  equivalence"
	@echo "  order"
	@echo "  function"
	@echo "  type"
	@echo "  composition"

# Primarily for use by CI.
# Installs dependencies as well as linking this as a package.
install:
	raco pkg install --deps search-auto

remove:
	raco pkg remove $(PACKAGE-NAME)

# TODO: research difference between raco setup relation
# and raco setup --pkg relation
# Add the former as a make target if warranted, as it seems to
# be necessary in some cases (e.g. linking to third party lib docs)

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
# Note: the removal of compiled test binaries before running tests is
# needed here at the moment because raco setup appears to build the
# test source before the library source. As a result, the compiled
# test binaries reflect the outdated (prior to raco setup) version of
# the library, resulting in an error. Deleting the compiled output
# forces a re-compile at test running time against the current
# installed version.
test:
	raco test -x -p $(PACKAGE-NAME)

test-logic:
	raco test -x tests/logic.rkt

test-equivalence:
	raco test -x tests/equivalence.rkt

test-order:
	raco test -x tests/order.rkt

test-function:
	raco test -x tests/function.rkt

test-type:
	raco test -x tests/type.rkt

test-composition:
	raco test -x tests/composition.rkt

build+test: build test

errortrace-logic:
	racket -l errortrace -l racket -e '(require (submod "tests/logic.rkt" test))'

errortrace-equivalence:
	racket -l errortrace -l racket -e '(require (submod "tests/equivalence.rkt" test))'

errortrace-order:
	racket -l errortrace -l racket -e '(require (submod "tests/order.rkt" test))'

errortrace-function:
	racket -l errortrace -l racket -e '(require (submod "tests/function.rkt" test))'

errortrace-type:
	racket -l errortrace -l racket -e '(require (submod "tests/type.rkt" test))'

errortrace-composition:
	racket -l errortrace -l racket -e '(require (submod "tests/composition.rkt" test))'

test-with-errortrace: errortrace-logic errortrace-equivalence errortrace-order errortrace-function errortrace-type errortrace-composition

errortrace: test-with-errortrace

coverage-check:
	raco cover -b -n dev -n algebraic.rkt -n transform.rkt -p $(PACKAGE-NAME)

coverage-report:
	open coverage/index.html

cover: coverage-check coverage-report

cover-coveralls:
	raco cover -b -n dev -n algebraic.rkt -n transform.rkt -f coveralls -p $(PACKAGE-NAME)

docs:
	raco docs $(PACKAGE-NAME)

profile-logic:
	echo "Profiling logical relations..."
	raco profile dev/profile/logic/builtin.rkt | grep "Total cpu time"
	raco profile dev/profile/logic/relation.rkt | grep "Total cpu time"

profile-equivalence:
	echo "Profiling equivalence relations..."
	raco profile dev/profile/equivalence/builtin.rkt | grep "Total cpu time"
	raco profile dev/profile/equivalence/relation.rkt | grep "Total cpu time"

profile-order:
	echo "Profiling order relations..."
	raco profile dev/profile/order/builtin.rkt | grep "Total cpu time"
	raco profile dev/profile/order/relation.rkt | grep "Total cpu time"

profile-function:
	echo "Profiling functional primitives..."
	raco profile dev/profile/function/builtin.rkt | grep "Total cpu time"
	raco profile dev/profile/function/relation.rkt | grep "Total cpu time"

profile-type:
	echo "Profiling type transformers..."
	raco profile dev/profile/type/builtin.rkt | grep "Total cpu time"
	raco profile dev/profile/type/relation.rkt | grep "Total cpu time"

profile-composition:
	echo "Profiling composition operators..."
	raco profile dev/profile/composition/builtin.rkt | grep "Total cpu time"
	raco profile dev/profile/composition/relation.rkt | grep "Total cpu time"

profile: profile-logic profile-equivalence profile-order profile-function profile-type profile-composition

.PHONY:	help install remove build build-docs build-all check-deps clean test test-logic test-equivalence test-order test-function test-type test-composition errortrace-logic errortrace-equivalence errortrace-order errortrace-function errortrace-type errortrace-composition test-with-errortrace errortrace docs profile-logic profile-equivalence profile-order profile-function profile-type profile-composition profile cover coverage-check coverage-report cover-coveralls
