# FERP Makefile
# Fortran Expression Regular Print - A GNU grep clone

# Compilers
FC = gfortran
CC = clang

# Compiler flags
FFLAGS_COMMON = -std=f2008 -Wall -Wextra -pedantic -cpp
FFLAGS_DEBUG = $(FFLAGS_COMMON) -g -O0 -fcheck=all -fbacktrace
# NOTE: deliberately no -fopenmp. See the warning above the parallel do in
# src/main.f90 -- multi-threaded search corrupts output with this compiler,
# because gfortran keeps the length temporary of every deferred-length
# character assignment in shared static storage. Do not add it back without
# reading that comment.
FFLAGS_RELEASE = $(FFLAGS_COMMON) -O2 -march=native

CFLAGS_DEBUG = -g -O0 -Wall
CFLAGS_RELEASE = -O2 -march=native -Wall

# Default to debug build (release includes OpenMP)
FFLAGS = $(FFLAGS_DEBUG)
CFLAGS = $(CFLAGS_DEBUG)

# PCRE2 library (required for -P option)
# Use pkg-config if available, otherwise fall back to defaults
PCRE2_CFLAGS := $(shell pkg-config --cflags libpcre2-8 2>/dev/null)
PCRE2_LIBS := $(shell pkg-config --libs libpcre2-8 2>/dev/null || echo "-lpcre2-8")
LDFLAGS = $(PCRE2_LIBS)

# Directories
SRC_DIR = src
REGEX_DIR = src/regex
BUILD_DIR = build
BIN_DIR = .

# Target binary
TARGET = $(BIN_DIR)/ferp

# Regex source files (in dependency order)
REGEX_SRCS = $(REGEX_DIR)/regex_charclass.f90 \
             $(REGEX_DIR)/regex_types.f90 \
             $(REGEX_DIR)/regex_lexer.f90 \
             $(REGEX_DIR)/regex_parser.f90 \
             $(REGEX_DIR)/regex_nfa.f90 \
             $(REGEX_DIR)/regex_engine.f90 \
             $(REGEX_DIR)/aho_corasick.f90 \
             $(REGEX_DIR)/regex_optimizer.f90 \
             $(REGEX_DIR)/regex_api.f90 \
             $(REGEX_DIR)/pcre_api.f90

# C source files (SIMD support)
C_SRCS = $(SRC_DIR)/simd_scan.c

# Main source files (in dependency order)
MAIN_SRCS = $(SRC_DIR)/ferp_kinds.f90 \
            $(SRC_DIR)/ferp_options.f90 \
            $(SRC_DIR)/ferp_mmap.f90 \
            $(SRC_DIR)/ferp_simd.f90 \
            $(SRC_DIR)/ferp_io.f90 \
            $(SRC_DIR)/ferp_output.f90 \
            $(SRC_DIR)/ferp_dir.f90 \
            $(SRC_DIR)/ferp_search.f90 \
            $(SRC_DIR)/ferp_cli.f90 \
            $(SRC_DIR)/ferp_matcher.f90 \
            $(SRC_DIR)/main.f90

# All source files
SRCS = $(REGEX_SRCS) $(MAIN_SRCS)

# Object files
REGEX_OBJS = $(patsubst $(REGEX_DIR)/%.f90,$(BUILD_DIR)/%.o,$(REGEX_SRCS))
MAIN_OBJS = $(patsubst $(SRC_DIR)/%.f90,$(BUILD_DIR)/%.o,$(MAIN_SRCS))
C_OBJS = $(patsubst $(SRC_DIR)/%.c,$(BUILD_DIR)/%.o,$(C_SRCS))
OBJS = $(REGEX_OBJS) $(MAIN_OBJS) $(C_OBJS)

# Default target
all: $(TARGET)

# Debug build
debug: FFLAGS = $(FFLAGS_DEBUG)
debug: CFLAGS = $(CFLAGS_DEBUG)
debug: clean $(TARGET)

# Release build
release: FFLAGS = $(FFLAGS_RELEASE)
release: CFLAGS = $(CFLAGS_RELEASE)
release: clean $(TARGET)

# Create build directory
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Link target
$(TARGET): $(BUILD_DIR) $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS) $(LDFLAGS)

# Compile regex source files
$(BUILD_DIR)/%.o: $(REGEX_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

# Compile main source files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -I$(BUILD_DIR) -c $< -o $@

# Compile C source files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Regex module dependencies (note: some depend on ferp_kinds for pattern_len function)
$(BUILD_DIR)/regex_charclass.o:
$(BUILD_DIR)/regex_types.o: $(BUILD_DIR)/regex_charclass.o
$(BUILD_DIR)/regex_lexer.o: $(BUILD_DIR)/regex_types.o $(BUILD_DIR)/ferp_kinds.o
$(BUILD_DIR)/regex_parser.o: $(BUILD_DIR)/regex_types.o
$(BUILD_DIR)/regex_nfa.o: $(BUILD_DIR)/regex_types.o $(BUILD_DIR)/regex_charclass.o $(BUILD_DIR)/regex_parser.o
$(BUILD_DIR)/regex_engine.o: $(BUILD_DIR)/regex_types.o
$(BUILD_DIR)/aho_corasick.o: $(BUILD_DIR)/ferp_kinds.o
$(BUILD_DIR)/regex_optimizer.o: $(BUILD_DIR)/regex_types.o $(BUILD_DIR)/regex_charclass.o $(BUILD_DIR)/aho_corasick.o $(BUILD_DIR)/ferp_kinds.o
$(BUILD_DIR)/regex_api.o: $(BUILD_DIR)/regex_types.o $(BUILD_DIR)/regex_lexer.o $(BUILD_DIR)/regex_parser.o $(BUILD_DIR)/regex_nfa.o $(BUILD_DIR)/regex_engine.o $(BUILD_DIR)/regex_optimizer.o $(BUILD_DIR)/ferp_kinds.o
$(BUILD_DIR)/pcre_api.o: $(BUILD_DIR)/ferp_kinds.o

# Main module dependencies
$(BUILD_DIR)/ferp_options.o: $(BUILD_DIR)/ferp_kinds.o
$(BUILD_DIR)/ferp_simd.o: $(BUILD_DIR)/simd_scan.o
$(BUILD_DIR)/ferp_mmap.o: $(BUILD_DIR)/ferp_kinds.o $(BUILD_DIR)/ferp_simd.o
$(BUILD_DIR)/ferp_io.o: $(BUILD_DIR)/ferp_kinds.o $(BUILD_DIR)/ferp_mmap.o
$(BUILD_DIR)/ferp_output.o: $(BUILD_DIR)/ferp_kinds.o $(BUILD_DIR)/ferp_options.o
$(BUILD_DIR)/ferp_dir.o: $(BUILD_DIR)/ferp_kinds.o
$(BUILD_DIR)/ferp_search.o: $(BUILD_DIR)/ferp_kinds.o
$(BUILD_DIR)/ferp_cli.o: $(BUILD_DIR)/ferp_kinds.o $(BUILD_DIR)/ferp_options.o
$(BUILD_DIR)/ferp_matcher.o: $(BUILD_DIR)/ferp_kinds.o $(BUILD_DIR)/ferp_options.o $(BUILD_DIR)/ferp_io.o $(BUILD_DIR)/ferp_output.o $(BUILD_DIR)/ferp_search.o $(BUILD_DIR)/regex_api.o $(BUILD_DIR)/pcre_api.o
$(BUILD_DIR)/main.o: $(BUILD_DIR)/ferp_kinds.o $(BUILD_DIR)/ferp_options.o $(BUILD_DIR)/ferp_cli.o $(BUILD_DIR)/ferp_io.o $(BUILD_DIR)/ferp_dir.o $(BUILD_DIR)/ferp_matcher.o

# Clean build artifacts
clean:
	rm -rf $(BUILD_DIR)
	rm -f $(TARGET)

# Install (optional)
install: release
	cp $(TARGET) /usr/local/bin/
	ln -sf /usr/local/bin/ferp /usr/local/bin/frep

# Uninstall
uninstall:
	rm -f /usr/local/bin/ferp
	rm -f /usr/local/bin/frep

# Run tests
test: $(TARGET)
	@echo "=== Basic matching tests ==="
	@echo "hello world" | ./ferp "hello" && echo "PASS: literal match"
	@echo "hello world" | ./ferp "goodbye" || echo "PASS: no match (exit 1)"
	@echo "hello world" | ./ferp -i "HELLO" && echo "PASS: case insensitive"
	@echo "hello world" | ./ferp -v "goodbye" && echo "PASS: invert match"
	@echo "=== BRE metacharacter tests ==="
	@echo "hello world" | ./ferp "hel.o" && echo "PASS: dot metachar"
	@echo "helllo" | ./ferp "hel*o" && echo "PASS: star quantifier"
	@echo "heo" | ./ferp "hel*o" && echo "PASS: star zero matches"
	@echo "hello" | ./ferp "^hello" && echo "PASS: start anchor"
	@echo "hello" | ./ferp "hello$$" && echo "PASS: end anchor"
	@echo "hello" | ./ferp "^hello$$" && echo "PASS: both anchors"
	@echo "=== Character class tests ==="
	@echo "abc" | ./ferp "[abc]" && echo "PASS: char class"
	@echo "abc" | ./ferp "[a-z]" && echo "PASS: char range"
	@echo "ABC" | ./ferp "[A-Z]" && echo "PASS: uppercase range"
	@echo "123" | ./ferp "[0-9]" && echo "PASS: digit range"
	@echo "!" | ./ferp "[^a-z]" && echo "PASS: negated class"
	@echo "]test" | ./ferp "[]a-z]" && echo "PASS: ] at class start"
	@echo "-test" | ./ferp "[-az]" && echo "PASS: - at class start"
	@echo "test-" | ./ferp "[az-]" && echo "PASS: - at class end"
	@echo "=== POSIX class tests ==="
	@echo "abc" | ./ferp "[[:alpha:]]" && echo "PASS: [:alpha:]"
	@echo "123" | ./ferp "[[:digit:]]" && echo "PASS: [:digit:]"
	@echo "abc123" | ./ferp "[[:alnum:]]" && echo "PASS: [:alnum:]"
	@echo " 	" | ./ferp "[[:space:]]" && echo "PASS: [:space:]"
	@echo "=== BRE grouping and quantifier tests ==="
	@echo "abab" | ./ferp '\(ab\)*' && echo "PASS: BRE group with star"
	@echo "aaa" | ./ferp 'a\{2,3\}' && echo "PASS: BRE bounded {2,3}"
	@echo "aa" | ./ferp 'a\{2\}' && echo "PASS: BRE exact {2}"
	@echo "aaaa" | ./ferp 'a\{2,\}' && echo "PASS: BRE minimum {2,}"
	@echo "=== Word boundary tests ==="
	@echo "hello world" | ./ferp '\<hello' && echo "PASS: word start \\<"
	@echo "hello world" | ./ferp 'world\>' && echo "PASS: word end \\>"
	@echo "hello world" | ./ferp '\<world\>' && echo "PASS: word boundaries"
	@echo "=== ERE tests ==="
	@echo "hello" | ./ferp -E "hel+" && echo "PASS: ERE plus"
	@echo "helo" | ./ferp -E "hel?o" && echo "PASS: ERE question"
	@echo "heo" | ./ferp -E "hel?o" && echo "PASS: ERE question zero"
	@echo "cat" | ./ferp -E "cat|dog" && echo "PASS: ERE alternation"
	@echo "dog" | ./ferp -E "cat|dog" && echo "PASS: ERE alternation 2"
	@echo "ab" | ./ferp -E "(ab)+" && echo "PASS: ERE group with plus"
	@echo "abab" | ./ferp -E "(ab){2}" && echo "PASS: ERE bounded {2}"
	@echo "aaa" | ./ferp -E "a{2,3}" && echo "PASS: ERE bounded {2,3}"
	@echo "=== Output option tests ==="
	@echo "hello world" | ./ferp -o "wor" | grep -q "^wor$$" && echo "PASS: -o only matching"
	@echo "hello world hello" | ./ferp -o "hello" | wc -l | grep -q "2" && echo "PASS: -o multiple matches"
	@printf "hello\nworld\n" | ./ferp -b "world" | grep -q "^6:" && echo "PASS: -b byte offset"
	@echo "=== Context line tests ==="
	@printf "a\nb\nmatch\nd\ne\n" | ./ferp -A 2 "match" | wc -l | grep -q "3" && echo "PASS: -A after context"
	@printf "a\nb\nmatch\nd\ne\n" | ./ferp -B 2 "match" | wc -l | grep -q "3" && echo "PASS: -B before context"
	@printf "a\nb\nmatch\nd\ne\n" | ./ferp -C 1 "match" | wc -l | grep -q "3" && echo "PASS: -C both context"
	@printf "a\nb\nmatch\nd\ne\n" | ./ferp -2 "match" | wc -l | grep -q "5" && echo "PASS: -NUM context shorthand"
	@echo "=== Edge case tests ==="
	@echo "" | ./ferp "" && echo "PASS: empty pattern on empty"
	@echo "hello" | ./ferp "" && echo "PASS: empty pattern matches"
	@echo "a+b" | ./ferp "a+b" && echo "PASS: BRE + is literal"
	@echo "a|b" | ./ferp "a|b" && echo "PASS: BRE | is literal"
	@echo "=== Recursive search tests ==="
	@./ferp -r "module" src/ | grep -q "ferp_kinds" && echo "PASS: -r recursive search"
	@./ferp -r "module" src/ | grep -q "regex_types" && echo "PASS: -r searches subdirs"
	@./ferp -r --include="*.f90" "module" src/ | grep -q "ferp_kinds" && echo "PASS: --include filter"
	@./ferp -r --exclude="*output*" "module" src/ | grep -qv "ferp_output" && echo "PASS: --exclude filter"
	@./ferp -r --exclude-dir="regex" "module" src/ | grep -qv "regex_types" && echo "PASS: --exclude-dir filter"
	@echo "=== Binary file tests ==="
	@printf 'hello\x00world\n' > /tmp/ferp_binary_test.txt
	@./ferp "hello" /tmp/ferp_binary_test.txt | grep -q "Binary file" && echo "PASS: binary file detection"
	@./ferp -a "hello" /tmp/ferp_binary_test.txt | grep -q "hello" && echo "PASS: -a treats binary as text"
	@./ferp -I "hello" /tmp/ferp_binary_test.txt; test $$? -eq 1 && echo "PASS: -I skips binary"
	@rm -f /tmp/ferp_binary_test.txt
	@echo "=== Color mode tests ==="
	@./ferp --color=always "module" src/ferp_kinds.f90 | grep -q '\[01;31m' && echo "PASS: --color=always outputs ANSI"
	@./ferp --color=never "module" src/ferp_kinds.f90 | grep -qv '\[01;31m' && echo "PASS: --color=never no ANSI"
	@./ferp --color=auto "module" src/ferp_kinds.f90 | grep -qv '\[01;31m' && echo "PASS: --color=auto no ANSI when piped"
	@echo "=== PCRE tests ==="
	@echo "hello world" | ./ferp -P "hello" | grep -q "hello" && echo "PASS: -P basic match"
	@echo "foobar" | ./ferp -P 'foo(?=bar)' | grep -q "foobar" && echo "PASS: -P positive lookahead"
	@echo "foobaz" | ./ferp -P 'foo(?!bar)' | grep -q "foobaz" && echo "PASS: -P negative lookahead"
	@echo "foobar" | ./ferp -P '(?<=foo)bar' | grep -q "foobar" && echo "PASS: -P positive lookbehind"
	@echo "bazbar" | ./ferp -P '(?<!foo)bar' | grep -q "bazbar" && echo "PASS: -P negative lookbehind"
	@echo "abcabc" | ./ferp -P '(?:abc)+' | grep -q "abcabc" && echo "PASS: -P non-capturing group"
	@echo "test123" | ./ferp -P '\d+' | grep -q "test123" && echo "PASS: -P digit class"
	@echo "test123more456" | ./ferp -P -o '\d+' | head -1 | grep -q "123" && echo "PASS: -P -o only matching"
	@echo "HELLO" | ./ferp -P -i 'hello' | grep -q "HELLO" && echo "PASS: -P -i case insensitive"
	@echo "Hello123" | ./ferp -P -o '\p{L}+' | grep -q "Hello" && echo "PASS: -P Unicode \\p{L} letter class"
	@echo "Hello123" | ./ferp -P -o '\p{N}+' | grep -q "123" && echo "PASS: -P Unicode \\p{N} number class"
	@echo "café" | ./ferp -P '\p{L}+' | grep -q "café" && echo "PASS: -P Unicode accented chars"
	@echo "CAFÉ" | ./ferp -P -i 'café' | grep -q "CAFÉ" && echo "PASS: -P Unicode case folding"
	@echo "=== All tests complete! ==="

# Help
help:
	@echo "FERP Makefile targets:"
	@echo "  all      - Build ferp (debug mode)"
	@echo "  debug    - Build with debug flags"
	@echo "  release  - Build with optimization"
	@echo "  test     - Run basic tests"
	@echo "  clean    - Remove build artifacts"
	@echo "  install  - Install to /usr/local/bin"
	@echo "  uninstall- Remove from /usr/local/bin"
	@echo "  help     - Show this message"

.PHONY: all debug release clean install uninstall test help
