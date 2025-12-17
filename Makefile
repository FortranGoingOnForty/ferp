# FERP Makefile
# Fortran Expression Regular Print - A GNU grep clone

# Compiler
FC = gfortran

# Compiler flags
FFLAGS_COMMON = -std=f2008 -Wall -Wextra -pedantic
FFLAGS_DEBUG = $(FFLAGS_COMMON) -g -O0 -fcheck=all -fbacktrace -Wno-unused-dummy-argument
FFLAGS_RELEASE = $(FFLAGS_COMMON) -O2 -march=native

# Default to debug build
FFLAGS = $(FFLAGS_DEBUG)

# Directories
SRC_DIR = src
BUILD_DIR = build
BIN_DIR = .

# Target binary
TARGET = $(BIN_DIR)/ferp

# Source files (in dependency order)
SRCS = $(SRC_DIR)/ferp_kinds.f90 \
       $(SRC_DIR)/ferp_options.f90 \
       $(SRC_DIR)/ferp_io.f90 \
       $(SRC_DIR)/ferp_output.f90 \
       $(SRC_DIR)/ferp_cli.f90 \
       $(SRC_DIR)/ferp_matcher.f90 \
       $(SRC_DIR)/main.f90

# Object files
OBJS = $(patsubst $(SRC_DIR)/%.f90,$(BUILD_DIR)/%.o,$(SRCS))

# Module files
MODS = $(BUILD_DIR)/*.mod

# Default target
all: $(TARGET)

# Debug build
debug: FFLAGS = $(FFLAGS_DEBUG)
debug: clean $(TARGET)

# Release build
release: FFLAGS = $(FFLAGS_RELEASE)
release: clean $(TARGET)

# Create build directory
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Link target
$(TARGET): $(BUILD_DIR) $(OBJS)
	$(FC) $(FFLAGS) -o $@ $(OBJS)

# Compile source files
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.f90 | $(BUILD_DIR)
	$(FC) $(FFLAGS) -J$(BUILD_DIR) -c $< -o $@

# Dependencies (module use dependencies)
$(BUILD_DIR)/ferp_options.o: $(BUILD_DIR)/ferp_kinds.o
$(BUILD_DIR)/ferp_io.o: $(BUILD_DIR)/ferp_kinds.o
$(BUILD_DIR)/ferp_output.o: $(BUILD_DIR)/ferp_kinds.o $(BUILD_DIR)/ferp_options.o
$(BUILD_DIR)/ferp_cli.o: $(BUILD_DIR)/ferp_kinds.o $(BUILD_DIR)/ferp_options.o
$(BUILD_DIR)/ferp_matcher.o: $(BUILD_DIR)/ferp_kinds.o $(BUILD_DIR)/ferp_options.o $(BUILD_DIR)/ferp_io.o $(BUILD_DIR)/ferp_output.o
$(BUILD_DIR)/main.o: $(BUILD_DIR)/ferp_kinds.o $(BUILD_DIR)/ferp_options.o $(BUILD_DIR)/ferp_cli.o $(BUILD_DIR)/ferp_io.o $(BUILD_DIR)/ferp_matcher.o

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
	@echo "Running basic tests..."
	@echo "hello world" | ./ferp "hello" && echo "PASS: basic match"
	@echo "hello world" | ./ferp "goodbye" || echo "PASS: no match (exit 1)"
	@echo "hello world" | ./ferp -i "HELLO" && echo "PASS: case insensitive"
	@echo "hello world" | ./ferp -v "goodbye" && echo "PASS: invert match"
	@echo "foo bar baz" | ./ferp -w "bar" && echo "PASS: word match"
	@echo "foobar" | ./ferp -w "bar" || echo "PASS: word no match (exit 1)"
	@echo -e "a\nb\nc" | ./ferp -c "a" | grep -q "1" && echo "PASS: count mode"
	@echo -e "a\nb\nc" | ./ferp -n "b" | grep -q "2:b" && echo "PASS: line numbers"
	@echo "Tests complete!"

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
