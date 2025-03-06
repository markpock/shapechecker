# Compiler settings
CC = gcc
CFLAGS = -Wall -Wextra -pedantic -std=c99 -O2

# Python settings (Python 3.13.1)
PYTHON_VERSION = 3.13
PYTHON_PREFIX = /opt/homebrew/opt/python@3.13/Frameworks/Python.framework/Versions/3.13
PYTHON_INCLUDES = -I$(PYTHON_PREFIX)/include/python$(PYTHON_VERSION)
PYTHON_LIBS = $(PYTHON_PREFIX)/lib/libpython$(PYTHON_VERSION).dylib

# Build directory
BUILD_DIR = ../build

# Library name
LIBRARY_NAME = $(BUILD_DIR)/libpython_adapter.a
DYLLIBRARY_NAME = $(BUILD_DIR)/libpython_adapter.dylib
EXEC_NAME = $(BUILD_DIR)/python_adapter_exec

# Source files
PYTHON_ADAPTER_SRC = PythonAdapter.c

# Object files
PYTHON_ADAPTER_OBJ = $(BUILD_DIR)/$(PYTHON_ADAPTER_SRC:.c=.o)

# Ensure build directory exists
$(shell mkdir -p $(BUILD_DIR))

# All targets
all: $(LIBRARY_NAME)

# Rule for building the static library
$(LIBRARY_NAME): $(PYTHON_ADAPTER_OBJ)
	ar rcs $@ $^ $(PYTHON_LIBS)

# Rule for building object files
$(BUILD_DIR)/%.o: %.c
	$(CC) $(CFLAGS) $(PYTHON_INCLUDES) -c $< -o $@

# Clean target
clean:
	rm -f $(LIBRARY_NAME) $(PYTHON_ADAPTER_OBJ) $(EXEC_NAME) $(DYLLIBRARY_NAME) $(DYLLIBRARY_NAME).*

# Debug target (builds with debug symbols)
debug: CFLAGS += -g -DDEBUG
debug: all

# Documentation target
docs:
	@echo "Generating documentation for Python C API projects..."
	# Add documentation generation commands here if needed

# Help target
help:
	@echo "Available targets:"
	@echo "  all       - Build the static library (default)"
	@echo "  clean     - Remove all built files"
	@echo "  debug     - Build with debug symbols"
	@echo "  docs      - Generate documentation"
	@echo "  help      - Display this help message"

# Phony targets
.PHONY: all clean debug docs help