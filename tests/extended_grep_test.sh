#!/bin/bash
#
# FERP Extended Grep Test Suite
# Tests edge cases, corner cases, and obscure grep behaviors
#
# This test suite covers:
# 1. Multiple pattern flags (-e, -f)
# 2. Backreferences in BRE
# 3. Null-data mode (-z)
# 4. Binary file handling
# 5. Recursive directory options
# 6. Context edge cases
# 7. Output format options (-T, -Z, --label)
# 8. Regex edge cases (empty matches, unicode, long patterns)
# 9. BRE vs ERE differences
# 10. Error handling scenarios
# 11. Unusual input (long lines, mixed line endings, etc.)
# 12. Flag interaction edge cases
# 13. PCRE-specific features (-P)
#
# Usage: ./extended_grep_test.sh [--verbose] [--stop-on-fail] [--filter PATTERN]
#

set -uo pipefail

#------------------------------------------------------------------------------
# Configuration
#------------------------------------------------------------------------------

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FERP="${SCRIPT_DIR}/../ferp"
FIXTURES="${SCRIPT_DIR}/extended_fixtures"

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_SKIPPED=0

# Options
VERBOSE=false
STOP_ON_FAIL=false
FILTER=""

# Track failures for summary
declare -a FAILED_TESTS=()

#------------------------------------------------------------------------------
# Argument Parsing
#------------------------------------------------------------------------------

while [[ $# -gt 0 ]]; do
    case $1 in
        --verbose|-v)
            VERBOSE=true
            shift
            ;;
        --stop-on-fail|-x)
            STOP_ON_FAIL=true
            shift
            ;;
        --filter|-f)
            FILTER="$2"
            shift 2
            ;;
        --help|-h)
            echo "Usage: $0 [--verbose] [--stop-on-fail] [--filter PATTERN]"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

#------------------------------------------------------------------------------
# Test Framework
#------------------------------------------------------------------------------

log() {
    if [[ "$VERBOSE" == "true" ]]; then
        echo -e "$1"
    fi
}

section() {
    echo ""
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}  $1${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

pass() {
    ((TESTS_PASSED++))
    ((TESTS_RUN++))
    echo -e "${GREEN}PASS${NC}: $1"
}

fail() {
    local name="$1"
    local reason="${2:-}"
    ((TESTS_FAILED++))
    ((TESTS_RUN++))
    echo -e "${RED}FAIL${NC}: $name"
    if [[ -n "$reason" ]]; then
        echo -e "      ${YELLOW}Reason:${NC} $reason"
    fi
    FAILED_TESTS+=("$name")
    if [[ "$STOP_ON_FAIL" == "true" ]]; then
        echo -e "${RED}Stopping on first failure${NC}"
        print_summary
        exit 1
    fi
}

skip() {
    local name="$1"
    local reason="${2:-}"
    ((TESTS_SKIPPED++))
    echo -e "${YELLOW}SKIP${NC}: $name${reason:+ ($reason)}"
}

should_run() {
    local name="$1"
    if [[ -n "$FILTER" && ! "$name" =~ $FILTER ]]; then
        return 1
    fi
    return 0
}

#------------------------------------------------------------------------------
# Core Comparison Function
#------------------------------------------------------------------------------

compare_with_grep() {
    local flags="$1"
    local pattern="$2"
    local file="$3"

    local grep_out grep_exit ferp_out ferp_exit

    grep_out=$(grep $flags -- "$pattern" "$file" 2>/dev/null) && grep_exit=0 || grep_exit=$?
    ferp_out=$("$FERP" $flags -- "$pattern" "$file" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?

    if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
        return 0
    else
        if [[ "$VERBOSE" == "true" ]]; then
            echo "  grep output ($grep_exit): $(echo "$grep_out" | head -3 | cat -A)"
            echo "  ferp output ($ferp_exit): $(echo "$ferp_out" | head -3 | cat -A)"
        fi
        return 1
    fi
}

compare_with_grep_stdin() {
    local flags="$1"
    local pattern="$2"
    local input="$3"

    local grep_out grep_exit ferp_out ferp_exit

    grep_out=$(printf '%s' "$input" | grep $flags -- "$pattern" 2>/dev/null) && grep_exit=0 || grep_exit=$?
    ferp_out=$(printf '%s' "$input" | "$FERP" $flags -- "$pattern" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?

    if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
        return 0
    else
        if [[ "$VERBOSE" == "true" ]]; then
            echo "  grep output ($grep_exit): $(echo "$grep_out" | head -3 | cat -A)"
            echo "  ferp output ($ferp_exit): $(echo "$ferp_out" | head -3 | cat -A)"
        fi
        return 1
    fi
}

test_grep_compat() {
    local name="$1"
    local flags="$2"
    local pattern="$3"
    local file="$4"

    should_run "$name" || return 0

    log "${CYAN}Testing:${NC} $name"
    log "  Command: grep $flags -- '$pattern' $file"

    if compare_with_grep "$flags" "$pattern" "$file" "$name"; then
        pass "$name"
    else
        fail "$name" "Output differs from grep"
    fi
}

test_grep_compat_stdin() {
    local name="$1"
    local flags="$2"
    local pattern="$3"
    local input="$4"

    should_run "$name" || return 0

    log "${CYAN}Testing:${NC} $name"

    if compare_with_grep_stdin "$flags" "$pattern" "$input" "$name"; then
        pass "$name"
    else
        fail "$name" "Output differs from grep"
    fi
}

# Compare with multiple files
compare_multi_files() {
    local name="$1"
    local flags="$2"
    local pattern="$3"
    shift 3
    local files=("$@")

    should_run "$name" || return 0

    log "${CYAN}Testing:${NC} $name"

    local grep_out grep_exit ferp_out ferp_exit

    grep_out=$(grep $flags -- "$pattern" "${files[@]}" 2>/dev/null) && grep_exit=0 || grep_exit=$?
    ferp_out=$("$FERP" $flags -- "$pattern" "${files[@]}" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?

    if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
        pass "$name"
    else
        fail "$name" "Output differs from grep"
        if [[ "$VERBOSE" == "true" ]]; then
            echo "  grep ($grep_exit): $(echo "$grep_out" | head -3)"
            echo "  ferp ($ferp_exit): $(echo "$ferp_out" | head -3)"
        fi
    fi
}

#------------------------------------------------------------------------------
# Fixture Generation
#------------------------------------------------------------------------------

generate_fixtures() {
    echo -e "${BLUE}Generating extended test fixtures...${NC}"

    rm -rf "$FIXTURES"
    mkdir -p "$FIXTURES"
    mkdir -p "$FIXTURES/subdir"
    mkdir -p "$FIXTURES/recursive/level1/level2"

    # Basic test file
    cat > "$FIXTURES/basic.txt" << 'EOF'
hello world
Hello World
HELLO WORLD
goodbye world
the quick brown fox
testing 123 testing
line with hello in middle
EOF

    # File for backreference tests
    cat > "$FIXTURES/backref.txt" << 'EOF'
aa
bb
ab
aaa
abba
abab
noon
deed
level
hello
abcabc
EOF

    # File for multiple pattern tests
    cat > "$FIXTURES/multi_pattern.txt" << 'EOF'
apple pie
banana bread
cherry cake
apple sauce
grape juice
banana split
EOF

    # Pattern file for -f flag
    cat > "$FIXTURES/patterns.txt" << 'EOF'
apple
cherry
grape
EOF

    # File with special characters
    cat > "$FIXTURES/special.txt" << 'EOF'
price is $100
50% off
path/to/file
array[0]
func()
a+b=c
a*b
hello.world
start^here
end$there
back\slash
pipe|char
question?
curly{brace}
(parens)
EOF

    # Unicode test file
    cat > "$FIXTURES/unicode.txt" << 'EOF'
cafe
café
résumé
naïve
NAÏVE
Ñoño
日本語
EOF

    # File with various line endings
    printf 'unix line\n' > "$FIXTURES/line_endings.txt"
    printf 'windows line\r\n' >> "$FIXTURES/line_endings.txt"
    printf 'old mac line\r' >> "$FIXTURES/line_endings.txt"
    printf 'final line\n' >> "$FIXTURES/line_endings.txt"

    # File for context tests (overlapping matches)
    cat > "$FIXTURES/context.txt" << 'EOF'
line 1
line 2
MATCH A
line 4
MATCH B
line 6
line 7
line 8
MATCH C
line 10
EOF

    # File with empty lines
    cat > "$FIXTURES/empty_lines.txt" << 'EOF'
first

second

third
EOF

    # File with only whitespace lines
    cat > "$FIXTURES/whitespace_lines.txt" << 'EOF'
normal line


normal again
EOF

    # Very long line
    printf 'start ' > "$FIXTURES/longline.txt"
    printf 'x%.0s' {1..5000} >> "$FIXTURES/longline.txt"
    printf ' MATCH ' >> "$FIXTURES/longline.txt"
    printf 'y%.0s' {1..5000} >> "$FIXTURES/longline.txt"
    printf ' end\n' >> "$FIXTURES/longline.txt"

    # Binary file with text
    printf 'text before\x00binary\x00text after\nmore text\n' > "$FIXTURES/binary.bin"

    # File for -T tab alignment tests
    cat > "$FIXTURES/tabs.txt" << 'EOF'
short	match here
verylongfilename	match here
x	match here
EOF

    # Files for recursive tests
    echo "match in root" > "$FIXTURES/recursive/root.txt"
    echo "match in level1" > "$FIXTURES/recursive/level1/file1.txt"
    echo "no match here" > "$FIXTURES/recursive/level1/file2.txt"
    echo "match in level2" > "$FIXTURES/recursive/level1/level2/deep.txt"
    echo "match in c file" > "$FIXTURES/recursive/code.c"
    echo "match in header" > "$FIXTURES/recursive/code.h"
    echo "compiled code" > "$FIXTURES/recursive/code.o"

    # Symlink for -R tests (if supported)
    ln -sf "$FIXTURES/basic.txt" "$FIXTURES/recursive/link_to_basic.txt" 2>/dev/null || true

    # Empty file
    touch "$FIXTURES/empty.txt"

    # Single line, no newline
    printf 'no trailing newline' > "$FIXTURES/no_newline.txt"

    # File with many matches per line
    echo "match match match match match" > "$FIXTURES/many_matches.txt"

    # Null-separated data
    printf 'record1\0record2\0record3\0' > "$FIXTURES/null_data.txt"

    # Multiple files for multi-file tests
    echo "has pattern here" > "$FIXTURES/multi1.txt"
    echo "nothing special" > "$FIXTURES/multi2.txt"
    echo "pattern again" > "$FIXTURES/multi3.txt"

    echo -e "${GREEN}Generated extended fixtures in $FIXTURES${NC}"
}

#------------------------------------------------------------------------------
# Test: Multiple Pattern Flags (-e, -f)
#------------------------------------------------------------------------------

test_multiple_patterns() {
    section "Multiple Pattern Flags (-e, -f)"

    local name

    # -e with single pattern (should work like no -e)
    test_grep_compat "-e: single pattern" "-e hello" "hello" "$FIXTURES/basic.txt"

    # -e with multiple patterns
    name="-e: two patterns"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -e "hello" -e "goodbye" "$FIXTURES/basic.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -e "hello" -e "goodbye" "$FIXTURES/basic.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
            log "  grep: $grep_out"
            log "  ferp: $ferp_out"
        fi
    }

    # -e with three patterns
    name="-e: three patterns"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -e "apple" -e "cherry" -e "grape" "$FIXTURES/multi_pattern.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -e "apple" -e "cherry" -e "grape" "$FIXTURES/multi_pattern.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # -e with -i (case insensitive)
    name="-e: with -i flag"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -i -e "HELLO" -e "GOODBYE" "$FIXTURES/basic.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -i -e "HELLO" -e "GOODBYE" "$FIXTURES/basic.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # -f: patterns from file
    name="-f: patterns from file"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -f "$FIXTURES/patterns.txt" "$FIXTURES/multi_pattern.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -f "$FIXTURES/patterns.txt" "$FIXTURES/multi_pattern.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # -f with -i
    name="-f: with -i flag"
    should_run "$name" && {
        # Create uppercase patterns file
        echo -e "APPLE\nCHERRY" > "$FIXTURES/patterns_upper.txt"
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -if "$FIXTURES/patterns_upper.txt" "$FIXTURES/multi_pattern.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -if "$FIXTURES/patterns_upper.txt" "$FIXTURES/multi_pattern.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # -e and -f combined
    name="-e and -f combined"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -e "banana" -f "$FIXTURES/patterns.txt" "$FIXTURES/multi_pattern.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -e "banana" -f "$FIXTURES/patterns.txt" "$FIXTURES/multi_pattern.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # -f with empty pattern file
    name="-f: empty pattern file"
    should_run "$name" && {
        touch "$FIXTURES/empty_patterns.txt"
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -f "$FIXTURES/empty_patterns.txt" "$FIXTURES/basic.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -f "$FIXTURES/empty_patterns.txt" "$FIXTURES/basic.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Exit codes differ: grep=$grep_exit ferp=$ferp_exit"
        fi
    }
}

#------------------------------------------------------------------------------
# Test: Backreferences (BRE)
#------------------------------------------------------------------------------

test_backreferences() {
    section "Backreferences (BRE)"

    # Simple backreference - doubled character
    test_grep_compat "backref: doubled char" "" '\(.\)\1' "$FIXTURES/backref.txt"

    # Backreference - doubled lowercase letter
    test_grep_compat "backref: doubled lowercase" "" '\([a-z]\)\1' "$FIXTURES/backref.txt"

    # Palindrome-like pattern
    test_grep_compat "backref: abba pattern" "" '\(.\)\(.\)\2\1' "$FIXTURES/backref.txt"

    # Backreference with quantifier before group
    test_grep_compat "backref: group with star" "" '\(ab\)*\1' "$FIXTURES/backref.txt"

    # Multiple groups
    test_grep_compat "backref: two groups" "" '\(a\)\(b\)\1\2' "$FIXTURES/backref.txt"

    # Backreference at word boundary
    test_grep_compat "backref: with word boundary" "-w" '\([a-z]\)\1' "$FIXTURES/backref.txt"

    # Backreference with -i (case insensitive)
    test_grep_compat "backref: case insensitive" "-i" '\([a-z]\)\1' "$FIXTURES/backref.txt"

    # Repeated group captures last match
    test_grep_compat_stdin "backref: repeated group" "" '\(ab*\)*\1' $'ababbabb\nababbab\n'
}

#------------------------------------------------------------------------------
# Test: Null-data Mode (-z)
#------------------------------------------------------------------------------

test_null_data() {
    section "Null-data Mode (-z)"

    local name

    # Basic null-terminated matching
    name="-z: basic null-terminated"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(printf 'foo\0bar\0baz\0' | grep -z 'bar' 2>/dev/null | cat -A) && grep_exit=0 || grep_exit=$?
        ferp_out=$(printf 'foo\0bar\0baz\0' | "$FERP" -z 'bar' 2>/dev/null | cat -A) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
            log "  grep: $grep_out"
            log "  ferp: $ferp_out"
        fi
    }

    # -z with multiple matches
    name="-z: multiple matches"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(printf 'match1\0nomatch\0match2\0' | grep -z 'match' 2>/dev/null | cat -A) && grep_exit=0 || grep_exit=$?
        ferp_out=$(printf 'match1\0nomatch\0match2\0' | "$FERP" -z 'match' 2>/dev/null | cat -A) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # -z with -c (count)
    name="-z: with count"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(printf 'match\0nomatch\0match\0' | grep -zc 'match' 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$(printf 'match\0nomatch\0match\0' | "$FERP" -zc 'match' 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs: grep='$grep_out' ferp='$ferp_out'"
        fi
    }

    # -z with -v (invert)
    name="-z: with invert"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(printf 'match\0nomatch\0other\0' | grep -zv 'match' 2>/dev/null | cat -A) && grep_exit=0 || grep_exit=$?
        ferp_out=$(printf 'match\0nomatch\0other\0' | "$FERP" -zv 'match' 2>/dev/null | cat -A) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }
}

#------------------------------------------------------------------------------
# Test: Binary File Handling
#------------------------------------------------------------------------------

test_binary_files() {
    section "Binary File Handling"

    local name

    # Default behavior with binary file
    name="binary: default behavior"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep 'text' "$FIXTURES/binary.bin" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" 'text' "$FIXTURES/binary.bin" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        # Both should either show "Binary file matches" or similar
        if [[ "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Exit codes differ: grep=$grep_exit ferp=$ferp_exit"
        fi
    }

    # -a/--text: treat as text
    name="binary: -a treat as text"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -a 'text' "$FIXTURES/binary.bin" 2>/dev/null | head -1) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -a 'text' "$FIXTURES/binary.bin" 2>/dev/null | head -1) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Exit codes differ: grep=$grep_exit ferp=$ferp_exit"
        fi
    }

    # -I: ignore binary files
    name="binary: -I ignore binary"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -I 'text' "$FIXTURES/binary.bin" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -I 'text' "$FIXTURES/binary.bin" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # --binary-files=without-match
    name="binary: --binary-files=without-match"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep --binary-files=without-match 'text' "$FIXTURES/binary.bin" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" --binary-files=without-match 'text' "$FIXTURES/binary.bin" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }
}

#------------------------------------------------------------------------------
# Test: Recursive Directory Options
#------------------------------------------------------------------------------

test_recursive() {
    section "Recursive Directory Options"

    local name

    # -r: recursive search
    name="-r: basic recursive"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -r 'match' "$FIXTURES/recursive" 2>/dev/null | sort) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -r 'match' "$FIXTURES/recursive" 2>/dev/null | sort) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
            log "  grep: $grep_out"
            log "  ferp: $ferp_out"
        fi
    }

    # -r with -l
    name="-r: with -l list files"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -rl 'match' "$FIXTURES/recursive" 2>/dev/null | sort) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -rl 'match' "$FIXTURES/recursive" 2>/dev/null | sort) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # -r with -c
    name="-r: with -c count"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -rc 'match' "$FIXTURES/recursive" 2>/dev/null | sort) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -rc 'match' "$FIXTURES/recursive" 2>/dev/null | sort) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # --include pattern
    name="--include: only .c files"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -r --include="*.c" 'match' "$FIXTURES/recursive" 2>/dev/null | sort) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -r --include="*.c" 'match' "$FIXTURES/recursive" 2>/dev/null | sort) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # --exclude pattern
    name="--exclude: skip .o files"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -r --exclude="*.o" 'match\|compiled' "$FIXTURES/recursive" 2>/dev/null | sort) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -r --exclude="*.o" 'match\|compiled' "$FIXTURES/recursive" 2>/dev/null | sort) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # --exclude-dir
    name="--exclude-dir: skip level2"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -r --exclude-dir="level2" 'match' "$FIXTURES/recursive" 2>/dev/null | sort) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -r --exclude-dir="level2" 'match' "$FIXTURES/recursive" 2>/dev/null | sort) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # Multiple --include patterns
    name="--include: multiple patterns"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -r --include="*.c" --include="*.h" 'match' "$FIXTURES/recursive" 2>/dev/null | sort) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -r --include="*.c" --include="*.h" 'match' "$FIXTURES/recursive" 2>/dev/null | sort) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }
}

#------------------------------------------------------------------------------
# Test: Context Edge Cases
#------------------------------------------------------------------------------

test_context_edge_cases() {
    section "Context Edge Cases"

    # Overlapping context (matches close together)
    test_grep_compat "context: overlapping -C1" "-C1" "MATCH" "$FIXTURES/context.txt"
    test_grep_compat "context: overlapping -C2" "-C2" "MATCH" "$FIXTURES/context.txt"

    # Context at file boundaries
    test_grep_compat "context: at start -B3" "-B3" "line 1" "$FIXTURES/context.txt"
    test_grep_compat "context: at end -A3" "-A3" "line 10" "$FIXTURES/context.txt"

    # Context with -v (inverted)
    test_grep_compat "context: -A2 with -v" "-A2 -v" "MATCH" "$FIXTURES/context.txt"
    test_grep_compat "context: -B2 with -v" "-B2 -v" "MATCH" "$FIXTURES/context.txt"

    # --group-separator
    local name="context: custom group separator"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep --group-separator="===" -C1 "MATCH" "$FIXTURES/context.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" --group-separator="===" -C1 "MATCH" "$FIXTURES/context.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # --no-group-separator
    name="context: no group separator"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep --no-group-separator -C1 "MATCH" "$FIXTURES/context.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" --no-group-separator -C1 "MATCH" "$FIXTURES/context.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # Context with -n (line numbers)
    test_grep_compat "context: -C2 with -n" "-C2 -n" "MATCH" "$FIXTURES/context.txt"

    # Context with -b (byte offset)
    test_grep_compat "context: -C1 with -b" "-C1 -b" "MATCH" "$FIXTURES/context.txt"
}

#------------------------------------------------------------------------------
# Test: Output Format Options
#------------------------------------------------------------------------------

test_output_format() {
    section "Output Format Options"

    # -T: initial tab for alignment
    test_grep_compat "-T: initial tab" "-T" "match" "$FIXTURES/tabs.txt"
    test_grep_compat "-T: with -n" "-Tn" "match" "$FIXTURES/tabs.txt"
    test_grep_compat "-T: with -H" "-TH" "match" "$FIXTURES/tabs.txt"

    # -Z: null after filename
    local name="-Z: null after filename"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -Z "pattern" "$FIXTURES/multi1.txt" 2>/dev/null | cat -A) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -Z "pattern" "$FIXTURES/multi1.txt" 2>/dev/null | cat -A) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # -Z with -l
    name="-Z: with -l"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -lZ "pattern" "$FIXTURES/multi1.txt" "$FIXTURES/multi3.txt" 2>/dev/null | cat -A) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -lZ "pattern" "$FIXTURES/multi1.txt" "$FIXTURES/multi3.txt" 2>/dev/null | cat -A) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # --label for stdin
    name="--label: custom stdin label"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(echo "hello world" | grep -H --label="MYSTDIN" "hello" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$(echo "hello world" | "$FERP" -H --label="MYSTDIN" "hello" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs: grep='$grep_out' ferp='$ferp_out'"
        fi
    }
}

#------------------------------------------------------------------------------
# Test: Regex Edge Cases
#------------------------------------------------------------------------------

test_regex_edge_cases() {
    section "Regex Edge Cases"

    # Empty pattern matches everything
    test_grep_compat "regex: empty pattern" "" "" "$FIXTURES/basic.txt"

    # Pattern matching empty string
    test_grep_compat "regex: a* matches empty" "-E" "a*" "$FIXTURES/basic.txt"

    # Empty line matching
    test_grep_compat "regex: match empty lines" "" "^$" "$FIXTURES/empty_lines.txt"

    # Very long line
    test_grep_compat "regex: very long line" "" "MATCH" "$FIXTURES/longline.txt"

    # Many matches per line with -o
    test_grep_compat "regex: many matches -o" "-o" "match" "$FIXTURES/many_matches.txt"

    # Unicode patterns (if supported)
    test_grep_compat "regex: unicode literal" "" "café" "$FIXTURES/unicode.txt"
    test_grep_compat "regex: unicode case -i" "-i" "CAFÉ" "$FIXTURES/unicode.txt"

    # Anchors with -o
    test_grep_compat "regex: ^ anchor with -o" "-o" "^hello" "$FIXTURES/basic.txt"

    # Newline in character class (should not match)
    test_grep_compat_stdin "regex: dot doesn't match newline" "" "a.b" $'a\nb\nacb\n'

    # Greedy vs non-greedy (ERE)
    test_grep_compat_stdin "regex: greedy quantifier" "-Eo" "a.*b" "aXXbYYb"
}

#------------------------------------------------------------------------------
# Test: BRE vs ERE Differences
#------------------------------------------------------------------------------

test_bre_ere_differences() {
    section "BRE vs ERE Differences"

    # BRE: + and ? are literal
    test_grep_compat "BRE: + is literal" "" "a+b" "$FIXTURES/special.txt"
    test_grep_compat "BRE: ? is literal" "" "question?" "$FIXTURES/special.txt"

    # BRE: | is literal (not alternation)
    test_grep_compat "BRE: | is literal" "" "pipe|char" "$FIXTURES/special.txt"

    # BRE: () need escaping for grouping
    test_grep_compat "BRE: escaped parens group" "" '\(hello\)' "$FIXTURES/basic.txt"

    # BRE: {} need escaping for quantifiers
    test_grep_compat "BRE: escaped braces quantify" "" 'l\{2\}' "$FIXTURES/basic.txt"

    # ERE: + and ? are special
    test_grep_compat "ERE: + is quantifier" "-E" "hel+" "$FIXTURES/basic.txt"
    test_grep_compat "ERE: ? is quantifier" "-E" "hell?o" "$FIXTURES/basic.txt"

    # ERE: | is alternation
    test_grep_compat "ERE: | is alternation" "-E" "hello|goodbye" "$FIXTURES/basic.txt"

    # ERE: () don't need escaping
    test_grep_compat "ERE: unescaped parens" "-E" "(hello)" "$FIXTURES/basic.txt"

    # ERE: {} don't need escaping
    test_grep_compat "ERE: unescaped braces" "-E" "l{2}" "$FIXTURES/basic.txt"

    # BRE with GNU extensions: \| for alternation
    test_grep_compat "BRE GNU: \\| alternation" "" 'hello\|goodbye' "$FIXTURES/basic.txt"

    # BRE with GNU extensions: \+ and \?
    test_grep_compat "BRE GNU: \\+ quantifier" "" 'hel\+' "$FIXTURES/basic.txt"
    test_grep_compat "BRE GNU: \\? quantifier" "" 'hell\?' "$FIXTURES/basic.txt"
}

#------------------------------------------------------------------------------
# Test: Error Handling
#------------------------------------------------------------------------------

test_error_handling() {
    section "Error Handling"

    local name grep_exit ferp_exit

    # Invalid regex
    name="error: invalid regex ERE"
    should_run "$name" && {
        grep -E "[" "$FIXTURES/basic.txt" 2>/dev/null && grep_exit=0 || grep_exit=$?
        "$FERP" -E "[" "$FIXTURES/basic.txt" 2>/dev/null && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_exit" == "2" && "$ferp_exit" == "2" ]]; then
            pass "$name"
        else
            fail "$name" "Exit codes differ: grep=$grep_exit ferp=$ferp_exit"
        fi
    }

    # Invalid regex BRE
    name="error: invalid regex BRE"
    should_run "$name" && {
        grep '\(' "$FIXTURES/basic.txt" 2>/dev/null && grep_exit=0 || grep_exit=$?
        "$FERP" '\(' "$FIXTURES/basic.txt" 2>/dev/null && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_exit" == "2" && "$ferp_exit" == "2" ]]; then
            pass "$name"
        else
            fail "$name" "Exit codes differ: grep=$grep_exit ferp=$ferp_exit"
        fi
    }

    # Non-existent file (exit 2)
    name="error: non-existent file"
    should_run "$name" && {
        local grep_err ferp_err
        grep "pattern" "/nonexistent/file/12345" 2>/dev/null && grep_exit=0 || grep_exit=$?
        "$FERP" "pattern" "/nonexistent/file/12345" 2>/dev/null && ferp_exit=0 || ferp_exit=$?
        # grep returns 2 for errors, ferp should too
        if [[ "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Exit codes differ: grep=$grep_exit ferp=$ferp_exit"
        fi
    }

    # Mix of existing and non-existing files
    name="error: partial file access"
    should_run "$name" && {
        local grep_out ferp_out
        grep_out=$(grep "hello" "$FIXTURES/basic.txt" "/nonexistent" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" "hello" "$FIXTURES/basic.txt" "/nonexistent" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        # Should still find matches in existing file
        if [[ -n "$grep_out" && -n "$ferp_out" ]]; then
            pass "$name"
        else
            fail "$name" "Should still output matches from existing file"
        fi
    }

    # -s suppresses error messages
    name="error: -s suppresses errors"
    should_run "$name" && {
        local grep_err ferp_err
        grep_err=$(grep -s "pattern" "/nonexistent" 2>&1)
        ferp_err=$("$FERP" -s "pattern" "/nonexistent" 2>&1)
        if [[ -z "$grep_err" && -z "$ferp_err" ]]; then
            pass "$name"
        else
            fail "$name" "Error messages not suppressed"
        fi
    }

    # Directory without -r
    name="error: directory without -r"
    should_run "$name" && {
        grep "pattern" "$FIXTURES" 2>/dev/null && grep_exit=0 || grep_exit=$?
        "$FERP" "pattern" "$FIXTURES" 2>/dev/null && ferp_exit=0 || ferp_exit=$?
        # Both should handle this gracefully
        if [[ "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Exit codes differ: grep=$grep_exit ferp=$ferp_exit"
        fi
    }
}

#------------------------------------------------------------------------------
# Test: Unusual Input
#------------------------------------------------------------------------------

test_unusual_input() {
    section "Unusual Input"

    # File with only newlines
    local name="input: only newlines"
    should_run "$name" && {
        printf '\n\n\n' > "$FIXTURES/only_newlines.txt"
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep "." "$FIXTURES/only_newlines.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" "." "$FIXTURES/only_newlines.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # Single character file
    name="input: single character"
    should_run "$name" && {
        printf 'x' > "$FIXTURES/single_char.txt"
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep "x" "$FIXTURES/single_char.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" "x" "$FIXTURES/single_char.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # Empty file
    test_grep_compat "input: empty file" "" "pattern" "$FIXTURES/empty.txt"

    # No trailing newline
    test_grep_compat "input: no trailing newline" "" "newline" "$FIXTURES/no_newline.txt"

    # Mixed line endings
    test_grep_compat "input: mixed line endings" "" "line" "$FIXTURES/line_endings.txt"

    # Lines with only whitespace
    test_grep_compat "input: whitespace-only lines" "" "^[ \t]*$" "$FIXTURES/whitespace_lines.txt"

    # Extremely long pattern
    name="input: long pattern"
    should_run "$name" && {
        local long_pattern
        long_pattern=$(printf 'x%.0s' {1..100})
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep "$long_pattern" "$FIXTURES/longline.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" "$long_pattern" "$FIXTURES/longline.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Exit codes differ"
        fi
    }
}

#------------------------------------------------------------------------------
# Test: Flag Interaction Edge Cases
#------------------------------------------------------------------------------

test_flag_interactions() {
    section "Flag Interaction Edge Cases"

    # -c with -l (count vs list)
    compare_multi_files "-c with -l" "-cl" "pattern" "$FIXTURES/multi1.txt" "$FIXTURES/multi2.txt" "$FIXTURES/multi3.txt"

    # -o with -c (count only matching parts)
    test_grep_compat "-o with -c" "-oc" "match" "$FIXTURES/many_matches.txt"

    # -m with -c (max count affects total)
    test_grep_compat "-m with -c" "-m2 -c" "match" "$FIXTURES/many_matches.txt"

    # -v with -o (only matching of non-matches - undefined?)
    test_grep_compat "-v with -o" "-vo" "match" "$FIXTURES/many_matches.txt"

    # -w with -o
    test_grep_compat "-w with -o" "-wo" "match" "$FIXTURES/many_matches.txt"

    # -x with -o
    test_grep_compat "-x with -o" "-xo" "hello world" "$FIXTURES/basic.txt"

    # -l with -L (conflicting - last wins?)
    local name="-l with -L"
    should_run "$name" && {
        # This is undefined behavior, just check they don't crash
        "$FERP" -lL "pattern" "$FIXTURES/multi1.txt" 2>/dev/null
        if [[ $? -le 2 ]]; then
            pass "$name"
        else
            fail "$name" "Unexpected exit code"
        fi
    }

    # -q with -c (quiet but count?)
    name="-q with -c"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -qc "hello" "$FIXTURES/basic.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -qc "hello" "$FIXTURES/basic.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # -n with -b with -o (all position info)
    test_grep_compat "-n -b -o combo" "-nbo" "hello" "$FIXTURES/basic.txt"

    # -H with -h (conflicting)
    name="-H with -h"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -Hh "hello" "$FIXTURES/basic.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -Hh "hello" "$FIXTURES/basic.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }
}

#------------------------------------------------------------------------------
# Test: PCRE Features (-P)
#------------------------------------------------------------------------------

test_pcre_features() {
    section "PCRE Features (-P)"

    # Check if PCRE is available
    if ! grep -P "test" "$FIXTURES/basic.txt" &>/dev/null; then
        skip "PCRE: not available in grep" "grep -P not supported"
        return
    fi

    if ! "$FERP" -P "test" "$FIXTURES/basic.txt" &>/dev/null; then
        skip "PCRE: not available in ferp" "ferp -P not supported"
        return
    fi

    # Basic PCRE
    test_grep_compat "PCRE: basic pattern" "-P" "hello" "$FIXTURES/basic.txt"

    # Lookahead
    test_grep_compat "PCRE: positive lookahead" "-P" 'hello(?= world)' "$FIXTURES/basic.txt"
    test_grep_compat "PCRE: negative lookahead" "-P" 'hello(?! there)' "$FIXTURES/basic.txt"

    # Lookbehind
    test_grep_compat "PCRE: positive lookbehind" "-P" '(?<=hello )world' "$FIXTURES/basic.txt"
    test_grep_compat "PCRE: negative lookbehind" "-P" '(?<!good)bye' "$FIXTURES/basic.txt"

    # Non-greedy quantifiers
    test_grep_compat_stdin "PCRE: non-greedy *?" "-Po" "a.*?b" "aXXbYYb"
    test_grep_compat_stdin "PCRE: non-greedy +?" "-Po" "a.+?b" "aXXbYYb"

    # Word boundary \b
    test_grep_compat "PCRE: word boundary \\b" "-P" '\btest\b' "$FIXTURES/basic.txt"

    # Character classes \d, \w, \s
    test_grep_compat "PCRE: \\d digit" "-P" '\d+' "$FIXTURES/basic.txt"
    test_grep_compat "PCRE: \\w word" "-P" '\w+' "$FIXTURES/basic.txt"
    test_grep_compat "PCRE: \\s space" "-P" '\s+' "$FIXTURES/basic.txt"

    # Negated classes \D, \W, \S
    test_grep_compat "PCRE: \\D non-digit" "-Po" '\D+' "$FIXTURES/basic.txt"

    # PCRE with -i
    test_grep_compat "PCRE: with -i" "-Pi" "HELLO" "$FIXTURES/basic.txt"

    # PCRE with -o
    test_grep_compat "PCRE: with -o" "-Po" '\w+' "$FIXTURES/basic.txt"

    # PCRE with -v
    test_grep_compat "PCRE: with -v" "-Pv" "hello" "$FIXTURES/basic.txt"

    # PCRE backreferences
    test_grep_compat "PCRE: backreference" "-P" '(\w)\1' "$FIXTURES/backref.txt"
}

#------------------------------------------------------------------------------
# Test: Fixed String Edge Cases (-F)
#------------------------------------------------------------------------------

test_fixed_string_edge_cases() {
    section "Fixed String Edge Cases (-F)"

    # All regex metacharacters as literals
    test_grep_compat "-F: dot literal" "-F" "hello.world" "$FIXTURES/special.txt"
    test_grep_compat "-F: star literal" "-F" "a*b" "$FIXTURES/special.txt"
    test_grep_compat "-F: plus literal" "-F" "a+b" "$FIXTURES/special.txt"
    test_grep_compat "-F: question literal" "-F" "question?" "$FIXTURES/special.txt"
    test_grep_compat "-F: brackets literal" "-F" "array[0]" "$FIXTURES/special.txt"
    test_grep_compat "-F: parens literal" "-F" "func()" "$FIXTURES/special.txt"
    test_grep_compat "-F: braces literal" "-F" "curly{brace}" "$FIXTURES/special.txt"
    test_grep_compat "-F: caret literal" "-F" "start^here" "$FIXTURES/special.txt"
    test_grep_compat "-F: dollar literal" "-F" 'end$there' "$FIXTURES/special.txt"
    test_grep_compat "-F: pipe literal" "-F" "pipe|char" "$FIXTURES/special.txt"
    test_grep_compat "-F: backslash literal" "-F" 'back\slash' "$FIXTURES/special.txt"

    # -F with -i
    test_grep_compat "-F with -i" "-Fi" "HELLO.WORLD" "$FIXTURES/special.txt"

    # -F with -w
    test_grep_compat "-F with -w" "-Fw" "hello" "$FIXTURES/basic.txt"

    # -F with -x
    test_grep_compat "-F with -x" "-Fx" "hello world" "$FIXTURES/basic.txt"

    # -F with multiple patterns via -e
    local name="-F with -e multiple"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -F -e "a+b" -e "a*b" "$FIXTURES/special.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -F -e "a+b" -e "a*b" "$FIXTURES/special.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # -F with newline-separated patterns
    name="-F with newline in pattern"
    should_run "$name" && {
        local grep_out ferp_out grep_exit ferp_exit
        grep_out=$(grep -F $'hello\ngoodbye' "$FIXTURES/basic.txt" 2>/dev/null) && grep_exit=0 || grep_exit=$?
        ferp_out=$("$FERP" -F $'hello\ngoodbye' "$FIXTURES/basic.txt" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }
}

#------------------------------------------------------------------------------
# Summary
#------------------------------------------------------------------------------

print_summary() {
    echo ""
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}  Summary${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
    echo "  Tests run:    $TESTS_RUN"
    echo -e "  ${GREEN}Passed:       $TESTS_PASSED${NC}"
    echo -e "  ${RED}Failed:       $TESTS_FAILED${NC}"
    echo -e "  ${YELLOW}Skipped:      $TESTS_SKIPPED${NC}"
    echo ""

    if [[ $TESTS_FAILED -gt 0 ]]; then
        echo -e "${RED}Failed tests:${NC}"
        for test in "${FAILED_TESTS[@]}"; do
            echo "  - $test"
        done
        echo ""
        echo -e "${RED}Some tests failed!${NC}"
        return 1
    else
        echo -e "${GREEN}All tests passed!${NC}"
        return 0
    fi
}

#------------------------------------------------------------------------------
# Main
#------------------------------------------------------------------------------

main() {
    echo -e "${BLUE}╔════════════════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║              FERP Extended Grep Test Suite                                 ║${NC}"
    echo -e "${BLUE}╚════════════════════════════════════════════════════════════════════════════╝${NC}"
    echo ""

    # Check prerequisites
    if [[ ! -x "$FERP" ]]; then
        echo -e "${RED}Error: ferp binary not found at $FERP${NC}"
        echo "Run 'make' first to build ferp"
        exit 1
    fi

    if ! command -v grep &> /dev/null; then
        echo -e "${RED}Error: grep not found${NC}"
        exit 1
    fi

    echo "ferp: $FERP"
    echo "grep: $(which grep) ($(grep --version | head -1))"
    echo ""

    # Generate fixtures
    generate_fixtures

    # Run test suites
    test_multiple_patterns
    test_backreferences
    test_null_data
    test_binary_files
    test_recursive
    test_context_edge_cases
    test_output_format
    test_regex_edge_cases
    test_bre_ere_differences
    test_error_handling
    test_unusual_input
    test_flag_interactions
    test_pcre_features
    test_fixed_string_edge_cases

    # Print summary
    print_summary
}

main "$@"
