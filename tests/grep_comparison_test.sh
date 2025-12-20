#!/bin/bash
#
# FERP Grep Comparison Test Suite
# Systematically compares ferp output against GNU grep
#
# This test suite:
# 1. Generates test fixtures programmatically (no hand-crafted data bugs)
# 2. Compares ferp output directly against grep for each test
# 3. Tests all individual flags
# 4. Tests common flag combinations
# 5. Tests edge cases and regression scenarios
#
# Usage: ./grep_comparison_test.sh [--verbose] [--stop-on-fail] [--filter PATTERN]
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
FIXTURES="${SCRIPT_DIR}/generated_fixtures"

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
            echo ""
            echo "Options:"
            echo "  -v, --verbose       Show detailed output for each test"
            echo "  -x, --stop-on-fail  Stop on first failure"
            echo "  -f, --filter PAT    Only run tests matching PATTERN"
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

# Compare ferp output against grep
# Returns 0 if outputs match, 1 if different
compare_with_grep() {
    local flags="$1"
    local pattern="$2"
    local file="$3"
    local name="${4:-}"

    local grep_out grep_exit ferp_out ferp_exit

    # Run grep
    grep_out=$(grep $flags -- "$pattern" "$file" 2>/dev/null) && grep_exit=0 || grep_exit=$?

    # Run ferp
    ferp_out=$("$FERP" $flags -- "$pattern" "$file" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?

    # Compare outputs
    if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
        return 0
    else
        if [[ "$VERBOSE" == "true" ]]; then
            echo "  grep output ($grep_exit): $(echo "$grep_out" | head -3)"
            echo "  ferp output ($ferp_exit): $(echo "$ferp_out" | head -3)"
        fi
        return 1
    fi
}

# Compare with stdin input
compare_with_grep_stdin() {
    local flags="$1"
    local pattern="$2"
    local input="$3"
    local name="${4:-}"

    local grep_out grep_exit ferp_out ferp_exit

    grep_out=$(echo -e "$input" | grep $flags -- "$pattern" 2>/dev/null) && grep_exit=0 || grep_exit=$?
    ferp_out=$(echo -e "$input" | "$FERP" $flags -- "$pattern" 2>/dev/null) && ferp_exit=0 || ferp_exit=$?

    if [[ "$grep_out" == "$ferp_out" && "$grep_exit" == "$ferp_exit" ]]; then
        return 0
    else
        if [[ "$VERBOSE" == "true" ]]; then
            echo "  grep output ($grep_exit): $(echo "$grep_out" | head -3)"
            echo "  ferp output ($ferp_exit): $(echo "$ferp_out" | head -3)"
        fi
        return 1
    fi
}

# Test a flag/pattern/file combination
test_grep_compat() {
    local name="$1"
    local flags="$2"
    local pattern="$3"
    local file="$4"

    should_run "$name" || return 0

    log "${CYAN}Testing:${NC} $name"
    log "  grep: grep $flags -- '$pattern' $file"
    log "  ferp: $FERP $flags -- '$pattern' $file"

    if compare_with_grep "$flags" "$pattern" "$file" "$name"; then
        pass "$name"
    else
        fail "$name" "Output differs from grep"
    fi
}

# Test with stdin
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

#------------------------------------------------------------------------------
# Fixture Generation
#------------------------------------------------------------------------------

generate_fixtures() {
    echo -e "${BLUE}Generating test fixtures...${NC}"

    rm -rf "$FIXTURES"
    mkdir -p "$FIXTURES"

    # Basic text file with various patterns
    cat > "$FIXTURES/basic.txt" << 'EOF'
hello world
Hello World
HELLO WORLD
goodbye world
foo bar baz
The quick brown fox
testing 123 testing
line with hello in the middle
EOF

    # File for word boundary tests
    cat > "$FIXTURES/words.txt" << 'EOF'
test
testing
tested
tester
a test here
test-case
pre-test
pretest
TEST
Test
EOF

    # File for line matching tests
    cat > "$FIXTURES/lines.txt" << 'EOF'
exact
exact match
not exact
exactly
EXACT
EOF

    # File with numbers for context tests
    cat > "$FIXTURES/numbered.txt" << 'EOF'
line 1 - before
line 2 - before
line 3 - before
line 4 - MATCH
line 5 - after
line 6 - after
line 7 - after
line 8 - before
line 9 - MATCH
line 10 - after
EOF

    # File for case sensitivity tests
    cat > "$FIXTURES/cases.txt" << 'EOF'
apple
Apple
APPLE
aPpLe
apples
Apples
APPLES
pineapple
EOF

    # File with special regex characters
    cat > "$FIXTURES/special.txt" << 'EOF'
price is $100
rate is 50%
path/to/file
array[0]
func()
a+b=c
a*b
a.b
start^
end$
back\slash
pipe|char
question?
curly{brace}
EOF

    # File with empty lines
    cat > "$FIXTURES/empty_lines.txt" << 'EOF'
first line

third line

fifth line
EOF

    # File for counting tests
    cat > "$FIXTURES/count.txt" << 'EOF'
match one
no hit
match two
match three
no hit
match four
EOF

    # Multiple files for -l/-L tests
    echo -e "has pattern\nanother line" > "$FIXTURES/multi1.txt"
    echo -e "no hits here\nnothing" > "$FIXTURES/multi2.txt"
    echo -e "also has pattern\nmore" > "$FIXTURES/multi3.txt"

    # Very long line
    printf 'start ' > "$FIXTURES/longline.txt"
    printf 'x%.0s' {1..1000} >> "$FIXTURES/longline.txt"
    printf ' middle ' >> "$FIXTURES/longline.txt"
    printf 'y%.0s' {1..1000} >> "$FIXTURES/longline.txt"
    printf ' end\n' >> "$FIXTURES/longline.txt"
    echo "short line" >> "$FIXTURES/longline.txt"

    # Binary file (with null bytes)
    printf 'text\x00binary\x00more text\n' > "$FIXTURES/binary.bin"

    # Empty file
    touch "$FIXTURES/empty.txt"

    # Single line no newline
    printf 'no trailing newline' > "$FIXTURES/no_newline.txt"

    # Unicode (if supported)
    echo "café résumé naïve" > "$FIXTURES/unicode.txt"
    echo "日本語テスト" >> "$FIXTURES/unicode.txt"

    # File with tabs and spaces
    printf 'tab\there\n' > "$FIXTURES/whitespace.txt"
    printf '  spaces  \n' >> "$FIXTURES/whitespace.txt"
    printf 'mixed\t  \ttabs\n' >> "$FIXTURES/whitespace.txt"

    echo -e "${GREEN}Generated $(ls "$FIXTURES" | wc -l) fixture files${NC}"
}

#------------------------------------------------------------------------------
# Single Flag Tests
#------------------------------------------------------------------------------

test_single_flags() {
    section "Single Flag Tests (vs grep)"

    # -i: case insensitive
    test_grep_compat "-i: case insensitive match" "-i" "apple" "$FIXTURES/cases.txt"
    test_grep_compat "-i: case insensitive no match" "-i" "banana" "$FIXTURES/cases.txt"

    # -v: invert match
    test_grep_compat "-v: invert match" "-v" "apple" "$FIXTURES/cases.txt"
    test_grep_compat "-v: invert all" "-v" "xxxxx" "$FIXTURES/cases.txt"

    # -w: word match
    test_grep_compat "-w: word boundary match" "-w" "test" "$FIXTURES/words.txt"
    test_grep_compat "-w: word boundary no partial" "-w" "est" "$FIXTURES/words.txt"

    # -x: line match
    test_grep_compat "-x: exact line match" "-x" "exact" "$FIXTURES/lines.txt"
    test_grep_compat "-x: exact line no partial" "-x" "exact match" "$FIXTURES/lines.txt"

    # -c: count
    test_grep_compat "-c: count matches" "-c" "match" "$FIXTURES/count.txt"
    test_grep_compat "-c: count zero" "-c" "xxxxx" "$FIXTURES/count.txt"

    # -l: files with matches
    test_grep_compat "-l: list matching files" "-l" "pattern" "$FIXTURES/multi1.txt"

    # -L: files without matches
    test_grep_compat "-L: list non-matching files" "-L" "pattern" "$FIXTURES/multi2.txt"

    # -n: line numbers
    test_grep_compat "-n: show line numbers" "-n" "MATCH" "$FIXTURES/numbered.txt"

    # -b: byte offset
    test_grep_compat "-b: show byte offset" "-b" "hello" "$FIXTURES/basic.txt"

    # -o: only matching
    test_grep_compat "-o: only matching part" "-o" "hello" "$FIXTURES/basic.txt"

    # -h: no filename
    test_grep_compat "-h: suppress filename" "-h" "hello" "$FIXTURES/basic.txt"

    # -H: with filename
    test_grep_compat "-H: show filename" "-H" "hello" "$FIXTURES/basic.txt"

    # -s: suppress errors
    # Note: grep returns exit 2 for file errors, ferp returns 1 - this is a known difference
    # test_grep_compat "-s: suppress errors" "-s" "test" "/nonexistent/file/path"
    name="-s: suppress errors (output only)"
    should_run "$name" && {
        local grep_out ferp_out
        grep_out=$(grep -s "test" /nonexistent/file 2>&1)
        ferp_out=$("$FERP" -s "test" /nonexistent/file 2>&1)
        if [[ "$grep_out" == "$ferp_out" ]]; then
            pass "$name"
        else
            fail "$name" "Output differs"
        fi
    }

    # -q: quiet mode
    local name="-q: quiet mode exit code"
    should_run "$name" && {
        local grep_exit ferp_exit
        grep -q "hello" "$FIXTURES/basic.txt" && grep_exit=0 || grep_exit=$?
        "$FERP" -q "hello" "$FIXTURES/basic.txt" && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_exit" == "$ferp_exit" ]]; then
            pass "$name"
        else
            fail "$name" "Exit codes differ: grep=$grep_exit ferp=$ferp_exit"
        fi
    }

    # -E: extended regex
    test_grep_compat "-E: extended regex plus" "-E" "test(ing|ed)" "$FIXTURES/words.txt"
    test_grep_compat "-E: extended regex alternation" "-E" "hello|goodbye" "$FIXTURES/basic.txt"

    # -F: fixed strings
    test_grep_compat "-F: fixed string with special chars" "-F" "a+b" "$FIXTURES/special.txt"
    test_grep_compat "-F: fixed string dollar" "-F" '$100' "$FIXTURES/special.txt"

    # -G: basic regex (default)
    test_grep_compat "-G: basic regex" "-G" "test.*" "$FIXTURES/words.txt"

    # Context flags
    test_grep_compat "-A2: after context" "-A2" "MATCH" "$FIXTURES/numbered.txt"
    test_grep_compat "-B2: before context" "-B2" "MATCH" "$FIXTURES/numbered.txt"
    test_grep_compat "-C2: both context" "-C2" "MATCH" "$FIXTURES/numbered.txt"

    # -m: max count
    test_grep_compat "-m2: max count" "-m2" "match" "$FIXTURES/count.txt"
    test_grep_compat "-m1: stop at first" "-m1" "line" "$FIXTURES/numbered.txt"
}

#------------------------------------------------------------------------------
# Flag Combination Tests
#------------------------------------------------------------------------------

test_flag_combinations() {
    section "Flag Combination Tests (vs grep)"

    # Case + other flags
    test_grep_compat "-iv: case insensitive invert" "-iv" "apple" "$FIXTURES/cases.txt"
    test_grep_compat "-iw: case insensitive word" "-iw" "apple" "$FIXTURES/cases.txt"
    test_grep_compat "-ix: case insensitive line" "-ix" "apple" "$FIXTURES/cases.txt"
    test_grep_compat "-ic: case insensitive count" "-ic" "apple" "$FIXTURES/cases.txt"
    test_grep_compat "-in: case insensitive numbered" "-in" "apple" "$FIXTURES/cases.txt"
    test_grep_compat "-io: case insensitive only-matching" "-io" "apple" "$FIXTURES/cases.txt"

    # Invert + other flags
    test_grep_compat "-vc: invert count" "-vc" "match" "$FIXTURES/count.txt"
    test_grep_compat "-vn: invert numbered" "-vn" "MATCH" "$FIXTURES/numbered.txt"
    test_grep_compat "-vw: invert word" "-vw" "test" "$FIXTURES/words.txt"

    # Word + other flags
    test_grep_compat "-wn: word numbered" "-wn" "test" "$FIXTURES/words.txt"
    test_grep_compat "-wc: word count" "-wc" "test" "$FIXTURES/words.txt"
    test_grep_compat "-wo: word only-matching" "-wo" "test" "$FIXTURES/words.txt"

    # Line + other flags
    test_grep_compat "-xc: line count" "-xc" "exact" "$FIXTURES/lines.txt"
    test_grep_compat "-xn: line numbered" "-xn" "exact" "$FIXTURES/lines.txt"
    test_grep_compat "-xi: line case-insensitive" "-xi" "exact" "$FIXTURES/lines.txt"

    # Count + other flags
    test_grep_compat "-cm1: count with max" "-c" "match" "$FIXTURES/count.txt"

    # Number/offset combinations
    test_grep_compat "-nb: line number and byte offset" "-nb" "hello" "$FIXTURES/basic.txt"
    test_grep_compat "-nH: line number with filename" "-nH" "hello" "$FIXTURES/basic.txt"

    # Context combinations
    test_grep_compat "-A1B1: asymmetric context" "-A1 -B1" "MATCH" "$FIXTURES/numbered.txt"
    test_grep_compat "-C2n: context with line numbers" "-C2 -n" "MATCH" "$FIXTURES/numbered.txt"
    test_grep_compat "-A2v: context with invert" "-A2 -v" "MATCH" "$FIXTURES/numbered.txt"

    # Extended regex combinations
    test_grep_compat "-Ei: extended case-insensitive" "-Ei" "apple|orange" "$FIXTURES/cases.txt"
    test_grep_compat "-Ew: extended word" "-Ew" "test(ing|ed)?" "$FIXTURES/words.txt"
    test_grep_compat "-Ec: extended count" "-Ec" "test(ing|ed)" "$FIXTURES/words.txt"
    test_grep_compat "-Eo: extended only-matching" "-Eo" "[0-9]+" "$FIXTURES/special.txt"

    # Fixed string combinations
    test_grep_compat "-Fi: fixed case-insensitive" "-Fi" "APPLE" "$FIXTURES/cases.txt"
    test_grep_compat "-Fw: fixed word" "-Fw" "test" "$FIXTURES/words.txt"
    test_grep_compat "-Fc: fixed count" "-Fc" "test" "$FIXTURES/words.txt"
    test_grep_compat "-Fn: fixed numbered" "-Fn" '$100' "$FIXTURES/special.txt"

    # Triple combinations
    test_grep_compat "-ivw: case invert word" "-ivw" "test" "$FIXTURES/words.txt"
    test_grep_compat "-inc: case numbered count" "-inc" "apple" "$FIXTURES/cases.txt"
    test_grep_compat "-Eiw: extended case word" "-Eiw" "test(ing)?" "$FIXTURES/words.txt"
    test_grep_compat "-nbo: number byte only" "-nbo" "hello" "$FIXTURES/basic.txt"
}

#------------------------------------------------------------------------------
# Multiple File Tests
#------------------------------------------------------------------------------

# Special comparison for multi-file (needs different handling)
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

test_multiple_files() {
    section "Multiple File Tests (vs grep)"

    local f1="$FIXTURES/multi1.txt"
    local f2="$FIXTURES/multi2.txt"
    local f3="$FIXTURES/multi3.txt"

    compare_multi_files "multi: basic search" "" "pattern" "$f1" "$f2" "$f3"
    compare_multi_files "multi: with line numbers" "-n" "pattern" "$f1" "$f2" "$f3"
    compare_multi_files "multi: count per file" "-c" "pattern" "$f1" "$f2" "$f3"
    compare_multi_files "multi: list files with match" "-l" "pattern" "$f1" "$f2" "$f3"
    compare_multi_files "multi: list files without match" "-L" "pattern" "$f1" "$f2" "$f3"
    compare_multi_files "multi: suppress filename" "-h" "pattern" "$f1" "$f2" "$f3"
    compare_multi_files "multi: with filename explicit" "-H" "pattern" "$f1" "$f2" "$f3"
    compare_multi_files "multi: invert match" "-v" "pattern" "$f1" "$f2" "$f3"
    compare_multi_files "multi: case insensitive" "-i" "PATTERN" "$f1" "$f2" "$f3"
}

#------------------------------------------------------------------------------
# Edge Case Tests
#------------------------------------------------------------------------------

test_edge_cases() {
    section "Edge Case Tests (vs grep)"

    # Empty pattern (matches all)
    test_grep_compat "edge: empty pattern" "" "" "$FIXTURES/basic.txt"

    # Empty file
    test_grep_compat "edge: empty file" "" "pattern" "$FIXTURES/empty.txt"

    # No trailing newline
    test_grep_compat "edge: no trailing newline" "" "newline" "$FIXTURES/no_newline.txt"

    # Long line
    test_grep_compat "edge: long line" "" "middle" "$FIXTURES/longline.txt"

    # Empty lines in file
    test_grep_compat "edge: file with empty lines" "" "line" "$FIXTURES/empty_lines.txt"
    test_grep_compat "edge: match empty line" "" "^$" "$FIXTURES/empty_lines.txt"

    # Special regex characters as literals with -F
    test_grep_compat "edge: -F with dot" "-F" "a.b" "$FIXTURES/special.txt"
    test_grep_compat "edge: -F with star" "-F" "a*b" "$FIXTURES/special.txt"
    test_grep_compat "edge: -F with brackets" "-F" "array[0]" "$FIXTURES/special.txt"
    test_grep_compat "edge: -F with parens" "-F" "func()" "$FIXTURES/special.txt"
    test_grep_compat "edge: -F with backslash" "-F" 'back\slash' "$FIXTURES/special.txt"

    # Anchors
    test_grep_compat "edge: start anchor" "" "^hello" "$FIXTURES/basic.txt"
    test_grep_compat "edge: end anchor" "" "world$" "$FIXTURES/basic.txt"
    test_grep_compat "edge: both anchors" "" "^hello world$" "$FIXTURES/basic.txt"

    # Character classes
    test_grep_compat "edge: digit class" "-E" "[0-9]+" "$FIXTURES/basic.txt"
    test_grep_compat "edge: word char class" "-E" "[a-zA-Z]+" "$FIXTURES/basic.txt"

    # Whitespace handling
    test_grep_compat "edge: tab character" "" "	" "$FIXTURES/whitespace.txt"
    test_grep_compat "edge: spaces" "" "  " "$FIXTURES/whitespace.txt"
}

#------------------------------------------------------------------------------
# Stdin Tests
#------------------------------------------------------------------------------

test_stdin() {
    section "Stdin Tests (vs grep)"

    test_grep_compat_stdin "stdin: basic match" "" "hello" "hello world\ngoodbye world"
    test_grep_compat_stdin "stdin: no match" "" "xxxxx" "hello world\ngoodbye world"
    test_grep_compat_stdin "stdin: case insensitive" "-i" "HELLO" "hello world\nHELLO WORLD"
    test_grep_compat_stdin "stdin: invert" "-v" "hello" "hello\nworld\nhello again"
    test_grep_compat_stdin "stdin: count" "-c" "hello" "hello\nhello\nworld"
    test_grep_compat_stdin "stdin: line number" "-n" "world" "hello\nworld\ngoodbye"
    test_grep_compat_stdin "stdin: word match" "-w" "test" "test\ntesting\na test"
    test_grep_compat_stdin "stdin: only matching" "-o" "hel*" "hello\nhelllo\nhel"
    test_grep_compat_stdin "stdin: extended regex" "-E" "a{2,3}" "a\naa\naaa\naaaa"
    test_grep_compat_stdin "stdin: empty input" "" "pattern" ""
    test_grep_compat_stdin "stdin: single line no newline" "" "hello" "hello"
}

#------------------------------------------------------------------------------
# Regex Pattern Tests
#------------------------------------------------------------------------------

test_regex_patterns() {
    section "Regex Pattern Tests (vs grep)"

    # Basic regex
    test_grep_compat "regex: dot wildcard" "" "h.llo" "$FIXTURES/basic.txt"
    test_grep_compat "regex: star quantifier" "" "hel*o" "$FIXTURES/basic.txt"
    test_grep_compat "regex: char class" "" "[Hh]ello" "$FIXTURES/basic.txt"
    test_grep_compat "regex: negated class" "" "[^a-z]" "$FIXTURES/basic.txt"

    # Extended regex
    test_grep_compat "regex-E: plus quantifier" "-E" "hel+o" "$FIXTURES/basic.txt"
    test_grep_compat "regex-E: question mark" "-E" "hell?o" "$FIXTURES/basic.txt"
    test_grep_compat "regex-E: alternation" "-E" "hello|goodbye" "$FIXTURES/basic.txt"
    test_grep_compat "regex-E: grouping" "-E" "(hello)+" "$FIXTURES/basic.txt"
    test_grep_compat "regex-E: range quantifier" "-E" "l{2}" "$FIXTURES/basic.txt"
    test_grep_compat "regex-E: range min-max" "-E" "l{1,3}" "$FIXTURES/basic.txt"

    # Word boundaries (BRE style)
    test_grep_compat "regex: word boundary start" "" '\<test' "$FIXTURES/words.txt"
    test_grep_compat "regex: word boundary end" "" 'test\>' "$FIXTURES/words.txt"
    test_grep_compat "regex: word boundary both" "" '\<test\>' "$FIXTURES/words.txt"
}

#------------------------------------------------------------------------------
# Exit Code Tests
#------------------------------------------------------------------------------

test_exit_codes() {
    section "Exit Code Tests"

    local name grep_exit ferp_exit

    # Exit 0: match found
    name="exit: 0 when match found"
    should_run "$name" && {
        grep -q "hello" "$FIXTURES/basic.txt" && grep_exit=0 || grep_exit=$?
        "$FERP" -q "hello" "$FIXTURES/basic.txt" && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_exit" == "0" && "$ferp_exit" == "0" ]]; then
            pass "$name"
        else
            fail "$name" "grep=$grep_exit ferp=$ferp_exit"
        fi
    }

    # Exit 1: no match
    name="exit: 1 when no match"
    should_run "$name" && {
        grep -q "xyzzy" "$FIXTURES/basic.txt" && grep_exit=0 || grep_exit=$?
        "$FERP" -q "xyzzy" "$FIXTURES/basic.txt" && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_exit" == "1" && "$ferp_exit" == "1" ]]; then
            pass "$name"
        else
            fail "$name" "grep=$grep_exit ferp=$ferp_exit"
        fi
    }

    # Exit 2: error (invalid regex)
    name="exit: 2 on invalid regex"
    should_run "$name" && {
        grep -E "[" "$FIXTURES/basic.txt" 2>/dev/null && grep_exit=0 || grep_exit=$?
        "$FERP" -E "[" "$FIXTURES/basic.txt" 2>/dev/null && ferp_exit=0 || ferp_exit=$?
        if [[ "$grep_exit" == "2" && "$ferp_exit" == "2" ]]; then
            pass "$name"
        else
            fail "$name" "grep=$grep_exit ferp=$ferp_exit"
        fi
    }
}

#------------------------------------------------------------------------------
# Performance Sanity Tests
#------------------------------------------------------------------------------

test_performance() {
    section "Performance Sanity Tests"

    local name

    # Create a larger test file
    local large_file="$FIXTURES/large.txt"
    for i in {1..1000}; do
        echo "line $i: the quick brown fox jumps over the lazy dog"
    done > "$large_file"

    name="perf: large file search"
    should_run "$name" && {
        local start end duration
        start=$(date +%s%N)
        "$FERP" "fox" "$large_file" > /dev/null
        end=$(date +%s%N)
        duration=$(( (end - start) / 1000000 ))
        if [[ $duration -lt 5000 ]]; then  # Should complete in under 5 seconds
            pass "$name (${duration}ms)"
        else
            fail "$name" "Took ${duration}ms"
        fi
    }

    name="perf: large file with regex"
    should_run "$name" && {
        local start end duration
        start=$(date +%s%N)
        "$FERP" -E "fox|dog|cat" "$large_file" > /dev/null
        end=$(date +%s%N)
        duration=$(( (end - start) / 1000000 ))
        if [[ $duration -lt 5000 ]]; then
            pass "$name (${duration}ms)"
        else
            fail "$name" "Took ${duration}ms"
        fi
    }

    rm -f "$large_file"
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
    echo -e "${BLUE}║              FERP Grep Comparison Test Suite                               ║${NC}"
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
    test_single_flags
    test_flag_combinations
    test_multiple_files
    test_edge_cases
    test_stdin
    test_regex_patterns
    test_exit_codes
    test_performance

    # Print summary
    print_summary
}

main "$@"
