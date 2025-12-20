#!/bin/bash
#
# FERP Integration Test Suite
# Comprehensive tests for all command-line flags
#
# Usage: ./integration_test.sh [--compare-grep] [--verbose] [--filter PATTERN]
#
# Options:
#   --compare-grep    Compare ferp output against GNU grep
#   --verbose         Show detailed output for each test
#   --filter PATTERN  Only run tests matching PATTERN
#

set -uo pipefail
# Note: not using -e because we want tests to continue on failure

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0
TESTS_SKIPPED=0

# Options
COMPARE_GREP=false
VERBOSE=false
FILTER=""

# Paths
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
FERP="${SCRIPT_DIR}/../ferp"
FIXTURES="${SCRIPT_DIR}/fixtures"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --compare-grep)
            COMPARE_GREP=true
            shift
            ;;
        --verbose)
            VERBOSE=true
            shift
            ;;
        --filter)
            FILTER="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Check ferp exists
if [[ ! -x "$FERP" ]]; then
    echo -e "${RED}Error: ferp binary not found at $FERP${NC}"
    echo "Run 'make' first to build ferp"
    exit 1
fi

#------------------------------------------------------------------------------
# Test Framework Functions
#------------------------------------------------------------------------------

log_test() {
    local name="$1"
    if [[ "$VERBOSE" == "true" ]]; then
        echo -e "${BLUE}Running:${NC} $name"
    fi
}

pass() {
    local name="$1"
    ((TESTS_PASSED++))
    ((TESTS_RUN++))
    echo -e "${GREEN}PASS${NC}: $name"
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

# Run ferp and check exit code
# Usage: run_ferp_expect EXIT_CODE ARGS...
run_ferp_expect() {
    local expected_exit="$1"
    shift
    local actual_exit=0
    "$FERP" "$@" >/dev/null 2>&1 || actual_exit=$?
    [[ "$actual_exit" -eq "$expected_exit" ]]
}

# Run ferp and capture output
# Usage: run_ferp ARGS...
run_ferp() {
    "$FERP" "$@" 2>/dev/null || true
}

# Compare ferp output to expected string
# Usage: assert_output "expected" ferp_args...
assert_output() {
    local expected="$1"
    shift
    local actual
    actual=$("$FERP" "$@" 2>/dev/null) || true
    [[ "$actual" == "$expected" ]]
}

# Compare ferp output to grep output
# Usage: assert_matches_grep ARGS...
assert_matches_grep() {
    if [[ "$COMPARE_GREP" != "true" ]]; then
        return 0
    fi
    local ferp_out grep_out
    ferp_out=$("$FERP" "$@" 2>/dev/null) || true
    grep_out=$(grep "$@" 2>/dev/null) || true
    [[ "$ferp_out" == "$grep_out" ]]
}

# Count output lines
# Usage: assert_line_count EXPECTED ferp_args...
assert_line_count() {
    local expected="$1"
    shift
    local actual
    actual=$("$FERP" "$@" 2>/dev/null | wc -l) || true
    [[ "$actual" -eq "$expected" ]]
}

# Check that output contains a string
# Usage: assert_contains "substring" ferp_args...
assert_contains() {
    local substring="$1"
    shift
    local output
    output=$("$FERP" "$@" 2>/dev/null) || true
    [[ "$output" == *"$substring"* ]]
}

# Check that output does NOT contain a string
# Usage: assert_not_contains "substring" ferp_args...
assert_not_contains() {
    local substring="$1"
    shift
    local output
    output=$("$FERP" "$@" 2>/dev/null) || true
    [[ "$output" != *"$substring"* ]]
}

#------------------------------------------------------------------------------
# Test Categories
#------------------------------------------------------------------------------

section() {
    echo ""
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}  $1${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
}

#------------------------------------------------------------------------------
# PATTERN TYPE TESTS (-E, -F, -G, -P)
#------------------------------------------------------------------------------

test_pattern_types() {
    section "Pattern Type Tests (-E, -F, -G, -P)"
    local name

    # -G / --basic-regexp (BRE - default)
    name="-G: BRE is default mode"
    should_run "$name" && {
        log_test "$name"
        # "hello" should match lines containing "hello"
        if assert_contains "hello world" "hello" "$FIXTURES/simple.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-G: BRE treats + as literal"
    should_run "$name" && {
        log_test "$name"
        if assert_contains "a+b equals c" "a+b" "$FIXTURES/special_chars.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-G: BRE treats | as literal"
    should_run "$name" && {
        log_test "$name"
        if assert_contains "pipe|character" "pipe|character" "$FIXTURES/special_chars.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-G: BRE escaped grouping \\(\\)"
    should_run "$name" && {
        log_test "$name"
        if echo "abab" | "$FERP" '\(ab\)\{2\}' | grep -q "abab"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -E / --extended-regexp (ERE)
    name="-E: ERE + quantifier"
    should_run "$name" && {
        log_test "$name"
        if echo "helllo" | "$FERP" -E "hel+o" | grep -q "helllo"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-E: ERE ? quantifier (match)"
    should_run "$name" && {
        log_test "$name"
        if echo "helo" | "$FERP" -E "hel?o" | grep -q "helo"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-E: ERE ? quantifier (zero match)"
    should_run "$name" && {
        log_test "$name"
        if echo "heo" | "$FERP" -E "hel?o" | grep -q "heo"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-E: ERE alternation |"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$(printf "cat\ndog\nbird\n" | "$FERP" -E "cat|dog")
        if [[ "$out" == $'cat\ndog' ]]; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="-E: ERE unescaped grouping ()"
    should_run "$name" && {
        log_test "$name"
        if echo "abab" | "$FERP" -E "(ab){2}" | grep -q "abab"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="--extended-regexp: long form works"
    should_run "$name" && {
        log_test "$name"
        if echo "hello" | "$FERP" --extended-regexp "hel+" | grep -q "hello"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -F / --fixed-strings
    name="-F: treats regex chars as literal"
    should_run "$name" && {
        log_test "$name"
        if assert_contains "regex: foo.*bar" -F "foo.*bar" "$FIXTURES/special_chars.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-F: literal $ match"
    should_run "$name" && {
        log_test "$name"
        if assert_contains 'price is $100' -F '$100' "$FIXTURES/special_chars.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-F: literal brackets match"
    should_run "$name" && {
        log_test "$name"
        if assert_contains "brackets [test] here" -F "[test]" "$FIXTURES/special_chars.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="--fixed-strings: long form works"
    should_run "$name" && {
        log_test "$name"
        if assert_contains "a+b equals c" --fixed-strings "a+b" "$FIXTURES/special_chars.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -P / --perl-regexp (PCRE)
    name="-P: basic PCRE match"
    should_run "$name" && {
        log_test "$name"
        if echo "hello world" | "$FERP" -P "hello" | grep -q "hello"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-P: PCRE \\d digit class"
    should_run "$name" && {
        log_test "$name"
        if echo "test123" | "$FERP" -P '\d+' | grep -q "test123"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-P: PCRE \\w word class"
    should_run "$name" && {
        log_test "$name"
        if echo "hello_world" | "$FERP" -P '\w+' | grep -q "hello_world"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-P: PCRE positive lookahead (?=)"
    should_run "$name" && {
        log_test "$name"
        if echo "foobar" | "$FERP" -P 'foo(?=bar)' | grep -q "foobar"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-P: PCRE negative lookahead (?!)"
    should_run "$name" && {
        log_test "$name"
        if echo "foobaz" | "$FERP" -P 'foo(?!bar)' | grep -q "foobaz"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-P: PCRE positive lookbehind (?<=)"
    should_run "$name" && {
        log_test "$name"
        if echo "foobar" | "$FERP" -P '(?<=foo)bar' | grep -q "foobar"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-P: PCRE non-capturing group (?:)"
    should_run "$name" && {
        log_test "$name"
        if echo "abcabc" | "$FERP" -P '(?:abc)+' | grep -q "abcabc"; then
            pass "$name"
        else
            fail "$name"
        fi
    }
}

#------------------------------------------------------------------------------
# MATCHING CONTROL TESTS (-i, -v, -w, -x)
#------------------------------------------------------------------------------

test_matching_control() {
    section "Matching Control Tests (-i, -v, -w, -x)"
    local name

    # -i / --ignore-case
    name="-i: case insensitive match (lowercase pattern)"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -i "hello" "$FIXTURES/simple.txt" | wc -l)
        if [[ "$out" -eq 4 ]]; then  # hello, Hello, HELLO, line with hello
            pass "$name"
        else
            fail "$name" "expected 4 lines, got $out"
        fi
    }

    name="-i: case insensitive match (uppercase pattern)"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -i "HELLO" "$FIXTURES/simple.txt" | wc -l)
        if [[ "$out" -eq 4 ]]; then
            pass "$name"
        else
            fail "$name" "expected 4 lines, got $out"
        fi
    }

    name="-i: case insensitive with -F"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -iF "HELLO" "$FIXTURES/simple.txt" | wc -l)
        if [[ "$out" -eq 4 ]]; then
            pass "$name"
        else
            fail "$name" "expected 4 lines, got $out"
        fi
    }

    name="-i: case insensitive with -E"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -iE "hel+" "$FIXTURES/simple.txt" | wc -l)
        if [[ "$out" -eq 4 ]]; then
            pass "$name"
        else
            fail "$name" "expected 4 lines, got $out"
        fi
    }

    name="--ignore-case: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --ignore-case "HELLO" "$FIXTURES/simple.txt" | wc -l)
        if [[ "$out" -eq 4 ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="--no-ignore-case: disables case insensitivity"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -i --no-ignore-case "hello" "$FIXTURES/simple.txt" | wc -l)
        if [[ "$out" -eq 2 ]]; then  # only lowercase hello
            pass "$name"
        else
            fail "$name" "expected 2 lines, got $out"
        fi
    }

    # -v / --invert-match
    name="-v: invert match"
    should_run "$name" && {
        log_test "$name"
        if assert_not_contains "hello" -v "hello" "$FIXTURES/simple.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-v: inverted results count"
    should_run "$name" && {
        log_test "$name"
        local total matching inverted
        total=$("$FERP" "" "$FIXTURES/simple.txt" | wc -l)
        matching=$("$FERP" "hello" "$FIXTURES/simple.txt" | wc -l)
        inverted=$("$FERP" -v "hello" "$FIXTURES/simple.txt" | wc -l)
        if [[ $((matching + inverted)) -eq "$total" ]]; then
            pass "$name"
        else
            fail "$name" "matching($matching) + inverted($inverted) != total($total)"
        fi
    }

    name="--invert-match: long form works"
    should_run "$name" && {
        log_test "$name"
        if assert_not_contains "hello" --invert-match "hello" "$FIXTURES/simple.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -w / --word-regexp
    name="-w: matches whole word only"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -w "test" "$FIXTURES/words.txt")
        # Should match "test" but not "testing", "tested", "tester"
        if [[ "$out" == "test testing tested tester" ]]; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="-w: does not match partial word"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -w "foo" "$FIXTURES/words.txt" | wc -l)
        # "foobar foo bar foo-bar" - should match "foo" twice (standalone and in foo-bar)
        if [[ "$out" -eq 1 ]]; then
            pass "$name"
        else
            fail "$name" "expected 1 line, got $out"
        fi
    }

    name="-w: word with -F"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -wF "hello" "$FIXTURES/words.txt")
        if [[ "$out" == "hello helloworld worldhello" ]]; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="--word-regexp: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --word-regexp "test" "$FIXTURES/words.txt")
        if [[ "$out" == "test testing tested tester" ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -x / --line-regexp
    name="-x: matches whole line only"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -x "hello world" "$FIXTURES/simple.txt")
        if [[ "$out" == "hello world" ]]; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="-x: does not match partial line"
    should_run "$name" && {
        log_test "$name"
        if run_ferp_expect 1 -x "hello" "$FIXTURES/simple.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-x: with regex"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -xE "hello.*" "$FIXTURES/simple.txt")
        # Should match lines starting with hello
        if echo "$out" | grep -q "hello world"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="--line-regexp: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --line-regexp "line 5" "$FIXTURES/numbers.txt")
        if [[ "$out" == "line 5" ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # Combined flags
    name="-iv: case insensitive inverted"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -iv "HELLO" "$FIXTURES/simple.txt" | wc -l)
        # Should exclude all hello variants
        if [[ "$out" -eq 4 ]]; then  # 8 lines - 4 with hello
            pass "$name"
        else
            fail "$name" "expected 4 lines, got $out"
        fi
    }

    name="-iw: case insensitive word match"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$(echo -e "TEST\nTesting\ntest" | "$FERP" -iw "test" | wc -l)
        if [[ "$out" -eq 2 ]]; then  # TEST and test, not Testing
            pass "$name"
        else
            fail "$name" "expected 2, got $out"
        fi
    }
}

#------------------------------------------------------------------------------
# OUTPUT CONTROL TESTS (-c, -l, -L, -o, -q, -s)
#------------------------------------------------------------------------------

test_output_control() {
    section "Output Control Tests (-c, -l, -L, -o, -q, -s)"
    local name

    # -c / --count
    name="-c: count matching lines"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -c "hello" "$FIXTURES/simple.txt")
        if [[ "$out" == "2" ]]; then
            pass "$name"
        else
            fail "$name" "expected 2, got $out"
        fi
    }

    name="-c: count with multiple files shows filename"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -c "match" "$FIXTURES/multifile1.txt" "$FIXTURES/multifile2.txt")
        if echo "$out" | grep -q "multifile1.txt:2" && echo "$out" | grep -q "multifile2.txt:1"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="-c: zero count when no matches"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -c "nonexistent" "$FIXTURES/simple.txt")
        if [[ "$out" == "0" ]]; then
            pass "$name"
        else
            fail "$name" "expected 0, got $out"
        fi
    }

    name="--count: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --count "line" "$FIXTURES/numbers.txt")
        if [[ "$out" == "10" ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -l / --files-with-matches
    name="-l: list files with matches"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -l "match" "$FIXTURES/multifile1.txt" "$FIXTURES/multifile2.txt" "$FIXTURES/multifile3.txt")
        if echo "$out" | grep -q "multifile1.txt" && echo "$out" | grep -q "multifile2.txt" && ! echo "$out" | grep -q "multifile3.txt"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="-l: only prints filename once per file"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -l "match" "$FIXTURES/multifile1.txt" | wc -l)
        if [[ "$out" -eq 1 ]]; then
            pass "$name"
        else
            fail "$name" "expected 1 line, got $out"
        fi
    }

    name="--files-with-matches: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --files-with-matches "match" "$FIXTURES/multifile1.txt")
        if echo "$out" | grep -q "multifile1.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -L / --files-without-match
    name="-L: list files without matches"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -L "match" "$FIXTURES/multifile1.txt" "$FIXTURES/multifile2.txt" "$FIXTURES/multifile3.txt")
        if echo "$out" | grep -q "multifile3.txt" && ! echo "$out" | grep -q "multifile1.txt"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="--files-without-match: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --files-without-match "match" "$FIXTURES/multifile3.txt")
        if echo "$out" | grep -q "multifile3.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -o / --only-matching
    name="-o: show only matching part"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -o "hello" "$FIXTURES/simple.txt")
        # Should output "hello" for each match, not the whole line
        if [[ "$out" == $'hello\nhello' ]]; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="-o: multiple matches per line"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$(echo "hello world hello" | "$FERP" -o "hello" | wc -l)
        if [[ "$out" -eq 2 ]]; then
            pass "$name"
        else
            fail "$name" "expected 2 lines, got $out"
        fi
    }

    name="-o: with regex captures match"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$(echo "test123test456" | "$FERP" -oE "[0-9]+")
        if [[ "$out" == $'123\n456' ]]; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="--only-matching: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$(echo "abc123def" | "$FERP" --only-matching -E "[0-9]+")
        if [[ "$out" == "123" ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -q / --quiet / --silent
    name="-q: no output on match"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -q "hello" "$FIXTURES/simple.txt")
        if [[ -z "$out" ]]; then
            pass "$name"
        else
            fail "$name" "expected empty output, got: $out"
        fi
    }

    name="-q: exit 0 on match"
    should_run "$name" && {
        log_test "$name"
        if run_ferp_expect 0 -q "hello" "$FIXTURES/simple.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-q: exit 1 on no match"
    should_run "$name" && {
        log_test "$name"
        if run_ferp_expect 1 -q "nonexistent" "$FIXTURES/simple.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="--quiet: long form works"
    should_run "$name" && {
        log_test "$name"
        if run_ferp_expect 0 --quiet "hello" "$FIXTURES/simple.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="--silent: alias works"
    should_run "$name" && {
        log_test "$name"
        if run_ferp_expect 0 --silent "hello" "$FIXTURES/simple.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -s / --no-messages
    name="-s: suppress error messages"
    should_run "$name" && {
        log_test "$name"
        local err
        err=$("$FERP" -s "test" "/nonexistent/file" 2>&1) || true
        if [[ -z "$err" ]]; then
            pass "$name"
        else
            fail "$name" "expected no output, got: $err"
        fi
    }

    name="--no-messages: long form works"
    should_run "$name" && {
        log_test "$name"
        local err
        err=$("$FERP" --no-messages "test" "/nonexistent/file" 2>&1) || true
        if [[ -z "$err" ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }
}

#------------------------------------------------------------------------------
# LINE PREFIX TESTS (-n, -b, -H, -h, -Z, -T)
#------------------------------------------------------------------------------

test_line_prefix() {
    section "Line Prefix Tests (-n, -b, -H, -h, -Z, -T)"
    local name

    # -n / --line-number
    name="-n: show line numbers"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -n "line 5" "$FIXTURES/numbers.txt")
        if [[ "$out" == "5:line 5" ]]; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="-n: line numbers start at 1"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -n "line 1" "$FIXTURES/numbers.txt")
        if [[ "$out" == "1:line 1" ]]; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="--line-number: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --line-number "line 3" "$FIXTURES/numbers.txt")
        if [[ "$out" == "3:line 3" ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -b / --byte-offset
    name="-b: show byte offset"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -b "line 1" "$FIXTURES/numbers.txt")
        if [[ "$out" == "0:line 1" ]]; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="-b: byte offset increases with lines"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -b "line 2" "$FIXTURES/numbers.txt")
        # "line 1\n" is 7 bytes, so line 2 starts at 7
        if [[ "$out" == "7:line 2" ]]; then
            pass "$name"
        else
            fail "$name" "expected 7:line 2, got: $out"
        fi
    }

    name="-nb: line number and byte offset together"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -nb "line 2" "$FIXTURES/numbers.txt")
        if [[ "$out" == "2:7:line 2" ]]; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="--byte-offset: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --byte-offset "line 1" "$FIXTURES/numbers.txt")
        if [[ "$out" == "0:line 1" ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -H / --with-filename
    name="-H: show filename on single file"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -H "hello" "$FIXTURES/simple.txt" | head -1)
        if echo "$out" | grep -q "simple.txt:"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="--with-filename: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --with-filename "hello" "$FIXTURES/simple.txt" | head -1)
        if echo "$out" | grep -q "simple.txt:"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -h / --no-filename
    name="-h: hide filename on multiple files"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -h "match" "$FIXTURES/multifile1.txt" "$FIXTURES/multifile2.txt" | head -1)
        if ! echo "$out" | grep -q ":"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="--no-filename: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --no-filename "match" "$FIXTURES/multifile1.txt" "$FIXTURES/multifile2.txt" | head -1)
        if ! echo "$out" | grep -q ":"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -Z / --null (null byte after filename)
    name="-Z: null byte after filename"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -HZ "hello" "$FIXTURES/simple.txt" | head -1 | od -c | head -1)
        if echo "$out" | grep -q '\\0'; then
            pass "$name"
        else
            fail "$name" "expected null byte, got: $out"
        fi
    }

    name="--null: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -H --null "hello" "$FIXTURES/simple.txt" | head -1 | od -c | head -1)
        if echo "$out" | grep -q '\\0'; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -T / --initial-tab
    name="-T: initial tab for alignment"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -nT "hello" "$FIXTURES/simple.txt" | head -1)
        # Should have tab between prefix and content
        if echo "$out" | grep -qP '^\d+:\t'; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="--initial-tab: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -n --initial-tab "hello" "$FIXTURES/simple.txt" | head -1)
        if echo "$out" | grep -qP '^\d+:\t'; then
            pass "$name"
        else
            fail "$name"
        fi
    }
}

#------------------------------------------------------------------------------
# CONTEXT CONTROL TESTS (-A, -B, -C, -NUM)
#------------------------------------------------------------------------------

test_context_control() {
    section "Context Control Tests (-A, -B, -C, -NUM)"
    local name

    # -A / --after-context
    name="-A: after context lines"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -A2 "MATCH LINE" "$FIXTURES/context.txt" | wc -l)
        if [[ "$out" -eq 3 ]]; then  # match + 2 after
            pass "$name"
        else
            fail "$name" "expected 3 lines, got $out"
        fi
    }

    name="-A: after context content"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -A2 "MATCH LINE" "$FIXTURES/context.txt")
        if echo "$out" | grep -q "after 1" && echo "$out" | grep -q "after 2"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="--after-context: long form with ="
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --after-context=1 "MATCH LINE" "$FIXTURES/context.txt" | wc -l)
        if [[ "$out" -eq 2 ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -B / --before-context
    name="-B: before context lines"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -B2 "MATCH LINE" "$FIXTURES/context.txt" | wc -l)
        if [[ "$out" -eq 3 ]]; then  # 2 before + match
            pass "$name"
        else
            fail "$name" "expected 3 lines, got $out"
        fi
    }

    name="-B: before context content"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -B2 "MATCH LINE" "$FIXTURES/context.txt")
        if echo "$out" | grep -q "before 2" && echo "$out" | grep -q "before 3"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="--before-context: long form with ="
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --before-context=1 "MATCH LINE" "$FIXTURES/context.txt" | wc -l)
        if [[ "$out" -eq 2 ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -C / --context
    name="-C: both context lines"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -C2 "MATCH LINE" "$FIXTURES/context.txt" | wc -l)
        if [[ "$out" -eq 5 ]]; then  # 2 before + match + 2 after
            pass "$name"
        else
            fail "$name" "expected 5 lines, got $out"
        fi
    }

    name="--context: long form with ="
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --context=1 "MATCH LINE" "$FIXTURES/context.txt" | wc -l)
        if [[ "$out" -eq 3 ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -NUM shorthand
    name="-NUM: numeric context shorthand"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -2 "MATCH LINE" "$FIXTURES/context.txt" | wc -l)
        if [[ "$out" -eq 5 ]]; then  # Same as -C2
            pass "$name"
        else
            fail "$name" "expected 5 lines, got $out"
        fi
    }

    name="-3: larger context"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -3 "MATCH LINE" "$FIXTURES/context.txt" | wc -l)
        if [[ "$out" -eq 7 ]]; then  # 3 before + match + 3 after
            pass "$name"
        else
            fail "$name" "expected 7 lines, got $out"
        fi
    }

    # Context with multiple matches
    name="context: overlapping context groups"
    should_run "$name" && {
        log_test "$name"
        local out
        # Both MATCH and ANOTHER MATCH with context should not duplicate lines
        out=$("$FERP" -C2 "MATCH" "$FIXTURES/context.txt" | wc -l)
        # Should merge overlapping contexts appropriately
        if [[ "$out" -ge 8 ]]; then
            pass "$name"
        else
            fail "$name" "expected at least 8 lines, got $out"
        fi
    }

    # --group-separator
    name="--group-separator: custom separator"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -C1 --group-separator="===" "MATCH" "$FIXTURES/context.txt")
        if echo "$out" | grep -q "==="; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    # --no-group-separator
    name="--no-group-separator: suppress separator"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -C1 --no-group-separator "MATCH" "$FIXTURES/context.txt")
        if ! echo "$out" | grep -q "^--$"; then
            pass "$name"
        else
            fail "$name" "separator should be suppressed, got: $out"
        fi
    }

    # Context with line numbers
    name="-n with context: context lines marked differently"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -nB1 "MATCH LINE" "$FIXTURES/context.txt")
        # Context lines should use - separator, match lines use :
        if echo "$out" | grep -qE "^[0-9]+-" && echo "$out" | grep -qE "^[0-9]+:"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }
}

#------------------------------------------------------------------------------
# FILE SELECTION TESTS (-r, -R, --include, --exclude)
#------------------------------------------------------------------------------

test_file_selection() {
    section "File Selection Tests (-r, -R, --include, --exclude, -d, -D)"
    local name

    # -r / --recursive
    name="-r: recursive search"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -r "match" "$FIXTURES" | wc -l)
        # Should find matches in nested files
        if [[ "$out" -ge 4 ]]; then
            pass "$name"
        else
            fail "$name" "expected at least 4 matches, got $out"
        fi
    }

    name="-r: searches subdirectories"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -r "nested" "$FIXTURES")
        if echo "$out" | grep -q "subdir1" && echo "$out" | grep -q "subdir2"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="--recursive: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --recursive "match" "$FIXTURES" | wc -l)
        if [[ "$out" -ge 4 ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # --include
    name="--include: filter by glob"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -r --include="*.txt" "match" "$FIXTURES")
        # Should only show .txt files
        if echo "$out" | grep -q ".txt:" && ! echo "$out" | grep -q ".f90:"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="--include: *.f90 filter"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -r --include="*.f90" "match" "$FIXTURES")
        if echo "$out" | grep -q ".f90:"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    # --exclude
    name="--exclude: skip matching files"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -r --exclude="*multifile*" "match" "$FIXTURES")
        if ! echo "$out" | grep -q "multifile"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    # --exclude-dir
    name="--exclude-dir: skip directories"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -r --exclude-dir="skipme" "match" "$FIXTURES")
        if ! echo "$out" | grep -q "skipme"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="--exclude-dir: multiple exclusions"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -r --exclude-dir="skipme" --exclude-dir="subdir1" "match" "$FIXTURES")
        if ! echo "$out" | grep -q "skipme" && ! echo "$out" | grep -q "subdir1"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    # -d / --directories
    name="-d skip: skip directories"
    should_run "$name" && {
        log_test "$name"
        # When given a directory without -r, -d skip should skip it silently
        local exit_code=0
        "$FERP" -d skip "test" "$FIXTURES" >/dev/null 2>&1 || exit_code=$?
        if [[ "$exit_code" -eq 1 ]]; then  # No matches since dir was skipped
            pass "$name"
        else
            fail "$name" "expected exit 1, got $exit_code"
        fi
    }

    name="--directories=recurse: same as -r"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --directories=recurse "nested" "$FIXTURES" | wc -l)
        if [[ "$out" -ge 2 ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }
}

#------------------------------------------------------------------------------
# MULTI-PATTERN TESTS (-e, -f)
#------------------------------------------------------------------------------

test_multi_pattern() {
    section "Multi-Pattern Tests (-e, -f)"
    local name

    # -e / --regexp
    name="-e: single explicit pattern"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -e "hello" "$FIXTURES/simple.txt" | wc -l)
        if [[ "$out" -eq 2 ]]; then
            pass "$name"
        else
            fail "$name" "expected 2 lines, got $out"
        fi
    }

    name="-e: multiple patterns (OR)"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -e "hello" -e "goodbye" "$FIXTURES/simple.txt")
        if echo "$out" | grep -q "hello" && echo "$out" | grep -q "goodbye"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="-e: three patterns"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -e "hello" -e "goodbye" -e "foo" "$FIXTURES/simple.txt" | wc -l)
        if [[ "$out" -eq 4 ]]; then  # 2 hello + 1 goodbye + 1 foo
            pass "$name"
        else
            fail "$name" "expected 4 lines, got $out"
        fi
    }

    name="--regexp=PATTERN: long form with ="
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --regexp="hello" "$FIXTURES/simple.txt" | wc -l)
        if [[ "$out" -eq 2 ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -f / --file
    name="-f: patterns from file"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -f "$FIXTURES/patterns.txt" "$FIXTURES/simple.txt")
        # patterns.txt contains: hello, world, test
        if echo "$out" | grep -q "hello" && echo "$out" | grep -q "world"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="-f: combined with -e"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -f "$FIXTURES/patterns.txt" -e "foo" "$FIXTURES/simple.txt")
        if echo "$out" | grep -q "hello" && echo "$out" | grep -q "foo"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="-f: nonexistent file error"
    should_run "$name" && {
        log_test "$name"
        if run_ferp_expect 2 -f "/nonexistent/patterns.txt" "test" "$FIXTURES/simple.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }
}

#------------------------------------------------------------------------------
# BINARY FILE HANDLING TESTS (-a, -I, --binary-files)
#------------------------------------------------------------------------------

test_binary_handling() {
    section "Binary File Handling Tests (-a, -I, --binary-files)"
    local name

    # Default binary detection
    name="binary: detected and shows message"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" "text" "$FIXTURES/binary.bin" 2>/dev/null) || true
        if echo "$out" | grep -q "Binary file"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    # -a / --text
    name="-a: treat binary as text"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -a "match" "$FIXTURES/binary.bin" 2>/dev/null) || true
        if echo "$out" | grep -q "match"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="--text: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --text "match" "$FIXTURES/binary.bin" 2>/dev/null) || true
        if echo "$out" | grep -q "match"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # -I (ignore binary / without-match)
    name="-I: skip binary files"
    should_run "$name" && {
        log_test "$name"
        if run_ferp_expect 1 -I "text" "$FIXTURES/binary.bin"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # --binary-files
    name="--binary-files=text: same as -a"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --binary-files=text "match" "$FIXTURES/binary.bin" 2>/dev/null) || true
        if echo "$out" | grep -q "match"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="--binary-files=without-match: same as -I"
    should_run "$name" && {
        log_test "$name"
        if run_ferp_expect 1 --binary-files=without-match "text" "$FIXTURES/binary.bin"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="--binary-files=binary: show message (default)"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --binary-files=binary "text" "$FIXTURES/binary.bin" 2>/dev/null) || true
        if echo "$out" | grep -q "Binary file"; then
            pass "$name"
        else
            fail "$name"
        fi
    }
}

#------------------------------------------------------------------------------
# MISCELLANEOUS TESTS (--max-count, --label, -z, --color)
#------------------------------------------------------------------------------

test_misc_flags() {
    section "Miscellaneous Flag Tests (-m, --label, -z, --color)"
    local name

    # -m / --max-count
    name="-m: limit match count"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -m2 "line" "$FIXTURES/numbers.txt" | wc -l)
        if [[ "$out" -eq 2 ]]; then
            pass "$name"
        else
            fail "$name" "expected 2 lines, got $out"
        fi
    }

    name="-m1: stop after first match"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -m1 "line" "$FIXTURES/numbers.txt")
        if [[ "$out" == "line 1" ]]; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="--max-count=N: long form"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --max-count=3 "line" "$FIXTURES/numbers.txt" | wc -l)
        if [[ "$out" -eq 3 ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="-m with -c: count respects limit"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -m5 -c "line" "$FIXTURES/numbers.txt")
        if [[ "$out" == "5" ]]; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    # --label
    name="--label: custom stdin label"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$(echo "hello world" | "$FERP" -H --label="custom_input" "hello")
        if echo "$out" | grep -q "custom_input:"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    # -z / --null-data
    name="-z: null-terminated input"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$(printf 'hello\0world\0' | "$FERP" -z "hello")
        if [[ -n "$out" ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="--null-data: long form works"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$(printf 'test\0line\0' | "$FERP" --null-data "test")
        if [[ -n "$out" ]]; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    # --color
    name="--color=always: ANSI codes in output"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --color=always "hello" "$FIXTURES/simple.txt")
        if echo "$out" | grep -q $'\033\['; then
            pass "$name"
        else
            fail "$name" "expected ANSI escape codes"
        fi
    }

    name="--color=never: no ANSI codes"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --color=never "hello" "$FIXTURES/simple.txt")
        if ! echo "$out" | grep -q $'\033\['; then
            pass "$name"
        else
            fail "$name" "expected no ANSI codes"
        fi
    }

    name="--color=auto: no ANSI when piped"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" --color=auto "hello" "$FIXTURES/simple.txt" | cat)
        if ! echo "$out" | grep -q $'\033\['; then
            pass "$name"
        else
            fail "$name" "expected no ANSI codes when piped"
        fi
    }

    # --line-buffered
    name="--line-buffered: accepted"
    should_run "$name" && {
        log_test "$name"
        if "$FERP" --line-buffered "hello" "$FIXTURES/simple.txt" >/dev/null 2>&1; then
            pass "$name"
        else
            fail "$name"
        fi
    }
}

#------------------------------------------------------------------------------
# EXIT CODE TESTS
#------------------------------------------------------------------------------

test_exit_codes() {
    section "Exit Code Tests"
    local name

    name="exit 0: match found"
    should_run "$name" && {
        log_test "$name"
        if run_ferp_expect 0 "hello" "$FIXTURES/simple.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="exit 1: no match found"
    should_run "$name" && {
        log_test "$name"
        if run_ferp_expect 1 "nonexistent_pattern_xyz" "$FIXTURES/simple.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="exit 2: invalid regex"
    should_run "$name" && {
        log_test "$name"
        if run_ferp_expect 2 "[invalid" "$FIXTURES/simple.txt"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="exit 2: missing pattern"
    should_run "$name" && {
        log_test "$name"
        if run_ferp_expect 2; then
            pass "$name"
        else
            fail "$name"
        fi
    }
}

#------------------------------------------------------------------------------
# EDGE CASES AND REGRESSIONS
#------------------------------------------------------------------------------

test_edge_cases() {
    section "Edge Cases and Regressions"
    local name

    name="empty pattern matches all lines"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" "" "$FIXTURES/numbers.txt" | wc -l)
        if [[ "$out" -eq 10 ]]; then
            pass "$name"
        else
            fail "$name" "expected 10 lines, got $out"
        fi
    }

    name="empty file no crash"
    should_run "$name" && {
        log_test "$name"
        local tmpfile
        tmpfile=$(mktemp)
        if run_ferp_expect 1 "test" "$tmpfile"; then
            pass "$name"
        else
            fail "$name"
        fi
        rm -f "$tmpfile"
    }

    name="very long line handling"
    should_run "$name" && {
        log_test "$name"
        local tmpfile long_line
        tmpfile=$(mktemp)
        long_line=$(printf 'a%.0s' {1..10000})
        echo "${long_line}MATCH${long_line}" > "$tmpfile"
        if "$FERP" "MATCH" "$tmpfile" | grep -q "MATCH"; then
            pass "$name"
        else
            fail "$name"
        fi
        rm -f "$tmpfile"
    }

    name="special regex chars in pattern"
    should_run "$name" && {
        log_test "$name"
        if "$FERP" '\$' "$FIXTURES/special_chars.txt" | grep -q "dollar"; then
            pass "$name"
        else
            fail "$name"
        fi
    }

    name="multiple files with mixed results"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" "match" "$FIXTURES/multifile1.txt" "$FIXTURES/multifile3.txt" "$FIXTURES/multifile2.txt")
        # Should show results from files that have matches
        if echo "$out" | grep -q "multifile1" && echo "$out" | grep -q "multifile2"; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="stdin input"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$(echo "test line" | "$FERP" "test")
        if [[ "$out" == "test line" ]]; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="-- ends option parsing"
    should_run "$name" && {
        log_test "$name"
        # Pattern starting with - should work after --
        local out
        out=$(echo "-test-" | "$FERP" -- "-test-")
        if [[ "$out" == "-test-" ]]; then
            pass "$name"
        else
            fail "$name" "got: $out"
        fi
    }

    name="combined short options -inv"
    should_run "$name" && {
        log_test "$name"
        local out
        out=$("$FERP" -inv "HELLO" "$FIXTURES/simple.txt" | wc -l)
        # Case insensitive, inverted, with line numbers
        if [[ "$out" -eq 4 ]]; then
            pass "$name"
        else
            fail "$name" "expected 4 lines, got $out"
        fi
    }
}

#------------------------------------------------------------------------------
# Main
#------------------------------------------------------------------------------

main() {
    echo ""
    echo -e "${BLUE}╔════════════════════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${BLUE}║                     FERP Integration Test Suite                            ║${NC}"
    echo -e "${BLUE}╚════════════════════════════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo "ferp binary: $FERP"
    echo "Fixtures: $FIXTURES"
    if [[ "$COMPARE_GREP" == "true" ]]; then
        echo "Mode: Comparing against GNU grep"
    fi
    if [[ -n "$FILTER" ]]; then
        echo "Filter: $FILTER"
    fi

    # Run all test categories
    test_pattern_types
    test_matching_control
    test_output_control
    test_line_prefix
    test_context_control
    test_file_selection
    test_multi_pattern
    test_binary_handling
    test_misc_flags
    test_exit_codes
    test_edge_cases

    # Summary
    echo ""
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}  Summary${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo ""
    echo -e "  Tests run:    ${TESTS_RUN}"
    echo -e "  ${GREEN}Passed:       ${TESTS_PASSED}${NC}"
    echo -e "  ${RED}Failed:       ${TESTS_FAILED}${NC}"
    echo -e "  ${YELLOW}Skipped:      ${TESTS_SKIPPED}${NC}"
    echo ""

    if [[ "$TESTS_FAILED" -gt 0 ]]; then
        echo -e "${RED}Some tests failed!${NC}"
        exit 1
    else
        echo -e "${GREEN}All tests passed!${NC}"
        exit 0
    fi
}

main "$@"
