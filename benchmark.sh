#!/usr/bin/env bash
#
# FERP vs grep Benchmark Suite
# Comprehensive performance comparison
#
# Requires: bash 4+, bc, python3 (for timing)
#

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Configuration
BENCH_DIR="/tmp/ferp_benchmark_$$"
FERP="./ferp"
GREP="grep"
RUNS=3  # Number of runs per benchmark (take median)

# Test file sizes
SMALL_LINES=10000      # ~700KB
MEDIUM_LINES=100000    # ~7MB
LARGE_LINES=1000000    # ~70MB

# Results storage (simple arrays for portability)
RESULT_NAMES=()
RESULT_FERP_TIMES=()
RESULT_GREP_TIMES=()

#------------------------------------------------------------------------------
# Utility Functions
#------------------------------------------------------------------------------

cleanup() {
    echo -e "\n${CYAN}Cleaning up...${NC}"
    rm -rf "$BENCH_DIR"
}

trap cleanup EXIT

die() {
    echo -e "${RED}ERROR: $1${NC}" >&2
    exit 1
}

check_prerequisites() {
    echo -e "${CYAN}Checking prerequisites...${NC}"

    # Check ferp exists
    if [[ ! -x "$FERP" ]]; then
        echo -e "${YELLOW}Building ferp (release mode)...${NC}"
        make release >/dev/null 2>&1 || die "Failed to build ferp"
    fi

    # Verify ferp works
    echo "test" | $FERP "test" >/dev/null 2>&1 || die "ferp not working"

    # Check grep exists
    command -v $GREP >/dev/null 2>&1 || die "grep not found"

    echo -e "${GREEN}Prerequisites OK${NC}"
}

create_test_files() {
    echo -e "\n${CYAN}Creating test files in $BENCH_DIR...${NC}"
    mkdir -p "$BENCH_DIR"

    # File 1: English-like text (varied content) - use awk for speed
    echo -e "  Creating english text file ($LARGE_LINES lines)..."
    awk -v n="$LARGE_LINES" 'BEGIN {
        lines[0] = "The quick brown fox jumps over the lazy dog near the riverbank."
        lines[1] = "Hello world, this is line number %d of the benchmark test file."
        lines[2] = "Lorem ipsum dolor sit amet, consectetur adipiscing elit sed do."
        lines[3] = "Error: connection timeout after 30000ms on server node-%d."
        lines[4] = "DEBUG [2024-01-15 10:23:45] Processing request id=%d status=pending"
        lines[5] = "user@example.com logged in from 192.168.1.%d at 12:00:00"
        lines[6] = "WARNING: disk usage at %d%% on /dev/sda1 partition"
        lines[7] = "Function calculate_total(items=[1,2,3]) returned value=%d"
        lines[8] = "The API endpoint /api/v2/users/%d responded with HTTP 200 OK"
        lines[9] = "Configuration: max_threads=16, timeout=5000, retry_count=3"
        for (i = 1; i <= n; i++) {
            idx = i % 10
            if (idx == 0 || idx == 2 || idx == 9) {
                print lines[idx]
            } else if (idx == 3) {
                printf lines[idx] "\n", i % 100
            } else if (idx == 5) {
                printf lines[idx] "\n", i % 256
            } else if (idx == 6) {
                printf lines[idx] "\n", 50 + (i % 50)
            } else if (idx == 7) {
                printf lines[idx] "\n", i * 42
            } else {
                printf lines[idx] "\n", i
            }
        }
    }' > "$BENCH_DIR/english_large.txt"

    # File 2: Log-like file (structured)
    echo -e "  Creating log file ($MEDIUM_LINES lines)..."
    awk -v n="$MEDIUM_LINES" 'BEGIN {
        levels[0] = "INFO"; levels[1] = "DEBUG"; levels[2] = "WARN"; levels[3] = "ERROR"
        for (i = 1; i <= n; i++) {
            day = 1 + (i % 28)
            hour = i % 24
            min = i % 60
            sec = i % 60
            comp = i % 20
            printf "[2024-01-%02d %02d:%02d:%02d] %s: Message number %d from component-%d\n", \
                   day, hour, min, sec, levels[i % 4], i, comp
        }
    }' > "$BENCH_DIR/logs_medium.txt"

    # File 3: Code-like file
    echo -e "  Creating code file ($MEDIUM_LINES lines)..."
    awk -v n="$MEDIUM_LINES" 'BEGIN {
        for (i = 1; i <= n; i++) {
            idx = i % 8
            if (idx == 0) printf "function process_data_%d(input) {\n", i
            else if (idx == 1) print "    const result = input.map(x => x * 2);"
            else if (idx == 2) print "    if (result.length > 0) {"
            else if (idx == 3) print "        console.log(\"Processing:\", result);"
            else if (idx == 4) print "        return result.filter(x => x > 10);"
            else if (idx == 5) print "    }"
            else if (idx == 6) print "    return [];"
            else print "}"
        }
    }' > "$BENCH_DIR/code_medium.txt"

    # File 4: CSV-like data
    echo -e "  Creating CSV file ($MEDIUM_LINES lines)..."
    awk -v n="$MEDIUM_LINES" 'BEGIN {
        print "id,name,email,score,timestamp"
        srand()
        for (i = 1; i <= n; i++) {
            score = int(rand() * 100)
            printf "%d,user_%d,user%d@domain%d.com,%d,%d\n", \
                   i, i, i, i % 100, score, 1700000000 + i
        }
    }' > "$BENCH_DIR/data_medium.csv"

    # File 5: Small file for quick tests
    echo -e "  Creating small file ($SMALL_LINES lines)..."
    head -n $SMALL_LINES "$BENCH_DIR/english_large.txt" > "$BENCH_DIR/english_small.txt"

    # Print file sizes
    echo -e "\n${CYAN}Test files created:${NC}"
    ls -lh "$BENCH_DIR"/*.txt "$BENCH_DIR"/*.csv 2>/dev/null | awk '{print "  " $9 ": " $5}'
}

#------------------------------------------------------------------------------
# Benchmark Functions
#------------------------------------------------------------------------------

# Run a command multiple times and return median time
run_timed() {
    local cmd="$1"
    local times=()

    for i in $(seq 1 $RUNS); do
        # Use /usr/bin/time for portable timing
        local t=$( { time eval "$cmd" >/dev/null 2>&1; } 2>&1 | grep real | sed 's/real[[:space:]]*//' )
        # Convert to seconds (handles both 0m0.123s and 0.123 formats)
        if [[ "$t" =~ ([0-9]+)m([0-9.]+)s ]]; then
            local mins="${BASH_REMATCH[1]}"
            local secs="${BASH_REMATCH[2]}"
            t=$(echo "$mins * 60 + $secs" | bc -l)
        elif [[ "$t" =~ ^[0-9.]+$ ]]; then
            : # already in seconds
        else
            t="999"  # Error case
        fi
        times+=("$t")
    done

    # Return median (sort and take middle)
    printf '%s\n' "${times[@]}" | sort -n | sed -n "$((($RUNS + 1) / 2))p"
}

# Alternative timing using date (more portable)
run_timed_portable() {
    local cmd="$1"
    local times=()

    for i in $(seq 1 $RUNS); do
        local start=$(python3 -c 'import time; print(time.time())' 2>/dev/null || date +%s.%N)
        eval "$cmd" >/dev/null 2>&1
        local end=$(python3 -c 'import time; print(time.time())' 2>/dev/null || date +%s.%N)
        local t=$(echo "$end - $start" | bc -l)
        times+=("$t")
    done

    # Return median
    printf '%s\n' "${times[@]}" | sort -n | sed -n "$((($RUNS + 1) / 2))p"
}

benchmark_pattern() {
    local name="$1"
    local file="$2"
    local ferp_args="$3"
    local grep_args="$4"
    local pattern="$5"

    printf "  %-35s" "$name"

    # Run ferp
    local ferp_time=$(run_timed_portable "$FERP $ferp_args '$pattern' '$file'")

    # Run grep
    local grep_time=$(run_timed_portable "$GREP $grep_args '$pattern' '$file'")

    # Calculate speedup
    local speedup=$(echo "scale=2; $grep_time / $ferp_time" | bc -l 2>/dev/null || echo "N/A")

    # Store results
    RESULT_NAMES+=("$name")
    RESULT_FERP_TIMES+=("$ferp_time")
    RESULT_GREP_TIMES+=("$grep_time")

    # Color-code the speedup
    local color="$NC"
    if (( $(echo "$speedup > 1.5" | bc -l) )); then
        color="$GREEN"
    elif (( $(echo "$speedup < 0.8" | bc -l) )); then
        color="$RED"
    fi

    printf "ferp: %6.3fs  grep: %6.3fs  ${color}%5.2fx${NC}\n" "$ferp_time" "$grep_time" "$speedup"
}

#------------------------------------------------------------------------------
# Benchmark Suites
#------------------------------------------------------------------------------

run_literal_benchmarks() {
    echo -e "\n${BOLD}${BLUE}=== Literal String Matching ===${NC}"
    local file="$BENCH_DIR/english_large.txt"

    benchmark_pattern "Simple word (hello)" "$file" "" "" "hello"
    benchmark_pattern "Common word (the)" "$file" "" "" "the"
    benchmark_pattern "Longer phrase (quick brown)" "$file" "" "" "quick brown"
    benchmark_pattern "Case insensitive (-i hello)" "$file" "-i" "-i" "hello"
    benchmark_pattern "Fixed string (-F hello)" "$file" "-F" "-F" "hello"
    benchmark_pattern "Word boundary (-w the)" "$file" "-w" "-w" "the"
}

run_regex_benchmarks() {
    echo -e "\n${BOLD}${BLUE}=== Regular Expression Matching ===${NC}"
    local file="$BENCH_DIR/english_large.txt"

    benchmark_pattern "Dot wildcard (h.llo)" "$file" "" "" "h.llo"
    benchmark_pattern "Star quantifier (hel*o)" "$file" "" "" "hel*o"
    benchmark_pattern "Character class ([a-z]+)" "$file" "-E" "-E" "[a-z]+"
    benchmark_pattern "Mixed class ([a-zA-Z0-9]+)" "$file" "-E" "-E" "[a-zA-Z0-9]+"
    benchmark_pattern "Digit class ([0-9]+)" "$file" "-E" "-E" "[0-9]+"
    benchmark_pattern "Alternation (cat|dog|fox)" "$file" "-E" "-E" "cat|dog|fox"
    benchmark_pattern "Optional (colou?r)" "$file" "-E" "-E" "colou?r"
    benchmark_pattern "One or more (hel+o)" "$file" "-E" "-E" "hel+o"
}

run_anchor_benchmarks() {
    echo -e "\n${BOLD}${BLUE}=== Anchor Patterns ===${NC}"
    local file="$BENCH_DIR/english_large.txt"

    benchmark_pattern "Start anchor (^The)" "$file" "" "" "^The"
    benchmark_pattern "End anchor (\\.$)" "$file" "" "" '\.$'
    benchmark_pattern "Both anchors (^The.*dog$)" "$file" "-E" "-E" "^The.*dog$"
    benchmark_pattern "Word start (\\<quick)" "$file" "" "" '\<quick'
    benchmark_pattern "Word end (fox\\>)" "$file" "" "" 'fox\>'
}

run_log_benchmarks() {
    echo -e "\n${BOLD}${BLUE}=== Log File Patterns ===${NC}"
    local file="$BENCH_DIR/logs_medium.txt"

    benchmark_pattern "Log level (ERROR)" "$file" "" "" "ERROR"
    benchmark_pattern "Log level (-i warn)" "$file" "-i" "-i" "warn"
    benchmark_pattern "Timestamp pattern ([0-9]{2}:[0-9]{2})" "$file" "-E" "-E" "[0-9]{2}:[0-9]{2}"
    benchmark_pattern "Component (component-[0-9]+)" "$file" "-E" "-E" "component-[0-9]+"
    benchmark_pattern "Multiple levels (ERROR|WARN)" "$file" "-E" "-E" "ERROR|WARN"
}

run_code_benchmarks() {
    echo -e "\n${BOLD}${BLUE}=== Code Pattern Matching ===${NC}"
    local file="$BENCH_DIR/code_medium.txt"

    benchmark_pattern "Function name (function)" "$file" "" "" "function"
    benchmark_pattern "Variable (const|let|var)" "$file" "-E" "-E" "const|let|var"
    benchmark_pattern "Return statement (return)" "$file" "" "" "return"
    benchmark_pattern "Console log (console\\.log)" "$file" "-E" "-E" "console\\.log"
}

run_csv_benchmarks() {
    echo -e "\n${BOLD}${BLUE}=== CSV/Data Pattern Matching ===${NC}"
    local file="$BENCH_DIR/data_medium.csv"

    benchmark_pattern "Email pattern (@.*\\.com)" "$file" "-E" "-E" "@.*\\.com"
    benchmark_pattern "Specific domain (domain50)" "$file" "" "" "domain50"
    benchmark_pattern "User pattern (user_[0-9]+)" "$file" "-E" "-E" "user_[0-9]+"
    benchmark_pattern "High score (,[89][0-9],)" "$file" "-E" "-E" ",[89][0-9],"
}

run_special_benchmarks() {
    echo -e "\n${BOLD}${BLUE}=== Special Cases ===${NC}"
    local file="$BENCH_DIR/english_large.txt"

    benchmark_pattern "Invert match (-v error)" "$file" "-v" "-v" "error"
    benchmark_pattern "Count only (-c the)" "$file" "-c" "-c" "the"
    benchmark_pattern "Line number (-n hello)" "$file" "-n" "-n" "hello"
    benchmark_pattern "Multiple patterns (cat|dog|bird|fish)" "$file" "-E" "-E" "cat|dog|bird|fish"
    benchmark_pattern "Long alternation (the|and|for|with|from)" "$file" "-E" "-E" "the|and|for|with|from"
}

run_scaling_benchmarks() {
    echo -e "\n${BOLD}${BLUE}=== Scaling Tests ===${NC}"

    echo -e "  ${CYAN}Small file (~700KB):${NC}"
    benchmark_pattern "  [a-z]+ on small" "$BENCH_DIR/english_small.txt" "-E" "-E" "[a-z]+"

    echo -e "  ${CYAN}Large file (~70MB):${NC}"
    benchmark_pattern "  [a-z]+ on large" "$BENCH_DIR/english_large.txt" "-E" "-E" "[a-z]+"

    # Calculate scaling factor (get last two results)
    local num_results=${#RESULT_FERP_TIMES[@]}
    local small_ferp="${RESULT_FERP_TIMES[$((num_results-2))]}"
    local large_ferp="${RESULT_FERP_TIMES[$((num_results-1))]}"
    local small_grep="${RESULT_GREP_TIMES[$((num_results-2))]}"
    local large_grep="${RESULT_GREP_TIMES[$((num_results-1))]}"

    echo -e "\n  ${CYAN}Scaling (large/small ratio):${NC}"
    local ferp_scale=$(echo "scale=1; $large_ferp / $small_ferp" | bc -l 2>/dev/null || echo "N/A")
    local grep_scale=$(echo "scale=1; $large_grep / $small_grep" | bc -l 2>/dev/null || echo "N/A")
    echo -e "    ferp: ${ferp_scale}x  grep: ${grep_scale}x  (lower is better for large files)"
}

#------------------------------------------------------------------------------
# Report Generation
#------------------------------------------------------------------------------

print_summary() {
    echo -e "\n${BOLD}${BLUE}══════════════════════════════════════════════════════════════${NC}"
    echo -e "${BOLD}${BLUE}                        SUMMARY                                 ${NC}"
    echo -e "${BOLD}${BLUE}══════════════════════════════════════════════════════════════${NC}"

    local total_ferp=0
    local total_grep=0
    local wins_ferp=0
    local wins_grep=0
    local count=${#RESULT_NAMES[@]}

    for i in "${!RESULT_NAMES[@]}"; do
        local ft="${RESULT_FERP_TIMES[$i]}"
        local gt="${RESULT_GREP_TIMES[$i]}"
        total_ferp=$(echo "$total_ferp + $ft" | bc -l)
        total_grep=$(echo "$total_grep + $gt" | bc -l)

        if (( $(echo "$ft < $gt" | bc -l) )); then
            wins_ferp=$((wins_ferp + 1))
        else
            wins_grep=$((wins_grep + 1))
        fi
    done

    local avg_speedup=$(echo "scale=2; $total_grep / $total_ferp" | bc -l 2>/dev/null || echo "N/A")

    echo -e "\n${CYAN}Overall Statistics:${NC}"
    echo -e "  Total benchmarks run: $count"
    echo -e "  ferp wins: ${GREEN}$wins_ferp${NC}"
    echo -e "  grep wins: ${RED}$wins_grep${NC}"
    printf "  Total time - ferp: %.3fs  grep: %.3fs\n" "$total_ferp" "$total_grep"
    echo -e "  ${BOLD}Average speedup: ${GREEN}${avg_speedup}x${NC}"

    echo -e "\n${CYAN}System Information:${NC}"
    echo -e "  OS: $(uname -s) $(uname -r)"
    echo -e "  CPU: $(sysctl -n machdep.cpu.brand_string 2>/dev/null || lscpu 2>/dev/null | grep 'Model name' | cut -d: -f2 | xargs || echo 'Unknown')"
    echo -e "  ferp version: $($FERP --version 2>&1 | head -1 || echo 'Unknown')"
    echo -e "  grep version: $($GREP --version 2>&1 | head -1 || echo 'Unknown')"
    echo -e "  Runs per benchmark: $RUNS (median taken)"

    echo -e "\n${BOLD}${BLUE}══════════════════════════════════════════════════════════════${NC}"
}

#------------------------------------------------------------------------------
# Main
#------------------------------------------------------------------------------

main() {
    echo -e "${BOLD}${BLUE}"
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║           FERP vs grep Benchmark Suite                       ║"
    echo "║           Comprehensive Performance Comparison               ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
    echo -e "${NC}"

    check_prerequisites
    create_test_files

    echo -e "\n${BOLD}${CYAN}Running benchmarks (${RUNS} runs each, reporting median)...${NC}"
    echo -e "${CYAN}Format: ferp time | grep time | speedup (>1 = ferp faster)${NC}\n"

    run_literal_benchmarks
    run_regex_benchmarks
    run_anchor_benchmarks
    run_log_benchmarks
    run_code_benchmarks
    run_csv_benchmarks
    run_special_benchmarks
    run_scaling_benchmarks

    print_summary

    echo -e "\n${GREEN}Benchmark complete!${NC}"
}

# Run with optional arguments
if [[ "$1" == "-h" || "$1" == "--help" ]]; then
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -r, --runs N    Number of runs per benchmark (default: 3)"
    echo "  -q, --quick     Quick mode (smaller files, fewer runs)"
    echo "  -h, --help      Show this help"
    exit 0
fi

if [[ "$1" == "-q" || "$1" == "--quick" ]]; then
    RUNS=1
    SMALL_LINES=1000
    MEDIUM_LINES=10000
    LARGE_LINES=100000
    echo -e "${YELLOW}Quick mode: reduced file sizes and single run${NC}"
fi

if [[ "$1" == "-r" || "$1" == "--runs" ]]; then
    RUNS="${2:-3}"
fi

main
