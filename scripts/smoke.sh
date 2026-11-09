#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

echo "Building textbook-word-sorter..."
cabal build exe:textbook-word-sorter >/dev/null

BIN="$(cabal list-bin exe:textbook-word-sorter)"
PASS=0

pass() {
  PASS=$((PASS + 1))
  echo "PASS $1"
}

fail() {
  echo "FAIL $1" >&2
  exit 1
}

run_ok() {
  local description="$1"
  shift
  local output
  if ! output="$("$@" 2>&1)"; then
    fail "$description (unexpected non-zero exit)"
  fi
  printf '%s' "$output"
}

run_fail() {
  local expected_code="$1"
  local description="$2"
  shift 2
  local output code
  set +e
  output="$("$@" 2>&1)"
  code=$?
  set -e
  if [[ "$code" -ne "$expected_code" ]]; then
    echo "$output" >&2
    fail "$description (expected exit $expected_code, got $code)"
  fi
  printf '%s' "$output"
}

# 1. Sort unique words from a file with the default letter.
out="$(run_ok "sorted unique words" "$BIN" --text --unique --letter а examples/simple.txt)"
grep -qx "це велика мова Ада програмування" <<<"$out" \
  || fail "sorted unique words (unexpected order)"
pass "sorted unique words from a file"

# 2. Keep all occurrences when --unique is absent.
out="$(run_ok "all word occurrences" "$BIN" --text examples/simple.txt)"
grep -qx "це велика мова мова Ада Ада програмування" <<<"$out" \
  || fail "all word occurrences (unexpected order)"
pass "all word occurrences"

# 3. Default table output contains seven rows and the summary.
out="$(run_ok "table output" "$BIN" --letter=а examples/simple.txt)"
grep -q "Відсортовані слова" <<<"$out" || fail "table title"
grep -q "Слів: 7 (різних: 5); у виводі: 7 (усі входження)" <<<"$out" \
  || fail "table summary"
rows="$(grep -c '^ *[0-9][0-9]*  ' <<<"$out")"
[[ "$rows" -eq 7 ]] || fail "table rows (expected 7, got $rows)"
pass "default table output"

# 4. Read from stdin.
out="$(printf 'Ада — мова.\n' | run_ok "stdin input" "$BIN" --text --unique --letter а)"
grep -qx "мова Ада" <<<"$out" || fail "stdin order"
pass "stdin input"

# 5. Normalize tabs, repeated spaces and line breaks.
out="$(printf '  Привіт,\t\tсвіт!\n\nHaskell   —   мова.  ' | run_ok "normalization" "$BIN" --normalized)"
grep -qx "Привіт, світ! Haskell — мова." <<<"$out" \
  || fail "normalized text"
pass "whitespace normalization"

# 6. Case-sensitive counting.
out="$(run_ok "case-sensitive count" "$BIN" --text --unique --case-sensitive --letter а examples/simple.txt)"
grep -qx "це Ада велика мова програмування" <<<"$out" \
  || fail "case-sensitive order"
pass "case-sensitive counting"

# 7. A non-default letter changes the order.
out="$(run_ok "letter selection" "$BIN" --text --unique --letter о examples/simple.txt)"
grep -qx "Ада велика це мова програмування" <<<"$out" \
  || fail "letter selection order"
pass "non-default letter"

# 8. Statistics are reported for the original document.
out="$(run_ok "statistics" "$BIN" --stats --text --unique examples/simple.txt)"
grep -q "речень: 2" <<<"$out" || fail "sentence statistics"
grep -q "слів: 7 (різних: 5)" <<<"$out" || fail "word statistics"
grep -q "розділових знаків: 4" <<<"$out" || fail "punctuation statistics"
pass "text statistics"

# 9. A text without words exits with code 1.
out="$(printf '  \t\n ...\n' | run_fail 1 "text without words" "$BIN")"
grep -q "не знайдено жодного слова" <<<"$out" || fail "no-words message"
pass "text without words error"

# 10. A missing file exits with code 1.
out="$(run_fail 1 "missing file" "$BIN" /definitely/missing/file.txt)"
grep -q "не вдалося прочитати файл" <<<"$out" || fail "missing-file message"
pass "missing file error"

# 11. Invalid options exit with code 1.
out="$(run_fail 1 "invalid letter" "$BIN" --letter=аб examples/simple.txt)"
grep -q "однією літерою" <<<"$out" || fail "invalid-letter message"
pass "invalid letter error"
run_fail 1 "unknown option" "$BIN" --nope >/dev/null
pass "unknown option error"
run_fail 1 "two input files" "$BIN" examples/simple.txt examples/textbook.txt >/dev/null
pass "two input files error"

# 12. Help exits with code 0.
out="$(run_ok "help" "$BIN" --help)"
grep -q "Використання: textbook-word-sorter" <<<"$out" || fail "help text"
pass "help output"

echo "Smoke test passed: $PASS checks."
