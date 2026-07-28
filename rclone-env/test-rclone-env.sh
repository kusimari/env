#!/usr/bin/env bash
# env/rclone-env/test-rclone-env.sh — mock tests for the rclone-env
# wrapper's mount surface.
#
# No network, no real mounts, no rclone config: `rclone`, `mount`,
# `uname`, `fusermount` and `diskutil` are all replaced by mocks on a
# throwaway PATH, so both the macOS (nfsmount) and Linux (mount) paths
# can be exercised from any machine.
#
# Covers the two things that are easy to get wrong and impossible to
# see in a dry-run:
#   1. is_mounted() — the idempotency guard. A mount point is a literal
#      path, not a regex; matching it as one made `.`/`*` in a path
#      report a *different* mount as already-mounted.
#   2. platform dispatch + the --daemon wait that makes the terminal
#      pause on macOS.
#
# Run: bash rclone-env/test-rclone-env.sh

set -uo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT="$SELF_DIR/rclone-env.sh"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
MOCKD="$TMP/bin"
LOG="$TMP/invoke.log"
mkdir -p "$MOCKD"

PASS=0
FAIL=0
ok()  { printf '  \033[32mPASS\033[0m %s\n' "$1"; PASS=$((PASS + 1)); }
bad() { printf '  \033[31mFAIL\033[0m %s\n       %s\n' "$1" "$2"; FAIL=$((FAIL + 1)); }

# assert_contains <haystack> <needle> <label>
assert_contains() {
  if [[ "$1" == *"$2"* ]]; then ok "$3"; else bad "$3" "$1"; fi
}

# Did the last run invoke this command? Records pass/fail directly.
assert_invoked() { # assert_invoked <pattern> <label>
  if grep -q "$1" "$LOG"; then ok "$2"; else bad "$2" "invocations: $(cat "$LOG")"; fi
}

# ── mocks ───────────────────────────────────────────────────────────
# `mount` with no args lists mounts (what is_mounted parses). MOCK_TABLE
# supplies the table; MOCK_UNAME picks the platform path.
cat > "$MOCKD/mount" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "${MOCK_TABLE:-}"
EOF

cat > "$MOCKD/uname" <<'EOF'
#!/usr/bin/env bash
echo "${MOCK_UNAME:-Linux}"
EOF

cat > "$MOCKD/rclone" <<'EOF'
#!/usr/bin/env bash
echo "rclone $*" >> "$MOCK_LOG"
case "$1" in
  listremotes) printf 'google-drive:\n' ;;
  # Real rclone blocks until the daemon reports ready; simulate that so
  # the wrapper's foreground wait is measurable.
  nfsmount|mount) sleep "${MOCK_MOUNT_DELAY:-0}" ;;
esac
exit 0
EOF

for tool in fusermount diskutil umount; do
  cat > "$MOCKD/$tool" <<EOF
#!/usr/bin/env bash
echo "$tool \$*" >> "\$MOCK_LOG"
exit "\${MOCK_${tool}_RC:-0}"
EOF
done
chmod +x "$MOCKD"/*

export MOCK_LOG="$LOG"
export PATH="$MOCKD:$PATH"

# Run the wrapper with a clean invocation log.
run() { : > "$LOG"; bash "$SCRIPT" "$@" 2>&1; }

# ── 1. is_mounted: the idempotency guard ────────────────────────────
# Driven through `mount` so we test the shipped function, not a copy.
# A real directory is needed because is_mounted resolves the path.
echo "=== is_mounted() — mount-point matching ==="
MNT="$TMP/dabba/gd"
mkdir -p "$MNT" "$TMP/dabba/gd-other" "$TMP/dabba/myXdrive"

guard() { # guard <table> <query> <expect-mounted: yes|no> <label>
  local out got
  out=$(MOCK_TABLE="$1" run mount google-drive: "$2")
  if [[ "$out" == *"Already mounted"* ]]; then got=yes; else got=no; fi
  if [[ "$got" == "$3" ]]; then ok "$4"; else bad "$4" "expect=$3 got=$got | $out"; fi
}

guard "gd: on $MNT (nfs)"                  "$MNT"   yes "exact path detected as mounted"
guard "gd: on $MNT type nfs (rw)"          "$MNT"   yes "Linux 'type <fs>' format parsed"
guard "gd: on $MNT (nfs)"                  "$MNT/"  yes "trailing slash normalised"
guard "o: on $TMP/dabba/gd-other (nfs)"    "$MNT"   no  "another mount sharing a prefix not matched"
guard ""                                   "$MNT"   no  "empty mount table"
# A literal '.' in the query would match any character if the path were
# treated as a regex — the bug this guards.
guard "x: on $TMP/dabba/myXdrive (nfs)" "$TMP/dabba/my.drive" no \
  "path metacharacter not matched as a regex"
# A mount point that doesn't exist yet can't be resolved; the unresolved
# path must still be compared (and must not degrade into an empty match).
guard "gd: on $TMP/dabba/not-created-yet (nfs)" "$TMP/dabba/not-created-yet" yes \
  "unresolvable path falls back to the literal path"
guard "gd: on $MNT (nfs)" "$TMP/dabba/not-created-yet" no \
  "unresolvable path does not match an unrelated mount"

# ── 2. platform dispatch ────────────────────────────────────────────
echo
echo "=== platform dispatch ==="
MOCK_UNAME=Darwin MOCK_TABLE='' run mount google-drive: "$TMP/m1" >/dev/null
assert_invoked 'rclone nfsmount' "Darwin uses nfsmount (no macFUSE)"

MOCK_UNAME=Linux MOCK_TABLE='' run mount google-drive: "$TMP/m2" >/dev/null
assert_invoked 'rclone mount' "Linux uses mount (fusermount)"

# The macOS --daemon wait is a constant sleep, so an explicit short
# --daemon-wait is what keeps the terminal from stalling for the 1m default.
MOCK_UNAME=Darwin MOCK_TABLE='' run mount google-drive: "$TMP/m3" >/dev/null
assert_invoked 'daemon-wait' "--daemon-wait passed (bounds the macOS mount stall)"

# ── 3. umount ───────────────────────────────────────────────────────
echo
echo "=== umount ==="
out=$(MOCK_UNAME=Linux MOCK_TABLE='' run umount "$TMP/m1")
assert_contains "$out" "Not mounted" "umount of an unmounted path is a no-op"

mkdir -p "$TMP/m4"
MOCK_UNAME=Linux MOCK_TABLE="gd: on $(cd "$TMP/m4" && pwd -P) (fuse.rclone)" \
  run umount "$TMP/m4" >/dev/null
assert_invoked 'fusermount -u' "Linux umount uses fusermount"

# Busy mount: first fusermount fails, the lazy fallback must run.
MOCK_UNAME=Linux MOCK_fusermount_RC=1 \
  MOCK_TABLE="gd: on $(cd "$TMP/m4" && pwd -P) (fuse.rclone)" \
  run umount "$TMP/m4" >/dev/null
assert_invoked 'fusermount -uz' "busy mount falls back to lazy unmount"

# ── 4. usage errors ─────────────────────────────────────────────────
echo
echo "=== usage ==="
out=$(run mount 2>&1)
assert_contains "$out" "Usage" "mount with no args prints usage"

out=$(run umount 2>&1)
assert_contains "$out" "Usage" "umount with no args prints usage"

echo
printf '=== %d passed, %d failed ===\n' "$PASS" "$FAIL"
[[ $FAIL -eq 0 ]]
