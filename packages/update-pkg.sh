#!/usr/bin/env bash
# update-pkg.sh — update the commit and sha256 for a git-fetched Guix package.
#
# Usage: update-pkg.sh <file.scm> <git-url>
#
# The script:
#   1. Reads the pinned commit from the .scm file
#   2. Fetches the current HEAD of the upstream repo
#   3. Skips if already up to date
#   4. Updates the 40-char commit and the 7-char short hash in the version string
#   5. Clones the repo at the new commit and recomputes the sha256 with guix hash
#   6. Replaces the old base32 hash in the .scm file

set -euo pipefail

FILE="$1"
URL="$2"

# ── read pinned state ──────────────────────────────────────────────────────────

OLD_COMMIT=$(grep -oE '\(commit "[0-9a-f]{40}"\)' "$FILE" \
             | grep -oE '[0-9a-f]{40}')

if [ -z "$OLD_COMMIT" ]; then
  echo "  ERROR: could not find a commit hash in $FILE" >&2
  exit 1
fi

OLD_HASH=$(grep -oE '\(base32 "[a-z0-9]+"\)' "$FILE" \
           | grep -oE '"[a-z0-9]+"' \
           | tr -d '"' \
           | head -1)

# ── check upstream ─────────────────────────────────────────────────────────────

NEW_COMMIT=$(git ls-remote "$URL" HEAD | cut -f1)

if [ -z "$NEW_COMMIT" ]; then
  echo "  ERROR: git ls-remote $URL failed" >&2
  exit 1
fi

if [ "$OLD_COMMIT" = "$NEW_COMMIT" ]; then
  echo "  already up to date (${OLD_COMMIT:0:7})"
  exit 0
fi

echo "  ${OLD_COMMIT:0:7} -> ${NEW_COMMIT:0:7}"

# ── update commit in file ──────────────────────────────────────────────────────

OLD_SHORT="${OLD_COMMIT:0:7}"
NEW_SHORT="${NEW_COMMIT:0:7}"

sed -i "s/$OLD_COMMIT/$NEW_COMMIT/g" "$FILE"
sed -i "s/$OLD_SHORT/$NEW_SHORT/g"   "$FILE"

# ── clone and hash ─────────────────────────────────────────────────────────────

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

echo "  cloning $URL @ ${NEW_COMMIT:0:7} ..."
git clone --quiet --no-tags "$URL" "$TMPDIR/src"
git -C "$TMPDIR/src" checkout --quiet "$NEW_COMMIT"

NEW_HASH=$(guix hash --serializer=nar -x "$TMPDIR/src")

if [ -z "$NEW_HASH" ]; then
  echo "  ERROR: guix hash returned empty result" >&2
  exit 1
fi

# ── update hash in file ────────────────────────────────────────────────────────

if [ -n "$OLD_HASH" ]; then
  sed -i "s/$OLD_HASH/$NEW_HASH/g" "$FILE"
else
  # hash was empty (e.g. from a previous failed run) — replace the empty slot
  sed -i 's|(base32 "")|(base32 "'"$NEW_HASH"'")|' "$FILE"
fi

echo "  hash  $NEW_HASH"
