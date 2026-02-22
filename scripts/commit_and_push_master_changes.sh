#!/usr/bin/env bash
set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_DIR"

COMMIT_MSG="${1:-Add master harmonized dataset + codebook and merge improvements}"

echo "Repo: $REPO_DIR"
echo "Branch: $(git branch --show-current)"

# Stage only intended files for this update.
git add \
  DESCRIPTION \
  NAMESPACE \
  README.md \
  R/qes_catalog.R \
  R/qes_download.R \
  R/master.R \
  man/get_qes_master.Rd \
  scripts/build_qes_master.R \
  tests/testthat/test-codebook.R \
  tests/testthat/test-codebook-enrich.R \
  tests/testthat/test-codes.R \
  tests/testthat/test-exports.R \
  tests/testthat/test-master.R \
  tests/testthat/test-readers.R \
  qes_master.csv \
  qes_master.rds \
  qes_master_source_map.csv

# Avoid committing temporary test artifacts if they exist.
git reset -- qes_master_test.csv qes_master_test.rds qes_master_test_source_map.csv 2>/dev/null || true

if git diff --cached --quiet; then
  echo "No staged changes to commit."
  exit 0
fi

git commit -m "$COMMIT_MSG"

# Rebase to avoid non-fast-forward errors, then push.
git fetch origin
if git show-ref --verify --quiet refs/remotes/origin/main; then
  git pull --rebase origin main
fi

git push -u origin main

echo "Done. Changes committed and pushed."
