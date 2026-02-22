#!/usr/bin/env bash
set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_DIR"

COMMIT_MSG="${1:-Add pkgdown website with analysis examples}"

echo "Repo: $REPO_DIR"
echo "Branch: $(git branch --show-current)"

git add \
  .Rbuildignore \
  .gitignore \
  .github/workflows/pkgdown.yml \
  _pkgdown.yml \
  DESCRIPTION \
  NEWS.md \
  R/master.R \
  README.md \
  man/get_qes_master.Rd \
  man/figures/logo.png \
  pkgdown/favicon/apple-touch-icon.png \
  pkgdown/favicon/favicon-96x96.png \
  pkgdown/favicon/favicon.ico \
  pkgdown/favicon/favicon.svg \
  pkgdown/favicon/site.webmanifest \
  pkgdown/favicon/web-app-manifest-192x192.png \
  pkgdown/favicon/web-app-manifest-512x512.png \
  pkgdown/index.md \
  pkgdown/extra.css \
  qes_master.csv \
  qes_master.rds \
  qes_master_source_map.csv \
  scripts/build_pkgdown_site.R \
  scripts/commit_and_push_master_changes.sh \
  scripts/commit_and_push_site_changes.sh \
  tests/testthat/test-master.R \
  vignettes/analysis-descriptive.Rmd \
  vignettes/analysis-sovereignty.Rmd \
  vignettes/fr-analyse-descriptive.Rmd \
  vignettes/fr-analyse-souverainete.Rmd \
  vignettes/fr-citations-etudes.Rmd \
  vignettes/fr-demarrage.Rmd \
  vignettes/fr-donnees-fusionnees.Rmd \
  vignettes/get-started.Rmd \
  vignettes/merged-dataset.Rmd \
  vignettes/study-citations.Rmd

git add -A vignettes

if git diff --cached --quiet; then
  echo "No staged website changes to commit."
  exit 0
fi

git commit -m "$COMMIT_MSG"

git fetch origin
if git show-ref --verify --quiet refs/remotes/origin/main; then
  git pull --rebase origin main
fi

git push -u origin main

echo "Done. Website changes committed and pushed."
