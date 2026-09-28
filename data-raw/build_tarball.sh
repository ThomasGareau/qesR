#!/bin/sh
# Build the package tarball (and optionally run the CRAN check) from a copy
# of the source tree whose file modes are normalised.
#
# Usage (from the package root):
#   sh data-raw/build_tarball.sh [<build dir>] [--check]
#
# The working tree may sit on a volume without Unix permissions (exFAT), where
# every file reads as executable (mode 744) and the repository needs
# core.fileMode=false. R CMD build copies the modes it sees into the tarball,
# so the tree is first copied to <build dir> (default: a new directory under
# $TMPDIR) with rsync, leaving out the AppleDouble files (._*), .git, docs,
# the check directories and codebooks, and with files 644 and directories 755,
# as git stores them. (The only 100755 file of the repository,
# data-raw/legacy_build_master.R, is build-ignored with data-raw/.)
# The script stops if the tarball still has an executable file.
#
# With --check it then runs, in <build dir>,
#   _R_CHECK_CRAN_INCOMING_REMOTE_=true R CMD check --as-cran <tarball>
# (network: the remote URL and DOI checks of the incoming check).

set -eu

src=$(pwd)
[ -f "$src/DESCRIPTION" ] || { echo "Run from the package root." >&2; exit 1; }

check=false
out=""
for a in "$@"; do
  case "$a" in
    --check) check=true ;;
    *) out="$a" ;;
  esac
done
if [ -z "$out" ]; then
  out=$(mktemp -d "${TMPDIR:-/tmp}/qesR-build.XXXXXX")
fi
mkdir -p "$out/qesR"

rsync -a --delete --chmod=Du=rwx,Dgo=rx,Fu=rw,Fgo=r \
  --exclude='._*' --exclude='.git' --exclude='docs' \
  --exclude='..Rcheck' --exclude='*.Rcheck' --exclude='codebooks' \
  --exclude='qesR_*.tar.gz' \
  "$src/" "$out/qesR/"
find "$out/qesR" -type f -exec chmod 644 {} +
find "$out/qesR" -type d -exec chmod 755 {} +

cd "$out"
rm -f qesR_*.tar.gz
R CMD build qesR
tarball=$(ls qesR_*.tar.gz)

bad=$(tar tzvf "$tarball" | awk '$1 ~ /^-/ && $1 ~ /x/ {print $1, $NF}')
if [ -n "$bad" ]; then
  echo "$tarball has executable files:" >&2
  echo "$bad" >&2
  exit 1
fi
echo "Modes in $tarball:"
tar tzvf "$tarball" | awk '{print $1}' | sort | uniq -c

if $check; then
  _R_CHECK_CRAN_INCOMING_REMOTE_=true R CMD check --as-cran "$tarball"
fi
echo "Built $out/$tarball"
