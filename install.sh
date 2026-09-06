#!/usr/bin/env bash
set -Eeuo pipefail
ROOT=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
SYSTEM=0
ONLY_SOURCE=
MANIFEST=
usage() {
  echo "usage: $0 [--system] [--only SOURCE] [MANIFEST]" >&2
  exit 2
}
while (($#)); do
  case $1 in
    --system)
      (( SYSTEM == 0 )) || usage
      SYSTEM=1
      shift
      ;;
    --only)
      [[ -z $ONLY_SOURCE && $# -ge 2 && $2 != --* ]] || usage
      ONLY_SOURCE=$2
      shift 2
      ;;
    --)
      shift
      (($# <= 1)) || usage
      [[ $# -eq 0 ]] || MANIFEST=$1
      break
      ;;
    -*) usage ;;
    *)
      [[ -z $MANIFEST ]] || usage
      MANIFEST=$1
      shift
      ;;
  esac
done

if (( SYSTEM )); then
  (( EUID == 0 )) || { echo 'system manifest requires root' >&2; exit 1; }
  MANIFEST=${MANIFEST:-$ROOT/system-manifest.tsv}
  PREFIX=/
  BACKUP="/var/lib/dotfiles/backups/$(date +%Y%m%d-%H%M%S)"
else
  MANIFEST=${MANIFEST:-$ROOT/manifest.tsv}
  PREFIX="$HOME/"
  BACKUP="$HOME/.local/state/dotfiles/backups/$(date +%Y%m%d-%H%M%S)"
fi
[[ -r $MANIFEST ]] || { echo "manifest is not readable: $MANIFEST" >&2; exit 1; }
[[ -z $ONLY_SOURCE || $ONLY_SOURCE != /* && $ONLY_SOURCE != *..* ]] || { echo "invalid source selector: $ONLY_SOURCE" >&2; exit 1; }

matched=0
while IFS=$'\t' read -r source target; do
  [[ $source == source || -z $source || $source == \#* ]] && continue
  [[ -z $ONLY_SOURCE || $source == "$ONLY_SOURCE" ]] || continue
  matched=$((matched + 1))
  [[ $source != /* && $source != *..* && $target != /* && $target != *..* ]] || { echo "invalid manifest entry: $source" >&2; exit 1; }
  src="$ROOT/$source"
  dst="$PREFIX$target"
  [[ -e $src && ! -L $src ]] || { echo "missing source: $source" >&2; exit 1; }

  if [[ -d $src ]]; then
    if [[ -e $dst && ! -d $dst || -L $dst ]]; then
      mkdir -p "$BACKUP/$(dirname "$target")"
      mv "$dst" "$BACKUP/$target"
    fi
    mkdir -p "$dst" "$BACKUP/$target"
    rsync -a --no-owner --no-group --no-perms --omit-dir-times --backup --backup-dir="$BACKUP/$target" "$src/" "$dst/"
  else
    if [[ -e $dst || -L $dst ]]; then
      cmp -s "$src" "$dst" && continue
      mkdir -p "$BACKUP/$(dirname "$target")"
      mv "$dst" "$BACKUP/$target"
    fi
    install -D -m "$(stat -c %a "$src")" "$src" "$dst"
  fi
done <"$MANIFEST"

[[ -z $ONLY_SOURCE || $matched -gt 0 ]] || { echo "source not found in manifest: $ONLY_SOURCE" >&2; exit 1; }
printf 'dotfiles_commit=%s\n' "$(git -C "$ROOT" rev-parse HEAD 2>/dev/null || echo uncommitted)"
