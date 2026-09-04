#!/usr/bin/env bash
set -Eeuo pipefail
ROOT=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
if [[ ${1:-} == --system ]]; then
  (( EUID == 0 )) || { echo 'system manifest requires root' >&2; exit 1; }
  MANIFEST=${2:-$ROOT/system-manifest.tsv}
  PREFIX=/
  BACKUP="/var/lib/dotfiles/backups/$(date +%Y%m%d-%H%M%S)"
else
  MANIFEST=${1:-$ROOT/manifest.tsv}
  PREFIX="$HOME/"
  BACKUP="$HOME/.local/state/dotfiles/backups/$(date +%Y%m%d-%H%M%S)"
fi

while IFS=$'\t' read -r source target; do
  [[ $source == source || -z $source || $source == \#* ]] && continue
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
    rsync -a --backup --backup-dir="$BACKUP/$target" "$src/" "$dst/"
  else
    if [[ -e $dst || -L $dst ]]; then
      cmp -s "$src" "$dst" && continue
      mkdir -p "$BACKUP/$(dirname "$target")"
      mv "$dst" "$BACKUP/$target"
    fi
    install -D -m "$(stat -c %a "$src")" "$src" "$dst"
  fi
done <"$MANIFEST"

printf 'dotfiles_commit=%s\n' "$(git -C "$ROOT" rev-parse HEAD 2>/dev/null || echo uncommitted)"
