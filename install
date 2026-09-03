#!/usr/bin/env bash
set -Eeuo pipefail
ROOT=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
MANIFEST=${1:?manifest.tsv path is required}
BACKUP="$HOME/.local/state/dotfiles/backups/$(date +%Y%m%d-%H%M%S)"
while IFS=$'\t' read -r source target mode; do
  [[ $source == source || -z $source || $source == \#* ]] && continue
  [[ $source == home/* && $target != /* && $target != *..* ]] || { echo "invalid manifest entry: $source" >&2; exit 1; }
  src="$ROOT/$source"; dst="$HOME/$target"
  [[ -f $src && ! -L $src ]] || { echo "missing source: $source" >&2; exit 1; }
  if [[ -e $dst || -L $dst ]]; then
    if [[ -f $dst && ! -L $dst ]] && cmp -s "$src" "$dst"; then chmod "$mode" "$dst"; continue; fi
    mkdir -p "$BACKUP/$(dirname "$target")"
    mv -- "$dst" "$BACKUP/$target"
  fi
  install -D -m "$mode" "$src" "$dst"
done <"$MANIFEST"
printf 'dotfiles_commit=%s\n' "$(git -C "$ROOT" rev-parse HEAD 2>/dev/null || echo uncommitted)"
