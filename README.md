# dotfiles

User configuration for the Omarchy deployment. The repository manifests deploy the complete `home/` and `system/` trees through `./install.sh`.

Sources are the active t14g2 configuration plus validated linux-setup skill settings. Existing files are backed up under `~/.local/state/dotfiles/backups/`.

This repository deliberately excludes browser profiles, Omarchy plugin sources, Mihomo subscriptions/runtime state, Rime models and learning data, Syncthing identity/data, Pi state, credentials, caches and user documents. Software and upstream trees such as Oh My Zsh are installed by the Omarchy V2 deployment, not stored here.

## Emacs

`home/.emacs.d/` contains the personal overlay for an upstream
[minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) checkout.
The deployment overlays these source files onto an upstream `~/.emacs.d/`
checkout; it does not vendor the upstream repository. This makes Minimal the
default configuration for `emacs`.

Runtime files are intentionally excluded:

- ELPA packages: `~/.local/share/emacs/minimal-emacs/elpa/`
- Native compilation cache: `~/.cache/emacs/minimal-emacs/eln-cache/`
- Byte/native compilation products: `*.elc`, `*.eln`
- History, recent files, package state, and personal Org data
