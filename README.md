# dotfiles

User configuration for the Omarchy deployment. The deployment manifest is maintained by linux-setup V2 and passed to `./install`.

Sources are the active t14g2 configuration plus validated linux-setup skill settings. Existing files are backed up under `~/.local/state/dotfiles/backups/`.

This repository deliberately excludes browser profiles, Omarchy plugin sources, Mihomo subscriptions/runtime state, Rime models and learning data, Syncthing identity/data, Pi state, credentials, caches and user documents. Software and upstream trees such as Oh My Zsh are installed by the Omarchy V2 deployment, not stored here.

## Emacs

`home/minimal-emacs.d/` contains the personal overlay for an upstream
[minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d) checkout.
The deployment copies these source files into `~/.emacs.d/`; it does
not vendor the upstream repository. This makes Minimal the default for
`emacs`; Doom remains available explicitly with
`emacs --init-directory ~/.config/emacs`.

Runtime files are intentionally excluded:

- ELPA packages: `~/.local/share/emacs/minimal-emacs/elpa/`
- Native compilation cache: `~/.cache/emacs/minimal-emacs/eln-cache/`
- Byte/native compilation products: `*.elc`, `*.eln`
- History, recent files, package state, and personal Org data
