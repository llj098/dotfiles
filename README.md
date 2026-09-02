# dotfiles

Private, manifest-driven user configuration for the Omarchy deployment.

Sources are the active t14g2 configuration plus validated linux-setup skill settings. Deploy with `./install`. Existing files are backed up under `~/.local/state/dotfiles/backups/`.

This repository deliberately excludes browser profiles, Omarchy plugin sources, Mihomo subscriptions/runtime state, Rime models and learning data, Syncthing identity/data, Pi state, credentials, caches and user documents. Software and upstream trees such as Oh My Zsh are installed by the Omarchy V2 deployment, not stored here.
