# Login-shell environment comes from the same source as XFCE/systemd user env.
[ -r "$HOME/.config/user-env.sh" ] && . "$HOME/.config/user-env.sh"
if [ -r "$HOME/.config/secrets/env" ]; then
  . "$HOME/.config/secrets/env"
fi
