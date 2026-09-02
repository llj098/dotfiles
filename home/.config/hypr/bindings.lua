-- Keep only your personal keybinding overrides here. Add new bindings or
-- unbind defaults before replacing them.

-- See current bindings and descriptions:
--   omarchy menu keybindings --print

-- To disable every Omarchy default binding, set this in
-- ~/.config/hypr/hyprland.lua before require("default.hypr.omarchy"), then add
-- only the bindings you want below:
--   omarchy_default_bindings = false

-- To disable all preinstalled app/webapp bindings, set:
--   omarchy_preinstalled_bindings = false

-- Add a new binding.
-- o.bind("SUPER + SHIFT + R", "SSH", "alacritty -e ssh your-server")

-- Change an existing binding by unbinding it first, then binding the key again.
-- This example changes SUPER+SPACE from the launcher to the Omarchy root menu.
-- hl.unbind("SUPER + SPACE")
-- o.bind("SUPER + SPACE", "Omarchy menu", "omarchy-menu toggle root")

-- Disable a default binding without replacing it.
-- hl.unbind("SUPER + SHIFT + B")

-- Logitech MX Keys examples:
-- o.bind("SUPER + SHIFT + S", nil, "omarchy-capture-screenshot")
-- o.bind("SUPER + H", nil, "voxtype record toggle")
-- o.bind("SUPER + PERIOD", nil, "omarchy-shell shell toggle omarchy.emojis")

-- Alt+1..9：仅当 group 中存在对应序号时才切换。
local function activate_group_tab(index)
  return function()
    local window = hl.get_active_window()
    local group = window and window.group

    if group and index <= group.size then
      hl.dispatch(hl.dsp.group.active({ index = index }))
    end
  end
end

for index = 1, 9 do
  local key = "ALT + code:" .. tostring(index + 9)

  hl.unbind(key)
  o.bind(key, "Switch to group window " .. index, activate_group_tab(index))
end

-- Tab navigation overrides: windows on Super, group tabs on Alt.
hl.unbind("SUPER + TAB")
hl.unbind("SUPER + SHIFT + TAB")
hl.unbind("ALT + TAB")
hl.unbind("ALT + SHIFT + TAB")

o.bind("SUPER + TAB", "Focus on next window", hl.dsp.window.cycle_next())
o.bind("SUPER + TAB", "Reveal active window on top", hl.dsp.window.bring_to_top())
o.bind("SUPER + SHIFT + TAB", "Focus on previous window", hl.dsp.window.cycle_next({ next = false }))
o.bind("SUPER + SHIFT + TAB", "Reveal active window on top", hl.dsp.window.bring_to_top())

o.bind("ALT + TAB", "Next window in group", hl.dsp.group.next())
o.bind("ALT + SHIFT + TAB", "Previous window in group", hl.dsp.group.prev())


-- linux-setup wechat binding
hl.unbind("SUPER + X")
o.bind("SUPER + X", "Show WeChat", {
  launch = os.getenv("HOME") .. "/.local/bin/wechat-show",
})
