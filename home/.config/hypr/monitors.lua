-- See https://wiki.hypr.land/Configuring/Basics/Monitors/
-- List current monitors and supported resolutions with: hyprctl monitors all

local omarchy_gdk_scale = 2
hl.env("GDK_SCALE", tostring(omarchy_gdk_scale))

local koios = "desc:KOS KOIOS K2720UD 0000000000000"
local lg = "desc:LG Electronics LG HDR 4K 0x1864B0ED"

-- Internal panel is the layout anchor.
hl.monitor({ output = "eDP-1", mode = "preferred", position = "0x0", scale = 1.25 })

-- Home: KOIOS sits to the left of the internal panel.
hl.monitor({ output = koios, mode = "preferred", position = "auto-left", scale = 1.6 })

-- Office: LG sits to the right of the internal panel.
hl.monitor({ output = lg, mode = "preferred", position = "auto-right", scale = 1.6 })
