-- Hammerspoon Configuration

-- Load helper functions
local func = require("func")

-- Change window focus within space
hs.hotkey.bind({"alt"}, "down", function() func.yabai("window --focus south") end)
hs.hotkey.bind({"alt"}, "up", function() func.yabai("window --focus north") end)
hs.hotkey.bind({"alt"}, "left", function() func.yabai("window --focus west") end)
hs.hotkey.bind({"alt"}, "right", function() func.yabai("window --focus east") end)

-- Focus to another monitor
hs.hotkey.bind({"alt"}, "s", function() func.yabai("display --focus west") end)
hs.hotkey.bind({"alt"}, "g", function() func.yabai("display --focus east") end)

-- Rotate layout clockwise
hs.hotkey.bind({"shift", "alt"}, "r", function() func.yabai("space --rotate 270") end)

-- Flip along y-axis
hs.hotkey.bind({"shift", "alt"}, "y", function() func.yabai("space --mirror y-axis") end)

-- Flip along x-axis
hs.hotkey.bind({"shift", "alt"}, "x", function() func.yabai("space --mirror x-axis") end)

-- Maximize a window
hs.hotkey.bind({"shift", "alt"}, "m", function() func.yabai("window --toggle zoom-fullscreen") end)

-- Balance out tree of windows
hs.hotkey.bind({"shift", "alt"}, "e", function() func.yabai("space --balance") end)

-- Swap windows
hs.hotkey.bind({"shift", "alt"}, "down", function() func.yabai("window --swap south") end)
hs.hotkey.bind({"shift", "alt"}, "up", function() func.yabai("window --swap north") end)
hs.hotkey.bind({"shift", "alt"}, "left", function() func.yabai("window --swap west") end)
hs.hotkey.bind({"shift", "alt"}, "right", function() func.yabai("window --swap east") end)

-- Move window and split
hs.hotkey.bind({"ctrl", "alt"}, "down", function() func.yabai("window --warp south") end)
hs.hotkey.bind({"ctrl", "alt"}, "up", function() func.yabai("window --warp north") end)
hs.hotkey.bind({"ctrl", "alt"}, "left", function() func.yabai("window --warp west") end)
hs.hotkey.bind({"ctrl", "alt"}, "right", function() func.yabai("window --warp east") end)

-- Move window to display left and right
hs.hotkey.bind({"shift", "alt"}, "s", function()
    func.yabaiMultiple({
        "window --display west",
        "display --focus west"
    })
end)
hs.hotkey.bind({"shift", "alt"}, "g", function()
    func.yabaiMultiple({
        "window --display east",
        "display --focus east"
    })
end)

-- Toggle layout
-- alt + , to change layout to float
hs.hotkey.bind({"alt"}, ",", function() func.yabai("space --layout float") end)
-- alt + . to change layout to bsp
hs.hotkey.bind({"alt"}, ".", function() func.yabai("space --layout bsp") end)

-- Resize window horizontally with alt + -/=
hs.hotkey.bind({"alt"}, "-", function()
    local cmd = "yabai -m window --resize right:-20:0 2> /dev/null || yabai -m window --resize left:-20:0 2> /dev/null"
    hs.task.new("/bin/sh", nil, {"-c", cmd}):start()
end)
hs.hotkey.bind({"alt"}, "=", function()
    local cmd = "yabai -m window --resize right:20:0 2> /dev/null || yabai -m window --resize left:20:0 2> /dev/null"
    hs.task.new("/bin/sh", nil, {"-c", cmd}):start()
end)

-- Resize window vertically with alt + shift + -/=
hs.hotkey.bind({"shift", "alt"}, "-", function()
    local cmd = "yabai -m window --resize bottom:0:-20 2> /dev/null || yabai -m window --resize top:0:-20 2> /dev/null"
    hs.task.new("/bin/sh", nil, {"-c", cmd}):start()
end)
hs.hotkey.bind({"shift", "alt"}, "=", function()
    local cmd = "yabai -m window --resize bottom:0:20 2> /dev/null || yabai -m window --resize top:0:20 2> /dev/null"
    hs.task.new("/bin/sh", nil, {"-c", cmd}):start()
end)

-- Close window
hs.hotkey.bind({"alt"}, "w", function() func.yabai("window --close") end)

-- Bind keyboard shortcuts for moving windows to desktops 1-9
for i = 1, 9 do
    hs.hotkey.bind({"shift", "alt"}, tostring(i), function()
        func.moveWindowToDesktop(i)
    end)
end

-- Open apps
hs.hotkey.bind({"alt"}, "return", function()
    os.execute("/opt/homebrew/bin/alacritty")
end)

hs.hotkey.bind({"alt"}, "f", function()
    hs.task.new("/usr/bin/open", nil, {"-a", "Finder"}):start()
end)

hs.hotkey.bind({"alt"}, "b", function()
    hs.task.new("/usr/bin/open", nil, {"-na", "Google Chrome"}):start()
end)

-- Reload Hammerspoon config
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "r", function()
    hs.reload()
end)
func.alert("Hammerspoon Config Loaded")
