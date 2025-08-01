-- Hammerspoon Helper Functions

local M = {}

-- Helper function to execute yabai commands
function M.yabai(args)
    local cmd = "/opt/homebrew/bin/yabai -m " .. args
    hs.task.new("/bin/sh", nil, {"-c", cmd}):start()
end

-- Helper function to execute yabai commands with multiple operations
function M.yabaiMultiple(commands)
    for _, cmd in ipairs(commands) do
        M.yabai(cmd)
    end
end

-- Move window to desktop functions (similar to Keyboard Maestro macro)
function M.moveWindowToDesktop(desktopNumber)
    local currentWindow = hs.window.focusedWindow()
    if not currentWindow then
        M.alert("No active window")
        return
    end

    -- Show notification
    M.alert("Moving to Desktop " .. desktopNumber, 1)

    -- Get window frame for mouse positioning
    local windowFrame = currentWindow:frame()
    local startPoint = {x = windowFrame.x + 15, y = windowFrame.y + 5}

    -- Store current mouse position
    local currentMousePos = hs.mouse.absolutePosition()

    -- Move mouse to window and start drag
    hs.mouse.absolutePosition(startPoint)
    hs.eventtap.event.newMouseEvent(hs.eventtap.event.types.leftMouseDown, startPoint):post()

    -- Small delay to ensure drag is initiated
    hs.timer.usleep(50000) -- 50ms

    -- Simulate Cmd + desktop number keystroke
    hs.eventtap.keyStroke({"alt"}, tostring(desktopNumber))

    -- Small delay before releasing
    hs.timer.usleep(100000) -- 100ms

    -- Release mouse drag
    hs.eventtap.event.newMouseEvent(hs.eventtap.event.types.leftMouseUp, startPoint):post()

    -- Restore mouse position
    hs.mouse.absolutePosition(currentMousePos)
end

-- Alternative method using yabai (if you prefer)
function M.moveWindowToSpace(spaceNumber)
    hs.alert.show("Move to Space " .. spaceNumber, 1)
    M.yabaiMultiple({
        "window --space " .. spaceNumber,
        "space --focus " .. spaceNumber
    })
end

function M.alert(text, duration)
    duration = duration or 2 -- Default 2 seconds

    -- Get screen dimensions
    local screen = hs.screen.mainScreen()
    local screenFrame = screen:frame()

    -- Create a small canvas for the text
    local canvas = hs.canvas.new({
        x = screenFrame.x + screenFrame.w - 300, -- Top right corner (adjusted for larger width)
        y = screenFrame.y + 50,
        w = 288, -- 240 * 1.2 = 288
        h = 36  -- 30 * 1.2 = 36
    })

    -- Set canvas properties - modern macOS light mode style
    canvas:insertElement({
        type = "rectangle",
        action = "fill",
        fillColor = { alpha = 0.95, red = 0.98, green = 0.98, blue = 0.98 },
        roundedRectRadii = { xRadius = 12, yRadius = 12 } -- 10 * 1.2 = 12
    })

    -- Add subtle shadow effect
    canvas:insertElement({
        type = "rectangle",
        action = "stroke",
        strokeColor = { alpha = 0.2, red = 0.0, green = 0.0, blue = 0.0 },
        strokeWidth = 1,
        roundedRectRadii = { xRadius = 12, yRadius = 12 } -- 10 * 1.2 = 12
    })

    canvas:insertElement({
        type = "text",
        text = text,
        textColor = { alpha = 1.0, red = 0.2, green = 0.2, blue = 0.2 },
        textSize = 16, -- 13 * 1.2 ≈ 16
        textAlignment = "center",
        frame = { x = 12, y = 8, w = 264, h = 19 } -- All dimensions increased by 20%
    })

    -- Show the canvas
    canvas:show()

    -- Hide after duration with fade effect
    hs.timer.doAfter(duration, function()
        canvas:hide()
        canvas:delete()
    end)

    return canvas
end

return M