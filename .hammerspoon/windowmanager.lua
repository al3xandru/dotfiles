-- WINDOW MANAGEMENT

-- settings
local DEBUG = true
local DEFAULT_WND_GAP = 5
--
-- Utility functions
-- 
AxWndManager = {}

AxWndManager.dynamicResizeLeft=function()
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("ERROR dynamicResizeLeft: no window found")
        return
    elseif DEBUG then   
        AxWndManager.wtf("dynamicResizeLeft", win)
    end

    local frm = win:frame()
    local scr = win:screen():frame()

    if not AxWndManager.between(frm.x, scr.x, 5) then
        -- move to the left
        frm.x = scr.x
        hs.alert.show("⬅ ")
    elseif not AxWndManager.between(frm.y, scr.y, 25) then
        -- now we also resize
        -- frm.x = scr.x
        -- frm.y = scr.y
        -- frm.w = scr.w * 2/3 - DEFAULT_WND_GAP
        -- frm.h = scr.h
        -- hs.alert.show("⬅  2/3")
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w * 1/2 - DEFAULT_WND_GAP
        frm.h = scr.h
        hs.alert.show("⬅  1/2")
    elseif AxWndManager.between(frm.w, scr.w * 2/3, DEFAULT_WND_GAP * 2) then
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w * 1/2 - DEFAULT_WND_GAP
        frm.h = scr.h
        hs.alert.show("⬅  1/2")
    elseif AxWndManager.between(frm.w, scr.w * 1/2, DEFAULT_WND_GAP * 2) then
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w * 1/3 - DEFAULT_WND_GAP
        frm.h = scr.h
        hs.alert.show("⬅  1/3")
    else
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w * 2/3 - DEFAULT_WND_GAP
        frm.h = scr.h
        hs.alert.show("⬅  2/3")
    end
    win:setFrame(frm)
end

AxWndManager.dynamicResizeRight=function()
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("dynamicResizeRight: ERROR no window found")
        return
    elseif DEBUG then
        print("dynamicResizeRight", win)
    end

    local frm = win:frame()
    local scr = win:screen():frame()

    if not AxWndManager.between(frm.x + frm.w, scr.x + scr.w, 5) then
        -- move to the left
        frm.x = scr.x + (scr.w - frm.w)
        hs.alert.show("➡")
    elseif not AxWndManager.between(frm.y, scr.y, 25) then
        -- now we also resize
        -- frm.w = scr.w * 2/3 - DEFAULT_WND_GAP
        -- frm.h = scr.h
        -- frm.x = scr.x + (scr.w - frm.w)
        -- frm.y = scr.y
        -- hs.alert.show("2/3 ➡")
        frm.w = scr.w * 1/2 - DEFAULT_WND_GAP
        frm.h = scr.h
        frm.x = scr.x + (scr.w - frm.w)
        frm.y = scr.y
        hs.alert.show("1/2 ➡")
    elseif AxWndManager.between(frm.w, scr.w * 2/3, DEFAULT_WND_GAP * 2) then
        frm.w = scr.w * 1/2 - DEFAULT_WND_GAP
        frm.h = scr.h
        frm.x = scr.x + (scr.w - frm.w)
        frm.y = scr.y
        hs.alert.show("1/2 ➡")
    elseif AxWndManager.between(frm.w, scr.w * 1/2, DEFAULT_WND_GAP * 2) then
        frm.w = scr.w * 1/3 - DEFAULT_WND_GAP
        frm.h = scr.h
        frm.x = scr.x + (scr.w - frm.w)
        frm.y = scr.y
        hs.alert.show("1/3 ➡")
    else
        frm.w = scr.w * 2/3 - DEFAULT_WND_GAP
        frm.h = scr.h
        frm.x = scr.x + (scr.w - frm.w)
        frm.y = scr.y
        hs.alert.show("2/3 ➡")
    end
    win:setFrame(frm)
end

AxWndManager.dynamicResizeTop=function()
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("dynamicResizeTop: ERROR no window found")
        return
    elseif DEBUG then   
        AxWndManager.wtf("dynamicResizeTop:", win)
    end

    local frm = win:frame()
    local scr = win:screen():frame()
    -- UndoStack:push({wnd=win:id(), frm=frm})

    if not AxWndManager.between(frm.y, scr.y, 5) then
        -- move to the left
        frm.y = scr.y
        hs.alert.show("⬆")
    elseif not AxWndManager.between(frm.x, scr.x, 5) then
        -- now we also resize
        -- frm.x = scr.x
        -- frm.y = scr.y
        -- frm.w = scr.w
        -- frm.h = scr.h * .7 - DEFAULT_WND_GAP
        -- hs.alert.show("⬆ 2/3")
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w
        frm.h = scr.h * .5 - DEFAULT_WND_GAP
        hs.alert.show("⬆ 1/2")
    elseif AxWndManager.between(frm.h, scr.h * .7, DEFAULT_WND_GAP * 2.5) then
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w
        frm.h = scr.h * .5 - DEFAULT_WND_GAP
        hs.alert.show("⬆ 1/2")
    elseif AxWndManager.between(frm.h, scr.h * .5, DEFAULT_WND_GAP * 2.5) then
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w
        frm.h = scr.h * 0.3 - DEFAULT_WND_GAP
        hs.alert.show("⬆ 1/3")
    else
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w
        frm.h = scr.h * 0.7 - DEFAULT_WND_GAP
        hs.alert.show("⬆ 2/3")
    end
    win:setFrame(frm)
end

AxWndManager.dynamicResizeBottom=function()
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("dynamicResizeBottom: ERROR no window found")
        return
    elseif DEBUG then   
        AxWndManager.wtf("dynamicResizeBottom:", win)
    end

    local frm = win:frame()
    local scr = win:screen():frame()
    -- UndoStack:push({wnd=win:id(), frm=frm})
    --
    if not AxWndManager.between(frm.y + frm.h, scr.y + scr.h, 5) then
        -- move to the left
        frm.y = scr.y + scr.h - frm.h
        hs.alert.show("⬇")
    elseif not (AxWndManager.between(frm.x, scr.x, 5) and AxWndManager.between(frm.w, scr.w, 25)) then
        -- now we also resize
        -- frm.w = scr.w
        -- frm.h = scr.h * .7 - DEFAULT_WND_GAP
        -- frm.x = scr.x
        -- frm.y = scr.y + scr.h - frm.h
        -- hs.alert.show("⬇ 2/3")
        frm.w = scr.w
        frm.h = scr.h * .5 - DEFAULT_WND_GAP
        frm.x = scr.x
        frm.y = scr.y + scr.h - frm.h
        hs.alert.show("⬇ 1/2")
    elseif AxWndManager.between(frm.h, scr.h * .7, DEFAULT_WND_GAP * 2) then
        frm.w = scr.w
        frm.h = scr.h * .5 - DEFAULT_WND_GAP
        frm.x = scr.x
        frm.y = scr.y + scr.h - frm.h
        hs.alert.show("⬇ 1/2")
    elseif AxWndManager.between(frm.h, scr.h * .5, DEFAULT_WND_GAP * 2.5) then 
        frm.w = scr.w
        frm.h = scr.h * .3 - DEFAULT_WND_GAP
        frm.x = scr.x
        frm.y = scr.y + scr.h - frm.h
        hs.alert.show("⬇ 1/3")
    else
        frm.w = scr.w
        frm.h = scr.h * .7 - DEFAULT_WND_GAP
        frm.x = scr.x
        frm.y = scr.y + scr.h - frm.h
        hs.alert.show("⬇ 2/3")
    end
    win:setFrame(frm)
end

AxWndManager.expandVertically=function()
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("expandVertically: ERROR no window found")
        return
    end
    local frm = win:frame()
    local scr = win:screen():frame()

    frm.y = scr.y
    frm.h = scr.h
    win:setFrame(frm)
end

AxWndManager.expandHorizontally=function()
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("expandHorizontally: ERROR no window found")
        return
    end
    local frm = win:frame()
    local scr = win:screen():frame()

    frm.x = scr.x
    frm.w = scr.w
    win:setFrame(frm)
end

-- Get list of screens and refresh that list whenever screens are plugged or unplugged:
--[[
local screens = hs.screen.allScreens()
local screenwatcher = hs.screen.watcher.new(function()
	screens = hs.screen.allScreens()
end)
screenwatcher:start()
--]]
-- Move window to `incr` monitors from current one 
-- Index starts with 1
AxWndManager.moveToMonitor=function(incr)
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("moveToMonitor: ERROR no window found")
        return
    end
    local screens = hs.screen.allScreens()
    local scr = win:screen()
    local currentScrIdx = nil

    for index, val in ipairs(screens) do
        if screens[index] == scr then
            currentScrIdx = index
            break
        end
    end

    if DEBUG then
        print("Screens:", #screens, ", current:", currentScrIdx)
    end
	local newScreen = nil
    if currentScrIdx + incr <= 0 then
        newScreen = screens[#screens + currentScrIdx + incr]
    elseif currentScrIdx + incr > #screens then
        newScreen = screens[currentScrIdx + incr - #screens]
    else
        newScreen = screens[currentScrIdx + incr]
    end

	win:moveToScreen(newScreen)
end


-- Move window to adjacent Desktop
-- inspired by https://github.com/Hammerspoon/hammerspoon/issues/235
--[[
function moveWndNextSpace(direction)
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("moveWndNextSpace(", direction, "): ERROR no wnd found")
        return
    elseif DEBUG then   
        wtf("moveWndNextSpace", win)
    end
    if not win:isStandard() then
        print("moveWndNextSpace(", direction, "): ERROR wnd is not standard")
        return
    end
    if win:isFullScreen() then
        print("moveWndNextSpace(", direction, "): ERROR wnd is full screen")
        return
    end
    local clickPoint = win:zoomButtonRect()
    clickPoint.x = clickPoint.x + clickPoint.w + 5
    clickPoint.y = clickPoint.y + (clickPoint.h / 2)
    hs.eventtap.event.newMouseEvent(hs.eventtap.event.types.leftMouseDown, clickPoint):post()
    
    -- implementing the keyevent this way seem to be required to actually trigger the desktop change
    hs.eventtap.event.newKeyEvent(hs.keycodes.map.ctrl, true):post()
    hs.eventtap.event.newKeyEvent(direction, true):post()
    hs.eventtap.event.newKeyEvent(direction, false):post()
    hs.eventtap.event.newKeyEvent(hs.keycodes.map.ctrl, false):post()

    hs.timer.doAfter(.25, function()
        hs.eventtap.event.newMouseEvent(hs.eventtap.event.types.leftMouseUp, clickPoint):post()
    end)
end
--]]

-- Started based on https://github.com/Hammerspoon/hammerspoon/issues/3111
AxWndManager.moveWndToSpace = function(direction, focus)
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("ERROR moveWndToSpace(" .. direction .. ") no window found")
        return
    end
    if not win:isStandard() then
        print("INFO  moveWndToSpace(" .. direction .. ") non-standard window")
        AxWndManager.flashScreen(win:screen())
        return
    end
    if win:isFullScreen() then
        print("INFO  moveWndToSpace(" .. direction .. ") full-screen window")
        AxWndManager.flashScreen(win:screen())
        return
    end
    local screenId = win:screen():getUUID()
    local screenSpaces = hs.spaces.allSpaces()[screenId]
    local currentDesktopIdx = 0
    -- print("INFO  moveWndToSpace: " .. screenId .. ", spaces:", screenSpaces, ", focused: " .. hs.spaces.focusedSpace())
    for idx, d in ipairs(screenSpaces) do
        -- print("find index of current space: " .. idx .. ", spaceIdx: " .. d)
        if d == hs.spaces.focusedSpace() then
            currentDesktopIdx = idx
            -- print("found: " .. currentDesktopIdx)
            break
        end
    end
    if currentDesktopIdx == 0 then
       print("ERROR moveWndToSpace: couldn't find index of current desktop")
       return
    end
    local idx = currentDesktopIdx
    local moved = false
    repeat 
        if direction == "left" then
            idx = (idx > 1) and (idx - 1) or #screenSpaces
        else
            idx = (idx < #screenSpaces) and (idx + 1) or 1
        end
        -- print("INFO  moveWndToSpace: try space[" .. idx .. "]=" .. screenSpaces[idx]) 
        moved = hs.spaces.moveWindowToSpace(win, screenSpaces[idx])
    until moved or idx == currentDesktopIdx
    if moved then
        if focus then
            win:focus()
        end
    else
        print("INFO  moveWndToSpace: no desktops to move to")
    end
end

AxWndManager.flashScreeni=function(screen)
    if not screen then
        return
    end

    local flash=hs.canvas.new(win:screen():fullFrame()):appendElements({
        action = "fill",
        fillColor = { alpha = 0.25, red = 1},
        type = "rectangle"})
    flash:show()
    hs.timer.doAfter(.15, function () flash:delete() end)
    hs.alert.show("Not Supported")
end

-- utilities/foundation functions
--
-- https://github.com/exark/dotfiles/blob/master/.hammerspoon/init.lua
--
-- nudge: move a window with xpos in x and ypos in y pixels
AxWndManager.nudge=function(xpos, ypos)
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("nudge: ERROR no window found")
        return
    end
    local frm = win:frame()

    frm.x = frm.x + xpos
    frm.y = frm.y + ypos
    win:setFrame(frm)
end

-- Resize window for chunk of screen.
-- For x and y: use 0 to expand fully in that dimension, 0.5 to expand halfway
-- For w and h: use 1 for full, 0.5 for half
AxWndManager.push=function(x, y, w, h)
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("push: ERROR no window found")
        return
    end
    local frm = win:frame()
    local scr = win:screen()
    local max = scr:frame()

    frm.x = max.x + (max.w * x)
    frm.y = max.y + (max.h * y)
    frm.w = max.w * w
    frm.h = max.h * h
    win:setFrame(frm)
end

AxWndManager.resize=function(xpixels, ypixels)
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("resize: ERROR no window found")
        return
    end
    local frm = win:frame()

    frm.w = frm.w + xpixels
    frm.h = frm.h + ypixels
    win:setFrame(frm)
end

AxWndManager.wtf=function(msg, wnd)
    if not wnd then
        print(msg ..  "(no wnd)")
        return
    end
    local frm = wnd:frame()
    local scr = wnd:screen():frame()

    print(msg .. "Window: [x:" .. frm.x .. ", y:" .. frm.y .. ", w:" .. frm.w .. ", h:" .. frm.h .. "]")
    print(msg .. "Screen: [x:" .. scr.x .. ", y:" .. scr.y .. ", w:" .. scr.w .. ", h:" .. scr.h ..  "]")
end

AxWndManager.between=function(value, reference, delta)
    if value >= (reference - delta) and value <= (reference + delta) then
        if DEBUG then
            print("TRUE  " .. value .. " in [" .. (reference - delta) .. ", " .. (reference + delta) .. "]")
        end
        return true
    end
    if DEBUG then
        print("FALSE " .. value .. " not in [" .. (reference - delta) .. ", " .. (reference + delta) .. "]")
    end
    return false
end

--
-- Bindings
--
-- Control ^, Option, Shift, Command
local ctrl_cmd = {"ctrl", "cmd"}
local ctrl_opt = {"ctrl", "alt"}
local ctrl_opt_cmd = {"ctrl", "alt", "cmd"}
local opt_cmd = {"alt", "cmd"}
local ctrl_opt_shift = {"ctrl", "alt", "shift"}

-- Snap to screen edge {{{1
-- Raycast introduced this behavior in 1.43.0
hs.hotkey.bind(opt_cmd, "left",  AxWndManager.dynamicResizeLeft)
hs.hotkey.bind(opt_cmd, "right", AxWndManager.dynamicResizeRight)
hs.hotkey.bind(opt_cmd, "up",    AxWndManager.dynamicResizeTop)
hs.hotkey.bind(opt_cmd, "down",  AxWndManager.dynamicResizeBottom)
hs.hotkey.bind(ctrl_cmd, "h", "◀️", AxWndManager.dynamicResizeLeft)
hs.hotkey.bind(ctrl_cmd, "l", "▶️", AxWndManager.dynamicResizeRight)
hs.hotkey.bind(ctrl_cmd, "k", "🔼", AxWndManager.dynamicResizeTop)
hs.hotkey.bind(ctrl_cmd, "j", "🔽", AxWndManager.dynamicResizeBottom)
-- }}}

-- Predefined positions:
-- Raycast supports Center
AxWndManager.center=function()
    local wnd = hs.window.focusedWindow() or hs.window.frontmostWindow() 
    if wnd then
        wnd:centerOnScreen() 
    end
end
hs.hotkey.bind(ctrl_cmd, ".", "", AxWndManager.center)
hs.hotkey.bind(ctrl_cmd, "z", ".", AxWndManager.center)

-- Raycast -- Enlarged vertically
hs.hotkey.bind(ctrl_cmd, "\\", AxWndManager.expandVertically)
-- -- Enlarged horizontally
hs.hotkey.bind(ctrl_cmd, "=", AxWndManager.expandHorizontally)
-- -- Fullscreen
hs.hotkey.bind(ctrl_opt_cmd, "f", function() push(0.01, 0.01, .95, .95) end)

-- Raycast works better for Move to different monitor
hs.hotkey.bind(ctrl_opt_cmd, "s", function() AxWndManager.moveToMonitor(-1) end)

-- Move to left/right dekstop
-- Raycast works better for these
hs.hotkey.bind(ctrl_opt_cmd, "l", function() AxWndManager.moveWndToSpace('right', true) end)
hs.hotkey.bind(ctrl_opt_cmd, "h", function() AxWndManager.moveWndToSpace('left', true) end)
hs.hotkey.bind(ctrl_opt_cmd, "k", function() AxWndManager.moveWndToSpace('right', false) end)
hs.hotkey.bind(ctrl_opt_cmd, "j", function() AxWndManager.moveWndToSpace('left', false) end)


-- -- Move
-- hs.hotkey.bind(alt_shift_cmd, 'down',  function() AxWndManager.nnudge(0, 50) end)
-- hs.hotkey.bind(alt_shift_cmd, "up",    function() AxWndManager.nudge(0,-50) end)
-- hs.hotkey.bind(alt_shift_cmd, "right", function() AxWndManager.nudge(50,0) end)
-- hs.hotkey.bind(alt_shift_cmd, "left",  function() AxWndManager.nudge(-50, 0) end)

-- hs.hotkey.bind(ctrl_opt_cmd, 'up',    function() AxWndManager.resize(0, -50) end)
-- hs.hotkey.bind(ctrl_opt_cmd, 'down',  function() AxWndManager.resize(0, 50) end)
-- hs.hotkey.bind(ctrl_opt_cmd, 'right', function() AxWndManager.resize(50, 0) end)
-- hs.hotkey.bind(ctrl_opt_cmd, 'left',  function() AxWndManager.resize(-50, 0) end)


