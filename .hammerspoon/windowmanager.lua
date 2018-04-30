-- WINDOW MANAGEMENT

-- settings
local DEBUG = true
local DEFAULT_WND_GAP = 3
--
-- Utility functions
-- 
function dynamicResizeLeft()
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("dynamicResizeLeft: ERROR no window found")
        return
    elseif DEBUG then   
        wtf("dynamicResizeLeft", win)
    end

    local frm = win:frame()
    local scr = win:screen():frame()

    if not between(frm.x, scr.x, 5) then
        -- move to the left
        frm.x = scr.x
        hs.alert.show("⬅ ")
    elseif not between(frm.y, scr.y, 25) then
        -- now we also resize
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w * 2/3 - DEFAULT_WND_GAP
        frm.h = scr.h
        hs.alert.show("⬅  2/3")
    elseif between(frm.w, scr.w * 2/3, DEFAULT_WND_GAP * 2) then
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w * 1/2 - DEFAULT_WND_GAP
        frm.h = scr.h
        hs.alert.show("⬅  1/2")
    elseif between(frm.w, scr.w * 1/2, DEFAULT_WND_GAP * 2) then
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

function dynamicResizeRight()
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("dynamicResizeRight: ERROR no window found")
        return
    elseif DEBUG then
        print("dynamicResizeRight", win)
    end

    local frm = win:frame()
    local scr = win:screen():frame()

    if not between(frm.x + frm.w, scr.x + scr.w, 5) then
        -- move to the left
        frm.x = scr.x + (scr.w - frm.w)
        hs.alert.show("➡")
    elseif not between(frm.y, scr.y, 25) then
        -- now we also resize
        frm.w = scr.w * 2/3 - DEFAULT_WND_GAP
        frm.h = scr.h
        frm.x = scr.x + (scr.w - frm.w)
        frm.y = scr.y
        hs.alert.show("2/3 ➡")
    elseif between(frm.w, scr.w * 2/3, DEFAULT_WND_GAP * 2) then
        frm.w = scr.w * 1/2 - DEFAULT_WND_GAP
        frm.h = scr.h
        frm.x = scr.x + (scr.w - frm.w)
        frm.y = scr.y
        hs.alert.show("1/2 ➡")
    elseif between(frm.w, scr.w * 1/2, DEFAULT_WND_GAP * 2) then
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

function dynamicResizeTop()
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("dynamicResizeTop: ERROR no window found")
        return
    elseif DEBUG then   
        wtf("dynamicResizeTop:", win)
    end

    local frm = win:frame()
    local scr = win:screen():frame()
    -- UndoStack:push({wnd=win:id(), frm=frm})

    if not between(frm.y, scr.y, 5) then
        -- move to the left
        frm.y = scr.y
        hs.alert.show("⬆")
    elseif not between(frm.x, scr.x, 5) then
        -- now we also resize
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w
        frm.h = scr.h * .7 - DEFAULT_WND_GAP
        print("top 2/3: " .. frm.string)
        hs.alert.show("⬆ 2/3")
    elseif between(frm.h, scr.h * .7, DEFAULT_WND_GAP * 2.5) then
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w
        frm.h = scr.h * .5 - DEFAULT_WND_GAP
        print("top 1/2: " .. frm.string)
        hs.alert.show("⬆ 1/2")
    elseif between(frm.h, scr.h * .5, DEFAULT_WND_GAP * 2.5) then
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w
        frm.h = scr.h * 0.3 - DEFAULT_WND_GAP
        print("top 1/3: " .. frm.string)
        hs.alert.show("⬆ 1/3")
    else
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w
        frm.h = scr.h * 0.7 - DEFAULT_WND_GAP
        print("re-top 2/3: " .. frm.string)
        hs.alert.show("⬆ 2/3")
    end
    win:setFrame(frm)
end

function dynamicResizeBottom()
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("dynamicResizeBottom: ERROR no window found")
        return
    elseif DEBUG then   
        wtf("dynamicResizeBottom:", win)
    end

    local frm = win:frame()
    local scr = win:screen():frame()
    -- UndoStack:push({wnd=win:id(), frm=frm})
    --
    if not between(frm.y + frm.h, scr.y + scr.h, 5) then
        -- move to the left
        frm.y = scr.y + scr.h - frm.h
        hs.alert.show("⬇")
    elseif not (between(frm.x, scr.x, 5) and between(frm.w, scr.w, 25)) then
        -- now we also resize
        frm.w = scr.w
        frm.h = scr.h * .7 - DEFAULT_WND_GAP
        frm.x = scr.x
        frm.y = scr.y + scr.h - frm.h
        print("bottom 2/3:" .. frm.string)
        hs.alert.show("⬇ 2/3")
    elseif between(frm.h, scr.h * .7, DEFAULT_WND_GAP * 2) then
        frm.w = scr.w
        frm.h = scr.h * .5 - DEFAULT_WND_GAP
        frm.x = scr.x
        frm.y = scr.y + scr.h - frm.h
        print("bottom 1/2:" .. frm.string)
        hs.alert.show("⬇ 1/2")
    elseif between(frm.h, scr.h * .5, DEFAULT_WND_GAP * 2.5) then 
        frm.w = scr.w
        frm.h = scr.h * .3 - DEFAULT_WND_GAP
        frm.x = scr.x
        frm.y = scr.y + scr.h - frm.h
        print("bottom 1/3:" .. frm.string)
        hs.alert.show("⬇ 1/3")
    else
        frm.w = scr.w
        frm.h = scr.h * .7 - DEFAULT_WND_GAP
        frm.x = scr.x
        frm.y = scr.y + scr.h - frm.h
        print("re-bottom 2/3:" .. frm.string)
        hs.alert.show("⬇ 2/3")
    end
    win:setFrame(frm)
end

function expandVertically()
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

function expandHorizontally()
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

-- Move window to `incr` monitors from current one 
-- Index starts with 1
function moveToMonitor(incr)
    local win = hs.window.focusedWindow() or hs.window.frontmostWindow()
    if not win then
        print("moveToMonitor: ERROR no window found")
        return
    end
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

-- utilities/foundation functions
--
-- https://github.com/exark/dotfiles/blob/master/.hammerspoon/init.lua
--
-- nudge: move a window with xpos in x and ypos in y pixels
function nudge(xpos, ypos)
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
function push(x, y, w, h)
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

function resize(xpixels, ypixels)
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

function wtf(msg, wnd)
    if not wnd then
        print(msg ..  "(no wnd)")
        return
    end
    local frm = wnd:frame()
    local scr = wnd:screen():frame()

    print(msg .. "Window: [x:" .. frm.x .. ", y:" .. frm.y .. ", w:" .. frm.w .. ", h:" .. frm.h .. "]")
    print(msg .. "Screen: [x:" .. scr.x .. ", y:" .. scr.y .. ", w:" .. scr.w .. ", h:" .. scr.h ..  "]")
end

function between(value, reference, delta)
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
local alt_cmd = {"⌘", "⌥"}
local alt_shift_cmd = {"⌥", "⇧", "⌘"}
local ctrl_alt_cmd = {"⌥", "⌃", "⌘"}

-- Snap to screen edge {{{1
hs.hotkey.bind(alt_cmd, "left",  dynamicResizeLeft)
hs.hotkey.bind(alt_cmd, "right", dynamicResizeRight)
hs.hotkey.bind(alt_cmd, "up",    dynamicResizeTop)
hs.hotkey.bind(alt_cmd, "down",  dynamicResizeBottom)
-- }}}

-- Predefined positions:
-- Center
hs.hotkey.bind(alt_cmd, ".", function() 
    local wnd = hs.window.focusedWindow() or hs.window.frontmostWindow() 
    if wnd then
        wnd:centerOnScreen() 
    end
end)
-- Enlarged vertically
hs.hotkey.bind(alt_cmd, "=", expandVertically)
-- Enlarged horizontally
hs.hotkey.bind(alt_cmd, "-", expandHorizontally)
-- Fullscreen
hs.hotkey.bind(ctrl_alt_cmd, "f", function() push(0,0,1,1) end)

-- Move to different monitor
hs.hotkey.bind(alt_cmd, "9", function() moveToMonitor(-1) end)
hs.hotkey.bind(alt_cmd, "0", function() moveToMonitor(1) end)



-- Move
hs.hotkey.bind(alt_shift_cmd, 'down',  function() nudge(0, 50) end)
hs.hotkey.bind(alt_shift_cmd, "up",    function() nudge(0,-50) end)
hs.hotkey.bind(alt_shift_cmd, "right", function() nudge(50,0) end)
hs.hotkey.bind(alt_shift_cmd, "left",  function() nudge(-50, 0) end)

hs.hotkey.bind(ctrl_alt_cmd, 'up',    function() resize(0, -50) end)
hs.hotkey.bind(ctrl_alt_cmd, 'down',  function() resize(0, 50) end)
hs.hotkey.bind(ctrl_alt_cmd, 'right', function() resize(50, 0) end)
hs.hotkey.bind(ctrl_alt_cmd, 'left',  function() resize(-50, 0) end)

