local DEBUG = true
local windowGap = 3
Stack = {}

function Stack:Create(size)
    local t = {}

    t._et = {}
    t._maxs = size or -1

    function t:push(...)
        if ...  then
            local targs = {...}
            for _, v in ipairs(targs) do
                if #self._et + 1 > self._maxs then
                    table.remove(self._et, 1)
                end
                table.insert(self._et, v)
                for k, z in pairs(v) do
                    print("added:", k, z)
                end
                print("stack size:", #self._et)
            end
        end
    end

    function t:pop(num)
        local num = num or 1
        local entries = {}

        for i = 1, num do
            if #self._et ~= 0 then
                table.insert(entries, self._et[#self._et])
                table.remove(self._et)
            else
                break
            end
        end
        for _, v in ipairs(entries) do
            for k, z in pairs(v) do
                print(k,z)
            end
        end
        return entries
        -- return unpack(entries)
    end

    function t:getn()
        return #self._et
    end

    function t:list()
        for i, v in pairs(self._et) do
            print(i, v)
        end
    end

    return t
end
local UndoStack = Stack:Create(10)

-- Disable animation
hs.window.animationDuration = 0

-- Get list of screens and refresh that list whenever screens are plugged or unplugged:
local screens = hs.screen.allScreens()
local screenwatcher = hs.screen.watcher.new(function()
	screens = hs.screen.allScreens()
end)
screenwatcher:start()

-- Key combinations
local alt = {"⌥"}
local ctrl_cmd = {"⌃", "⌘"}
local alt_shift_cmd = {"⌥", "⇧", "⌘"}
local alt_cmd = {"⌘", "⌥"}
local ctrl_alt_cmd = {"⌥", "⌃", "⌘"}
local hyper = {"⌘", "⌥", "⌃", "⇧"}

-- Hints
hs.hotkey.bind(alt_cmd, "tab", function()
    hs.hints.windowHints()
end)

-- Grid
hs.grid.HINTS = {
    {"q", "w", "e", "r", "t", "y", "u", "i"}, 
    {"o", "p", "a", "s", "d", "f", "g", "h"},
    {"j", "k", "l", "z", "x", "c", "v", "b"},
    {"n", "m", "[", "]", ";", "'", "9", "0"},
    {"1", "2", "3", "4", "5", "6", "7", "8"}
}
hs.grid.setGrid('8x6')
hs.grid.ui.textSize = 64
-- hs.grid.ui.cellStrokeColor = {0.25, 0.85, 0.85, 0.75}
-- hs.grid.ui.highlightColor = {0.40, 0.60, 0.8, 0.5}
hs.hotkey.bind(ctrl_alt_cmd, "g", function()
    hs.grid.toggleShow()
end)

hs.hotkey.bind(ctrl_alt_cmd, "z", function()
    UndoStack:list()
    local prev = UndoStack:pop()
    if #prev ~= 0 then
        state = prev[1]
        local wnd = hs.window.find(state["wnd"])
        if wnd then
            wnd:setFrame(state["frm"])
        end
    end
end)

function dynamicResizeLeft()
    local win = hs.window.focusedWindow()
    local frm = win:frame()
    local scr = win:screen():frame()
    -- UndoStack:push({wnd=win:id(), frm=frm})
    if DEBUG then   
        wtf("dynamicResizeLeft", win)
    end
    if not between(frm.x, scr.x, 5) then
        -- move to the left
        frm.x = scr.x
        hs.alert.show("< left")
    elseif not between(frm.y, scr.y, 25) then
        -- now we also resize
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w * 2/3 - windowGap
        frm.h = scr.h
        hs.alert.show("< left 2/3")
    elseif between(frm.w, scr.w * 2/3, windowGap * 2) then
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w * 1/2 - windowGap
        frm.h = scr.h
        hs.alert.show("< left 1/2")
    elseif between(frm.w, scr.w * 1/2, windowGap * 2) then
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w * 1/3 - windowGap
        frm.h = scr.h
        hs.alert.show("< left 1/3")
    else
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w * 2/3 - windowGap
        frm.h = scr.h
        hs.alert.show("< left 2/3")
    end
    win:setFrame(frm)
end

function dynamicResizeRight()
    local win = hs.window.focusedWindow()
    local frm = win:frame()
    local scr = win:screen():frame()
    -- UndoStack:push({wnd=win:id(), frm=frm})
    if DEBUG then   
        wtf("dynamicResizeRight:", win)
    end
    if not between(frm.x + frm.w, scr.x + scr.w, 5) then
        -- move to the left
        frm.x = scr.x + (scr.w - frm.w)
        hs.alert.show("right >")
    elseif not between(frm.y, scr.y, 25) then
        -- now we also resize
        frm.w = scr.w * 2/3 - windowGap
        frm.h = scr.h
        frm.x = scr.x + (scr.w - frm.w)
        frm.y = scr.y
        hs.alert.show("2/3 right >")
    elseif between(frm.w, scr.w * 2/3, windowGap * 2) then
        frm.w = scr.w * 1/2 - windowGap
        frm.h = scr.h
        frm.x = scr.x + (scr.w - frm.w)
        frm.y = scr.y
        hs.alert.show("1/2 right >")
    elseif between(frm.w, scr.w * 1/2, windowGap * 2) then
        frm.w = scr.w * 1/3 - windowGap
        frm.h = scr.h
        frm.x = scr.x + (scr.w - frm.w)
        frm.y = scr.y
        hs.alert.show("1/3 right >")
    else
        frm.w = scr.w * 2/3 - windowGap
        frm.h = scr.h
        frm.x = scr.x + (scr.w - frm.w)
        frm.y = scr.y
        hs.alert.show("2/3 right >")
    end
    win:setFrame(frm)
end


function dynamicResizeTop()
    local win = hs.window.focusedWindow()
    local frm = win:frame()
    local scr = win:screen():frame()
    -- UndoStack:push({wnd=win:id(), frm=frm})
    if DEBUG then   
        wtf("dynamicResizeTop:", win)
    end

    if not between(frm.y, scr.y, 5) then
        -- move to the left
        frm.y = scr.y
        hs.alert.show("^ top")
    elseif not between(frm.x, scr.x, 5) then
        -- now we also resize
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w
        frm.h = scr.h * .7 - windowGap
        print("top 2/3: " .. frm.string)
        hs.alert.show("^ top 2/3")
    elseif between(frm.h, scr.h * .7, windowGap * 2.5) then
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w
        frm.h = scr.h * .5 - windowGap
        print("top 1/2: " .. frm.string)
        hs.alert.show("^ top 1/2")
    elseif between(frm.h, scr.h * .5, windowGap * 2.5) then
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w
        frm.h = scr.h * 0.3 - windowGap
        print("top 1/3: " .. frm.string)
        hs.alert.show("^ top 1/3")
    else
        frm.x = scr.x
        frm.y = scr.y
        frm.w = scr.w
        frm.h = scr.h * 0.7 - windowGap
        print("re-top 2/3: " .. frm.string)
        hs.alert.show("^ re-top 2/3")
    end
    win:setFrame(frm)
end

function dynamicResizeBottom()
    local win = hs.window.focusedWindow()
    local frm = win:frame()
    local scr = win:screen():frame()
    -- UndoStack:push({wnd=win:id(), frm=frm})
    if DEBUG then   
        wtf("dynamicResizeBottom:", win)
    end
    if not between(frm.y + frm.h, scr.y + scr.h, 5) then
        -- move to the left
        frm.y = scr.y + scr.h - frm.h
        hs.alert.show("v bottom")
    elseif not (between(frm.x, scr.x, 5) and between(frm.w, scr.w, 25)) then
        -- now we also resize
        frm.w = scr.w
        frm.h = scr.h * .7 - windowGap
        frm.x = scr.x
        frm.y = scr.y + scr.h - frm.h
        print("bottom 2/3:" .. frm.string)
        hs.alert.show("v bottom 2/3")
    elseif between(frm.h, scr.h * .7, windowGap * 2) then
        frm.w = scr.w
        frm.h = scr.h * .5 - windowGap
        frm.x = scr.x
        frm.y = scr.y + scr.h - frm.h
        print("bottom 1/2:" .. frm.string)
        hs.alert.show("v bottom 1/2")
    elseif between(frm.h, scr.h * .5, windowGap * 2.5) then 
        frm.w = scr.w
        frm.h = scr.h * .3 - windowGap
        frm.x = scr.x
        frm.y = scr.y + scr.h - frm.h
        print("bottom 1/3:" .. frm.string)
        hs.alert.show("v bottom 1/3")
    else
        frm.w = scr.w
        frm.h = scr.h * .7 - windowGap
        frm.x = scr.x
        frm.y = scr.y + scr.h - frm.h
        print("re-bottom 2/3:" .. frm.string)
        hs.alert.show("v re-bottom 2/3")
    end
    win:setFrame(frm)
end

function expandVertically()
    local win = hs.window.focusedWindow()
    local frm = win:frame()
    local scr = win:screen():frame()

    frm.y = scr.y
    frm.h = scr.h
    win:setFrame(frm)
end

function expandHorizontally()
    local win = hs.window.focusedWindow()
    local frm = win:frame()
    local scr = win:screen():frame()

    frm.x = scr.x
    frm.w = scr.w
    win:setFrame(frm)
end

-- Move window to `incr` monitors from current one 
-- Index starts with 1
function moveToMonitor(incr)
	local win = hs.window.focusedWindow()
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
    local win = hs.window.focusedWindow()
    local frm = win:frame()
    frm.x = frm.x + xpos
    frm.y = frm.y + ypos
    win:setFrame(frm)
end
-- Resize window for chunk of screen.
-- For x and y: use 0 to expand fully in that dimension, 0.5 to expand halfway
-- For w and h: use 1 for full, 0.5 for half
function push(x, y, w, h)
    local win = hs.window.focusedWindow()
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
    local win = hs.window.focusedWindow()
    if not win then
        return
    end
    local frm = win:frame()

    frm.w = frm.w + xpixels
    frm.h = frm.h + ypixels
    win:setFrame(frm)
end

function wtf(msg, wnd)
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
-- Undo
--


--
-- Bindings
--

-- Snap to screen edge {{{1
hs.hotkey.bind(alt_cmd, "left", dynamicResizeLeft) 		-- left side
hs.hotkey.bind(alt_cmd, "right", dynamicResizeRight) 		-- left side
hs.hotkey.bind(alt_cmd, "up", dynamicResizeTop) 		-- top half
hs.hotkey.bind(alt_cmd, "down", dynamicResizeBottom) 		-- top half
-- }}}

-- Predefined positions:
-- Center
hs.hotkey.bind(alt_cmd, ".", function() hs.window.focusedWindow():centerOnScreen() end)
-- Enlarged vertically
hs.hotkey.bind(alt_cmd, "\\", expandVertically)
-- Enlarged horizontally
hs.hotkey.bind(alt_cmd, "=", expandHorizontally)
-- Fullscreen
hs.hotkey.bind(ctrl_alt_cmd, "f", function() push(0,0,1,1) end)

-- Move to different monitor
hs.hotkey.bind(alt_cmd, "9", function() moveToMonitor(-1) end)
hs.hotkey.bind(alt_cmd, "0", function() moveToMonitor(1) end)



-- Move
hs.hotkey.bind(alt_shift_cmd, 'down', function() nudge(0,25) end)   --down
hs.hotkey.bind(alt_shift_cmd, "up", function() nudge(0,-25) end)    --up
hs.hotkey.bind(alt_shift_cmd, "right", function() nudge(25,0) end)  --right
hs.hotkey.bind(alt_shift_cmd, "left", function() nudge(-25, 0) end)  --left

hs.hotkey.bind(hyper, 'up',    function() resize(0, -100) end)
hs.hotkey.bind(hyper, 'down',  function() resize(0, 100) end)
hs.hotkey.bind(hyper, 'right', function() resize(100, 0) end)
hs.hotkey.bind(hyper, 'left',  function() resize(-100, 0) end)

-- https://github.com/digitalbase/hammerspoon/blob/master/init.lua
