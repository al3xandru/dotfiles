local DEBUG = true

-- Key combinations
local alt = {"⌥"}
local alt_cmd = {"⌘", "⌥"}
local ctrl_cmd = {"⌃", "⌘"}
local alt_shift_cmd = {"⌥", "⇧", "⌘"}
local ctrl_alt_cmd = {"⌥", "⌃", "⌘"}
local hyper = {"⌘", "⌥", "⌃", "⇧"}

-- Disable animation
hs.window.animationDuration = 0

-- Get list of screens and refresh that list whenever screens are plugged or unplugged:
local screens = hs.screen.allScreens()
local screenwatcher = hs.screen.watcher.new(function()
	screens = hs.screen.allScreens()
end)
screenwatcher:start()

-- Managing UNDO
local undoStack = require('stack')
local UndoStack = undoStack:new(50, true)

function windowKey(wnd)
    return wnd:application():title()..":"..wnd:id()
end

function stackPosition(wnd)
    local wndKey = windowKey(wnd)
    local frm = wnd:frame()
    UndoStack:push({key=wndKey, 
                    val={
                        wnd=wnd:id(), 
                        app=wnd:application():title(),
                        x=frm.x,
                        y=frm.y,
                        w=frm.w,
                        h=frm.h}})
end

hs.hotkey.bind(ctrl_alt_cmd, "z", function()
    local wnd = hs.window.focusedWindow()
    local wndKey = windowKey(wnd)
    local prev = UndoStack:pop(wndKey)

    if prev ~= nil then
        local state = prev["val"]
        if DEBUG then
            print("Restore", wndKey, "to:{", state["x"], state["y"], state["w"], state["h"], "}")
        end
        -- this is an important trick to avoid cycling between the last 2 states
        UndoStack:deleteCache(wndKey)

        local frm = wnd:frame()
        frm.x = state["x"]
        frm.y = state["y"]
        frm.w = state["w"]
        frm.h = state["h"]
        wnd:setFrame(frm)
    end
end)

-- https://groups.google.com/d/msg/hammerspoon/d371xDcRsCo/W89am9oACwAJ
hs.window.filter.allowedWindowRoles = {AXStandardWindow=true,AXDialog=false}
local wndFilter = hs.window.filter.new()
wndFilter:setAppFilter("Alfred 3", false)
wndFilter:setAppFilter("Bartender 2", false)
wndFilter:setAppFilter("Day One Networking")
wndFilter:setAppFilter("Electron Helper", false)
wndFilter:setAppFilter("Evernote Networking")
wndFilter:setAppFilter("Safari Technology Preview Networking", false)
wndFilter:subscribe({hs.window.filter.windowCreated, 
                     hs.window.filter.windowMoved,
                     hs.window.filter.windowUnminimized}, stackPosition)

-- Predefined layouts
require('layouts')

-- Layout snapshots
-- require('snapshot')

-- Hints
hs.hotkey.bind(alt_cmd, "tab", function()
    hs.hints.windowHints()
end)

--
-- Window management
--
--- Grid
hs.grid.HINTS = {
    {"q", "w", "e", "r", "t", "y", "u", "i"}, 
    {"o", "p", "a", "s", "d", "f", "g", "h"},
    {"j", "k", "l", "z", "x", "c", "v", "b"},
    {"n", "[", "]", "m",  "9", ";", "'", "0"},
    {"1", "2", "3", "4", "5", "6", "7", "8"},
}
hs.grid.setGrid('8x8')
hs.grid.ui.textSize = 32
hs.hotkey.bind(ctrl_alt_cmd, "g", function()
    hs.grid.toggleShow()
end)

--- Window moves
require('windowmanager')





-- require("controlescape")

-- https://github.com/digitalbase/hammerspoon/blob/master/init.lua
