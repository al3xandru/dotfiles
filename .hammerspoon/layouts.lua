--
-- PREDEFINED LAYOUTS
--

-- Define window layouts
--   Format reminder:
--     {"App name", "Window name", "Display Name/function", "unitrect", "framerect", "fullframerect"},
LAYOUTS = {
    calls = {
        name = "Video calls",
        subtitle = "Zoom, Evernote, Safari, Slack",
        layout = {
            {"zoom.us", nil, nil, hs.geometry.unitrect(0, 0, 0.6, 0.6), nil, nil},
            {"zoom.us", "Zoom - Pro Account", nil, hs.geometry.unitrect(0.8, 0, 0.1, 0.1)},
            {"Evernote", nil, nil, hs.geometry.unitrect(0.6, 0, 0.4, 0.8), nil, nil},
            {"Slack", nil, attemptSecondaryScreen, hs.geometry.unitrect(0, 0.5, 0.75, 0.5), nil, nil},
            {"Safari", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.25, 0.5, 0.75, 0.5), nil, nil}
        }
    },
    communication = {
        name = "Comms & Scheduling",
        subtitle = "Mail, Slack, Calendar, Zimbra",
        layout = {
            {"Mail", nil, attemptSecondaryScreen, hs.geometry.unitrect(0, 0, 1, .7), nil, nil},
            {"Slack", nil, attemptSecondaryScreen, hs.geometry.unitrect(0, .3, 1, .7), nil, nil},
            {"Calendar", nil, attemptSecondaryScreen, hs.geometry.unitrect(.5, 0, .5, .75), nil, nil},
            {"Firefox", nil, attemptSecondaryScreen, hs.geometry.unitrect(.5, .25, .5, .75), nil, nil}
        }
    },
    planning = {
        name = "Task planning",
        subtitle = "TaskPaper, OmniFocus",
        layout = {
            {"OmniFocus", nil, nil, hs.geometry.unitrect(0, 0, 1, 0.70), nil, nil},
            {"TaskPaper", nil, nil, hs.geometry.unitrect(0, .35, 1, .65), nil, nil}
        }
    },
    -- coding setups
    -- goals:
    -- 1.  find a way for some apps to be visible at the same way
    ide = {
        name = "IDE Session - Single monitor",
        subtitle = "IntelliJ/PyCharm/GoLand, Safari/Chrome, Dash, iBooks/Preview",
        layout = {
            {"iBooks", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.5, 0, 0.5, 0.98), nil, nil},
            {"Preview", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.5, 0.02, 0.5, 0.98), nil, nil},
            {"Safari", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.45, 0.04, 0.55, 0.94), nil, nil},
            {"Google Chrome", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.45, 0.06, 0.5, 0.94), nil, nil},
            {"Dash", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.4, 0, 0.6, 0.94), nil, nil},
            {"IntelliJ IDEA", nil, nil, hs.geometry.unitrect(0, 0, .6, 1)},
            {"PyCharm", nil, nil, hs.geometry.unitrect(0, 0, .6, 1)},
            {"GoLand", nil, nil, hs.geometry.unitrect(0, 0, .6, 1)},
        }
    },
    ide_dualmonitor = {
        name = "IDE Session - Dual monitor",
        subtitle = "IntelliJ/PyCharm/GoLand, Safari/Chrome, Dash, iBooks/Preview",
        layout = {
            {"iBooks", nil, attemptSecondaryScreen, hs.geometry.unitrect(0, 0, 0.5, 0.98), nil, nil},
            {"Preview", nil, attemptSecondaryScreen, hs.geometry.unitrect(0, 0.02, 0.5, 0.98), nil, nil},
            {"Safari", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.4, 0.0, 0.6, 0.98), nil, nil},
            {"Google Chrome", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.4, 0.02, 0.6, 0.98), nil, nil},
            {"Dash", nil, nil, hs.geometry.unitrect(0.4, 0, 0.6, 1), nil, nil},
            {"IntelliJ IDEA", nil, nil, hs.geometry.unitrect(0, 0, .6, 1)},
            {"PyCharm", nil, nil, hs.geometry.unitrect(0, 0, .6, 1)},
            {"GoLand", nil, nil, hs.geometry.unitrect(0, 0, .6, 1)},
        }
    },
    terminal = {
        name = "Terminal coding - Single monitor",
        subtitle = "Terminal, Dash, Safari/Chrome, iBooks/Preview",
        layout = {
            {"iBooks", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.5, 0.04, 0.5, 0.96), nil, nil},
            {"Preview", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.5, 0.03, 0.5, 0.96), nil, nil},
            {"Google Chrome", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.5, 0.02, 0.5, 0.96), nil, nil},
            {"Safari", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.5, 0.01, 0.5, 0.96), nil, nil},
            {"Dash", nil, nil, hs.geometry.unitrect(0.5, 0, 0.5, 0.96), nil, nil},
            {"Terminal", nil, nil, hs.geometry.unitrect(0, 0, 0.5, 1), nil, nil},
        },
        actions = {
            function() hs.application.launchOrFocus("Minuteur") end,
            function() hs.notify.show("Hammerspoon layout", "Activated \"Terminal coding - Single monitor\"", "Turn on Hocus Focus Coding") end,
        }
    },
    terminal_dualmonior= {
        name = "Terminal coding - Dual monitor",
        subtitle = "Terminal, Dash, Safari/Chrome, iBooks/Preview",
        layout = {
            {"Bear", nil, nil, hs.geometry.unitrect(0.6, 0.5, 0.4, 0.5), nil, nil},
            {"iBooks", nil, attemptSecondaryScreen, hs.geometry.unitrect(0, 0, 0.5, 0.98), nil, nil},
            {"Preview", nil, attemptSecondaryScreen, hs.geometry.unitrect(0, 0.02, 0.5, 0.98), nil, nil},
            {"Safari", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.4, 0, 0.6, 0.98), nil, nil},
            {"Google Chrome", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.4, 0.02, 0.6, 0.98), nil, nil},
            {"Dash", nil, nil, hs.geometry.unitrect(0.45, 0, 0.55, 0.9), nil, nil},
            {"Terminal", nil, nil, hs.geometry.unitrect(0, 0, 0.6, 1), nil, nil},
        },
        actions = {
            function() hs.application.launchOrFocus("Minuteur") end,
            function() hs.notify.show("Hammerspoon layout", "Activated \"Terminal coding - Dual monitor\"", "Turn on Hocus Focus Coding") end,
        }
    },
    vim = {
        name = "Vim GUI - Single monitor",
        subtitle = "MacVim/VimR, Terminal, Safari/Chrome, Dash, iBooks/Preview",
        layout = {
            {"iBooks", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.5, 0, 0.5, 0.98), nil, nil},
            {"Preview", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.5, 0.02, 0.5, 0.98), nil, nil},
            {"Safari", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.45, 0.04, 0.55, 0.94), nil, nil},
            {"Google Chrome", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.45, 0.06, 0.55, 0.94), nil, nil},
            {"Dash", nil, nil, hs.geometry.unitrect(0.4, 0, 0.6, 0.94), nil, nil},
            {"MacVim", nil, nil, hs.geometry.unitrect(0, 0, 0.5, 0.80), nil, nil},
            {"VimR", nil, nil, hs.geometry.unitrect(0, 0, 0.5, 0.80), nil, nil},
            {"Terminal", nil, nil, hs.geometry.unitrect(0, 0.4, 1, 0.6), nil, nil},
        }
    },
    vim_dualmonitor = {
        name = "Vim GUI - Dual monitor",
        subtitle = "MacVim/VimR, Terminal, Safari/Chrome, Dash, iBooks/Preview",
        layout = {
            {"iBooks", nil, attemptSecondaryScreen, hs.geometry.unitrect(0, 0, 0.5, 0.98), nil, nil},
            {"Preview", nil, attemptSecondaryScreen, hs.geometry.unitrect(0, 0.02, 0.5, 0.98), nil, nil},
            {"Safari", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.45, 0, 0.55, 0.98), nil, nil},
            {"Google Chrome", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.45, 0.02, 0.55, 0.98), nil, nil},
            {"Dash", nil, nil, hs.geometry.unitrect(0.4, 0, 0.6, 0.85), nil, nil},
            {"MacVim", nil, nil, hs.geometry.unitrect(0, 0, 0.5, 0.85), nil, nil},
            {"VimR", nil, nil, hs.geometry.unitrect(0, 0, 0.5, 0.85), nil, nil},
            {"Terminal", nil, nil, hs.geometry.unitrect(0, 0.4, 1, 0.6), nil, nil},
        }
    },
    
}

-- dynamically determine if there's a secondary screen
function attemptSecondaryScreen()
    local screens = hs.screen.allScreens()
    local primary = hs.screen.primaryScreen()
    -- print("main", primary:id(), primary:name(), primary)
    local usePrimary = true
    local result = primary
    if #screens > 1 then
        for _, scr in ipairs(screens) do
            if DEBUG then
                print("scr ", scr:id(), scr:name(), scr)
            end
            if scr ~= hs.screen.primaryScreen() then
                result = scr
                usePrimary = false
                break
            end
        end
    end
    if DEBUG then
        print("use screen:", result, " primary:", usePrimary)
    end
    return result
end

-- apply a selected layout
function applyLayoutPreset(result)
    if DEBUG then
        print("activate layout:", result["uuid"])
    end
    local selectedLayout = LAYOUTS[result["uuid"]]["layout"]

    -- hide all apps that are not in the selected layout
    local layoutActiveApps = {}
    layoutActiveApps["Hammerspoon"] = true
    for _, appLayout in pairs(selectedLayout) do
        layoutActiveApps[appLayout[1]] = true
    end
    if DEBUG then
        print("  1. apps in layout", hs.inspect.inspect(layoutActiveApps))
    end

    if DEBUG then
        print("  2. minimize other apps")
    end
    for _, wnd in pairs(hs.window.allWindows()) do
        if not layoutActiveApps[wnd:application():title()] then
            wnd:minimize()
        end
    end

    -- unsure if layouts work with minimized windows
    -- so let's restore them
    if DEBUG then
        print("  3. unminimize apps in layout")
    end
    if UNMINIMIZE_WINDOWS_IN_LAYOUT then
        for _, l in pairs(selectedLayout) do
            local app = hs.appfinder.appFromName(l[1])
            if app ~= nil then
                for _, wnd in ipairs(app:allWindows()) do
                    wnd:unminimize()
                end
            end
        end
    end

    if DEBUG then
        print("  4. calculate layout details")
    end
    local updatedLayout = {}
    for i, l in ipairs(selectedLayout) do
        if DEBUG then
            print("     processing layout def", hs.inspect.inspect(l))
        end
        local app = hs.appfinder.appFromName(l[1])
        if app ~= nil and app:isRunning() then
            if type(l[3]) ~= "function" then
                table.insert(updatedLayout, {l[1], l[2], l[3], l[4], l[5], l[6]})
                print("      use static screen for app ", app:title(), hs.inspect.inspect(updatedLayout[#updatedLayout]))
            else
                local scr = l[3]()
                table.insert(updatedLayout, {l[1], l[2], scr, l[4], l[5], l[6]})
                print("      use dynamic screen for app", app:title(), hs.inspect.inspect(updatedLayout[#updatedLayout]))
            end
        else
            if DEBUG then
                print("      dismiss def", l[1], "app is not running")
            end
        end
    end
    if DEBUG then
        print("  5. applying computed layout", result["uuid"], hs.inspect.inspect(updatedLayout))
    end
    hs.layout.apply(updatedLayout)
    if DEBUG then
        print("  6. execute end actions", result["uuid"])
    end 
    
    if LAYOUTS[result["uuid"]]["actions"] ~= nil then
        print("    6.1. layout \"", LAYOUTS[result["uuid"]]["name"], "\" has actions")
        for _, f in ipairs(LAYOUTS[result["uuid"]]["actions"]) do
            if not pcall(f) then
                print("        action invocation failed")
            end
        end
    else
        print("    6.1. layout \"", LAYOUTS[result["uuid"]]["name"], "\" has no actions")
    end
end

function saveLayoutSnapshot()
    -- print("d:1")
    local ok, apps, _ = hs.osascript.applescript('tell application "System Events" to get name of (processes where background only is false)')
    -- print("d:2", ok)
    if not ok then
        return
    end
    -- print("d:3")
    function rectToString(r)
        return "hs.geometry.rect(" ..  r.x ..  "," ..  r.y ..  "," ..  r.w ..  "," ..  r.h ..  ")"
    end

    function rectToTbl(r)
        return {["x"] = r.x, ["y"] = r.y, ["w"] = r.w, ["h"] = r.h}
    end


    local result = {}
    for _, appName in ipairs(apps) do
        if appName ~= "Textual" then
            -- print("d:4:inc", appName)
            local app = hs.appfinder.appFromName(appName)
            -- print("d:4:beg", app:name())

            if app ~= nil and app:isRunning() and not app:isHidden() then
                local wnds = app:visibleWindows()
                -- print("  d:5:beg", app:name())
                if wnds ~= nil and #wnds > 0 then
                    local wndPositions = {}
                    for _, wnd in ipairs(wnds) do
                        table.insert(wndPositions, rectToTbl(wnd:frame()))
                    end
                    result[app:title()] = wndPositions

                end
                -- print("  d:5:end")
            end
            -- print("d:4:end", app:name())
        end
    end

    print(hs.json.encode(result))
    local fileName = os.date("%Y-%m-%dT%H%M%S.json")
    io.output(os.getenv("HOME").."/.sessions/hammerspoon_layout/"..fileName)
    io.write(hs.json.encode(result))
    io.close()
    hs.alert.show("Layout saved:"..fileName)
    -- print(hs.inspect.inspect(result))
    -- return result
end

function loadLayoutSnapshot(result)
    print("selected:", result["uuid"])
    local snapshotFile = os.getenv("HOME").."/.sessions/hammerspoon_layout/"..result["uuid"]
    io.input(snapshotFile)
    local layout = hs.json.decode(io.read("*all"))
    io.close()
    print(hs.inspect.inspect(layout))

    for _, wnd in pairs(hs.window.allWindows()) do
        if wnd:application():title() ~= "Hammerspoon" then
            wnd:minimize()
        end
    end
    for appName, wnds in pairs(layout) do
        print("app:", appName, "has", #wnds, "windows")
        app = hs.application.get(appName)
        if app == nil then
            print("  ", appName, "not found")
            app = hs.application.open(appName)
        end 
        if app ~= nil and app:isRunning() then
            print("  ", appName, "now running")
            local idx = 0
            while(idx < #wnds - #app:visibleWindows())
            do
                idx = idx + 1
                if not app:selectMenuItem("New Window", true) then
                    print("    app doesn't have menu 'New Window'", appName)
                    if not app:selectMenuItem("New", true) then
                        print("    app doesn't have menu 'New'", appName)
                        hs.osascript.applescript('tell application "System Events" \n activate application "' ..  appName ..  '"\n tell process "' ..  appName ..  '" to keystroke "n" using command down \n end tell')
                    else
                        print("    app has menu 'New'", appName)
                    end
                else
                    print("    app has menu 'New Window'", appName)
                end
            end
            for idx, visibleWnd in ipairs(app:visibleWindows()) do
                print("    layout for window", idx, "of", appName)
                local frm = visibleWnd:frame()
                frm.x = wnds[idx]["x"]
                frm.y = wnds[idx]["y"]
                frm.w = wnds[idx]["w"]
                frm.h = wnds[idx]["h"]
                visibleWnd:setFrame(frm)
            end
        else
            print("app:", appName, "could not be started")
        end
    end
end

-- bind shortcut
local ctrl_alt_cmd = {"⌥", "⌃", "⌘"}
hs.hotkey.bind(ctrl_alt_cmd, "l", function()
    local chooser = hs.chooser.new(function(result)
        if result == nil then
            return
        end
        if result["uuid"] == "save" then
            saveLayoutSnapshot()
        elseif result["uuid"] =="load" then
            loadLayoutSnapshotList()
        else
            chooseLayoutPresets()
        end
    end)

    -- index layouts for presenting the chooser
    local choices = {
        {
            ["uuid"] = "save",
            ["text"] = "Save",
            ["subText"] = "Saving a new layout"
        },
        {
            ["uuid"] = "load",
            ["text"] = "Load",
            ["subText"] = "Loading a layout"
        },
        {
            ["uuid"] = "preset",
            ["text"] = "Presets",
            ["subText"] = "Load presets"
        }
    }
    chooser:choices(choices)
    chooser:show()
end)

function loadLayoutSnapshotList()
    local chooser = hs.chooser.new(function(result)
        if result == nil then
            return
        end
        loadLayoutSnapshot(result)
    end)
    local dir = os.getenv("HOME").."/.sessions/hammerspoon_layout/"
    local choices = {}
    iter_fn, dir_obj = hs.fs.dir(dir)
    while true do
        local file = iter_fn(dir_obj)
        if file == nil then break end
        if hs.fs.attributes(dir ..  file)["mode"] == "file" then
            table.insert(choices, {["uuid"] = file, ["text"] = file})
        end
    end
    chooser:choices(choices)
    chooser:show()
end

function chooseLayoutPresets()
    local chooser = hs.chooser.new(function(result)
        if result == nil then
            return
        end
        applyLayoutPreset(result)
    end)

    -- index layouts for presenting the chooser
    local choices = {}
    for k, v in pairs(LAYOUTS) do
        table.insert(choices, {["text"] = v["name"], ["subText"] = v["subtitle"], ["uuid"] = k}) 
    end
    chooser:choices(choices)
    chooser:show()
end
