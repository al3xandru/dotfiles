--
-- PREDEFINED LAYOUTS
--
local DEBUG = true
local UNMINIMIZE_WINDOWS_IN_LAYOUT = false

-- dynamically determine if there's a secondary screen
function attemptSecondaryScreen()
    local screens = hs.screen.allScreens()
    local primary = hs.screen.primaryScreen()
    print("main", primary:id(), primary:name(), primary)
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
            {"GoLand", nil, nil, hs.geometry.new("[0, 0, 60, 95]")},
            {"IntelliJ IDEA", nil, nil, hs.geometry.new("[0, 0, 60, 95]")},
            {"PyCharm", nil, nil, hs.geometry.new("[0, 0, 60, 95]")},

            {"Brave Browser", nil, nil, hs.geometry.new("[60, 0, 100, 85]"), nil, nil},
            {"Firefox", nil, nil, hs.geometry.new("[60, 0, 100, 85]"), nil, nil},
            {"Google Chrome", nil, nil, hs.geometry.new("[60, 0, 100, 85]"), nil, nil},
            {"qutebrowser", nil, nil, hs.geometry.new("[60, 0, 100, 85]"), nil, nil},
            {"Safari", nil, nil, hs.geometry.new("[60, 5, 1, 90]"), nil, nil},

            {"Dash", nil, nil, hs.geometry.new("[60, 50, 100, 95]"), nil, nil},
            {"Terminal", nil, nil, hs.geometry.new("[0, 65, 100, 100]"), nil, nil},

            -- {"Books", nil, nil, hs.geometry.unitrect(0.5, 0, 0.5, 0.98), nil, nil},
            -- {"Preview", nil, nil, hs.geometry.unitrect(0.5, 0.02, 0.5, 0.98), nil, nil},
        },
        actions = {
            function() hs.notify.show("Hammerspoon layouts", "Activated \"IDE Session - Single monitor\"",  "Turn Hocus Focus to Coding") end
        }
    },
    terminal = {
        name = "Terminal coding - Single monitor",
        subtitle = "Terminal, Dash, Safari/Chrome, iBooks/Preview",
        layout = {
            -- {"iBooks", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.5, 0.04, 0.5, 0.96), nil, nil},
            -- {"Preview", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.5, 0.03, 0.5, 0.96), nil, nil},
            {"Terminal", nil, nil, hs.geometry.new("[0, 0, 55, 90]"), nil, nil},

            {"Brave Browser", nil, nil, hs.geometry.new("[56, 0, 100, 85]"), nil, nil},
            {"Firefox", nil, nil, hs.geometry.new("[56, 0, 100, 85]"), nil, nil},
            {"Google Chrome", nil, nil, hs.geometry.new("[56, 0, 100, 85]"), nil, nil},
            {"qutebrowser", nil, nil, hs.geometry.new("[56, 0, 100, 85]"), nil, nil},
            {"Safari", nil, nil, hs.geometry.new("[56, 5, 1, 90]"), nil, nil},

            {"Dash", nil, nil, hs.geometry.new("[56, 45, 100, 90]"), nil, nil},
        },
        actions = {
            -- function() hs.application.launchOrFocus("Minuteur") end,
            function() hs.notify.show("Hammerspoon layout", "Activated \"Terminal coding - Single monitor\"", "Turn on Hocus Focus Coding") end,
        }
    },
    vim = {
        name = "Vim GUI - Single monitor",
        subtitle = "MacVim/VimR, Terminal, Safari/Chrome, Dash, iBooks/Preview",
        layout = {
            {"MacVim", nil, nil, hs.geometry.new("[0, 0, 55, 90]"), nil, nil},
            {"VimR", nil, nil, hs.geometry.new("[0, 0, 55, 90]"), nil, nil},

            {"Brave Browser", nil, nil, hs.geometry.new("[56, 0, 100, 85]"), nil, nil},
            {"Firefox", nil, nil, hs.geometry.new("[56, 0, 100, 85]"), nil, nil},
            {"Google Chrome", nil, nil, hs.geometry.new("[56, 0, 100, 85]"), nil, nil},
            {"qutebrowser", nil, nil, hs.geometry.new("[56, 0, 100, 85]"), nil, nil},
            {"Safari", nil, nil, hs.geometry.new("[56, 5, 1, 90]"), nil, nil},

            {"Dash", nil, nil, hs.geometry.new("[56, 45, 100, 90]"), nil, nil},
            {"Terminal", nil, nil, hs.geometry.new("[0, 60, 100, 100]"), nil, nil},
            -- {"iBooks", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.5, 0, 0.5, 0.98), nil, nil},
            -- {"Preview", nil, attemptSecondaryScreen, hs.geometry.unitrect(0.5, 0.02, 0.5, 0.98), nil, nil},
        },
        actions = {
            function() hs.notify.show("Hammerspoon layouts", "Activated \"Vim GUI - Single monitor\"",  "Turn Hocus Focus to Coding") end
        }
    },
    -- Layouts for dual monitor environments
    ide_dualmonitor = {
        name = "IDE Session - Dual monitor",
        subtitle = "IDE, browser, Dash, Books/Preview, Terminal",
        layout = {
            -- secondary left
            {"Preview", nil, attemptSecondaryScreen, hs.geometry.new("[0, 0, 35, 90]"), nil, nil},
            {"Safari", nil, attemptSecondaryScreen, hs.geometry.new("[0, 10, 35, 100]"), nil, nil},
            {"Safari Technology Preview", nil, attemptSecondaryScreen, hs.geometry.new("[0, 10, 35, 100]"), nil, nil},
            -- secondary center
            {"Terminal", nil, attemptSecondaryScreen, hs.geometry.new("[35, 0, 65, 50]"), nil, nil},
            {"Dash", nil, attemptSecondaryScreen, hs.geometry.new("[35, 50, 65, 100]"), nil, nil},
            -- secondary right
            {"Brave Browser", nil, attemptSecondaryScreen, hs.geometry.new("[65, 10, 100, 100]"), nil, nil},
            {"Firefox", nil, attemptSecondaryScreen, hs.geometry.new("[65, 10, 100, 100]"), nil, nil},
            {"Google Chrome", nil, attemptSecondaryScreen, hs.geometry.new("[65, 10, 100, 100]"), nil, nil},
            {"qutebrowser", nil, attemptSecondaryScreen, hs.geometry.new("[65, 10, 100, 100]"), nil, nil},
            {"Books", nil, attemptSecondaryScreen, hs.geometry.new("[65, 0, 100, 90]"), nil, nil},
            -- main
            {"IntelliJ IDEA", nil, nil, hs.geometry.new("[0, 0, 90, 100]")},
            {"PyCharm", nil, nil, hs.geometry.new("[0, 0, 90, 100]")},
            {"GoLand", nil, nil, hs.geometry.new("[0, 0, 90, 100]")}
        },
        actions = {
            function() hs.notify.show("Hammerspoon layouts", "Activated \"IDE Session - Dual monitor\"",  "Turn Hocus Focus to Coding") end
        }
    },
    terminal_dualmonior= {
        name = "Terminal coding - Dual monitor",
        subtitle = "Terminal, Dash, Safari/Chrome, iBooks/Preview",
        layout = {
            -- secondary left
            {"Preview", nil, attemptSecondaryScreen, hs.geometry.new("[0, 0, 35, 90]"), nil, nil},
            {"Safari", nil, attemptSecondaryScreen, hs.geometry.new("[0, 10, 35, 100]"), nil, nil},
            -- secondary center
            {"Bear", nil, attemptSecondaryScreen, hs.geometry.new("[35, 0, 65, 50]"), nil, nil},
            {"DayOne", nil, attemptSecondaryScreen, hs.geometry.new("[35, 0, 65, 50]"), nil, nil},
            {"MacVim", nil, attemptSecondaryScreen, hs.geometry.new("[35, 0, 65, 50]"), nil, nil},
            {"Dash", nil, attemptSecondaryScreen, hs.geometry.new("[35, 50, 65, 100]"), nil, nil},
            -- secondary right
            {"Brave Browser", nil, attemptSecondaryScreen, hs.geometry.new("[65, 10, 100, 100]"), nil, nil},
            {"Firefox", nil, attemptSecondaryScreen, hs.geometry.new("[65, 10, 100, 100]"), nil, nil},
            {"Google Chrome", nil, attemptSecondaryScreen, hs.geometry.new("[65, 10, 100, 100]"), nil, nil},
            {"qutebrowser", nil, attemptSecondaryScreen, hs.geometry.new("[65, 10, 100, 100]"), nil, nil},
            {"Books", nil, attemptSecondaryScreen, hs.geometry.new("[65, 0, 100, 90]"), nil, nil},

            {"Terminal", nil, nil, hs.geometry.unitrect(0, 0, 0.6, .9), nil, nil},
        },
        actions = {
            function() hs.application.launchOrFocus("Minuteur") end,
            function() hs.notify.show("Hammerspoon layout", "Activated \"Terminal coding - Dual monitor\"", "Turn on Hocus Focus Coding") end,
        }
    },
    vim_dualmonitor = {
        name = "Vim GUI - Dual monitor",
        subtitle = "MacVim/VimR, Terminal, Safari/Chrome, Dash, iBooks/Preview",
        layout = {
            -- left
            {"Safari", nil, attemptSecondaryScreen, hs.geometry.new("[0, 10, 35, 100]"), nil, nil},
            {"Preview", nil, attemptSecondaryScreen, hs.geometry.new("[0, 0, 35, 90]"), nil, nil},
            -- center
            {"Terminal", nil, attemptSecondaryScreen, hs.geometry.new("[35, 0, 65, 50]"), nil, nil},
            {"Dash", nil, attemptSecondaryScreen, hs.geometry.new("[35, 50, 65, 100]"), nil, nil},
            -- right
            {"qutebrowser", nil, attemptSecondaryScreen, hs.geometry.new("[65, 10, 100, 100]"), nil, nil},
            {"Google Chrome", nil, attemptSecondaryScreen, hs.geometry.new("[65, 10, 100, 100]"), nil, nil},
            {"Firefox", nil, attemptSecondaryScreen, hs.geometry.new("[65, 10, 100, 100]"), nil, nil},
            {"Books", nil, attemptSecondaryScreen, hs.geometry.new("[65, 0, 100, 90]"), nil, nil},


            {"MacVim", nil, nil, hs.geometry.new("[0, 0, 90, 100]")},
            {"VimR", nil, nil, hs.geometry.new("[0, 0, 90, 100]")},
        },
        actions = {
            function() hs.notify.show("Hammerspoon layouts", "Activated \"Vim GUI - Dual monitor\"",  "Turn Hocus Focus to Coding") end
        }
    },
    
}

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
                print("    3.1 unminimize", app.name())
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
            print("        parameter screen is of type:", type(l[3]))
            -- if type(l[3]) ~= "function" then
            if l[3] == nil then
                table.insert(updatedLayout, {l[1], l[2], l[3], l[4], l[5], l[6]})
                print("      use static screen for app ", app:title(), hs.inspect.inspect(updatedLayout[#updatedLayout]))
            else
                local scr = l[3]()
                table.insert(updatedLayout, {l[1], l[2], l[3], l[4], l[5], l[6]})
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

    function rectToTbl(r)
        return {["x"] = r.x, ["y"] = r.y, ["w"] = r.w, ["h"] = r.h}
    end

    local timestampTerminal = ""
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
                if appName == "Terminal" then
                    app:activate()
                    hs.eventtap.keyStroke({"ctrl"}, "s")
                    hs.eventtap.keyStroke({"ctrl"}, "s")
                    timestampTerminal = os.date("%Y-%m-%dT%H:%M:%S")
                end
                -- print("  d:5:end")
            end
            -- print("d:4:end", app:name())
        end
    end

    print(hs.json.encode(result))
    local timestamp = os.date("%Y-%m-%dT%H%M%S")
    local fileName = timestamp ..  ".json"
    io.output(os.getenv("HOME").."/.sessions/hammerspoon_layout/"..fileName)
    io.write(hs.json.encode(result))
    io.close()
    hs.alert.show("Layout saved:"..fileName)
    -- let's take a screenshot too
    hs.eventtap.keyStroke({"cmd", "shift"}, "3")
    local screenshotTimestamp = os.date("%Y-%m-%dT%H%M%S")

    local snapshotManifest = os.getenv("HOME").."/.sessions/"..timestamp..".txt"
    io.output(snapshotManifest)
    io.write("Layout file: ", "~/.sessions/hammerspoon_layout/"..fileName, "\n")
    io.write("Screenshot : ", "~/Dropbox/Photos/screenshots/ ", screenshotTimestamp.."-Screenshot_from_[...]", "\n")
    io.write("Vim (opt)  : ", "~/.sesisons/vim/"..timestamp.."[...]", "\n")
    if result["Terminal"] ~= nil then
        io.write("tmux       : ", "~/.sessions/tmux_resurrect/last", "\n", 
                 "             ", "~/.sessions/tmux_resurrect/", " tmux_resurect_"..timestampTerminal..".txt", "\n")
    end
    io.write("\n")
    if result["Safari"] ~= nil then
        io.write("# Safari tabs", "\n\n")
    end
    if result["Google Chrome"] ~= nil then
        io.write("# Chrome tabs", "\n\n")
    end
    io.close()
    hs.alert.show("Snapshot manifest:"..snapshotManifest)
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
            app = hs.application.get(appName)
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
                        app:activate()
                        hs.eventtap.keyStroke("cmd", "n")
                        -- hs.osascript.applescript('tell application "System Events" \n activate application "' ..  appName ..  '"\n tell process "' ..  appName ..  '" to keystroke "n" using command down \n end tell')
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
