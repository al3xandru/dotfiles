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
function applyLayout(result)
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

-- bind shortcut
local ctrl_alt_cmd = {"⌥", "⌃", "⌘"}
hs.hotkey.bind(ctrl_alt_cmd, "l", function()
    local chooser = hs.chooser.new(function(result)
        if result == nil then
            return
        end
        applyLayout(result)
    end)

    -- index layouts for presenting the chooser
    local choices = {}
    for k, v in pairs(LAYOUTS) do
        table.insert(choices, {["text"] = v["name"], ["subText"] = v["subtitle"], ["uuid"] = k}) 
    end
    chooser:choices(choices)
    chooser:show()
end)

