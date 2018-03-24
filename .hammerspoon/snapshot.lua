local ctrl_alt_cmd = {"⌥", "⌃", "⌘"}
hs.hotkey.bind(ctrl_alt_cmd, "k", function()
    local chooser = hs.chooser.new(function(result)
        if result == nil then
            return
        end
        print("Result:", result)
        if result["uuid"] == "save" then
            saveLayoutSnapshot()
        else
            loadLayoutSnapshot()
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
        }
    }
    chooser:choices(choices)
    chooser:show()
end)

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

function loadLayoutSnapshot()
    local snapshotFile = os.getenv("HOME").."/.sessions/hammerspoon_layout/last"
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
