-- http://lua-users.org/wiki/SimpleStack
-- http://giderosmobile.com/forum/discussion/6658/how-to-implement-a-simple-last-in-first-out-lifo-stack-in-lua/p1
local log = hs.logger.new('stack', 'debug')
UndoStack = {}

function UndoStack:new(size, debug)
    local t = {}

    t._stack = {}
    t._cache = {}
    t._maxs  = size or -1
    t._debug = debug or false

    function t:push(entry)
        if type(entry) ~= "table" then
            error("entry must be a table")
        end
        if entry["key"] == nil then
            error("entry must have a key")
        end
        local prevState = self._cache[entry["key"]]
        if prevState ~= nil then
            if self._maxs > 0 and #self._stack + 1 > self._maxs then
                table.remove(self._stack, 1)
            end
            table.insert(self._stack, prevState)
            if self._debug then
                print("_stack["..#self._stack.."]=", hs.inspect.inspect(prevState))
            end
        end
        self._cache[entry["key"]] = entry
        if self._debug then
            print("_cache["..entry["key"].."]=", hs.inspect(entry["val"]))
        end
    end

    function t:pop(byKey)
        if #self._stack == 0 then
            return nil
        end
        local result = nil
        if byKey == nil then
            result = self._stack[#self._stack]
            table.remove(self._stack)
        else
            for k = #self._stack, 1, -1 do
                if byKey == self._stack[k]["key"] then
                    result = self._stack[k]
                    table.remove(self._stack, k)
                    break
                end
            end
        end
        if self._debug then
            local k = byKey or ""
            print("pop("..k.."):", hs.inspect(result))
        end

        return result
    end

    function t:deleteCache(key)
        self._cache[key] = nil
    end

    function t:getn()
        return #self._stack 
    end

    return t
end

stack = {}

function stack:Create(size)
    local t = {}

    t._et = {}
    t._maxs = size or -1

    function t:push(...)
        if ... then
            local targs = {...}
            for _, v in ipairs(targs) do
                if self._maxs > 0 and #self._et + 1 > self._maxs then
                    table.remove(self._et, 1)
                end
                table.insert(self._et, v)
                log.i("stack["..#self._et.."]=",v["wnd"],"->", hs.inspect.inspect(v))
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

        for i, v in pairs(entries) do
            print("entries[", i, "]=")
            for k, u in pairs(v) do
                print("    ", k, "->", u)
            end
        end
        -- return unpack(entries)
        return entries
    end

    function t:getn()
        return #self._et
    end

    function t:list(fn)
        for i, v in pairs(self._et) do
            if fn ~= nil then
                print(fn(i, v))
            else
                print(i, v)
            end
        end
    end

    return t
end

-- stack = Stack:Create(5)
-- stack:push("a", "b", "c", "d", "e", "f", "g")
-- stack:list()
-- print("---")
-- stack:pop()
-- stack:list()
-- print("---")
-- stack:push("h")
-- stack:list()
-- print(stack._maxs)

return UndoStack
