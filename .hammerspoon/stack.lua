-- http://lua-users.org/wiki/SimpleStack
-- http://giderosmobile.com/forum/discussion/6658/how-to-implement-a-simple-last-in-first-out-lifo-stack-in-lua/p1
Stack = {}

function Stack:Create(size)
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
                print("stack["..#self._et.."]=",v["wnd"],"->", hs.inspect.inspect(v))
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

return Stack
