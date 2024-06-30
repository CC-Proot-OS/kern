local expect = require "cc.expect".expect

-- Initialization code
local PrimeUI = {}
do
    local coros = {}
    local restoreCursor

    --- Adds a task to run in the main loop.
    ---@param func function The function to run, usually an `os.pullEvent` loop
    function PrimeUI.addTask(func)
        expect(1, func, "function")
        local t = {coro = coroutine.create(func)}
        coros[#coros+1] = t
        _, t.filter = coroutine.resume(t.coro)
    end

    --- Sends the provided arguments to the run loop, where they will be returned.
    ---@param ... any The parameters to send
    function PrimeUI.resolve(...)
        coroutine.yield(coros, ...)
    end

    --- Clears the screen and resets all components. Do not use any previously
    --- created components after calling this function.
    function PrimeUI.clear()
        -- Reset the screen.
        term.setCursorPos(1, 1)
        term.setCursorBlink(false)
        term.setBackgroundColor(colors.black)
        term.setTextColor(colors.white)
        term.clear()
        -- Reset the task list and cursor restore function.
        coros = {}
        restoreCursor = nil
    end

    --- Sets or clears the window that holds where the cursor should be.
    ---@param win window|nil The window to set as the active window
    function PrimeUI.setCursorWindow(win)
        expect(1, win, "table", "nil")
        restoreCursor = win and win.restoreCursor
    end

    --- Gets the absolute position of a coordinate relative to a window.
    ---@param win window The window to check
    ---@param x number The relative X position of the point
    ---@param y number The relative Y position of the point
    ---@return number x The absolute X position of the window
    ---@return number y The absolute Y position of the window
    function PrimeUI.getWindowPos(win, x, y)
        if win == term then return x, y end
        while win ~= term.native() and win ~= term.current() do
            if not win.getPosition then return x, y end
            local wx, wy = win.getPosition()
            x, y = x + wx - 1, y + wy - 1
            _, win = debug.getupvalue(select(2, debug.getupvalue(win.isColor, 1)), 1) -- gets the parent window through an upvalue
        end
        return x, y
    end

    --- Runs the main loop, returning information on an action.
    ---@return any ... The result of the coroutine that exited
    function PrimeUI.run()
        while true do
            -- Restore the cursor and wait for the next event.
            if restoreCursor then restoreCursor() end
            local ev = table.pack(os.pullEvent())
            -- Run all coroutines.
            for _, v in ipairs(coros) do
                if v.filter == nil or v.filter == ev[1] then
                    -- Resume the coroutine, passing the current event.
                    local res = table.pack(coroutine.resume(v.coro, table.unpack(ev, 1, ev.n)))
                    -- If the call failed, bail out. Coroutines should never exit.
                    if not res[1] then error(res[2], 2) end
                    -- If the coroutine resolved, return its values.
                    if res[2] == coros then return table.unpack(res, 3, res.n) end
                    -- Set the next event filter.
                    v.filter = res[2]
                end
            end
        end
    end
end

--- Draws a line of text at a position.
---@param win window The window to draw on
---@param x number The X position of the left side of the text
---@param y number The Y position of the text
---@param text string The text to draw
---@param fgColor color|nil The color of the text (defaults to white)
---@param bgColor color|nil The color of the background (defaults to black)
function PrimeUI.label(win, x, y, text, fgColor, bgColor)
    expect(1, win, "table")
    expect(2, x, "number")
    expect(3, y, "number")
    expect(4, text, "string")
    fgColor = expect(5, fgColor, "number", "nil") or colors.white
    bgColor = expect(6, bgColor, "number", "nil") or colors.black
    win.setCursorPos(x, y)
    win.setTextColor(fgColor)
    win.setBackgroundColor(bgColor)
    win.write(text)
end

--- Draws a BIMG-formatted image to the screen. This does not support transparency,
--- and does not handle animation on its own (but the index parameter may be
--- used by apps to implement animation).
---@param win window The window to draw on
---@param x number The X position of the top left corner of the image
---@param y number The Y position of the top left corner of the image
---@param data string|table The path to the image to load, or the image data itself
---@param index number|nil The index of the frame to draw (defaults to 1)
---@param setPalette boolean|nil Whether to set the palette if the image contains one (defaults to true)
function PrimeUI.drawImage(win, x, y, data, index, setPalette)
    expect(1, win, "table")
    expect(2, x, "number")
    expect(3, y, "number")
    expect(4, data, "string", "table")
    index = expect(5, index, "number", "nil") or 1
    expect(6, setPalette, "boolean", "nil")
    if setPalette == nil then setPalette = true end
    -- Load the image file if a string was passed. (This consists of reading the file and unserializing.)
    if type(data) == "string" then
        local file = assert(fs.open(data, "rb"))
        local filedata = file.readAll()
        file.close()
        data = assert(textutils.unserialize(filedata), "File is not a valid BIMG file")
    end
    -- Blit each line to the screen.
    for line = 1, #data[index] do
        win.setCursorPos(x, y + line - 1)
        win.blit(table.unpack(data[index][line]))
    end
    -- Set the palette if one exists and is desired.
    local palette = data[index].palette or data.palette
    if setPalette and palette then
        for i = 0, #palette do
            win.setPaletteColor(2^i, table.unpack(palette[i]))
        end
    end
end

return PrimeUI