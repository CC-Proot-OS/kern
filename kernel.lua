print("Proot Os")
TASKS = {}
kernel = {}

require("theme")

local log = require("logs")
local syslog = log.make("SYS")
syslog.log(log.levels.DEBUG,"Starting OS...")

local function centerWrite(text)
    local width, height = term.getSize() -- Get terminal size
    local x, y = term.getCursorPos() -- Get current cursor position
    local new_x = math.ceil((width / 2) - (#text / 2))
    term.setCursorPos(new_x, y)
    term.write(text)
end

function kernel.panic(ae)
    term.setBackgroundColor(2)
    term.setTextColor(1)
    term.clear()
    term.setCursorBlink(false)
    term.setCursorPos(1,2)
    local p, q = term.getCursorPos()
    p = 1
    centerWrite("ProotOS")
    q = 3
    local af, ag = term.getSize()
    ae = "panic: " .. (ae or "unknown")
    for ah in ae:gmatch "%S+" do
        if p + #ah >= af then
            p, q = 1, q + 1
            if q > ag then
                term.scroll(1)
                q = q - 1
            end
        end
        term.setCursorPos(p, q)
        if p == 1 then term.clearLine() end
        term.write(ah .. " ")
        p = p + #ah + 1
    end
    p, q = 1, q + 1
    if q > ag then
        term.scroll(1)
        q = q - 1
    end
    if debug then
        local ai = debug.traceback(nil, 2)
        for aj in ai:gmatch "[^\n]+" do
            term.setCursorPos(1, q)
            term.write(aj)
            q = q + 1
            if q > ag then
                term.scroll(1)
                q = q - 1
            end
        end
    end
    term.setCursorPos(1, q)
    term.setTextColor(2)
    term.write("panic: We are hanging here...")
    mainThread = nil
    while true do coroutine.yield() end
end






local taskmaster = require("taskmaster")()

function kernel.run(name,func)
    taskmaster:addTask(function(Task)
        TASKS[name] = Task
        xpcall(func,printError,Task)
        TASKS[name] = nil
    end)
end

kernel.run("kernel",function(Task)
    syslog.log(log.levels.DEBUG,"kernel thread started")
    read()
end)

syslog.log(log.levels.INFO,"Starting Threadding")
taskmaster:run()
read()
kernel.panic("CRITICAL system process died")