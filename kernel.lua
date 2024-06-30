print("Proot Os")

args = {
    loglevel = 1,
    runSrv = true
}

for z, i in ipairs({ ... }) do
    local aE, aF = i:match("^([^=]+)=(.+)$")
    if aE and aF then
        if type(args[aE]) == "boolean" then
            args[aE] = aF:lower() == "true" or aF == "1"
        elseif type(args[aE]) == "number" then
            args[aE] = tonumber(aF)
        else
            args[aE] = aF
        end
    elseif aE == "silent" then
        args.loglevel = 5
    elseif aE == "quiet" then
        args.loglevel = 3
    end
end

local pfunc = require("pfunc")

local kernProt = pfunc.make()

TASKS = {}
kernel = kernProt.public

local function SplitFilename(strFilename)
    -- Returns the Path, Filename, and Extension as 3 values
    return string.match(strFilename, "(.-)([^\\]-([^\\%.]+))$")
end

do
    local h = fs.open("/rom/modules/main/cc/expect.lua", "r")
    local f, err = (_VERSION == "Lua 5.1" and loadstring or load)(h.readAll(), "/rom/modules/main/cc/expect.lua")
    h.close()

    if not f then error(err) end
    expect = f()
    field = expect.field
end

require("theme")
local logo = require("logo")
logo()
print()

local log = require("logs")
local syslog = log.make("SYS")
syslog.log(log.levels.DEBUG,"Starting OS...")

kernel.log = {
    syslog = syslog,
    levels = log.levels,
    log = syslog.logThrd
}

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

function panic(ae)
    term.setBackgroundColor(32768)
    term.setTextColor(16384)
    term.setCursorBlink(false)
    local p, q = term.getCursorPos()
    p = 1
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

kernProt.private.devs = {}
kernProt.private.drvs = {}

function kernProt.protected.regDrv(name,drv)
    kernProt.private.drvs[name] = drv
    syslog.logThrd("drvsys",log.levels.DEBUG,string.format("loaded driver [%8s]",name))
end

local ofs = fs
dofile("sys/boot/filesystem/init.lua")
syslog.log(log.levels.INFO,"FS Loaded")
fs = filesystem

local taskmaster = require("taskmaster")()

function kernel.run(name,func)
    taskmaster:addTask(function(Task)
        TASKS[name] = Task
        xpcall(func,printError,Task)
        TASKS[name] = nil
        syslog.log(log.levels.DEBUG,"thread",name,"closed")
    end)
    return Task
end
function kernProt.private.run(name,func)
    taskmaster:addTask(function(Task)
        TASKS[name] = Task
        xpcall(func,printError,Task)
        TASKS[name] = nil
        syslog.log(log.levels.CRITICAL,"thread",name,"closed")
    end)
    return Task
end
function kernel.runBG(name,func)
    taskmaster:addTask(function(Task)
        TASKS[name] = Task
        xpcall(func,printError,Task)
        TASKS[name] = nil
        syslog.log(log.levels.DEBUG,"thread",name,"closed")
    end):setEventBlacklist {"key", "key_up", "char", "paste", "mouse_click", "mouse_up", "mouse_scroll", "mouse_drag"}
    return Task
end

local srvsys = require("services")(kernProt:prot())

xpcall(function()
    local files = fs.list("srv")
    for i = 1, #files do
        --local pth,name,ext = string.match(files[i], "(.-)([^\\]-([^\\%.]+))$")
        local name = string.gsub(files[i],"(%.a)","")
        syslog.logThrd("ld_srv",log.levels.DEBUG,"loading service",name)
        srvsys.load(name)
    end

    srvsys.enable("warp")
end,panic)

syslog.log(log.levels.INFO,"Services started")

kernProt.private.run("kernel",function(Task)
    syslog.log(log.levels.DEBUG,"kernel thread started")
    print("Welcome to \27[96mProot\27[93mOS\27[0m")
    read()
end)

syslog.log(log.levels.INFO,"Starting Threadding")
taskmaster:run()
read()
kernel.panic("CRITICAL system process died")