local log = {}
log.levels = {
    DEBUG    = 1,
    VERBOSE  = 2,
    INFO     = 3,
    NOTICE   = 4,
    WARN     = 5,
    CRITICAL = 6,
}
log.names = {}

for key, value in pairs(log.levels) do
    log.names[value] = key
end

local cols = {
    colors.gray,
    colors.white,
    colors.blue,
    colors.yellow,
    colors.orange,
    colors.red
}
function log.make(id)
    local logger = {}
    function logger.log(lvl,...)
        local c = term.getTextColor()
        term.setTextColor(cols[lvl])
        print(string.format("[%3s][%8s]",id,log.names[lvl]),...)
        term.setTextColor(c)
    end
    return logger
end
return log
