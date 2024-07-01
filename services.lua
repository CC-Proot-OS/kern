return function(kernProt)
    local ar = require("ar")
    local toml = require("toml")

    local srvsys = {}
    local ldSRV = {}
    local services = {}

    ---@generic service
    local SRVREQ = {
        "load", "unload"
    }

    local function load(srv)
        local s = ar.load("srv/"..srv..".a")
        return s
    end
    local function lookup(s,name)
        for key, value in pairs(s) do
            if value.name == name then
                return value
            end
        end
    end
    local function lookupKey(s,name)
        for key, value in pairs(s) do
            if value.name == name then
                return key
            end
        end
    end

    local function onLoad(srv,s)
        local t = toml.decode(lookup(s,"cfg.toml").data)
        --local t = {enabled = true}
        if t.enabled then
            srvsys.run(srv,s)
        end
    end

    local function SRV(t) --- NOT Storm Research Vehicle
        for key, value in pairs(SRVREQ) do
            field(t,value,"function")
        end
        return t
    end

    function srvsys.run(srv,s)
        local d = lookup(s,"srv.lua")
        local task,e = loadstring(d.data,srv)
        assert(task,tostring(task))
        local S = SRV(task(kernProt))
        services[srv] = S
        S.load()
    end

    function srvsys.enable(srv)
        if ldSRV[srv] then
            if services[srv] then
            else
                local s = ldSRV[srv]
                local k =lookupKey(s,"cfg.toml")
                local t = toml.decode(s[k].data)
                t.enabled = true
                t.drvr = t.drvr
                s[k].data = toml.encode(t)
                ar.save(s,"srv/"..srv..".a")
                srvsys.run(srv,s)
            end

        end
    end

    function srvsys.load(srv)
        local s = load(srv)
        ldSRV[srv] = s
        --print(lookup(s,"srv.lua").data)
        onLoad(srv,s)
        --TODO
    end

    function srvsys.unload(srv)
        --TODO
    end

    return srvsys
end