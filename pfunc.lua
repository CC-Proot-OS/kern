---@module pfunc
--@since 0.0.1


local pfunc = {}

---makes a ptbl
---@return table
function pfunc.make()
    ---@type ptbl
    local ptbl = {}
    
    --- private values - only for use in main
    ptbl.private = {}

    --- protected values - used for external utilitys 
    ptbl.protected = {}
    
    --- public values
    ptbl.public = {}

    ---returns only the protected and public functions
    --@return table
    function ptbl:prot()
        return {
            protected = self.protected,
            public = self.public
        }
    end
    return ptbl
end

return pfunc