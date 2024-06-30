local pfunc = {}

function pfunc.make()
    local ptbl = {
        private = {},
        protected = {},
        public = {},
    }
    function ptbl:prot()
        return {
            protected = self.protected,
            public = self.public
        }
    end
    return ptbl
end

return pfunc