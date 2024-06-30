return function()
    local image = {
        {
            "  \159\131 \159\148        ",
            "00ff0f400000000",
            "ff44f4fffffffff"
        },
        {
            "  \129 \148\129\149        ",
            "00f044100000000",
            "ff41f14ffffffff"
        },
        {
            "   \138\149\157\148        ",
            "000144400000000",
            "ff41444ffffffff"
        },
        {
            "  \144\139\144\159\143\129       ",
            "004444770000000",
            "ff4447fffffffff"
        },
        {
            " \130\134\133\136\136\138\133\130  \159\144  ",
            "04444f11100f700",
            "ff4447fffff7fff"
        },
        {
            "  \130\131\139\143\133    \139\143  ",
            "004481700008400",
            "fffffffffffffff"
        }
    }
    
    term.clear()
    for y, r in ipairs(image) do
        term.setCursorPos(1, y)
        term.blit(table.unpack(r))
    end
end