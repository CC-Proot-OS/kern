local loading = {}
local oldRequire, preload, loaded = require, {}, { startup = loading }

local function require(name)
	local result = loaded[name]

	if result ~= nil then
		if result == loading then
			error("loop or previous error loading module '" .. name .. "'", 2)
		end

		return result
	end

	loaded[name] = loading
	local contents = preload[name]
	if contents then
		result = contents(name)
	elseif oldRequire then
		result = oldRequire(name)
	else
		error("cannot load '" .. name .. "'", 2)
	end

	if result == nil then result = true end
	loaded[name] = result
	return result
end
preload["vector"] = function(...)
-- SPDX-FileCopyrightText: 2017 Daniel Ratcliffe
--
-- SPDX-License-Identifier: LicenseRef-CCPL

--- The vector API provides methods to create and manipulate vectors.
--
-- An introduction to vectors can be found on [Wikipedia][wiki].
--
-- [wiki]: http://en.wikipedia.org/wiki/Euclidean_vector
--
-- @module vector
-- @since 1.31

--- A 3-dimensional vector, with `x`, `y`, and `z` values.
--
-- This is suitable for representing both position and directional vectors.
--
-- @type Vector
local vector = {
    --- Adds two vectors together.
    --
    -- @tparam Vector self The first vector to add.
    -- @tparam Vector o The second vector to add.
    -- @treturn Vector The resulting vector
    -- @usage v1:add(v2)
    -- @usage v1 + v2
    add = function(self, o)
        return vector.new(
            self.x + o.x,
            self.y + o.y,
            self.z + o.z
        )
    end,

    --- Subtracts one vector from another.
    --
    -- @tparam Vector self The vector to subtract from.
    -- @tparam Vector o The vector to subtract.
    -- @treturn Vector The resulting vector
    -- @usage v1:sub(v2)
    -- @usage v1 - v2
    sub = function(self, o)
        return vector.new(
            self.x - o.x,
            self.y - o.y,
            self.z - o.z
        )
    end,

    --- Multiplies a vector by a scalar value.
    --
    -- @tparam Vector self The vector to multiply.
    -- @tparam number m The scalar value to multiply with.
    -- @treturn Vector A vector with value `(x * m, y * m, z * m)`.
    -- @usage v:mul(3)
    -- @usage v * 3
    mul = function(self, m)
        return vector.new(
            self.x * m,
            self.y * m,
            self.z * m
        )
    end,

    --- Divides a vector by a scalar value.
    --
    -- @tparam Vector self The vector to divide.
    -- @tparam number m The scalar value to divide by.
    -- @treturn Vector A vector with value `(x / m, y / m, z / m)`.
    -- @usage v:div(3)
    -- @usage v / 3
    div = function(self, m)
        return vector.new(
            self.x / m,
            self.y / m,
            self.z / m
        )
    end,

    --- Negate a vector
    --
    -- @tparam Vector self The vector to negate.
    -- @treturn Vector The negated vector.
    -- @usage -v
    unm = function(self)
        return vector.new(
            -self.x,
            -self.y,
            -self.z
        )
    end,

    --- Compute the dot product of two vectors
    --
    -- @tparam Vector self The first vector to compute the dot product of.
    -- @tparam Vector o The second vector to compute the dot product of.
    -- @treturn Vector The dot product of `self` and `o`.
    -- @usage v1:dot(v2)
    dot = function(self, o)
        return self.x * o.x + self.y * o.y + self.z * o.z
    end,

    --- Compute the cross product of two vectors
    --
    -- @tparam Vector self The first vector to compute the cross product of.
    -- @tparam Vector o The second vector to compute the cross product of.
    -- @treturn Vector The cross product of `self` and `o`.
    -- @usage v1:cross(v2)
    cross = function(self, o)
        return vector.new(
            self.y * o.z - self.z * o.y,
            self.z * o.x - self.x * o.z,
            self.x * o.y - self.y * o.x
        )
    end,

    --- Get the length (also referred to as magnitude) of this vector.
    -- @tparam Vector self This vector.
    -- @treturn number The length of this vector.
    length = function(self)
        return math.sqrt(self.x * self.x + self.y * self.y + self.z * self.z)
    end,

    --- Divide this vector by its length, producing with the same direction, but
    -- of length 1.
    --
    -- @tparam Vector self The vector to normalise
    -- @treturn Vector The normalised vector
    -- @usage v:normalize()
    normalize = function(self)
        return self:mul(1 / self:length())
    end,

    --- Construct a vector with each dimension rounded to the nearest value.
    --
    -- @tparam Vector self The vector to round
    -- @tparam[opt] number tolerance The tolerance that we should round to,
    -- defaulting to 1. For instance, a tolerance of 0.5 will round to the
    -- nearest 0.5.
    -- @treturn Vector The rounded vector.
    round = function(self, tolerance)
        tolerance = tolerance or 1.0
        return vector.new(
            math.floor((self.x + tolerance * 0.5) / tolerance) * tolerance,
            math.floor((self.y + tolerance * 0.5) / tolerance) * tolerance,
            math.floor((self.z + tolerance * 0.5) / tolerance) * tolerance
        )
    end,

    --- Convert this vector into a string, for pretty printing.
    --
    -- @tparam Vector self This vector.
    -- @treturn string This vector's string representation.
    -- @usage v:tostring()
    -- @usage tostring(v)
    tostring = function(self)
        return self.x .. "," .. self.y .. "," .. self.z
    end,

    --- Check for equality between two vectors.
    --
    -- @tparam Vector self The first vector to compare.
    -- @tparam Vector other The second vector to compare to.
    -- @treturn boolean Whether or not the vectors are equal.
    equals = function(self, other)
        return self.x == other.x and self.y == other.y and self.z == other.z
    end,
}

local vmetatable = {
    __index = vector,
    __add = vector.add,
    __sub = vector.sub,
    __mul = vector.mul,
    __div = vector.div,
    __unm = vector.unm,
    __tostring = vector.tostring,
    __eq = vector.equals,
}

--- Construct a new @{Vector} with the given coordinates.
--
-- @tparam number x The X coordinate or direction of the vector.
-- @tparam number y The Y coordinate or direction of the vector.
-- @tparam number z The Z coordinate or direction of the vector.
-- @treturn Vector The constructed vector.
local function new(x, y, z)
    return setmetatable({
        x = tonumber(x) or 0,
        y = tonumber(y) or 0,
        z = tonumber(z) or 0,
    }, vmetatable)
end
return {new=new}
end
preload["userspace"] = function(...)
local userspace = {}
userspace.env = {}
for key, value in pairs(preload) do
    if key ~= "userspace" then
        userspace.env[key]=require(key)
    end
end
function userspace.update(env)
    setmetatable(userspace.env)
    setmetatable(userspace.env,{__index=env})
end

return userspace
end
preload["settings"] = function(...)
-- SPDX-FileCopyrightText: 2017 Daniel Ratcliffe
--
-- SPDX-License-Identifier: LicenseRef-CCPL

--[[- Read and write configuration options for CraftOS and your programs.

When a computer starts, it reads the current value of settings from the
`/.settings` file. These values then may be @{settings.get|read} or
@{settings.set|modified}.

:::caution
Calling @{settings.set} does _not_ update the settings file by default. You
_must_ call @{settings.save} to persist values.
:::

@module settings
@since 1.78
@usage Define an basic setting `123` and read its value.

    settings.define("my.setting", {
        description = "An example setting",
        default = 123,
        type = number,
    })
    print("my.setting = " .. settings.get("my.setting")) -- 123

You can then use the `set` program to change its value (e.g. `set my.setting 456`),
and then re-run the `example` program to check it has changed.

]]

local expect = dofile("rom/modules/main/cc/expect.lua")
local type, expect, field = type, expect.expect, expect.field
local settings = {}
local details, values = {}, {}

local function reserialize(value)
    if type(value) ~= "table" then return value end
    return textutils.unserialize(textutils.serialize(value))
end

local function copy(value)
    if type(value) ~= "table" then return value end
    local result = {}
    for k, v in pairs(value) do result[k] = copy(v) end
    return result
end

local valid_types = { "number", "string", "boolean", "table" }
for _, v in ipairs(valid_types) do valid_types[v] = true end

--- Define a new setting, optional specifying various properties about it.
--
-- While settings do not have to be added before being used, doing so allows
-- you to provide defaults and additional metadata.
--
-- @tparam string name The name of this option
-- @tparam[opt] { description? = string, default? = any, type? = string } options
-- Options for this setting. This table accepts the following fields:
--
--  - `description`: A description which may be printed when running the `set` program.
--  - `default`: A default value, which is returned by @{settings.get} if the
--    setting has not been changed.
--  - `type`: Require values to be of this type. @{set|Setting} the value to another type
--    will error.
-- @since 1.87.0
function settings.define(name, options)
    expect(1, name, "string")
    expect(2, options, "table", "nil")

    if options then
        options = {
            description = field(options, "description", "string", "nil"),
            default = reserialize(field(options, "default", "number", "string", "boolean", "table", "nil")),
            type = field(options, "type", "string", "nil"),
        }

        if options.type and not valid_types[options.type] then
            error(("Unknown type %q. Expected one of %s."):format(options.type, table.concat(valid_types, ", ")), 2)
        end
    else
        options = {}
    end

    details[name] = options
end

--- Remove a @{define|definition} of a setting.
--
-- If a setting has been changed, this does not remove its value. Use @{settings.unset}
-- for that.
--
-- @tparam string name The name of this option
-- @since 1.87.0
function settings.undefine(name)
    expect(1, name, "string")
    details[name] = nil
end

local function set_value(name, new)
    local old = values[name]
    if old == nil then
        local opt = details[name]
        old = opt and opt.default
    end

    values[name] = new
    if old ~= new then
        -- This should be safe, as os.queueEvent copies values anyway.
        os.queueEvent("setting_changed", name, new, old)
    end
end

--[[- Set the value of a setting.

:::caution
Calling @{settings.set} does _not_ update the settings file by default. You
_must_ call @{settings.save} to persist values.
:::

@tparam string name The name of the setting to set
@param value The setting's value. This cannot be `nil`, and must be
serialisable by @{textutils.serialize}.
@throws If this value cannot be serialised
@see settings.unset
]]
function settings.set(name, value)
    expect(1, name, "string")
    expect(2, value, "number", "string", "boolean", "table")

    local opt = details[name]
    if opt and opt.type then expect(2, value, opt.type) end

    set_value(name, reserialize(value))
end

--- Get the value of a setting.
--
-- @tparam string name The name of the setting to get.
-- @param[opt] default The value to use should there be pre-existing value for
-- this setting. If not given, it will use the setting's default value if given,
-- or `nil` otherwise.
-- @return The setting's, or the default if the setting has not been changed.
-- @changed 1.87.0 Now respects default value if pre-defined and `default` is unset.
function settings.get(name, default)
    expect(1, name, "string")
    local result = values[name]
    if result ~= nil then
        return copy(result)
    elseif default ~= nil then
        return default
    else
        local opt = details[name]
        return opt and copy(opt.default)
    end
end

--- Get details about a specific setting.
--
-- @tparam string name The name of the setting to get.
-- @treturn { description? = string, default? = any, type? = string, value? = any }
-- Information about this setting. This includes all information from @{settings.define},
-- as well as this setting's value.
-- @since 1.87.0
function settings.getDetails(name)
    expect(1, name, "string")
    local deets = copy(details[name]) or {}
    deets.value = values[name]
    deets.changed = deets.value ~= nil
    if deets.value == nil then deets.value = deets.default end
    return deets
end

--- Remove the value of a setting, setting it to the default.
--
-- @{settings.get} will return the default value until the setting's value is
-- @{settings.set|set}, or the computer is rebooted.
--
-- @tparam string name The name of the setting to unset.
-- @see settings.set
-- @see settings.clear
function settings.unset(name)
    expect(1, name, "string")
    set_value(name, nil)
end

--- Resets the value of all settings. Equivalent to calling @{settings.unset}
--- on every setting.
--
-- @see settings.unset
function settings.clear()
    for name in pairs(values) do
        set_value(name, nil)
    end
end

--- Get the names of all currently defined settings.
--
-- @treturn { string } An alphabetically sorted list of all currently-defined
-- settings.
function settings.getNames()
    local result, n = {}, 1
    for k in pairs(details) do
        result[n], n = k, n + 1
    end
    for k in pairs(values) do
        if not details[k] then result[n], n = k, n + 1 end
    end
    table.sort(result)
    return result
end

--- Load settings from the given file.
--
-- Existing settings will be merged with any pre-existing ones. Conflicting
-- entries will be overwritten, but any others will be preserved.
--
-- @tparam[opt] string sPath The file to load from, defaulting to `.settings`.
-- @treturn boolean Whether settings were successfully read from this
-- file. Reasons for failure may include the file not existing or being
-- corrupted.
--
-- @see settings.save
-- @changed 1.87.0 `sPath` is now optional.
function settings.load(sPath)
    expect(1, sPath, "string", "nil")
    local file = fs.open(sPath or ".settings", "r")
    if not file then
        return false
    end

    local sText = file.readAll()
    file.close()

    local tFile = textutils.unserialize(sText)
    if type(tFile) ~= "table" then
        return false
    end

    for k, v in pairs(tFile) do
        local ty_v = type(v)
        if type(k) == "string" and (ty_v == "string" or ty_v == "number" or ty_v == "boolean" or ty_v == "table") then
            local opt = details[k]
            if not opt or not opt.type or ty_v == opt.type then
                -- This may fail if the table is recursive (or otherwise cannot be serialized).
                local ok, v = pcall(reserialize, v)
                if ok then set_value(k, v) end
            end
        end
    end

    return true
end

--- Save settings to the given file.
--
-- This will entirely overwrite the pre-existing file. Settings defined in the
-- file, but not currently loaded will be removed.
--
-- @tparam[opt] string sPath The path to save settings to, defaulting to `.settings`.
-- @treturn boolean If the settings were successfully saved.
--
-- @see settings.load
-- @changed 1.87.0 `sPath` is now optional.
function settings.save(sPath)
    expect(1, sPath, "string", "nil")
    local file = fs.open(sPath or ".settings", "w")
    if not file then
        return false
    end

    file.write(textutils.serialize(values))
    file.close()

    return true
end

return settings
end
preload["paintutils"] = function(...)
-- SPDX-FileCopyrightText: 2017 Daniel Ratcliffe
--
-- SPDX-License-Identifier: LicenseRef-CCPL

--- An API for advanced systems which can draw pixels and lines, load and draw
-- image files. You can use the `colors` API for easier color manipulation. In
-- CraftOS-PC, this API can also be used in graphics mode.
--
-- @module paintutils
-- @since 1.45

local expect = dofile("rom/modules/main/cc/expect.lua").expect
local paintutils = {}
local function drawPixelInternal(xPos, yPos)
    if term.getGraphicsMode and term.getGraphicsMode() then
        term.setPixel(xPos - 1, yPos - 1, term.getBackgroundColor())
    else
        term.setCursorPos(xPos, yPos)
        term.write(" ")
    end
end

local tColourLookup = {}
for n = 1, 16 do
    tColourLookup[string.byte("0123456789abcdef", n, n)] = 2 ^ (n - 1)
end

local function parseLine(tImageArg, sLine)
    local tLine = {}
    for x = 1, sLine:len() do
        tLine[x] = tColourLookup[string.byte(sLine, x, x)] or 0
    end
    table.insert(tImageArg, tLine)
end

-- Sorts pairs of startX/startY/endX/endY such that the start is always the min
local function sortCoords(startX, startY, endX, endY)
    local minX, maxX, minY, maxY

    if startX <= endX then
        minX, maxX = startX, endX
    else
        minX, maxX = endX, startX
    end

    if startY <= endY then
        minY, maxY = startY, endY
    else
        minY, maxY = endY, startY
    end

    return minX, maxX, minY, maxY
end

--- Parses an image from a multi-line string
--
-- @tparam string image The string containing the raw-image data.
-- @treturn table The parsed image data, suitable for use with
-- @{paintutils.drawImage}.
-- @since 1.80pr1
function paintutils.parseImage(image)
    expect(1, image, "string")
    local tImage = {}
    for sLine in (image .. "\n"):gmatch("(.-)\n") do
        parseLine(tImage, sLine)
    end
    return tImage
end

--- Loads an image from a file.
--
-- You can create a file suitable for being loaded using the `paint` program.
--
-- @tparam string path The file to load.
--
-- @treturn table|nil The parsed image data, suitable for use with
-- @{paintutils.drawImage}, or `nil` if the file does not exist.
-- @usage Load an image and draw it.
--
--     local image = paintutils.loadImage("test-image.nfp")
--     paintutils.drawImage(image, term.getCursorPos())
function paintutils.loadImage(path)
    expect(1, path, "string")

    if fs.exists(path) then
        local file = io.open(path, "r")
        local sContent = file:read("*a")
        file:close()
        return parseImage(sContent)
    end
    return nil
end

--- Draws a single pixel to the current term at the specified position.
--
-- Be warned, this may change the position of the cursor and the current
-- background colour. You should not expect either to be preserved.
--
-- @tparam number xPos The x position to draw at, where 1 is the far left.
-- @tparam number yPos The y position to draw at, where 1 is the very top.
-- @tparam[opt] number colour The @{colors|color} of this pixel. This will be
-- the current background colour if not specified.
function paintutils.drawPixel(xPos, yPos, colour)
    expect(1, xPos, "number")
    expect(2, yPos, "number")
    expect(3, colour, "number", "nil")

    if colour then
        term.setBackgroundColor(colour)
    end
    return drawPixelInternal(xPos, yPos)
end

--- Draws a straight line from the start to end position.
--
-- Be warned, this may change the position of the cursor and the current
-- background colour. You should not expect either to be preserved.
--
-- @tparam number startX The starting x position of the line.
-- @tparam number startY The starting y position of the line.
-- @tparam number endX The end x position of the line.
-- @tparam number endY The end y position of the line.
-- @tparam[opt] number colour The @{colors|color} of this pixel. This will be
-- the current background colour if not specified.
-- @usage paintutils.drawLine(2, 3, 30, 7, colors.red)
function paintutils.drawLine(startX, startY, endX, endY, colour)
    expect(1, startX, "number")
    expect(2, startY, "number")
    expect(3, endX, "number")
    expect(4, endY, "number")
    expect(5, colour, "number", "nil")

    startX = math.floor(startX)
    startY = math.floor(startY)
    endX = math.floor(endX)
    endY = math.floor(endY)

    if colour then
        term.setBackgroundColor(colour)
    end
    if startX == endX and startY == endY then
        drawPixelInternal(startX, startY)
        return
    end

    local minX = math.min(startX, endX)
    local maxX, minY, maxY
    if minX == startX then
        minY = startY
        maxX = endX
        maxY = endY
    else
        minY = endY
        maxX = startX
        maxY = startY
    end

    -- TODO: clip to screen rectangle?

    local xDiff = maxX - minX
    local yDiff = maxY - minY

    if xDiff > math.abs(yDiff) then
        local y = minY
        local dy = yDiff / xDiff
        for x = minX, maxX do
            drawPixelInternal(x, math.floor(y + 0.5))
            y = y + dy
        end
    else
        local x = minX
        local dx = xDiff / yDiff
        if maxY >= minY then
            for y = minY, maxY do
                drawPixelInternal(math.floor(x + 0.5), y)
                x = x + dx
            end
        else
            for y = minY, maxY, -1 do
                drawPixelInternal(math.floor(x + 0.5), y)
                x = x - dx
            end
        end
    end
end

--- Draws the outline of a box on the current term from the specified start
-- position to the specified end position.
--
-- Be warned, this may change the position of the cursor and the current
-- background colour. You should not expect either to be preserved.
--
-- @tparam number startX The starting x position of the line.
-- @tparam number startY The starting y position of the line.
-- @tparam number endX The end x position of the line.
-- @tparam number endY The end y position of the line.
-- @tparam[opt] number colour The @{colors|color} of this pixel. This will be
-- the current background colour if not specified.
-- @usage paintutils.drawBox(2, 3, 30, 7, colors.red)
function paintutils.drawBox(startX, startY, endX, endY, nColour)
    expect(1, startX, "number")
    expect(2, startY, "number")
    expect(3, endX, "number")
    expect(4, endY, "number")
    expect(5, nColour, "number", "nil")

    startX = math.floor(startX)
    startY = math.floor(startY)
    endX = math.floor(endX)
    endY = math.floor(endY)

    if nColour then
        term.setBackgroundColor(nColour) -- Maintain legacy behaviour
    else
        nColour = term.getBackgroundColour()
    end

    if term.getGraphicsMode and term.getGraphicsMode() then
        local c, w, h = nColour or term.getBackgroundColor(), endX - startX, endY - startY
        term.drawPixels(startX, startY, c, w, 1)
        term.drawPixels(startX, startY, c, 1, h)
        term.drawPixels(endX, startY+1, c, 1, h)
        term.drawPixels(startX+1, endY, c, w, 1)
    else
        local colourHex = colours.toBlit(nColour)

        if startX == endX and startY == endY then
            drawPixelInternal(startX, startY)
            return
        end

        local minX, maxX, minY, maxY = sortCoords(startX, startY, endX, endY)
        local width = maxX - minX + 1

        for y = minY, maxY do
            if y == minY or y == maxY then
                term.setCursorPos(minX, y)
                term.blit((" "):rep(width), colourHex:rep(width), colourHex:rep(width))
            else
                term.setCursorPos(minX, y)
                term.blit(" ", colourHex, colourHex)
                term.setCursorPos(maxX, y)
                term.blit(" ", colourHex, colourHex)
            end
        end
    end
end

--- Draws a filled box on the current term from the specified start position to
-- the specified end position.
--
-- Be warned, this may change the position of the cursor and the current
-- background colour. You should not expect either to be preserved.
--
-- @tparam number startX The starting x position of the line.
-- @tparam number startY The starting y position of the line.
-- @tparam number endX The end x position of the line.
-- @tparam number endY The end y position of the line.
-- @tparam[opt] number colour The @{colors|color} of this pixel. This will be
-- the current background colour if not specified.
-- @usage paintutils.drawFilledBox(2, 3, 30, 7, colors.red)
function paintutils.drawFilledBox(startX, startY, endX, endY, nColour)
    expect(1, startX, "number")
    expect(2, startY, "number")
    expect(3, endX, "number")
    expect(4, endY, "number")
    expect(5, nColour, "number", "nil")

    startX = math.floor(startX)
    startY = math.floor(startY)
    endX = math.floor(endX)
    endY = math.floor(endY)

    if nColour then
        term.setBackgroundColor(nColour) -- Maintain legacy behaviour
    else
        nColour = term.getBackgroundColour()
    end

    if term.getGraphicsMode and term.getGraphicsMode() then
        local c = nColour or term.getBackgroundColor()
        term.drawPixels(startX, startY, c, endX - startX, endY - startY)
    else
        local colourHex = colours.toBlit(nColour)

        if startX == endX and startY == endY then
            drawPixelInternal(startX, startY)
            return
        end

        local minX, maxX, minY, maxY = sortCoords(startX, startY, endX, endY)
        local width = maxX - minX + 1

        for y = minY, maxY do
            term.setCursorPos(minX, y)
            term.blit((" "):rep(width), colourHex:rep(width), colourHex:rep(width))
        end
    end
end

--- Draw an image loaded by @{paintutils.parseImage} or @{paintutils.loadImage}.
--
-- @tparam table image The parsed image data.
-- @tparam number xPos The x position to start drawing at.
-- @tparam number yPos The y position to start drawing at.
function paintutils.drawImage(image, xPos, yPos)
    expect(1, image, "table")
    expect(2, xPos, "number")
    expect(3, yPos, "number")
    for y = 1, #image do
        local tLine = image[y]
        for x = 1, #tLine do
            if tLine[x] > 0 then
                term.setBackgroundColor(tLine[x])
                drawPixelInternal(x + xPos - 1, y + yPos - 1)
            end
        end
    end
end

return paintutils
end
preload["keys"] = function(...)
-- SPDX-FileCopyrightText: 2017 Daniel Ratcliffe
--
-- SPDX-License-Identifier: LicenseRef-CCPL


-- Minecraft key code bindings
-- See http://www.minecraftwiki.net/wiki/Key_codes for more info

local tKeys = {
	nil,	 	"one", 		"two", 		"three", 	"four",			-- 1
	"five", 	"six", 		"seven", 	"eight", 	"nine",			-- 6
	"zero", 	"minus", 	"equals", 	"backspace","tab",			-- 11
	"q", 		"w", 		"e", 		"r",		"t",			-- 16
	"y",		"u",		"i",		"o",		"p",			-- 21
	"leftBracket","rightBracket","enter","leftCtrl","a",			-- 26
	"s",		"d",		"f",		"g",		"h",			-- 31
	"j",		"k",		"l",		"semiColon","apostrophe",	-- 36
	"grave",	"leftShift","backslash","z",		"x",			-- 41
	"c",		"v",		"b",		"n",		"m",			-- 46
	"comma",	"period",	"slash",	"rightShift","multiply",	-- 51
	"leftAlt",	"space",	"capsLock",	"f1",		"f2",			-- 56
	"f3",		"f4",		"f5",		"f6",		"f7",			-- 61
	"f8",		"f9",		"f10",		"numLock",	"scrollLock",	-- 66
	"numPad7",	"numPad8",	"numPad9",	"numPadSubtract","numPad4",	-- 71
	"numPad5",	"numPad6",	"numPadAdd","numPad1",	"numPad2",		-- 76
	"numPad3",	"numPad0",	"numPadDecimal",nil,	nil,			-- 81
	nil,	 	"f11",		"f12",		nil,		nil,			-- 86
	nil,		nil,		nil,		nil,		nil,			-- 91
	nil,		nil,		nil,		nil,		"f13",			-- 96
	"f14",		"f15",		nil,		nil,		nil,			-- 101
	nil,		nil,		nil,		nil,		nil,			-- 106
	nil,		"kana",		nil,		nil,		nil,			-- 111
	nil,		nil,		nil,		nil,		nil,			-- 116
	"convert",	nil,		"noconvert",nil,		"yen",			-- 121
	nil,		nil,		nil,		nil,		nil,			-- 126
	nil,		nil,		nil,		nil,		nil,			-- 131
	nil,		nil,		nil,		nil,		nil,			-- 136
	"numPadEquals",nil,		nil,		"circumflex","at",			-- 141
	"colon",	"underscore","kanji",	"stop",		"ax",			-- 146
	nil,		nil,		nil,		nil,		nil,			-- 151
	"numPadEnter","rightCtrl",nil,      nil,		nil,			-- 156
	nil,		nil,		nil,		nil,		nil,			-- 161
	nil,		nil,		nil,		nil,		nil,			-- 166
	nil,		nil,		nil,		nil,		nil,			-- 171
	nil,		nil,		nil,		"numPadComma",nil,			-- 176
	"numPadDivide",nil,		nil,		"rightAlt",	nil,			-- 181
	nil,		nil,		nil,		nil,		nil,			-- 186
	nil,		nil,		nil,		nil,		nil,			-- 191
	nil,		"pause",	nil,		"home",		"up",			-- 196
	"pageUp",	nil,		"left",		nil,		"right",		-- 201
	nil,		"end",		"down",		"pageDown",	"insert",		-- 206
	"delete"														-- 211
}

local keys = {}
for nKey, sKey in pairs( tKeys ) do
	keys[sKey] = nKey
end
keys["return"] = keys.enter
-- (Un-)fix some misspellings
keys.scollLock = keys.scrollLock
keys.cimcumflex = keys.circumflex

function keys.getName( _nKey )
    if type( _nKey ) ~= "number" then
        error( "bad argument #1 (number expected, got " .. type( _nKey ) .. ")", 2 ) 
    end
	return tKeys[ _nKey ]
end
return keys
end
preload["http"] = function(...)
-- SPDX-FileCopyrightText: 2017 Daniel Ratcliffe
--
-- SPDX-License-Identifier: LicenseRef-CCPL

--[[- Make HTTP requests, sending and receiving data to a remote web server.
@module http
@since 1.1
@see local_ips To allow accessing servers running on your local network.
]]

local expect = dofile("rom/modules/main/cc/expect.lua").expect
local sys_http = {}
local native = http
local nativeHTTPRequest = native.request

local methods = {
    GET = true, POST = true, HEAD = true,
    OPTIONS = true, PUT = true, DELETE = true,
    PATCH = true, TRACE = true,
}

local function checkKey(options, key, ty, opt)
    local value = options[key]
    local valueTy = type(value)

    if (value ~= nil or not opt) and valueTy ~= ty then
        error(("bad field '%s' (%s expected, got %s"):format(key, ty, valueTy), 4)
    end
end

local function checkOptions(options, body)
    checkKey(options, "url", "string")
    if body == false then
        checkKey(options, "body", "nil")
    else
        checkKey(options, "body", "string", not body)
    end
    checkKey(options, "headers", "table", true)
    checkKey(options, "method", "string", true)
    checkKey(options, "redirect", "boolean", true)

    if options.method and not methods[options.method] then
        error("Unsupported HTTP method", 3)
    end
end

local function wrapRequest(_url, ...)
    local ok, err = nativeHTTPRequest(...)
    if ok then
        while true do
            local event, param1, param2, param3 = os.pullEvent()
            if event == "http_success" and param1 == _url then
                return param2
            elseif event == "http_failure" and param1 == _url then
                return nil, param2, param3
            end
        end
    end
    return nil, err
end

--[[- Make a HTTP GET request to the given url.

@tparam string url   The url to request
@tparam[opt] { [string] = string } headers Additional headers to send as part
of this request.
@tparam[opt] boolean binary Whether to make a binary HTTP request. If true,
the body will not be UTF-8 encoded, and the received response will not be
decoded.

@tparam[2] {
  url = string, headers? = { [string] = string },
  binary? = boolean, method? = string, redirect? = boolean,
} request Options for the request. See @{http.request} for details on how
these options behave.
@treturn Response The resulting http response, which can be read from.
@treturn[2] nil When the http request failed, such as in the event of a 404
error or connection timeout.
@treturn string A message detailing why the request failed.
@treturn Response|nil The failing http response, if available.

@changed 1.63 Added argument for headers.
@changed 1.80pr1 Response handles are now returned on error if available.
@changed 1.80pr1 Added argument for binary handles.
@changed 1.80pr1.6 Added support for table argument.
@changed 1.86.0 Added PATCH and TRACE methods.

@usage Make a request to [example.tweaked.cc](https://example.tweaked.cc),
and print the returned page.

```lua
local request = http.get("https://example.tweaked.cc")
print(request.readAll())
-- => HTTP is working!
request.close()
```
]]
function sys_http.get(_url, _headers, _binary)
    if type(_url) == "table" then
        checkOptions(_url, false)
        return wrapRequest(_url.url, _url)
    end

    expect(1, _url, "string")
    expect(2, _headers, "table", "nil")
    expect(3, _binary, "boolean", "nil")
    return wrapRequest(_url, _url, nil, _headers, _binary)
end

--[[- Make a HTTP POST request to the given url.

@tparam string url   The url to request
@tparam string body  The body of the POST request.
@tparam[opt] { [string] = string } headers Additional headers to send as part
of this request.
@tparam[opt] boolean binary Whether to make a binary HTTP request. If true,
the body will not be UTF-8 encoded, and the received response will not be
decoded.

@tparam[2] {
  url = string, body? = string, headers? = { [string] = string },
  binary? = boolean, method? = string, redirect? = boolean,
} request Options for the request. See @{http.request} for details on how
these options behave.

@treturn Response The resulting http response, which can be read from.
@treturn[2] nil When the http request failed, such as in the event of a 404
error or connection timeout.
@treturn string A message detailing why the request failed.
@treturn Response|nil The failing http response, if available.

@since 1.31
@changed 1.63 Added argument for headers.
@changed 1.80pr1 Response handles are now returned on error if available.
@changed 1.80pr1 Added argument for binary handles.
@changed 1.80pr1.6 Added support for table argument.
@changed 1.86.0 Added PATCH and TRACE methods.
]]
function sys_http.post(_url, _post, _headers, _binary)
    if type(_url) == "table" then
        checkOptions(_url, true)
        return wrapRequest(_url.url, _url)
    end

    expect(1, _url, "string")
    expect(2, _post, "string")
    expect(3, _headers, "table", "nil")
    expect(4, _binary, "boolean", "nil")
    return wrapRequest(_url, _url, _post, _headers, _binary)
end

for k in pairs(methods) do if k ~= "GET" and k ~= "POST" then
    sys_http[k:lower()] = function(_url, _post, _headers, _binary)
        if type(_url) == "table" then
            checkOptions(_url, true)
            return wrapRequest(_url.url, _url)
        end

        expect(1, _url, "string")
        expect(2, _post, "string")
        expect(3, _headers, "table", "nil")
        expect(4, _binary, "boolean", "nil")
        return wrapRequest(_url, {url = _url, body = _post, headers = _headers, binary = _binary, method = k})
    end
end end

--[[- Asynchronously make a HTTP request to the given url.

This returns immediately, a @{http_success} or @{http_failure} will be queued
once the request has completed.

@tparam      string url   The url to request
@tparam[opt] string body  An optional string containing the body of the
request. If specified, a `POST` request will be made instead.
@tparam[opt] { [string] = string } headers Additional headers to send as part
of this request.
@tparam[opt] boolean binary Whether to make a binary HTTP request. If true,
the body will not be UTF-8 encoded, and the received response will not be
decoded.

@tparam[2] {
  url = string, body? = string, headers? = { [string] = string },
  binary? = boolean, method? = string, redirect? = boolean,
} request Options for the request.

This table form is an expanded version of the previous syntax. All arguments
from above are passed in as fields instead (for instance,
`http.request("https://example.com")` becomes `http.request { url =
"https://example.com" }`).
 This table also accepts several additional options:

 - `method`: Which HTTP method to use, for instance `"PATCH"` or `"DELETE"`.
 - `redirect`: Whether to follow HTTP redirects. Defaults to true.

@see http.get  For a synchronous way to make GET requests.
@see http.post For a synchronous way to make POST requests.

@changed 1.63 Added argument for headers.
@changed 1.80pr1 Added argument for binary handles.
@changed 1.80pr1.6 Added support for table argument.
@changed 1.86.0 Added PATCH and TRACE methods.
]]
function sys_http.request(_url, _post, _headers, _binary)
    local url
    if type(_url) == "table" then
        checkOptions(_url)
        url = _url.url
    else
        expect(1, _url, "string")
        expect(2, _post, "string", "nil")
        expect(3, _headers, "table", "nil")
        expect(4, _binary, "boolean", "nil")
        url = _url
    end

    local ok, err = nativeHTTPRequest(_url, _post, _headers, _binary)
    if not ok then
        os.queueEvent("http_failure", url, err)
    end

    -- Return true/false for legacy reasons. Undocumented, as it shouldn't be relied on.
    return ok, err
end

if native.addListener then
    function sys_http.listen( _port, _callback )
        expect(1, _port, "number")
        expect(2, _callback, "function")
        native.addListener( _port )
        while true do
            local ev, p1, p2, p3 = os.pullEvent()
            if ev == "server_stop" then
                native.removeListener( _port )
                break
            elseif ev == "http_request" and p1 == _port then
                if _callback( p2, p3 ) then 
                    native.removeListener( _port )
                    break
                end
            end
        end
    end
end

local nativeCheckURL = native.checkURL

--[[- Asynchronously determine whether a URL can be requested.

If this returns `true`, one should also listen for @{http_check} which will
container further information about whether the URL is allowed or not.

@tparam string url The URL to check.
@treturn true When this url is not invalid. This does not imply that it is
allowed - see the comment above.
@treturn[2] false When this url is invalid.
@treturn string A reason why this URL is not valid (for instance, if it is
malformed, or blocked).

@see http.checkURL For a synchronous version.
]]
sys_http.checkURLAsync = nativeCheckURL

--[[- Determine whether a URL can be requested.

If this returns `true`, one should also listen for @{http_check} which will
container further information about whether the URL is allowed or not.

@tparam string url The URL to check.
@treturn true When this url is valid and can be requested via @{http.request}.
@treturn[2] false When this url is invalid.
@treturn string A reason why this URL is not valid (for instance, if it is
malformed, or blocked).

@see http.checkURLAsync For an asynchronous version.

@usage
```lua
print(http.checkURL("https://example.tweaked.cc/"))
-- => true
print(http.checkURL("http://localhost/"))
-- => false Domain not permitted
print(http.checkURL("not a url"))
-- => false URL malformed
```
]]
function sys_http.checkURL(_url)
    expect(1, _url, "string")
    local ok, err = nativeCheckURL(_url)
    if not ok then return ok, err end

    while true do
        local _, url, ok, err = os.pullEvent("http_check")
        if url == _url then return ok, err end
    end
end

local nativeWebsocket = native.websocket

--[[- Asynchronously open a websocket.

This returns immediately, a @{websocket_success} or @{websocket_failure}
will be queued once the request has completed.

@tparam string url The websocket url to connect to. This should have the
`ws://` or `wss://` protocol.
@tparam[opt] { [string] = string } headers Additional headers to send as part
of the initial websocket connection.
@since 1.80pr1.3
@changed 1.95.3 Added User-Agent to default headers.
]]
function sys_http.websocketAsync(url, headers)
    expect(1, url, "string")
    expect(2, headers, "table", "nil")

    local ok, err = nativeWebsocket(url, headers)
    if not ok then
        os.queueEvent("websocket_failure", url, err)
    end

    -- Return true/false for legacy reasons. Undocumented, as it shouldn't be relied on.
    return ok, err
end

--[[- Open a websocket.

@tparam string url The websocket url to connect to. This should have the
`ws://` or `wss://` protocol.
@tparam[opt] { [string] = string } headers Additional headers to send as part
of the initial websocket connection.

@treturn Websocket The websocket connection.
@treturn[2] false If the websocket connection failed.
@treturn string An error message describing why the connection failed.
@since 1.80pr1.1
@changed 1.80pr1.3 No longer asynchronous.
@changed 1.95.3 Added User-Agent to default headers.
]]
function sys_http.websocket(_url, _headers)
    expect(1, _url, "string", "number")
    expect(2, _headers, "table", "nil")

    local ok, err = nativeWebsocket(_url, _headers)
    if not ok then return ok, err end

    while true do
        local event, url, param, wsid = os.pullEvent( )
        if event == "websocket_success" and url == _url then
            return param, wsid
        elseif event == "websocket_failure" and url == _url then
            return false, param
        end
    end
end

sys_http.addListener = native.addListener
sys_http.removeListener = native.removeListener

return sys_http

end
preload["help"] = function(...)
-- SPDX-FileCopyrightText: 2017 Daniel Ratcliffe
--
-- SPDX-License-Identifier: LicenseRef-CCPL

--- Provides an API to read help files.
--
-- @module help
-- @since 1.2

local expect = dofile("rom/modules/main/cc/expect.lua").expect
local help = {}
local sPath = "/rom/help"

--- Returns a colon-separated list of directories where help files are searched
-- for. All directories are absolute.
--
-- @treturn string The current help search path, separated by colons.
-- @see help.setPath
function help.path()
    return sPath
end

--- Sets the colon-seperated list of directories where help files are searched
-- for to `newPath`
--
-- @tparam string newPath The new path to use.
-- @usage help.setPath( "/disk/help/" )
-- @usage help.setPath( help.path() .. ":/myfolder/help/" )
-- @see help.path
function help.setPath(_sPath)
    expect(1, _sPath, "string")
    sPath = _sPath
end

local extensions = { "", ".md", ".txt" }

--- Returns the location of the help file for the given topic.
--
-- @tparam string topic The topic to find
-- @treturn string|nil The path to the given topic's help file, or `nil` if it
-- cannot be found.
-- @usage help.lookup("disk")
-- @changed 1.80pr1 Now supports finding .txt files.
-- @changed 1.97.0 Now supports finding Markdown files.
function help.lookup(topic)
    expect(1, topic, "string")
    -- Look on the path variable
    for path in string.gmatch(sPath, "[^:]+") do
        path = fs.combine(path, topic)
        for _, extension in ipairs(extensions) do
            local file = path .. extension
            if fs.exists(file) and not fs.isDir(file) then
                return file
            end
        end
    end

    -- Not found
    return nil
end

--- Returns a list of topics that can be looked up and/or displayed.
--
-- @treturn table A list of topics in alphabetical order.
-- @usage help.topics()
function help.topics()
    -- Add index
    local tItems = {
        ["index"] = true,
    }

    -- Add topics from the path
    for sPath in string.gmatch(sPath, "[^:]+") do
        if fs.isDir(sPath) then
            local tList = fs.list(sPath)
            for _, sFile in pairs(tList) do
                if string.sub(sFile, 1, 1) ~= "." then
                    if not fs.isDir(fs.combine(sPath, sFile)) then
                        for i = 2, #extensions do
                            local extension = extensions[i]
                            if #sFile > #extension and sFile:sub(-#extension) == extension then
                                sFile = sFile:sub(1, -#extension - 1)
                            end
                        end
                        tItems[sFile] = true
                    end
                end
            end
        end
    end

    -- Sort and return
    local tItemList = {}
    for sItem in pairs(tItems) do
        table.insert(tItemList, sItem)
    end
    table.sort(tItemList)
    return tItemList
end

--- Returns a list of topic endings that match the prefix. Can be used with
-- `read` to allow input of a help topic.
--
-- @tparam string prefix The prefix to match
-- @treturn table A list of matching topics.
-- @since 1.74
function help.completeTopic(sText)
    expect(1, sText, "string")
    local tTopics = topics()
    local tResults = {}
    for n = 1, #tTopics do
        local sTopic = tTopics[n]
        if #sTopic > #sText and string.sub(sTopic, 1, #sText) == sText then
            table.insert(tResults, string.sub(sTopic, #sText + 1))
        end
    end
    return tResults
end

return help
end
preload["gps"] = function(...)
-- SPDX-FileCopyrightText: 2017 Daniel Ratcliffe
--
-- SPDX-License-Identifier: LicenseRef-CCPL

--[[- The GPS API provides a method for turtles and computers to retrieve their
own locations.

It broadcasts a PING message over @{rednet} and wait for responses. In order for
this system to work, there must be at least 4 computers used as gps hosts which
will respond and allow trilateration. Three of these hosts should be in a plane,
and the fourth should be either above or below the other three. The three in a
plane should not be in a line with each other. You can set up hosts using the
gps program.

:::note
When entering in the coordinates for the host you need to put in the `x`, `y`,
and `z` coordinates of the computer, not the modem, as all modem distances are
measured from the block the computer is in.
:::

Also note that you may choose which axes x, y, or z refers to - so long as your
systems have the same definition as any GPS servers that're in range, it works
just the same. For example, you might build a GPS cluster according to [this
tutorial][1], using z to account for height, or you might use y to account for
height in the way that Minecraft's debug screen displays.

[1]: http://www.computercraft.info/forums2/index.php?/topic/3088-how-to-guide-gps-global-position-system/

@module gps
@since 1.31
]]

local expect = dofile("rom/modules/main/cc/expect.lua").expect
local gps = {}
--- The channel which GPS requests and responses are broadcast on.
gps.CHANNEL_GPS = 65534

local function trilaterate(A, B, C)
    local a2b = B.vPosition - A.vPosition
    local a2c = C.vPosition - A.vPosition

    if math.abs(a2b:normalize():dot(a2c:normalize())) > 0.999 then
        return nil
    end

    local d = a2b:length()
    local ex = a2b:normalize( )
    local i = ex:dot(a2c)
    local ey = (a2c - ex * i):normalize()
    local j = ey:dot(a2c)
    local ez = ex:cross(ey)

    local r1 = A.nDistance
    local r2 = B.nDistance
    local r3 = C.nDistance

    local x = (r1 * r1 - r2 * r2 + d * d) / (2 * d)
    local y = (r1 * r1 - r3 * r3 - x * x + (x - i) * (x - i) + j * j) / (2 * j)

    local result = A.vPosition + ex * x + ey * y

    local zSquared = r1 * r1 - x * x - y * y
    if zSquared > 0 then
        local z = math.sqrt(zSquared)
        local result1 = result + ez * z
        local result2 = result - ez * z

        local rounded1, rounded2 = result1:round(0.01), result2:round(0.01)
        if rounded1.x ~= rounded2.x or rounded1.y ~= rounded2.y or rounded1.z ~= rounded2.z then
            return rounded1, rounded2
        else
            return rounded1
        end
    end
    return result:round(0.01)

end

local function narrow(p1, p2, fix)
    local dist1 = math.abs((p1 - fix.vPosition):length() - fix.nDistance)
    local dist2 = math.abs((p2 - fix.vPosition):length() - fix.nDistance)

    if math.abs(dist1 - dist2) < 0.01 then
        return p1, p2
    elseif dist1 < dist2 then
        return p1:round(0.01)
    else
        return p2:round(0.01)
    end
end

--- Tries to retrieve the computer or turtles own location.
--
-- @tparam[opt=2] number timeout The maximum time in seconds taken to establish our
-- position.
-- @tparam[opt=false] boolean debug Print debugging messages
-- @treturn[1] number This computer's `x` position.
-- @treturn[1] number This computer's `y` position.
-- @treturn[1] number This computer's `z` position.
-- @treturn[2] nil If the position could not be established.
function gps.locate(_nTimeout, _bDebug)
    expect(1, _nTimeout, "number", "nil")
    expect(2, _bDebug, "boolean", "nil")
    -- Let command computers use their magic fourth-wall-breaking special abilities
    if commands then
        return commands.getBlockPosition()
    end

    -- Find a modem
    local sModemSide = nil
    for _, sSide in ipairs(rs.getSides()) do
        if peripheral.getType(sSide) == "modem" and peripheral.call(sSide, "isWireless") then
            sModemSide = sSide
            break
        end
    end

    if sModemSide == nil then
        if _bDebug then
            print("No wireless modem attached")
        end
        return nil
    end

    if _bDebug then
        print("Finding position...")
    end

    -- Open GPS channel to listen for ping responses
    local modem = peripheral.wrap(sModemSide)
    local bCloseChannel = false
    if not modem.isOpen(CHANNEL_GPS) then
        modem.open(CHANNEL_GPS)
        bCloseChannel = true
    end

    -- Send a ping to listening GPS hosts
    modem.transmit(CHANNEL_GPS, CHANNEL_GPS, "PING")

    -- Wait for the responses
    local tFixes = {}
    local pos1, pos2 = nil, nil
    local timeout = os.startTimer(_nTimeout or 2)
    while true do
        local e, p1, p2, p3, p4, p5 = os.pullEvent()
        if e == "modem_message" then
            -- We received a reply from a modem
            local sSide, sChannel, sReplyChannel, tMessage, nDistance = p1, p2, p3, p4, p5
            if sSide == sModemSide and sChannel == CHANNEL_GPS and sReplyChannel == CHANNEL_GPS and nDistance then
                -- Received the correct message from the correct modem: use it to determine position
                if type(tMessage) == "table" and #tMessage == 3 and tonumber(tMessage[1]) and tonumber(tMessage[2]) and tonumber(tMessage[3]) then
                    local tFix = { vPosition = vector.new(tMessage[1], tMessage[2], tMessage[3]), nDistance = nDistance }
                    if _bDebug then
                        print(tFix.nDistance .. " metres from " .. tostring(tFix.vPosition))
                    end
                    if tFix.nDistance == 0 then
                        pos1, pos2 = tFix.vPosition, nil
                    else
                        -- Insert our new position in our table, with a maximum of three items. If this is close to a
                        -- previous position, replace that instead of inserting.
                        local insIndex = math.min(3, #tFixes + 1)
                        for i, older in pairs(tFixes) do
                            if (older.vPosition - tFix.vPosition):length() < 5 then
                                insIndex = i
                                break
                            end
                        end
                        tFixes[insIndex] = tFix

                        if #tFixes >= 3 then
                            if not pos1 then
                                pos1, pos2 = trilaterate(tFixes[1], tFixes[2], tFixes[3])
                            else
                                pos1, pos2 = narrow(pos1, pos2, tFixes[3])
                            end
                        end
                    end
                    if pos1 and not pos2 then
                        break
                    end
                end
            end

        elseif e == "timer" then
            -- We received a timeout
            local timer = p1
            if timer == timeout then
                break
            end

        end
    end

    -- Close the channel, if we opened one
    if bCloseChannel then
        modem.close(CHANNEL_GPS)
    end

    -- Return the response
    if pos1 and pos2 then
        if _bDebug then
            print("Ambiguous position")
            print("Could be " .. pos1.x .. "," .. pos1.y .. "," .. pos1.z .. " or " .. pos2.x .. "," .. pos2.y .. "," .. pos2.z)
        end
        return nil
    elseif pos1 then
        if _bDebug then
            print("Position is " .. pos1.x .. "," .. pos1.y .. "," .. pos1.z)
        end
        return pos1.x, pos1.y, pos1.z
    else
        if _bDebug then
            print("Could not determine position")
        end
        return nil
    end
end

return gps
end
preload["disk"] = function(...)
-- SPDX-FileCopyrightText: 2017 Daniel Ratcliffe
--
-- SPDX-License-Identifier: LicenseRef-CCPL

--[[- The Disk API allows you to interact with disk drives.

These functions can operate on locally attached or remote disk drives. To use a
locally attached drive, specify “side” as one of the six sides (e.g. `left`); to
use a remote disk drive, specify its name as printed when enabling its modem
(e.g. `drive_0`).

:::tip
All computers (except command computers), turtles and pocket computers can be
placed within a disk drive to access it's internal storage like a disk.
:::

@module disk
@since 1.2
]]
local disk = {}

local function isDrive(name)
    if type(name) ~= "string" then
        error("bad argument #1 (string expected, got " .. type(name) .. ")", 3)
    end
    return peripheral.getType(name) == "drive"
end

--- Checks whether any item at all is in the disk drive
--
-- @tparam string name The name of the disk drive.
-- @treturn boolean If something is in the disk drive.
-- @usage disk.isPresent("top")
function disk.isPresent(name)
    if isDrive(name) then
        return peripheral.call(name, "isDiskPresent")
    end
    return false
end

--- Get the label of the floppy disk, record, or other media within the given
-- disk drive.
--
-- If there is a computer or turtle within the drive, this will set the label as
-- read by `os.getComputerLabel`.
--
-- @tparam string name The name of the disk drive.
-- @treturn string|nil The name of the current media, or `nil` if the drive is
-- not present or empty.
-- @see disk.setLabel
function disk.getLabel(name)
    if isDrive(name) then
        return peripheral.call(name, "getDiskLabel")
    end
    return nil
end

--- Set the label of the floppy disk or other media
--
-- @tparam string name The name of the disk drive.
-- @tparam string|nil label The new label of the disk
function disk.setLabel(name, label)
    if isDrive(name) then
        peripheral.call(name, "setDiskLabel", label)
    end
end

--- Check whether the current disk provides a mount.
--
-- This will return true for disks and computers, but not records.
--
-- @tparam string name The name of the disk drive.
-- @treturn boolean If the disk is present and provides a mount.
-- @see disk.getMountPath
function disk.hasData(name)
    if isDrive(name) then
        return peripheral.call(name, "hasData")
    end
    return false
end

--- Find the directory name on the local computer where the contents of the
-- current floppy disk (or other mount) can be found.
--
-- @tparam string name The name of the disk drive.
-- @treturn string|nil The mount's directory, or `nil` if the drive does not
-- contain a floppy or computer.
-- @see disk.hasData
function disk.getMountPath(name)
    if isDrive(name) then
        return peripheral.call(name, "getMountPath")
    end
    return nil
end

--- Whether the current disk is a [music disk][disk] as opposed to a floppy disk
-- or other item.
--
-- If this returns true, you will can @{disk.playAudio|play} the record.
--
-- [disk]: https://minecraft.gamepedia.com/Music_Disc
--
-- @tparam string name The name of the disk drive.
-- @treturn boolean If the disk is present and has audio saved on it.
function disk.hasAudio(name)
    if isDrive(name) then
        return peripheral.call(name, "hasAudio")
    end
    return false
end

--- Get the title of the audio track from the music record in the drive.
--
-- This generally returns the same as @{disk.getLabel} for records.
--
-- @tparam string name The name of the disk drive.
-- @treturn string|false|nil The track title, @{false} if there is not a music
-- record in the drive or `nil` if no drive is present.
function disk.getAudioTitle(name)
    if isDrive(name) then
        return peripheral.call(name, "getAudioTitle")
    end
    return nil
end

--- Starts playing the music record in the drive.
--
-- If any record is already playing on any disk drive, it stops before the
-- target drive starts playing. The record stops when it reaches the end of the
-- track, when it is removed from the drive, when @{disk.stopAudio} is called, or
-- when another record is started.
--
-- @tparam string name The name of the disk drive.
-- @usage disk.playAudio("bottom")
function disk.playAudio(name)
    if isDrive(name) then
        peripheral.call(name, "playAudio")
    end
end

--- Stops the music record in the drive from playing, if it was started with
-- @{disk.playAudio}.
--
-- @tparam string name The name o the disk drive.
function disk.stopAudio(name)
    if not name then
        for _, sName in ipairs(peripheral.getNames()) do
            stopAudio(sName)
        end
    else
        if isDrive(name) then
            peripheral.call(name, "stopAudio")
        end
    end
end

--- Ejects any item currently in the drive, spilling it into the world as a loose item.
--
-- @tparam string name The name of the disk drive.
-- @usage disk.eject("bottom")
function disk.eject(name)
    if isDrive(name) then
        peripheral.call(name, "ejectDisk")
    end
end

--- Returns a number which uniquely identifies the disk in the drive.
--
-- Note, unlike @{disk.getLabel}, this does not return anything for other media,
-- such as computers or turtles.
--
-- @tparam string name The name of the disk drive.
-- @treturn string|nil The disk ID, or `nil` if the drive does not contain a floppy disk.
-- @since 1.4
function disk.getID(name)
    if isDrive(name) then
        return peripheral.call(name, "getDiskID")
    end
    return nil
end


return disk

end
preload["colours"] = function(...)
-- SPDX-FileCopyrightText: 2017 Daniel Ratcliffe
--
-- SPDX-License-Identifier: LicenseRef-CCPL

--- Colours for lovers of British spelling.
--
-- @see colors
-- @module colours
-- @since 1.2

local colours = {}
for k, v in pairs(colors) do
    colours[k] = v
end

--- Grey. Written as `7` in paint files and @{term.blit}, has a default
-- terminal colour of #4C4C4C.
--
-- @see colors.gray
colours.grey = colors.gray
colours.gray = nil --- @local

--- Light grey. Written as `8` in paint files and @{term.blit}, has a
-- default terminal colour of #999999.
--
-- @see colors.lightGray
colours.lightGrey = colors.lightGray
colours.lightGray = nil --- @local

return colours
end
return preload["userspace"](...)
