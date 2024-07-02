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
preload["userspace"] = function(...)
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

end
preload["toml"] = function(...)
-- TOML library for Lua/CC
-- From Phoenix libsystem serialization.toml
--
-- MIT License
--
-- Copyright (c) 2024 JackMacWindows
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

local expect
local field

do
    local h = fs.open("/rom/modules/main/cc/expect.lua", "r")
    local f, err = (_VERSION == "Lua 5.1" and loadstring or load)(h.readAll(), "/rom/modules/main/cc/expect.lua")
    h.close()

    if not f then error(err) end
    expect = f()
    field = expect.field
end

local toml = {}

-- From json.lua by rxi
-- MIT license

local escape_char_map = {
    [ "\\" ] = "\\",
    [ "\"" ] = "\"",
    [ "\b" ] = "b",
    [ "\f" ] = "f",
    [ "\n" ] = "n",
    [ "\r" ] = "r",
    [ "\t" ] = "t",
}

local escape_char_map_inv = { [ "/" ] = "/" }
for k, v in pairs(escape_char_map) do
    escape_char_map_inv[v] = k
end

local function create_set(...)
    local res = {}
    for i = 1, select("#", ...) do
        res[ select(i, ...) ] = true
    end
    return res
end

local escape_chars  = create_set("\\", "/", '"', "b", "f", "n", "r", "t", "u")

local function decode_error(str, idx, msg)
    local line_count = 1
    local col_count = 1
    for i = 1, idx - 1 do
        col_count = col_count + 1
        if str:sub(i, i) == "\n" then
            line_count = line_count + 1
            col_count = 1
        end
    end
    error( string.format("%s at line %d col %d", msg, line_count, col_count) )
end


local function codepoint_to_utf8(n)
    -- http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=iws-appendixa
    local f = math.floor
    if n <= 0x7f then
        return string.char(n)
    elseif n <= 0x7ff then
        return string.char(f(n / 64) + 192, n % 64 + 128)
    elseif n <= 0xffff then
        return string.char(f(n / 4096) + 224, f(n % 4096 / 64) + 128, n % 64 + 128)
    elseif n <= 0x10ffff then
        return string.char(f(n / 262144) + 240, f(n % 262144 / 4096) + 128,
                                             f(n % 4096 / 64) + 128, n % 64 + 128)
    end
    error( string.format("invalid unicode codepoint '%x'", n) )
end


local function parse_unicode_escape(s)
    local n1 = tonumber( s:sub(1, 4),  16 )
    local n2 = tonumber( s:sub(7, 10), 16 )
     -- Surrogate pair?
    if n2 then
        return codepoint_to_utf8((n1 - 0xd800) * 0x400 + (n2 - 0xdc00) + 0x10000)
    else
        return codepoint_to_utf8(n1)
    end
end


local function parse_string(str, i)
    local res = ""
    local j = i + 1
    local k = j

    while j <= #str do
        local x = str:byte(j)

        if x < 32 then
            decode_error(str, j, "control character in string")

        elseif x == 92 then -- `\`: Escape
            res = res .. str:sub(k, j - 1)
            j = j + 1
            local c = str:sub(j, j)
            if c == "u" then
                local hex = str:match("^[dD][89aAbB]%x%x\\u%x%x%x%x", j + 1)
                                 or str:match("^%x%x%x%x", j + 1)
                                 or decode_error(str, j - 1, "invalid unicode escape in string")
                res = res .. parse_unicode_escape(hex)
                j = j + #hex
            else
                if not escape_chars[c] then
                    decode_error(str, j - 1, "invalid escape char '" .. c .. "' in string")
                end
                res = res .. escape_char_map_inv[c]
            end
            k = j + 1

        elseif x == 34 then -- `"`: End of string
            res = res .. str:sub(k, j - 1)
            return res, j + 1
        end

        j = j + 1
    end

    decode_error(str, i, "expected closing quote for string")
end

-- End json.lua

local keywords = {
    ["and"] = true,
    ["break"] = true,
    ["do"] = true,
    ["else"] = true,
    ["elseif"] = true,
    ["end"] = true,
    ["false"] = true,
    ["for"] = true,
    ["function"] = true,
    ["goto"] = true,
    ["if"] = true,
    ["in"] = true,
    ["local"] = true,
    ["nil"] = true,
    ["not"] = true,
    ["or"] = true,
    ["repeat"] = true,
    ["return"] = true,
    ["then"] = true,
    ["true"] = true,
    ["until"] = true,
    ["while"] = true,
}

local function lua_serialize(val, stack, opts, level)
    if stack[val] then error("Cannot serialize recursive value", 0) end
    local tt = type(val)
    if tt == "table" then
        if not next(val) then return "{}" end
        stack[val] = true
        local res = opts.minified and "{" or "{\n"
        local num = {}
        for i, v in ipairs(val) do
            if not opts.minified then res = res .. ("    "):rep(level) end
            num[i] = true
            res = res .. lua_serialize(v, stack, opts, level + 1) .. (opts.minified and "," or ",\n")
        end
        for k, v in pairs(val) do if not num[k] then
            if not opts.minified then res = res .. ("    "):rep(level) end
            if type(k) == "string" and k:match "^[A-Za-z_][A-Za-z0-9_]*$" and not keywords[k] then res = res .. k
            else res = res .. "[" .. lua_serialize(k, stack, opts, level + 1) .. "]" end
            res = res .. (opts.minified and "=" or " = ") .. lua_serialize(v, stack, opts, level + 1) .. (opts.minified and "," or ",\n")
        end end
        if opts.minified then res = res:gsub(",$", "")
        else res = res .. ("    "):rep(level - 1) end
        stack[val] = nil
        return res .. "}"
    elseif tt == "function" and opts.allow_functions then
        local ok, dump = pcall(string.dump, val)
        if not ok then error("Cannot serialize C function", 0) end
        dump = ("%q"):format(dump):gsub("\\[%z\1-\31\127-\255]", function(c) return ("\\%03d"):format(string.byte(c)) end)
        local ups = {n = 0}
        stack[val] = true
        for i = 1, math.huge do
            local ok, name, value = pcall(debug.getupvalue, val, i)
            if not ok or not name then break end
            ups[i] = value
            ups.n = i
        end
        local name = "=(serialized function)"
        local ok, info = pcall(debug.getinfo, val, "S")
        if ok then name = info.source or name end
        local v = ("__function(%s,%q,%s)"):format(dump, name, lua_serialize(ups, stack, opts, level + 1))
        stack[val] = nil
        return v
    elseif tt == "nil" or tt == "number" or tt == "boolean" or tt == "string" then
        return ("%q"):format(val):gsub("\\\n", "\\n"):gsub("\\?[%z\1-\31\127-\255]", function(c) return ("\\%03d"):format(string.byte(c)) end)
    else
        error("Cannot serialize type " .. tt, 0)
    end
end

local function encodeTOMLArray(arr, opts, names)
    local int, str = false, false
    for l in pairs(arr) do
        if type(l) == "number" then int = true
        elseif type(l) == "string" then str = true
        else error("key " .. table.concat(names, ".") .. "." .. tostring(l) .. " is not a string") end
    end
    local e = #names + 1
    if not int and not str then return "[]"
    elseif int and str then error("invalid entry " .. table.concat(names, ".") .. " (contains both array and dictionary values)")
    elseif int then
        local retval = "["
        for i, v in ipairs(arr) do
            if type(v) == "table" then
                names[e] = tostring(i)
                retval = retval .. (retval == "[" and "" or ", ") .. encodeTOMLArray(v, opts, names)
                names[e] = nil
            else retval = retval .. (retval == "[" and "" or ", ") .. lua_serialize(v, {}, {}, #names) end
        end
        return retval .. "]"
    else
        local res = "{"
        for k, v in pairs(arr) do
            if res ~= "{" then res = res .. ", " end
            if type(k) == "string" and k:match "^[A-Za-z0-9_%-]+$" then res = res .. k
            else res = res .. lua_serialize(k, {}, {}, #names) end
            if type(v) == "table" then
                names[e] = k
                res = res .. " = " .. encodeTOMLArray(v, opts, names)
                names[e] = nil
            else res = res .. " = " .. lua_serialize(v, {}, {}, #names) end
        end
        return res .. "}"
    end
end

local function encodeTOML(tbl, opts, names)
    local retval = ""
    local indent = opts.indent == false and "" or ("    "):rep(#names)
    if #names > 0 then retval = ("%s[%s]\n"):format(("    "):rep(#names - 1), table.concat(names, ".")) end
    local tbls, arrs = {}, {}
    local e = #names + 1
    for k, v in pairs(tbl) do
        assert(type(k) == "string", "key " .. table.concat(names, ".") .. "." .. tostring(k) .. " is not a string")
        local key = k:match("^[A-Za-z0-9_%-]+$") and k or lua_serialize(k, {}, {}, 1)
        local t = type(v)
        if t == "table" then
            local int, str, tab = false, false, true
            for l, w in pairs(v) do
                if type(l) == "number" then int = true
                elseif type(l) == "string" then str = true
                else error("key " .. table.concat(names, ".") .. "." .. tostring(k) .. "." .. tostring(l) .. " is not a string") end
                if type(w) ~= "table" then tab = false
                else for m in pairs(w) do if type(m) ~= "string" then tab = false break end end end
            end
            if not int and not str then retval = retval .. indent .. key .. " = []\n"
            elseif int and str then error("invalid entry " .. table.concat(names, ".") .. "." .. tostring(k) .. " (contains both array and dictionary values)")
            elseif int then
                if tab then
                    arrs[k] = v
                else
                    names[e] = k
                    retval = retval .. indent .. key .. " = " .. encodeTOMLArray(v, opts, names)
                    names[e] = nil
                end
            else tbls[k] = v end
        else retval = retval .. indent .. key .. " = " .. lua_serialize(v, {}, {}, #names) .. "\n" end
    end
    for k, arr in pairs(arrs) do
        names[e] = k
        for _, v in ipairs(arr) do
            retval = retval .. ("%s[[%s]]\n"):format(indent, table.concat(names, ".")) .. encodeTOML(v, opts, names) .. "\n"
        end
    end
    for k, v in pairs(tbls) do
        names[e] = k
        retval = retval .. ("%s[%s]\n"):format(indent, table.concat(names, ".")) .. encodeTOML(v, opts, names) .. "\n"
    end
    names[e] = nil
    return retval
end

--- Encodes a table into TOML format. This table must only have integer or
-- string keys in itself and each subtable, and cannot mix strings and ints.
-- @tparam table tbl The table to encode
-- @tparam[opt] {indent=boolean} opts Any options to specify while encoding
-- @treturn string The encoded TOML data
function toml.encode(tbl, opts)
    expect(1, tbl, "table")
    expect(2, opts, "table", "nil")
    return encodeTOML(tbl, opts or {}, {})
end

local function traverse(tab, name, pos, ln, wantlast)
    local last, nm
    while pos < #name do
        if pos > 1 then
            pos = name:match("^%s*()", pos)
            if wantlast and name:sub(pos, pos) == "=" then return last, nm, pos + 1 end
            if name:sub(pos, pos) ~= "." then error("Expected . on line " .. ln, 3) end
            pos = name:match("^%s*()", pos + 1)
        end
        local key
        if name:match('^"', pos) then key, pos = parse_string(name, pos + 1)
        elseif name:match("^'", pos) then key, pos = name:match("'([^']*)'()", pos)
        else key, pos = name:match("^([A-Za-z0-9_%-]+)()", pos) end
        if not key then error("Invalid key name on line " .. ln, 3) end
        last, nm = tab, key
        if not tab[key] then tab[key] = {} end
        tab = tab[key]
    end
    if wantlast then error("Expected = on line " .. ln, 3) end
    return tab
end

local function next_token(line, pos, ln)
    pos = line:match("^%s*()", pos)
    while pos > #line or line:sub(pos, pos) == "#" do
        line = coroutine.yield()
        ln = ln + 1
        pos = line:match "^%s*()"
    end
    return line, pos, ln
end

local function toml_assign(tab, key, line, pos, ln)
    local op = line:sub(pos, pos)
    while op == "#" do
        line = coroutine.yield()
        ln = ln + 1
        pos = line:match "^%s*()"
        op = line:sub(pos, pos)
    end
    if op == "[" then
        local retval = {}
        local i = 1
        line, pos, ln = next_token(line, pos + 1, ln)
        while true do
            op = line:sub(pos, pos)
            if op == "]" then break end
            line, pos, ln = toml_assign(retval, i, line, pos, ln)
            line, pos, ln = next_token(line, pos, ln)
            op = line:sub(pos, pos)
            if op == "]" then break end
            if op ~= "," then error("Expected , on line " .. ln, 0) end
            line, pos, ln = next_token(line, pos + 1, ln)
            i = i + 1
        end
        tab[key] = retval
        return line, pos + 1, ln
    elseif op == "{" then
        local retval = {}
        line, pos, ln = next_token(line, pos + 1, ln)
        while true do
            op = line:sub(pos, pos)
            if op == "}" then break end
            local t, k
            t, k, pos = traverse(retval, line, pos, ln, true)
            line, pos, ln = next_token(line, pos, ln)
            line, pos, ln = toml_assign(t, k, line, pos, ln)
            line, pos, ln = next_token(line, pos, ln)
            op = line:sub(pos, pos)
            if op == "}" then break end
            if op ~= "," then error("Expected , on line " .. ln, 0) end
            line, pos, ln = next_token(line, pos + 1, ln)
        end
        tab[key] = retval
        return line, pos + 1, ln
    elseif op == "'" then
        if line:match("^'''", pos) then
            pos = pos + 3
            local str = ""
            while not line:find("'''", pos) do
                if not (str == "" and pos == #line) then
                    str = str .. line:sub(pos) .. "\n"
                end
                line = coroutine.yield()
                ln, pos = ln + 1, 1
            end
            str = str .. line:sub(pos, line:find("'''", pos) - 1)
            pos = line:match("'''()", pos)
            tab[key] = str
            return line, pos, ln
        else
            local str, pos = line:match("^'([^']*)'()", pos)
            if not str then error("Invalid literal string on line " .. ln, 0) end
            tab[key] = str
            return line, pos, ln
        end
    elseif op == '"' then
        if line:match('^"""', pos) then
            local s = ""
            while not line:find('"""', pos) do
                if not (s == "" and pos == #line) then
                    s = s .. line:sub(pos) .. "\n"
                end
                line = coroutine.yield()
                ln, pos = ln + 1, 1
            end
            s = s .. line:sub(pos, line:find('"""', pos) - 1)
            s = s:gsub("\\\r?\n", ""):gsub('"', '\\"') .. '"'
            tab[key] = parse_string(s, 1)
            pos = line:match('"""()', pos)
            return line, pos, ln
        else
            local str, pos = parse_string(line, pos)
            if not str then error("Invalid string on line " .. ln, 0) end
            tab[key] = str
            return line, pos, ln
        end
    elseif line:match("^%d%d%d%d%-%d%d%-%d%d[T ]%d%d:%d%d:%d%d", pos) then
        local y, M, d, h, m, s, pos = line:match("^(%d%d%d%d)%-(%d%d)%-(%d%d)[T ](%d%d):(%d%d):(%d%d)()", pos)
        local date = {
            year = tonumber(y),
            month = tonumber(M),
            day = tonumber(d),
            hour = tonumber(h),
            min = tonumber(m),
            sec = tonumber(s)
        }
        local time = os.time(date)
        if line:match("^%.%d+", pos) then
            local ss
            ss, pos = line:match("(%.%d+)()", pos)
            time = time + tonumber("0" .. ss)
        end
        local c = line:sub(pos, pos)
        if c == "+" or c == "-" then
            local oh, om
            oh, om, pos = line:match("^[%+%-](%d%d):(%d%d)()", pos)
            if not oh then error("Invalid date format on line " .. ln, 0) end
            local offset = tonumber(oh) * 3600 + tonumber(om) * 60
            if c == "-" then offset = -offset end
            time = time + offset
        elseif c == "Z" then pos = pos + 1 end
        tab[key] = time
        return line, pos, ln
    elseif line:match("^%d%d%d%d%-%d%d%-%d%d", pos) then
        local y, M, d, pos = line:match("^(%d%d%d%d)%-(%d%d)%-(%d%d)()", pos)
        local date = {
            year = tonumber(y),
            month = tonumber(M),
            day = tonumber(d),
            hour = 0,
            min = 0,
            sec = 0
        }
        local time = os.time(date)
        tab[key] = time
        return line, pos, ln
    elseif line:match("^%d%d%:%d%d:%d%d", pos) then
        local h, m, s, pos = line:match("^(%d%d):(%d%d):(%d%d)()", pos)
        local time = h * 3600 + m * 60 * s
        if line:match("^%.%d+", pos) then
            local ss
            ss, pos = line:match("(%.%d+)()", pos)
            time = time + tonumber("0" .. ss)
        end
        tab[key] = time
        return line, pos, ln
    elseif op:match "%d" or op == "+" or op == "-" then
        if line:match("^%+inf", pos) then tab[key] = math.huge return line, pos + 4, ln
        elseif line:match("^%-inf", pos) then tab[key] = -math.huge return line, pos + 4, ln
        elseif line:match("^%+nan", pos) then tab[key] = -(0/0) return line, pos + 4, ln
        elseif line:match("^%-nan", pos) then tab[key] = 0/0 return line, pos + 4, ln
        elseif line:match("^[%+%-]?0o", pos) then
            local sign, num, pos = line:match("^([%+%-]?)0o([0-7_]+)()", pos):gsub("_", "")
            if not num then error("Invalid number on line " .. ln, 0) end
            num = tonumber(num, 8)
            if not num then error("Invalid number on line " .. ln, 0) end
            if sign == "-" then num = -num end
            tab[key] = num
            return line, pos, ln
        elseif line:match ("^[%+%-]?0b", pos) then
            local sign, num, pos = line:match("^([%+%-]?)0b([01_]+)()", pos):gsub("_", "")
            if not num then error("Invalid number on line " .. ln, 0) end
            num = tonumber(num, 2)
            if not num then error("Invalid number on line " .. ln, 0) end
            if sign == "-" then num = -num end
            tab[key] = num
            return line, pos, ln
        else
            local num, pos = line:match("^([%+%-]?[%d_]+%.?[%d_]*[Ee]?[%+%-]?[%d_]*)()", pos)
            num = num:gsub("_", "")
            num = tonumber(num)
            if not num then error("Invalid number on line " .. ln, 0) end
            tab[key] = num
            return line, pos, ln
        end
    elseif line:match("^true", pos) then tab[key] = true return line, pos + 4, ln
    elseif line:match("^false", pos) then tab[key] = false return line, pos + 5, ln
    elseif line:match("^nil", pos) then tab[key] = nil return line, pos + 3, ln -- extension
    elseif line:match("^inf", pos) then tab[key] = math.huge return line, pos + 3, ln
    elseif line:match("^nan", pos) then tab[key] = -(0/0) return line, pos + 3, ln
    else error("Unexpected " .. op .. " on line " .. ln, 0) end
end

--- Parses TOML data into a table.
-- @tparam string str The TOML data to decode
-- @tparam[opt] table opts Options (none available in this version)
-- @treturn table A table representing the TOML data
function toml.decode(str, opts)
    expect(1, str, "string")
    opts = expect(2, opts, "table", "nil") or {}
    local retval = {}
    local current = retval
    local ln = 1
    local coro
    for line in str:gmatch "([^\r\n]*)\r?\n" do
        if coro then
            -- continuation of multi-line value
            local ok, err = coroutine.resume(coro, line)
            if not ok then error(err, 3) end
            if coroutine.status(coro) == "dead" then coro = nil end
        else
            line = line:gsub("^%s+", "")
            if line:match "^#" or line == "" then -- nothing
            elseif line:match "^%[%[" then
                local tag = line:match "^%[(%b[])%]"
                if not tag then error("Expected ]] on line " .. ln, 2) end
                current = traverse(retval, tag:sub(2, -2), 1, ln)
                current[#current+1] = {}
                current = current[#current]
            elseif line:match "^%[" then
                local tag = line:match "^%b[]"
                if not tag then error("Expected ] on line " .. ln, 2) end
                current = traverse(retval, tag:sub(2, -2), 1, ln)
            else
                local last, key, pos = traverse(current, line, 1, ln, true)
                pos = line:match("^%s*()", pos)
                if not pos then error("Expected value on line " .. ln, 2) end
                coro = coroutine.create(toml_assign)
                local ok, err = coroutine.resume(coro, last, key, line, pos, ln)
                if not ok then error(err, 3) end
                if coroutine.status(coro) == "dead" then coro = nil end
            end
        end
        ln = ln + 1
    end
    if coro then error("Unfinished value at end of file", 2) end
    return retval
end

return toml
end
preload["theme"] = function(...)
term.setPaletteColor(2, 0xD06018)
term.setPaletteColor(16, 0xffa300)
end
preload["taskmaster"] = function(...)
---@module taskmaster
--@since 0.0.1

-- Taskmaster: A simple and highly flexible task runner/coroutine manager for ComputerCraft
-- Supports adding/removing tasks, early exits for tasks, event white/blacklists, automatic
-- terminal redirection, task pausing, promises, and more.
-- Made by JackMacWindows
-- Licensed under CC0 in the public domain




--[[
    Examples:

    - Run three functions in parallel, and wait for any to exit.

        require("taskmaster")(
            func1, func2, func3
        ):waitForAny()
    
    - Run three functions in parallel, and wait for all to exit.

        require("taskmaster")(
            func1, func2, func3
        ):waitForAll()

    - Builder-style creation of three event listeners for keyboard events.

        require("taskmaster")()
            :eventListener("key", function(ev, key) print("Key:", keys.getName(key)) end)
            :eventListener("key_up", function(ev, key) print("Key up:", keys.getName(key)) end)
            :eventListener("char", function(ev, char) print("Character:", char) end)
            :run()

    - Create a loop with two background tasks (which don't receive user interaction events) and one foreground task.
      The foreground task may exit itself if a specific character is pressed.

        local loop = require("taskmaster")()
        loop:setEventBlacklist {"key", "key_up", "char", "paste", "mouse_click", "mouse_up", "mouse_scroll", "mouse_drag"}
        loop:addTask(bgFunc)
        loop:addTimer(2, pollingFunction)

        local function fgFunc(task)
            while true do
                local event, p1 = os.pullEvent()
                if event == "char" and p1 == "q" then
                    task:remove()
                end
            end
        end

        local task = loop:addTask(fgFunc)
        task:setEventBlacklist {}
        task:setPriority(10)

        loop:run()
    
    - Fetch a remote JSON resource in parallel using promises.

        local loop = require("taskmaster")()

        local function main()
            loop.Promise.fetch("https://httpbin.org/headers")
                :next(function(handle) return handle.json() end)
                :next(function(data) print(data.headers["User-Agent"]) end)
                :catch(printError)
        end

        loop:task(main):run()
]]

local expect = require "cc.expect"
--#region task
---@type Task
--@field master Taskmaster The event loop for the task
local Task = {}
local Task_mt = {__name = "Task", __index = Task}

--- Pauses the task, preventing it from running. This will yield if the task calls this method on itself.
function Task:pause()
    self.paused = true
    if self.master.currentTask == self then coroutine.yield() end
end

--- Unpauses the task if it was previously paused by @{Task.pause}.
function Task:unpause()
    self.paused = false
end

--- Removes the task from the run loop, as if it returned. This will yield if the task calls this method on itself.
function Task:remove()
    self.master.dead[#self.master.dead+1] = self
    self.paused = true
    if self.master.currentTask == self then coroutine.yield() end
end

--- Sets the priority of the task. This determines the order tasks are run in.
--@param priority number The priority of the task (0 is the default)
function Task:setPriority(priority)
    expect(1, priority, "number")
    self.priority = priority
    self.master.shouldSort = true
end

--- Sets a blacklist for events to send to this task.
--@param list? string[] A list of events to not send to this task
function Task:setEventBlacklist(list)
    if expect(1, list, "table", "nil") then
        self.blacklist = {}
        for _, v in ipairs(list) do self.blacklist[v] = true end
    else self.blacklist = nil end
end

--- Sets a whitelist for events to send to this task.
--@param list? string[] A list of events to send to this task (others are discarded)
function Task:setEventWhitelist(list)
    if expect(1, list, "table", "nil") then
        self.whitelist = {}
        for _, v in ipairs(list) do self.whitelist[v] = true end
    else self.whitelist = nil end
end

--- Sets an error handler for a task.
--@param errh? fun(err: any, task: Task) A function to call if the task throws an error
function Task:setErrorHandler(errh)
    self.errh = expect(1, errh, "function", "nil")
end
--#endregion


--#region Promise 
---@type Promise
--@field task Task
--@field resolve fun(...: any)|nil
--@field reject fun(err: any)|nil
--@field final fun()|nil
local Promise = {}
local Promise_mt = {__name = "Promise", __index = Promise}

--- Creates a new Promise on the selected run loop.
--@param loop Taskmaster The loop to create the promise on
--@param fn fun(resolve: fun(...: any), reject: fun(err: any)) The main function for the promise
--@return Promise promise The new promise
function Promise:new(loop, fn)
    expect(1, loop, "table")
    expect(2, fn, "function")
    local obj = setmetatable({}, Promise_mt)
    obj.task = loop:addTask(function()
        local ok, err = pcall(fn,
            function(...) if obj.resolve then return obj.resolve(...) end end,
            function(err)
                while obj do
                    if obj.reject then return obj.reject(err) end
                    obj = obj.next_promise
                end
            end
        )
        if not ok and obj.reject then obj.reject(err) end
    end)
    return obj
end

--- Creates a new Promise that resolves once all of the listed promises resolve.
--@param loop Taskmaster The loop to create the promise on
--@param list Promise[] The promises to wait for
--@return Promise promise The new promise
function Promise:all(loop, list)
    expect(1, loop, "table")
    expect(2, list, "table")
    return Promise:new(loop, function(resolve, reject)
        local count = 0
        for _, v in ipairs(list) do
            v:next(function(...)
                count = count + 1
                if count == #list then resolve(...) end
            end, reject)
        end
    end)
end


--- Creates a new Promise that resolves once any of the listed promises resolve, or rejects if all promises reject.
--@param loop Taskmaster The loop to create the promise on
--@param list Promise[] The promises to wait for
--@return Promise promise The new promise
function Promise:any(loop, list)
    expect(1, loop, "table")
    expect(2, list, "table")
    return Promise:new(loop, function(resolve, reject)
        local count = 0
        for _, v in ipairs(list) do
            v:next(resolve, function(err)
                count = count + 1
                if count == #list then reject(err) end
            end)
        end
    end)
end

--- Creates a new Promise that resolves once any of the listed promises resolve.
--@param loop Taskmaster The loop to create the promise on
--@param list Promise[] The promises to wait for
--@return Promise promise The new promise
function Promise:race(loop, list)
    expect(1, loop, "table")
    expect(2, list, "table")
    return Promise:new(loop, function(resolve, reject)
        for _, v in ipairs(list) do v:next(resolve, reject) end
    end)
end

--- Creates a new Promise that immediately resolves to a value.
--@param loop Taskmaster The loop to create the promise on
--@param val any The value to resolve to
--@return Promise promise The new promise
function Promise:_resolve(loop, val)
    expect(1, loop, "table")
    local obj = setmetatable({}, Promise_mt)
    obj.task = loop:addTask(function()
        if obj.resolve then obj.resolve(val) end
    end)
    return obj
end

--- Creates a new Promise that immediately rejects with an error.
--@param loop Taskmaster The loop to create the promise on
--@param err any The value to resolve to
--@return Promise promise The new promise
function Promise:_reject(loop, err)
    expect(1, loop, "table")
    local obj = setmetatable({}, Promise_mt)
    obj.task = loop:addTask(function()
        if obj.reject then obj.reject(err) end
    end)
    return obj
end

--- Adds a function to call when the promise resolves.
--@param fn fun(...: any): Promise|nil The function to call
--@param err? fun(err: any) A function to catch errors
--@return Promise next The next promise in the chain
function Promise:next(fn, err)
    expect(1, fn, "function")
    expect(2, err, "function", "nil")
    self.resolve = function(...)
        self.resolve = nil
        local res = fn(...)
        if self.next_promise then
            if type(res) == "table" and getmetatable(res) == Promise_mt then
                for k, v in pairs(self.next_promise) do res[k] = v end
                self.next_promise = res
            else
                self.next_promise.resolve(res)
            end
        end
        if self.final then self.final() end
    end
    if err then self.reject = function(v) self.reject = nil err(v) if self.final then self.final() end end end
    self.next_promise = setmetatable({}, Promise_mt)
    return self.next_promise
end
Promise.Then = Promise.next

--- Sets the error handler for the promise.
--@param fn fun(err: any) The error handler to use
--@return Promise self
function Promise:catch(fn)
    expect(1, fn, "function")
    self.reject = function(err) self.reject = nil fn(err) if self.final then self.final() end end
    return self
end

--- Sets a function to call after the promise settles.
--@param fn fun() The function to call
--@return Promise self
function Promise:finally(fn)
    expect(1, fn, "function")
    self.final = function() self.final = nil return fn() end
    return self
end

---@diagnostic disable: missing-return

---@class PromiseConstructor
local PromiseConstructor = {}

--- Creates a new Promise on the selected run loop.
--@param fn fun(resolve: fun(...: any), reject: fun(err: any)) The main function for the promise
--@return Promise promise The new promise
function PromiseConstructor.new(fn) end

--- Creates a new Promise that resolves once all of the listed promises resolve.
--@param list Promise[] The promises to wait for
--@return Promise promise The new promise
function PromiseConstructor.all(list) end

--- Creates a new Promise that resolves once any of the listed promises resolve, or rejects if all promises reject.
--@param list Promise[] The promises to wait for
--@return Promise promise The new promise
function PromiseConstructor.any(list) end

--- Creates a new Promise that resolves once any of the listed promises resolve.
--@param list Promise[] The promises to wait for
--@return Promise promise The new promise
function PromiseConstructor.race(list) end

--- Creates a new Promise that immediately resolves to a value.
--@param val any The value to resolve to
--@return Promise promise The new promise
function PromiseConstructor.resolve(val) end

--- Creates a new Promise that immediately rejects with an error.
--@param err any The value to resolve to
--@return Promise promise The new promise
function PromiseConstructor.reject(err) end

--- Makes an HTTP request to a URL, and returns a Promise for the result.
-- The promise will resolve with the handle to the response, which will also
-- have the following methods:
-- - res.text(): Returns a promise that resolves to the body of the response.
-- - res.table(): Returns a promise that resolves to the body unserialized as a Lua table.
-- - res.json(): Returns a promise that resolves to the body unserialized as JSON.
--@param url string The URL to connect to
--@param body? string If specified, a POST body to send
--@param headers? table<string, string> Any HTTP headers to add to the request
--@param binary? boolean Whether to send in binary mode (deprecated as of CC:T 1.109.0)
--@overload fun(options: {url: string, body?: string, headers?: string, method?: string, binary?: string, timeout?: number}): Promise
--@return Promise promise The new promise
function PromiseConstructor.fetch(url, body, headers, binary) end

---@diagnostic enable: missing-return
--#endregion

--#region Taskmaster

---@type Taskmaster
--@field Promise PromiseConstructor
local Taskmaster = {}
local Taskmaster_mt = {__name = "Taskmaster", __index = Taskmaster}

--- Adds a task to the loop.
--@param fn fun(Task) The main function to add, which receives the task as an argument
--@return Task task The created task
function Taskmaster:addTask(fn)
    expect(1, fn, "function")
    local task = setmetatable({coro = coroutine.create(fn), master = self, priority = 0}, Task_mt)
    self.new[#self.new+1] = task
    self.shouldSort = true
    return task
end

--- Adds a task to the loop in builder style.
--@param fn fun(Task) The main function to add
--@return Taskmaster self
function Taskmaster:task(fn) self:addTask(fn) return self end

--- Adds a function to the loop. This is just like a task, but allows extra arguments.
--@param fn function The main function to add, which receives the arguments passed
--@param ... any Any arguments to pass to the function
--@return Task task The created task
function Taskmaster:addFunction(fn, ...)
    expect(1, fn, "function")
    local args = table.pack(...)
    local task = setmetatable({coro = coroutine.create(function() return fn(table.unpack(args, 1, args.n)) end), master = self, priority = 0}, Task_mt)
    self.new[#self.new+1] = task
    self.shouldSort = true
    return task
end

--- Adds a function to the loop in builder style.
--@param fn function The main function to add
--@param ... any Any arguments to pass to the function
--@return Taskmaster self
function Taskmaster:func(fn, ...) self:addFunction(fn, ...) return self end

--- Adds an event listener to the loop. This is a special task that calls a function whenever an event is triggered.
--@param name string The name of the event to listen for
--@param fn fun(string, ...) The function to call for each event
--@return Task task The created task
function Taskmaster:addEventListener(name, fn)
    expect(1, name, "string")
    expect(2, fn, "function")
    local task = setmetatable({coro = coroutine.create(function() while true do fn(os.pullEvent(name)) end end), master = self, priority = 0}, Task_mt)
    self.new[#self.new+1] = task
    self.shouldSort = true
    return task
end

--- Adds an event listener to the loop in builder style. This is a special task that calls a function whenever an event is triggered.
--@param name string The name of the event to listen for
--@param fn fun(string, ...) The function to call for each event
--@return Taskmaster self
function Taskmaster:eventListener(name, fn) self:addEventListener(name, fn) return self end

--- Adds a task that triggers a function repeatedly after an interval. The function may modify or cancel the interval through a return value.
--@param timeout number The initial interval to run the function after
--@param fn fun():number|nil The function to call.
--If this returns a number, that number replaces the timeout.
--If this returns a number less than or equal to 0, the timer is canceled.
--If this returns nil, the timeout remains the same.
--@return Task task The created task
function Taskmaster:addTimer(timeout, fn)
    expect(1, timeout, "number")
    expect(2, fn, "function")
    local task = setmetatable({coro = coroutine.create(function()
        while true do
            sleep(timeout)
            timeout = fn() or timeout
            if timeout <= 0 then return end
        end
    end), master = self, priority = 0}, Task_mt)
    self.new[#self.new+1] = task
    self.shouldSort = true
    return task
end

--- Adds a task that triggers a function repeatedly after an interval in builder style. The function may modify or cancel the interval through a return value.
--@param timeout number The initial interval to run the function after
--@param fn fun():number|nil The function to call.
--If this returns a number, that number replaces the timeout.
--If this returns a number less than or equal to 0, the timer is canceled.
--If this returns nil, the timeout remains the same.
--@return Taskmaster self
function Taskmaster:timer(timeout, fn) self:addTimer(timeout, fn) return self end

--- Sets a blacklist for events to send to all tasks. Tasks can override this with their own blacklist.
---@param list? string[] A list of events to not send to any task
function Taskmaster:setEventBlacklist(list)
    if expect(1, list, "table", "nil") then
        self.blacklist = {}
        for _, v in ipairs(list) do self.blacklist[v] = true end
    else self.blacklist = nil end
end

--- Sets a whitelist for events to send to all tasks. Tasks can override this with their own whitelist.
--@param list? string[] A list of events to send to all tasks (others are discarded)
function Taskmaster:setEventWhitelist(list)
    if expect(1, list, "table", "nil") then
        self.whitelist = {}
        for _, v in ipairs(list) do self.whitelist[v] = true end
    else self.whitelist = nil end
end

--- Sets a function that is used to transform events. This function takes a task
-- and event table, and may modify the event table to adjust the event for that task.
--@param fn fun(Task, table)|nil A function to use to transform events
function Taskmaster:setEventTransformer(fn)
    expect(1, fn, "function", "nil")
    self.transformer = fn
end

--- Sets a function to call before yielding. This can be used to reset state such
-- as terminal cursor position.
--@param fn? fun() The function to call
function Taskmaster:setPreYieldHook(fn)
    expect(1, fn, "function", "nil")
    self.preYieldHook = fn
end

--- Runs the main loop, processing events and running each task.
--@param count? number The number of tasks that can exit before stopping the loop
function Taskmaster:run(count)
    count = expect(1, count, "number", "nil") or math.huge
    self.running = true
    while self.running and (#self.tasks + #self.new) > 0 and count > 0 do
        self.dead = {}
        for i, task in ipairs(self.new) do
            self.currentTask = task
            local old = term.current()
            local ok, filter = coroutine.resume(task.coro, task)
            task.window = term.redirect(old)
            if not ok then
                self.currentTask = nil
                self.running = false
                self.new = {table.unpack(self.new, i + 1)}
                return error(filter, 0)
            end
            task.filter = filter
            if coroutine.status(task.coro) == "dead" then count = count - 1
            else self.tasks[#self.tasks+1], self.shouldSort = task, true end
            if not self.running or count <= 0 then break end
        end
        self.new = {}
        if self.shouldSort then table.sort(self.tasks, function(a, b) return a.priority > b.priority end) self.shouldSort = false end
        if self.running and #self.tasks > 0 and count > 0 then
            if self.preYieldHook then self.preYieldHook() end
            local _ev = table.pack(os.pullEventRaw())
            for i, task in ipairs(self.tasks) do
                local ev = _ev
                if self.transformer then
                    ev = table.pack(table.unpack(_ev, 1, _ev.n))
                    self.transformer(task, ev)
                end
                local wl, bl = task.whitelist or self.whitelist, task.blacklist or self.blacklist
                if not task.paused and
                    (task.filter == nil or task.filter == ev[1] or ev[1] == "terminate") and
                    (not bl or not bl[ev[1]]) and
                    (not wl or wl[ev[1]]) then
                    self.currentTask = task
                    local old = term.redirect(task.window)
                    local ok, filter = coroutine.resume(task.coro, table.unpack(ev, 1, ev.n))
                    task.window = term.redirect(old)
                    if not ok then
                        if task.errh then
                            task.errh(filter, task)
                        else
                            self.currentTask = nil
                            self.running = false
                            table.remove(self.tasks, i)
                            return error(filter, 0)
                        end
                    end
                    task.filter = filter
                    if coroutine.status(task.coro) == "dead" then self.dead[#self.dead+1] = task end
                    if not self.running or #self.dead >= count then break end
                end
            end
        end
        self.currentTask = nil
        for _, task in ipairs(self.dead) do
            for i, v in ipairs(self.tasks) do
                if v == task then
                    table.remove(self.tasks, i)
                    count = count - 1
                    break
                end
            end
        end
    end
    self.running = false
end

--- Runs all tasks until a single task exits.
function Taskmaster:waitForAny() return self:run(1) end
--- Runs all tasks until all tasks exit.
function Taskmaster:waitForAll() return self:run() end

--- Stops the main loop if it is running. This will yield if called from a running task.
function Taskmaster:stop()
    self.running = false
    if self.currentTask then coroutine.yield() end
end

Taskmaster_mt.__call = Taskmaster.run

--#endregion

local function fetch(loop, url, ...)
    local ok, err = http.request(url, ...)
    if not ok then return Promise:_reject(loop, err) end
    return loop.Promise.new(function(resolve, reject)
        while true do
            local event, p1, p2, p3 = os.pullEvent()
            if event == "http_success" and p1 == url then
                p2.text = function()
                    return loop.Promise.new(function(_resolve, _reject)
                        local data = p2.readAll()
                        p2.close()
                        _resolve(data)
                    end)
                end
                p2.json = function()
                    return loop.Promise.new(function(_resolve, _reject)
                        local data = p2.readAll()
                        p2.close()
                        local d = textutils.unserializeJSON(data)
                        if d ~= nil then _resolve(d)
                        else _reject("Failed to parse JSON") end
                    end)
                end
                p2.table = function()
                    return loop.Promise.new(function(_resolve, _reject)
                        local data = p2.readAll()
                        p2.close()
                        local d = textutils.unserialize(data)
                        if d ~= nil then _resolve(d)
                        else _reject("Failed to parse Lua table") end
                    end)
                end
                return resolve(p2)
            elseif event == "http_failure" and p1 == url then
                if p3 then p3.close() end
                return reject(p2)
            end
        end
    end)
end

--- Creates a new Taskmaster run loop.
--@param ... fun() Any tasks to add to the loop
--@return Taskmaster loop The new Taskmaster
return function(...)
    local loop = setmetatable({tasks = {}, dead = {}, new = {}}, Taskmaster_mt)
    for i, v in ipairs{...} do
        expect(i, v, "function")
        loop:addTask(v)
    end
    loop.Promise = {
        new = function(fn) return Promise:new(loop, fn) end,
        all = function(list) return Promise:all(loop, list) end,
        any = function(list) return Promise:any(loop, list) end,
        race = function(list) return Promise:race(loop, list) end,
        resolve = function(val) return Promise:_resolve(loop, val) end,
        reject = function(err) return Promise:_reject(loop, err) end,
        fetch = function(...) return fetch(loop, ...) end
    }
    setmetatable(loop.Promise, {__call = function(self, ...) return Promise:new(loop, ...) end})
    return loop
end

end
preload["services"] = function(...)
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
end
preload["primeUI"] = function(...)
---@module PrimeUI
--@since 0.0.1

local expect = require "cc.expect".expect

-- Initialization code



-- UI lib

local PrimeUI = {}

do
    local coros = {}
    local restoreCursor

    --- Adds a task to run in the main loop.
    --@param func function The function to run, usually an `os.pullEvent` loop
    function PrimeUI.addTask(func)
        expect(1, func, "function")
        local t = {coro = coroutine.create(func)}
        coros[#coros+1] = t
        _, t.filter = coroutine.resume(t.coro)
    end

    --- Sends the provided arguments to the run loop, where they will be returned.
    --@param ... any The parameters to send
    function PrimeUI.resolve(...)
        coroutine.yield(coros, ...)
    end

    --- Clears the screen and resets all components. Do not use any previously
    -- created components after calling this function.
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
    --@param win window|nil The window to set as the active window
    function PrimeUI.setCursorWindow(win)
        expect(1, win, "table", "nil")
        restoreCursor = win and win.restoreCursor
    end

    --- Gets the absolute position of a coordinate relative to a window.
    --@param win window The window to check
    --@param x number The relative X position of the point
    --@param y number The relative Y position of the point
    --@return number x The absolute X position of the window
    --@return number y The absolute Y position of the window
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
    --@return any ... The result of the coroutine that exited
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
--@param win window The window to draw on
--@param x number The X position of the left side of the text
--@param y number The Y position of the text
--@param text string The text to draw
--@param fgColor color|nil The color of the text (defaults to white)
--@param bgColor color|nil The color of the background (defaults to black)
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
-- and does not handle animation on its own (but the index parameter may be
-- used by apps to implement animation).
--@param win window The window to draw on
--@param x number The X position of the top left corner of the image
--@param y number The Y position of the top left corner of the image
--@param data string|table The path to the image to load, or the image data itself
--@param index number|nil The index of the frame to draw (defaults to 1)
--@param setPalette boolean|nil Whether to set the palette if the image contains one (defaults to true)
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
end
preload["pfunc"] = function(...)
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
end
preload["logs"] = function(...)
---@module logs
--@since 0.0.1

local log = {}

--- Log levels
log.levels = {
    DEBUG    = 1,
    VERBOSE  = 2,
    INFO     = 3,
    NOTICE   = 4,
    WARN     = 5,
    CRITICAL = 6,
}
---@see log.levels
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

---creates a logger
--@param id any
--@return table
function log.make(id)
    local logger = {}
    function logger.log(lvl,...)
        local c = term.getTextColor()
        term.setTextColor(cols[lvl])
        print(string.format("[%3s][%6s][%8s]",id,"",log.names[lvl]),...)
        term.setTextColor(c)
    end
    function logger.logThrd(thrd,lvl,...)
        local c = term.getTextColor()
        term.setTextColor(cols[lvl])
        print(string.format("[%3s][%6s][%8s]",id,thrd,log.names[lvl]),...)
        term.setTextColor(c)
    end
    return logger
end
return log


end
preload["logo"] = function(...)
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
end
preload["kernel"] = function(...)
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
---@module kernel
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

io = require("io")
local userspace = require("userspace")
userspace.update(_G)

local taskmaster = require("taskmaster")()

function kernel.run(name,func)
    taskmaster:addTask(function(Task)
        TASKS[name] = Task
        setfenv(func,userspace)
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
    kernel.run("cash",loadfile("bin/cash.lua"))
end)

syslog.log(log.levels.INFO,"Starting Threadding")
taskmaster:run()
read()
kernel.panic("CRITICAL system process died")
end
preload["io"] = function(...)
-- SPDX-FileCopyrightText: 2017 Daniel Ratcliffe
--
-- SPDX-License-Identifier: LicenseRef-CCPL

--- Emulates Lua's standard [io library][io].
--
-- [io]: https://www.lua.org/manual/5.1/manual.html#5.7
--
-- @module io

local io = {}

local expect, type_of = dofile("rom/modules/main/cc/expect.lua").expect, _G.type

--- If we return nil then close the file, as we've reached the end.
-- We use this weird wrapper function as we wish to preserve the varargs
local function checkResult(handle, ...)
    if ... == nil and handle._autoclose and not handle._closed then handle:close() end
    return ...
end

--- A file handle which can be read or written to.
--
-- @type Handle
local handleMetatable
handleMetatable = {
    __name = "FILE*",
    __tostring = function(self)
        if self._closed then
            return "file (closed)"
        else
            local hash = tostring(self._handle):match("table: (%x+)")
            return "file (" .. hash .. ")"
        end
    end,

    __index = {
        --- Close this file handle, freeing any resources it uses.
        --
        -- @treturn[1] true If this handle was successfully closed.
        -- @treturn[2] nil If this file handle could not be closed.
        -- @treturn[2] string The reason it could not be closed.
        -- @throws If this handle was already closed.
        close = function(self)
            if type_of(self) ~= "table" or getmetatable(self) ~= handleMetatable then
                error("bad argument #1 (FILE expected, got " .. type_of(self) .. ")", 2)
            end
            if self._closed then error("attempt to use a closed file", 2) end

            local handle = self._handle
            if handle.close then
                self._closed = true
                handle.close()
                return true
            else
                return nil, "attempt to close standard stream"
            end
        end,

        --- Flush any buffered output, forcing it to be written to the file
        --
        -- @throws If the handle has been closed
        flush = function(self)
            if type_of(self) ~= "table" or getmetatable(self) ~= handleMetatable then
                error("bad argument #1 (FILE expected, got " .. type_of(self) .. ")", 2)
            end
            if self._closed then error("attempt to use a closed file", 2) end

            local handle = self._handle
            if handle.flush then handle.flush() end
            return true
        end,

        --[[- Returns an iterator that, each time it is called, returns a new
        line from the file.

        This can be used in a for loop to iterate over all lines of a file

        Once the end of the file has been reached, @{nil} will be returned. The file is
        *not* automatically closed.

        @param ... The argument to pass to @{Handle:read} for each line.
        @treturn function():string|nil The line iterator.
        @throws If the file cannot be opened for reading
        @since 1.3

        @see io.lines
        @usage Iterate over every line in a file and print it out.

        ```lua
        local file = io.open("/rom/help/intro.txt")
        for line in file:lines() do
          print(line)
        end
        file:close()
        ```
        ]]
        lines = function(self, ...)
            if type_of(self) ~= "table" or getmetatable(self) ~= handleMetatable then
                error("bad argument #1 (FILE expected, got " .. type_of(self) .. ")", 2)
            end
            if self._closed then error("attempt to use a closed file", 2) end

            local handle = self._handle
            if not handle.read then return nil, "file is not readable" end

            local args = table.pack(...)
            return function()
                if self._closed then error("file is already closed", 2) end
                return checkResult(self, self:read(table.unpack(args, 1, args.n)))
            end
        end,

        --[[- Reads data from the file, using the specified formats. For each
        format provided, the function returns either the data read, or `nil` if
        no data could be read.

        The following formats are available:
        - `l`: Returns the next line (without a newline on the end).
        - `L`: Returns the next line (with a newline on the end).
        - `a`: Returns the entire rest of the file.
        - ~~`n`: Returns a number~~ (not implemented in CC).

        These formats can be preceded by a `*` to make it compatible with Lua 5.1.

        If no format is provided, `l` is assumed.

        @param ... The formats to use.
        @treturn (string|nil)... The data read from the file.
        ]]
        read = function(self, ...)
            if type_of(self) ~= "table" or getmetatable(self) ~= handleMetatable then
                error("bad argument #1 (FILE expected, got " .. type_of(self) .. ")", 2)
            end
            if self._closed then error("attempt to use a closed file", 2) end

            local handle = self._handle
            if not handle.read and not handle.readLine then return nil, "Not opened for reading" end

            local n = select("#", ...)
            local output = {}
            for i = 1, n do
                local arg = select(i, ...)
                local res
                if type_of(arg) == "number" then
                    if handle.read then res = handle.read(arg) end
                elseif type_of(arg) == "string" then
                    local format = arg:gsub("^%*", ""):sub(1, 1)

                    if format == "l" then
                        if handle.readLine then res = handle.readLine() end
                    elseif format == "L" and handle.readLine then
                        if handle.readLine then res = handle.readLine(true) end
                    elseif format == "a" then
                        if handle.readAll then res = handle.readAll() or "" end
                    elseif format == "n" then
                        res = nil -- Skip this format as we can't really handle it
                    else
                        error("bad argument #" .. i .. " (invalid format)", 2)
                    end
                else
                    error("bad argument #" .. i .. " (string expected, got " .. type_of(arg) .. ")", 2)
                end

                output[i] = res
                if not res then break end
            end

            -- Default to "l" if possible
            if n == 0 and handle.readLine then return handle.readLine() end
            return table.unpack(output, 1, n)
        end,

        --[[- Seeks the file cursor to the specified position, and returns the
        new position.

        `whence` controls where the seek operation starts, and is a string that
        may be one of these three values:
        - `set`: base position is 0 (beginning of the file)
        - `cur`: base is current position
        - `end`: base is end of file

        The default value of `whence` is `cur`, and the default value of `offset`
        is 0. This means that `file:seek()` without arguments returns the current
        position without moving.

        @tparam[opt] string whence The place to set the cursor from.
        @tparam[opt] number offset The offset from the start to move to.
        @treturn number The new location of the file cursor.
        ]]
        seek = function(self, whence, offset)
            if type_of(self) ~= "table" or getmetatable(self) ~= handleMetatable then
                error("bad argument #1 (FILE expected, got " .. type_of(self) .. ")", 2)
            end
            if self._closed then error("attempt to use a closed file", 2) end

            local handle = self._handle
            if not handle.seek then return nil, "file is not seekable" end

            -- It's a tail call, so error positions are preserved
            return handle.seek(whence, offset)
        end,

        --[[- Sets the buffering mode for an output file.

        This has no effect under ComputerCraft, and exists with compatility
        with base Lua.
        @tparam string mode The buffering mode.
        @tparam[opt] number size The size of the buffer.
        @see file:setvbuf Lua's documentation for `setvbuf`.
        @deprecated This has no effect in CC.
        ]]
        setvbuf = function(self, mode, size) end,

        --- Write one or more values to the file
        --
        -- @tparam string|number ... The values to write.
        -- @treturn[1] Handle The current file, allowing chained calls.
        -- @treturn[2] nil If the file could not be written to.
        -- @treturn[2] string The error message which occurred while writing.
        -- @changed 1.81.0 Multiple arguments are now allowed.
        write = function(self, ...)
            if type_of(self) ~= "table" or getmetatable(self) ~= handleMetatable then
                error("bad argument #1 (FILE expected, got " .. type_of(self) .. ")", 2)
            end
            if self._closed then error("attempt to use a closed file", 2) end

            local handle = self._handle
            if not handle.write then return nil, "file is not writable" end

            for i = 1, select("#", ...) do
                local arg = select(i, ...)
                expect(i, arg, "string", "number")
                handle.write(arg)
            end
            return self
        end,
    },
}

local function make_file(handle)
    return setmetatable({ _handle = handle }, handleMetatable)
end

local defaultInput = make_file({ readLine = _G.read })

local defaultOutput = make_file({ write = _G.write })

local defaultError = make_file({
    write = function(...)
        local oldColour
        if term.isColour() then
            oldColour = term.getTextColour()
            term.setTextColour(colors.red)
        end
        _G.write(...)
        if term.isColour() then term.setTextColour(oldColour) end
    end,
})

local currentInput = defaultInput
local currentOutput = defaultOutput

--- A file handle representing the "standard input". Reading from this
-- file will prompt the user for input.
io.stdin = defaultInput

--- A file handle representing the "standard output". Writing to this
-- file will display the written text to the screen.
io.stdout = defaultOutput

--- A file handle representing the "standard error" stream.
--
-- One may use this to display error messages, writing to it will display
-- them on the terminal.
io.stderr = defaultError

--- Closes the provided file handle.
--
-- @tparam[opt] Handle file The file handle to close, defaults to the
-- current output file.
--
-- @see Handle:close
-- @see io.output
-- @since 1.55
function io.close(file)
    if file == nil then return currentOutput:close() end

    if type_of(file) ~= "table" or getmetatable(file) ~= handleMetatable then
        error("bad argument #1 (FILE expected, got " .. type_of(file) .. ")", 2)
    end
    return file:close()
end

--- Flushes the current output file.
--
-- @see Handle:flush
-- @see io.output
-- @since 1.55
function io.flush()
    return currentOutput:flush()
end

--- Get or set the current input file.
--
-- @tparam[opt] Handle|string file The new input file, either as a file path or pre-existing handle.
-- @treturn Handle The current input file.
-- @throws If the provided filename cannot be opened for reading.
-- @since 1.55
function io.input(file)
    if type_of(file) == "string" then
        local res, err = open(file, "r")
        if not res then error(err, 2) end
        currentInput = res
    elseif type_of(file) == "table" and getmetatable(file) == handleMetatable then
        currentInput = file
    elseif file ~= nil then
        error("bad fileument #1 (FILE expected, got " .. type_of(file) .. ")", 2)
    end

    return currentInput
end

--[[- Opens the given file name in read mode and returns an iterator that,
each time it is called, returns a new line from the file.

This can be used in a for loop to iterate over all lines of a file

Once the end of the file has been reached, @{nil} will be returned. The file is
automatically closed.

If no file name is given, the @{io.input|current input} will be used instead.
In this case, the handle is not used.

@tparam[opt] string filename The name of the file to extract lines from
@param ... The argument to pass to @{Handle:read} for each line.
@treturn function():string|nil The line iterator.
@throws If the file cannot be opened for reading

@see Handle:lines
@see io.input
@since 1.55
@usage Iterate over every line in a file and print it out.

```lua
for line in io.lines("/rom/help/intro.txt") do
  print(line)
end
```
]]
function io.lines(filename, ...)
    expect(1, filename, "string", "nil")
    if filename then
        local ok, err = open(filename, "r")
        if not ok then error(err, 2) end

        -- We set this magic flag to mark this file as being opened by io.lines and so should be
        -- closed automatically
        ok._autoclose = true
        return ok:lines(...)
    else
        return currentInput:lines(...)
    end
end

--- Open a file with the given mode, either returning a new file handle
-- or @{nil}, plus an error message.
--
-- The `mode` string can be any of the following:
--  - **"r"**: Read mode
--  - **"w"**: Write mode
--  - **"a"**: Append mode
--
-- The mode may also have a `b` at the end, which opens the file in "binary
-- mode". This allows you to read binary files, as well as seek within a file.
--
-- @tparam string filename The name of the file to open.
-- @tparam[opt] string mode The mode to open the file with. This defaults to `rb`.
-- @treturn[1] Handle The opened file.
-- @treturn[2] nil In case of an error.
-- @treturn[2] string The reason the file could not be opened.
function io.open(filename, mode)
    expect(1, filename, "string")
    expect(2, mode, "string", "nil")

    local sMode = mode and mode:gsub("%+", "") or "r"
    local file, err = fs.open(filename, sMode)
    if not file then return nil, err end

    return make_file(file)
end

--- Get or set the current output file.
--
-- @tparam[opt] Handle|string file The new output file, either as a file path or pre-existing handle.
-- @treturn Handle The current output file.
-- @throws If the provided filename cannot be opened for writing.
-- @since 1.55
function io.output(file)
    if type_of(file) == "string" then
        local res, err = open(file, "wb")
        if not res then error(err, 2) end
        currentOutput = res
    elseif type_of(file) == "table" and getmetatable(file) == handleMetatable then
        currentOutput = file
    elseif file ~= nil then
        error("bad argument #1 (FILE expected, got " .. type_of(file) .. ")", 2)
    end

    return currentOutput
end

--- Read from the currently opened input file.
--
-- This is equivalent to `io.input():read(...)`. See @{Handle:read|the
-- documentation} there for full details.
--
-- @tparam string ... The formats to read, defaulting to a whole line.
-- @treturn (string|nil)... The data read, or @{nil} if nothing can be read.
function io.read(...)
    return currentInput:read(...)
end

--- Checks whether `handle` is a given file handle, and determine if it is open
-- or not.
--
-- @param obj The value to check
-- @treturn string|nil `"file"` if this is an open file, `"closed file"` if it
-- is a closed file handle, or `nil` if not a file handle.
function io.type(obj)
    if type_of(obj) == "table" and getmetatable(obj) == handleMetatable then
        if obj._closed then
            return "closed file"
        else
            return "file"
        end
    end
    return nil
end

--- Write to the currently opened output file.
--
-- This is equivalent to `io.output():write(...)`. See @{Handle:write|the
-- documentation} there for full details.
--
-- @tparam string ... The strings to write
-- @changed 1.81.0 Multiple arguments are now allowed.
function io.write(...)
    return currentOutput:write(...)
end

return io
end
preload["ar"] = function(...)
-- Unix ar archive library & program
-- Use in the shell or with require

local function trim(s) return string.match(s, '^()%s*$') and '' or string.match(s, '^%s*(.*%S)') end
local function u2cc(p) return bit.band(p, 0x1) * 8 + bit.band(p, 0x2) + bit.band(p, 0x4) / 4 + 4 end
local function cc2u(p) return bit.band(p, 0x8) / 8 + bit.band(p, 0x2) + bit.band(p, 0x1) * 4 end
local function pad(str, len, c) return string.len(str) < len and string.sub(str, 1, len) .. string.rep(c or " ", len - string.len(str)) or str end
local verbosity = 0

local ar = {}

-- Loads an archive into a table
function ar.load(path)
    if not fs.exists(path) then return nil end
    local file = fs.open(path, "rb")
    local oldread = file.read
    local seek = 0
    file.read = function(c) if c then
        local retval = nil
        if c < 1 then c = 1 end
        if file.seek then retval = oldread(c) else
            for i = 1, c do
                local n = oldread()
                if n == nil then return retval end
                retval = (retval or "") .. string.char(n)
                if (seek + i) % 102400 == 0 then os.queueEvent(os.pullEvent()) end
            end
        end
        seek = seek + c
        return retval
    else return string.char(oldread()) end end
    if file.read(8) ~= "!<arch>\n" then
        file.close()
        error("Not an ar archive", 2)
    end
    local retval = {}
    local name_table = nil
    local name_rep = {}
    os.queueEvent("nosleep")
    while true do
        local data = {}
        local first_c = file.read()
        while first_c == "\n" do first_c = file.read() end
        if first_c == nil then break end
        local name = file.read(15)
        if name == nil then break end
        name = first_c .. name
        if string.find(name, "/") and string.find(name, "/") > 1 then name = string.sub(name, 1, string.find(name, "/") - 1)
        else name = trim(name) end
        data.timestamp = tonumber(trim(file.read(12)))
        data.owner = tonumber(trim(file.read(6)))
        data.group = tonumber(trim(file.read(6)))
        data.mode = tonumber(trim(file.read(8)), 8)
        local size = tonumber(trim(file.read(10)))
        if file.read(2) ~= "`\n" then error("Invalid header for file " .. name, 2) end
        if string.match(name, "^#1/%d+$") then name = file.read(tonumber(string.match(name, "#1/(%d+)"))) 
        elseif string.match(name, "^/%d+$") then if name_table then 
            local n = tonumber(string.match(name, "/(%d+)"))
            name = string.sub(name_table, n+1, string.find(name_table, "\n", n+1) - 1)
        else table.insert(name_rep, name) end end
        data.name = name
        data.data = file.read(size)
        if name == "//" then name_table = data.data
        elseif name ~= "/" and name ~= "/SYM64/" then table.insert(retval, data) end
        os.queueEvent(os.pullEvent())
    end
    file.close()
    if name_table then for k,v in pairs(name_rep) do
        local n = tonumber(string.match(v, "/(%d+)"))
        for l,w in pairs(retval) do if w.name == v then w.name = string.sub(name_table, n, string.find(name_table, "/", n) - 1); break end end
    end end
    return retval
end

-- Writes a table entry to a file
function ar.write(v, p)
    local file = fs.open(p, "wb")
    for s in string.gmatch(v.data, ".") do file.write(string.byte(s)) end
    file.close()
    if fs.setPermissions and v.owner ~= 0 then
        fs.setPermissions(p, v.owner, u2cc(v.mode) + bit.band(v.mode, 0x800) / 0x80)
        fs.setPermissions(p, "*", u2cc(bit.brshift(v.mode, 6)) + bit.band(v.mode, 0x800) / 0x80)
        fs.setOwner(p, v.owner)
    end
    if verbosity > 0 then print("Extracted to " .. p) end
end

-- Extracts files from a table or file to a directory
function ar.extract(data, path)
    if type(data) == "string" then data = ar.load(data) end
    if not fs.exists(path) then fs.makeDir(path) end
    for k,v in pairs(data) do
        local p = fs.combine(path, v.name)
        ar.write(v, p)
    end
end

-- Reads a file into a table entry
function ar.read(p)
    local file = fs.open(p, "rb")
    local retval = {
        name = fs.getName(p),
        timestamp = os.epoch and math.floor(os.epoch("utc") / 1000) or 0, 
        owner = fs.getOwner and fs.getOwner(p) or 0, 
        group = 0,
        mode = fs.getPermissions and cc2u(fs.getPermissions(p, fs.getOwner(p) or 0)) * 0x40 + cc2u(fs.getPermissions(p, "*")) + bit.band(fs.getPermissions(p, "*"), 0x10) * 0x80 or 0x1FF,
        data = ""
    }
    if file.seek then retval.data = file.read(fs.getSize(p)) else
        local c = file.read()
        while c ~= nil do 
            retval.data = retval.data .. string.char(c)
            c = file.read()
        end
    end
    file.close()
    return retval
end

-- Packs files in a directory into a table (skips subdirectories)
function ar.pack(path)
    local retval = {}
    for k,v in pairs(fs.list(path)) do
        local p = fs.combine(path, v)
        if not fs.isDir(p) then retval[v] = ar.read(p) end
    end
    return retval
end

-- Saves a table to an archive file
function ar.save(data, path)
    local file = fs.open(path, "wb")
    local oldwrite = file.write
    local seek = 0
    file.write = function(str) 
        for c in string.gmatch(str, ".") do oldwrite(string.byte(c)) end
        seek = seek + string.len(str)
    end
    file.write("!<arch>\n")
    local name_table = {}
    local name_str = nil
    for k,v in pairs(data) do if string.len(v.name) > 15 then 
        name_table[v.name] = string.len(name_str or "")
        name_str = (name_str or "") .. v.name .. "\n"
    end end
    if name_str then
        file.write("//" .. string.rep(" ", 46) .. pad(tostring(string.len(name_str)), 10) .. "`\n" .. name_str)
        if seek / 2 == 1 then file.write("\n") end
    end
    for k,v in pairs(data) do
        local name = name_table[v.name] and "/" .. name_table[v.name] or v.name .. (name_str and "/" or "")
        file.write(pad(name, 16) .. pad(tostring(v.timestamp), 12) .. pad(tostring(v.owner), 6) .. pad(tostring(v.group), 6))
        file.write(pad(string.format("%o", v.mode), 8) .. pad(tostring(string.len(v.data)), 10) .. "`\n" .. v.data)
        if seek % 2 == 1 then file.write("\n") end
    end
    file.close()
    os.queueEvent("nosleep")
    os.pullEvent()
end

local function strmap(num, str, c)
    local retval = ""
    for i = 1, string.len(str) do retval = retval .. (bit.band(num, bit.blshift(1, string.len(str)-i)) == 0 and c or string.sub(str, i, i)) end
    return retval
end

local function CurrentDate(z)
    local z = math.floor(z / 86400) + 719468
    local era = math.floor(z / 146097)
    local doe = math.floor(z - era * 146097)
    local yoe = math.floor((doe - doe / 1460 + doe / 36524 - doe / 146096) / 365)
    local y = math.floor(yoe + era * 400)
    local doy = doe - math.floor((365 * yoe + yoe / 4 - yoe / 100))
    local mp = math.floor((5 * doy + 2) / 153)
    local d = math.ceil(doy - (153 * mp + 2) / 5 + 1)
    local m = math.floor(mp + (mp < 10 and 3 or -9))
    return y + (m <= 2 and 1 or 0), m, d
end
    
local function CurrentTime(unixTime)
    local hours = math.floor(unixTime / 3600 % 24)
    local minutes = math.floor(unixTime / 60 % 60)
    local seconds = math.floor(unixTime % 60)
    local year, month, day = CurrentDate(unixTime)
    return {
        year = year,
        month = month,
        day = day,
        hours = hours,
        minutes = minutes < 10 and "0" .. minutes or minutes,
        seconds = seconds < 10 and "0" .. seconds or seconds
    }
end

local months = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}

if pcall(require, "ar") then
    local args = {...}
    if #args < 2 then error("Usage: ar <dpqrtx[cfTuv]> <archive.a> [path] [files...]") end
    if args[1] == "--version" then
        print("CraftOS ar (CCKernel2 binutils) 1.0 (compatible with GNU/BSD ar)\nCopyright (c) 2019-2020 JackMacWindows.")
        return 2
    end
    local mode = nil
    local update = false
    local truncate = false
    if string.find(args[1], "d") then mode = 0 end -- delete
    if string.find(args[1], "p") then mode = 1 end -- print file
    if string.find(args[1], "q") then mode = 2 end -- quick append
    if string.find(args[1], "r") then mode = 3 end -- replace or add
    if string.find(args[1], "t") then mode = 4 end -- list
    if string.find(args[1], "x") then mode = 5 end -- extract
    if string.find(args[1], "c") then verbosity = -1 end
    if string.find(args[1], "v") then verbosity = 1 end
    if string.find(args[1], "u") then update = true end
    if string.find(args[1], "T") then truncate = true end
    if string.find(args[1], "f") then truncate = true end
    local data = ar.load(shell.resolve(args[2]))
    local files = {...}
    table.remove(files, 1)
    table.remove(files, 1)
    if data == nil then
        if verbosity > -1 then print("ar: Creating archive " .. shell.resolve(args[2])) end
        data = {}
    end
    if mode == 0 then
        for k,v in pairs(data) do for l,w in pairs(files) do if v.name == w then data[k] = nil; break end end end
        ar.save(data, shell.resolve(args[2]))
    elseif mode == 1 then
        if #args > 2 then for k,v in pairs(data) do for l,w in pairs(files) do if v.name == w then print(v.data); break end end end
        else for k,v in pairs(data) do print(v.data) end end
    elseif mode == 2 then
        for k,v in pairs(files) do 
            local f = ar.read(shell.resolve(v))
            f.name = string.sub(f.name, 1, truncate and 15 or nil)
            table.insert(data, f) 
        end
        ar.save(data, shell.resolve(args[2]))
    elseif mode == 3 then
        for k,v in pairs(files) do
            local f = ar.read(shell.resolve(v))
            f.name = string.sub(f.name, 1, truncate and 15 or nil)
            local found = false
            for l,w in pairs(data) do if w.name == f.name then
                found = true
                for m,x in pairs(f) do w[m] = f[m] end
                break
            end end
            if not found then table.insert(data, f) end
        end
        ar.save(data, shell.resolve(args[2]))
    elseif mode == 4 then
        if verbosity > 0 then
            local tmp = {}
            local max = {0, 0, 0, 0, 0}
            for k,v in pairs(data) do
                local date = CurrentTime(v.timestamp)
                local d = months[date.month] .. " " .. date.day .. " " .. date.hours .. ":" .. date.minutes .. " " .. date.year
                local p = {strmap(v.mode, "rwxrwxrwx", "-"), v.owner .. "/" .. v.group, string.len(v.data), d, v.name}
                for l,w in pairs(p) do if string.len(w) + 3 > max[l] then max[l] = string.len(w) + 3 end end
                table.insert(tmp, p)
            end
            for k,v in pairs(tmp) do
                for l,w in pairs(v) do write(pad(w, max[l])) end
                print("")   
            end
        else for k,v in pairs(data) do print(v.name) end end
    elseif mode == 5 then
        local path = #files > 0 and table.remove(files, 1) or "."
        local f
        if #files > 0 then
            f = {}
            for k,v in pairs(data) do for l,w in pairs(files) do if v.name == w then table.insert(f, v); break end end end
        else f = data end
        ar.extract(f, shell.resolve(path))
    else error("Unknown mode") end
end

return ar
end
return preload["kernel"](...)
