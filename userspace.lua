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
    userspace.env[key]=require(key)
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
preload["bin.userspace"] = function(...)
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
    userspace.env[key]=require(key)
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
preload["bin.userspace"] = function(...)
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
preload["bin.userspace"] = function(...)
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
preload["window"] = function(...)
-- SPDX-FileCopyrightText: 2017 Daniel Ratcliffe
--
-- SPDX-License-Identifier: LicenseRef-CCPL

--- The Window API allows easy definition of spaces within the display that can
-- be written/drawn to, then later redrawn/repositioned/etc as need be. The API
-- itself contains only one function, @{window.create}, which returns the
-- windows themselves.
--
-- Windows are considered terminal objects - as such, they have access to nearly
-- all the commands in the term API (plus a few extras of their own, listed
-- within said API) and are valid targets to redirect to.
--
-- Each window has a "parent" terminal object, which can be the computer's own
-- display, a monitor, another window or even other, user-defined terminal
-- objects. Whenever a window is rendered to, the actual screen-writing is
-- performed via that parent (or, if that has one too, then that parent, and so
-- forth). Bear in mind that the cursor of a window's parent will hence be moved
-- around etc when writing a given child window.
--
-- Windows retain a memory of everything rendered "through" them (hence acting
-- as display buffers), and if the parent's display is wiped, the window's
-- content can be easily redrawn later. A window may also be flagged as
-- invisible, preventing any changes to it from being rendered until it's
-- flagged as visible once more.
--
-- A parent terminal object may have multiple children assigned to it, and
-- windows may overlap. For example, the Multishell system functions by
-- assigning each tab a window covering the screen, each using the starting
-- terminal display as its parent, and only one of which is visible at a time.
--
-- @module window
-- @since 1.6

local expect = dofile("rom/modules/main/cc/expect.lua").expect

local tHex = {
    [colors.white] = "0",
    [colors.orange] = "1",
    [colors.magenta] = "2",
    [colors.lightBlue] = "3",
    [colors.yellow] = "4",
    [colors.lime] = "5",
    [colors.pink] = "6",
    [colors.gray] = "7",
    [colors.lightGray] = "8",
    [colors.cyan] = "9",
    [colors.purple] = "a",
    [colors.blue] = "b",
    [colors.brown] = "c",
    [colors.green] = "d",
    [colors.red] = "e",
    [colors.black] = "f",
}

local type = type
local string_rep = string.rep
local string_sub = string.sub

--- Returns a terminal object that is a space within the specified parent
-- terminal object. This can then be used (or even redirected to) in the same
-- manner as eg a wrapped monitor. Refer to @{term|the term API} for a list of
-- functions available to it.
--
-- @{term} itself may not be passed as the parent, though @{term.native} is
-- acceptable. Generally, @{term.current} or a wrapped monitor will be most
-- suitable, though windows may even have other windows assigned as their
-- parents.
--
-- @tparam term.Redirect parent The parent terminal redirect to draw to.
-- @tparam number nX The x coordinate this window is drawn at in the parent terminal
-- @tparam number nY The y coordinate this window is drawn at in the parent terminal
-- @tparam number nWidth The width of this window
-- @tparam number nHeight The height of this window
-- @tparam[opt] boolean bStartVisible Whether this window is visible by
-- default. Defaults to `true`.
-- @treturn Window The constructed window
function create(parent, nX, nY, nWidth, nHeight, bStartVisible)
    expect(1, parent, "table")
    expect(2, nX, "number")
    expect(3, nY, "number")
    expect(4, nWidth, "number")
    expect(5, nHeight, "number")
    expect(6, bStartVisible, "boolean", "nil")

    if parent == term then
        error("term is not a recommended window parent, try term.current() instead", 2)
    end

    local sEmptySpaceLine
    local tEmptyColorLines = {}
    local function createEmptyLines(nWidth)
        sEmptySpaceLine = string_rep(" ", nWidth)
        for n = 0, 15 do
            local nColor = 2 ^ n
            local sHex = tHex[nColor]
            tEmptyColorLines[nColor] = string_rep(sHex, nWidth)
        end
    end

    createEmptyLines(nWidth)

    -- Setup
    local bVisible = bStartVisible ~= false
    local nCursorX = 1
    local nCursorY = 1
    local bCursorBlink = false
    local nTextColor = colors.white
    local nBackgroundColor = colors.black
    local tLines = {}
    local tPalette = {}
    do
        local sEmptyText = sEmptySpaceLine
        local sEmptyTextColor = tEmptyColorLines[nTextColor]
        local sEmptyBackgroundColor = tEmptyColorLines[nBackgroundColor]
        for y = 1, nHeight do
            tLines[y] = { sEmptyText, sEmptyTextColor, sEmptyBackgroundColor }
        end

        for i = 0, 15 do
            local c = 2 ^ i
            tPalette[c] = { parent.getPaletteColour(c) }
        end
    end

    -- Helper functions
    local function updateCursorPos()
        if nCursorX >= 1 and nCursorY >= 1 and
           nCursorX <= nWidth and nCursorY <= nHeight then
            parent.setCursorPos(nX + nCursorX - 1, nY + nCursorY - 1)
        else
            parent.setCursorPos(0, 0)
        end
    end

    local function updateCursorBlink()
        parent.setCursorBlink(bCursorBlink)
    end

    local function updateCursorColor()
        parent.setTextColor(nTextColor)
    end

    local function redrawLine(n)
        local tLine = tLines[n]
        parent.setCursorPos(nX, nY + n - 1)
        parent.blit(tLine[1], tLine[2], tLine[3])
    end

    local function redraw()
        for n = 1, nHeight do
            redrawLine(n)
        end
    end

    local function updatePalette()
        for k, v in pairs(tPalette) do
            parent.setPaletteColour(k, v[1], v[2], v[3])
        end
    end

    local function internalBlit(sText, sTextColor, sBackgroundColor)
        local nStart = nCursorX
        local nEnd = nStart + #sText - 1
        if nCursorY >= 1 and nCursorY <= nHeight then
            if nStart <= nWidth and nEnd >= 1 then
                -- Modify line
                local tLine = tLines[nCursorY]
                if nStart == 1 and nEnd == nWidth then
                    tLine[1] = sText
                    tLine[2] = sTextColor
                    tLine[3] = sBackgroundColor
                else
                    local sClippedText, sClippedTextColor, sClippedBackgroundColor
                    if nStart < 1 then
                        local nClipStart = 1 - nStart + 1
                        local nClipEnd = nWidth - nStart + 1
                        sClippedText = string_sub(sText, nClipStart, nClipEnd)
                        sClippedTextColor = string_sub(sTextColor, nClipStart, nClipEnd)
                        sClippedBackgroundColor = string_sub(sBackgroundColor, nClipStart, nClipEnd)
                    elseif nEnd > nWidth then
                        local nClipEnd = nWidth - nStart + 1
                        sClippedText = string_sub(sText, 1, nClipEnd)
                        sClippedTextColor = string_sub(sTextColor, 1, nClipEnd)
                        sClippedBackgroundColor = string_sub(sBackgroundColor, 1, nClipEnd)
                    else
                        sClippedText = sText
                        sClippedTextColor = sTextColor
                        sClippedBackgroundColor = sBackgroundColor
                    end

                    local sOldText = tLine[1]
                    local sOldTextColor = tLine[2]
                    local sOldBackgroundColor = tLine[3]
                    local sNewText, sNewTextColor, sNewBackgroundColor
                    if nStart > 1 then
                        local nOldEnd = nStart - 1
                        sNewText = string_sub(sOldText, 1, nOldEnd) .. sClippedText
                        sNewTextColor = string_sub(sOldTextColor, 1, nOldEnd) .. sClippedTextColor
                        sNewBackgroundColor = string_sub(sOldBackgroundColor, 1, nOldEnd) .. sClippedBackgroundColor
                    else
                        sNewText = sClippedText
                        sNewTextColor = sClippedTextColor
                        sNewBackgroundColor = sClippedBackgroundColor
                    end
                    if nEnd < nWidth then
                        local nOldStart = nEnd + 1
                        sNewText = sNewText .. string_sub(sOldText, nOldStart, nWidth)
                        sNewTextColor = sNewTextColor .. string_sub(sOldTextColor, nOldStart, nWidth)
                        sNewBackgroundColor = sNewBackgroundColor .. string_sub(sOldBackgroundColor, nOldStart, nWidth)
                    end

                    tLine[1] = sNewText
                    tLine[2] = sNewTextColor
                    tLine[3] = sNewBackgroundColor
                end

                -- Redraw line
                if bVisible then
                    redrawLine(nCursorY)
                end
            end
        end

        -- Move and redraw cursor
        nCursorX = nEnd + 1
        if bVisible then
            updateCursorColor()
            updateCursorPos()
        end
    end

    --- The window object. Refer to the @{window|module's documentation} for
    -- a full description.
    --
    -- @type Window
    -- @see term.Redirect
    local window = {}

    function window.write(sText)
        sText = tostring(sText)
        internalBlit(sText, string_rep(tHex[nTextColor], #sText), string_rep(tHex[nBackgroundColor], #sText))
    end

    function window.blit(sText, sTextColor, sBackgroundColor)
        if type(sText) ~= "string" then expect(1, sText, "string") end
        if type(sTextColor) ~= "string" then expect(2, sTextColor, "string") end
        if type(sBackgroundColor) ~= "string" then expect(3, sBackgroundColor, "string") end
        if #sTextColor ~= #sText or #sBackgroundColor ~= #sText then
            error("Arguments must be the same length", 2)
        end
        sTextColor = sTextColor:lower()
        sBackgroundColor = sBackgroundColor:lower()
        internalBlit(sText, sTextColor, sBackgroundColor)
    end

    function window.clear()
        if term.getGraphicsMode and term.getGraphicsMode() then return term.native().clear() end
        local sEmptyText = sEmptySpaceLine
        local sEmptyTextColor = tEmptyColorLines[nTextColor]
        local sEmptyBackgroundColor = tEmptyColorLines[nBackgroundColor]
        for y = 1, nHeight do
            local line = tLines[y]
            line[1] = sEmptyText
            line[2] = sEmptyTextColor
            line[3] = sEmptyBackgroundColor
        end
        if bVisible then
            redraw()
            updateCursorColor()
            updateCursorPos()
        end
    end

    function window.clearLine()
        if nCursorY >= 1 and nCursorY <= nHeight then
            local line = tLines[nCursorY]
            line[1] = sEmptySpaceLine
            line[2] = tEmptyColorLines[nTextColor]
            line[3] = tEmptyColorLines[nBackgroundColor]
            if bVisible then
                redrawLine(nCursorY)
                updateCursorColor()
                updateCursorPos()
            end
        end
    end

    function window.getCursorPos()
        return nCursorX, nCursorY
    end

    function window.setCursorPos(x, y)
        if type(x) ~= "number" then expect(1, x, "number") end
        if type(y) ~= "number" then expect(2, y, "number") end
        nCursorX = math.floor(x)
        nCursorY = math.floor(y)
        if bVisible then
            updateCursorPos()
        end
    end

    function window.setCursorBlink(blink)
        if type(blink) ~= "boolean" then expect(1, blink, "boolean") end
        bCursorBlink = blink
        if bVisible then
            updateCursorBlink()
        end
    end

    function window.getCursorBlink()
        return bCursorBlink
    end

    local function isColor()
        return parent.isColor()
    end

    function window.isColor()
        return isColor()
    end

    function window.isColour()
        return isColor()
    end

    local function setTextColor(color)
        if type(color) ~= "number" then expect(1, color, "number") end
        if tHex[color] == nil then
            error("Invalid color (got " .. color .. ")" , 2)
        end

        nTextColor = color
        if bVisible then
            updateCursorColor()
        end
    end

    window.setTextColor = setTextColor
    window.setTextColour = setTextColor

    function window.setPaletteColour(colour, r, g, b)
        if type(colour) ~= "number" then expect(1, colour, "number") end

        if parent.getGraphicsMode and parent.getGraphicsMode() == 2 then
            if colour < 0 or colour > 255 then
                error("Invalid color (got " .. colour .. ")" , 2)
            end
        else
            if tHex[colour] == nil then
                error("Invalid color (got " .. colour .. ")" , 2)
            end
        end

        local tCol
        if type(r) == "number" and g == nil and b == nil then
            tCol = { colours.unpackRGB(r) }
            tPalette[colour] = tCol
        else
            if type(r) ~= "number" then expect(2, r, "number") end
            if type(g) ~= "number" then expect(3, g, "number") end
            if type(b) ~= "number" then expect(4, b, "number") end

            if not tPalette[colour] then tPalette[colour] = {0, 0, 0} end
            tCol = tPalette[colour]
            tCol[1] = r
            tCol[2] = g
            tCol[3] = b
        end

        if bVisible then
            return parent.setPaletteColour(colour, tCol[1], tCol[2], tCol[3])
        end
    end

    window.setPaletteColor = window.setPaletteColour

    function window.getPaletteColour(colour)
        if type(colour) ~= "number" then expect(1, colour, "number") end
        if parent.getGraphicsMode and parent.getGraphicsMode() == 2 then
            if colour < 0 or colour > 255 then
                error("Invalid color (got " .. colour .. ")" , 2)
            end
        else
            if tHex[colour] == nil then
                error("Invalid color (got " .. colour .. ")" , 2)
            end
        end
        local tCol = tPalette[colour] or {0, 0, 0}
        return tCol[1], tCol[2], tCol[3]
    end

    window.getPaletteColor = window.getPaletteColour

    local function setBackgroundColor(color)
        if type(color) ~= "number" then expect(1, color, "number") end
        if tHex[color] == nil then
            error("Invalid color (got " .. color .. ")", 2)
        end
        nBackgroundColor = color
    end

    window.setBackgroundColor = setBackgroundColor
    window.setBackgroundColour = setBackgroundColor

    function window.getSize(mode)
        if mode and mode ~= 0 then return term.native().getSize(mode) end
        return nWidth, nHeight
    end

    function window.scroll(n)
        if type(n) ~= "number" then expect(1, n, "number") end
        if n ~= 0 then
            local tNewLines = {}
            local sEmptyText = sEmptySpaceLine
            local sEmptyTextColor = tEmptyColorLines[nTextColor]
            local sEmptyBackgroundColor = tEmptyColorLines[nBackgroundColor]
            for newY = 1, nHeight do
                local y = newY + n
                if y >= 1 and y <= nHeight then
                    tNewLines[newY] = tLines[y]
                else
                    tNewLines[newY] = { sEmptyText, sEmptyTextColor, sEmptyBackgroundColor }
                end
            end
            tLines = tNewLines
            if bVisible then
                redraw()
                updateCursorColor()
                updateCursorPos()
            end
        end
    end

    function window.getTextColor()
        return nTextColor
    end

    function window.getTextColour()
        return nTextColor
    end

    function window.getBackgroundColor()
        return nBackgroundColor
    end

    function window.getBackgroundColour()
        return nBackgroundColor
    end

    --- Get the buffered contents of a line in this window.
    --
    -- @tparam number y The y position of the line to get.
    -- @treturn string The textual content of this line.
    -- @treturn string The text colours of this line, suitable for use with @{term.blit}.
    -- @treturn string The background colours of this line, suitable for use with @{term.blit}.
    -- @throws If `y` is not between 1 and this window's height.
    function window.getLine(y)
        if type(y) ~= "number" then expect(1, y, "number") end

        if y < 1 or y > nHeight then
            error("Line is out of range.", 2)
        end

        local line = tLines[y]
        return line[1], line[2], line[3]
    end

    -- Other functions

    --- Set whether this window is visible. Invisible windows will not be drawn
    -- to the screen until they are made visible again.
    --
    -- Making an invisible window visible will immediately draw it.
    --
    -- @tparam boolean visible Whether this window is visible.
    function window.setVisible(visible)
        if type(visible) ~= "boolean" then expect(1, visible, "boolean") end
        if bVisible ~= visible then
            bVisible = visible
            if bVisible then
                window.redraw()
            end
        end
    end

    --- Get whether this window is visible. Invisible windows will not be
    -- drawn to the screen until they are made visible again.
    --
    -- @treturn boolean Whether this window is visible.
    -- @see Window:setVisible
    function window.isVisible()
        return bVisible
    end
    --- Draw this window. This does nothing if the window is not visible.
    --
    -- @see Window:setVisible
    function window.redraw()
        if bVisible then
            redraw()
            updatePalette()
            updateCursorBlink()
            updateCursorColor()
            updateCursorPos()
        end
    end

    --- Set the current terminal's cursor to where this window's cursor is. This
    -- does nothing if the window is not visible.
    function window.restoreCursor()
        if bVisible then
            updateCursorBlink()
            updateCursorColor()
            updateCursorPos()
        end
    end

    --- Get the position of the top left corner of this window.
    --
    -- @treturn number The x position of this window.
    -- @treturn number The y position of this window.
    function window.getPosition()
        return nX, nY
    end

    --- Reposition or resize the given window.
    --
    -- This function also accepts arguments to change the size of this window.
    -- It is recommended that you fire a `term_resize` event after changing a
    -- window's, to allow programs to adjust their sizing.
    --
    -- @tparam number new_x The new x position of this window.
    -- @tparam number new_y The new y position of this window.
    -- @tparam[opt] number new_width The new width of this window.
    -- @tparam number new_height The new height of this window.
    -- @tparam[opt] term.Redirect new_parent The new redirect object this
    -- window should draw to.
    function window.reposition(new_x, new_y, new_width, new_height, new_parent)
        if type(new_x) ~= "number" then expect(1, new_x, "number") end
        if type(new_y) ~= "number" then expect(2, new_y, "number") end
        if new_width ~= nil or new_height ~= nil then
            expect(3, new_width, "number")
            expect(4, new_height, "number")
        end
        if new_parent ~= nil and type(new_parent) ~= "table" then expect(5, new_parent, "table") end

        nX = new_x
        nY = new_y

        if new_parent then parent = new_parent end

        if new_width and new_height then
            local tNewLines = {}
            createEmptyLines(new_width)
            local sEmptyText = sEmptySpaceLine
            local sEmptyTextColor = tEmptyColorLines[nTextColor]
            local sEmptyBackgroundColor = tEmptyColorLines[nBackgroundColor]
            for y = 1, new_height do
                if y > nHeight then
                    tNewLines[y] = { sEmptyText, sEmptyTextColor, sEmptyBackgroundColor }
                else
                    local tOldLine = tLines[y]
                    if new_width == nWidth then
                        tNewLines[y] = tOldLine
                    elseif new_width < nWidth then
                        tNewLines[y] = {
                            string_sub(tOldLine[1], 1, new_width),
                            string_sub(tOldLine[2], 1, new_width),
                            string_sub(tOldLine[3], 1, new_width),
                        }
                    else
                        tNewLines[y] = {
                            tOldLine[1] .. string_sub(sEmptyText, nWidth + 1, new_width),
                            tOldLine[2] .. string_sub(sEmptyTextColor, nWidth + 1, new_width),
                            tOldLine[3] .. string_sub(sEmptyBackgroundColor, nWidth + 1, new_width),
                        }
                    end
                end
            end
            nWidth = new_width
            nHeight = new_height
            tLines = tNewLines
        end
        if bVisible then
            window.redraw()
        end
    end

    if parent.getGraphicsMode then
        function window.setGraphicsMode(mode)
            local oldMode = parent.getGraphicsMode()
            parent.setGraphicsMode(mode)
            if oldMode ~= 2 and mode == 2 then
                local newPalette = {}
                for i = 0, 15 do newPalette[i] = tPalette[2^i] end
                tPalette = newPalette
            elseif oldMode == 2 and mode ~= 2 then
                local newPalette = {}
                for i = 0, 15 do newPalette[2^i] = tPalette[i] end
                tPalette = newPalette
            end
        end
    end

    if bVisible then
        window.redraw()
    end
    return window
end

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
function new(x, y, z)
    return setmetatable({
        x = tonumber(x) or 0,
        y = tonumber(y) or 0,
        z = tonumber(z) or 0,
    }, vmetatable)
end

end
preload["userspace"] = function(...)

end
preload["textutils"] = function(...)
-- SPDX-FileCopyrightText: 2017 Daniel Ratcliffe
--
-- SPDX-License-Identifier: LicenseRef-CCPL

--- The @{textutils} API provides helpful utilities for formatting and
-- manipulating strings.
--
-- @module textutils
-- @since 1.2

local expect = dofile("rom/modules/main/cc/expect.lua")
local expect, field = expect.expect, expect.field
local wrap = dofile("rom/modules/main/cc/strings.lua").wrap

--- Slowly writes string text at current cursor position,
-- character-by-character.
--
-- Like @{_G.write}, this does not insert a newline at the end.
--
-- @tparam string text The the text to write to the screen
-- @tparam[opt] number rate The number of characters to write each second,
-- Defaults to 20.
-- @usage textutils.slowWrite("Hello, world!")
-- @usage textutils.slowWrite("Hello, world!", 5)
-- @since 1.3
function slowWrite(text, rate)
    expect(2, rate, "number", "nil")
    rate = rate or 20
    if rate < 0 then
        error("Rate must be positive", 2)
    end
    local to_sleep = 1 / rate

    local wrapped_lines = wrap(tostring(text), (term.getSize()))
    local wrapped_str = table.concat(wrapped_lines, "\n")

    for n = 1, #wrapped_str do
        sleep(to_sleep)
        write(wrapped_str:sub(n, n))
    end
end

--- Slowly prints string text at current cursor position,
-- character-by-character.
--
-- Like @{print}, this inserts a newline after printing.
--
-- @tparam string sText The the text to write to the screen
-- @tparam[opt] number nRate The number of characters to write each second,
-- Defaults to 20.
-- @usage textutils.slowPrint("Hello, world!")
-- @usage textutils.slowPrint("Hello, world!", 5)
function slowPrint(sText, nRate)
    slowWrite(sText, nRate)
    print()
end

--- Takes input time and formats it in a more readable format such as `6:30 PM`.
--
-- @tparam number nTime The time to format, as provided by @{os.time}.
-- @tparam[opt] boolean bTwentyFourHour Whether to format this as a 24-hour
-- clock (`18:30`) rather than a 12-hour one (`6:30 AM`)
-- @treturn string The formatted time
-- @usage Print the current in-game time as a 12-hour clock.
--
--     textutils.formatTime(os.time())
-- @usage Print the local time as a 24-hour clock.
--
--     textutils.formatTime(os.time("local"), true)
function formatTime(nTime, bTwentyFourHour)
    expect(1, nTime, "number")
    expect(2, bTwentyFourHour, "boolean", "nil")
    local sTOD = nil
    if not bTwentyFourHour then
        if nTime >= 12 then
            sTOD = "PM"
        else
            sTOD = "AM"
        end
        if nTime >= 13 then
            nTime = nTime - 12
        end
    end

    local nHour = math.floor(nTime)
    local nMinute = math.floor((nTime - nHour) * 60)
    if sTOD then
        return string.format("%d:%02d %s", nHour == 0 and 12 or nHour, nMinute, sTOD)
    else
        return string.format("%d:%02d", nHour, nMinute)
    end
end

local function makePagedScroll(_term, _nFreeLines)
    local nativeScroll = _term.scroll
    local nFreeLines = _nFreeLines or 0
    return function(_n)
        for _ = 1, _n do
            nativeScroll(1)

            if nFreeLines <= 0 then
                local _, h = _term.getSize()
                _term.setCursorPos(1, h)
                _term.write("Press any key to continue")
                os.pullEvent("key")
                _term.clearLine()
                _term.setCursorPos(1, h)
            else
                nFreeLines = nFreeLines - 1
            end
        end
    end
end

--[[- Prints a given string to the display.

If the action can be completed without scrolling, it acts much the same as
@{print}; otherwise, it will throw up a "Press any key to continue" prompt at
the bottom of the display. Each press will cause it to scroll down and write a
single line more before prompting again, if need be.

@tparam string text The text to print to the screen.
@tparam[opt] number free_lines The number of lines which will be
automatically scrolled before the first prompt appears (meaning free_lines +
1 lines will be printed). This can be set to the cursor's y position - 2 to
always try to fill the screen. Defaults to 0, meaning only one line is
displayed before prompting.
@treturn number The number of lines printed.

@usage Generates several lines of text and then prints it, paging once the
bottom of the terminal is reached.

    local lines = {}
    for i = 1, 30 do lines[i] = ("This is line #%d"):format(i) end
    local message = table.concat(lines, "\n")

    local width, height = term.getCursorPos()
    textutils.pagedPrint(message, height - 2)
]]
function pagedPrint(text, free_lines)
    expect(2, free_lines, "number", "nil")
    -- Setup a redirector
    local oldTerm = term.current()
    local newTerm = {}
    for k, v in pairs(oldTerm) do
        newTerm[k] = v
    end
    newTerm.scroll = makePagedScroll(oldTerm, free_lines)
    term.redirect(newTerm)

    -- Print the text
    local result
    local ok, err = pcall(function()
        if text ~= nil then
            result = print(text)
        else
            result = print()
        end
    end)

    -- Removed the redirector
    term.redirect(oldTerm)

    -- Propogate errors
    if not ok then
        error(err, 0)
    end
    return result
end

local function tabulateCommon(bPaged, ...)
    local tAll = table.pack(...)
    for i = 1, tAll.n do
        expect(i, tAll[i], "number", "table")
    end

    local w, h = term.getSize()
    local nMaxLen = w / 8
    for n, t in ipairs(tAll) do
        if type(t) == "table" then
            for nu, sItem in pairs(t) do
                local ty = type(sItem)
                if ty ~= "string" and ty ~= "number" then
                    error("bad argument #" .. n .. "." .. nu .. " (string expected, got " .. ty .. ")", 3)
                end
                nMaxLen = math.max(#tostring(sItem) + 1, nMaxLen)
            end
        end
    end
    local nCols = math.floor(w / nMaxLen)
    local nLines = 0
    local function newLine()
        if bPaged and nLines >= h - 3 then
            pagedPrint()
        else
            print()
        end
        nLines = nLines + 1
    end

    local function drawCols(_t)
        local nCol = 1
        for _, s in ipairs(_t) do
            if nCol > nCols then
                nCol = 1
                newLine()
            end

            local cx, cy = term.getCursorPos()
            cx = 1 + (nCol - 1) * nMaxLen
            term.setCursorPos(cx, cy)
            term.write(s)

            nCol = nCol + 1
        end
        print()
    end

    local previous_colour = term.getTextColour()
    for _, t in ipairs(tAll) do
        if type(t) == "table" then
            if #t > 0 then
                drawCols(t)
            end
        elseif type(t) == "number" then
            term.setTextColor(t)
        end
    end
    term.setTextColor(previous_colour)
end

--[[- Prints tables in a structured form.

This accepts multiple arguments, either a table or a number. When
encountering a table, this will be treated as a table row, with each column
width being auto-adjusted.

When encountering a number, this sets the text color of the subsequent rows to it.

@tparam {string...}|number ... The rows and text colors to display.
@since 1.3
@usage

    textutils.tabulate(
      colors.orange, { "1", "2", "3" },
      colors.lightBlue, { "A", "B", "C" }
    )
]]
function tabulate(...)
    return tabulateCommon(false, ...)
end

--[[- Prints tables in a structured form, stopping and prompting for input should
the result not fit on the terminal.

This functions identically to @{textutils.tabulate}, but will prompt for user
input should the whole output not fit on the display.

@tparam {string...}|number ... The rows and text colors to display.
@see textutils.tabulate
@see textutils.pagedPrint
@since 1.3

@usage Generates a long table, tabulates it, and prints it to the screen.

    local rows = {}
    for i = 1, 30 do rows[i] = {("Row #%d"):format(i), math.random(1, 400)} end

    textutils.pagedTabulate(colors.orange, {"Column", "Value"}, colors.lightBlue, table.unpack(rows))
]]
function pagedTabulate(...)
    return tabulateCommon(true, ...)
end

local g_tLuaKeywords = {
    ["and"] = true,
    ["break"] = true,
    ["do"] = true,
    ["else"] = true,
    ["elseif"] = true,
    ["end"] = true,
    ["false"] = true,
    ["for"] = true,
    ["function"] = true,
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

--- A version of the ipairs iterator which ignores metamethods
local function inext(tbl, i)
    i = (i or 0) + 1
    local v = rawget(tbl, i)
    if v == nil then return nil else return i, v end
end

local serialize_infinity = math.huge
local function serialize_impl(t, tracking, indent, opts)
    local sType = type(t)
    if sType == "table" then
        if tracking[t] ~= nil then
            if tracking[t] == false then
                error("Cannot serialize table with repeated entries", 0)
            else
                error("Cannot serialize table with recursive entries", 0)
            end
        end
        tracking[t] = true

        local result
        if next(t) == nil then
            -- Empty tables are simple
            result = "{}"
        else
            -- Other tables take more work
            local open, sub_indent, open_key, close_key, equal, comma = "{\n", indent .. "  ", "[ ", " ] = ", " = ", ",\n"
            if opts.compact then
                open, sub_indent, open_key, close_key, equal, comma = "{", "", "[", "]=", "=", ","
            end

            result = open
            local seen_keys = {}
            for k, v in inext, t do
                seen_keys[k] = true
                result = result .. sub_indent .. serialize_impl(v, tracking, sub_indent, opts) .. comma
            end
            for k, v in next, t do
                if not seen_keys[k] then
                    local sEntry
                    if type(k) == "string" and not g_tLuaKeywords[k] and string.match(k, "^[%a_][%a%d_]*$") then
                        sEntry = k .. equal .. serialize_impl(v, tracking, sub_indent, opts) .. comma
                    else
                        sEntry = open_key .. serialize_impl(k, tracking, sub_indent, opts) .. close_key .. serialize_impl(v, tracking, sub_indent, opts) .. comma
                    end
                    result = result .. sub_indent .. sEntry
                end
            end
            result = result .. indent .. "}"
        end

        if opts.allow_repetitions then
            tracking[t] = nil
        else
            tracking[t] = false
        end
        return result

    elseif sType == "string" then
        return string.format("%q", t)

    elseif sType == "number" then
        if t ~= t then --nan
            return "0/0"
        elseif t == serialize_infinity then
            return "1/0"
        elseif t == -serialize_infinity then
            return "-1/0"
        else
            return tostring(t)
        end

    elseif sType == "boolean" or sType == "nil" then
        return tostring(t)

    else
        error("Cannot serialize type " .. sType, 0)

    end
end

local function mk_tbl(str, name)
    local msg = "attempt to mutate textutils." .. name
    return setmetatable({}, {
        __newindex = function() error(msg, 2) end,
        __tostring = function() return str end,
    })
end

--- A table representing an empty JSON array, in order to distinguish it from an
-- empty JSON object.
--
-- The contents of this table should not be modified.
--
-- @usage textutils.serialiseJSON(textutils.empty_json_array)
-- @see textutils.serialiseJSON
-- @see textutils.unserialiseJSON
empty_json_array = mk_tbl("[]", "empty_json_array")

--- A table representing the JSON null value.
--
-- The contents of this table should not be modified.
--
-- @usage textutils.serialiseJSON(textutils.json_null)
-- @see textutils.serialiseJSON
-- @see textutils.unserialiseJSON
json_null = mk_tbl("null", "json_null")

local serializeJSONString
do
    local function hexify(c)
        return ("\\u00%02X"):format(c:byte())
    end

    local map = {
        ["\""] = "\\\"",
        ["\\"] = "\\\\",
        ["\b"] = "\\b",
        ["\f"] = "\\f",
        ["\n"] = "\\n",
        ["\r"] = "\\r",
        ["\t"] = "\\t",
    }
    for i = 0, 0x1f do
        local c = string.char(i)
        if map[c] == nil then map[c] = hexify(c) end
    end

    serializeJSONString = function(s, options)
        if options and options.unicode_strings and s:find("[\x80-\xff]") then
            local retval = '"'
            for _, code in utf8.codes(s) do
                if code > 0xFFFF then
                    -- Encode the codepoint as a UTF-16 surrogate pair
                    code = code - 0x10000
                    local high, low = bit32.extract(code, 10, 10) + 0xD800, bit32.extract(code, 0, 10) + 0xDC00
                    retval = retval .. ("\\u%04X\\u%04X"):format(high, low)
                elseif code <= 0x5C and map[string.char(code)] then -- 0x5C = `\`, don't run `string.char` if we don't need to
                    retval = retval .. map[string.char(code)]
                elseif code < 0x20 or code >= 0x7F then
                    retval = retval .. ("\\u%04X"):format(code)
                else
                    retval = retval .. string.char(code)
                end
            end
            return retval .. '"'
        else
            return ('"%s"'):format(s:gsub("[%z\1-\x1f\"\\]", map):gsub("[\x7f-\xff]", hexify))
        end
    end
end

local function serializeJSONImpl(t, tracking, options)
    local sType = type(t)
    if t == empty_json_array then return "[]"
    elseif t == json_null then return "null"

    elseif sType == "table" then
        if tracking[t] ~= nil then
            if tracking[t] == false then
                error("Cannot serialize table with repeated entries", 0)
            else
                error("Cannot serialize table with recursive entries", 0)
            end
        end
        tracking[t] = true

        local result
        if next(t) == nil then
            -- Empty tables are simple
            result = "{}"
        else
            -- Other tables take more work
            local sObjectResult = "{"
            local sArrayResult = "["
            local nObjectSize = 0
            local nArraySize = 0
            local largestArrayIndex = 0
            local bNBTStyle = options.nbt_style
            for k, v in pairs(t) do
                if type(k) == "string" then
                    local sEntry
                    if bNBTStyle then
                        sEntry = tostring(k) .. ":" .. serializeJSONImpl(v, tracking, options)
                    else
                        sEntry = serializeJSONString(k, options) .. ":" .. serializeJSONImpl(v, tracking, options)
                    end
                    if nObjectSize == 0 then
                        sObjectResult = sObjectResult .. sEntry
                    else
                        sObjectResult = sObjectResult .. "," .. sEntry
                    end
                    nObjectSize = nObjectSize + 1
                elseif type(k) == "number" and k > largestArrayIndex then --the largest index is kept to avoid losing half the array if there is any single nil in that array
                    largestArrayIndex = k
                end
            end
            for k = 1, largestArrayIndex, 1 do --the array is read up to the very last valid array index, ipairs() would stop at the first nil value and we would lose any data after.
                local sEntry
                if t[k] == nil then --if the array is nil at index k the value is "null" as to keep the unused indexes in between used ones.
                    sEntry = "null"
                else -- if the array index does not point to a nil we serialise it's content.
                    sEntry = serializeJSONImpl(t[k], tracking, options)
                end
                if nArraySize == 0 then
                    sArrayResult = sArrayResult .. sEntry
                else
                    sArrayResult = sArrayResult .. "," .. sEntry
                end
                nArraySize = nArraySize + 1
            end
            sObjectResult = sObjectResult .. "}"
            sArrayResult = sArrayResult .. "]"
            if nObjectSize > 0 or nArraySize == 0 then
                result = sObjectResult
            else
                result = sArrayResult
            end
        end

        if options.allow_repetitions then
            tracking[t] = nil
        else
            tracking[t] = false
        end
        return result

    elseif sType == "string" then
        return serializeJSONString(t, options)

    elseif sType == "number" or sType == "boolean" then
        return tostring(t)

    else
        error("Cannot serialize type " .. sType, 0)

    end
end

local unserialise_json
do
    local sub, find, match, concat, tonumber = string.sub, string.find, string.match, table.concat, tonumber

    --- Skip any whitespace
    local function skip(str, pos)
        local _, last = find(str, "^[ \n\r\t]+", pos)
        if last then return last + 1 else return pos end
    end

    local escapes = {
        ["b"] = '\b', ["f"] = '\f', ["n"] = '\n', ["r"] = '\r', ["t"] = '\t',
        ["\""] = "\"", ["/"] = "/", ["\\"] = "\\",
    }

    local mt = {}

    local function error_at(pos, msg, ...)
        if select('#', ...) > 0 then msg = msg:format(...) end
        error(setmetatable({ pos = pos, msg = msg }, mt))
    end

    local function expected(pos, actual, exp)
        if actual == "" then actual = "end of input" else actual = ("%q"):format(actual) end
        error_at(pos, "Unexpected %s, expected %s.", actual, exp)
    end

    local function parse_string(str, pos, terminate)
        local buf, n = {}, 1

        while true do
            local c = sub(str, pos, pos)
            if c == "" then error_at(pos, "Unexpected end of input, expected '\"'.") end
            if c == terminate then break end

            if c == '\\' then
                -- Handle the various escapes
                c = sub(str, pos + 1, pos + 1)
                if c == "" then error_at(pos, "Unexpected end of input, expected escape sequence.") end

                if c == "u" then
                    local num_str = match(str, "^%x%x%x%x", pos + 2)
                    if not num_str then error_at(pos, "Malformed unicode escape %q.", sub(str, pos + 2, pos + 5)) end
                    buf[n], n, pos = utf8.char(tonumber(num_str, 16)), n + 1, pos + 6
                else
                    local unesc = escapes[c]
                    if not unesc then error_at(pos + 1, "Unknown escape character %q.", c) end
                    buf[n], n, pos = unesc, n + 1, pos + 2
                end
            elseif c >= '\x20' then
                buf[n], n, pos = c, n + 1, pos + 1
            else
                error_at(pos + 1, "Unescaped whitespace %q.", c)
            end
        end

        return concat(buf, "", 1, n - 1), pos + 1
    end

    local num_types = { b = true, B = true, s = true, S = true, l = true, L = true, f = true, F = true, d = true, D = true }
    local function parse_number(str, pos, opts)
        local _, last, num_str = find(str, '^(-?%d+%.?%d*[eE]?[+-]?%d*)', pos)
        local val = tonumber(num_str)
        if not val then error_at(pos, "Malformed number %q.", num_str) end

        if opts.nbt_style and num_types[sub(str, last + 1, last + 1)] then return val, last + 2 end

        return val, last + 1
    end

    local function parse_ident(str, pos)
        local _, last, val = find(str, '^([%a][%w_]*)', pos)
        return val, last + 1
    end

    local arr_types = { I = true, L = true, B = true }
    local function decode_impl(str, pos, opts)
        local c = sub(str, pos, pos)
        if c == '"' then return parse_string(str, pos + 1, '"')
        elseif c == "'" and opts.nbt_style then return parse_string(str, pos + 1, "\'")
        elseif c == "-" or c >= "0" and c <= "9" then return parse_number(str, pos, opts)
        elseif c == "t" then
            if sub(str, pos + 1, pos + 3) == "rue" then return true, pos + 4 end
        elseif c == 'f' then
            if sub(str, pos + 1, pos + 4) == "alse" then return false, pos + 5 end
        elseif c == 'n' then
            if sub(str, pos + 1, pos + 3) == "ull" then
                if opts.parse_null then
                    return json_null, pos + 4
                else
                    return nil, pos + 4
                end
            end
        elseif c == "{" then
            local obj = {}

            pos = skip(str, pos + 1)
            c = sub(str, pos, pos)

            if c == "" then return error_at(pos, "Unexpected end of input, expected '}'.") end
            if c == "}" then return obj, pos + 1 end

            while true do
                local key, value
                if c == "\"" then key, pos = parse_string(str, pos + 1, "\"")
                elseif opts.nbt_style then key, pos = parse_ident(str, pos)
                else return expected(pos, c, "object key")
                end

                pos = skip(str, pos)

                c = sub(str, pos, pos)
                if c ~= ":" then return expected(pos, c, "':'") end

                value, pos = decode_impl(str, skip(str, pos + 1), opts)
                obj[key] = value

                -- Consume the next delimiter
                pos = skip(str, pos)
                c = sub(str, pos, pos)
                if c == "}" then break
                elseif c == "," then pos = skip(str, pos + 1)
                else return expected(pos, c, "',' or '}'")
                end

                c = sub(str, pos, pos)
            end

            return obj, pos + 1

        elseif c == "[" then
            local arr, n = {}, 1

            pos = skip(str, pos + 1)
            c = sub(str, pos, pos)

            if arr_types[c] and sub(str, pos + 1, pos + 1) == ";" and opts.nbt_style then
                pos = skip(str, pos + 2)
                c = sub(str, pos, pos)
            end

            if c == "" then return expected(pos, c, "']'") end
            if c == "]" then
                if opts.parse_empty_array ~= false then
                    return empty_json_array, pos + 1
                else
                    return {}, pos + 1
                end
            end

            while true do
                n, arr[n], pos = n + 1, decode_impl(str, pos, opts)

                -- Consume the next delimiter
                pos = skip(str, pos)
                c = sub(str, pos, pos)
                if c == "]" then break
                elseif c == "," then pos = skip(str, pos + 1)
                else return expected(pos, c, "',' or ']'")
                end
            end

            return arr, pos + 1
        elseif c == "" then error_at(pos, 'Unexpected end of input.')
        end

        error_at(pos, "Unexpected character %q.", c)
    end

    --[[- Converts a serialised JSON string back into a reassembled Lua object.

    This may be used with @{textutils.serializeJSON}, or when communicating
    with command blocks or web APIs.

    If a `null` value is encountered, it is converted into `nil`. It can be converted
    into @{textutils.json_null} with the `parse_null` option.

    If an empty array is encountered, it is converted into @{textutils.empty_json_array}.
    It can be converted into a new empty table with the `parse_empty_array` option.

    @tparam string s The serialised string to deserialise.
    @tparam[opt] { nbt_style? = boolean, parse_null? = boolean, parse_empty_array? = boolean } options
    Options which control how this JSON object is parsed.

    - `nbt_style`: When true, this will accept [stringified NBT][nbt] strings,
       as produced by many commands.
    - `parse_null`: When true, `null` will be parsed as @{json_null}, rather than
       `nil`.
    - `parse_empty_array`: When false, empty arrays will be parsed as a new table.
       By default (or when this value is true), they are parsed as @{empty_json_array}.

    [nbt]: https://minecraft.gamepedia.com/NBT_format
    @return[1] The deserialised object
    @treturn[2] nil If the object could not be deserialised.
    @treturn string A message describing why the JSON string is invalid.
    @since 1.87.0
    @changed 1.100.6 Added `parse_empty_array` option
    @see textutils.json_null Use to serialize a JSON `null` value.
    @see textutils.empty_json_array Use to serialize a JSON empty array.
    @usage Unserialise a basic JSON object

        textutils.unserialiseJSON('{"name": "Steve", "age": null}')

    @usage Unserialise a basic JSON object, returning null values as @{json_null}.

        textutils.unserialiseJSON('{"name": "Steve", "age": null}', { parse_null = true })
    ]]
    unserialise_json = function(s, options)
        expect(1, s, "string")
        expect(2, options, "table", "nil")

        if options then
            field(options, "nbt_style", "boolean", "nil")
            field(options, "parse_null", "boolean", "nil")
            field(options, "parse_empty_array", "boolean", "nil")
        else
            options = {}
        end

        local ok, res, pos = pcall(decode_impl, s, skip(s, 1), options)
        if not ok then
            if type(res) == "table" and getmetatable(res) == mt then
                return nil, ("Malformed JSON at position %d: %s"):format(res.pos, res.msg)
            end

            error(res, 0)
        end

        pos = skip(s, pos)
        if pos <= #s then
            return nil, ("Malformed JSON at position %d: Unexpected trailing character %q."):format(pos, sub(s, pos, pos))
        end
        return res

    end
end

--[[- Convert a Lua object into a textual representation, suitable for
saving in a file or pretty-printing.

@param t The object to serialise
@tparam { compact? = boolean, allow_repetitions? = boolean } opts Options for serialisation.
 - `compact`: Do not emit indentation and other whitespace between terms.
 - `allow_repetitions`: Relax the check for recursive tables, allowing them to appear multiple
   times (as long as tables do not appear inside themselves).

@treturn string The serialised representation
@throws If the object contains a value which cannot be
serialised. This includes functions and tables which appear multiple
times.
@see cc.pretty.pretty_print An alternative way to display a table, often more
suitable for pretty printing.
@since 1.3
@changed 1.97.0 Added `opts` argument.
@usage Serialise a basic table.

    textutils.serialise({ 1, 2, 3, a = 1, ["another key"] = { true } })

@usage Demonstrates some of the other options

    local tbl = { 1, 2, 3 }
    print(textutils.serialise({ tbl, tbl }, { allow_repetitions = true }))

    print(textutils.serialise(tbl, { compact = true }))
]]
function serialize(t, opts)
    local tTracking = {}
    expect(2, opts, "table", "nil")

    if opts then
        field(opts, "compact", "boolean", "nil")
        field(opts, "allow_repetitions", "boolean", "nil")
    else
        opts = {}
    end
    return serialize_impl(t, tTracking, "", opts)
end

serialise = serialize -- GB version

--- Converts a serialised string back into a reassembled Lua object.
--
-- This is mainly used together with @{textutils.serialise}.
--
-- @tparam string s The serialised string to deserialise.
-- @return[1] The deserialised object
-- @treturn[2] nil If the object could not be deserialised.
-- @since 1.3
function unserialize(s)
    expect(1, s, "string")
    local func = load("return " .. s, "unserialize", "t", {})
    if func then
        local ok, result = pcall(func)
        if ok then
            return result
        end
    end
    return nil
end

unserialise = unserialize -- GB version

--[[- Returns a JSON representation of the given data.

This function attempts to guess whether a table is a JSON array or
object. However, empty tables are assumed to be empty objects - use
@{textutils.empty_json_array} to mark an empty array.

This is largely intended for interacting with various functions from the
@{commands} API, though may also be used in making @{http} requests.

@param[1] t The value to serialise. Like @{textutils.serialise}, this should not
contain recursive tables or functions.
@tparam[1,opt] {
    nbt_style? = boolean,
    unicode_strings? = boolean,
    allow_repetitions? = boolean
} options Options for serialisation.
 - `nbt_style`: Whether to produce NBT-style JSON (non-quoted keys) instead of standard JSON.
 - `unicode_strings`: Whether to treat strings as containing UTF-8 characters instead of
    using the default 8-bit character set.
 - `allow_repetitions`: Relax the check for recursive tables, allowing them to appear multiple
   times (as long as tables do not appear inside themselves).

@param[2] t The value to serialise. Like @{textutils.serialise}, this should not
contain recursive tables or functions.
@tparam[2] boolean bNBTStyle Whether to produce NBT-style JSON (non-quoted keys)
instead of standard JSON.

@treturn string The JSON representation of the input.
@throws If the object contains a value which cannot be serialised. This includes
functions and tables which appear multiple times.

@usage Serialise a simple object
    
    textutils.serialiseJSON({ values = { 1, "2", true } })

@usage Serialise an object to a NBT-style string
    
    textutils.serialiseJSON({ values = { 1, "2", true } }, { nbt_style = true })

@since 1.7
@changed 1.106.0 Added `options` overload and `unicode_strings` option.
@changed 1.109.0 Added `allow_repetitions` option.

@see textutils.json_null Use to serialise a JSON `null` value.
@see textutils.empty_json_array Use to serialise a JSON empty array.
]]
function serializeJSON(t, options)
    expect(1, t, "table", "string", "number", "boolean")
    expect(2, options, "table", "boolean", "nil")
    if type(options) == "boolean" then
        options = { nbt_style = options }
    elseif type(options) == "table" then
        field(options, "nbt_style", "boolean", "nil")
        field(options, "unicode_strings", "boolean", "nil")
        field(options, "allow_repetitions", "boolean", "nil")
    else
        options = {}
    end

    local tTracking = {}
    return serializeJSONImpl(t, tTracking, options)
end

serialiseJSON = serializeJSON -- GB version

unserializeJSON = unserialise_json
unserialiseJSON = unserialise_json

--- Replaces certain characters in a string to make it safe for use in URLs or POST data.
--
-- @tparam string str The string to encode
-- @treturn string The encoded string.
-- @usage print("https://example.com/?view=" .. textutils.urlEncode("some text&things"))
-- @since 1.31
function urlEncode(str)
    expect(1, str, "string")
    if str then
        str = string.gsub(str, "\n", "\r\n")
        str = string.gsub(str, "([^A-Za-z0-9 %-%_%.])", function(c)
            local n = string.byte(c)
            if n < 128 then
                -- ASCII
                return string.format("%%%02X", n)
            else
                -- Non-ASCII (encode as UTF-8)
                return
                    string.format("%%%02X", 192 + bit32.band(bit32.arshift(n, 6), 31)) ..
                    string.format("%%%02X", 128 + bit32.band(n, 63))
            end
        end)
        str = string.gsub(str, " ", "+")
    end
    return str
end

local tEmpty = {}

--- Provides a list of possible completions for a partial Lua expression.
--
-- If the completed element is a table, suggestions will have `.` appended to
-- them. Similarly, functions have `(` appended to them.
--
-- @tparam string sSearchText The partial expression to complete, such as a
-- variable name or table index.
--
-- @tparam[opt] table tSearchTable The table to find variables in, defaulting to
-- the global environment (@{_G}). The function also searches the "parent"
-- environment via the `__index` metatable field.
--
-- @treturn { string... } The (possibly empty) list of completions.
-- @see shell.setCompletionFunction
-- @see _G.read
-- @usage textutils.complete( "pa", _ENV )
-- @since 1.74
function complete(sSearchText, tSearchTable)
    expect(1, sSearchText, "string")
    expect(2, tSearchTable, "table", "nil")

    if g_tLuaKeywords[sSearchText] then return tEmpty end
    local nStart = 1
    local nDot = string.find(sSearchText, ".", nStart, true)
    local tTable = tSearchTable or _ENV
    while nDot do
        local sPart = string.sub(sSearchText, nStart, nDot - 1)
        local value = tTable[sPart]
        if type(value) == "table" then
            tTable = value
            nStart = nDot + 1
            nDot = string.find(sSearchText, ".", nStart, true)
        else
            return tEmpty
        end
    end
    local nColon = string.find(sSearchText, ":", nStart, true)
    if nColon then
        local sPart = string.sub(sSearchText, nStart, nColon - 1)
        local value = tTable[sPart]
        if type(value) == "table" then
            tTable = value
            nStart = nColon + 1
        else
            return tEmpty
        end
    end

    local sPart = string.sub(sSearchText, nStart)
    local nPartLength = #sPart

    local tResults = {}
    local tSeen = {}
    while tTable do
        for k, v in pairs(tTable) do
            if not tSeen[k] and type(k) == "string" then
                if string.find(k, sPart, 1, true) == 1 then
                    if not g_tLuaKeywords[k] and string.match(k, "^[%a_][%a%d_]*$") then
                        local sResult = string.sub(k, nPartLength + 1)
                        if nColon then
                            if type(v) == "function" then
                                table.insert(tResults, sResult .. "(")
                            elseif type(v) == "table" then
                                local tMetatable = getmetatable(v)
                                if tMetatable and (type(tMetatable.__call) == "function" or  type(tMetatable.__call) == "table") then
                                    table.insert(tResults, sResult .. "(")
                                end
                            end
                        else
                            if type(v) == "function" then
                                sResult = sResult .. "("
                            elseif type(v) == "table" and next(v) ~= nil then
                                sResult = sResult .. "."
                            end
                            table.insert(tResults, sResult)
                        end
                    end
                end
            end
            tSeen[k] = true
        end
        local tMetatable = getmetatable(tTable)
        if tMetatable and type(tMetatable.__index) == "table" then
            tTable = tMetatable.__index
        else
            tTable = nil
        end
    end

    table.sort(tResults)
    return tResults
end

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
function define(name, options)
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
function undefine(name)
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
function set(name, value)
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
function get(name, default)
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
function getDetails(name)
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
function unset(name)
    expect(1, name, "string")
    set_value(name, nil)
end

--- Resets the value of all settings. Equivalent to calling @{settings.unset}
--- on every setting.
--
-- @see settings.unset
function clear()
    for name in pairs(values) do
        set_value(name, nil)
    end
end

--- Get the names of all currently defined settings.
--
-- @treturn { string } An alphabetically sorted list of all currently-defined
-- settings.
function getNames()
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
function load(sPath)
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
function save(sPath)
    expect(1, sPath, "string", "nil")
    local file = fs.open(sPath or ".settings", "w")
    if not file then
        return false
    end

    file.write(textutils.serialize(values))
    file.close()

    return true
end

end
preload["rednet"] = function(...)
-- SPDX-FileCopyrightText: 2017 Daniel Ratcliffe
--
-- SPDX-License-Identifier: LicenseRef-CCPL

--[[- The Rednet API allows computers to communicate between each other by using
@{modem|modems}. It provides a layer of abstraction on top of the main @{modem}
peripheral, making it slightly easier to use.

## Basic usage
In order to send a message between two computers, each computer must have a
modem on one of its sides (or in the case of pocket computers and turtles, the
modem must be equipped as an upgrade). The two computers should then call
@{rednet.open}, which sets up the modems ready to send and receive messages.

Once rednet is opened, you can send messages using @{rednet.send} and receive
them using @{rednet.receive}. It's also possible to send a message to _every_
rednet-using computer using @{rednet.broadcast}.

:::caution Network security

While rednet provides a friendly way to send messages to specific computers, it
doesn't provide any guarantees about security. Other computers could be
listening in to your messages, or even pretending to send messages from other computers!

If you're playing on a multi-player server (or at least one where you don't
trust other players), it's worth encrypting or signing your rednet messages.
:::

## Protocols and hostnames
Several rednet messages accept "protocol"s - simple string names describing what
a message is about. When sending messages using @{rednet.send} and
@{rednet.broadcast}, you can optionally specify a protocol for the message. This
same protocol can then be given to @{rednet.receive}, to ignore all messages not
using this protocol.

It's also possible to look-up computers based on protocols, providing a basic
system for service discovery and [DNS]. A computer can advertise that it
supports a particular protocol with @{rednet.host}, also providing a friendly
"hostname". Other computers may then find all computers which support this
protocol using @{rednet.lookup}.

[DNS]: https://en.wikipedia.org/wiki/Domain_Name_System "Domain Name System"

@module rednet
@since 1.2
@see rednet_message Queued when a rednet message is received.
@see modem Rednet is built on top of the modem peripheral. Modems provide a more
bare-bones but flexible interface.
]]

local expect = dofile("rom/modules/main/cc/expect.lua").expect

--- The channel used by the Rednet API to @{broadcast} messages.
CHANNEL_BROADCAST = 65535

--- The channel used by the Rednet API to repeat messages.
CHANNEL_REPEAT = 65533

--- The number of channels rednet reserves for computer IDs. Computers with IDs
-- greater or equal to this limit wrap around to 0.
MAX_ID_CHANNELS = 65500

local received_messages = {}
local hostnames = {}
local prune_received_timer

local function id_as_channel(id)
    return (id or os.getComputerID()) % MAX_ID_CHANNELS
end

--[[- Opens a modem with the given @{peripheral} name, allowing it to send and
receive messages over rednet.

This will open the modem on two channels: one which has the same
@{os.getComputerID|ID} as the computer, and another on
@{CHANNEL_BROADCAST|the broadcast channel}.

@tparam string modem The name of the modem to open.
@throws If there is no such modem with the given name
@usage Open rednet on the back of the computer, allowing you to send and receive
rednet messages using it.

    rednet.open("back")

@usage Open rednet on all attached modems. This abuses the "filter" argument to
@{peripheral.find}.

    peripheral.find("modem", rednet.open)
@see rednet.close
@see rednet.isOpen
]]
function open(modem)
    expect(1, modem, "string")
    if peripheral.getType(modem) ~= "modem" then
        error("No such modem: " .. modem, 2)
    end
    peripheral.call(modem, "open", id_as_channel())
    peripheral.call(modem, "open", CHANNEL_BROADCAST)
end

--- Close a modem with the given @{peripheral} name, meaning it can no longer
-- send and receive rednet messages.
--
-- @tparam[opt] string modem The side the modem exists on. If not given, all
-- open modems will be closed.
-- @throws If there is no such modem with the given name
-- @see rednet.open
function close(modem)
    expect(1, modem, "string", "nil")
    if modem then
        -- Close a specific modem
        if peripheral.getType(modem) ~= "modem" then
            error("No such modem: " .. modem, 2)
        end
        peripheral.call(modem, "close", id_as_channel())
        peripheral.call(modem, "close", CHANNEL_BROADCAST)
    else
        -- Close all modems
        for _, modem in ipairs(peripheral.getNames()) do
            if isOpen(modem) then
                close(modem)
            end
        end
    end
end

--- Determine if rednet is currently open.
--
-- @tparam[opt] string modem Which modem to check. If not given, all connected
-- modems will be checked.
-- @treturn boolean If the given modem is open.
-- @since 1.31
-- @see rednet.open
function isOpen(modem)
    expect(1, modem, "string", "nil")
    if modem then
        -- Check if a specific modem is open
        if peripheral.getType(modem) == "modem" then
            return peripheral.call(modem, "isOpen", id_as_channel()) and peripheral.call(modem, "isOpen", CHANNEL_BROADCAST)
        end
    else
        -- Check if any modem is open
        for _, modem in ipairs(peripheral.getNames()) do
            if isOpen(modem) then
                return true
            end
        end
    end
    return false
end

--[[- Allows a computer or turtle with an attached modem to send a message
intended for a sycomputer with a specific ID. At least one such modem must first
be @{rednet.open|opened} before sending is possible.

Assuming the target was in range and also had a correctly opened modem, the
target computer may then use @{rednet.receive} to collect the message.

@tparam number recipient The ID of the receiving computer.
@param message The message to send. Like with @{modem.transmit}, this can
contain any primitive type (numbers, booleans and strings) as well as
tables. Other types (like functions), as well as metatables, will not be
transmitted.
@tparam[opt] string protocol The "protocol" to send this message under. When
using @{rednet.receive} one can filter to only receive messages sent under a
particular protocol.
@treturn boolean If this message was successfully sent (i.e. if rednet is
currently @{rednet.open|open}). Note, this does not guarantee the message was
actually _received_.
@changed 1.6 Added protocol parameter.
@changed 1.82.0 Now returns whether the message was successfully sent.
@see rednet.receive
@usage Send a message to computer #2.

    rednet.send(2, "Hello from rednet!")
]]
function send(recipient, message, protocol)
    expect(1, recipient, "number")
    expect(3, protocol, "string", "nil")
    -- Generate a (probably) unique message ID
    -- We could do other things to guarantee uniqueness, but we really don't need to
    -- Store it to ensure we don't get our own messages back
    local message_id = math.random(1, 2147483647)
    received_messages[message_id] = os.clock() + 9.5
    if not prune_received_timer then prune_received_timer = os.startTimer(10) end

    -- Create the message
    local reply_channel = id_as_channel()
    local message_wrapper = {
        nMessageID = message_id,
        nRecipient = recipient,
        nSender = os.getComputerID(),
        message = message,
        sProtocol = protocol,
    }

    local sent = false
    if recipient == os.getComputerID() then
        -- Loopback to ourselves
        os.queueEvent("rednet_message", os.getComputerID(), message, protocol)
        sent = true
    else
        -- Send on all open modems, to the target and to repeaters
        if recipient ~= CHANNEL_BROADCAST then
            recipient = id_as_channel(recipient)
        end

        for _, modem in ipairs(peripheral.getNames()) do
            if isOpen(modem) then
                peripheral.call(modem, "transmit", recipient, reply_channel, message_wrapper)
                peripheral.call(modem, "transmit", CHANNEL_REPEAT, reply_channel, message_wrapper)
                sent = true
            end
        end
    end

    return sent
end

--[[- Broadcasts a string message over the predefined @{CHANNEL_BROADCAST}
channel. The message will be received by every device listening to rednet.

@param message The message to send. This should not contain coroutines or
functions, as they will be converted to @{nil}.  @tparam[opt] string protocol
The "protocol" to send this message under. When using @{rednet.receive} one can
filter to only receive messages sent under a particular protocol.
@see rednet.receive
@changed 1.6 Added protocol parameter.
@usage Broadcast the words "Hello, world!" to every computer using rednet.

    rednet.broadcast("Hello, world!")
]]
function broadcast(message, protocol)
    expect(2, protocol, "string", "nil")
    send(CHANNEL_BROADCAST, message, protocol)
end

--[[- Wait for a rednet message to be received, or until `nTimeout` seconds have
elapsed.

@tparam[opt] string protocol_filter The protocol the received message must be
sent with. If specified, any messages not sent under this protocol will be
discarded.
@tparam[opt] number timeout The number of seconds to wait if no message is
received.
@treturn[1] number The computer which sent this message
@return[1] The received message
@treturn[1] string|nil The protocol this message was sent under.
@treturn[2] nil If the timeout elapsed and no message was received.
@see rednet.broadcast
@see rednet.send
@changed 1.6 Added protocol filter parameter.
@usage Receive a rednet message.

    local id, message = rednet.receive()
    print(("Computer %d sent message %s"):format(id, message))

@usage Receive a message, stopping after 5 seconds if no message was received.

    local id, message = rednet.receive(nil, 5)
    if not id then
        printError("No message received")
    else
        print(("Computer %d sent message %s"):format(id, message))
    end

@usage Receive a message from computer #2.

    local id, message
    repeat
        id, message = rednet.receive()
    until id == 2

    print(message)
]]
function receive(protocol_filter, timeout)
    -- The parameters used to be ( nTimeout ), detect this case for backwards compatibility
    if type(protocol_filter) == "number" and timeout == nil then
        protocol_filter, timeout = nil, protocol_filter
    end
    expect(1, protocol_filter, "string", "nil")
    expect(2, timeout, "number", "nil")

    -- Start the timer
    local timer = nil
    local event_filter = nil
    if timeout then
        timer = os.startTimer(timeout)
        event_filter = nil
    else
        event_filter = "rednet_message"
    end

    -- Wait for events
    while true do
        local event, p1, p2, p3 = os.pullEvent(event_filter)
        if event == "rednet_message" then
            -- Return the first matching rednet_message
            local sender_id, message, protocol = p1, p2, p3
            if protocol_filter == nil or protocol == protocol_filter then
                return sender_id, message, protocol
            end
        elseif event == "timer" then
            -- Return nil if we timeout
            if p1 == timer then
                return nil
            end
        end
    end
end

--[[- Register the system as "hosting" the desired protocol under the specified
name. If a rednet @{rednet.lookup|lookup} is performed for that protocol (and
maybe name) on the same network, the registered system will automatically
respond via a background process, hence providing the system performing the
lookup with its ID number.

Multiple computers may not register themselves on the same network as having the
same names against the same protocols, and the title `localhost` is specifically
reserved. They may, however, share names as long as their hosted protocols are
different, or if they only join a given network after "registering" themselves
before doing so (eg while offline or part of a different network).

@tparam string protocol The protocol this computer provides.
@tparam string hostname The name this computer exposes for the given protocol.
@throws If trying to register a hostname which is reserved, or currently in use.
@see rednet.unhost
@see rednet.lookup
@since 1.6
]]
function host(protocol, hostname)
    expect(1, protocol, "string")
    expect(2, hostname, "string")
    if hostname == "localhost" then
        error("Reserved hostname", 2)
    end
    if hostnames[protocol] ~= hostname then
        if lookup(protocol, hostname) ~= nil then
            error("Hostname in use", 2)
        end
        hostnames[protocol] = hostname
    end
end

--- Stop @{rednet.host|hosting} a specific protocol, meaning it will no longer
-- respond to @{rednet.lookup} requests.
--
-- @tparam string protocol The protocol to unregister your self from.
-- @since 1.6
function unhost(protocol)
    expect(1, protocol, "string")
    hostnames[protocol] = nil
end

--[[- Search the local rednet network for systems @{rednet.host|hosting} the
desired protocol and returns any computer IDs that respond as "registered"
against it.

If a hostname is specified, only one ID will be returned (assuming an exact
match is found).

@tparam string protocol The protocol to search for.
@tparam[opt] string hostname The hostname to search for.

@treturn[1] number... A list of computer IDs hosting the given protocol.
@treturn[2] number|nil The computer ID with the provided hostname and protocol,
or @{nil} if none exists.
@since 1.6
@usage Find all computers which are hosting the `"chat"` protocol.

    local computers = {rednet.lookup("chat")}
    print(#computers .. " computers available to chat")
    for _, computer in pairs(computers) do
      print("Computer #" .. computer)
    end

@usage Find a computer hosting the `"chat"` protocol with a hostname of `"my_host"`.

    local id = rednet.lookup("chat", "my_host")
    if id then
      print("Found my_host at computer #" .. id)
    else
      printError("Cannot find my_host")
    end

]]
function lookup(protocol, hostname)
    expect(1, protocol, "string")
    expect(2, hostname, "string", "nil")

    -- Build list of host IDs
    local results = nil
    if hostname == nil then
        results = {}
    end

    -- Check localhost first
    if hostnames[protocol] then
        if hostname == nil then
            table.insert(results, os.getComputerID())
        elseif hostname == "localhost" or hostname == hostnames[protocol] then
            return os.getComputerID()
        end
    end

    if not isOpen() then
        if results then
            return table.unpack(results)
        end
        return nil
    end

    -- Broadcast a lookup packet
    broadcast({
        sType = "lookup",
        sProtocol = protocol,
        sHostname = hostname,
    }, "dns")

    -- Start a timer
    local timer = os.startTimer(2)

    -- Wait for events
    while true do
        local event, p1, p2, p3 = os.pullEvent()
        if event == "rednet_message" then
            -- Got a rednet message, check if it's the response to our request
            local sender_id, message, message_protocol = p1, p2, p3
            if message_protocol == "dns" and type(message) == "table" and message.sType == "lookup response" then
                if message.sProtocol == protocol then
                    if hostname == nil then
                        table.insert(results, sender_id)
                    elseif message.sHostname == hostname then
                        return sender_id
                    end
                end
            end
        elseif event == "timer" and p1 == timer then
            -- Got a timer event, check it's the end of our timeout
            break
        end
    end
    if results then
        return table.unpack(results)
    end
    return nil
end

local started = false

--- Listen for modem messages and converts them into rednet messages, which may
-- then be @{receive|received}.
--
-- This is automatically started in the background on computer startup, and
-- should not be called manually.
function run()
    if started then
        error("rednet is already running", 2)
    end
    started = true

    while true do
        local event, p1, p2, p3, p4 = os.pullEventRaw()
        if event == "modem_message" then
            -- Got a modem message, process it and add it to the rednet event queue
            local modem, channel, reply_channel, message = p1, p2, p3, p4
            if channel == id_as_channel() or channel == CHANNEL_BROADCAST then
                if type(message) == "table" and type(message.nMessageID) == "number"
                    and message.nMessageID == message.nMessageID and not received_messages[message.nMessageID]
                    and (type(message.nSender) == "nil" or (type(message.nSender) == "number" and message.nSender == message.nSender))
                    and ((message.nRecipient and message.nRecipient == os.getComputerID()) or channel == CHANNEL_BROADCAST)
                    and isOpen(modem)
                then
                    received_messages[message.nMessageID] = os.clock() + 9.5
                    if not prune_received_timer then prune_received_timer = os.startTimer(10) end
                    os.queueEvent("rednet_message", message.nSender or reply_channel, message.message, message.sProtocol)
                end
            end

        elseif event == "rednet_message" then
            -- Got a rednet message (queued from above), respond to dns lookup
            local sender, message, protocol = p1, p2, p3
            if protocol == "dns" and type(message) == "table" and message.sType == "lookup" then
                local hostname = hostnames[message.sProtocol]
                if hostname ~= nil and (message.sHostname == nil or message.sHostname == hostname) then
                    send(sender, {
                        sType = "lookup response",
                        sHostname = hostname,
                        sProtocol = message.sProtocol,
                    }, "dns")
                end
            end

        elseif event == "timer" and p1 == prune_received_timer then
            -- Got a timer event, use it to prune the set of received messages
            prune_received_timer = nil
            local now, has_more = os.clock(), nil
            for message_id, deadline in pairs(received_messages) do
                if deadline <= now then received_messages[message_id] = nil
                else has_more = true end
            end
            prune_received_timer = has_more and os.startTimer(10)
        end
    end
end

end
preload["peripheral"] = function(...)
-- SPDX-FileCopyrightText: 2017 Daniel Ratcliffe
--
-- SPDX-License-Identifier: LicenseRef-CCPL

--[[- Peripherals are blocks (or turtle and pocket computer upgrades) which can
be controlled by a computer. For instance, the @{speaker} peripheral allows a
computer to play music and the @{monitor} peripheral allows you to display text
in the world.

## Referencing peripherals

Computers can interact with adjacent peripherals. Each peripheral is given a
name based on which direction it is in. For instance, a disk drive below your
computer will be called `"bottom"` in your Lua code, one to the left called
`"left"` , and so on for all 6 directions (`"bottom"`, `"top"`, `"left"`,
`"right"`, `"front"`, `"back"`).

You can list the names of all peripherals with the `peripherals` program, or the
@{peripheral.getNames} function.

It's also possible to use peripherals which are further away from your computer
through the use of @{modem|Wired Modems}. Place one modem against your computer,
run Networking Cable to your peripheral, and then place another modem against
that block. You can then right click the modem to use (or *attach*) the
peripheral. This will print a peripheral name to chat, which can then be used
just like a direction name to access the peripheral. You can click on the message
to copy the name to your clipboard.

## Using peripherals

Once you have the name of a peripheral, you can call functions on it using the
@{peripheral.call} function. This takes the name of our peripheral, the name of
the function we want to call, and then its arguments.

:::info
Some bits of the peripheral API call peripheral functions *methods* instead
(for example, the @{peripheral.getMethods} function). Don't worry, they're the
same thing!
:::

Let's say we have a monitor above our computer (and so "top") and want to
@{monitor.write|write some text to it}. We'd write the following:

```lua
peripheral.call("top", "write", "This is displayed on a monitor!")
```

Once you start calling making a couple of peripheral calls this can get very
repetitive, and so we can @{peripheral.wrap|wrap} a peripheral. This builds a
table of all the peripheral's functions so you can use it like an API or module.

For instance, we could have written the above example as follows:

```lua
local my_monitor = peripheral.wrap("top")
my_monitor.write("This is displayed on a monitor!")
```

## Finding peripherals

Sometimes when you're writing a program you don't care what a peripheral is
called, you just need to know it's there. For instance, if you're writing a
music player, you just need a speaker - it doesn't matter if it's above or below
the computer.

Thankfully there's a quick way to do this: @{peripheral.find}. This takes a
*peripheral type* and returns all the attached peripherals which are of this
type.

What is a peripheral type though? This is a string which describes what a
peripheral is, and so what functions are available on it. For instance, speakers
are just called `"speaker"`, and monitors `"monitor"`. Some peripherals might
have more than one type - a Minecraft chest is both a `"minecraft:chest"` and
`"inventory"`.

You can get all the types a peripheral has with @{peripheral.getType}, and check
a peripheral is a specific type with @{peripheral.hasType}.

To return to our original example, let's use @{peripheral.find} to find an
attached speaker:

```lua
local speaker = peripheral.find("speaker")
speaker.playNote("harp")
```

@module peripheral
@see event!peripheral This event is fired whenever a new peripheral is attached.
@see event!peripheral_detach This event is fired whenever a peripheral is detached.
@since 1.3
@changed 1.51 Add support for wired modems.
@changed 1.99 Peripherals can have multiple types.
]]

local expect = dofile("rom/modules/main/cc/expect.lua").expect

local native = peripheral
local sides = rs.getSides()


--- Provides a list of all peripherals available.
--
-- If a device is located directly next to the system, then its name will be
-- listed as the side it is attached to. If a device is attached via a Wired
-- Modem, then it'll be reported according to its name on the wired network.
--
-- @treturn { string... } A list of the names of all attached peripherals.
-- @since 1.51
function getNames()
    local results = {}
    for n = 1, #sides do
        local side = sides[n]
        if native.isPresent(side) then
            table.insert(results, side)
            if native.hasType(side, "peripheral_hub") then
                local remote = native.call(side, "getNamesRemote")
                for _, name in ipairs(remote) do
                    table.insert(results, name)
                end
            end
        end
    end
    return results
end

--- Determines if a peripheral is present with the given name.
--
-- @tparam string name The side or network name that you want to check.
-- @treturn boolean If a peripheral is present with the given name.
-- @usage peripheral.isPresent("top")
-- @usage peripheral.isPresent("monitor_0")
function isPresent(name)
    expect(1, name, "string")
    if native.isPresent(name) then
        return true
    end

    for n = 1, #sides do
        local side = sides[n]
        if native.hasType(side, "peripheral_hub") and native.call(side, "isPresentRemote", name) then
            return true
        end
    end
    return false
end

--[[- Get the types of a named or wrapped peripheral.

@tparam string|table peripheral The name of the peripheral to find, or a
wrapped peripheral instance.
@treturn string... The peripheral's types, or `nil` if it is not present.
@changed 1.88.0 Accepts a wrapped peripheral as an argument.
@changed 1.99 Now returns multiple types.
@usage Get the type of a peripheral above this computer.

    peripheral.getType("top")
]]
function getType(peripheral)
    expect(1, peripheral, "string", "table")
    if type(peripheral) == "string" then -- Peripheral name passed
        if native.isPresent(peripheral) then
            return native.getType(peripheral)
        end
        for n = 1, #sides do
            local side = sides[n]
            if native.hasType(side, "peripheral_hub") and native.call(side, "isPresentRemote", peripheral) then
                return native.call(side, "getTypeRemote", peripheral)
            end
        end
        return nil
    else
        local mt = getmetatable(peripheral)
        if not mt or mt.__name ~= "peripheral" or type(mt.types) ~= "table" then
            error("bad argument #1 (table is not a peripheral)", 2)
        end
        return table.unpack(mt.types)
    end
end

--[[- Check if a peripheral is of a particular type.

@tparam string|table peripheral The name of the peripheral or a wrapped peripheral instance.
@tparam string peripheral_type The type to check.

@treturn boolean|nil If a peripheral has a particular type, or `nil` if it is not present.
@since 1.99
]]
function hasType(peripheral, peripheral_type)
    expect(1, peripheral, "string", "table")
    expect(2, peripheral_type, "string")
    if type(peripheral) == "string" then -- Peripheral name passed
        if native.isPresent(peripheral) then
            return native.hasType(peripheral, peripheral_type)
        end
        for n = 1, #sides do
            local side = sides[n]
            if native.hasType(side, "peripheral_hub") and native.call(side, "isPresentRemote", peripheral) then
                return native.call(side, "hasTypeRemote", peripheral, peripheral_type)
            end
        end
        return nil
    else
        local mt = getmetatable(peripheral)
        if not mt or mt.__name ~= "peripheral" or type(mt.types) ~= "table" then
            error("bad argument #1 (table is not a peripheral)", 2)
        end
        return mt.types[peripheral_type] ~= nil
    end
end

--- Get all available methods for the peripheral with the given name.
--
-- @tparam string name The name of the peripheral to find.
-- @treturn { string... }|nil A list of methods provided by this peripheral, or `nil` if
-- it is not present.
function getMethods(name)
    expect(1, name, "string")
    if native.isPresent(name) then
        return native.getMethods(name)
    end
    for n = 1, #sides do
        local side = sides[n]
        if native.hasType(side, "peripheral_hub") and native.call(side, "isPresentRemote", name) then
            return native.call(side, "getMethodsRemote", name)
        end
    end
    return nil
end

--- Get the name of a peripheral wrapped with @{peripheral.wrap}.
--
-- @tparam table peripheral The peripheral to get the name of.
-- @treturn string The name of the given peripheral.
-- @since 1.88.0
function getName(peripheral)
    expect(1, peripheral, "table")
    local mt = getmetatable(peripheral)
    if not mt or mt.__name ~= "peripheral" or type(mt.name) ~= "string" then
        error("bad argument #1 (table is not a peripheral)", 2)
    end
    return mt.name
end

--- Call a method on the peripheral with the given name.
--
-- @tparam string name The name of the peripheral to invoke the method on.
-- @tparam string method The name of the method
-- @param ... Additional arguments to pass to the method
-- @return The return values of the peripheral method.
--
-- @usage Open the modem on the top of this computer.
--
--     peripheral.call("top", "open", 1)
function call(name, method, ...)
    expect(1, name, "string")
    expect(2, method, "string")
    if native.isPresent(name) then
        return native.call(name, method, ...)
    end

    for n = 1, #sides do
        local side = sides[n]
        if native.hasType(side, "peripheral_hub") and native.call(side, "isPresentRemote", name) then
            return native.call(side, "callRemote", name, method, ...)
        end
    end
    return nil
end

--- Get a table containing all functions available on a peripheral. These can
-- then be called instead of using @{peripheral.call} every time.
--
-- @tparam string name The name of the peripheral to wrap.
-- @treturn table|nil The table containing the peripheral's methods, or `nil` if
-- there is no peripheral present with the given name.
-- @usage Open the modem on the top of this computer.
--
--     local modem = peripheral.wrap("top")
--     modem.open(1)
function wrap(name)
    expect(1, name, "string")

    local methods = peripheral.getMethods(name)
    if not methods then
        return nil
    end

    -- We store our types array as a list (for getType) and a lookup table (for hasType).
    local types = { peripheral.getType(name) }
    for i = 1, #types do types[types[i]] = true end
    local result = setmetatable({}, {
        __name = "peripheral",
        name = name,
        type = types[1],
        types = types,
    })
    for _, method in ipairs(methods) do
        result[method] = function(...)
            return peripheral.call(name, method, ...)
        end
    end
    return result
end

--[[- Find all peripherals of a specific type, and return the
@{peripheral.wrap|wrapped} peripherals.

@tparam string ty The type of peripheral to look for.
@tparam[opt] function(name:string, wrapped:table):boolean filter A
filter function, which takes the peripheral's name and wrapped table
and returns if it should be included in the result.
@treturn table... 0 or more wrapped peripherals matching the given filters.
@usage Find all monitors and store them in a table, writing "Hello" on each one.

    local monitors = { peripheral.find("monitor") }
    for _, monitor in pairs(monitors) do
      monitor.write("Hello")
    end

@usage Find all wireless modems connected to this computer.

    local modems = { peripheral.find("modem", function(name, modem)
        return modem.isWireless() -- Check this modem is wireless.
    end) }

@usage This abuses the `filter` argument to call @{rednet.open} on every modem.

    peripheral.find("modem", rednet.open)
@since 1.6
]]
function find(ty, filter)
    expect(1, ty, "string")
    expect(2, filter, "function", "nil")

    local results = {}
    for _, name in ipairs(peripheral.getNames()) do
        if peripheral.hasType(name, ty) then
            local wrapped = peripheral.wrap(name)
            if filter == nil or filter(name, wrapped) then
                table.insert(results, wrapped)
            end
        end
    end
    return table.unpack(results)
end

end
preload["parallel"] = function(...)
-- SPDX-FileCopyrightText: 2017 Daniel Ratcliffe
--
-- SPDX-License-Identifier: LicenseRef-CCPL

--[[- Provides a simple implementation of multitasking.

Functions are not actually executed simultaniously, but rather this API will
automatically switch between them whenever they yield (eg whenever they call
@{coroutine.yield}, or functions that call that - eg @{os.pullEvent} - or
functions that call that, etc - basically, anything that causes the function
to "pause").

Each function executed in "parallel" gets its own copy of the event queue,
and so "event consuming" functions (again, mostly anything that causes the
script to pause - eg @{sleep}, @{rednet.receive}, most of the @{turtle} API,
etc) can safely be used in one without affecting the event queue accessed by
the other.

@module parallel
@since 1.2
]]

local function create(...)
    local tFns = table.pack(...)
    local tCos = {}
    for i = 1, tFns.n, 1 do
        local fn = tFns[i]
        if type(fn) ~= "function" then
            error("bad argument #" .. i .. " (function expected, got " .. type(fn) .. ")", 3)
        end

        tCos[i] = coroutine.create(fn)
    end

    return tCos
end

local function runUntilLimit(_routines, _limit)
    local count = #_routines
    if count < 1 then return 0 end
    local living = count

    local tFilters = {}
    local eventData = { n = 0 }
    while true do
        for n = 1, count do
            local r = _routines[n]
            if r then
                if tFilters[r] == nil or tFilters[r] == eventData[1] or eventData[1] == "terminate" then
                    local ok, param = coroutine.resume(r, table.unpack(eventData, 1, eventData.n))
                    if not ok then
                        error(param, 0)
                    else
                        tFilters[r] = param
                    end
                    if coroutine.status(r) == "dead" then
                        _routines[n] = nil
                        living = living - 1
                        if living <= _limit then
                            return n
                        end
                    end
                end
            end
        end
        for n = 1, count do
            local r = _routines[n]
            if r and coroutine.status(r) == "dead" then
                _routines[n] = nil
                living = living - 1
                if living <= _limit then
                    return n
                end
            end
        end
        eventData = table.pack(os.pullEventRaw())
    end
end

--[[- Switches between execution of the functions, until any of them
finishes. If any of the functions errors, the message is propagated upwards
from the @{parallel.waitForAny} call.

@tparam function ... The functions this task will run
@usage Print a message every second until the `q` key is pressed.

    local function tick()
        while true do
            os.sleep(1)
            print("Tick")
        end
    end
    local function wait_for_q()
        repeat
            local _, key = os.pullEvent("key")
        until key == keys.q
        print("Q was pressed!")
    end

    parallel.waitForAny(tick, wait_for_q)
    print("Everything done!")
]]
function waitForAny(...)
    local routines = create(...)
    return runUntilLimit(routines, #routines - 1)
end

--[[- Switches between execution of the functions, until all of them are
finished. If any of the functions errors, the message is propagated upwards
from the @{parallel.waitForAll} call.

@tparam function ... The functions this task will run
@usage Start off two timers and wait for them both to run.

    local function a()
        os.sleep(1)
        print("A is done")
    end
    local function b()
        os.sleep(3)
        print("B is done")
    end

    parallel.waitForAll(a, b)
    print("Everything done!")
]]
function waitForAll(...)
    local routines = create(...)
    return runUntilLimit(routines, 0)
end

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
function parseImage(image)
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
function loadImage(path)
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
function drawPixel(xPos, yPos, colour)
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
function drawLine(startX, startY, endX, endY, colour)
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
function drawBox(startX, startY, endX, endY, nColour)
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
function drawFilledBox(startX, startY, endX, endY, nColour)
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
function drawImage(image, xPos, yPos)
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

local keys = _ENV
for nKey, sKey in pairs( tKeys ) do
	keys[sKey] = nKey
end
keys["return"] = keys.enter
-- (Un-)fix some misspellings
keys.scollLock = keys.scrollLock
keys.cimcumflex = keys.circumflex

function getName( _nKey )
    if type( _nKey ) ~= "number" then
        error( "bad argument #1 (number expected, got " .. type( _nKey ) .. ")", 2 ) 
    end
	return tKeys[ _nKey ]
end

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
function get(_url, _headers, _binary)
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
function post(_url, _post, _headers, _binary)
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
    _ENV[k:lower()] = function(_url, _post, _headers, _binary)
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
function request(_url, _post, _headers, _binary)
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
    function listen( _port, _callback )
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
checkURLAsync = nativeCheckURL

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
function checkURL(_url)
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
function websocketAsync(url, headers)
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
function websocket(_url, _headers)
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

addListener = native.addListener
removeListener = native.removeListener
websocketServer = native.websocketServer

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

local sPath = "/rom/help"

--- Returns a colon-separated list of directories where help files are searched
-- for. All directories are absolute.
--
-- @treturn string The current help search path, separated by colons.
-- @see help.setPath
function path()
    return sPath
end

--- Sets the colon-seperated list of directories where help files are searched
-- for to `newPath`
--
-- @tparam string newPath The new path to use.
-- @usage help.setPath( "/disk/help/" )
-- @usage help.setPath( help.path() .. ":/myfolder/help/" )
-- @see help.path
function setPath(_sPath)
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
function lookup(topic)
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
function topics()
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
function completeTopic(sText)
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

--- The channel which GPS requests and responses are broadcast on.
CHANNEL_GPS = 65534

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
function locate(_nTimeout, _bDebug)
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
function isPresent(name)
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
function getLabel(name)
    if isDrive(name) then
        return peripheral.call(name, "getDiskLabel")
    end
    return nil
end

--- Set the label of the floppy disk or other media
--
-- @tparam string name The name of the disk drive.
-- @tparam string|nil label The new label of the disk
function setLabel(name, label)
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
function hasData(name)
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
function getMountPath(name)
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
function hasAudio(name)
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
function getAudioTitle(name)
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
function playAudio(name)
    if isDrive(name) then
        peripheral.call(name, "playAudio")
    end
end

--- Stops the music record in the drive from playing, if it was started with
-- @{disk.playAudio}.
--
-- @tparam string name The name o the disk drive.
function stopAudio(name)
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
function eject(name)
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
function getID(name)
    if isDrive(name) then
        return peripheral.call(name, "getDiskID")
    end
    return nil
end

--- Inserts a disk into the drive.
--
-- This function takes one of the following path formats:
-- * A path to a directory to mount as a disk.
-- * A path to an audio file to insert a music disc.
-- * A number to mount a disk with an ID.
-- * A path in the form "treasure:<name>/<program>" to mount a treasure disk if available.
--
-- @tparam string name The name of the disk drive.
-- @tparam string path The path to mount as described above.
function insertDisk(name, path)
    if isDrive(name) then
        peripheral.call(name, "insertDisk", path)
    end
end

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

local colours = _ENV
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

end
preload["Howlfile"] = function(...)
Options:Default "trace"

Tasks:clean()

Tasks:minify "minify" {
	input = "/bin/userspace.lua",
	output = "/bin/userspace.min.lua",
}

Tasks:require "main" {
	include = "*.lua",
	startup = "userspace.lua",
	output = "/bin/userspace.lua",
}

Tasks:Task "build" { "clean", "minify" } :Description "Main build task"


Tasks:Default "main"
end
return preload["userspace"](...)

end
preload["Howlfile"] = function(...)
Options:Default "trace"

Tasks:clean()

Tasks:minify "minify" {
	input = "/bin/userspace.lua",
	output = "/bin/userspace.min.lua",
}

Tasks:require "main" {
	include = "*.lua",
	startup = "userspace.lua",
	output = "/bin/userspace.lua",
}

Tasks:Task "build" { "clean", "minify" } :Description "Main build task"


Tasks:Default "main"
end
return preload["userspace"](...)

end
preload["Howlfile"] = function(...)
Options:Default "trace"

Tasks:clean()

Tasks:minify "minify" {
	input = "/bin/userspace.lua",
	output = "/bin/userspace.min.lua",
}

Tasks:require "main" {
	include = "*.lua",
	startup = "userspace.lua",
	output = "/bin/userspace.lua",
}

Tasks:Task "build" { "clean", "minify" } :Description "Main build task"


Tasks:Default "main"
end
return preload["userspace"](...)

end
preload["Howlfile"] = function(...)
Options:Default "trace"

Tasks:clean()

Tasks:minify "minify" {
	input = "/bin/userspace.lua",
	output = "/bin/userspace.min.lua",
}

Tasks:require "main" {
	include = "*.lua",
	startup = "userspace.lua",
	output = "/bin/userspace.lua",
}

Tasks:Task "build" { "clean", "minify" } :Description "Main build task"


Tasks:Default "main"
end
return preload["userspace"](...)
