local component = require("component")
local robot = require("robot")
local side = require("sides")

-- North = 0, West = 1, South = 2, east = 3
local facing = 0
local coords = {0, 0, 0}

local function turnLeff ()
	robot.turnLeft()
	
	facing = facing + 1
	facing = facing % 4
end

local function turnRoight ()
	robot.turnRight()
	
	facing = facing - 1
	facing = facing % 4
end

local function turnArund ()
	robot.turnLeft()
	robot.turnLeft()
	
	facing = facing + 2
	facing = facing % 4
end

local function fwrd ()
	local a, b = robot.forward()
	
	if a then
		if facing == 0 then
			coords[1] = coords[1] + 1
		elseif facing == 1 then
			coords[3] = coords[3] - 1
		elseif facing == 2 then
			coords[1] = coords[1] - 1
		elseif facing == 3 then
			coords[3] = coords[3] + 1
		end
	end
	
	return a, b
end

local function bck ()
	local a, b = robot.back()
	
	if a then
		if facing == 0 then
			coords[1] = coords[1] - 1
		elseif facing == 1 then
			coords[3] = coords[3] + 1
		elseif facing == 2 then
			coords[1] = coords[1] + 1
		elseif facing == 3 then
			coords[3] = coords[3] - 1
		end
	end
	
	return a, b
end

local function upp ()
	local a, b = robot.up()
	
	if a then
		coords[2] = coords[2] + 1
	end
	
	return a, b
end

local function dwn ()
	local a, b = robot.down()
	
	if a then
		coords[2] = coords[2] - 1
	end
	
	return a, b
end