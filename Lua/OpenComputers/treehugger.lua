local component = require("component")
local robot = require("robot")
local side = require("sides")
local itemCntrl = component.inventory_controller

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


local function chkInvForItem (itemName, face)
	local invSz = itemCntrl.getInventorySize(face)
	
	if invSz ~= nil then
		local cntr = {} 
		
		for i = 1, invSz do 
			local itemPointer = itemCntrl.getStackInSlot(face, i)
			
			if itemPointer ~= nil and itemPointer.name == itemName then
				table.insert(cntr, i)
			end
		end
		
		if #cntr == 0 then
			return 0
		else 
			return cntr
		end
	else
		return nil 
	end
end

local function chkSelfForItem (itemName)
	local invSz = robot.inventorySize()

	if invSz ~= nil then
		local cntr = {} 
		
		for i = 1, invSz do 
			local itemPointer = itemCntrl.getStackInInternalSlot(i)
			
			if itemPointer ~= nil and itemPointer.name == itemName then
				table.insert(cntr, i)
			end
		end
		
		if #cntr == 0 then
			return 0
		else 
			return cntr
		end
	else
		return nil 
	end
end

local function scavengeInvForItem (itemName, face, maxAmount, maxStackSize) 
	if maxAmount == nil or maxAmount == "_" then
		maxAmount = math.huge
	end
	
	if maxAmount > 0 and maxStackSize > 0 then
		local itemTable = chkInvForItem(itemName, face)

		if itemTable ~= nil and itemTable ~= 0 then
			local selfInvSz = robot.inventorySize()
			local invSpace = 0
        
			for i = 1, selfInvSz do
				local peekedItem = itemCntrl.getStackInInternalSlot(i) 
            
				if peekedItem == nil then
					invSpace = invSpace + maxStackSize
				elseif peekedItem.name == itemName then
					invSpace = invSpace + (peekedItem.maxSize - peekedItem.size) 
				end
			end    
        
			if invSpace == 0 then
				return 0
			else 
				local amountLeft = maxAmount
            
				for i = 1, #itemTable do
					local peekedItem = itemCntrl.getStackInSlot(face, itemTable[i])
					
					invSpace = invSpace - peekedItem.size
					amountLeft = amountLeft - peekedItem.size
                
					if invSpace <= 0 then
						itemCntrl.suckFromSlot(face, itemTable[i], amountLeft + peekedItem.size)
						return {false, amountLeft}
					elseif amountLeft <= 0 then     
						itemCntrl.suckFromSlot(face, itemTable[i], amountLeft + peekedItem.size)
						return {true, 0}
					else 
						itemCntrl.suckFromSlot(face, itemTable[i], amountLeft + peekedItem.size)
					end
				end
            
				return {true, amountLeft}
			end
		elseif itemTable == 0 then
			return 0    
		else
			return nil
		end    
	else
		return nil
	end	
end

local function listContains (list, value) 
    for i = 1, #list do
        if list[i] == value then
            return true
        end
    end
    
    return false
end

local function dumpSelfToInv (itemBlackList, face, maxAmount)
    local invSize = itemCntrl.getInventorySize(face)
    
    if maxAmount == nil or maxAmount == "_" then
        maxAmount = math.huge
    end    
    
    if invSize ~= nil then
        local selfInvSize = robot.inventorySize()
        local origSlot = robot.select()
        local allowance = maxAmount
        
        for i = 1, selfInvSize do
            local itemPointer = itemCntrl.getStackInInternalSlot(i)
            
            if itemPointer ~= null and not listContains(itemBlackList, itemPointer.name) then
                allowance = allowance - itemPointer.size
                robot.select(i)
                
                for k = 1, invSize do
                    local chk, b = itemCntrl.dropIntoSlot(face, k, allowance + itemPointer.size)
                
                    if allowance <= 0 then
                        goto finishedTransfer
                    elseif chk then
                        break
                    end
                end
            end
        end
        
        ::finishedTransfer::
        
        robot.select(origSlot)
        
        local itemsTransferred = maxAmount - allowance
        if itemsTransferred == 0 then
            return {false, maxAmount}
        elseif maxAmount == itemsTransferred then
            return {true, 0}
        else 
            return {true, maxAmount - itemsTransferred}
        end
    else
        return nil
    end    
end

local function dumpInvToSelf (itemBlackList, face)
	
end

-- Startup procedure
while true do
	local axes = 0 

	local selfToolTable = chkSelfForItem("minecraft:stone_axe")
	if selfToolTable ~= nil and selfToolTable ~= 0 then
		axes = #selfToolTable
	end
	
	 
	for r = 1, 3 do
		if axes >= 3 then
			break
		end	
			
		turnLeff()
		local peeksy = scavengeInvForItem("minecraft:stone_axe", side.front, 3 - axes, 1)

		if peeksy ~= nil and peeksy ~= 0 then
			if peeksy[1] == true then
			   axes = 3
			else 
				axes = axes + (3 - peeksy[2])  
		end
	end
	
	if axes > 0 then
		break
	end	
	os.sleep(10)
end

::begin::
