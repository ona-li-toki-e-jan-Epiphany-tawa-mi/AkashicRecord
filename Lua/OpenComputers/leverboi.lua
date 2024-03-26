local component = require("component")
local rddy = component.redstone
local side = require("sides")
local robot = require("robot")

while true do
	if rddy.getInput(side.left) > 0 then
		os.sleep(5)
		
		robot.turnRight()
		robot.turnRight()
		
		robot.use()
		
		robot.turnLeft()
		robot.turnLeft()
		robot.forward()
		robot.turnRight()
		robot.forward()
		robot.turnRight()
		robot.up()
		robot.up()
		
		robot.use()
		
		robot.down()
		robot.down()
		robot.turnRight()
		robot.forward()
		robot.turnLeft()
		robot.forward()
		
		robot.use()
		
		robot.turnLeft()
		robot.turnLeft()
		
		os.sleep(2)
	end
	
	os.sleep(1)
end