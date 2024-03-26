/**
 * Runs a function a given number of times.
 * 
 * @param {number} times The number of times to run the function.
 * @param {function} funct The function to run.
 */
function repeat(times, funct) {
    for (; times > 0; times--)
        funct()
}



let flying = false

/**
 * Disables flight if there is ground under the vehicle.
 */
function disableFlightIfSafe() {
    let hardnessBelow = vehicle.sensors.readSeismicDet().down[0]
    
    if (hardnessBelow !== 0) {
        vehicle.flight.off()
        flying = false
    }
}

/**
 * Turns on flight.
 */
function enableFlight() {
    vehicle.flight.on()
    flying = true
}



let returnSteps = []

/**
 * Attempts to return the vehicle to the garage.
 */
function returnToGarage() {
    vehicle.turn()
    
    
    returnSteps.reverse()
    
    for (let i = 0; i < returnSteps.length; i++) {
        if (!flying && returnSteps[i] === vehicle.up)
            enableFlight()
        
        // красиво.
        returnSteps[i]()
        
        if (flying)
            disableFlightIfSafe()
    }
    
    returnSteps = []
}

/**
 * Moves the vehicle forward.
 * Records moving forward for returnToGarage.
 */
function moveForward() {
    vehicle.forward()
    returnSteps.push(vehicle.forward)
}

/**
 * Turns the vehicle.
 * Records turning for returnToGarage.
 */
function turnVehicle() {
    vehicle.turn()
    returnSteps.push(vehicle.turn)
}

/**
 * Digs down and moves the vehicle safely down.
 * If there is an empty space below the block being mined the vehicle will preemptively enter flight mode.
 * The vehicle will stop flying once it reaches land.
 * 
 * Records moving downward for returnToGarage.
 */
function digDown() {
    let seismicResults = vehicle.sensors.readSeismicDet()
    
    if (seismicResults.down[1] === 0) 
        enableFlight()
        
    if (seismicResults.down[0] !== 0)
        vehicle.dig.down()
    
    if (flying) {
        vehicle.down()
        disableFlightIfSafe()
    }
    
    
    returnSteps.push(vehicle.up)
}



function main() {
    vehicle.engine.on()
    
    
    repeat(30, digDown)
    repeat(2, function() {
        vehicle.dig.forward()
        moveForward()
    })
    repeat(4, digDown)
    turnVehicle()
    repeat(2, function() {
        digDown()
        vehicle.dig.forward()
    })

    returnToGarage()
    
    vehicle.cargo.unload()
    vehicle.forward()
    vehicle.engine.off()
}

main()