/**
 * A program I wrote to make a new stand for a monitor that I did not have one for.
 * The parameters for everything are right at the start, you need just tune them to your specific monitor and how you'd like it to be oriented.
 *
 * These specific numbers are for a Proscan 19" PLED1960A oriented vertically, using the screws that I happend to have at the time.
 *
 * The units being used depends upon your printer; mine uses millimeters. All calculations will work fine with other units.
 *
 * Author: ona li toki e jan Epiphany tawa mi.
 */

/**
 * Configuration parameters.
 * Set them based on the desired size and the screws you are using.
 */
// Distance from the base of the monitor to the surface the stand is to sit on.
monitorToFloor = 40;
legWidth = 60;
// The leg and plate thickness together should be a little larger then the length of your screws so that they get a good hold but you don't accidentally drill into your monitor ;).
legThickness = 18;
attachmentPlateThickness = 12;
// Diameter for the holes that are used to screw together the stand itself. Make sure that this is the diameter of the shaft, not the threads. Holes will be scaled by 1.05x to prevent splitting the plastic.
assemblyHoleDiameter = 3;
// Length of screws used to join the base and leg.
baseLegHoleDepth = 30;
// The width and length of the base.
baseDimensions = [200, 150];

/**
 * Static parameters.
 * Set them based on the measurements of the monitor. 
 */
// The diameter of the fasteners that are used to attach to the monitor. Holes will be scaled by 1.05x to prevent splitting the plastic.
attachmentHoleDiameter = 4;
// How far apart those fasteners need to be spaced from the center of the plate.
attachmentHoleSpacing = 75 / 2;
// Height (and width) of the attachment plate. Needs to be larger than the spacing between the fasteners. 
attachmentPlateHeight = 90;
// The length from the center of the attachment plate to the bottom of the monitor (depends on orientation.
plateToMonitorBase = 230;

// Comment out or disable all but one of these at a time and export them individually.
attachmentPlate();
leg();
base();



legHeight = plateToMonitorBase + (monitorToFloor - attachmentPlateThickness);
legHoleSpacing = legWidth / 4;

holeScalar = 1.05;

$fn = 200;

module attachmentHole(xOffset, zOffset) {
    // We make the holes using a super long cylinder to make sure the hole is complete, without a phantom edge. 
    bigNumber = 1000;
    
    translate([xOffset, -bigNumber / 2, zOffset]) rotate([-90, 0, 0]) cylinder(bigNumber, d=attachmentHoleDiameter*holeScalar, true);
}

module legAttachentHole(xOffset) {
    // We make the holes using a super long cylinder to make sure the hole is complete, without a phantom edge. 
    bigNumber = 1000;
    
    translate([xOffset, -bigNumber / 2, -attachmentPlateHeight / 4]) rotate([-90, 0, 0]) cylinder(bigNumber, d=assemblyHoleDiameter*holeScalar, true);
}

module attachmentPlate() {
    difference() {
        cube([attachmentPlateHeight, attachmentPlateThickness, attachmentPlateHeight], true);
    
        attachmentHole( attachmentHoleSpacing,  attachmentHoleSpacing);
        attachmentHole(-attachmentHoleSpacing,  attachmentHoleSpacing);
        attachmentHole( attachmentHoleSpacing, -attachmentHoleSpacing);
        attachmentHole(-attachmentHoleSpacing, -attachmentHoleSpacing);
    
        legAttachentHole( legHoleSpacing);
        legAttachentHole( 0);
        legAttachentHole(-legHoleSpacing);
    }
}

module baseLegHole(xOffset) {
    epsilon = 0.002;
    
    translate([xOffset, -attachmentPlateThickness, -legHeight - attachmentPlateThickness / (2 - epsilon)]) cylinder(baseLegHoleDepth, d=assemblyHoleDiameter*holeScalar, true);
}

module leg() {
    difference() {
        translate([0, -attachmentPlateThickness, -legHeight / 2]) cube([legWidth, legThickness, legHeight], true);
    
        legAttachentHole(legHoleSpacing);
        legAttachentHole(0);
        legAttachentHole(-legHoleSpacing);
        
        baseLegHole( legHoleSpacing);
        baseLegHole( 0);
        baseLegHole(-legHoleSpacing);
    }
}


module base() {
    epsilon = 0.005;
    baseYOffset = -attachmentPlateThickness + baseDimensions[0] / 5;
    
    
    difference() {
        translate([0, -attachmentPlateThickness + baseDimensions[0] / 5, -legHeight]) cube([baseDimensions[0], baseDimensions[1], attachmentPlateThickness], true);
    
        baseLegHole( legHoleSpacing);
        baseLegHole( 0);
        baseLegHole(-legHoleSpacing);
        
        translate([0, baseYOffset + baseDimensions[1] * 3 / 20, -legHeight - epsilon / 2]) cube([baseDimensions[0] * 7 / 10, baseDimensions[1] / 2, attachmentPlateThickness + epsilon * 2], true);
    }
}