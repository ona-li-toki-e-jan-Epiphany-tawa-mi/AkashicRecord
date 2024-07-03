/**
 * Hole caps, a simple configurable plug for holes. I made theses just to plug the holes in my wall because I'd rather not make a mess with drywall at the moment.
 *
 * Set the configuration parameters to what you need and print away.
 *
 * Author: ona li toki e jan Epiphany tawa mi.
 */



/**
 * Configuration parameters. Make sure to set these to what you need.
 */
// How long to make the plug-end that goes into the hole.
plugLength = 12.7;
// How wide to make the plug-end. Make sure that this is slightly smaller than the hole so it will fit.
plugDiameter = 6.45;
// How long to make the cap that goes over the hole.
capLength = 2;
// How wide to make the cap.
capDiameter = 20;
// The diameter of the hole cap's hole. Leave negative for a solid hole cap.
innerDiameter = -1;
// If true, assumes the input units are inches and converts them to millimeters for the final model. If false, assumes the input units are millimeters.
usingImperial = false;

$fn = 200;



/**
 * Performs integer to millimeter conversion.
 */
_plugLength = usingImperial ? plugLength * 25.4 : plugLength;
_plugDiameter = usingImperial ? plugDiameter * 25.4 : plugDiameter;
_capLength = usingImperial ? capLength * 25.4 : capLength;
_capDiameter = usingImperial ? capDiameter * 25.4 : capDiameter;
_innerDiameter = usingImperial ? innerDiameter * 25.4 : innerDiameter;



/**
 * Creates the hole cap.
 */
difference() {
    union() {
        epsilon = 0.01;
        
        translate([0, 0, _capLength - epsilon]) cylinder( _plugLength, d=_plugDiameter);
        cylinder(_capLength, d=_capDiameter);
    }
    
    // Generates inner hole.
    if (_innerDiameter > 0) 
        translate([0, 0, (_plugLength + _capLength) / -2]) cylinder((_plugLength + _capLength) * 2, d=_innerDiameter);
}