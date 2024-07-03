/*
 * Copyright 2023 ona-li-toki-e-jan-Epiphany-tawa-mi
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permit * ted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS”
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

fluteDiameter_mm         = 25.7;
lipPlateLength_mm        = 25;
lipPlateThickness_mm     = 5;
embochureHoleDiameter_mm = 10.6;

$fn = 200;

difference() {
    // We shift the cylinder over based on how thick it needs to be and subtract
    // it from itself untranslated to cut out the space taken up by the flute,
    // leaving a plate that will fit right on it.
    translate([lipPlateThickness_mm, 0, 0])
        cylinder(lipPlateLength_mm, d=fluteDiameter_mm);

    translate([0, 0, -lipPlateLength_mm / 2])
        cylinder(lipPlateLength_mm * 2, d=fluteDiameter_mm);

    // And then make the embochure hole.
    translate([fluteDiameter_mm / 2 - lipPlateThickness_mm, 0, lipPlateLength_mm / 2])
        rotate([0, 90, 0])
        cylinder(lipPlateThickness_mm * 2, d=embochureHoleDiameter_mm);
}
