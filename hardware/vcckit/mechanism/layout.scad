include <parts.scad>


translate([-30,0,0])
cube([10,10,10]);
connector();
translate([30,0,0])
connector2();
translate([60,50,0])
crank();