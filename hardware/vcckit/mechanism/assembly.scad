include <parts.scad>
include <servo.scad>

translate([-6,-6.1,-28.5])
servo();

color("red") {
crank();
translate([47.8,-4,$clearance+$wood_thickness]) 
rotate([0,0,34])
connector();
translate([0,62,0]) 
connector2();
}

color("green") {
translate([0,0,($clearance+$wood_thickness)*2])
rotate([0,0,-90])
crank();
translate([0,-50,($clearance+$wood_thickness)*3])
connector();
translate([0,31,($clearance+$wood_thickness)*2]) 
connector2();
}

color("blue") {
translate([0,0,($clearance+$wood_thickness)*4])
rotate([0,0,-180])
crank();
translate([-47.8,-4,($clearance+$wood_thickness)*5]) 
rotate([0,0,-34])
connector();
translate([0,62,($clearance+$wood_thickness)*4]) 
connector2();
}

color("yellow") {
translate([0,0,($clearance+$wood_thickness)*6])
rotate([0,0,-270])
crank();
translate([0,40,($clearance+$wood_thickness)*7])
connector();
translate([0,121,($clearance+$wood_thickness)*6]) 
connector2();
}

translate([0,142,0])
cube([100,5,150],true);
translate([0,170,0])
cube([100,5,150],true);
