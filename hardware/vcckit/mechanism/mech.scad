include <servo.scad>
translate([-6,-6.1,-28.5])
servo();

$fn = 100;
$dowel_dia = 5;
$wood_thickness = 6;
$clearance = 1;
$fixed_hole_dia = $dowel_dia-0.5;
$loose_hole_dia = $dowel_dia+1.0;


// stock
// 5mm dowel

module crank() {
    difference() {        
        intersection() {
            cylinder(h=$wood_thickness,d=100);            
            translate([30,-30,-2])            
            cube([80,180,15],true);
        }
        translate([0,0,-0.2]) 
        union () {
            cylinder(h=$wood_thickness,d=4);            
            translate([45,0,0])
            cylinder(h=$wood_thickness,d=$fixed_hole_dia);
            translate([0,-45,0])
            cylinder(h=$wood_thickness,d=$fixed_hole_dia);
            translate([0,45,0])
            cylinder(h=$wood_thickness,d=$fixed_hole_dia);
        }
    }
}

module connector() { 
    translate([-5,0,0])
    difference() {
        union() {
            translate([-5,5,0])
            cube([20,82,$wood_thickness],false);
            translate([5,5,0])
            cylinder(h=$wood_thickness,d=20);
            translate([5,85,0])
            cylinder(h=$wood_thickness,d=20);
        }
        union() {
            translate([5,5,-0.1]) {
                cylinder(h=10,d=$loose_hole_dia);
                translate([0,80,0])
                cylinder(h=10,d=$fixed_hole_dia);
            }
        }
    }
}

module connector2() { 
    translate([-5,0,0])
    difference() {
        union() {
            translate([-5,5,0])
            cube([20,150,$wood_thickness],false);
            translate([5,5,0])
            cylinder(h=$wood_thickness,d=20);
            translate([5,155,0])
            cylinder(h=$wood_thickness,d=20);
        }
        union() {
            translate([5,5,-0.1]) {
                cylinder(h=10,d=$loose_hole_dia);
                translate([0,150,0])
                cylinder(h=10,d=$fixed_hole_dia);
            }
        }
    }
}


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
