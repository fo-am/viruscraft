

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
            cube([80,80,15],true);
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


