include <kit_components_3d.scad>

module loose_dowel_loose(length) {
    loose_end();
    translate([0,5.5,$wood_thickness/2])
    rotate([-90,0,0])
    dowel(length-11);
    translate([0,length,0])
    rotate([0,0,180])
    loose_end();
}

module fixed_dowel_fixed(length) {
    fixed_end();
    translate([0,5.5,$wood_thickness/2])
    rotate([-90,0,0])
    dowel(length-11);
    translate([0,length,0])
    rotate([0,0,180])
    fixed_end();
}

module fixed_dowel_loose(length) {
    fixed_end();
    translate([0,5.5,$wood_thickness/2])
    rotate([-90,0,0])
    dowel(length-11);
    translate([0,length,0])
    rotate([0,0,180])
    loose_end();
}

module crank_arm(length) {
    translate([0,0,8]) {
        dowel(length-24);
        translate([$wood_thickness/2,0,length-12])
        rotate([0,-90,0])
        rotate([0,0,45])
        angle_connector();
        translate([0,0,length-12])
        rotate([90+45,0,0])
        translate([0,0,12])
        dowel(sqrt(length*length*2)-24-6);
    }
}

module crank(length) {    
    cross_connector();
    translate([0,0,$wood_thickness/2]) {
        rotate([0,-90,-45])
        crank_arm(length);
        rotate([0,-90,45])
        crank_arm(length);
        rotate([0,-90,90+45])
        crank_arm(length);
        rotate([0,-90,180+45])
        crank_arm(length);
    }
}

