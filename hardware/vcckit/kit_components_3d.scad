include <kit_components_2d.scad>

module fixed_end() {
    linear_extrude($wood_thickness) {
        fixed_end_outline();
    }
}

module loose_end() {
    linear_extrude($wood_thickness) {
        loose_end_outline();
    }
}

module dowel($length) {
    cylinder(h=$length,d=$dowel_dia);
}

module cross_connector() {
    // odd in preview??

    difference() {
        linear_extrude($wood_thickness) {
            cross_connector_outline();
        }
        translate([0,0,-0.01])
        linear_extrude(2.01) {
            cross_connector_pocket_outline(); 
        }
    }
}

module angle_connector() {
    linear_extrude($wood_thickness) {
        angle_connector_outline();
    }
}

module loose_middle_connector() {
    linear_extrude($wood_thickness) {
        loose_middle_outline();
    }
}

module circlip_3d() {
    linear_extrude(0.75) {
        circlip();
    }
}

/*
rotate([180,0,0])
translate([0,0,-6])
cross_connector();
translate([32,0,0])
angle_connector();
translate([52,0,0])
fixed_end();
translate([75,0,0])
loose_end();
translate([100,0,0])
loose_middle_connector();
translate([120,0,0])
circlip_3d();
*/
