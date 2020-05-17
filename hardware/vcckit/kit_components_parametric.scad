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

module crank(length) {
    angle_connector();
    translate([0,12,$wood_thickness/2])
    rotate([-90,0,0])
    dowel(length-24);
    translate([0,length,0])
    rotate([0,0,90])
    angle_connector();
    translate([-12,length,$wood_thickness/2])
    rotate([0,-90,0])
    dowel(length-24);
    translate([-length,length,0])
    rotate([0,0,-180])
    angle_connector();
    translate([-length,length-12,$wood_thickness/2])
    rotate([90,0,0])
    dowel(length-24);
    translate([-length,0,0])
    rotate([0,0,-90])
    angle_connector();
    translate([-length+12,0,$wood_thickness/2])
    rotate([0,90,0])
    dowel(length-24);
    translate([-length/2,length/2,0])
    cross_connector();
    
    translate([0,0,$wood_thickness/2])
    rotate([0,-90,-45])
    translate([0,0,12])
    dowel(length-24);
    translate([0,0,$wood_thickness/2])
    rotate([0,-90,45])
    translate([0,0,12])
    dowel(length-24);
    
    $opp_len = asin(0.1)*length;
     echo(asin(0.1));

}

crank(80);