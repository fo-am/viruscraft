include <kit_components_2d.scad>

loose_end_outline();
translate([30,0,0])
fixed_end_outline();
translate([60,0,0])
cross_connector_outline();
translate([90,0,0])
cross_connector_pocket_outline();
translate([120,0,0])
angle_connector_outline();
translate([150,0,0])
fixed_middle_outline();
translate([180,0,0])
loose_middle_outline();
