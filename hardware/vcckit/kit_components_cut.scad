include <kit_components_2d.scad>

loose_end_outline();
translate([25,0,0])
fixed_end_outline();
translate([50,0,0])
cross_connector_outline();
translate([50,20,0])
cross_connector_pocket_outline();
translate([80,0,0])
angle_connector_outline();
translate([100,0,0])
fixed_middle_outline();
translate([125,0,0])
loose_middle_outline();
translate([145,0,0])
circlip();
