include <servo.scad>
include <kit_components_3d.scad>
include <kit_components_parametric.scad>

translate([0,-6,0])
servo();

// servo holder
translate([-150,-10,6])
rotate([0,90,0])
dowel(400);
translate([-150,10,6])
rotate([0,90,0])
dowel(400);

translate([-2.5,0,6]) cube([5,44,20],true);
translate([25,0,6]) cube([5,44,20],true);
translate([-80,0,36]) cube([5,44,100],true);
translate([220,0,36]) cube([5,44,100],true);

// mechanism
translate([6,0.1,28])
rotate([0,0,20])
half_crank(50+24);

translate([-55,30,35])
rotate([0,0,-103])
fixed_dowel_loose(120);
translate([260,0,42])
rotate([0,0,90])
fixed_middle_loose(200,290);

translate([70,-30,50])
rotate([0,0,-75])
fixed_dowel_loose(120);
translate([390,0,55])
rotate([0,0,90])
fixed_middle_loose(200,290);
