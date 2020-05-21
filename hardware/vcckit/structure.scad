include <servo.scad>
include <kit_components_3d.scad>
include <kit_components_parametric.scad>

translate([0,-6,0])
servo();

// servo holder
translate([-50,-10,6])
rotate([0,90,0])
dowel(300);
translate([-50,10,6])
rotate([0,90,0])
dowel(300);

translate([-2.5,0,6]) cube([5,44,20],true);
translate([25,0,6]) cube([5,44,20],true);
translate([225,0,26]) cube([5,44,60],true);
translate([190,0,26]) cube([5,44,60],true);

// mechanism
translate([6,0.1,28])
crank(50+24);
translate([143,0,20])
rotate([0,0,61])
fixed_dowel_loose(100);
translate([292,0,12])
rotate([0,0,90])
fixed_dowel_loose(150);