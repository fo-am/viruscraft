include <mech3.scad>

$fn = 50;

rotate([0,0,18])
scale([27,27,27])
translate([-100,-100,0])
#import("Icosahedron.stl");

module pair() {
translate([-207,0,310])
rotate([0,12,0])
mech3();

translate([0,0,310])
rotate([0,-20,37])
translate([-220,0,0])
rotate([0,12,0])
mech3();
}

module quad() {
pair();
rotate([0,0,72])
pair();
}

module ring() {
quad();
rotate([0,0,72*2])
quad();
rotate([0,0,-72])
pair();
}

ring();

translate([-200,0,390])
color([1,0,0])
cube([10,280,10],true);