include <mech3b.scad>

$fn = 10;

rotate([0,0,18])
scale([22,22,22])
translate([-100,-100,0])
#import("Icosahedron.stl");

module pair() {
translate([-170,0,260])
rotate([180,12,0])
mech3();

translate([0,0,260])
rotate([0,-20,37])
translate([-190,0,0])
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
//pair();

translate([-160,0,320])
color([1,0,0])
cube([10,230,10],true);