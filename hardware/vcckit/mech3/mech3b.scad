include <mech3b_2d.scad>
include <servo_arm3b.scad>
$fn = 100;

module servos(a1,a2,a3,a4) {
    translate([0,0,-13.5]) {
    translate([0,41,-3])
    rotate([90,0,0])
    servo_arm(a1);

    translate([0,-41,9])
    rotate([-90,0,0])
    servo_arm(a2);

    translate([0,51,18])
    rotate([90,0,0])
    servo_arm(a3);

    translate([0,-51,12+18])
    rotate([-90,0,0])
    servo_arm(a4);
    }
}


module ligand() {
    rotate([0,90,0])
    translate([0,0,22])
    union() {
        
        // front rod
        translate([0,0,-100]) 
        cylinder(h=120,d=6);

        // shape
        translate([0,0,-110])
        rotate([90,0,0])
        translate([0,0,-1.5])
        cylinder(h=3,d=40);
    }
}

module linkages() {
    color([0.2,0.2,0.2])
    translate([50,9.5,-25])
    rotate([0,65,0])
    cylinder(d=2,h=55);

    // extended one
    color([0.2,0.2,0.2])
    translate([20,-9.5,-54])
    rotate([0,20,0])
    cylinder(d=2,h=55);

    color([0.2,0.2,0.2])
    translate([50,-19,25])
    rotate([0,180-65,0])
    cylinder(d=2,h=55);

    color([0.2,0.2,0.2])
    translate([50,19,25])
    rotate([0,180-65,0])
    cylinder(d=2,h=55);
}

module runner() {
    rotate([0,90,0])
    translate([0,0,22])
    color([0.5,0.5,0.5])
    union() {
        
        // front rod
        translate([0,0,-30]) 
        cylinder(h=120,d=6);
    }
}


module mech3() {
translate([0,-15,0]) {
    translate([60,0,0]) ligand();
    translate([0,0,10]) runner();
}
translate([0,-5,0]) {
    translate([0,0,0]) ligand();
    translate([0,0,-10]) runner();
}

translate([0,5,0]) {
    translate([60,0,0]) ligand();
    translate([0,0,-10]) runner();
}
translate([0,15,0]) {
    translate([60,0,0]) ligand();
    translate([0,0,10]) runner();
}

linkages();
servos(-20,70,70,-20);

//supports

// back support
translate([110,0,0])
rotate([-90,0,-90])
%linear_extrude(3){
    back_support_2d();
}

// front support
translate([-4,0,0])
rotate([-90,0,-90])
%linear_extrude(3){
    front_support_2d();
}

// side support
translate([23,36,4.5])
rotate([-90,0,-90])
linear_extrude(3){
 side_support_2d();
}
}

mech3();