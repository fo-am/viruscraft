include <mech3b_2d.scad>
include <servo.scad>

module servo_arm(rot) {
    servo();
    translate([6,5,27.5])
    rotate(rot)
    translate([-3,-4,0]) 
    difference() {    
        linear_extrude(3) {             
            servo_arm_2d();
        }
        translate([0,0,1.5])
        linear_extrude(1.51) {             
            servo_arm_cutaway_2d();
        }       
    }
}
