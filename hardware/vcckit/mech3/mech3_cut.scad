include <mech3_2d.scad>

rotate(90)
translate([10,-160]) {
    servo_arm_2d();
    translate([0,15]) servo_arm_2d();
    translate([0,30]) servo_arm_2d();
    translate([0,45]) servo_arm_2d();

    translate([70,0]) joint_2d();
    translate([70,20]) joint_2d();
    translate([70,40]) joint_2d();
    translate([70,60]) joint_2d();

    translate([100,3]) side_support_2d();
    translate([100,40]) side_support_2d();

    translate([50,83]) back_support_2d();
    translate([50,133]) front_support_2d();
}