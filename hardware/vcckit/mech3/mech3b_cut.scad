include <mech3b_2d.scad>
$fn = 100;

rotate(90)
translate([10,-160]) {

    translate([170,20]) {
    servo_arm_2d();
    translate([-30,0]) servo_arm_cutaway_2d();  
    translate([0,15]) servo_arm_2d(); 
    translate([-30,15]) servo_arm_cutaway_2d();  
    translate([0,30]) servo_arm_2d();
    translate([-30,30]) servo_arm_cutaway_2d();  
    translate([0,45]) servo_arm_2d();
    translate([-30,45]) servo_arm_cutaway_2d();  
    }
    
    translate([10,100]) side_support_2d();
    translate([120,40]) side_support_2d();

    translate([30,53]) rotate(180) back_support_2d();
    translate([80,110]) front_support_2d();
}