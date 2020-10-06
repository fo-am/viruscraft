fixed_dowel_rad=3;
loose_dowel_rad=fixed_dowel_rad+0.2;

module side_support_2d() {
       difference() {
        union() {
            square([11,22]);
            translate([-16,-10])
            square([16,32]);
        }
        translate([-4,4.5])
        circle(fixed_dowel_rad);
        translate([-9,16])
        circle(fixed_dowel_rad);
    }
}

module back_support_2d() {
        difference() {
        translate([0,5])
        square([105,30],true);

        // holes for rods
        translate([5,0]) circle(3.2);
        translate([-5,0]) circle(3.2);
        translate([-15,0]) circle(3.2);
        translate([15,0]) circle(3.2);

        // left servo supports
        translate([-36,-4.5]) {
            translate([-4,4.5])
            circle(fixed_dowel_rad);
            translate([-9,16])
            circle(fixed_dowel_rad);
        }

        // right servo supports
        scale([-1,1,1])
        translate([-36,-4.5]) {
            translate([-4,4.5])
            circle(fixed_dowel_rad);
            translate([-9,16])
            circle(fixed_dowel_rad);
        }

    }
}

module front_support_2d() {
    difference() {
        square([105,45],true);

        // holes for rods
        translate([5,0]) circle(3.2);
        translate([-5,0]) circle(3.2);
        translate([-15,0]) circle(3.2);
        translate([15,0]) circle(3.2);

        // bottom servo supports
        translate([-36,-4.5]) {
            translate([-4,4.5])
            circle(fixed_dowel_rad);
            translate([-9,16])
            circle(fixed_dowel_rad);
            translate([11,8.5]) {
                square([3.2,12]);
                translate([0,3])
                square([50.8,10]);
                translate([47.5,0])
                square([3.2,12]);
            }
        }

        // top servo supports
        scale([-1,1,1])
        translate([-36,-4.5]) {
            translate([-4,4.5])
            circle(fixed_dowel_rad);
            translate([-9,16])
            circle(fixed_dowel_rad);
            translate([1,-13]) {
                square([3.2,13.5]);
                translate([0,5])
                square([70.2,5]);
                translate([67,0])
                square([3.2,13.5]);
                
            }
        }
    }
}

module servo_arm_2d() {
    difference() {
        union() {
            square([50,10],false);
            translate([3,5])
            circle(6.5);
            translate([50,5])
            circle(5);
        }
        translate([3,5])
        circle(7/2);
    }
}

module servo_arm_cutaway_2d() {
    translate([3,5])  {       
        translate([7,0]) square([20-7,7],true);
        translate([0,0]) circle(3.5);
        translate([20-7,0]) circle(3.5);
    }
}


module joint_2d() {
    difference() {
        circle(9);
        translate([0,6])
        square(fixed_dowel_rad*2,true);             
        translate([0,-6])
        square(fixed_dowel_rad*2,true);             
    }
}
