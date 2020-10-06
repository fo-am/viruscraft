

module side_support_2d() {
       difference() {
        union() {
            square([11,22]);
            translate([-16,-10])
            square([16,32]);
        }
        translate([-4,4.5])
        circle(3);
        translate([-9,16])
        circle(3);
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
            circle(3);
            translate([-9,16])
            circle(3);
        }

        // right servo supports
        scale([-1,1,1])
        translate([-36,-4.5]) {
            translate([-4,4.5])
            circle(3);
            translate([-9,16])
            circle(3);
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

        // left servo supports
        translate([-36,-4.5]) {
            translate([-4,4.5])
            circle(3);
            translate([-9,16])
            circle(3);
            translate([11,8.5]) {
                square([3,12]);
                translate([0,3])
                square([50.5,9]);
                translate([47.5,0])
                square([3,12]);
            }
        }

        // right servo supports
        scale([-1,1,1])
        translate([-36,-4.5]) {
            translate([-4,4.5])
            circle(3);
            translate([-9,16])
            circle(3);
            translate([1,-13]) {
                square([3,12]);
                translate([0,5])
                square([68.5,9]);
                translate([67,0])
                square([3,12]);
                
            }
        }
    }
}

module servo_arm_2d() {
    difference() {
        union() {
            square([50,8],false);
            translate([3,4])
            circle(6);
            translate([50,4])
            circle(4);
        }
        translate([3,4])
        circle(4.8/2);
    }
}

module joint_2d() {
    difference() {
        circle(9);
        translate([0,6])
        square(6,true);             
        translate([0,-6])
        square(6,true);             
    }
}
