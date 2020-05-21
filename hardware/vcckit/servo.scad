module servo() {
    union() {
        color("blue")
        difference() {
            union() {
                cube([22.6,12.2,22.65],false);
                translate([-4.8,0,16])
                cube([32.2,12.2,2.5],false);
            }
            translate([-2.4,6.1,15]) {
                cylinder(h=6,d=2);
                translate([32.2-4.8,0,0])
                cylinder(h=6,d=2);
            }
        }
        
        translate([6,6.1,22.65]) {    
            color("blue")
            union() {
            cylinder(h=4.15,d=12);
            translate([6,0,0])
            cylinder(h=4.15,d=5);
            }
            translate([0,0,4.15])
            color("grey")
            difference() {
                cylinder(h=3.2,d=4);
                cylinder(h=3.3,d=1);
            }
        }
    }
}
