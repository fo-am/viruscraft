

module donut_jaw_2d() {
    union() {
        difference() {
            union() {
                import("extrude_svgs/jaw.dxf");
                translate([210,26,0]) 
                circle(20);
            }
            translate([210,26,0]) 
            circle(5);
        }
        //%translate([170,0,0]) square(60);
    }
}

module donut_square_2d() {
    union() {
        import("extrude_svgs/jaw.dxf");
        translate([210,26,0]) square(40,true);
    }
}

module donut_jaw() {
    linear_extrude(3) {
        donut_jaw_2d();
    }
}

module donut_square() {
    linear_extrude(3) {
        donut_square_2d();
    }
}

module cog() {
    linear_extrude(6) {
        import("extrude_svgs/cog.dxf");
    }
}

module assm(diff) {
    rotate([180,0,0])
    translate([-diff,-52,-6.5])
    donut_jaw();
    translate([diff,0,0])
    donut_square();
    translate([83,26,0])
    rotate([0,0,-5])
    cog();
}

assm(60);
translate([0,0,10])
%assm(0);
