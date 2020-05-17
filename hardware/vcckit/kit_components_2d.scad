$fn = 100;
$dowel_dia = 5;
$wood_thickness = 6;
$clearance = 1;
$fixed_hole_dia = $dowel_dia-0.5;
$loose_hole_dia = $dowel_dia+1.0;

module loose_end_outline() {
    difference() {
        circle(d=20);
        union() {
            circle(d=$loose_hole_dia);
            translate([0,10,0])
            square([$fixed_hole_dia,10],true);            
        }
    }
}
    
module fixed_end_outline() {
    difference() {
        circle(d=20);
        union() {
            circle(d=$fixed_hole_dia);
            translate([0,10,0])
            square([$fixed_hole_dia,10],true);            
        }
    }
}

module cross_connector_outline() {
    rotate(45)
    difference() {
        circle(d=25);
        union() {
            circle(d=$fixed_hole_dia);
            translate([0,12,0])
            square([$fixed_hole_dia,10],true);            
            translate([0,-12,0])
            square([$fixed_hole_dia,10],true);            
            translate([12,0,0])
            square([10,$fixed_hole_dia],true);            
            translate([-12,0,0])
            square([10,$fixed_hole_dia],true);            
        }
    }
}

// fits micro servo arm
module cross_connector_pocket_outline() {
    union() {
        circle(d=8);
        square([25,5],true);
    }
}

module angle_connector_outline() {
    intersection() {
        difference() {
            circle(d=32);
            union() {
                circle(d=$fixed_hole_dia);        
                translate([0,16,0])
                square([$fixed_hole_dia,10],true);            
                rotate(45)
                translate([0,16,0])
                square([$fixed_hole_dia,10],true);            
                rotate(90)
                translate([0,16,0])
                square([$fixed_hole_dia,10],true);            
            }
        }
        rotate(90)
        union() {
            circle(d=15);
            translate([-7.5,0])
            square([18,30],false);
            translate([0,-7.5])
            square([30,18],false);
        }        
    }
}

// cnc area
//translate([0,50])
//square([86,55],false);