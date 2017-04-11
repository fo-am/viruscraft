function load_obj(url) {
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.open( "GET", url, false );
    xmlHttp.overrideMimeType("script");
    xmlHttp.send( null );
    var str=xmlHttp.responseText;
    return inner_load_obj(xmlHttp.responseText);
}

function inner_load_obj(string) {
    var lines = string.split("\n");
    var positions = [];
    var normals = [];
    var vp = [];
    var vn = [];

    for ( var i = 0 ; i < lines.length ; i++ ) {
        var parts = lines[i].trimRight().split(' ');
        if ( parts.length > 0 ) {
            switch(parts[0]) {
            case 'v':  positions.push(
		vec3.create(
                    [parseFloat(parts[1]),
                     parseFloat(parts[2]),
                     parseFloat(parts[3])]
		));
		break;
            case 'vn':
		normals.push(
                    vec3.create(
			[parseFloat(parts[1]),
			 parseFloat(parts[2]),
			 parseFloat(parts[3])]));
		break;
            case 'f': {
		var f1 = parts[1].split('/');
		var f2 = parts[2].split('/');
		var f3 = parts[3].split('/');
		Array.prototype.push.apply(
                    vp, positions[parseInt(f1[0]) - 1]);
		Array.prototype.push.apply(
                    vn, normals[parseInt(f1[2]) - 1]);
		Array.prototype.push.apply(
                    vp, positions[parseInt(f2[0]) - 1]);
		Array.prototype.push.apply(
                    vn, normals[parseInt(f2[2]) - 1]);
		Array.prototype.push.apply(
                    vp, positions[parseInt(f3[0]) - 1]);
		Array.prototype.push.apply(
                    vn, normals[parseInt(f3[2]) - 1]);
		break;
            }
            }
        }
    }
    // returns raw data for building into a primitive later
    return [vp,vn];
}

