precision mediump float;
uniform sampler2D texture;
uniform vec3 DiffuseColour;
varying vec3 P;
varying vec3 T;
varying vec3 N;
varying vec3 L;
varying vec3 V;
varying vec3 C;

void main() {
    vec3 l = normalize(L);
    vec3 n = normalize(N);
    vec3 v = normalize(V);
    float diffuse = clamp(dot(n,l),0.0,1.0);
    float specular = 0.0;
    if (diffuse>0.0) {
        specular = max(0.0,pow(max(0.0, dot(reflect(-l, n), v)),6.0));
    }
    float height = length(P);
    vec3 col = vec3(DiffuseColour);

    gl_FragColor = vec4(texture2D(texture, vec2(T.s, T.t)).xyz *
                        (C*col*diffuse)+specular,1.0);
				
}
