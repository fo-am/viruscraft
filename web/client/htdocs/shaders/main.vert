precision mediump float;
varying vec3 P;
varying vec3 T;
varying vec3 C;
varying vec3 N;
varying vec3 L;
uniform mat4 ModelViewMatrix;
uniform mat4 NormalMatrix;
uniform vec3 CameraPos;
attribute vec3 p;
attribute vec3 t;
attribute vec3 n;
attribute vec3 c;

void main()
{
    P = p.xyz;
    T = t;
    C = c;
    L = vec3(0,0,1); /* vec3(ModelViewMatrix*vec4((vec3(0,10,0)-p),1)); */
    N = normalize(vec3(NormalMatrix*vec4(n,1)));	
    gl_Position = ModelViewMatrix * vec4(p,1);
}
