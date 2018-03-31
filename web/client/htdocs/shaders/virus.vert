precision mediump float;
varying vec3 P;
varying vec3 T;
varying vec3 N;
varying vec3 L;
varying vec3 V;
varying vec3 C;
uniform mat4 ModelViewMatrix;
uniform mat4 NormalMatrix;
uniform vec3 CameraPos;
attribute vec3 p;
attribute vec3 t;
attribute vec3 n;
attribute vec3 c;

void main()
{
    P = p;
    T = t;
    C = c;
    L = vec3(6,3,12)-p;
    N = normalize(vec3(NormalMatrix*vec4(n,1.0))); 
    gl_Position = ModelViewMatrix*vec4(p,1.0);
    V = CameraPos-vec3(gl_Position); 
}
