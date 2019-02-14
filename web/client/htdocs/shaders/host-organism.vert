/* precision needed for android devices */
precision highp float;
varying vec3 P;
varying vec3 T;
varying vec3 C;
varying vec3 N;
varying vec3 L;
uniform mat4 ModelViewMatrix;
uniform mat4 NormalMatrix;
uniform vec3 CameraPos;
uniform float time;
uniform float age;
uniform float health;
uniform float speed;
uniform float movetime;

attribute vec3 p;
attribute vec3 t;
attribute vec3 n;
attribute vec3 c;
attribute vec3 p2;

const float adult_age = 20.0;
const float low_health = 10.0;
const float min_scale = 0.3;

void main()
{
  P=p;
  
  /* main animation */
  if (health>=low_health) {
    P = mix(p,p2,abs(mod(movetime*speed,2.0)-1.0));
  }
  
  /* child scale up growth */
  if (age<adult_age) P *= max(min_scale,age/adult_age);

  if (health<low_health) {
    /* stop moving and lower into the ground! */
    P.z-=((low_health-health)/low_health)*3.0;
  }

  T = t;
  C = c;
  L = vec3(0,0,1); /* vec3(ModelViewMatrix*vec4((vec3(0,10,0)-p),1)); */
  N = normalize(vec3(NormalMatrix*vec4(n,1)));	
  gl_Position = ModelViewMatrix * vec4(P,1);
}
