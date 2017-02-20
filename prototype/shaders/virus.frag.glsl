// Copyright (C) 2007 Dave Griffiths
// Licence: GPLv2 (see COPYING)
// Fluxus Shader Library
// ---------------------
// Wood Shader
// Cheezy procedural wood grain shader

#extension GL_OES_standard_derivatives : enable

uniform vec3 AmbientColour;
uniform vec3 DiffuseColour1;
uniform vec3 DiffuseColour2;
uniform vec3 SpecularColour;
uniform float AmbientIntensity;
uniform float DiffuseIntensity;
uniform float SpecularIntensity1;
uniform float SpecularIntensity2;
uniform float Roughness;
uniform vec3 GrainCentre;
uniform float GrainMult;

varying vec3 N;
varying vec3 P;
varying vec3 V;
varying vec3 L;
varying vec3 B;
varying vec3 FA;

float edge() {
  vec3 d = fwidth(B);
  vec3 a3 = smoothstep(vec3(0.0), d*2.0, B);
  return min(min(a3.x, a3.y), a3.z);
}

float pattern_one() {
  return smoothstep(0.0, -fwidth(B.x), sin(100.0*FA.y*B.x));
}

float pattern_two() {
  return smoothstep(0.0, -fwidth(B.x)*20.0, sin(100.0*FA.y*min(min(B.x, B.y), B.z)));
}

void main() { 	    
    vec3 l = normalize(L);
    vec3 n = normalize(N);
    vec3 v = normalize(V);
    vec3 h = normalize(l+v);
    
    float e=edge();
    float d=pattern_one();
    if (FA.x>0.5) {
      d=pattern_two();
    }
    float r = dot(v,n);
    //clamp(r,0,1.0);
    float f = e*d*r;
    gl_FragColor = vec4(vec3(f),1.0);
}
