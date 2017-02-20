// Copyright (C) 2007 Dave Griffiths
// Licence: GPLv2 (see COPYING)
// Fluxus Shader Library
// ---------------------
// Wood Shader
// Cheezy procedural wood grain shader

uniform vec3 LightPos;
varying vec3 N;
varying vec3 P;
varying vec3 V;
varying vec3 L;

attribute vec3 bary;
varying vec3 B;
attribute vec3 face_attr;
varying vec3 FA;

void main()
{    
    N = normalize(gl_NormalMatrix*gl_Normal);
    P = gl_Vertex.xyz;
    V = -vec3(gl_ModelViewMatrix*gl_Vertex);
    L = vec3((vec4(LightPos,1)-gl_Vertex));
    B = bary;
    FA = face_attr;
    gl_Position = ftransform();
}
