#version 110

attribute vec4 vertex;

uniform mat4 modelviewMatrix;
uniform mat4 projectionMatrix;
uniform mat4 jointModelviewMatrix;

void main(void) {
  vec4 vertexToUse = vec4(vertex.xyz, 1.0);
  mat4 matrixToUse = (modelviewMatrix*(1.0-vertex.w))+(jointModelviewMatrix*vertex.w);
  vec4 vertexPos = matrixToUse*vertexToUse;
  gl_Position = projectionMatrix*vertexPos;
}
