#version 110

attribute vec4 vertex;

varying vec2 texCoords;

uniform mat4 modelviewMatrix;
uniform mat4 projectionMatrix;

void main(void) {
  texCoords = vertex.xy;
  gl_Position = projectionMatrix*(modelviewMatrix*vertex);
}
