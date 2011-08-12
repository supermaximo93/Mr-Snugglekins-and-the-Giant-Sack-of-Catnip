#version 110

attribute vec4 vertex;

uniform mat4 modelviewMatrix;
uniform mat4 projectionMatrix;

void main(void) {
  gl_Position = projectionMatrix*(modelviewMatrix*vertex);
}
