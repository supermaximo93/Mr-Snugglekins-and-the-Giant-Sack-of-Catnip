#version 150

in vec4 vertex;

uniform mat4 modelviewMatrix;
uniform mat4 projectionMatrix;

void main(void) {
  gl_Position = projectionMatrix*(modelviewMatrix*vertex);
}
