#version 150

in vec4 vertex;

smooth out vec2 texCoords;

uniform mat4 modelviewMatrix;
uniform mat4 projectionMatrix;

void main(void) {
  texCoords = vertex.xy;
  gl_Position = projectionMatrix*(modelviewMatrix*vertex);
}
