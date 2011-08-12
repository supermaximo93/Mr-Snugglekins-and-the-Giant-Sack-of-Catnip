#version 110
#extension GL_ARB_texture_rectangle : enable

varying vec2 texCoords;

uniform sampler2DRect colorMap;

void main(void) {
  gl_FragColor = texture2DRect(colorMap, texCoords);
  if (gl_FragColor == vec4(0, 0, 0, 0)) discard;
}
