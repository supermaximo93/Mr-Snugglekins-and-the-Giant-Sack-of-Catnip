#version 150

smooth in vec2 texCoords;

out vec4 fragColor;

uniform sampler2DRect colorMap;
uniform vec4 color;
uniform float cutoff;

void main(void) {
  if (texCoords.t < cutoff) discard;
  fragColor = texture2DRect(colorMap, texCoords);
  if (fragColor == vec4(0.0, 0.0, 0.0, 0.0)) discard;
  fragColor *= color;
}
