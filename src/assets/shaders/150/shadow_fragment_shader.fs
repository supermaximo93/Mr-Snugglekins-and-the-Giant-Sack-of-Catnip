#version 150

smooth in vec2 texCoords;

out vec4 fragColor;

uniform sampler2DRect colorMap;

void main(void) {
  fragColor = texture2DRect(colorMap, texCoords);
  if (fragColor.a >= 0.2) fragColor.a -= 0.2;
  if (fragColor == vec4(0, 0, 0, 0)) discard;
}
