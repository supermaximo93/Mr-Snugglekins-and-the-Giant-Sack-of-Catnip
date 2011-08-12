#version 150

flat in vec3 fragAmbientColor;
smooth in vec3 fragDiffuseColor;
flat in vec3 fragSpecularColor;
smooth in vec2 fragTexCoords;
flat in float fragMtlNum;
flat in int fragHasTexture;
flat in float fragShininess;
flat in float fragAlpha;

out vec4 fragColor;

uniform sampler2DArray colorMap;
uniform sampler2D dissolveMap;
uniform float percentage;

void main(void) {
  fragColor = vec4(fragAmbientColor, fragAlpha)*0.8;
  fragColor += vec4(fragDiffuseColor, fragAlpha);
  fragColor = texture2DArray(colorMap, vec3(fragTexCoords.st, fragMtlNum));
  vec4 tempFragColor = texture2D(dissolveMap, fragTexCoords.st);
  tempFragColor.r += percentage;
  if (tempFragColor.r > 0.9) discard; else if (tempFragColor.r > 0.8) fragColor = vec4(tempFragColor.r-0.5, tempFragColor.r-0.5, 1.0, 1.0);
}
