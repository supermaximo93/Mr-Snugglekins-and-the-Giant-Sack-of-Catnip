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
uniform vec4 tint;
uniform sampler2D wearMap;
uniform int severeWear;

void main(void) {
  fragColor = vec4(fragAmbientColor, fragAlpha)*0.8;
  fragColor += vec4(fragDiffuseColor, fragAlpha);
  fragColor *= texture2DArray(colorMap, vec3(fragTexCoords.st, fragMtlNum));
  fragColor *= tint;
  fragColor *= texture2D(wearMap, fragTexCoords.st);
  if (texture2D(wearMap, fragTexCoords.st).r < 0.5*severeWear) discard;
}
