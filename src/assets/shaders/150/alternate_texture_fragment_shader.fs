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
uniform sampler2DArray alternateColorMap;
uniform int useAlternateTexture;
uniform vec4 tint;

void main(void) {
  fragColor = vec4(fragAmbientColor, fragAlpha)*0.8;
  fragColor += vec4(fragDiffuseColor, fragAlpha);

  vec4 tempColor = mix(texture2DArray(colorMap, vec3(fragTexCoords.st, fragMtlNum)), texture2DArray(alternateColorMap, vec3(fragTexCoords.st, fragMtlNum)), useAlternateTexture);
  fragColor *= mix(vec4(1.0), tempColor, fragHasTexture);
  fragColor *= tint;
  
  /*if (bool(fragHasTexture)) {
    vec4 colorToUse;
    if (bool(useAlternateTexture)) colorToUse = texture2DArray(alternateColorMap, vec3(fragTexCoords.st, fragMtlNum)); else colorToUse = texture2DArray(colorMap, vec3(fragTexCoords.st, fragMtlNum));
    fragColor *= colorToUse;
    fragColor *= tint;
  }*/
}
