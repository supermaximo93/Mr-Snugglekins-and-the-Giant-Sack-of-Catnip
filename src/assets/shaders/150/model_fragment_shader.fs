#version 150

flat in vec3 fragAmbientColor;
flat in vec3 fragDiffuseColor;
flat in vec3 fragSpecularColor;
smooth in vec2 fragTexCoords;
flat in int fragMtlNum;
flat in int fragHasTexture;
flat in float fragShininess;
flat in float fragAlpha;

out vec4 fragColor;

uniform sampler2DArray colorMap;

void main(void) {
  fragColor = vec4(fragDiffuseColor.rgb, 1.0);
  fragColor *= mix(vec4(1.0), texture2DArray(colorMap, vec3(fragTexCoords, 0.0)), fragHasTexture);
  //if (bool(fragHasTexture)) fragColor *= texture2DArray(colorMap, vec3(fragTexCoords, 0));
}
