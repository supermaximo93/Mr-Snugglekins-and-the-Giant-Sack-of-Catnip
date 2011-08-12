#version 110

varying vec3 fragAmbientColor;
varying vec3 fragDiffuseColor;
varying vec3 fragSpecularColor;
varying vec2 fragTexCoords;
varying float fragMtlNum;
varying float fragHasTexture;
varying float fragShininess;
varying float fragAlpha;

uniform sampler2D colorMap;
uniform vec4 tint;
uniform sampler2D wearMap;
uniform int severeWear;
uniform int materialCount;

void main(void) {
  gl_FragColor = vec4(fragAmbientColor, fragAlpha)*0.8;
  gl_FragColor += vec4(fragDiffuseColor, fragAlpha);
  gl_FragColor *= texture2D(colorMap, vec2(((1.0/float(materialCount))*fragTexCoords.s)+((1.0/float(materialCount))*fragMtlNum), fragTexCoords.t));
  gl_FragColor *= tint;
  gl_FragColor *= texture2D(wearMap, fragTexCoords.st);
  if (texture2D(wearMap, fragTexCoords.st).r < 0.5*float(severeWear)) discard;
}
