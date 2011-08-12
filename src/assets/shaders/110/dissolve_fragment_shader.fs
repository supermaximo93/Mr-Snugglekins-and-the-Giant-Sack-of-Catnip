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
uniform sampler2D dissolveMap;
uniform float percentage;
uniform int materialCount;

void main(void) {
  gl_FragColor = vec4(fragAmbientColor, fragAlpha)*0.8;
  gl_FragColor += vec4(fragDiffuseColor, fragAlpha);
  gl_FragColor = texture2D(colorMap, vec2(((1.0/float(materialCount))*fragTexCoords.s)+((1.0/float(materialCount))*fragMtlNum), fragTexCoords.t));
  vec4 tempFragColor = texture2D(dissolveMap, fragTexCoords.st);
  tempFragColor.r += percentage;
  if (tempFragColor.r > 0.9) discard; else if (tempFragColor.r > 0.8) gl_FragColor = vec4(tempFragColor.r-0.5, tempFragColor.r-0.5, 1.0, 1.0);
}
