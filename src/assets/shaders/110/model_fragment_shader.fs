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
uniform int materialCount;

void main(void) {
  gl_FragColor = vec4(fragDiffuseColor.rgb, 1.0); 
  gl_FragColor *= mix(vec4(1.0), texture2D(colorMap, vec2(fragTexCoords.s, fragTexCoords.t)), fragHasTexture);
  //if (bool(int(fragHasTexture))) gl_FragColor *= texture2D(colorMap, vec2(fragTexCoords.s, fragTexCoords.t));
}
