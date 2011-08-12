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
uniform sampler2D alternateColorMap;
uniform int useAlternateTexture;
uniform vec4 tint;
uniform int materialCount;

void main(void) {
  gl_FragColor = vec4(fragAmbientColor, fragAlpha)*0.8;
  gl_FragColor += vec4(fragDiffuseColor, fragAlpha);
    
  vec4 tempColor = mix(texture2D(colorMap, vec2(((1.0/float(materialCount))*fragTexCoords.s)+((1.0/float(materialCount))*fragMtlNum), fragTexCoords.t)), texture2D(alternateColorMap, vec2(((1.0/float(materialCount))*fragTexCoords.s)+((1.0/float(materialCount))*fragMtlNum), fragTexCoords.t)), float(useAlternateTexture));
  gl_FragColor *= mix(vec4(1.0), tempColor, fragHasTexture);
  gl_FragColor *= tint;
  
  /*if (bool(int(fragHasTexture))) {
    vec4 colorToUse;
    if (bool(useAlternateTexture)) colorToUse = texture2D(alternateColorMap, vec2(((1.0/float(materialCount))*fragTexCoords.s)+((1.0/float(materialCount))*fragMtlNum), fragTexCoords.t)); else colorToUse = texture2D(colorMap, vec2(((1.0/float(materialCount))*fragTexCoords.s)+((1.0/float(materialCount))*fragMtlNum), fragTexCoords.t));
    gl_FragColor *= colorToUse;   
    gl_FragColor *= tint; 
  }*/
}
