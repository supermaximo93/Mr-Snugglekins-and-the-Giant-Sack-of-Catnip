#version 110

attribute vec4 vertex;
attribute vec3 normal;
attribute vec3 ambientColor;
attribute vec3 diffuseColor;
attribute vec3 specularColor;
attribute vec3 texCoords;
attribute float mtlNum;
attribute float hasTexture;
attribute float shininess;
attribute float alpha;

varying vec3 fragAmbientColor;
varying vec3 fragDiffuseColor;
varying vec3 fragSpecularColor;
varying vec2 fragTexCoords;
varying float fragMtlNum;
varying float fragHasTexture;
varying float fragShininess;
varying float fragAlpha;

uniform mat4 modelviewMatrix;
uniform mat4 projectionMatrix;
uniform mat4 jointModelviewMatrix;

void main(void) {
  fragAmbientColor = ambientColor;
  fragDiffuseColor = diffuseColor;
  fragSpecularColor = specularColor;
  fragTexCoords = texCoords.st;
  fragMtlNum = mtlNum;
  fragHasTexture = hasTexture;
  fragShininess = shininess;
  fragAlpha = alpha;

  gl_Position = projectionMatrix*(modelviewMatrix*vec4(vertex.xyz, 1.0));
}
