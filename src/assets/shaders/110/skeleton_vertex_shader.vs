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
  vec4 vertexToUse = vec4(vertex.xyz, 1.0);
  mat4 matrixToUse = (modelviewMatrix*(1.0-vertex.w))+(jointModelviewMatrix*vertex.w);
  vec4 vertexPos = matrixToUse*vertexToUse;
  vec3 surfaceNormal = vec3(matrixToUse*vec4(normal, 0.0));
  float diff = max(0.0, dot(normalize(surfaceNormal), normalize(vec3(0.0, 100.0, 0.0))));

  fragAmbientColor = ambientColor;
  fragDiffuseColor = diffuseColor*diff;
  fragSpecularColor = specularColor;
  fragTexCoords = texCoords.st;
  fragMtlNum = mtlNum;
  fragHasTexture = hasTexture;
  fragShininess = shininess;
  fragAlpha = alpha;
   
  gl_Position = projectionMatrix*vertexPos;
}
