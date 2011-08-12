#version 150

in vec4 vertex;
in vec3 normal;
in vec3 ambientColor;
in vec3 diffuseColor;
in vec3 specularColor;
in vec3 texCoords;
in float mtlNum;
in float hasTexture;
in float shininess;
in float alpha;

flat out vec3 fragAmbientColor;
flat out vec3 fragDiffuseColor;
flat out vec3 fragSpecularColor;
smooth out vec2 fragTexCoords;
flat out int fragMtlNum;
flat out int fragHasTexture;
flat out float fragShininess;
flat out float fragAlpha;

uniform mat4 modelviewMatrix;
uniform mat4 projectionMatrix;
uniform mat4 jointModelviewMatrix;

void main(void) {
  fragAmbientColor = ambientColor;
  fragDiffuseColor = diffuseColor;
  fragSpecularColor = specularColor;
  fragTexCoords = texCoords.st;
  fragMtlNum = int(mtlNum);
  fragHasTexture = int(hasTexture);
  fragShininess = shininess;
  fragAlpha = alpha;

  gl_Position = projectionMatrix*(modelviewMatrix*vec4(vertex.xyz, 1.0));
}
