#version 330

in vec3 v_normal;
in vec3 v_position;
in vec2 v_uv_0;
in vec2 v_uv_1;

layout (binding = 0) uniform sampler2D diffuseMap;

uniform bool u_lightMap;
uniform mat3 u_texMod;

void main()
{
  float ndotl = dot(v_normal, normalize(vec3(0, 5, 0) - v_position));
  gl_FragColor = texture(diffuseMap, u_lightMap ? v_uv_1 : (u_texMod * vec3(v_uv_0.xy, 1)).xy).rgba;
  // gl_FragColor = texture(diffuseMap, u_lightMap ? v_uv_1 : v_uv_0.xy).rgba;
}
