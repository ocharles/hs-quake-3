#version 330

in vec3 a_position;
in vec3 a_normal;
in vec2 a_uv_0;
in vec2 a_uv_1;

out vec3 v_normal;
out vec3 v_position;
out vec2 v_uv_0;
out vec2 v_uv_1;

uniform mat4 u_projViewModel;

vec3 swizzle(vec3 a) {
  return vec3(a.x, a.z, -a.y);
}

void main() {
    vec3 position = swizzle(a_position);
    gl_Position = u_projViewModel * vec4(position, 1);
    v_normal = swizzle(a_normal);
    v_position = a_position;
    v_uv_0 = a_uv_0;
    v_uv_1 = a_uv_1;
}
