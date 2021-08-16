#version 440

layout(location = 0) in vec2 position;

layout(location = 0) out vec3 v_color;

layout(std140, binding = 0) uniform buf {
    mat4 model;
    mat4 view_matrix;
} mvp_uniform;

out gl_PerVertex { vec4 gl_Position; };

void main()
{
    // These are actually mat3's, but encoded as mat4 to satisfy alignment
    // requirements.
    mat3 model_view_matrix =  mat3(mvp_uniform.view_matrix) * mat3(mvp_uniform.model);

    gl_Position = vec4((model_view_matrix * vec3(position, 1)).xy, 0, 1);
    v_color = vec3(0.95, 0.95, 0.95);
}