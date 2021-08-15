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
    mat4 model_view_matrix =  mvp_uniform.view_matrix * mvp_uniform.model;
    gl_Position = model_view_matrix * vec4(position, 0, 1);
    v_color = vec3(0.95, 0.95, 0.95);
}