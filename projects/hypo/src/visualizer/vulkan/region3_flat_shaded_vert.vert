#version 440

layout(location = 0) in vec4 position;
layout(location = 1) in vec3 normal;

layout(location = 0) out vec4 v_color;

layout(std140, binding = 0) uniform buf {
    mat4 model;
    mat4 projection_view_matrix;
} mvp_uniform;

out gl_PerVertex { vec4 gl_Position; };

void main()
{
    const vec3 LIGHT_DIRECTION = normalize(vec3(1.2, 1.5, -0.8));
    vec3 transformed_normal =
        normalize(transpose(inverse(mat3(mvp_uniform.model))) * normal);

    float cosLightAngle = dot(transformed_normal, LIGHT_DIRECTION);
    v_color = vec4(vec3(cosLightAngle), 1);

    mat4 mvp =  mvp_uniform.projection_view_matrix * mvp_uniform.model;
    gl_Position = mvp * position;
}