#version 130

in vec2 v_tex_coords;
out vec4 color;

uniform sampler2D tex;

void main() {
    vec2 normal_colors = texture(tex, v_tex_coords).ra;
    vec2 normal_xy = (normal_colors * 2.0) - 1.0;
    vec2 normal_sq = normal_xy * normal_xy;
    float normal_z = sqrt(max(0.1, 1.0 - normal_sq.x - normal_sq.y));
    vec3 normal = normalize(vec3(normal_xy, normal_z));
    color.rgb = (normal + 1.0) * 0.5;
    color.a = 1.0;
}
