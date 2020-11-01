#version 130

in vec2 pos;
in vec2 tex;
out vec2 v_tex_coords;

uniform mat4 transform;

void main() {
    gl_Position = transform * vec4(pos.xy, 0.0, 1.0);
    v_tex_coords = tex;
}
