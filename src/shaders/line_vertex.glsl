#version 130

in vec2 pos;
in vec4 color;
out vec4 v_color;

uniform mat4 transform;

void main() {
    gl_Position = transform * vec4(pos.xy, 0.0, 1.0);
    v_color = color;
}
