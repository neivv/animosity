#version 130

in vec2 v_tex_coords;
out vec4 color;

uniform sampler2D tex;
uniform sampler1D palette;

void main() {
    float idx = texture(tex, v_tex_coords).r;
    color.rgb = texture(palette, idx).rgb;
    color.a = 1.0;
}
