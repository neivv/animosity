pub const SPRITE_VERTEX: &str = r#"
    #version 130

    in vec2 pos;
    in vec2 tex;
    out vec2 v_tex_coords;

    uniform mat4 transform;

    void main() {
        gl_Position = transform * vec4(pos.xy, 0.0, 1.0);
        v_tex_coords = tex;
    }
"#;

pub const SPRITE_FRAGMENT: &str = r#"
    #version 130

    in vec2 v_tex_coords;
    out vec4 color;

    uniform sampler2D tex;

    void main() {
        color = texture(tex, v_tex_coords);
    }
"#;

pub const LINE_VERTEX: &str = r#"
    #version 130

    in vec2 pos;
    in vec4 color;
    out vec4 v_color;

    uniform mat4 transform;

    void main() {
        gl_Position = transform * vec4(pos.xy, 0.0, 1.0);
        v_color = color;
    }
"#;

pub const LINE_FRAGMENT: &str = r#"
    #version 130

    in vec4 v_color;
    out vec4 color;

    void main() {
        color = v_color;
    }
"#;
