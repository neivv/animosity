use std::fs;
use std::io::{self, Read};
use std::time::SystemTime;
use std::path::{Path, PathBuf};

use glium::backend::glutin::headless::Headless;

pub struct Shader {
    filename: &'static str,
    default: &'static str,
}

pub struct Program {
    program: glium::program::Program,
    vertex: &'static Shader,
    vertex_time: Option<SystemTime>,
    fragment: &'static Shader,
    fragment_time: Option<SystemTime>,
}

impl Shader {
    const fn new(filename: &'static str, default: &'static str) -> Shader {
        Shader {
            filename,
            default,
        }
    }
}

impl Program {
    pub fn new(facade: &Headless, vertex: &'static Shader, fragment: &'static Shader) -> Program {
        let vertex_file = load_shader_from_file(vertex);
        let fragment_file = load_shader_from_file(fragment);
        if let (Ok(mut vertex_file), Ok(mut fragment_file)) = (vertex_file, fragment_file) {
            loop {
                let result = glium::program::Program::from_source(
                    facade,
                    &vertex_file.0,
                    &fragment_file.0,
                    None,
                );
                match result {
                    Ok(program) => {
                        return Program {
                            program,
                            vertex,
                            vertex_time: Some(vertex_file.1),
                            fragment,
                            fragment_time: Some(fragment_file.1),
                        };
                    }
                    Err(e) => {
                        print_shader_err(e, vertex, fragment);
                        wait_for_changed(vertex, vertex_file.1, fragment, fragment_file.1);
                    }
                }
                loop {
                    if let Ok(o) = load_shader_from_file(vertex) {
                        vertex_file = o;
                        break;
                    }
                }
                loop {
                    if let Ok(o) = load_shader_from_file(fragment) {
                        fragment_file = o;
                        break;
                    }
                }
            }
        }
        let program = glium::program::Program::from_source(
            facade,
            vertex.default,
            fragment.default,
            None,
        ).expect("Couldn't compile program");
        Program {
            program,
            vertex,
            vertex_time: None,
            fragment,
            fragment_time: None,
        }
    }

    pub fn program(&mut self, facade: &Headless) -> &glium::program::Program {
        if let (Some(mut vertex_time), Some(mut fragment_time)) =
            (self.vertex_time, self.fragment_time)
        {
            let vertex_path = shader_path(self.vertex);
            let fragment_path = shader_path(self.fragment);
            if has_changed(&vertex_path, vertex_time) ||
                has_changed(&fragment_path, fragment_time)
            {
                loop {
                    let vertex_text;
                    let fragment_text;
                    loop {
                        if let Ok(o) = load_shader_from_file(self.vertex) {
                            vertex_text = o.0;
                            vertex_time = o.1;
                            break;
                        }
                    }
                    loop {
                        if let Ok(o) = load_shader_from_file(self.fragment) {
                            fragment_text = o.0;
                            fragment_time = o.1;
                            break;
                        }
                    }
                    let result = glium::program::Program::from_source(
                        facade,
                        &vertex_text,
                        &fragment_text,
                        None,
                    );
                    match result {
                        Ok(program) => {
                            println!(
                                "Reloaded {}/{}",
                                self.vertex.filename, self.fragment.filename,
                            );
                            self.program = program;
                            self.vertex_time = Some(vertex_time);
                            self.fragment_time = Some(fragment_time);
                            return &self.program;
                        }
                        Err(e) => {
                            print_shader_err(e, self.vertex, self.fragment);
                            wait_for_changed(
                                self.vertex,
                                vertex_time,
                                self.fragment,
                                fragment_time,
                            );
                        }
                    }
                }
            }
        }
        &self.program
    }
}

fn print_shader_err(err: glium::ProgramCreationError, vertex: &Shader, fragment: &Shader) {
    eprintln!("Compiling {}/{} failed: {}", vertex.filename, fragment.filename, err);
}

fn load_shader_from_file(shader: &Shader) -> Result<(String, SystemTime), io::Error> {
    let path = shader_path(shader);
    let mut file = fs::File::open(path)?;
    let mut buf = Vec::new();
    file.read_to_end(&mut buf)?;
    let metadata = file.metadata()?;
    let time = metadata.modified()?;
    Ok((String::from_utf8_lossy(&buf).into(), time))
}

fn wait_for_changed(
    first: &Shader,
    first_time: SystemTime,
    second: &Shader,
    second_time: SystemTime,
) {
    let first_path = shader_path(first);
    let second_path = shader_path(second);
    loop {
        if has_changed(&first_path, first_time) {
            return;
        }
        if has_changed(&second_path, second_time) {
            return;
        }
        std::thread::sleep(std::time::Duration::from_millis(500));
    }
}

fn has_changed(path: &Path, time: SystemTime) -> bool {
    fs::metadata(path)
        .and_then(|m| m.modified())
        .map(|x| x != time)
        .unwrap_or(false)
}

fn shader_path(shader: &Shader) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("src/shaders")
        .join(shader.filename)
}

macro_rules! shader {
    ($filename:expr) => {
        Shader::new($filename, include_str!(concat!("shaders/", $filename)))
    };
}

pub static SPRITE_VERTEX: Shader = shader!("sprite_vertex.glsl");
pub static SPRITE_FRAGMENT: Shader = shader!("sprite_fragment.glsl");
pub static LINE_VERTEX: Shader = shader!("line_vertex.glsl");
pub static LINE_FRAGMENT: Shader = shader!("line_fragment.glsl");
