use std::convert::TryFrom;
use std::rc::Rc;

use anyhow::{anyhow, Context, Error};
use cgmath::conv::array4x4;
use cgmath::{Matrix4, vec4};
use glium::backend::glutin::headless::Headless;
use glium::index::{IndexBuffer, PrimitiveType};
use glium::texture::{self, ClientFormat, Texture1d, Texture2d};
use glium::vertex::VertexBuffer;
use glium::Surface;

use crate::SpriteType;
use crate::anim::RawTexture;
use crate::gl;
use crate::shaders::{self, Program};

pub struct RenderState {
    gl: gl::Context,
    draw_params: DrawParams,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SpriteMode {
    Raw,
    Ao,
    Depth,
    Normal,
}

impl RenderState {
    pub fn new(width: u32, height: u32) -> RenderState {
        let mut gl = gl::Context::new(width, height);
        let vertices = gl.set_vertices(&[
            gl::Vertex { pos: [-1.0, 1.0], tex: [0.0, 1.0] },
            gl::Vertex { pos: [1.0, 1.0], tex: [1.0, 1.0] },
            gl::Vertex { pos: [-1.0, -1.0], tex: [0.0, 0.0] },
            gl::Vertex { pos: [1.0, -1.0], tex: [1.0, 0.0] },
        ]);
        let indices = glium::index::IndexBuffer::new(
            gl.facade(),
            PrimitiveType::TrianglesList,
            &[0, 1, 2, 1, 3, 2],
        ).expect("Unable to create index buffer");
        let program = sprite_render_program(&mut gl);
        let ao_program = Program::new(
            gl.facade(),
            &shaders::SPRITE_VERTEX,
            &shaders::AO_FRAGMENT,
        );
        let depth_program = Program::new(
            gl.facade(),
            &shaders::SPRITE_VERTEX,
            &shaders::DEPTH_FRAGMENT,
        );
        let normal_program = Program::new(
            gl.facade(),
            &shaders::SPRITE_VERTEX,
            &shaders::NORMAL_FRAGMENT,
        );
        let paletted_program = Program::new(
            gl.facade(),
            &shaders::PALETTED_VERTEX,
            &shaders::PALETTED_FRAGMENT,
        );
        let lines = DrawLines::new(&mut gl);
        RenderState {
            gl,
            draw_params: DrawParams {
                vertices,
                indices,
                program,
                ao_program,
                depth_program,
                normal_program,
                paletted_program,
                cached_textures: Vec::new(),
                cached_palette: None,
                lines,
            },
        }
    }

    pub fn resize_buf(&mut self, width: u32, height: u32) {
        self.gl.resize_buf(width, height);
    }

    pub fn clear_cache_all(&mut self) {
        self.draw_params.cached_textures.clear();
        self.draw_params.lines.texture_lines.0.clear();
        self.draw_params.cached_palette = None;
    }

    pub fn clear_cached(&mut self, tex_id: TextureId) {
        self.draw_params.cached_textures.retain(|x| x.1 != tex_id);
        self.draw_params.lines.texture_lines.0.retain(|x| x.0 != tex_id);
        self.draw_params.cached_palette = None;
    }

    pub fn framebuf_bytes(&self) -> (Vec<u8>, u32, u32) {
        self.gl.framebuf_bytes()
    }

    pub fn clear_framebuf(&mut self) {
        let (mut buf, _facade) = self.gl.framebuf();
        buf.clear_color(0.0, 0.0, 0.0, 1.0);
    }

    pub fn render_sprite(&mut self, texture: &Texture2d, mode: SpriteMode) -> Result<(), Error> {
        let glium_params = glium::draw_parameters::DrawParameters {
            blend: glium::Blend::alpha_blending(),
            ..Default::default()
        };
        let sampler = glium::uniforms::Sampler::new(texture)
            .magnify_filter(glium::uniforms::MagnifySamplerFilter::Nearest)
            .minify_filter(glium::uniforms::MinifySamplerFilter::Linear);

        let (mut buf, facade) = self.gl.framebuf();
        let (buf_width, buf_height) = self.gl.buf_dimensions();
        // scale to view, scale + transform view to
        let tex_width = texture.width() as f32;
        let tex_height = texture.height() as f32;
        let mut render_width = tex_width.min(buf_width as f32);
        let mut render_height = tex_height.min(buf_height as f32);
        // Keep aspect ratio
        if render_width / tex_width < render_height / tex_height {
            render_height = (render_width / tex_width) * tex_height;
        } else {
            render_width = (render_height / tex_height) * tex_width;
        }
        let tex_to_window = self.to_window_matrix(render_width, render_height);
        let uniforms = uniform! {
            transform: array4x4(tex_to_window),
            tex: sampler,
        };
        let program = match mode {
            SpriteMode::Raw => self.draw_params.program.program(facade),
            SpriteMode::Ao => self.draw_params.ao_program.program(facade),
            SpriteMode::Depth => self.draw_params.depth_program.program(facade),
            SpriteMode::Normal => self.draw_params.normal_program.program(facade),
        };
        buf.draw(
            &self.draw_params.vertices,
            &self.draw_params.indices,
            program,
            &uniforms,
            &glium_params,
        )?;
        Ok(())
    }

    pub fn render_paletted(
        &mut self,
        texture: &Texture2d,
        palette: &Texture1d,
    ) -> Result<(), Error> {
        let glium_params = glium::draw_parameters::DrawParameters {
            blend: glium::Blend::alpha_blending(),
            ..Default::default()
        };
        let sampler = glium::uniforms::Sampler::new(texture)
            .magnify_filter(glium::uniforms::MagnifySamplerFilter::Nearest)
            .minify_filter(glium::uniforms::MinifySamplerFilter::Linear);
        let palette_sampler = glium::uniforms::Sampler::new(palette)
            .magnify_filter(glium::uniforms::MagnifySamplerFilter::Nearest)
            .minify_filter(glium::uniforms::MinifySamplerFilter::Nearest);

        let (mut buf, facade) = self.gl.framebuf();
        let (buf_width, buf_height) = self.gl.buf_dimensions();
        // scale to view, scale + transform view to
        let tex_width = texture.width() as f32;
        let tex_height = texture.height() as f32;
        let mut render_width = tex_width.min(buf_width as f32);
        let mut render_height = tex_height.min(buf_height as f32);
        // Keep aspect ratio
        if render_width / tex_width < render_height / tex_height {
            render_height = (render_width / tex_width) * tex_height;
        } else {
            render_width = (render_height / tex_height) * tex_width;
        }
        let tex_to_window = self.to_window_matrix(render_width, render_height);
        let uniforms = uniform! {
            transform: array4x4(tex_to_window),
            tex: sampler,
            palette: palette_sampler,
        };
        buf.draw(
            &self.draw_params.vertices,
            &self.draw_params.indices,
            self.draw_params.paletted_program.program(facade),
            &uniforms,
            &glium_params,
        )?;
        Ok(())
    }

    fn to_window_matrix(&self, width: f32, height: f32) -> Matrix4<f32> {
        // (render_width / buf_width) * (buf_width / buf_stride)
        let (buf_width, buf_height) = self.gl.buf_dimensions();
        let buf_stride = self.gl.stride();
        let scale_x = width / buf_stride as f32;
        let scale_y = height / buf_height as f32;
        let shift_x = -1.0 + buf_width as f32 / buf_stride as f32;
        let shift_y = 0.0;
        Matrix4::from_cols(
            vec4(scale_x,   0.0,        0.0,    0.0),
            vec4(0.0,       scale_y,    0.0,    0.0),
            vec4(0.0,       0.0,        1.0,    0.0),
            vec4(shift_x,   shift_y,    0.0,    1.0),
        )
    }

    pub fn render_lines<F: FnOnce() -> Vec<(Rect, Color, u8)>>(
        &mut self,
        tex_id: TextureId,
        texture: &Texture2d,
        gen_lines: F,
    ) -> Result<(), Error> {
        let glium_params = glium::draw_parameters::DrawParameters {
            blend: glium::Blend::alpha_blending(),
            ..Default::default()
        };
        let (mut buf, facade) = self.gl.framebuf();
        let (buf_width, buf_height) = self.gl.buf_dimensions();
        let lines =
            self.draw_params.lines.texture_lines.buffer_for_texture(facade, &tex_id, gen_lines);

        let buf_stride = self.gl.stride();
        let tex_width = texture.width() as f32;
        let tex_height = texture.height() as f32;
        let mut render_width = tex_width.min(buf_width as f32);
        let mut render_height = tex_height.min(buf_height as f32);
        // Keep aspect ratio
        if render_width / tex_width < render_height / tex_height {
            render_height = (render_width / tex_width) * tex_height;
        } else {
            render_width = (render_height / tex_height) * tex_width;
        }
        // (render_width / buf_width) * (buf_width / buf_stride)
        let scale_x = render_width / buf_stride as f32;
        let scale_y = render_height / buf_height as f32;
        let shift_x = -1.0 + buf_width as f32 / buf_stride as f32;
        let shift_y = 0.0;
        let tex_to_window = Matrix4::from_cols(
            vec4(scale_x,   0.0,        0.0,    0.0),
            vec4(0.0,       scale_y,    0.0,    0.0),
            vec4(0.0,       0.0,        1.0,    0.0),
            vec4(shift_x,   shift_y,    0.0,    1.0),
        );
        let pixel_to_tex = Matrix4::from_cols(
            vec4(2.0 / tex_width,   0.0,                0.0,    0.0),
            vec4(0.0,               2.0 / tex_height,   0.0,    0.0),
            vec4(0.0,               0.0,                1.0,    0.0),
            vec4(-1.0,              -1.0,               0.0,    1.0),
        );
        let uniforms = uniform! {
            //transform: array4x4(pixel_to_tex * tex_to_window),
            transform: array4x4(tex_to_window * pixel_to_tex),
        };
        buf.draw(
            &lines.vertices,
            &lines.indices,
            self.draw_params.lines.program.program(facade),
            &uniforms,
            &glium_params,
        )?;
        Ok(())
    }

    pub fn cached_texture<F>(&mut self, tex_id: TextureId, gen_image: F) ->
        Result<Rc<Texture2d>, Error>
    where F: FnOnce() -> Result<RawTexture, Error>
    {
        let cached_textures = &mut self.draw_params.cached_textures;
        let cached = cached_textures.iter().position(|x| x.1 == tex_id);
        if let Some(index) = cached {
            Ok(cached_textures[index].0.clone())
        } else {
            let facade = self.gl.facade();
            let image = gen_image()
                .context("Couldn't get image for texture")?;
            let texture = if image.is_paletted {
                let image = glium::texture::RawImage2d {
                    data: (&image.data[..]).into(),
                    width: image.width,
                    height: image.height,
                    format: ClientFormat::U8,
                };
                Texture2d::with_format(
                    facade,
                    image,
                    texture::UncompressedFloatFormat::U8,
                    texture::MipmapsOption::NoMipmap,
                )?
            } else {
                let image = glium::texture::RawImage2d::from_raw_rgba(
                    image.data,
                    (image.width, image.height),
                );
                Texture2d::with_format(
                    facade,
                    image,
                    texture::UncompressedFloatFormat::U8U8U8U8,
                    texture::MipmapsOption::AutoGeneratedMipmaps,
                )?
            };
            // Hacky, clear cache when sprite id changes, so the sprite can be reloaded
            // by clicking away and back.
            let clear = cached_textures.first().map(|x| (x.1).0 != tex_id.0).unwrap_or(false);
            if clear {
                cached_textures.clear();
            }
            cached_textures.push((Rc::new(texture), tex_id));
            Ok(cached_textures.last().unwrap().0.clone())
        }
    }

    pub fn cached_palette_texture(&mut self, palette: &[u8]) -> Result<Rc<Texture1d>, Error> {
        if palette.len() != 0x400 {
            return Err(anyhow!("Palette must have 0x100 RGB0 entries"));
        }
        if let Some(ref palette) = self.draw_params.cached_palette {
            Ok(palette.clone())
        } else {
            let facade = self.gl.facade();
            let texture = {
                let image = glium::texture::RawImage1d {
                    data: palette.into(),
                    width: u32::try_from(palette.len() / 4)?,
                    format: ClientFormat::U8U8U8U8,
                };
                Texture1d::with_format(
                    facade,
                    image,
                    texture::UncompressedFloatFormat::U8U8U8U8,
                    texture::MipmapsOption::NoMipmap,
                )?
            };
            let texture = Rc::new(texture);
            self.draw_params.cached_palette = Some(texture.clone());
            Ok(texture)
        }
    }
}

struct DrawParams {
    vertices: VertexBuffer<gl::Vertex>,
    indices: IndexBuffer<u32>,
    lines: DrawLines,
    program: Program,
    ao_program: Program,
    depth_program: Program,
    normal_program: Program,
    paletted_program: Program,
    cached_textures: Vec<(Rc<Texture2d>, TextureId)>,
    cached_palette: Option<Rc<Texture1d>>,
}

/// sprite_id, type, layer
#[derive(Eq, Copy, Clone, PartialEq, Debug)]
pub struct TextureId(pub usize, pub SpriteType, pub usize);

struct TextureLines(Vec<(TextureId, LineBuffer)>);

struct DrawLines {
    texture_lines: TextureLines,
    program: Program,
}

impl TextureLines {
    fn buffer_for_texture<F: FnOnce() -> Vec<(Rect, Color, u8)>>(
        &mut self,
        facade: &Headless,
        tex_id: &TextureId,
        init: F,
    ) -> &mut LineBuffer {
        match self.0.iter().position(|x| x.0 == *tex_id) {
            Some(s) => &mut self.0[s].1,
            None => {
                let rects = init();
                let mut vertices = Vec::with_capacity(rects.len() * 4);
                for &(ref rect, color, ty) in rects.iter() {
                    let color = [color.0, color.1, color.2, color.3];
                    let left = rect.x as f32;
                    let top = rect.y as f32;
                    let right = left + rect.width as f32;
                    let bottom = top + rect.height as f32;
                    vertices.extend([
                        gl::LineVertex {
                            pos: [left, top],
                            color,
                            ty,
                        },
                        gl::LineVertex {
                            pos: [right, top],
                            color,
                            ty,
                        },
                        gl::LineVertex {
                            pos: [left, bottom],
                            color,
                            ty,
                        },
                        gl::LineVertex {
                            pos: [right, bottom],
                            color,
                            ty,
                        },
                    ].iter().cloned());
                }
                let mut indices = Vec::with_capacity(rects.len() * 8);
                for i in 0..rects.len() {
                    let i = i as u32 * 4;
                    indices.extend(
                        [i, i + 1, i + 1, i + 3, i + 3, i + 2, i + 2, i].iter().cloned()
                    );
                }
                let vertices = VertexBuffer::new(facade, &vertices)
                    .expect("Couldn't create vertex buffer");
                let indices = IndexBuffer::new(facade, PrimitiveType::LinesList, &indices)
                    .expect("Couldn't create vertex buffer");

                // Hacky, clear cache when sprite id changes, so the sprite can be reloaded
                // by clicking away and back.
                let clear = self.0.first().map(|x| (x.0).0 != tex_id.0).unwrap_or(false);
                if clear {
                    self.0.clear();
                }

                self.0.push((tex_id.clone(), LineBuffer {
                    vertices,
                    indices,
                }));
                let pos = self.0.len() - 1;
                &mut self.0[pos].1
            }
        }
    }
}

struct LineBuffer {
    vertices: VertexBuffer<gl::LineVertex>,
    indices: IndexBuffer<u32>,
}

impl DrawLines {
    fn new(gl: &mut gl::Context) -> DrawLines {
        let program = Program::new(gl.facade(), &shaders::LINE_VERTEX, &shaders::LINE_FRAGMENT);
        DrawLines {
            texture_lines: TextureLines(Vec::new()),
            program,
        }
    }
}

fn sprite_render_program(gl: &mut gl::Context) -> Program {
    Program::new(gl.facade(), &shaders::SPRITE_VERTEX, &shaders::SPRITE_FRAGMENT)
}

#[derive(Copy, Clone)]
pub struct Color(pub f32, pub f32, pub f32, pub f32);

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Rect {
    pub x: u32,
    pub y: u32,
    pub width: u32,
    pub height: u32,
}

impl Rect {
    pub fn new(x: u32, y: u32, width: u32, height: u32) -> Rect {
        Rect {
            x,
            y,
            width,
            height,
        }
    }
}
