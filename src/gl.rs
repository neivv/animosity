use glium::backend::glutin::headless::Headless;
use glium::framebuffer::SimpleFrameBuffer;
use glium::texture::Texture2d;
use glium::vertex::VertexBuffer;
use glium::glutin;

pub struct Context {
    facade: Headless,
    render_target: Texture2d,
    width: u32,
    height: u32,
    stride: u32,
}

#[derive(Copy, Clone)]
pub struct Vertex {
    pub pos: [f32; 2],
    pub tex: [f32; 2],
}

implement_vertex!(Vertex, pos, tex);

#[derive(Copy, Clone, Debug)]
pub struct LineVertex {
    pub pos: [f32; 2],
    pub color: [f32; 4],
    pub ty: u8,
}

implement_vertex!(LineVertex, pos, color, ty);

impl Context {
    pub fn new(width: u32, height: u32) -> Context {
        let stride = width.next_power_of_two();
        let context = glutin::HeadlessRendererBuilder::new(stride, height)
            .with_gl(glutin::GlRequest::Specific(glutin::Api::OpenGl, (3, 0)))
            .with_gl_profile(glutin::GlProfile::Core)
            .build()
            .expect("Unable to create GL context");
        let facade = Headless::new(context)
            .expect("Unable to create GL context");
        let render_target = Texture2d::empty(&facade, stride, height)
            .expect("Unable to create texture");
        Context {
            facade,
            render_target,
            height,
            width,
            stride,
        }
    }

    pub fn set_vertices(&mut self, vertices: &[Vertex]) -> VertexBuffer<Vertex> {
        VertexBuffer::new(self.facade(), vertices)
            .expect("Couldn't create vertex buffer")
    }

    pub fn buf_dimensions(&self) -> (u32, u32) {
        (self.width, self.height)
    }

    pub fn stride(&self) -> u32 {
        self.stride
    }

    pub fn facade(&self) -> &Headless {
        &self.facade
    }

    pub fn framebuf(&self) -> (SimpleFrameBuffer, &Headless) {
        (self.render_target.as_surface(), &self.facade)
    }

    pub fn resize_buf(&mut self, width: u32, height: u32) {
        let stride = width.next_power_of_two();
        self.width = width;
        if stride == self.stride && height == self.height {
            return;
        }
        self.stride = stride;
        self.height = height;
        self.render_target = Texture2d::empty(&self.facade, stride, height)
            .expect("Unable to create texture");
    }

    pub fn framebuf_bytes(&self) -> (Vec<u8>, u32, u32) {
        let (width, height) = self.render_target.dimensions();
        let mut buffer = self.render_target.read_to_pixel_buffer();
        let mut bytes = buffer.map_read().to_owned();
        while bytes.len() & 0xf != 0 {
            bytes.push((0, 0, 0, 0));
        }
        let len = bytes.len() * 4;
        let mut result = Vec::with_capacity(len);
        unsafe {
            let mut out = result.as_mut_ptr();
            let mut input = bytes.as_ptr();
            for _ in 0..bytes.len() / 0x10 {
                for _ in 0..0x10 {
                    let (r, g, b, a) = *input;
                    *out = b;
                    *out.offset(1) = g;
                    *out.offset(2) = r;
                    *out.offset(3) = a;
                    out = out.offset(4);
                    input = input.offset(1);
                }
            }
            result.set_len(len);
        }

        (result, width, height)
    }
}
