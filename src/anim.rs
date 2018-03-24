use std::io::{Read, Seek, SeekFrom};

use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use ddsfile::{Dds, D3DFormat};
use failure::{Error, ResultExt};

pub struct Anim {
    layer_names: Vec<String>,
    sprite: SpriteData,
    read: Box<ReadSeek>,
}

pub struct MainSd {
    layer_names: Vec<String>,
    sprites: Vec<SpriteType>,
    read: Box<ReadSeek>,
}

trait ReadSeek: Read + Seek + Send { }
impl<T: Read + Seek + Send> ReadSeek for T { }

enum SpriteType {
    Ref(u32),
    Data(SpriteData),
}

pub struct SpriteData {
    frames: Vec<Frame>,
    // The textures for each layer, they are not required to exist.
    textures: Vec<Option<Texture>>,
    pub unk2: u16,
    pub unk3: u32,
}

struct Frame {
    tex_x: u16,
    tex_y: u16,
    x_off: u16,
    y_off: u16,
    width: u16,
    height: u16,
}

#[derive(Clone, Debug)]
struct Texture {
    offset: u32,
    size: u32,
    width: u16,
    height: u16,
}

const ANIM_MAGIC: u32 = 0x4d494e41;

impl Anim {
    pub fn read<R: Read + Seek + Send + 'static>(mut r: R) -> Result<Anim, Error> {
        let magic = r.read_u32::<LE>()?;
        if magic != ANIM_MAGIC {
            return Err(format_err!("Incorrect magic {:08x}", magic));
        }
        let ty = r.read_u32::<LE>()?;
        if ty != 0x0202 && ty != 0x0204 {
            return Err(format_err!("Incorrect type {:x}", ty));
        }
        let layers = r.read_u16::<LE>()?;
        let entries = r.read_u16::<LE>()?;
        if entries != 1 {
            return Err(format_err!("Not a single-entry file ({} entries)", entries));
        }
        let mut layer_names = Vec::with_capacity(layers as usize);
        for i in 0..layers {
            if i < 10 {
                let mut buf = [0u8; 0x20];
                r.read_exact(&mut buf)?;
                layer_names.push(String::from_utf8_lossy(&buf).into_owned());
            } else {
                layer_names.push(format!("Layer {}", i));
            }
        }
        r.seek(SeekFrom::Start(0x14c))?;

        let frame_count = r.read_u16::<LE>()?;
        if frame_count == 0 {
            return Err(format_err!("No frames for a single-entry file"));
        }
        let unk2 = r.read_u16::<LE>()?;
        let unk3 = r.read_u32::<LE>()?;
        let frame_arr_offset = r.read_u32::<LE>()?;
        let textures = read_textures(&mut r, layers as u32)?;
        r.seek(SeekFrom::Start(frame_arr_offset as u64))?;
        let frames = read_frames(&mut r, frame_count)?;
        let sprite = SpriteData {
            frames,
            textures,
            unk2,
            unk3,
        };

        Ok(Anim {
            layer_names,
            sprite,
            read: Box::new(r),
        })
    }

    pub fn texture(&mut self, layer: usize) -> Result<RgbaTexture, Error> {
        let texture = self.sprite.textures.get(layer).and_then(|x| x.as_ref())
            .ok_or_else(|| format_err!("No layer {:x}", layer))?
            .clone();
        self.read.seek(SeekFrom::Start(texture.offset as u64))?;
        read_texture(&mut self.read, &texture)
    }
}

impl MainSd {
    pub fn read<R: Read + Seek + Send + 'static>(mut r: R) -> Result<MainSd, Error> {
        let magic = r.read_u32::<LE>()?;
        if magic != ANIM_MAGIC {
            return Err(format_err!("Incorrect magic {:08x}", magic));
        }
        let ty = r.read_u32::<LE>()?;
        if ty != 0x0101 {
            return Err(format_err!("Incorrect type {:x}", ty));
        }
        let layers = r.read_u16::<LE>()?;
        let entries = r.read_u16::<LE>()?;
        let mut layer_names = Vec::with_capacity(layers as usize);
        for i in 0..layers {
            if i < 10 {
                let mut buf = [0u8; 0x20];
                r.read_exact(&mut buf)?;
                layer_names.push(String::from_utf8_lossy(&buf).into_owned());
            } else {
                layer_names.push(format!("Layer {}", i));
            }
        }
        r.seek(SeekFrom::Start(0x14c))?;
        let mut sprite_offsets = vec![0; entries as usize];
        r.read_u32_into::<LE>(&mut sprite_offsets)?;
        let mut sprites = Vec::with_capacity(entries as usize);
        for (i, offset) in sprite_offsets.into_iter().enumerate() {
            r.seek(SeekFrom::Start(offset as u64))?;
            let frame_count = r.read_u16::<LE>()?;
            if frame_count == 0 {
                let ref_id = r.read_u32::<LE>()?;
                if ref_id > entries as u32 {
                    return Err(
                        format_err!("Image {:x} refers past entry amount to {:x}", i, ref_id)
                    );
                }
                sprites.push(SpriteType::Ref(ref_id));
            } else {
                let unk2 = r.read_u16::<LE>()?;
                let unk3 = r.read_u32::<LE>()?;
                let frame_arr_offset = r.read_u32::<LE>()?;
                let textures = read_textures(&mut r, layers as u32)
                    .with_context(|_| format!("Invalid image {:x}", i))?;
                r.seek(SeekFrom::Start(frame_arr_offset as u64))?;
                let frames = read_frames(&mut r, frame_count)?;
                sprites.push(SpriteType::Data(SpriteData {
                    frames,
                    textures,
                    unk2,
                    unk3,
                }));
            }
        }
        Ok(MainSd {
            layer_names,
            sprites,
            read: Box::new(r),
        })
    }

    pub fn sprites(&self) -> MainSdSprites {
        MainSdSprites(self, 0)
    }

    pub fn sprite_data(&self, sprite: usize) -> Option<&SpriteData> {
        match *self.sprites.get(sprite)? {
            SpriteType::Ref(r) => match *self.sprites.get(r as usize)? {
                SpriteType::Ref(r2) => {
                    warn!("Double ref {:x} -> {:x} -> {:x}", sprite, r, r2);
                    None
                }
                SpriteType::Data(ref d) => Some(d),
            },
            SpriteType::Data(ref d) => Some(d),
        }
    }

    /// Format is RGBA
    pub fn texture(&mut self, sprite: usize, layer: usize) -> Result<RgbaTexture, Error> {
        let texture = self.sprite_data(sprite)
            .ok_or_else(|| format_err!("No data for sprite {:x}", sprite))?
            .textures.get(layer).and_then(|x| x.as_ref())
            .ok_or_else(|| format_err!("No layer {:x} for sprite {:x}", layer, sprite))?
            .clone();
        self.read.seek(SeekFrom::Start(texture.offset as u64))?;
        read_texture(&mut self.read, &texture)
    }
}

fn read_texture<R: Read>(mut read: R, texture: &Texture) -> Result<RgbaTexture, Error> {
    let dds = Dds::read(&mut read)
        .map_err(|e| format_err!("Unable to read DDS: {}", e))?;
    let format = dds.get_d3d_format()
        .ok_or_else(|| format_err!("Unsupported DDS format"))?;
    let data = dds.get_data(0)
        .map_err(|e| format_err!("Unable to get DDS data: {}", e))?;
    let data = match format {
        D3DFormat::DXT1 => {
            decode_dxt1(&data, texture.width as u32, texture.height as u32)?
        }
        D3DFormat::DXT5 => {
            decode_dxt5(&data, texture.width as u32, texture.height as u32)?
        }
        _ => return Err(format_err!("Unsupported DDS format {:?}", format)),
    };
    Ok(RgbaTexture {
        data,
        width: texture.width as u32,
        height: texture.height as u32,
    })
}

/// Returns the bytes with alpha multiplied
fn decode_dxt5(data: &[u8], width: u32, height: u32) -> Result<Vec<u8>, Error> {
    let mut read = data;
    let size = (width * height) as usize;
    let mut out = vec![0u8; size * 4];
    let mut pos = 0u32;
    for _y_tile in 0..(height / 4) {
        for x_tile in 0..(width / 4) {
            let alpha = read.read_u64::<LE>()?;
            let a0_raw = (alpha & 0xff) as u8;
            let a1_raw = ((alpha >> 8) & 0xff) as u8;
            let a0 = a0_raw as f32;
            let a1 = a1_raw as f32;
            let mut alpha = alpha >> 16;
            let alpha_table = if a0_raw > a1_raw {
                [
                    a0_raw,
                    a1_raw,
                    ((a0 * 6.0 + a1) / 7.0) as u8,
                    ((a0 * 5.0 + a1 * 2.0) / 7.0) as u8,
                    ((a0 * 4.0 + a1 * 3.0) / 7.0) as u8,
                    ((a0 * 3.0 + a1 * 4.0) / 7.0) as u8,
                    ((a0 * 2.0 + a1 * 5.0) / 7.0) as u8,
                    ((a0 + a1 * 6.0) / 7.0) as u8,
                ]
            } else {
                [
                    a0_raw,
                    a1_raw,
                    ((a0 * 4.0 + a1) / 5.0) as u8,
                    ((a0 * 3.0 + a1 * 2.0) / 5.0) as u8,
                    ((a0 * 2.0 + a1 * 3.0) / 5.0) as u8,
                    ((a0 + a1 * 4.0) / 5.0) as u8,
                    0,
                    255,
                ]
            };
            let c0_raw = read.read_u16::<LE>()?;
            let c1_raw = read.read_u16::<LE>()?;
            let c0 = color16(c0_raw);
            let c1 = color16(c1_raw);
            let mut colors = read.read_u32::<LE>()?;
            let (c2, c3) = (
                (
                    (c0.0 * 2.0 / 3.0 + c1.0 / 3.0),
                    (c0.1 * 2.0 / 3.0 + c1.1 / 3.0),
                    (c0.2 * 2.0 / 3.0 + c1.2 / 3.0),
                ),
                (
                    (c1.0 * 2.0 / 3.0 + c0.0 / 3.0),
                    (c1.1 * 2.0 / 3.0 + c0.1 / 3.0),
                    (c1.2 * 2.0 / 3.0 + c0.2 / 3.0),
                ),
            );
            let table = [rgba(c0) & 0xffffff, rgba(c1) & 0xffffff, rgb(c2), rgb(c3)];
            let mut pos = pos;
            for _y in 0..4 {
                for x in 0..4 {
                    let alpha_mul = alpha_table[(alpha & 7) as usize] as f32 / 255.0;
                    let color = table[(colors & 3) as usize];
                    let color =
                        (((color & 0xff) as f32 * alpha_mul) as u32) |
                        (((((color >> 8) & 0xff) as f32 * alpha_mul) as u32) << 8) |
                        (((((color >> 16) & 0xff) as f32 * alpha_mul) as u32) << 16) |
                        ((alpha_table[(alpha & 7) as usize] as u32) << 24);
                    colors = colors >> 2;
                    alpha = alpha >> 3;
                    let pixel_pos = (pos + x_tile as u32 * 4 + x) as usize;
                    (&mut out[(pixel_pos * 4)..]).write_u32::<LE>(color)?;
                }
                pos += width;
            }
        }
        pos += width as u32 * 4;
    }
    Ok(out)
}

fn decode_dxt1(data: &[u8], width: u32, height: u32) -> Result<Vec<u8>, Error> {
    let mut read = data;
    let size = (width * height) as usize;
    let mut out = vec![0u8; size * 4];
    let mut pos = 0u32;
    for _y_tile in 0..(height / 4) {
        for x_tile in 0..(width / 4) {
            let c0_raw = read.read_u16::<LE>()?;
            let c1_raw = read.read_u16::<LE>()?;
            let c0 = color16(c0_raw);
            let c1 = color16(c1_raw);
            let mut colors = read.read_u32::<LE>()?;
            let (c2, c3) = match c0_raw > c1_raw {
                true => (
                    (
                        (c0.0 * 2.0 / 3.0 + c1.0 / 3.0),
                        (c0.1 * 2.0 / 3.0 + c1.1 / 3.0),
                        (c0.2 * 2.0 / 3.0 + c1.2 / 3.0),
                        (1.0),
                    ),
                    (
                        (c1.0 * 2.0 / 3.0 + c0.0 / 3.0),
                        (c1.1 * 2.0 / 3.0 + c0.1 / 3.0),
                        (c1.2 * 2.0 / 3.0 + c0.2 / 3.0),
                        (1.0),
                    ),
                ),
                false => (
                    (
                        (c0.0 / 2.0 + c1.0 / 2.0),
                        (c0.1 / 2.0 + c1.1 / 2.0),
                        (c0.2 / 2.0 + c1.2 / 2.0),
                        (1.0),
                    ),
                    (0.0, 0.0, 0.0, 0.0),
                ),
            };
            let table = [rgba(c0), rgba(c1), rgba(c2), rgba(c3)];
            let mut pos = pos;
            for _y in 0..4 {
                for x in 0..4 {
                    let color = table[(colors & 3) as usize];
                    colors = colors >> 2;
                    let pixel_pos = (pos + x_tile as u32 * 4 + x) as usize;
                    (&mut out[(pixel_pos * 4)..]).write_u32::<LE>(color)?;
                }
                pos += width;
            }
        }
        pos += width as u32 * 4;
    }
    Ok(out)
}

fn color16(input: u16) -> (f32, f32, f32, f32) {
    (
        ((input & 0xf800) >> 11) as f32 / (0x1f as f32),
        ((input & 0x7e0) >> 5) as f32 / (0x3f as f32),
        (input & 0x1f) as f32 / (0x1f as f32),
        1.0,
    )
}

fn rgba(input: (f32, f32, f32, f32)) -> u32 {
    (input.0 * 255.0) as u8 as u32 |
        (((input.1 * 255.0) as u8 as u32) << 8) |
        (((input.2 * 255.0) as u8 as u32) << 16) |
        (((input.3 * 255.0) as u8 as u32) << 24)
}

fn rgb(input: (f32, f32, f32)) -> u32 {
    (input.0 * 255.0) as u8 as u32 |
        (((input.1 * 255.0) as u8 as u32) << 8) |
        (((input.2 * 255.0) as u8 as u32) << 16)
}

pub struct RgbaTexture {
    pub data: Vec<u8>,
    pub width: u32,
    pub height: u32,
}

fn read_textures<R: Read>(mut r: R, count: u32) -> Result<Vec<Option<Texture>>, Error> {
    (0..count).map(|_| {
        let texture = Texture {
            offset: r.read_u32::<LE>()?,
            size: r.read_u32::<LE>()?,
            width: r.read_u16::<LE>()?,
            height: r.read_u16::<LE>()?,
        };
        Ok(if texture.offset != 0 {
            Some(texture)
        } else {
            None
        })
    }).collect()
}

fn read_frames<R: Read>(mut r: R, count: u16) -> Result<Vec<Frame>, Error> {
    (0..count).map(|_| {
        Ok(Frame {
            tex_x: r.read_u16::<LE>()?,
            tex_y: r.read_u16::<LE>()?,
            x_off: r.read_u16::<LE>()?,
            y_off: r.read_u16::<LE>()?,
            width: r.read_u16::<LE>()?,
            height: r.read_u16::<LE>()?,
        })
    }).collect()
}

pub struct MainSdSprites<'a>(&'a MainSd, u32);

impl<'a> Iterator for MainSdSprites<'a> {
    type Item = Sprite;
    fn next(&mut self) -> Option<Self::Item> {
        let sprite_count = self.0.sprites.len() as u32;
        if self.1 >= sprite_count {
            None
        } else {
            self.1 += 1;
            Some(Sprite)
        }
    }
}

pub struct Sprite;
