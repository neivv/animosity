use std::convert::TryFrom;
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::iter;
use std::sync::Mutex;

use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use ddsfile::{Dds, D3DFormat};
use failure::{Error, ResultExt};

pub struct Anim {
    layer_names: Vec<String>,
    sprite: SpriteData,
    read: Mutex<Box<ReadSeek>>,
}

pub struct MainSd {
    layer_names: Vec<String>,
    sprites: Vec<SpriteType>,
    read: Mutex<Box<ReadSeek>>,
}

trait ReadSeek: Read + Seek + Send { }
impl<T: Read + Seek + Send> ReadSeek for T { }

pub enum SpriteType {
    Ref(u32),
    Data(SpriteData),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ValuesOrRef {
    Values(SpriteValues),
    Ref(u32),
}

pub struct SpriteData {
    frames: Vec<Frame>,
    // The textures for each layer, they are not required to exist.
    textures: Vec<Option<Texture>>,
    values: SpriteValues,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct SpriteValues {
    pub unk2: u16,
    pub unk3a: u16,
    pub unk3b: u16,
}

#[derive(Clone, Debug)]
pub struct TexChanges {
    pub frames: Vec<Frame>,
    // The textures for each layer, they are not required to exist.
    pub textures: Vec<Option<(Texture, Vec<u8>)>>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Frame {
    pub tex_x: u16,
    pub tex_y: u16,
    pub x_off: u16,
    pub y_off: u16,
    pub width: u16,
    pub height: u16,
    pub unknown: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Texture {
    pub offset: u32,
    pub size: u32,
    pub width: u16,
    pub height: u16,
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
                let end = buf.iter().position(|x| *x == 0).unwrap_or(buf.len());
                layer_names.push(String::from_utf8_lossy(&buf[..end]).into_owned());
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
        let unk3a = r.read_u16::<LE>()?;
        let unk3b = r.read_u16::<LE>()?;
        let frame_arr_offset = r.read_u32::<LE>()?;
        let textures = read_textures(&mut r, layers as u32)?;
        r.seek(SeekFrom::Start(frame_arr_offset as u64))?;
        let frames = read_frames(&mut r, frame_count)?;
        let sprite = SpriteData {
            frames,
            textures,
            values: SpriteValues {
                unk2,
                unk3a,
                unk3b,
            },
        };

        Ok(Anim {
            layer_names,
            sprite,
            read: Mutex::new(Box::new(r)),
        })
    }

    /// Writes a patched anim to `out`. `textures` contains the changed textures and their
    /// layer id, other textures are read from the original read object.
    ///
    /// Since the read object is kept open (and used) here, `out` cannot be the same file as
    /// what was used to read this anim.
    pub fn write_patched<W: Write + Seek>(
        &mut self,
        mut out: W,
        scale: u8,
        layer_names: &[String],
        data_changes: &SpriteValues,
        textures: Option<&TexChanges>,
    ) -> Result<(), Error> {
        out.write_u32::<LE>(ANIM_MAGIC)?;
        out.write_u32::<LE>(scale as u32 | 0x200)?;
        let texture_count = match textures {
            Some(s) => s.textures.len() as u16,
            None => self.sprite.textures.len() as u16,
        };
        out.write_u16::<LE>(texture_count)?;
        out.write_u16::<LE>(1)?;
        if layer_names.len() > 10 {
            return Err(format_err!("Cannot have more than 10 layers, had {}", layer_names.len()));
        }
        for name in layer_names.iter().map(|x| &**x).chain(iter::repeat("")).take(10) {
            let mut buf = [0u8; 0x20];
            (&mut buf[..]).write_all(name.as_bytes())?;
            out.write_all(&buf)?;
        }
        let frames = match textures {
            Some(changes) => &changes.frames,
            None => &self.sprite.frames,
        };
        if frames.len() == 0 {
            return Err(format_err!("Image has no frames"));
        }
        out.write_u16::<LE>(frames.len() as u16)?;
        out.write_u16::<LE>(data_changes.unk2)?;
        out.write_u16::<LE>(data_changes.unk3a)?;
        out.write_u16::<LE>(data_changes.unk3b)?;
        let frame_arr_offset_pos = out.seek(SeekFrom::Current(0))?;
        out.write_u32::<LE>(!0)?;
        if let Some(changes) = textures {
            write_textures_patched(&mut out, changes)?;
        } else {
            let mut read = self.read.lock().unwrap();
            write_textures_unchanged(&mut out, &mut *read, &self.sprite.textures)?;
        }
        let frame_arr_offset = u32::try_from(out.seek(SeekFrom::Current(0))?)
            .map_err(|_| format_err!("Output file too big"))?;
        write_frames(&mut out, frames)?;
        out.seek(SeekFrom::Start(frame_arr_offset_pos))?;
        out.write_u32::<LE>(frame_arr_offset)?;
        Ok(())
    }

    pub fn frames(&self) -> &[Frame] {
        &self.sprite.frames
    }

    pub fn texture_sizes(&self) -> &[Option<Texture>] {
        &self.sprite.textures
    }

    pub fn layer_names(&self) -> &[String] {
        &self.layer_names
    }

    pub fn sprite_values(&self) -> SpriteValues {
        self.sprite.values
    }

    pub fn sprite_data(&self) -> &SpriteData {
        &self.sprite
    }

    pub fn texture(&self, layer: usize) -> Result<RgbaTexture, Error> {
        let texture = self.sprite.textures.get(layer).and_then(|x| x.as_ref())
            .ok_or_else(|| format_err!("No layer {:x}", layer))?
            .clone();
        let mut read = self.read.lock().unwrap();
        read.seek(SeekFrom::Start(texture.offset as u64))?;
        read_texture(&mut *read, &texture)
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
                let end = buf.iter().position(|x| *x == 0).unwrap_or(buf.len());
                layer_names.push(String::from_utf8_lossy(&buf[..end]).into_owned());
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
                let unk3a = r.read_u16::<LE>()?;
                let unk3b = r.read_u16::<LE>()?;
                let frame_arr_offset = r.read_u32::<LE>()?;
                let textures = read_textures(&mut r, layers as u32)
                    .with_context(|_| format!("Invalid image {:x}", i))?;
                r.seek(SeekFrom::Start(frame_arr_offset as u64))?;
                let frames = read_frames(&mut r, frame_count)?;
                sprites.push(SpriteType::Data(SpriteData {
                    frames,
                    textures,
                    values: SpriteValues {
                        unk2,
                        unk3a,
                        unk3b,
                    },
                }));
            }
        }
        Ok(MainSd {
            layer_names,
            sprites,
            read: Mutex::new(Box::new(r)),
        })
    }

    /// Writes a patched anim to `out`. `textures` contains the changed textures and their
    /// sprite and layer ids, other textures are read from the original read object.
    ///
    /// Since the read object is kept open (and used) here, `out` cannot be the same file as
    /// what was used to read this anim.
    pub fn write_patched<W: Write + Seek>(
        &mut self,
        mut out: W,
        sprite_count: u16,
        layer_names: &[String],
        data_changes: &[(usize, ValuesOrRef)],
        textures: &[(usize, &TexChanges)],
    ) -> Result<(), Error> {
        out.write_u32::<LE>(ANIM_MAGIC)?;
        out.write_u32::<LE>(0x0101)?;
        out.write_u16::<LE>(layer_names.len() as u16)?;
        out.write_u16::<LE>(sprite_count)?;
        if layer_names.len() > 10 {
            return Err(format_err!("Cannot have more than 10 layers, had {}", layer_names.len()));
        }
        for name in layer_names.iter().map(|x| &**x).chain(iter::repeat("")).take(10) {
            let mut buf = [0u8; 0x20];
            (&mut buf[..]).write_all(name.as_bytes())?;
            out.write_all(&buf)?;
        }
        let sprite_offset_pos = out.seek(SeekFrom::Current(0))?;
        let mut sprite_offsets = vec![!0u32; sprite_count as usize];
        for &x in &sprite_offsets {
            out.write_u32::<LE>(x)?;
        }
        let mut read = self.read.lock().unwrap();
        for i in 0..sprite_count {
            let i = i as usize;
            sprite_offsets[i] = u32::try_from(out.seek(SeekFrom::Current(0))?)
                .map_err(|_| format_err!("Output file too big"))?;

            let store;
            let values = match data_changes.iter().find(|x| x.0 == i) {
                Some(&(_, ref vals)) => vals,
                None => {
                    store = values_or_ref_sd(&self.sprites, i);
                    &store
                }
            };
            match *values {
                ValuesOrRef::Ref(img) => {
                    if img >= sprite_count as u32 {
                        return Err(format_err!("Image {} refers to invalid image {}", i, img));
                    }
                    out.write_u16::<LE>(0)?;
                    out.write_u32::<LE>(img)?;
                    for _ in 0..3 {
                        out.write_u16::<LE>(0)?;
                    }
                }
                ValuesOrRef::Values(ref values) => {
                    let texture_changes = textures.iter().find(|x| x.0 == i).map(|x| &x.1);
                    let frames = match texture_changes {
                        Some(changes) => &changes.frames,
                        None => match self.sprites[i] {
                            SpriteType::Ref(_) => {
                                return Err(format_err!("No texture to write for image {}", i));
                            }
                            SpriteType::Data(ref data) => &data.frames,
                        },
                    };
                    if frames.len() == 0 {
                        return Err(format_err!("Image {} has no frames", i));
                    }
                    out.write_u16::<LE>(frames.len() as u16)?;
                    out.write_u16::<LE>(values.unk2)?;
                    out.write_u16::<LE>(values.unk3a)?;
                    out.write_u16::<LE>(values.unk3b)?;
                    let frame_arr_offset_pos = out.seek(SeekFrom::Current(0))?;
                    out.write_u32::<LE>(!0)?;
                    if let Some(changes) = texture_changes {
                        write_textures_patched(&mut out, changes)?;
                    } else {
                        let textures = match self.sprites[i] {
                            SpriteType::Ref(_) => {
                                return Err(format_err!("No texture to write for image {}", i));
                            }
                            SpriteType::Data(ref data) => &data.textures,
                        };
                        write_textures_unchanged(&mut out, &mut *read, textures)?;
                    }
                    let frame_arr_offset = u32::try_from(out.seek(SeekFrom::Current(0))?)
                        .map_err(|_| format_err!("Output file too big"))?;
                    write_frames(&mut out, frames)?;
                    let cont_pos = out.seek(SeekFrom::Current(0))?;
                    out.seek(SeekFrom::Start(frame_arr_offset_pos))?;
                    out.write_u32::<LE>(frame_arr_offset)?;
                    out.seek(SeekFrom::Start(cont_pos))?;
                }
            }
        }
        out.seek(SeekFrom::Start(sprite_offset_pos))?;
        for &x in &sprite_offsets {
            out.write_u32::<LE>(x)?;
        }
        Ok(())
    }

    pub fn frames(&self, sprite: usize) -> Option<&[Frame]> {
        self.sprite_data(sprite).map(|x| &x.frames[..])
    }

    pub fn texture_sizes(&self, sprite: usize) -> Option<&[Option<Texture>]> {
        self.sprite_data(sprite).map(|x| &x.textures[..])
    }

    pub fn layer_names(&self) -> &[String] {
        &self.layer_names
    }

    pub fn sprites(&self) -> &[SpriteType] {
        &self.sprites
    }

    pub fn sprite_values(&self, sprite: usize) -> Option<SpriteValues> {
        sprite_values_sd(&self.sprites, sprite)
    }

    pub fn values_or_ref(&self, sprite: usize) -> ValuesOrRef {
        values_or_ref_sd(&self.sprites, sprite)
    }

    pub fn sprite_data(&self, sprite: usize) -> Option<&SpriteData> {
        sprite_data_sd(&self.sprites, sprite)
    }

    /// Format is RGBA
    pub fn texture(&self, sprite: usize, layer: usize) -> Result<RgbaTexture, Error> {
        let texture = self.sprite_data(sprite)
            .ok_or_else(|| format_err!("No data for sprite {:x}", sprite))?
            .textures.get(layer).and_then(|x| x.as_ref())
            .ok_or_else(|| format_err!("No layer {:x} for sprite {:x}", layer, sprite))?
            .clone();
        let mut read = self.read.lock().unwrap();
        read.seek(SeekFrom::Start(texture.offset as u64))?;
        read_texture(&mut *read, &texture)
    }
}

fn sprite_data_sd<'a>(sprites: &'a [SpriteType], sprite: usize) -> Option<&'a SpriteData> {
    match *sprites.get(sprite)? {
        SpriteType::Ref(r) => match *sprites.get(r as usize)? {
            SpriteType::Ref(r2) => {
                warn!("Double ref {:x} -> {:x} -> {:x}", sprite, r, r2);
                None
            }
            SpriteType::Data(ref d) => Some(d),
        },
        SpriteType::Data(ref d) => Some(d),
    }
}

fn values_or_ref_sd(sprites: &[SpriteType], index: usize) -> ValuesOrRef {
    match sprites[index] {
        SpriteType::Ref(r) => ValuesOrRef::Ref(r),
        SpriteType::Data(ref d) => ValuesOrRef::Values(d.values),
    }
}

fn sprite_values_sd(sprites: &[SpriteType], index: usize) -> Option<SpriteValues> {
    let sprite = sprite_data_sd(sprites, index)?;
    Some(sprite.values.clone())
}

fn read_texture<R: ReadSeek>(mut read: R, texture: &Texture) -> Result<RgbaTexture, Error> {
    const DDS_MAGIC: u32 = 0x20534444;
    const BMP_MAGIC: u32 = 0x20504d42;
    let magic = read.read_u32::<LE>()?;
    if magic == DDS_MAGIC {
        read.seek(SeekFrom::Current(-4))?;
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
    } else if magic == BMP_MAGIC {
        // Raw monochrome bitmap, 0x00 or 0xff per pixel
        let mut pixels = vec![0; texture.width as usize * texture.height as usize];
        read.read_exact(&mut pixels[..])?;
        let mut data = Vec::with_capacity(pixels.len() * 4);
        for p in pixels {
            if p == 0 {
                data.write_u32::<LE>(0).unwrap();
            } else {
                data.write_u32::<LE>(!0).unwrap();
            }
        }
        Ok(RgbaTexture {
            data,
            width: texture.width as u32,
            height: texture.height as u32,
        })
    } else {
        return Err(format_err!("Unknown texture format {:08x}", magic));
    }
}

/// Returns the bytes with alpha multiplied
fn decode_dxt5(data: &[u8], width: u32, height: u32) -> Result<Vec<u8>, Error> {
    let mut read = data;
    let size = (width * height) as usize;
    let mut out = vec![0u8; size * 4];
    let mut pos = 0u32;
    for _y_tile in 0..(height / 4) {
        'single_block: for x_tile in 0..(width / 4) {
            let (mut block, rest) = match read.len() {
                x if x < 16 => return Err(format_err!("Reached end of input")),
                _ => read.split_at(16),
            };
            read = rest;
            let alpha = block.read_u64::<LE>()?;
            if alpha & !0xff00 == 0 {
                // Fully transparent block, rest can be skipped since `out` was zero-filled =)
                // (Full transparency can be specified in several ways though, and this won't
                // catch them all)
                continue 'single_block;
            }
            let a0_raw = (alpha & 0xff) as u8;
            let a1_raw = ((alpha >> 8) & 0xff) as u8;
            let a0 = a0_raw as f32;
            let a1 = a1_raw as f32;
            let mut alpha = alpha >> 16;
            let alpha_table = if a0_raw > a1_raw {
                [
                    a0,
                    a1,
                    a0 * (6.0 / 7.0) + a1 / 7.0,
                    a0 * (5.0 / 7.0) + a1 * (2.0 / 7.0),
                    a0 * (4.0 / 7.0) + a1 * (3.0 / 7.0),
                    a0 * (3.0 / 7.0) + a1 * (4.0 / 7.0),
                    a0 * (2.0 / 7.0) + a1 * (5.0 / 7.0),
                    a0 / 7.0 + a1 * (6.0 / 7.0),
                ]
            } else {
                [
                    a0,
                    a1,
                    a0 * (4.0 / 5.0) + a1 / 5.0,
                    a0 * (3.0 / 5.0) + a1 * (2.0 / 5.0),
                    a0 * (2.0 / 5.0) + a1 * (3.0 / 5.0),
                    a0 / 5.0 + a1 * (4.0 / 5.0),
                    0.0,
                    255.0,
                ]
            };
            let c0_raw = block.read_u16::<LE>()?;
            let c1_raw = block.read_u16::<LE>()?;
            let c0 = color16_no_alpha(c0_raw);
            let c1 = color16_no_alpha(c1_raw);
            let mut colors = block.read_u32::<LE>()?;
            let (c2, c3) = (
                (
                    ((c0.0 * 2.0 + c1.0) / 3.0),
                    ((c0.1 * 2.0 + c1.1) / 3.0),
                    ((c0.2 * 2.0 + c1.2) / 3.0),
                ),
                (
                    ((c1.0 * 2.0 + c0.0) / 3.0),
                    ((c1.1 * 2.0 + c0.1) / 3.0),
                    ((c1.2 * 2.0 + c0.2) / 3.0),
                ),
            );
            let table = [c0, c1, c2, c3];
            let mut pos = pos;
            for _y in 0..4 {
                // Skipping overflow checks
                let pixel_pos = pos.wrapping_add((x_tile as u32).wrapping_mul(4)) as usize;
                let byte_pos = pixel_pos.wrapping_mul(4);
                let line = &mut out[byte_pos..byte_pos + 16];

                for x in 0..4 {
                    let alpha_mul = alpha_table[(alpha & 7) as usize];
                    let color = table[(colors & 3) as usize];
                    line[x * 4 + 0] = (color.0 * alpha_mul) as u8;
                    line[x * 4 + 1] = (color.1 * alpha_mul) as u8;
                    line[x * 4 + 2] = (color.2 * alpha_mul) as u8;
                    line[x * 4 + 3] = alpha_table[(alpha & 7) as usize] as u8;
                    colors = colors >> 2;
                    alpha = alpha >> 3;
                }
                pos = pos.wrapping_add(width);
            }
        }
        pos = pos.wrapping_add((width as u32).wrapping_mul(4));
    }
    let time = start.elapsed();
    Ok(out)
}

fn decode_dxt1(data: &[u8], width: u32, height: u32) -> Result<Vec<u8>, Error> {
    let mut read = data;
    let size = (width * height) as usize;
    let mut out = vec![0u8; size * 4];
    let mut pos = 0u32;
    for _y_tile in 0..(height / 4) {
        for x_tile in 0..(width / 4) {
            let (mut block, rest) = match read.len() {
                x if x < 8 => return Err(format_err!("Reached end of input")),
                _ => read.split_at(8),
            };
            read = rest;
            let c0_raw = block.read_u16::<LE>()?;
            let c1_raw = block.read_u16::<LE>()?;
            let c0 = color16(c0_raw);
            let c1 = color16(c1_raw);
            let mut colors = block.read_u32::<LE>()?;
            let (c2, c3) = match c0_raw > c1_raw {
                true => (
                    (
                        (c0.0 * 2.0 + c1.0) / 3.0,
                        (c0.1 * 2.0 + c1.1) / 3.0,
                        (c0.2 * 2.0 + c1.2) / 3.0,
                        1.0,
                    ),
                    (
                        (c1.0 * 2.0 + c0.0) / 3.0,
                        (c1.1 * 2.0 + c0.1) / 3.0,
                        (c1.2 * 2.0 + c0.2) / 3.0,
                        1.0,
                    ),
                ),
                false => (
                    (
                        (c0.0 + c1.0) / 2.0,
                        (c0.1 + c1.1) / 2.0,
                        (c0.2 + c1.2) / 2.0,
                        1.0,
                    ),
                    (0.0, 0.0, 0.0, 0.0),
                ),
            };
            let table = [rgba(c0), rgba(c1), rgba(c2), rgba(c3)];
            let mut pos = pos;
            for _y in 0..4 {
                let pixel_pos = pos.wrapping_add((x_tile as u32).wrapping_mul(4)) as usize;
                let byte_pos = pixel_pos.wrapping_mul(4);
                let mut line = &mut out[byte_pos..byte_pos + 16];

                for _x in 0..4 {
                    let color = table[(colors & 3) as usize];
                    colors = colors >> 2;
                    line.write_u32::<LE>(color)?;
                }
                pos = pos.wrapping_add(width);
            }
        }
        pos = pos.wrapping_add((width as u32).wrapping_mul(4));
    }
    Ok(out)
}

fn color16_no_alpha(input: u16) -> (f32, f32, f32) {
    (
        ((input & 0xf800) >> 11) as f32 / (0x1f as f32),
        ((input & 0x7e0) >> 5) as f32 / (0x3f as f32),
        (input & 0x1f) as f32 / (0x1f as f32),
    )
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

fn write_textures_unchanged<W: Write + Seek, R: ReadSeek>(
    out: &mut W,
    read: &mut R,
    textures: &[Option<Texture>],
) -> Result<(), Error> {
    let start = out.seek(SeekFrom::Current(0))?;
    let mut zeroes = io::repeat(0).take(textures.len() as u64 * 0xc);
    io::copy(&mut zeroes, out)?;
    for (i, tex) in textures.iter().enumerate() {
        if let Some(ref tex) = *tex {
            let offset = u32::try_from(out.seek(SeekFrom::Current(0))?)
                .map_err(|_| format_err!("Output file too big"))?;
            let mut bytes = vec![0; tex.size as usize];
            read.seek(SeekFrom::Start(tex.offset.into()))?;
            read.read_exact(&mut bytes)?;
            out.write_all(&bytes)?;
            out.seek(SeekFrom::Start(start + i as u64 * 0xc))?;
            out.write_u32::<LE>(offset)?;
            out.write_u32::<LE>(tex.size)?;
            out.write_u16::<LE>(tex.width)?;
            out.write_u16::<LE>(tex.height)?;
            out.seek(SeekFrom::Start(offset as u64 + tex.size as u64))?;
        }
    }
    Ok(())
}

fn write_textures_patched<W: Write + Seek>(
    out: &mut W,
    changes: &TexChanges,
) -> Result<(), Error> {
    let start = out.seek(SeekFrom::Current(0))?;
    let mut zeroes = io::repeat(0).take(changes.textures.len() as u64 * 0xc);
    io::copy(&mut zeroes, out)?;
    for (i, tex) in changes.textures.iter().enumerate() {
        if let Some((ref tex, ref bytes)) = *tex {
            let size = bytes.len() as u32;
            let offset = u32::try_from(out.seek(SeekFrom::Current(0))?)
                .map_err(|_| format_err!("Output file too big"))?;
            out.write_all(bytes)?;
            out.seek(SeekFrom::Start(start + i as u64 * 0xc))?;
            out.write_u32::<LE>(offset)?;
            out.write_u32::<LE>(size)?;
            out.write_u16::<LE>(tex.width)?;
            out.write_u16::<LE>(tex.height)?;
            out.seek(SeekFrom::Start(offset as u64 + size as u64))?;
        }
    }
    Ok(())
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
            unknown: r.read_u32::<LE>()?,
        })
    }).collect()
}

fn write_frames<W: Write>(
    out: &mut W,
    frames: &[Frame],
) -> Result<(), Error> {
    for f in frames {
        out.write_u16::<LE>(f.tex_x)?;
        out.write_u16::<LE>(f.tex_y)?;
        out.write_u16::<LE>(f.x_off)?;
        out.write_u16::<LE>(f.y_off)?;
        out.write_u16::<LE>(f.width)?;
        out.write_u16::<LE>(f.height)?;
        out.write_u32::<LE>(f.unknown)?;
    }
    Ok(())
}
