use std::convert::TryFrom;
use std::fmt;
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::iter;
use std::sync::{Mutex};

use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use ddsfile::{Dds, D3DFormat};
use quick_error::quick_error;

pub struct Anim {
    layer_names: Vec<String>,
    sprites: Vec<SpriteType>,
    scale: u8,
    unknown: u16,
    read: Mutex<Box<dyn ReadSeek>>,
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
    pub width: u16,
    pub height: u16,
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
    pub x_off: i16,
    pub y_off: i16,
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TextureFormat {
    Dxt1,
    Dxt5,
    Monochrome,
}

quick_error! {
    #[derive(Debug)]
    pub enum ImageWriteError {
        Io(e: io::Error) {
            from()
            display("I/O error: {}", e)
        }
        NoFrames {
            display("No frames")
        }
        TextureDataRequired {
            display("No texture data set")
        }
        OutputTooBig {
            display("Output file too big")
        }
        InvalidRef(referenced: u32) {
            display("Referencing invalid image {}", referenced)
        }
    }
}

quick_error! {
    #[derive(Debug)]
    pub enum ErrKind {
        ImageWrite(image: u16, e: ImageWriteError) {
            display("Error writing image {}: {}", image, e)
        }
        TextureReadError(image: u16, e: io::Error) {
            display("Couldn't read textures for image {}: {}", image, e)
        }
        TooManyLayers {
            display("Too many layers")
        }
        TooManySprites {
            display("Too many sprites")
        }
        Eof {
            display("Reached end of input")
        }
        NoSpriteData {
            display("Sprite does not exist")
        }
        NoLayer {
            display("Layer does not exist")
        }
        NoFrame {
            display("Frame does not exist")
        }
        Format(msg: String) {
            display("{}", msg)
        }
        Io(e: io::Error) {
            from()
            display("I/O error: {}", e)
        }
        Dds(ctx: &'static str, e: String) {
            display("DDS error on {}: {}", ctx, e)
        }
        NoDxtFormat {
            display("No DXT format in DDS")
        }
        UnsupportedDdsFormat(format: ddsfile::D3DFormat) {
            display("Unsupported DDS format {:?}", format)
        }
        UnknownTextureFormat(magic: u32) {
            display("Unknown texture format, magic {:08x}", magic)
        }
        InvalidPalette {
            display("Palette must be 256 RGB0 entries")
        }
        ImportNoFrames {
            display("Cannot import 0-frame image")
        }
        InvalidPalettedFrame(frame: u32) {
            display("Frame {} does not have same size as the first frame", frame)
        }
    }
}

#[derive(Debug)]
pub struct Error(pub Box<ErrKind>);

impl From<ErrKind> for Error {
    fn from(ty: ErrKind) -> Error {
        Error(Box::new(ty))
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error(Box::new(ErrKind::Io(e)))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl std::error::Error for Error {}

const ANIM_MAGIC: u32 = 0x4d494e41;

impl Anim {
    pub fn read<R: Read + Seek + Send + 'static>(mut r: R) -> Result<Anim, Error> {
        let magic = r.read_u32::<LE>()?;
        if magic != ANIM_MAGIC {
            return Err(ErrKind::Format(format!("Incorrect magic {:08x}", magic)).into());
        }
        let scale = r.read_u8()?;
        let ty = r.read_u8()?;
        let unknown = r.read_u16::<LE>()?;
        if ty != 0x1 && ty != 0x2 {
            return Err(ErrKind::Format(format!("Unknown type {:x}", ty)).into());
        }
        let layers = r.read_u16::<LE>()?;
        let entries = r.read_u16::<LE>()?;
        if ty != 1 && entries != 1 {
            return Err(ErrKind::Format(format!("Type {:x} with {} entries", ty, entries)).into());
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
        let mut sprite_offsets = vec![0; entries as usize];
        if ty == 1 {
            r.read_u32_into::<LE>(&mut sprite_offsets)?;
        } else {
            sprite_offsets[0] = r.seek(SeekFrom::Current(0))? as u32;
        }
        let mut sprites = Vec::with_capacity(entries as usize);
        for (i, offset) in sprite_offsets.into_iter().enumerate() {
            r.seek(SeekFrom::Start(offset as u64))?;
            let frame_count = r.read_u16::<LE>()?;
            if frame_count == 0 {
                let ref_id = r.read_u32::<LE>()?;
                if ref_id > entries as u32 {
                    return Err(ErrKind::Format(
                        format!("Image {:x} refers past entry amount to {:x}", i, ref_id)
                    ).into());
                }
                sprites.push(SpriteType::Ref(ref_id));
            } else {
                let unk2 = r.read_u16::<LE>()?;
                let width = r.read_u16::<LE>()?;
                let height = r.read_u16::<LE>()?;
                let frame_arr_offset = r.read_u32::<LE>()?;
                let textures = read_textures(&mut r, layers as u32)
                    .map_err(|e| ErrKind::TextureReadError(i as u16, e))?;
                r.seek(SeekFrom::Start(frame_arr_offset as u64))?;
                let frames = read_frames(&mut r, frame_count)?;
                sprites.push(SpriteType::Data(SpriteData {
                    frames,
                    textures,
                    values: SpriteValues {
                        unk2,
                        width,
                        height,
                    },
                }));
            }
        }
        Ok(Anim {
            layer_names,
            sprites,
            scale,
            unknown,
            read: Mutex::new(Box::new(r)),
        })
    }

    pub fn scale(&self) -> u8 {
        self.scale
    }

    /// Writes anim containing only the sprites passed in as parameters.
    ///
    /// The anim-global "unknown" is set to 0.
    pub fn write_new<W: Write + Seek>(
        mut out: W,
        scale: u8,
        layer_names: &[String],
        sprites: &[(ValuesOrRef, &TexChanges)],
    ) -> Result<(), Error> {
        let sprite_count = u16::try_from(sprites.len())
            .map_err(|_| Error::from(ErrKind::TooManySprites))?;
        if layer_names.len() > 10 {
            return Err(ErrKind::TooManyLayers.into());
        }

        out.write_u32::<LE>(ANIM_MAGIC)?;
        out.write_u8(scale)?;
        let ty = match sprite_count == 1 {
            true => 2,
            false => 1,
        };
        out.write_u8(ty)?;
        out.write_u16::<LE>(0)?;
        out.write_u16::<LE>(layer_names.len() as u16)?;
        out.write_u16::<LE>(sprite_count)?;
        for name in layer_names.iter().map(|x| &**x).chain(iter::repeat("")).take(10) {
            let mut buf = [0u8; 0x20];
            (&mut buf[..]).write_all(name.as_bytes())?;
            out.write_all(&buf)?;
        }
        let sprite_offset_pos = out.seek(SeekFrom::Current(0))?;
        let mut sprite_offsets = vec![!0u32; sprite_count as usize];
        if ty == 1 {
            for &x in &sprite_offsets {
                out.write_u32::<LE>(x)?;
            }
        }
        for (i, &(ref values, tex_changes)) in sprites.iter().enumerate() {
            let i = i as u16;
            sprite_offsets[i as usize] = u32::try_from(out.seek(SeekFrom::Current(0))?)
                .map_err(|_| ErrKind::ImageWrite(i, ImageWriteError::OutputTooBig))?;
            Anim::write_new_image(
                &mut out,
                sprite_count,
                values,
                tex_changes,
                layer_names.len(),
            ).map_err(|e| ErrKind::ImageWrite(i, e))?;
        }
        if ty == 1 {
            out.seek(SeekFrom::Start(sprite_offset_pos))?;
            for &x in &sprite_offsets {
                out.write_u32::<LE>(x)?;
            }
        }

        Ok(())
    }

    /// Internal helper for write_new
    fn write_new_image<W: Write + Seek>(
        out: &mut W,
        image_count: u16,
        values: &ValuesOrRef,
        textures: &TexChanges,
        texture_count: usize,
    ) -> Result<(), ImageWriteError> {
        match *values {
            ValuesOrRef::Ref(img) => {
                if img >= image_count as u32 {
                    return Err(ImageWriteError::InvalidRef(img));
                }
                out.write_u16::<LE>(0)?;
                out.write_u32::<LE>(img)?;
                for _ in 0..3 {
                    out.write_u16::<LE>(0)?;
                }
            }
            ValuesOrRef::Values(ref values) => {
                let frames = &textures.frames;
                if frames.len() == 0 {
                    return Err(ImageWriteError::NoFrames);
                }
                out.write_u16::<LE>(frames.len() as u16)?;
                out.write_u16::<LE>(values.unk2)?;
                out.write_u16::<LE>(values.width)?;
                out.write_u16::<LE>(values.height)?;
                let frame_arr_offset_pos = out.seek(SeekFrom::Current(0))?;
                out.write_u32::<LE>(!0)?;
                write_textures_patched(out, textures, texture_count)?;
                let frame_arr_offset = u32::try_from(out.seek(SeekFrom::Current(0))?)
                    .map_err(|_| ImageWriteError::OutputTooBig)?;
                write_frames(out, frames)?;
                let cont_pos = out.seek(SeekFrom::Current(0))?;
                out.seek(SeekFrom::Start(frame_arr_offset_pos))?;
                out.write_u32::<LE>(frame_arr_offset)?;
                out.seek(SeekFrom::Start(cont_pos))?;
            }
        }
        Ok(())
    }

    /// Writes a patched anim to `out`. `textures` contains the changed textures and their
    /// sprite and layer ids, other textures are read from the original read object.
    ///
    /// Since the read object is kept open (and used) here, `out` cannot be the same file as
    /// what was used to read this anim.
    pub fn write_patched<W: Write + Seek>(
        &self,
        mut out: W,
        scale: u8,
        sprite_count: u16,
        layer_names: &[String],
        data_changes: &[(usize, ValuesOrRef)],
        textures: &[(usize, &TexChanges)],
    ) -> Result<(), Error> {
        out.write_u32::<LE>(ANIM_MAGIC)?;
        out.write_u8(scale)?;
        let ty = match sprite_count == 1 {
            true => 2,
            false => 1,
        };
        out.write_u8(ty)?;
        out.write_u16::<LE>(self.unknown)?;
        out.write_u16::<LE>(layer_names.len() as u16)?;
        out.write_u16::<LE>(sprite_count)?;
        if layer_names.len() > 10 {
            return Err(ErrKind::TooManyLayers.into());
        }
        for name in layer_names.iter().map(|x| &**x).chain(iter::repeat("")).take(10) {
            let mut buf = [0u8; 0x20];
            (&mut buf[..]).write_all(name.as_bytes())?;
            out.write_all(&buf)?;
        }
        let sprite_offset_pos = out.seek(SeekFrom::Current(0))?;
        let mut sprite_offsets = vec![!0u32; sprite_count as usize];
        if ty == 1 {
            for &x in &sprite_offsets {
                out.write_u32::<LE>(x)?;
            }
        }
        let mut read = self.read.lock().unwrap();
        for i in 0..sprite_count {
            sprite_offsets[i as usize] = u32::try_from(out.seek(SeekFrom::Current(0))?)
                .map_err(|_| ErrKind::ImageWrite(i, ImageWriteError::OutputTooBig))?;

            let store;
            let values = match data_changes.iter().find(|x| x.0 == i as usize) {
                Some(&(_, ref vals)) => vals,
                None => {
                    store = values_or_ref_sd(&self.sprites, i as usize);
                    &store
                }
            };
            self.write_patched_image(
                &mut out,
                &mut read,
                i,
                sprite_count,
                values,
                textures,
                layer_names.len(),
            ).map_err(|e| ErrKind::ImageWrite(i, e))?;
        }
        if ty == 1 {
            out.seek(SeekFrom::Start(sprite_offset_pos))?;
            for &x in &sprite_offsets {
                out.write_u32::<LE>(x)?;
            }
        }
        Ok(())
    }

    /// Internal helper for write_patched
    fn write_patched_image<W: Write + Seek>(
        &self,
        out: &mut W,
        read: &mut Box<dyn ReadSeek>,
        image_id: u16,
        image_count: u16,
        values: &ValuesOrRef,
        textures: &[(usize, &TexChanges)],
        texture_count: usize,
    ) -> Result<(), ImageWriteError> {
        match *values {
            ValuesOrRef::Ref(img) => {
                if img >= image_count as u32 {
                    return Err(ImageWriteError::InvalidRef(img));
                }
                out.write_u16::<LE>(0)?;
                out.write_u32::<LE>(img)?;
                for _ in 0..3 {
                    out.write_u16::<LE>(0)?;
                }
            }
            ValuesOrRef::Values(ref values) => {
                let texture_changes = textures.iter()
                    .find(|x| x.0 == image_id as usize)
                    .map(|x| &x.1);
                let frames = match texture_changes {
                    Some(changes) => &changes.frames,
                    None => match self.sprites[image_id as usize] {
                        SpriteType::Ref(_) => {
                            return Err(ImageWriteError::TextureDataRequired);
                        }
                        SpriteType::Data(ref data) => &data.frames,
                    },
                };
                if frames.len() == 0 {
                    return Err(ImageWriteError::NoFrames);
                }
                out.write_u16::<LE>(frames.len() as u16)?;
                out.write_u16::<LE>(values.unk2)?;
                out.write_u16::<LE>(values.width)?;
                out.write_u16::<LE>(values.height)?;
                let frame_arr_offset_pos = out.seek(SeekFrom::Current(0))?;
                out.write_u32::<LE>(!0)?;
                if let Some(changes) = texture_changes {
                    write_textures_patched(out, changes, texture_count)?;
                } else {
                    let textures = match self.sprites[image_id as usize] {
                        SpriteType::Ref(_) => {
                            return Err(ImageWriteError::TextureDataRequired);
                        }
                        SpriteType::Data(ref data) => &data.textures,
                    };
                    write_textures_unchanged(out, &mut *read, textures)?;
                }
                let frame_arr_offset = u32::try_from(out.seek(SeekFrom::Current(0))?)
                    .map_err(|_| ImageWriteError::OutputTooBig)?;
                write_frames(out, frames)?;
                let cont_pos = out.seek(SeekFrom::Current(0))?;
                out.seek(SeekFrom::Start(frame_arr_offset_pos))?;
                out.write_u32::<LE>(frame_arr_offset)?;
                out.seek(SeekFrom::Start(cont_pos))?;
            }
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
            .ok_or_else(|| ErrKind::NoSpriteData)?
            .textures.get(layer).and_then(|x| x.as_ref())
            .ok_or_else(|| ErrKind::NoLayer)?
            .clone();
        let mut read = self.read.lock().unwrap();
        read.seek(SeekFrom::Start(texture.offset as u64))?;
        read_texture(&mut *read, &texture)
    }

    pub fn texture_formats(&self, sprite: usize) -> Vec<Result<Option<TextureFormat>, Error>> {
        let mut read = self.read.lock().unwrap();
        let mut read = &mut *read;
        let sprite = match sprite_data_sd(&self.sprites, sprite) {
            Some(o) => o,
            None => return Vec::new(),
        };
        sprite.textures.iter().map(|x| {
            match *x {
                Some(ref texture) => {
                    read.seek(SeekFrom::Start(texture.offset as u64))?;
                    texture_format(&mut read, texture.size).map(Some)
                }
                None => Ok(None),
            }
        }).collect()
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

const DDS_MAGIC: u32 = 0x20534444;
const BMP_MAGIC: u32 = 0x20504d42;

pub fn texture_format<R: Read + Seek>(mut read: R, limit: u32) -> Result<TextureFormat, Error> {
    let magic = read.read_u32::<LE>()?;
    if magic == DDS_MAGIC {
        // TODO dds lib reads everything
        read.seek(SeekFrom::Current(-4))?;
        let mut read = read.take(limit.into());
        let dds = Dds::read(&mut read)
            .map_err(|e| ErrKind::Dds("reading DDS", e.to_string()))?;
        let format = dds.get_d3d_format().ok_or_else(|| ErrKind::NoDxtFormat)?;
        match format {
            D3DFormat::DXT1 => Ok(TextureFormat::Dxt1),
            D3DFormat::DXT5 => Ok(TextureFormat::Dxt5),
            x => Err(ErrKind::UnsupportedDdsFormat(x).into()),
        }
    } else if magic == BMP_MAGIC {
        Ok(TextureFormat::Monochrome)
    } else {
        Err(ErrKind::UnknownTextureFormat(magic).into())
    }
}

pub fn read_texture<R: Read + Seek>(
    mut read: R,
    texture: &Texture,
) -> Result<RgbaTexture, Error> {
    let magic = read.read_u32::<LE>()?;
    if magic == DDS_MAGIC {
        read.seek(SeekFrom::Current(-4))?;
        let mut read = read.take(texture.size.into());
        let dds = Dds::read(&mut read)
            .map_err(|e| ErrKind::Dds("reading DDS", e.to_string()))?;
        let format = dds.get_d3d_format().ok_or_else(|| ErrKind::NoDxtFormat)?;
        let data = dds.get_data(0)
            .map_err(|e| ErrKind::Dds("getting data", e.to_string()))?;

        let data = decode_dxt(data, texture.width as u32, texture.height as u32, format)?;
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
            data.push(255);
            data.push(255);
            data.push(255);
            data.push(p);
        }
        Ok(RgbaTexture {
            data,
            width: texture.width as u32,
            height: texture.height as u32,
        })
    } else {
        return Err(ErrKind::UnknownTextureFormat(magic).into());
    }
}

fn decode_dxt(
    data: &[u8],
    width: u32,
    height: u32,
    format: D3DFormat,
) -> Result<Vec<u8>, Error> {
    let aligned_width = ((width as u32 - 1) | 3) + 1;
    let aligned_height = ((height as u32 - 1) | 3) + 1;
    let mut data = match format {
        D3DFormat::DXT1 => decode_dxt1(&data, aligned_width, aligned_height)?,
        D3DFormat::DXT5 => decode_dxt5(&data, aligned_width, aligned_height)?,
        _ => return Err(ErrKind::UnsupportedDdsFormat(format).into()),
    };
    // The decoding functions only work with multiplies of 4,
    // but if some textures aren't such, resize them here.
    if width & 0x3 != 0 {
        let remove_bytes = 4 - (width & 0x3) as usize;
        for y in 1..(height as usize) {
            let start = y * width as usize * 4;
            let len = ((width as usize) + remove_bytes * y) * 4;
            data[start..start + len].rotate_left(remove_bytes * y * 4);
        }
    }
    data.resize(width as usize * height as usize * 4, 0);
    Ok(data)
}

/// Returns the bytes without alpha multiplied
fn decode_dxt5(data: &[u8], width: u32, height: u32) -> Result<Vec<u8>, Error> {
    assert!(width & 3 == 0);
    assert!(height & 3 == 0);
    let mut read = data;
    let size = (width * height) as usize;
    let mut out = vec![0u8; size * 4];
    let mut pos = 0u32;
    for _y_tile in 0..(height / 4) {
        for x_tile in 0..(width / 4) {
            let (mut block, rest) = match read.len() {
                x if x < 16 => return Err(ErrKind::Eof.into()),
                _ => read.split_at(16),
            };
            read = rest;
            let alpha = block.read_u64::<LE>()?;
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
                    let color = table[(colors & 3) as usize];
                    line[x * 4 + 0] = (color.0 * 255.0) as u8;
                    line[x * 4 + 1] = (color.1 * 255.0) as u8;
                    line[x * 4 + 2] = (color.2 * 255.0) as u8;
                    line[x * 4 + 3] = alpha_table[(alpha & 7) as usize] as u8;
                    colors = colors >> 2;
                    alpha = alpha >> 3;
                }
                pos = pos.wrapping_add(width);
            }
        }
        pos = pos.wrapping_add((width as u32).wrapping_mul(4));
    }
    Ok(out)
}

fn decode_dxt1(data: &[u8], width: u32, height: u32) -> Result<Vec<u8>, Error> {
    assert!(width & 3 == 0);
    assert!(height & 3 == 0);
    let mut read = data;
    let size = (width * height) as usize;
    let mut out = vec![0u8; size * 4];
    let mut pos = 0u32;
    for _y_tile in 0..(height / 4) {
        for x_tile in 0..(width / 4) {
            let (mut block, rest) = match read.len() {
                x if x < 8 => return Err(ErrKind::Eof.into()),
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

/// Either RGBA or paletted texture.
pub struct RawTexture {
    pub data: Vec<u8>,
    pub width: u32,
    pub height: u32,
    pub is_paletted: bool,
}

impl From<RgbaTexture> for RawTexture {
    fn from(val: RgbaTexture) -> RawTexture {
        RawTexture {
            data: val.data,
            width: val.width,
            height: val.height,
            is_paletted: false,
        }
    }
}

fn read_textures<R: Read>(mut r: R, count: u32) -> Result<Vec<Option<Texture>>, io::Error> {
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
) -> Result<(), ImageWriteError> {
    let start = out.seek(SeekFrom::Current(0))?;
    let mut zeroes = io::repeat(0).take(textures.len() as u64 * 0xc);
    io::copy(&mut zeroes, out)?;
    for (i, tex) in textures.iter().enumerate() {
        if let Some(ref tex) = *tex {
            let offset = u32::try_from(out.seek(SeekFrom::Current(0))?)
                .map_err(|_| ImageWriteError::OutputTooBig)?;
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
    texture_count: usize,
) -> Result<(), ImageWriteError> {
    let start = out.seek(SeekFrom::Current(0))?;
    let mut zeroes = io::repeat(0).take(changes.textures.len().max(texture_count) as u64 * 0xc);
    io::copy(&mut zeroes, out)?;
    for (i, tex) in changes.textures.iter().enumerate() {
        if let Some((ref tex, ref bytes)) = *tex {
            let size = bytes.len() as u32;
            let offset = u32::try_from(out.seek(SeekFrom::Current(0))?)
                .map_err(|_| ImageWriteError::OutputTooBig)?;
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
            x_off: r.read_i16::<LE>()?,
            y_off: r.read_i16::<LE>()?,
            width: r.read_u16::<LE>()?,
            height: r.read_u16::<LE>()?,
            unknown: r.read_u32::<LE>()?,
        })
    }).collect()
}

fn write_frames<W: Write>(
    out: &mut W,
    frames: &[Frame],
) -> Result<(), ImageWriteError> {
    for f in frames {
        out.write_u16::<LE>(f.tex_x)?;
        out.write_u16::<LE>(f.tex_y)?;
        out.write_i16::<LE>(f.x_off)?;
        out.write_i16::<LE>(f.y_off)?;
        out.write_u16::<LE>(f.width)?;
        out.write_u16::<LE>(f.height)?;
        out.write_u32::<LE>(f.unknown)?;
    }
    Ok(())
}
