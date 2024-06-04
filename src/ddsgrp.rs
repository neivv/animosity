use std::io::{Read, Seek, SeekFrom, Write};
use std::sync::Mutex;

use byteorder::{ReadBytesExt, LE, WriteBytesExt};

use crate::anim::{self, ErrKind, Error};

#[allow(dead_code)]
pub struct DdsGrp {
    pub frame_count: u16,
    pub scale: u8,
    pub version: u8,
    pub frames: Vec<Frame>,
    // Only used if flag 0x10 is set
    // 256 R/G/B/0 entries
    palette: Vec<u8>,
    read: Mutex<Box<dyn ReadSeek>>,
}

trait ReadSeek: Read + Seek + Send { }
impl<T: Read + Seek + Send> ReadSeek for T { }

#[derive(Copy, Clone, Debug)]
pub struct Frame {
    pub unknown: u32,
    pub width: u16,
    pub height: u16,
    pub size: u32,
    pub offset: u32,
}

impl Frame {
    pub fn to_anim_texture_coords(&self) -> anim::Texture {
        anim::Texture {
            offset: self.offset,
            size: self.size,
            width: self.width,
            height: self.height,
        }
    }
}

impl DdsGrp {
    pub fn read<R: Read + Seek + Send + 'static>(mut r: R) -> Result<DdsGrp, Error> {
        let _size = r.read_u32::<LE>()?;
        let frame_count = r.read_u16::<LE>()?;
        let flags = r.read_u8()?;
        let version = r.read_u8()?;
        let scale = flags & 0xf;
        let has_palette = flags & 0x10 != 0;
        if !matches!(scale, 1 | 2 | 4) {
            return Err(ErrKind::Format(format!("Invalid scale {}", scale)).into());
        }
        if flags & 0xe0 != 0 {
            return Err(ErrKind::Format(format!("Unknown flags 0x{:x}", flags & 0xe0)).into());
        }
        if version != 16 {
            return Err(ErrKind::Format(format!("Unknown version {}", version)).into());
        }
        let mut frames = Vec::with_capacity(frame_count as usize);
        let mut palette = Vec::new();
        if has_palette {
            let width = r.read_u16::<LE>()?;
            let height = r.read_u16::<LE>()?;
            palette.resize_with(0x400, || 0);
            r.read_exact(&mut palette[..])?;
            let mut offset = 0x40c;
            let frame_size = width as u32 * height as u32;
            for _ in 0..frame_count {
                frames.push(Frame {
                    unknown: 0,
                    width,
                    height,
                    size: frame_size,
                    offset,
                });
                offset += frame_size;
            }
        } else {
            for _ in 0..frame_count {
                let unknown = r.read_u32::<LE>()?;
                let width = r.read_u16::<LE>()?;
                let height = r.read_u16::<LE>()?;
                let size = r.read_u32::<LE>()?;
                let offset = r.seek(SeekFrom::Current(0))? as u32;
                r.seek(SeekFrom::Current(size as i64))?;
                frames.push(Frame {
                    unknown,
                    width,
                    height,
                    size,
                    offset,
                });
            }
        }
        Ok(DdsGrp {
            frame_count,
            scale,
            version,
            frames,
            read: Mutex::new(Box::new(r)),
            palette,
        })
    }

    pub fn has_palette(&self) -> bool {
        !self.palette.is_empty()
    }

    pub fn palette(&self) -> Option<&[u8]> {
        if self.has_palette() {
            Some(&self.palette[..])
        } else {
            None
        }
    }

    pub fn frame(&self, frame: usize) -> Result<anim::RawTexture, Error> {
        let frame = self.frames.get(frame).ok_or_else(|| ErrKind::NoFrame)?;
        let mut read = self.read.lock().unwrap();
        read.seek(SeekFrom::Start(frame.offset as u64))?;
        if self.has_palette() {
            let frame_size = frame.width as usize * frame.height as usize;
            let mut buffer = vec![0u8; frame_size];
            read.read_exact(&mut buffer[..])?;
            Ok(anim::RawTexture {
                data: buffer,
                width: frame.width.into(),
                height: frame.height.into(),
                is_paletted: true,
            })
        } else {
            let texture = anim::read_texture(&mut *read, &frame.to_anim_texture_coords())?;
            Ok(texture.into())
        }
    }

    pub fn texture_size(&self, frame: usize) -> Option<anim::Texture> {
        self.frames.get(frame).map(|x| x.to_anim_texture_coords())
    }

    pub fn texture_formats(&self) -> Vec<Result<Option<anim::TextureFormat>, Error>> {
        if self.has_palette() {
            vec![]
        } else {
            let mut read = self.read.lock().unwrap();
            self.frames.iter().map(|f| {
                read.seek(SeekFrom::Start(f.offset as u64))?;
                let format = anim::texture_format(&mut *read, f.size)?;
                Ok(Some(format))
            }).collect()
        }
    }

    pub fn write<W: Write + Seek>(
        mut out: W,
        scale: u8,
        frames: &[(Frame, Vec<u8>)],
        palette: Option<&[u8]>,
    ) -> Result<(), Error> {
        if let Some(palette) = palette {
            if palette.len() != 0x400 {
                return Err(Error(Box::new(anim::ErrKind::InvalidPalette)));
            }
            let first_frame = match frames.get(0) {
                Some(s) => s,
                None => return Err(Error(Box::new(anim::ErrKind::ImportNoFrames))),
            };
            let width = first_frame.0.width;
            let height = first_frame.0.height;
            let size = (width as usize) * (height as usize);
            for (i, &(ref f, ref data)) in frames.iter().enumerate() {
                if f.width != width || f.height != height || data.len() != size {
                    return Err(Error(Box::new(anim::ErrKind::InvalidPalettedFrame(i as u32))));
                }
            }
        };
        let flags = if palette.is_some() {
            scale | 0x10
        } else {
            scale
        };
        out.write_u32::<LE>(0)?;
        out.write_u16::<LE>(frames.len() as u16)?;
        out.write_u8(flags)?;
        out.write_u8(0x10)?;
        if let Some(palette) = palette {
            let first_frame = match frames.get(0) {
                Some(s) => s,
                None => return Err(Error(Box::new(anim::ErrKind::ImportNoFrames))),
            };
            out.write_u16::<LE>(first_frame.0.width)?;
            out.write_u16::<LE>(first_frame.0.height)?;
            out.write_all(palette)?;
        }
        for &(ref f, ref data) in frames {
            if palette.is_none() {
                out.write_u32::<LE>(f.unknown)?;
                out.write_u16::<LE>(f.width)?;
                out.write_u16::<LE>(f.height)?;
                out.write_u32::<LE>(data.len() as u32)?;
            }
            out.write_all(&data)?;
        }
        let file_size = out.seek(SeekFrom::Current(0))? as u32;
        out.seek(SeekFrom::Start(0))?;
        out.write_u32::<LE>(file_size)?;
        Ok(())
    }
}
