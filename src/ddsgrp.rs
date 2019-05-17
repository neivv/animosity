use std::io::{Read, Seek, SeekFrom, Write};
use std::sync::Mutex;

use byteorder::{ReadBytesExt, LE, WriteBytesExt};

use crate::anim::{self, ErrKind, Error};

pub struct DdsGrp {
    pub frame_count: u16,
    pub scale: u8,
    pub version: u8,
    pub frames: Vec<Frame>,
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
        let scale = r.read_u8()?;
        let version = r.read_u8()?;
        if version != 16 {
            return Err(ErrKind::Format(format!("Unknown version {}", version)).into());
        }
        let mut frames = Vec::with_capacity(frame_count as usize);
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
        Ok(DdsGrp {
            frame_count,
            scale,
            version,
            frames,
            read: Mutex::new(Box::new(r)),
        })
    }

    pub fn frame(&self, frame: usize) -> Result<anim::RgbaTexture, Error> {
        let frame = self.frames.get(frame).ok_or_else(|| ErrKind::NoFrame)?;
        let mut read = self.read.lock().unwrap();
        read.seek(SeekFrom::Start(frame.offset as u64))?;
        Ok(anim::read_texture(&mut *read, &frame.to_anim_texture_coords())?)
    }

    pub fn texture_size(&self, frame: usize) -> Option<anim::Texture> {
        self.frames.get(frame).map(|x| x.to_anim_texture_coords())
    }

    pub fn texture_formats(&self) -> Vec<Result<Option<anim::TextureFormat>, Error>> {
        let mut read = self.read.lock().unwrap();
        self.frames.iter().map(|f| {
            read.seek(SeekFrom::Start(f.offset as u64))?;
            let format = anim::texture_format(&mut *read, f.size)?;
            Ok(Some(format))
        }).collect()
    }

    pub fn write<W: Write + Seek>(
        mut out: W,
        scale: u8,
        frames: &[(Frame, Vec<u8>)],
    ) -> Result<(), Error> {
        out.write_u32::<LE>(0)?;
        out.write_u16::<LE>(frames.len() as u16)?;
        out.write_u8(scale)?;
        out.write_u8(0x10)?;
        for &(ref f, ref data) in frames {
            out.write_u32::<LE>(f.unknown)?;
            out.write_u16::<LE>(f.width)?;
            out.write_u16::<LE>(f.height)?;
            out.write_u32::<LE>(data.len() as u32)?;
            out.write_all(&data)?;
        }
        let file_size = out.seek(SeekFrom::Current(0))? as u32;
        out.seek(SeekFrom::Start(0))?;
        out.write_u32::<LE>(file_size)?;
        Ok(())
    }
}
