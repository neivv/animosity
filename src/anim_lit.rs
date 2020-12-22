use std::convert::TryFrom;
use std::collections::hash_map::Entry;
use std::io::{self, Read, Seek, Write};
use std::sync::Arc;

use anyhow::Error;
use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use fxhash::FxHashMap;

#[derive(Clone, Eq, PartialEq)]
pub struct Lit {
    sprites: Vec<Sprite>,
}

#[derive(Clone, Eq, PartialEq)]
pub struct Sprite {
    // Frames are 0 if the file had all 0 data frames, so that it can be shown
    // as "disabled". Keeping track of frame count even for sprites with disabled
    // lighting so that the 0 data frames can be written back.
    frames: Option<Arc<Vec<Frame>>>,
    frame_count: u32,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub struct Frame {
    /// Relative to anim texture x/y offset, *NOT* 0,0
    pub x: i32,
    pub y: i32,
    pub color: u32,
    pub intensity: u32,
    pub radius: u32,
}

impl Lit {
    pub fn read<R: Read + Seek>(read: &mut R) -> Result<Lit, Error> {
        let image_count = read.read_u32::<LE>()?;
        let unk1 = read.read_u32::<LE>()?;
        let unk2 = read.read_u32::<LE>()?;
        if unk1 != 0 || unk2 != 0 {
            return Err(anyhow!("Unk1/2 weren't 0: {:x} {:x}", unk1, unk2));
        }
        let mut offsets = vec![0u32; image_count as usize];
        read.read_u32_into::<LE>(&mut offsets)?;
        let mut sprites = Vec::with_capacity(image_count as usize);
        let mut buffer: Vec<u32> = Vec::new();
        for off in offsets {
            read.seek(io::SeekFrom::Start(off as u64))?;
            let frame_count = read.read_u32::<LE>()?;
            buffer.clear();
            buffer.resize(frame_count as usize * 5, 0u32);
            read.read_u32_into::<LE>(&mut buffer)?;
            let frames = if buffer.iter().all(|&x| x == 0) {
                None
            } else {
                let frames = buffer.chunks_exact(5)
                    .map(|c| {
                        Frame {
                            x: c[0] as i32,
                            y: c[1] as i32,
                            color: c[2],
                            intensity: c[3],
                            radius: c[4],
                        }
                    })
                    .collect();
                Some(Arc::new(frames))
            };
            sprites.push(Sprite {
                frames,
                frame_count,
            });
        }
        Ok(Lit {
            sprites,
        })
    }

    pub fn write<W: Write + Seek>(&self, out: &mut W) -> Result<(), Error> {
        // Aliasing multiple equivalent frame arrays is ok
        let mut frame_arr_offsets: FxHashMap<(u32, Option<Arc<Vec<Frame>>>), u32> =
            FxHashMap::with_capacity_and_hasher(self.sprites.len(), Default::default());
        out.write_u32::<LE>(u32::try_from(self.sprites.len())?)?;
        out.write_u32::<LE>(0)?;
        out.write_u32::<LE>(0)?;
        io::copy(&mut io::repeat(0).take(4 * self.sprites.len() as u64), out)?;
        let mut offsets = Vec::with_capacity(self.sprites.len());
        for sprite in &self.sprites {
            let frames = sprite.frames.clone()
                .filter(|x| x.iter().any(|f| *f != default_frame()));
            let key = (sprite.frame_count, frames);
            let entry = frame_arr_offsets.entry(key);
            match entry {
                Entry::Occupied(e) => {
                    offsets.push(*e.get());
                }
                Entry::Vacant(e) => {
                    let pos = out.seek(io::SeekFrom::Current(0))?;
                    let offset = u32::try_from(pos)?;
                    let frames = &e.key().1;
                    out.write_u32::<LE>(sprite.frame_count)?;
                    if let Some(ref frames) = frames {
                        for f in frames.iter() {
                            out.write_i32::<LE>(f.x)?;
                            out.write_i32::<LE>(f.y)?;
                            out.write_u32::<LE>(f.color)?;
                            out.write_u32::<LE>(f.intensity)?;
                            out.write_u32::<LE>(f.radius)?;
                        }
                    } else {
                        io::copy(
                            &mut io::repeat(0).take(4 * 5 * sprite.frame_count as u64),
                            out,
                        )?;
                    }
                    e.insert(offset);
                    offsets.push(offset);
                }
            }
        }
        out.seek(io::SeekFrom::Start(12))?;
        for offset in offsets {
            out.write_u32::<LE>(offset)?;
        }
        Ok(())
    }

    pub fn sprite(&self, index: usize) -> Option<&Sprite> {
        self.sprites.get(index)
    }

    pub fn sprite_mut(&mut self, index: usize) -> Option<&mut Sprite> {
        self.sprites.get_mut(index)
    }

    pub fn remove_sprite(&mut self, index: usize) {
        self.sprites[index].frames = None;
    }

    pub fn sprites(&self) -> &[Sprite] {
        &self.sprites
    }

    pub fn resize(&mut self, new_size: u16) {
        self.sprites.resize_with(new_size as usize, || Sprite {
            frames: None,
            frame_count: 1,
        });
    }
}

impl Sprite {
    pub fn frame_count(&self) -> u32 {
        self.frame_count
    }

    pub fn set_frame_count(&mut self, frame_count: u32) {
        if self.frame_count == frame_count {
            return;
        }
        self.frame_count = frame_count;
        if let Some(ref mut frames) = self.frames {
            Arc::make_mut(frames).resize_with(frame_count as usize, default_frame);
        }
    }

    pub fn frames_mut(&mut self) -> &mut [Frame] {
        let frame_count = self.frame_count;
        let frames = self.frames.get_or_insert_with(|| {
            let frames = (0..frame_count).map(|_| default_frame()).collect();
            Arc::new(frames)
        });
        let vec: &mut Vec<_> = Arc::make_mut(frames);
        &mut *vec
    }

    pub fn has_data(&self) -> bool {
        self.frames.is_some()
    }
}

fn default_frame() -> Frame{
    Frame {
        x: 0,
        y: 0,
        color: 0,
        intensity: 0,
        radius: 0,
    }
}

#[test]
fn no_changes() {
    static DATA: &[u8] = include_bytes!("../test_inputs/main.lit");
    let lit = Lit::read(&mut io::Cursor::new(DATA)).unwrap();
    let mut buf = Vec::new();
    lit.write(&mut io::Cursor::new(&mut buf)).unwrap();
    let lit2 = Lit::read(&mut io::Cursor::new(&buf)).unwrap();
    let mut buf2 = Vec::new();
    lit.write(&mut io::Cursor::new(&mut buf2)).unwrap();
    if lit != lit2 {
        std::fs::write("main_err.lit", &buf).unwrap();
        panic!("Writing lit changed data, wrong file written to main_err.lit");
    }
    if buf != buf2 {
        std::fs::write("main_err.lit", &buf).unwrap();
        std::fs::write("main_err2.lit", &buf2).unwrap();
        panic!("Writing lit second time changed bytes, wrong files written to main_err[2].lit");
    }
    assert!(buf.len() <= DATA.len());
}
