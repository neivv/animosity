use std::collections::HashMap;
use std::convert::TryFrom;
use std::io::{Write, Seek, SeekFrom};
use std::rc::Rc;

use anyhow::Context;
use byteorder::{LE, WriteBytesExt};
use ddsfile::{Dds, D3DFormat};

use crate::{anim, Error};

#[derive(Hash, Eq, PartialEq, Clone)]
struct Frame {
    width: u32,
    height: u32,
    data: Vec<u8>,
}

#[derive(Debug, Copy, Clone)]
pub struct FrameCoords {
    pub x_offset: i32,
    pub y_offset: i32,
    pub width: u32,
    pub height: u32,
}

/// Frame graphics for each layer
#[derive(Clone, Eq, PartialEq, Hash)]
struct LayerFrames {
    /// The coords are an offset relative to topleft of the area
    /// taken by all graphics of this frame.
    /// (E.g at least one layer must have X = 0 and one must have Y = 0,
    /// not necessarily the same layer though)
    frames: Vec<(Rc<Frame>, (i32, i32))>,
    width: u32,
    height: u32,
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct FrameOffset {
    x: i32,
    y: i32,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct TexCoords {
    x: u32,
    y: u32,
}

pub struct Layout {
    // One hashmap for each layer, equivalent frame data
    frames: Vec<HashMap<Rc<Frame>, Vec<(usize, (i32, i32))>>>,
    // layer id -> frame id
    frame_lookup: Vec<Vec<Option<(Rc<Frame>, i32, i32)>>>,
}

pub struct LayoutResult {
    /// Same graphics can be used for multiple frames (with potentially different offsets)
    /// Contains unique graphics (in no specific order),
    /// for each graphic there's
    ///  - Vec<(frame_id, offset)>,
    ///  - graphics for each layer
    ///  - Texture coordinates (Anim format requires it to be same for each layer)
    frames: Vec<(Vec<(usize, FrameOffset)>, LayerFrames, TexCoords)>,
    texture_width: u32,
    texture_height: u32,
    frame_count: usize,
}

impl LayoutResult {
    pub fn encode(
        &self,
        first_layer: usize,
        layers: &[Option<anim::TextureFormat>],
        scale: u32,
    ) -> anim::TexChanges {
        let tex_width = self.texture_width / scale;
        let tex_height = self.texture_height / scale;
        let tex_width = ((tex_width - 1) | 3) + 1;
        let tex_height = ((tex_height - 1) | 3) + 1;
        let textures = layers.iter().enumerate().map(|(layer, x)| {
            x.map(|format| {
                let layer = first_layer + layer;
                let bytes = match format {
                    anim::TextureFormat::Dxt1 => {
                        encode_dxt1(&self.frames, layer, tex_width, tex_height, scale)
                    }
                    anim::TextureFormat::Dxt5 => {
                        encode_dxt5(&self.frames, layer, tex_width, tex_height, scale)
                    }
                    anim::TextureFormat::Monochrome => {
                        encode_monochrome(&self.frames, layer, tex_width, tex_height, scale)
                    }
                };
                (anim::Texture {
                    offset: !0,
                    size: bytes.len() as u32,
                    width: tex_width as u16,
                    height: tex_height as u16,
                }, bytes)
            })
        }).collect::<Vec<_>>();
        let mut anim_frames = (0..self.frame_count).map(|_| anim::Frame {
            tex_x: 0,
            tex_y: 0,
            x_off: 0,
            y_off: 0,
            width: 0,
            height: 0,
            unknown: 0,
        }).collect::<Vec<_>>();
        for (ref f, ref layer_f, ref tex_coords) in &self.frames {
            for (frame_id, frame_off) in f {
                anim_frames[*frame_id] = anim::Frame {
                    tex_x: tex_coords.x as u16,
                    tex_y: tex_coords.y as u16,
                    x_off: frame_off.x as i16,
                    y_off: frame_off.y as i16,
                    width: layer_f.width as u16,
                    height: layer_f.height as u16,
                    unknown: 0,
                };
            }
        }
        anim::TexChanges {
            frames: anim_frames,
            textures,
        }
    }
}

impl Layout {
    pub fn new() -> Layout {
        Layout {
            frames: Vec::new(),
            frame_lookup: Vec::new(),
        }
    }

    /// Data must be RGBA encoded
    pub fn add_frame(
        &mut self,
        layer: usize,
        frame: usize,
        data: Vec<u8>,
        coords: FrameCoords,
    ) {
        if data.is_empty() {
            return;
        }
        while self.frames.len() <= layer {
            self.frames.push(HashMap::new());
            self.frame_lookup.push(Vec::new());
        }

        let entry = self.frames[layer].entry(Rc::new(Frame {
            data,
            width: coords.width,
            height: coords.height,
        }));
        let frame_rc = entry.key().clone();
        entry.or_insert_with(Default::default).push((frame, (coords.x_offset, coords.y_offset)));
        let lookup = &mut self.frame_lookup[layer];
        while lookup.len() < frame + 1 {
            lookup.push(None);
        }
        lookup[frame] = Some((frame_rc, coords.x_offset, coords.y_offset));
    }

    pub fn layout(mut self) -> LayoutResult {
        let mut final_map: HashMap<LayerFrames, Vec<(usize, FrameOffset)>> = HashMap::new();
        let frame_count = self.frame_lookup.iter().map(|x| x.len()).max().unwrap_or(0);
        let dummy_frame = Rc::new(Frame {
            width: 0,
            height: 0,
            data: vec![],
        });
        for f in (0..frame_count).rev() {
            let self_frames = &self.frames;
            // `(Frame, x_off, y_off)` for each layer
            let mut frames: Vec<(Rc<Frame>, (i32, i32))> = self.frame_lookup.iter_mut()
                .enumerate()
                .map(|(layer, vec)| {
                    if f < vec.len() {
                        vec.swap_remove(f).map(|(frame_data, _x, _y)| (frame_data, layer))
                    } else {
                        None
                    }
                }).map(|x| match x {
                    Some((x, layer)) => {
                        let offsets = self_frames[layer].get(&x)
                            .expect("Lookups weren't synced")
                            .iter()
                            .find(|x| x.0 == f)
                            .map(|x| x.1)
                            .expect("Lookups weren't synced");
                        (x, offsets)
                    }
                    None => (dummy_frame.clone(), (0, 0)),
                }).collect::<Vec<_>>();

            let base_x_offset = frames.iter().filter(|x| !x.0.data.is_empty())
                .map(|x| (x.1).0).min().unwrap_or(0);
            let base_y_offset = frames.iter().filter(|x| !x.0.data.is_empty())
                .map(|x| (x.1).1).min().unwrap_or(0);
            // Remap x/y offsets so that topleftmost frames are at 0,0 and
            // others relative to that.
            for f in &mut frames {
                if f.0.data.is_empty() {
                    (f.1).0 = base_x_offset;
                    (f.1).1 = base_y_offset;
                } else {
                    (f.1).0 -= base_x_offset;
                    (f.1).1 -= base_y_offset;
                }
            }
            let width = frames.iter().filter(|x| !x.0.data.is_empty())
                .map(|x| (x.1).0 as u32 + (x.0).width).max().unwrap_or(0);
            let height = frames.iter().filter(|x| !x.0.data.is_empty())
                .map(|x| (x.1).1 as u32 + (x.0).height).max().unwrap_or(0);
            let frames = LayerFrames {
                frames,
                width,
                height,
            };

            let entry = final_map.entry(frames);
            entry.or_insert_with(Default::default).push((f, FrameOffset {
                x: base_x_offset,
                y: base_y_offset,
            }));
        }
        for f in self.frame_lookup {
            assert!(f.is_empty());
        }

        let mut layout_order = final_map.into_iter().map(|(k, v)| (v, k)).collect::<Vec<_>>();
        // Place tallest frames first
        layout_order.sort_by(|a, b| {
            match (a.1.height, a.1.width).cmp(&(b.1.height, b.1.width)) {
                std::cmp::Ordering::Equal => {
                    // Use lowest found frame to break the tie
                    let a_frame = a.0.iter().map(|x| x.0).min().unwrap_or(0);
                    let b_frame = b.0.iter().map(|x| x.0).min().unwrap_or(0);
                    a_frame.cmp(&b_frame)
                }
                x => x,
            }
        });

        layout_frames(layout_order, 8, frame_count)
    }

    pub fn write_grp<W: Write + Seek>(
        &self,
        out: &mut W,
        width: u16,
        height: u16,
    ) -> Result<(), Error> {
        let frames = self.frame_lookup.get(0)
            .ok_or_else(|| anyhow!("No frames for layer 0"))?;
        out.write_u16::<LE>(u16::try_from(frames.len()).context("Too many frames")?)?;
        out.write_u16::<LE>(width)?;
        out.write_u16::<LE>(height)?;
        for frame in frames.iter() {
            if let Some((frame, x, y)) = frame {
                out.write_u8(u8::try_from(*x).context("Bad frame x")?)?;
                out.write_u8(u8::try_from(*y).context("Bad frame y")?)?;
                out.write_u8(u8::try_from(frame.width).context("Bad frame width")?)?;
                out.write_u8(u8::try_from(frame.height).context("Bad frame height")?)?;
            } else {
                out.write_u32::<LE>(0)?;
            }
            out.write_u32::<LE>(0)?;
        }
        let mut frame_offsets = Vec::with_capacity(frames.len());
        for frame in frames.iter() {
            frame_offsets.push(out.seek(SeekFrom::Current(0))? as u32);
            if let Some((frame, _, _)) = frame {
                write_grp_frame(out, frame)?;
            }
        }
        out.seek(SeekFrom::Start(10))?;
        for offset in frame_offsets {
            out.write_u32::<LE>(offset)?;
            out.seek(SeekFrom::Current(4))?;
        }

        Ok(())
    }
}

fn write_grp_frame<W: Write + Seek>(
    out: &mut W,
    frame: &Frame,
) -> Result<(), Error> {
    let mut line_offsets = Vec::with_capacity(frame.height as usize);
    let start = out.seek(SeekFrom::Current(0))?;
    for _ in 0..frame.height {
        out.write_u16::<LE>(0)?;
    }
    let mut offset = frame.height as u16 * 2;
    for line in frame.data.chunks_exact(frame.width as usize * 4) {
        line_offsets.push(offset);
        let mut pos = line;
        while pos.len() >= 4 {
            let len;
            if pos[3] == 0 {
                // Write transparent
                len = pos.chunks_exact(4).take(0x7f).take_while(|x| x[3] == 0).count();
                out.write_u8(0x80 | len as u8)?;
                offset += 1;
            } else {
                len = pos.chunks_exact(4).take(0x3f).take_while(|x| x[3] != 0).count();
                out.write(&[0x40 | len as u8, 0x01])?;
                offset += 2;
            }
            pos = &pos[4 * len..];
        }
    }
    let end = out.seek(SeekFrom::Current(0))?;
    out.seek(SeekFrom::Start(start))?;
    for offset in line_offsets {
        out.write_u16::<LE>(offset)?;
    }
    out.seek(SeekFrom::Start(end))?;
    Ok(())
}

fn layout_frames(
    mut frames: Vec<(Vec<(usize, FrameOffset)>, LayerFrames)>,
    alignment: u32,
    frame_count: usize,
) -> LayoutResult {
    fn fits(
        placed: &[(Vec<(usize, FrameOffset)>, LayerFrames, TexCoords)],
        pos: &TexCoords,
        dimensions: &(u32, u32),
    ) -> bool {
        let left = pos.x;
        let right = pos.x + dimensions.0;
        let top = pos.y;
        let bottom = pos.y + dimensions.1;
        !placed.iter().any(|&(_, ref f, ref tex)| {
            (left < tex.x + f.width && right > tex.x) &&
                (top < tex.y + f.height && bottom > tex.y)
        })
    }

    let mut result: Vec<(_, LayerFrames, TexCoords)> = Vec::with_capacity(frames.len());
    let mut out_width = 0;
    let mut out_height = 0;
    while let Some((uses, frame)) = frames.pop() {
        let mask = alignment - 1;
        let round_to_alignment = |x: u32| {
            ((x - 1) | mask) + 1
        };
        let width = round_to_alignment(frame.width);
        let height = round_to_alignment(frame.height);
        // Find a place which ideally adds as little as possible to width/height,
        // or allow increasing width as long as it doesn't go past next power of two.
        let mut best_on_right: Option<(TexCoords, u32, u32)> = None;
        let mut best_on_bottom: Option<(TexCoords, u32, u32, u32)> = None;
        for placed in &result {
            let right_pos = TexCoords {
                x: round_to_alignment(placed.2.x + placed.1.width),
                y: placed.2.y,
            };
            let width_add = (right_pos.x + width).saturating_sub(out_width);
            let right_squared = right_pos.x * right_pos.x + right_pos.y * right_pos.y;
            let right_better = match best_on_right {
                Some((_old_pos, old_squared, old_add)) => {
                    if old_add == width_add {
                        old_squared > right_squared
                    } else {
                        old_add > width_add
                    }
                }
                None => true,
            };
            let bottom_pos = TexCoords {
                x: placed.2.x,
                y: round_to_alignment(placed.2.y + placed.1.height),
            };
            let height_add = (bottom_pos.y + height).saturating_sub(out_height);
            let bottom_width_add = (bottom_pos.x + width).saturating_sub(out_width);
            let bottom_squared = bottom_pos.x * bottom_pos.x + bottom_pos.y * bottom_pos.y;
            let bottom_better = match best_on_bottom {
                Some((_old_pos, old_squared, old_add, old_width_add)) => {
                    if bottom_width_add > old_width_add {
                        false
                    } else if old_add == height_add {
                        old_squared > bottom_squared
                    } else {
                        old_add > height_add
                    }
                }
                None => true,
            };
            if right_better {
                if fits(&result, &right_pos, &(width, height))  {
                    best_on_right = Some((right_pos, right_squared, width_add));
                }
            }
            if bottom_better {
                if fits(&result, &bottom_pos, &(width, height))  {
                    best_on_bottom =
                        Some((bottom_pos, bottom_squared, height_add, bottom_width_add));
                }
            }
        }
        let coords = if let (Some(right), Some(bottom)) = (best_on_right, best_on_bottom) {
            let take_right = {
                if out_width + right.2 <= 256 {
                    true
                } else if out_width + right.2 <= out_width.next_power_of_two() {
                    true
                } else {
                    out_width + right.2 < out_height + bottom.2
                }
            };
            if take_right {
                right.0
            } else {
                bottom.0
            }
        } else {
            TexCoords {
                x: 0,
                y: 0,
            }
        };
        out_width = out_width.max(coords.x + width);
        out_height = out_height.max(coords.y + height);
        debug!("Placing to {}, {} - {}, {}", coords.x, coords.y, width, height);
        result.push((uses, frame, coords));
    }
    debug!("Result size {} {}", out_width, out_height);
    LayoutResult {
        frames: result,
        texture_width: out_width,
        texture_height: out_height,
        frame_count,
    }
}

const BMP_MAGIC: u32 = 0x20504d42;
fn encode_monochrome(
    frames: &[(Vec<(usize, FrameOffset)>, LayerFrames, TexCoords)],
    layer: usize,
    width: u32,
    height: u32,
    scale: u32,
) -> Vec<u8> {
    let mut out = vec![0; (width * height) as usize + 4];
    (&mut out[..]).write_u32::<LE>(BMP_MAGIC).unwrap();
    for (_, f, place) in frames {
        let &(ref frame, ref offset) = &f.frames[layer];
        if frame.data.is_empty() {
            continue;
        }
        let mut out_pos = (
            (place.y + offset.1 as u32) / scale * width + (place.x + offset.0 as u32) / scale
        ) as usize + 4;
        for c in frame.data.chunks(frame.width as usize * 4) {
            let out = &mut out[out_pos..out_pos + frame.width as usize];
            for (out, x) in out.iter_mut().zip(c.chunks(4)) {
                *out = if x[3] < 128 { 0 } else { 255 };
            }
            out_pos += width as usize;
        }
    }
    out
}

fn encode_dxt5(
    frames: &[(Vec<(usize, FrameOffset)>, LayerFrames, TexCoords)],
    layer: usize,
    width: u32,
    height: u32,
    scale: u32,
) -> Vec<u8> {
    let width = ((width - 1) | 3) + 1;
    let height = ((height - 1) | 3) + 1;

    let mut out = vec![0; (width * height) as usize];
    let mut tmp_buf = Vec::new();
    for (_, f, place) in frames {
        let &(ref frame, ref offset) = &f.frames[layer];
        if frame.data.is_empty() {
            continue;
        }

        let x_block = (place.x + offset.0 as u32) / scale / 4;
        let y_block = (place.y + offset.1 as u32) / scale / 4;
        let frame_width = frame.width / scale;
        let frame_height = frame.height / scale;
        let width_aligned = match frame_width & 3 == 0 {
            true => frame_width,
            false => (frame_width | 3) + 1,
        };
        let height_aligned = match frame_height & 3 == 0 {
            true => frame_height,
            false => (frame_height | 3) + 1,
        };

        tmp_buf.clear();
        tmp_buf.resize((width_aligned * height_aligned) as usize, 0);
        squish::Format::Bc3.compress(
            &frame.data,
            frame_width as usize,
            frame_height as usize,
            squish::Params {
                algorithm: squish::Algorithm::IterativeClusterFit,
                weights: squish::COLOUR_WEIGHTS_PERCEPTUAL,
                weigh_colour_by_alpha: true,
            },
            &mut tmp_buf,
        );

        let mut y = y_block;
        let mut in_y = 0;
        let block_size_bytes = 16;
        let in_stride_bytes = (width_aligned / 4) * block_size_bytes;
        while in_y < height_aligned / 4 {
            let out_pos = ((y * (width / 4) + x_block) * block_size_bytes) as usize;
            let in_pos = (in_y * in_stride_bytes) as usize;
            (&mut out[out_pos..][..in_stride_bytes as usize])
                .copy_from_slice(&tmp_buf[in_pos..][..in_stride_bytes as usize]);
            y += 1;
            in_y += 1;
        }
    }

    let mut dds = Dds::new_d3d(height, width, None, D3DFormat::DXT5, None, None).unwrap();
    dds.data = out;
    let mut dds_out = Vec::new();
    dds.write(&mut dds_out).unwrap();
    dds_out
}

fn encode_dxt1(
    frames: &[(Vec<(usize, FrameOffset)>, LayerFrames, TexCoords)],
    layer: usize,
    width: u32,
    height: u32,
    scale: u32,
) -> Vec<u8> {
    let width = ((width - 1) | 3) + 1;
    let height = ((height - 1) | 3) + 1;

    let mut out = vec![0; (width * height) as usize / 2];
    let mut tmp_buf = Vec::new();
    for (_, f, place) in frames {
        let &(ref frame, ref offset) = &f.frames[layer];
        if frame.data.is_empty() {
            continue;
        }

        let x_block = (place.x + offset.0 as u32) / scale / 4;
        let y_block = (place.y + offset.1 as u32) / scale / 4;
        let frame_width = frame.width / scale;
        let frame_height = frame.height / scale;
        let width_aligned = match frame_width & 3 == 0 {
            true => frame_width,
            false => (frame_width | 3) + 1,
        };
        let height_aligned = match frame_height & 3 == 0 {
            true => frame_height,
            false => (frame_height | 3) + 1,
        };

        tmp_buf.clear();
        tmp_buf.resize((width_aligned * height_aligned / 2) as usize, 0);
        squish::Format::Bc1.compress(
            &frame.data,
            frame_width as usize,
            frame_height as usize,
            squish::Params {
                algorithm: squish::Algorithm::IterativeClusterFit,
                weights: squish::COLOUR_WEIGHTS_PERCEPTUAL,
                weigh_colour_by_alpha: false,
            },
            &mut tmp_buf,
        );

        let mut y = y_block;
        let mut in_y = 0;
        let block_size_bytes = 8;
        let in_stride_bytes = (width_aligned / 4) * block_size_bytes;
        while in_y < height_aligned / 4 {
            let out_pos = ((y * (width / 4) + x_block) * block_size_bytes) as usize;
            let in_pos = (in_y * in_stride_bytes) as usize;
            (&mut out[out_pos..][..in_stride_bytes as usize])
                .copy_from_slice(&tmp_buf[in_pos..][..in_stride_bytes as usize]);
            y += 1;
            in_y += 1;
        }
    }

    let mut dds = Dds::new_d3d(height, width, None, D3DFormat::DXT1, None, None).unwrap();
    dds.data = out;
    let mut dds_out = Vec::new();
    dds.write(&mut dds_out).unwrap();
    dds_out
}

pub fn encode(rgba: &[u8], width: u32, height: u32, format: anim::TextureFormat) -> Vec<u8> {
    assert_eq!(rgba.len(), (width * height) as usize * 4);
    let frames = [(
        vec![(0, FrameOffset { x: 0, y: 0 })],
        LayerFrames {
            frames: vec![(Rc::new(Frame {
                width,
                height,
                // ugh
                data: rgba.into(),
            }), (0, 0))],
            width,
            height,
        },
        TexCoords {
            x: 0,
            y: 0,
        }
    )];
    match format {
        anim::TextureFormat::Dxt1 => encode_dxt1(&frames, 0, width, height, 1),
        anim::TextureFormat::Dxt5 => encode_dxt5(&frames, 0, width, height, 1),
        anim::TextureFormat::Monochrome => encode_monochrome(&frames, 0, width, height, 1),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn check_roundtrip(
        color: &[u8; 4],
        width: u32,
        height: u32,
        format: anim::TextureFormat,
    ) {
        let mut bytes = Vec::new();
        bytes.extend((0..(width * height)).flat_map(|_| color.iter().copied()));
        let encoded = encode(&bytes, width, height, format);

        println!("Checking {} x {}", width, height);
        let cursor = std::io::Cursor::new(&encoded);
        let decoded = anim::read_texture(cursor, &anim::Texture {
            width: width as u16,
            height: height as u16,
            offset: 0,
            size: encoded.len() as u32,
        }).unwrap();
        let decoded = decoded.data;
        assert!(bytes == decoded);
    }

    #[test]
    fn dxt1_roundtrip() {
        for i in 0..4 {
            for j in 0..4 {
                check_roundtrip(&[0xff, 0x00, 0xff, 0xff], 40 + i, 20 + j, anim::TextureFormat::Dxt1);
            }
        }
    }

    #[test]
    fn dxt5_roundtrip() {
        for i in 0..4 {
            for j in 0..4 {
                check_roundtrip(&[0xff, 0x80, 0x00, 0x80], 40 + i, 20 + j, anim::TextureFormat::Dxt5);
            }
        }
    }
}
