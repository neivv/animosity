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
        let frame_width = frame.width / scale;
        for c in frame.data.chunks(frame_width as usize * 4) {
            let out = &mut out[out_pos..out_pos + frame_width as usize];
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
    let width = align4(width);
    let height = align4(height);

    let mut out = vec![0; (width * height) as usize];
    let mut tmp_buf = Vec::new();
    let mut in_buf = Vec::new();
    for (_, f, place) in frames {
        let &(ref frame, ref offset) = &f.frames[layer];
        if frame.data.is_empty() {
            continue;
        }

        let place_x = (place.x + offset.0 as u32) / scale;
        let place_y = (place.y + offset.1 as u32) / scale;
        let x_block = place_x / 4;
        let y_block = place_y / 4;
        let frame_width = frame.width / scale;
        let frame_height = frame.height / scale;
        let width_aligned = align4((place_x & 3) + frame_width);
        let height_aligned = align4((place_y & 3) + frame_height);

        tmp_buf.clear();
        tmp_buf.resize((width_aligned * height_aligned) as usize, 0);
        let (in_data, in_width, in_height) = {
            // Copy frame to a buffer that is 4-aligned as expected
            in_buf.clear();
            in_buf.resize(4 * (width_aligned * height_aligned) as usize, 0);
            for (frame_y, in_buf_y) in ((place_y & 3)..).take(frame_height as usize).enumerate() {
                let out_pos = (in_buf_y * width_aligned * 4 + (place_x & 3) * 4) as usize;
                let in_pos = frame_y * frame_width as usize * 4;
                let out_slice = &mut in_buf[out_pos..][..frame_width as usize * 4];
                let in_slice = &frame.data[in_pos..][..frame_width as usize * 4];
                out_slice.copy_from_slice(in_slice);
            }
            (&mut in_buf, width_aligned, height_aligned)
        };
        // squish appears to have some bug with color encoding if red/blue values of
        // a block are all equal. Add some variation to red by setting/clearing alternating
        // least significant bits.
        // (This makes a major difference in ao_depth importing since it's just green + alpha)
        for pixel in in_data.chunks_exact_mut(8) {
            pixel[0] |= 1;
            pixel[4] &= !1;
        }
        squish::Format::Bc3.compress(
            in_data,
            in_width as usize,
            in_height as usize,
            squish::Params {
                algorithm: squish::Algorithm::IterativeClusterFit,
                weights: squish::COLOUR_WEIGHTS_PERCEPTUAL,
                weigh_colour_by_alpha: false,
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

fn align4(val: u32) -> u32 {
    (val.wrapping_sub(1) | 3).wrapping_add(1)
}

fn encode_dxt1(
    frames: &[(Vec<(usize, FrameOffset)>, LayerFrames, TexCoords)],
    layer: usize,
    width: u32,
    height: u32,
    scale: u32,
) -> Vec<u8> {
    let width = align4(width);
    let height = align4(height);

    let mut out = vec![0; (width * height) as usize / 2];
    let mut in_buf = Vec::new();
    let mut tmp_buf = Vec::new();
    for (_, f, place) in frames {
        let &(ref frame, ref offset) = &f.frames[layer];
        if frame.data.is_empty() {
            continue;
        }

        let place_x = (place.x + offset.0 as u32) / scale;
        let place_y = (place.y + offset.1 as u32) / scale;
        let x_block = place_x / 4;
        let y_block = place_y / 4;
        let frame_width = frame.width / scale;
        let frame_height = frame.height / scale;
        let width_aligned = align4((place_x & 3) + frame_width);
        let height_aligned = align4((place_y & 3) + frame_height);

        tmp_buf.clear();
        tmp_buf.resize((width_aligned * height_aligned / 2) as usize, 0);
        let (in_data, in_width, in_height) = if place_x & 3 == 0 && place_y & 3 == 0 {
            // Can just use frame data.
            // Could also require widt/hheight be multiple of 4 and otherwise fill
            // with [0, 0, 0, 0], as squish defaults to [0, 0, 0, 255] for unspecified
            // pixels
            (&frame.data, frame_width, frame_height)
        } else {
            // Copy frame to a buffer that is 4-aligned as expected
            in_buf.clear();
            in_buf.resize(4 * (width_aligned * height_aligned) as usize, 0);
            for (frame_y, in_buf_y) in ((place_y & 3)..).take(frame_height as usize).enumerate() {
                let out_pos = (in_buf_y * width_aligned * 4 + (place_x & 3) * 4) as usize;
                let in_pos = frame_y * frame_width as usize * 4;
                let out_slice = &mut in_buf[out_pos..][..frame_width as usize * 4];
                let in_slice = &frame.data[in_pos..][..frame_width as usize * 4];
                out_slice.copy_from_slice(in_slice);
            }
            (&in_buf, width_aligned, height_aligned)
        };
        squish::Format::Bc1.compress(
            in_data,
            in_width as usize,
            in_height as usize,
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

    use std::io;

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

    fn bmp_eq_data(w: u32, h: u32, valid_l: u32, valid_t: u32, valid_r: u32, valid_b: u32)
        -> Vec<u8>
    {
        let mut eq_data = vec![66u8, 77, 80, 32]; // BMP\x20
        eq_data.extend((0u32..(w * h)).map(|i| {
            let col = i % w;
            let row = i / w;
            if row < valid_t || row >= valid_b || col < valid_l || col >= valid_r {
                0u8
            } else {
                255
            }
        }));
        eq_data
    }

    fn dxt1_eq_data(w: u32, h: u32, valid_l: u32, valid_t: u32, valid_r: u32, valid_b: u32)
        -> Vec<u8>
    {
        let mut eq_data = vec![];
        eq_data.extend((0u32..(w * h)).flat_map(|i| {
            let col = i % w;
            let row = i / w;
            if row < valid_t || row >= valid_b || col < valid_l || col >= valid_r {
                // Changing code so that pixels outside tex area were [0, 0, 0, 0] would be ok
                if row >= align4(valid_b) || col >= align4(valid_r) {
                    vec![0u8, 0, 0, 255]
                } else {
                    vec![0u8, 0, 0, 0]
                }
            } else {
                vec![255u8, 255, 255, 255]
            }
        }));
        eq_data
    }

    fn dxt5_eq_data(w: u32, h: u32, valid_l: u32, valid_t: u32, valid_r: u32, valid_b: u32)
        -> Vec<u8>
    {
        let mut eq_data = vec![];
        eq_data.extend((0u32..(w * h)).flat_map(|i| {
            let col = i % w;
            let row = i / w;
            if row < valid_t || row >= valid_b || col < valid_l || col >= valid_r {
                vec![0u8, 0, 0, 0]
            } else {
                vec![255u8, 255, 255, 255]
            }
        }));
        eq_data
    }

    #[test]
    fn hd_hd2_offsets() {
        // Set HD2 offset to 4, 4
        // but HD to 9, 9.
        // Make sure that the HD2 offset being scaled to 8, 8
        // won't change HD offset.
        // (Maybe the encoding code should handle the HD-HD2 relation better)
        let mut layout = Layout::new();
        let coords = FrameCoords {
            x_offset: 9,
            y_offset: 9,
            width: 8,
            height: 8,
        };
        let data = (0usize..(8*8)).flat_map(|_| {
            vec![255, 255, 255, 255]
        }).collect();
        layout.add_frame(0, 0, data, coords);
        let coords = FrameCoords {
            x_offset: 8,
            y_offset: 8,
            width: 8,
            height: 8,
        };
        let data = (0usize..(4*4)).flat_map(|_| {
            vec![255, 255, 255, 255]
        }).collect();
        layout.add_frame(1, 0, data, coords);
        let result = layout.layout();

        // --- Monochrome ---
        let hd = result.encode(0, &[Some(anim::TextureFormat::Monochrome)], 1);
        let hd2 = result.encode(1, &[Some(anim::TextureFormat::Monochrome)], 2);
        assert_eq!(hd2.frames[0].x_off, 8);
        assert_eq!(hd2.frames[0].y_off, 8);
        assert_eq!(hd2.frames[0].width, 9);
        assert_eq!(hd2.frames[0].height, 9);
        let (tex, tex_data) = hd2.textures[0].as_ref().unwrap();
        let eq_data = bmp_eq_data(8, 8, 0, 0, 4, 4);
        assert_eq!(tex.width, 8);
        assert_eq!(tex.height, 8);
        assert_eq!(*tex_data, eq_data);

        assert_eq!(hd.frames[0].x_off, 8);
        assert_eq!(hd.frames[0].y_off, 8);
        assert_eq!(hd.frames[0].width, 9);
        assert_eq!(hd.frames[0].height, 9);
        let (tex, tex_data) = hd.textures[0].as_ref().unwrap();
        let eq_data = bmp_eq_data(16, 16, 1, 1, 9, 9);
        assert_eq!(tex.width, 16);
        assert_eq!(tex.height, 16);
        assert_eq!(*tex_data, eq_data);

        // --- DXT1 ---
        let hd = result.encode(0, &[Some(anim::TextureFormat::Dxt1)], 1);
        let hd2 = result.encode(1, &[Some(anim::TextureFormat::Dxt1)], 2);
        let (tex, tex_data) = hd2.textures[0].as_ref().unwrap();
        assert_eq!(tex.width, 8);
        assert_eq!(tex.height, 8);
        let decoded = crate::anim::read_texture(io::Cursor::new(&tex_data), tex).unwrap();
        assert_eq!(decoded.width, 8);
        assert_eq!(decoded.height, 8);
        let eq_data = dxt1_eq_data(8, 8, 0, 0, 4, 4);
        assert_eq!(decoded.data, eq_data);

        let (tex, tex_data) = hd.textures[0].as_ref().unwrap();
        assert_eq!(tex.width, 16);
        assert_eq!(tex.height, 16);
        let decoded = crate::anim::read_texture(io::Cursor::new(&tex_data), tex).unwrap();
        assert_eq!(decoded.width, 16);
        assert_eq!(decoded.height, 16);
        let eq_data = dxt1_eq_data(16, 16, 1, 1, 9, 9);
        assert_eq!(decoded.data, eq_data);

        // --- DXT5 ---
        let hd = result.encode(0, &[Some(anim::TextureFormat::Dxt5)], 1);
        let hd2 = result.encode(1, &[Some(anim::TextureFormat::Dxt5)], 2);
        let (tex, tex_data) = hd2.textures[0].as_ref().unwrap();
        assert_eq!(tex.width, 8);
        assert_eq!(tex.height, 8);
        let decoded = crate::anim::read_texture(io::Cursor::new(&tex_data), tex).unwrap();
        assert_eq!(decoded.width, 8);
        assert_eq!(decoded.height, 8);
        let eq_data = dxt5_eq_data(8, 8, 0, 0, 4, 4);
        assert_eq!(decoded.data, eq_data);

        let (tex, tex_data) = hd.textures[0].as_ref().unwrap();
        assert_eq!(tex.width, 16);
        assert_eq!(tex.height, 16);
        let decoded = crate::anim::read_texture(io::Cursor::new(&tex_data), tex).unwrap();
        assert_eq!(decoded.width, 16);
        assert_eq!(decoded.height, 16);
        let eq_data = dxt5_eq_data(16, 16, 1, 1, 9, 9);
        assert_eq!(decoded.data, eq_data);
    }

    #[test]
    fn hd_hd2_offsets_tall() {
        let mut layout = Layout::new();
        let coords = FrameCoords {
            x_offset: 9,
            y_offset: 9,
            width: 8,
            height: 16,
        };
        let data = (0usize..(8*16)).flat_map(|_| {
            vec![255, 255, 255, 255]
        }).collect();
        layout.add_frame(0, 0, data, coords);
        let coords = FrameCoords {
            x_offset: 8,
            y_offset: 8,
            width: 8,
            height: 16,
        };
        let data = (0usize..(4*8)).flat_map(|_| {
            vec![255, 255, 255, 255]
        }).collect();
        layout.add_frame(1, 0, data, coords);
        let result = layout.layout();

        // --- Monochrome ---
        let hd = result.encode(0, &[Some(anim::TextureFormat::Monochrome)], 1);
        let hd2 = result.encode(1, &[Some(anim::TextureFormat::Monochrome)], 2);
        assert_eq!(hd2.frames[0].x_off, 8);
        assert_eq!(hd2.frames[0].y_off, 8);
        assert_eq!(hd2.frames[0].width, 9);
        assert_eq!(hd2.frames[0].height, 17);
        let (tex, tex_data) = hd2.textures[0].as_ref().unwrap();
        let eq_data = bmp_eq_data(8, 12, 0, 0, 4, 8);
        assert_eq!(tex.width, 8);
        assert_eq!(tex.height, 12);
        assert_eq!(*tex_data, eq_data);

        assert_eq!(hd.frames[0].x_off, 8);
        assert_eq!(hd.frames[0].y_off, 8);
        assert_eq!(hd.frames[0].width, 9);
        assert_eq!(hd.frames[0].height, 17);
        let (tex, tex_data) = hd.textures[0].as_ref().unwrap();
        let eq_data = bmp_eq_data(16, 24, 1, 1, 9, 17);
        assert_eq!(tex.width, 16);
        assert_eq!(tex.height, 24);
        assert_eq!(*tex_data, eq_data);

        // --- DXT1 ---
        let hd = result.encode(0, &[Some(anim::TextureFormat::Dxt1)], 1);
        let hd2 = result.encode(1, &[Some(anim::TextureFormat::Dxt1)], 2);
        let (tex, tex_data) = hd2.textures[0].as_ref().unwrap();
        assert_eq!(tex.width, 8);
        assert_eq!(tex.height, 12);
        let decoded = crate::anim::read_texture(io::Cursor::new(&tex_data), tex).unwrap();
        assert_eq!(decoded.width, 8);
        assert_eq!(decoded.height, 12);
        let eq_data = dxt1_eq_data(8, 12, 0, 0, 4, 8);
        assert_eq!(decoded.data, eq_data);

        let (tex, tex_data) = hd.textures[0].as_ref().unwrap();
        assert_eq!(tex.width, 16);
        assert_eq!(tex.height, 24);
        let decoded = crate::anim::read_texture(io::Cursor::new(&tex_data), tex).unwrap();
        assert_eq!(decoded.width, 16);
        assert_eq!(decoded.height, 24);
        let eq_data = dxt1_eq_data(16, 24, 1, 1, 9, 17);
        assert_eq!(decoded.data, eq_data);

        // --- DXT5 ---
        let hd = result.encode(0, &[Some(anim::TextureFormat::Dxt5)], 1);
        let hd2 = result.encode(1, &[Some(anim::TextureFormat::Dxt5)], 2);
        let (tex, tex_data) = hd2.textures[0].as_ref().unwrap();
        assert_eq!(tex.width, 8);
        assert_eq!(tex.height, 12);
        let decoded = crate::anim::read_texture(io::Cursor::new(&tex_data), tex).unwrap();
        assert_eq!(decoded.width, 8);
        assert_eq!(decoded.height, 12);
        let eq_data = dxt5_eq_data(8, 12, 0, 0, 4, 8);
        assert_eq!(decoded.data, eq_data);

        let (tex, tex_data) = hd.textures[0].as_ref().unwrap();
        assert_eq!(tex.width, 16);
        assert_eq!(tex.height, 24);
        let decoded = crate::anim::read_texture(io::Cursor::new(&tex_data), tex).unwrap();
        assert_eq!(decoded.width, 16);
        assert_eq!(decoded.height, 24);
        let eq_data = dxt5_eq_data(16, 24, 1, 1, 9, 17);
        assert_eq!(decoded.data, eq_data);
    }

    #[test]
    fn hd_hd2_offsets_wide() {
        let mut layout = Layout::new();
        let coords = FrameCoords {
            x_offset: 9,
            y_offset: 9,
            width: 16,
            height: 8,
        };
        let data = (0usize..(16*8)).flat_map(|_| {
            vec![255, 255, 255, 255]
        }).collect();
        layout.add_frame(0, 0, data, coords);
        let coords = FrameCoords {
            x_offset: 8,
            y_offset: 8,
            width: 16,
            height: 8,
        };
        let data = (0usize..(8*4)).flat_map(|_| {
            vec![255, 255, 255, 255]
        }).collect();
        layout.add_frame(1, 0, data, coords);
        let result = layout.layout();

        // --- Monochrome ---
        let hd = result.encode(0, &[Some(anim::TextureFormat::Monochrome)], 1);
        let hd2 = result.encode(1, &[Some(anim::TextureFormat::Monochrome)], 2);
        assert_eq!(hd2.frames[0].x_off, 8);
        assert_eq!(hd2.frames[0].y_off, 8);
        assert_eq!(hd2.frames[0].width, 17);
        assert_eq!(hd2.frames[0].height, 9);
        let (tex, tex_data) = hd2.textures[0].as_ref().unwrap();
        let eq_data = bmp_eq_data(12, 8, 0, 0, 8, 4);
        assert_eq!(tex.width, 12);
        assert_eq!(tex.height, 8);
        assert_eq!(*tex_data, eq_data);

        assert_eq!(hd.frames[0].x_off, 8);
        assert_eq!(hd.frames[0].y_off, 8);
        assert_eq!(hd.frames[0].width, 17);
        assert_eq!(hd.frames[0].height, 9);
        let (tex, tex_data) = hd.textures[0].as_ref().unwrap();
        let eq_data = bmp_eq_data(24, 16, 1, 1, 17, 9);
        assert_eq!(tex.width, 24);
        assert_eq!(tex.height, 16);
        assert_eq!(*tex_data, eq_data);

        // --- DXT1 ---
        let hd = result.encode(0, &[Some(anim::TextureFormat::Dxt1)], 1);
        let hd2 = result.encode(1, &[Some(anim::TextureFormat::Dxt1)], 2);
        let (tex, tex_data) = hd2.textures[0].as_ref().unwrap();
        assert_eq!(tex.width, 12);
        assert_eq!(tex.height, 8);
        let decoded = crate::anim::read_texture(io::Cursor::new(&tex_data), tex).unwrap();
        assert_eq!(decoded.width, 12);
        assert_eq!(decoded.height, 8);
        let eq_data = dxt1_eq_data(12, 8, 0, 0, 8, 4);
        assert_eq!(decoded.data, eq_data);

        let (tex, tex_data) = hd.textures[0].as_ref().unwrap();
        assert_eq!(tex.width, 24);
        assert_eq!(tex.height, 16);
        let decoded = crate::anim::read_texture(io::Cursor::new(&tex_data), tex).unwrap();
        assert_eq!(decoded.width, 24);
        assert_eq!(decoded.height, 16);
        let eq_data = dxt1_eq_data(24, 16, 1, 1, 17, 9);
        assert_eq!(decoded.data, eq_data);

        // --- DXT5 ---
        let hd = result.encode(0, &[Some(anim::TextureFormat::Dxt5)], 1);
        let hd2 = result.encode(1, &[Some(anim::TextureFormat::Dxt5)], 2);
        let (tex, tex_data) = hd2.textures[0].as_ref().unwrap();
        assert_eq!(tex.width, 12);
        assert_eq!(tex.height, 8);
        let decoded = crate::anim::read_texture(io::Cursor::new(&tex_data), tex).unwrap();
        assert_eq!(decoded.width, 12);
        assert_eq!(decoded.height, 8);
        let eq_data = dxt5_eq_data(12, 8, 0, 0, 8, 4);
        assert_eq!(decoded.data, eq_data);

        let (tex, tex_data) = hd.textures[0].as_ref().unwrap();
        assert_eq!(tex.width, 24);
        assert_eq!(tex.height, 16);
        let decoded = crate::anim::read_texture(io::Cursor::new(&tex_data), tex).unwrap();
        assert_eq!(decoded.width, 24);
        assert_eq!(decoded.height, 16);
        let eq_data = dxt5_eq_data(24, 16, 1, 1, 17, 9);
        assert_eq!(decoded.data, eq_data);
    }
}
