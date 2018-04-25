use std::collections::HashMap;
use std::mem;
use std::rc::Rc;

use byteorder::{LE, WriteBytesExt};

use anim;
use ddsfile::{Dds, D3DFormat};

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

/// Frames for a single layer
#[derive(Clone, Eq, PartialEq, Hash)]
struct LayerFrames {
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
    frame_lookup: Vec<Vec<Option<Rc<Frame>>>>,
}

pub struct LayoutResult {
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
        lookup[frame] = Some(frame_rc);
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
            let mut frames = self.frame_lookup.iter_mut().enumerate().map(|(i, vec)| {
                if f < vec.len() {
                    vec.swap_remove(f).map(|f| (f, i))
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

            let mut entry = final_map.entry(frames);
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
        layout_order.sort_by_key(|x| x.1.height);

        layout_frames(layout_order, 32, frame_count)
    }
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
        result.push((uses, frame, coords));
    }
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
            (place.y / scale + offset.1 as u32) * width + (place.x / scale + offset.0 as u32)
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
    for (_, f, place) in frames {
        let &(ref frame, ref offset) = &f.frames[layer];
        if frame.data.is_empty() {
            continue;
        }

        let x_pixel = place.x / scale + offset.0 as u32;
        let y_pixel = place.y / scale + offset.1 as u32;
        let frame_width = frame.width / scale;
        let last_x = x_pixel + frame_width;
        let last_y = y_pixel + frame.height / scale;
        assert!(last_x <= width);
        assert!(last_y <= height);

        let mut y = y_pixel;
        while y < last_y {
            let in_y = y - y_pixel;
            let y_block = y / 4;
            let mut x = x_pixel;
            while x < last_x {
                let in_x = x - x_pixel;
                let x_block = x / 4;
                let mut block = [[0; 4]; 16];
                for block_y in (y & 3)..(last_y - y).min(4) {
                    for block_x in (x & 3)..(last_x - x).min(4) {
                        let src_y = in_y + block_y - (y & 3);
                        let src_x = in_x + block_x - (x & 3);
                        let input_pos = (src_y * frame_width + src_x) as usize * 4;
                        let input_slice = &frame.data[input_pos..input_pos + 4];
                        block[(block_y * 4 + block_x) as usize].copy_from_slice(input_slice);
                    }
                }
                let has_far_alpha = block.iter().any(|x| x[3] < 16) ||
                    block.iter().any(|x| x[3] >= 240);
                let alpha = if has_far_alpha {
                    dxt5_alpha5(&block)
                } else {
                    dxt5_alpha7(&block)
                };
                let (colors, lookup) = colors_dxt5(&block);
                let out_pos = (y_block * width / 4 + x_block) as usize * 16;
                let mut out = &mut out[out_pos..out_pos + 16];
                out.write_u64::<LE>(alpha).unwrap();
                out.write_u32::<LE>(colors).unwrap();
                out.write_u32::<LE>(lookup).unwrap();

                x = (x | 3) + 1;
            }
            y = (y | 3) + 1;
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
    for (_, f, place) in frames {
        let &(ref frame, ref offset) = &f.frames[layer];
        if frame.data.is_empty() {
            continue;
        }

        let x_pixel = place.x / scale + offset.0 as u32;
        let y_pixel = place.y / scale + offset.1 as u32;
        let frame_width = frame.width / scale;
        let last_x = x_pixel + frame_width;
        let last_y = y_pixel + frame.height / scale;
        assert!(last_x <= width);
        assert!(last_y <= height);

        let mut y = y_pixel;
        while y < last_y {
            let in_y = y - y_pixel;
            let y_block = y / 4;
            let mut x = x_pixel;
            while x < last_x {
                let in_x = x - x_pixel;
                let x_block = x / 4;
                let mut block = [[0; 4]; 16];
                for block_y in (y & 3)..(last_y - y).min(4) {
                    for block_x in (x & 3)..(last_x - x).min(4) {
                        let src_y = in_y + block_y - (y & 3);
                        let src_x = in_x + block_x - (x & 3);
                        let input_pos = (src_y * frame_width + src_x) as usize * 4;
                        let input_slice = &frame.data[input_pos..input_pos + 4];
                        block[(block_y * 4 + block_x) as usize].copy_from_slice(input_slice);
                    }
                }
                let has_alpha = block.iter().any(|x| x[3] < 128);
                let (colors, lookup) = if has_alpha {
                    colors_dxt1_alpha(&block)
                } else {
                    colors_dxt1_no_alpha(&block)
                };
                let out_pos = (y_block * width / 4 + x_block) as usize * 8;
                let mut out = &mut out[out_pos..out_pos + 8];
                out.write_u32::<LE>(colors).unwrap();
                out.write_u32::<LE>(lookup).unwrap();

                x = (x | 3) + 1;
            }
            y = (y | 3) + 1;
        }
    }

    let mut dds = Dds::new_d3d(height, width, None, D3DFormat::DXT1, None, None).unwrap();
    dds.data = out;
    let mut dds_out = Vec::new();
    dds.write(&mut dds_out).unwrap();
    dds_out
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct SquaredColor(i32);

fn square_color(val: (i32, i32, i32)) -> SquaredColor {
    SquaredColor(val.0 * val.0 + val.1 * val.1 + val.2 * val.2)
}

fn square_distance(color: (i32, i32, i32), compare_sq: SquaredColor) -> i32 {
    (compare_sq.0 - (color.0 * color.0 + color.1 * color.1 + color.2 * color.2)).abs()
}

fn color_towards(src: i32, dest: i32, fraction: i32) -> i32 {
    src - (src - dest) / fraction
}

fn distances_sum(block: &[[u8; 4]; 16], squared: &[SquaredColor; 3]) -> i32 {
    let mut sum = 0i32;
    for x in block {
        if x[3] >= 128 {
            let color = (x[0] as i32, x[1] as i32, x[2] as i32);
            let distances = [
                square_distance(color, squared[0]),
                square_distance(color, squared[1]),
                square_distance(color, squared[2]),
            ];
            sum += distances.iter().cloned().min().unwrap();
        }
    }
    sum
}

fn distances_sum4(block: &[[u8; 4]; 16], squared: &[SquaredColor; 4]) -> i32 {
    let mut sum = 0i32;
    for x in block {
        let color = (x[0] as i32, x[1] as i32, x[2] as i32);
        let distances = [
            square_distance(color, squared[0]),
            square_distance(color, squared[1]),
            square_distance(color, squared[2]),
            square_distance(color, squared[3]),
        ];
        sum += distances.iter().cloned().min().unwrap();
    }
    sum
}

fn distances_sum_dxt5(block: &[[u8; 4]; 16], squared: &[SquaredColor; 4]) -> i32 {
    let mut sum = 0i32;
    for x in block {
        if x[3] > 32 {
            let color = (x[0] as i32, x[1] as i32, x[2] as i32);
            let distances = [
                square_distance(color, squared[0]),
                square_distance(color, squared[1]),
                square_distance(color, squared[2]),
                square_distance(color, squared[3]),
            ];
            sum += distances.iter().cloned().min().unwrap();
        }
    }
    sum
}

#[allow(unused_parens)]
fn colors_dxt1_alpha(block: &[[u8; 4]; 16]) -> (u32, u32) {
    // Calculate average of colors,
    // Calculate furthest color from average,
    // Calculate furthest color from furthest,
    // Try two variations for the actual colors.
    let mut color_sum = (0i32, 0i32, 0i32);
    let mut nonalpha_pixels = 0;
    for c in block {
        if c[3] >= 128 {
            nonalpha_pixels += 1;
            color_sum.0 += c[0] as i32;
            color_sum.1 += c[1] as i32;
            color_sum.2 += c[2] as i32;
        }
    }
    if nonalpha_pixels == 0 {
        return (!0, !0);
    }
    let average_color = (
        color_sum.0 / nonalpha_pixels,
        color_sum.1 / nonalpha_pixels,
        color_sum.2 / nonalpha_pixels,
    );
    let average_squared = square_color(average_color);

    let furthest_color = |compare_squared: SquaredColor| {
        let mut furthest_color = (0, 0, 0);
        let mut furthest_distance = 0;

        for c in block {
            if c[3] >= 128 {
                let distance = (compare_squared.0 - (
                    (c[0] as i32 * c[0] as i32) +
                    (c[1] as i32 * c[1] as i32) +
                    (c[2] as i32 * c[2] as i32)
                )).abs();
                if distance > furthest_distance {
                    furthest_distance = distance;
                    furthest_color = (c[0] as i32, c[1] as i32, c[2] as i32);
                }
            }
        }
        furthest_color
    };
    let first = furthest_color(average_squared);
    let first_squared = square_color(first);
    let second = furthest_color(first_squared);
    let second_squared = square_color(second);
    let average = (
        first.0 - (first.0 - second.0) / 2,
        first.1 - (first.1 - second.1) / 2,
        first.2 - (first.2 - second.2) / 2,
    );
    let average_squared = square_color(average);
    let (first, second) = {
        let variant1_sum = distances_sum(
            block,
            &[first_squared, second_squared, average_squared],
        );
        let variant2_first = (
            color_towards(first.0 as i32, second.0 as i32, 5),
            color_towards(first.1 as i32, second.1 as i32, 5),
            color_towards(first.2 as i32, second.2 as i32, 5),
        );
        let variant2_first_squared = square_color(variant2_first);
        let variant2_second = (
            color_towards(second.0 as i32, first.0 as i32, 5),
            color_towards(second.1 as i32, first.1 as i32, 5),
            color_towards(second.2 as i32, first.2 as i32, 5),
        );
        let variant2_second_squared = square_color(variant2_second);
        let variant2_sum = distances_sum(
            block,
            &[variant2_first_squared, variant2_second_squared, average_squared],
        );
        if variant1_sum <= variant2_sum {
            (first, second)
        } else {
            (variant2_first, variant2_second)
        }
    };
    let mut first_16bit = (
        ((first.0 + 3) & !0x7).min(255),
        ((first.1 + 1) & !0x3).min(255),
        ((first.2 + 3) & !0x7).min(255),
    );
    let mut second_16bit = (
        ((second.0 + 3) & !0x7).min(255),
        ((second.1 + 1) & !0x3).min(255),
        ((second.2 + 3) & !0x7).min(255),
    );
    if first_16bit == second_16bit {
        // Could be better logic, now just toggles a green bit
        second_16bit.1 ^= 0x4;
    }
    let mut first_raw = ((first_16bit.0 & 0xf8) << 8) |
        ((first_16bit.1 & 0xfc) << 3) |
        ((first_16bit.2 & 0xf8) >> 3);
    let mut second_raw = ((second_16bit.0 & 0xf8) << 8) |
        ((second_16bit.1 & 0xfc) << 3) |
        ((second_16bit.2 & 0xf8) >> 3);
    if first_raw > second_raw {
        mem::swap(&mut first_raw, &mut second_raw);
        mem::swap(&mut first_16bit, &mut second_16bit);
    }
    let average = (
        (first_16bit.0 + second_16bit.0) / 2,
        (first_16bit.1 + second_16bit.1) / 2,
        (first_16bit.2 + second_16bit.2) / 2,
    );
    let squared = [
        square_color(first_16bit),
        square_color(second_16bit),
        square_color(average),
    ];
    let mut lookup = 0;
    for pixel in block.iter().rev() {
        lookup = lookup << 2;
        if pixel[3] < 128 {
            lookup |= 3;
        } else {
            let color = (pixel[0] as i32, pixel[1] as i32, pixel[2] as i32);
            let distances = [
                square_distance(color, squared[0]),
                square_distance(color, squared[1]),
                square_distance(color, squared[2]),
            ];
            let index = distances.iter().enumerate()
                .min_by_key(|(_i, &x)| x)
                .unwrap().0 as u32;
            lookup |= index;
        }
    }
    (first_raw as u32 | ((second_raw as u32) << 16), lookup)
}

#[allow(unused_parens)]
fn colors_dxt1_no_alpha(block: &[[u8; 4]; 16]) -> (u32, u32) {
    // Calculate average of colors,
    // Calculate furthest color from average,
    // Calculate furthest color from furthest,
    // Try two variations for the actual colors.
    let mut color_sum = (0i32, 0i32, 0i32);
    for c in block {
        color_sum.0 += c[0] as i32;
        color_sum.1 += c[1] as i32;
        color_sum.2 += c[2] as i32;
    }
    let average_color = (
        color_sum.0 / 16,
        color_sum.1 / 16,
        color_sum.2 / 16,
    );
    let average_squared = square_color(average_color);

    let furthest_color = |compare_squared: SquaredColor| {
        let mut furthest_color = (0, 0, 0);
        let mut furthest_distance = 0;

        for c in block {
            let distance = (compare_squared.0 - (
                (c[0] as i32 * c[0] as i32) +
                (c[1] as i32 * c[1] as i32) +
                (c[2] as i32 * c[2] as i32)
            )).abs();
            if distance > furthest_distance {
                furthest_distance = distance;
                furthest_color = (c[0] as i32, c[1] as i32, c[2] as i32);
            }
        }
        furthest_color
    };
    let first = furthest_color(average_squared);
    let first_squared = square_color(first);
    let second = furthest_color(first_squared);
    let second_squared = square_color(second);
    let first_mid = (
        first.0 - (first.0 - second.0) / 3,
        first.1 - (first.1 - second.1) / 3,
        first.2 - (first.2 - second.2) / 3,
    );
    let second_mid = (
        first.0 - (first.0 - second.0) / 3 * 2,
        first.1 - (first.1 - second.1) / 3 * 2,
        first.2 - (first.2 - second.2) / 3 * 2,
    );
    let (first, second) = {
        let variant1_sum = distances_sum_dxt5(
            block,
            &[first_squared, second_squared, square_color(first_mid), square_color(second_mid)],
        );
        let variant2_first = (
            color_towards(first.0 as i32, second.0 as i32, 6),
            color_towards(first.1 as i32, second.1 as i32, 6),
            color_towards(first.2 as i32, second.2 as i32, 6),
        );
        let variant2_first_squared = square_color(variant2_first);
        let variant2_second = (
            color_towards(second.0 as i32, first.0 as i32, 6),
            color_towards(second.1 as i32, first.1 as i32, 6),
            color_towards(second.2 as i32, first.2 as i32, 6),
        );
        let variant2_second_squared = square_color(variant2_second);
        let variant2_first_mid = (
            variant2_first.0 - (variant2_first.0 - variant2_second.0) / 3,
            variant2_first.1 - (variant2_first.1 - variant2_second.1) / 3,
            variant2_first.2 - (variant2_first.2 - variant2_second.2) / 3,
        );
        let variant2_second_mid = (
            variant2_first.0 - (variant2_first.0 - variant2_second.0) / 3 * 2,
            variant2_first.1 - (variant2_first.1 - variant2_second.1) / 3 * 2,
            variant2_first.2 - (variant2_first.2 - variant2_second.2) / 3 * 2,
        );
        let variant2_sum = distances_sum_dxt5(
            block,
            &[
                variant2_first_squared,
                variant2_second_squared,
                square_color(variant2_first_mid),
                square_color(variant2_second_mid),
            ],
        );
        if variant1_sum <= variant2_sum {
            (first, second)
        } else {
            (variant2_first, variant2_second)
        }
    };
    let mut first_16bit = (
        ((first.0 + 3) & !0x7).min(255),
        ((first.1 + 1) & !0x3).min(255),
        ((first.2 + 3) & !0x7).min(255),
    );
    let mut second_16bit = (
        ((second.0 + 3) & !0x7).min(255),
        ((second.1 + 1) & !0x3).min(255),
        ((second.2 + 3) & !0x7).min(255),
    );
    if first_16bit == second_16bit {
        // Could be better logic, now just toggles a green bit
        second_16bit.1 ^= 0x4;
    }
    let mut first_raw = ((first_16bit.0 & 0xf8) << 8) |
        ((first_16bit.1 & 0xfc) << 3) |
        ((first_16bit.2 & 0xf8) >> 3);
    let mut second_raw = ((second_16bit.0 & 0xf8) << 8) |
        ((second_16bit.1 & 0xfc) << 3) |
        ((second_16bit.2 & 0xf8) >> 3);
    // First has to be larger
    if first_raw < second_raw {
        mem::swap(&mut first_raw, &mut second_raw);
        mem::swap(&mut first_16bit, &mut second_16bit);
    }
    let first_mid = (
        (first.0 * 2 + second.0) / 3,
        (first.1 * 2 + second.1) / 3,
        (first.2 * 2 + second.2) / 3,
    );
    let second_mid = (
        (second.0 * 2 + first.0) / 3,
        (second.1 * 2 + first.1) / 3,
        (second.2 * 2 + first.2) / 3,
    );
    let squared = [
        square_color(first_16bit),
        square_color(second_16bit),
        square_color(first_mid),
        square_color(second_mid),
    ];
    let mut lookup = 0;
    for pixel in block.iter().rev() {
        lookup = lookup << 2;
        let color = (pixel[0] as i32, pixel[1] as i32, pixel[2] as i32);
        let distances = [
            square_distance(color, squared[0]),
            square_distance(color, squared[1]),
            square_distance(color, squared[2]),
            square_distance(color, squared[3]),
        ];
        let index = distances.iter().enumerate()
            .min_by_key(|(_i, &x)| x)
            .unwrap().0 as u32;
        lookup |= index;
    }
    (first_raw as u32 | ((second_raw as u32) << 16), lookup)
}

#[allow(unused_parens)]
fn colors_dxt5(block: &[[u8; 4]; 16]) -> (u32, u32) {
    // Calculate average of colors,
    // Calculate furthest color from average,
    // Calculate furthest color from furthest,
    // Try two variations for the actual colors.
    let mut color_sum = (0i32, 0i32, 0i32);
    for c in block {
        if c[3] > 32 {
            color_sum.0 += c[0] as i32;
            color_sum.1 += c[1] as i32;
            color_sum.2 += c[2] as i32;
        }
    }
    let average_color = (
        color_sum.0 / 16,
        color_sum.1 / 16,
        color_sum.2 / 16,
    );
    let average_squared = square_color(average_color);

    let furthest_color = |compare_squared: SquaredColor| {
        let mut furthest_color = (0, 0, 0);
        let mut furthest_distance = 0;

        for c in block {
            if c[3] > 32 {
                let distance = (compare_squared.0 - (
                    (c[0] as i32 * c[0] as i32) +
                    (c[1] as i32 * c[1] as i32) +
                    (c[2] as i32 * c[2] as i32)
                )).abs();
                if distance > furthest_distance {
                    furthest_distance = distance;
                    furthest_color = (c[0] as i32, c[1] as i32, c[2] as i32);
                }
            }
        }
        furthest_color
    };
    let first = furthest_color(average_squared);
    let first_squared = square_color(first);
    let second = furthest_color(first_squared);
    let second_squared = square_color(second);
    let first_mid = (
        first.0 - (first.0 - second.0) / 3,
        first.1 - (first.1 - second.1) / 3,
        first.2 - (first.2 - second.2) / 3,
    );
    let second_mid = (
        first.0 - (first.0 - second.0) / 3 * 2,
        first.1 - (first.1 - second.1) / 3 * 2,
        first.2 - (first.2 - second.2) / 3 * 2,
    );
    let (first, second) = {
        let variant1_sum = distances_sum4(
            block,
            &[first_squared, second_squared, square_color(first_mid), square_color(second_mid)],
        );
        let variant2_first = (
            color_towards(first.0 as i32, second.0 as i32, 6),
            color_towards(first.1 as i32, second.1 as i32, 6),
            color_towards(first.2 as i32, second.2 as i32, 6),
        );
        let variant2_first_squared = square_color(variant2_first);
        let variant2_second = (
            color_towards(second.0 as i32, first.0 as i32, 6),
            color_towards(second.1 as i32, first.1 as i32, 6),
            color_towards(second.2 as i32, first.2 as i32, 6),
        );
        let variant2_second_squared = square_color(variant2_second);
        let variant2_first_mid = (
            variant2_first.0 - (variant2_first.0 - variant2_second.0) / 3,
            variant2_first.1 - (variant2_first.1 - variant2_second.1) / 3,
            variant2_first.2 - (variant2_first.2 - variant2_second.2) / 3,
        );
        let variant2_second_mid = (
            variant2_first.0 - (variant2_first.0 - variant2_second.0) / 3 * 2,
            variant2_first.1 - (variant2_first.1 - variant2_second.1) / 3 * 2,
            variant2_first.2 - (variant2_first.2 - variant2_second.2) / 3 * 2,
        );
        let variant2_sum = distances_sum4(
            block,
            &[
                variant2_first_squared,
                variant2_second_squared,
                square_color(variant2_first_mid),
                square_color(variant2_second_mid),
            ],
        );
        if variant1_sum <= variant2_sum {
            (first, second)
        } else {
            (variant2_first, variant2_second)
        }
    };
    let mut first_16bit = (
        ((first.0 + 3) & !0x7).min(255),
        ((first.1 + 1) & !0x3).min(255),
        ((first.2 + 3) & !0x7).min(255),
    );
    let mut second_16bit = (
        ((second.0 + 3) & !0x7).min(255),
        ((second.1 + 1) & !0x3).min(255),
        ((second.2 + 3) & !0x7).min(255),
    );
    if first_16bit == second_16bit {
        // Could be better logic, now just toggles a green bit
        second_16bit.1 ^= 0x4;
    }
    let mut first_raw = ((first_16bit.0 & 0xf8) << 8) |
        ((first_16bit.1 & 0xfc) << 3) |
        ((first_16bit.2 & 0xf8) >> 3);
    let mut second_raw = ((second_16bit.0 & 0xf8) << 8) |
        ((second_16bit.1 & 0xfc) << 3) |
        ((second_16bit.2 & 0xf8) >> 3);
    // First has to be larger
    if first_raw < second_raw {
        mem::swap(&mut first_raw, &mut second_raw);
        mem::swap(&mut first_16bit, &mut second_16bit);
    }
    let first_mid = (
        (first.0 * 2 + second.0) / 3,
        (first.1 * 2 + second.1) / 3,
        (first.2 * 2 + second.2) / 3,
    );
    let second_mid = (
        (second.0 * 2 + first.0) / 3,
        (second.1 * 2 + first.1) / 3,
        (second.2 * 2 + first.2) / 3,
    );
    let squared = [
        square_color(first_16bit),
        square_color(second_16bit),
        square_color(first_mid),
        square_color(second_mid),
    ];
    let mut lookup = 0;
    for pixel in block.iter().rev() {
        lookup = lookup << 2;
        let color = (pixel[0] as i32, pixel[1] as i32, pixel[2] as i32);
        let distances = [
            square_distance(color, squared[0]),
            square_distance(color, squared[1]),
            square_distance(color, squared[2]),
            square_distance(color, squared[3]),
        ];
        let index = distances.iter().enumerate()
            .min_by_key(|(_i, &x)| x)
            .unwrap().0 as u32;
        lookup |= index;
    }
    (first_raw as u32 | ((second_raw as u32) << 16), lookup)
}

fn dxt5_alpha5_far(val: u8) -> bool {
    val < 16 || val >= 240
}

#[allow(unused_parens)]
fn dxt5_alpha5(block: &[[u8; 4]; 16]) -> u64 {
    let mut first = block.iter().filter_map(|x| {
        if !dxt5_alpha5_far(x[3]) {
            Some(x[3])
        } else {
            None
        }
    }).min().unwrap_or(0);
    let mut second = block.iter().filter_map(|x| {
        if !dxt5_alpha5_far(x[3]) {
            Some(x[3])
        } else {
            None
        }
    }).max().unwrap_or(0);
    if first == second {
        second ^= 8;
    }
    // First has to be smaller
    if first >= second {
        mem::swap(&mut first, &mut second);
    }
    let first_f32 = first as f32;
    let second_f32 = second as f32;
    let alphas = [
        first,
        second,
        (first_f32 * (4.0 / 5.0) + second_f32 / 5.0) as u8,
        (first_f32 * (3.0 / 5.0) + second_f32 * (2.0 / 5.0)) as u8,
        (first_f32 * (2.0 / 5.0) + second_f32 * (3.0 / 5.0)) as u8,
        (first_f32 / 5.0 + second_f32 * (4.0 / 5.0)) as u8,
    ];
    let mut lookup = 0u64;
    for pixel in block.iter().rev() {
        lookup = lookup << 3;
        let alpha = pixel[3];
        let distances = [
            (alpha as i32 - alphas[0] as i32).abs(),
            (alpha as i32 - alphas[1] as i32).abs(),
            (alpha as i32 - alphas[2] as i32).abs(),
            (alpha as i32 - alphas[3] as i32).abs(),
            (alpha as i32 - alphas[4] as i32).abs(),
            (alpha as i32 - alphas[5] as i32).abs(),
            alpha as i32,
            255i32 - alpha as i32,
        ];
        let index = distances.iter().enumerate()
            .min_by_key(|(_i, &x)| x)
            .unwrap().0 as u64;
        lookup |= index;
    }
    (lookup << 16) | ((second as u64) << 8) | (first as u64)
}

#[allow(unused_parens)]
fn dxt5_alpha7(block: &[[u8; 4]; 16]) -> u64 {
    let mut first = block.iter().map(|x| x[3]).min().unwrap_or(0);
    let mut second = block.iter().map(|x| x[3]).max().unwrap_or(0);
    if first == second {
        second ^= 8;
    }
    // First has to be larger
    if first < second {
        mem::swap(&mut first, &mut second);
    }
    let first_f32 = first as f32;
    let second_f32 = second as f32;
    let alphas = [
        first,
        second,
        (first_f32 * (6.0 / 7.0) + second_f32 / 7.0) as u8,
        (first_f32 * (5.0 / 7.0) + second_f32 * (2.0 / 7.0)) as u8,
        (first_f32 * (4.0 / 7.0) + second_f32 * (3.0 / 7.0)) as u8,
        (first_f32 * (3.0 / 7.0) + second_f32 * (4.0 / 7.0)) as u8,
        (first_f32 * (2.0 / 7.0) + second_f32 * (5.0 / 7.0)) as u8,
        (first_f32 / 7.0 + second_f32 * (6.0 / 7.0)) as u8,
    ];
    let mut lookup = 0u64;
    for pixel in block.iter().rev() {
        lookup = lookup << 3;
        let alpha = pixel[3];
        let distances = [
            (alpha as i32 - alphas[0] as i32).abs(),
            (alpha as i32 - alphas[1] as i32).abs(),
            (alpha as i32 - alphas[2] as i32).abs(),
            (alpha as i32 - alphas[3] as i32).abs(),
            (alpha as i32 - alphas[4] as i32).abs(),
            (alpha as i32 - alphas[5] as i32).abs(),
            (alpha as i32 - alphas[6] as i32).abs(),
            (alpha as i32 - alphas[7] as i32).abs(),
        ];
        let index = distances.iter().enumerate()
            .min_by_key(|(_i, &x)| x)
            .unwrap().0 as u64;
        lookup |= index;
    }
    (lookup << 16) | ((second as u64) << 8) | (first as u64)
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
