use std::convert::TryFrom;
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

use failure::ResultExt;
use image::{GenericImageView, ImageFormat};

use crate::anim;
use crate::anim_encoder;
use crate::ddsgrp;
use crate::files;
use crate::frame_info::{FrameInfo};
use crate::{SpriteType, Error};

pub fn import_frames_grp(
    files: &mut files::Files,
    dir: &Path,
    filename_prefix: &str,
    frame_scale: f32,
    format: anim::TextureFormat,
    sprite: usize,
    scale: u8,
) -> Result<u32, Error> {
    let mut frame_count = 0;
    let mut frames = Vec::new();
    for i in 0.. {
        let path = &dir.join(format!("{}_{:03}.png", filename_prefix, i));
        if !path.is_file() {
            frame_count = i;
            break;
        }

        let file = File::open(path)
            .with_context(|_| format!("Unable to open {}", path.to_string_lossy()))?;
        let image = image::load(BufReader::new(file), ImageFormat::PNG)
            .with_context(|_| format!("Unable to load PNG {}", path.to_string_lossy()))?;
        let buffer = if frame_scale != 1.0 {
            let new_width = (image.width() as f32 * frame_scale) as u32;
            let new_height = (image.height() as f32 * frame_scale) as u32;
            image::imageops::resize(&image, new_width, new_height, image::FilterType::Lanczos3)
        } else {
            image.to_rgba()
        };

        let (width, height) = buffer.dimensions();
        let data = buffer.into_raw();
        let data = anim_encoder::encode(&data, width, height, format);
        frames.push((ddsgrp::Frame {
            unknown: 0,
            width: u16::try_from(width)
                .map_err(|_| format_err!("Frame {} width too large", i))?,
            height: u16::try_from(height)
                .map_err(|_| format_err!("Frame {} width too large", i))?,
            size: data.len() as u32,
            offset: !0,
        }, data));
    }
    files.set_grp_changes(sprite, frames, scale);
    Ok(frame_count)
}

pub fn import_frames(
    files: &mut files::Files,
    frame_info: &FrameInfo,
    hd2_frame_info: Option<&FrameInfo>,
    dir: &Path,
    hd2_dir: Option<&Path>,
    frame_scale: f32,
    hd2_frame_scale: Option<f32>,
    formats: &[anim::TextureFormat],
    sprite: usize,
    ty: SpriteType,
) -> Result<(), Error> {
    fn add_layers(
        layout: &mut anim_encoder::Layout,
        frame_info: &FrameInfo,
        dir: &Path,
        first_layer: usize,
        frame_scale: f32,
        scale: u32,
    ) -> Result<(), Error> {
        for &(i, ref layer_prefix) in &frame_info.layers {
            let layer = first_layer + i as usize;
            for f in 0..frame_info.frame_count {
                let path = &dir.join(format!("{}_{:03}.png", layer_prefix, f));

                let file = File::open(path)
                    .with_context(|_| format!("Unable to open {}", path.to_string_lossy()))?;
                let image = image::load(BufReader::new(file), ImageFormat::PNG)
                    .with_context(|_| format!("Unable to load PNG {}", path.to_string_lossy()))?;
                let buffer = if frame_scale != 1.0 {
                    let new_width = (image.width() as f32 * frame_scale) as u32;
                    let new_height = (image.height() as f32 * frame_scale) as u32;
                    image::imageops::resize(
                        &image,
                        new_width,
                        new_height,
                        image::FilterType::Lanczos3,
                    )
                } else {
                    image.to_rgba()
                };
                let (width, height) = buffer.dimensions();
                let data = buffer.into_raw();
                let mut bounded = rgba_bounding_box(&data, width, height);
                bounded.coords.x_offset =
                    bounded.coords.x_offset.saturating_add(frame_info.offset_x) * scale as i32;
                bounded.coords.y_offset =
                    bounded.coords.y_offset.saturating_add(frame_info.offset_y) * scale as i32;
                bounded.coords.width *= scale;
                bounded.coords.height *= scale;
                layout.add_frame(layer, f as usize, bounded.data, bounded.coords);
            }
        }
        Ok(())
    }

    let hd2_frame_info = match (hd2_frame_info, hd2_dir) {
        (Some(a), Some(b)) => Some((a, b)),
        _ => None,
    };

    let layer_count = formats.len();
    let mut layout = anim_encoder::Layout::new();
    add_layers(&mut layout, frame_info, dir, 0, frame_scale, 1)?;
    if let Some((hd2, dir)) = hd2_frame_info {
        add_layers(&mut layout, hd2, dir, layer_count, hd2_frame_scale.unwrap_or(1.0), 2)?;
    }
    let layout_result = layout.layout();

    let formats = formats.iter().enumerate().map(|(i, &f)| {
        if frame_info.layers.iter().any(|x| x.0 as usize == i) {
            Some(f)
        } else {
            None
        }
    }).collect::<Vec<_>>();

    let ty = if hd2_frame_info.is_some() {
        SpriteType::Hd
    } else {
        ty
    };

    let scale_mul = match ty {
        SpriteType::Hd2 => 2u16,
        _ => 1,
    };
    let alignment = match ty {
        SpriteType::Hd => 8,
        SpriteType::Hd2 => 4,
        SpriteType::Sd => 4,
    };
    let align = |val: u16| {
        ((val - 1) | (alignment - 1)) + 1
    };

    let mut changes = layout_result.encode(0, &formats, 1);
    for f in &mut changes.frames {
        f.tex_x *= scale_mul;
        f.tex_y *= scale_mul;
        f.x_off *= scale_mul as i16;
        f.y_off *= scale_mul as i16;
        f.width = align(f.width) * scale_mul;
        f.height = align(f.height) * scale_mul;
    }
    for ty in &frame_info.frame_types {
        for f in ty.first_frame..ty.last_frame + 1 {
            if let Some(f) = changes.frames.get_mut(f as usize) {
                f.unknown = ty.frame_type;
            }
        }
    }
    files.set_tex_changes(sprite, ty, changes);
    if let Some((hd2, _dir)) = hd2_frame_info {
        let mut changes = layout_result.encode(layer_count, &formats, 2);
        for f in &mut changes.frames {
            // The coordinates are already 2x otherwise
            f.width = align(f.width);
            f.height = align(f.height);
        }
        for ty in &hd2.frame_types {
            for f in ty.first_frame..ty.last_frame + 1 {
                if let Some(f) = changes.frames.get_mut(f as usize) {
                    f.unknown = ty.frame_type;
                }
            }
        }
        files.set_tex_changes(sprite, SpriteType::Hd2, changes);
    }
    Ok(())
}

fn rgba_bounding_box(data: &[u8], width: u32, height: u32) -> Bounded {
    assert_eq!(data.len(), 4 * (width * height) as usize);
    let top = match data.chunks(width as usize * 4)
        .position(|x| !x.chunks(4).all(|x| x[3] == 0))
    {
        Some(s) => s as u32,
        None => return Bounded {
            data: vec![],
            coords: anim_encoder::FrameCoords {
                x_offset: 0,
                y_offset: 0,
                width: 0,
                height: 0,
            },
        },
    };
    let bottom = height - data.chunks(width as usize * 4).rev()
        .position(|x| !x.chunks(4).all(|x| x[3] == 0)).unwrap() as u32;
    let left = (0..width)
        .find(|x| !(top..bottom).all(|y| data[(y * width + x) as usize * 4 + 3] == 0))
        .unwrap();
    let right = 1 + (0..width).rev()
        .find(|x| !(top..bottom).all(|y| data[(y * width + x) as usize * 4 + 3] == 0))
        .unwrap();
    let out_width = right - left;
    let out_height = bottom - top;
    let mut out = vec![0; (out_width * out_height) as usize * 4];
    let mut out_pos = 0;
    let mut in_pos = (top * width + left) as usize * 4;
    for _ in 0..out_height {
        (&mut out[out_pos..out_pos + out_width as usize * 4])
            .copy_from_slice(&data[in_pos..in_pos + out_width as usize * 4]);
        out_pos += out_width as usize * 4;
        in_pos += width as usize * 4;
    }
    Bounded {
        data: out,
        coords: anim_encoder::FrameCoords {
            x_offset: left as i32,
            y_offset: top as i32,
            width: out_width,
            height: out_height,
        },
    }
}

#[test]
fn test_rgba_bounding_box() {
    let data = vec![1; 40 * 70 * 4];
    let result = rgba_bounding_box(&data, 40, 70);
    assert_eq!(result.coords.x_offset, 0);
    assert_eq!(result.coords.y_offset, 0);
    assert_eq!(result.coords.width, 40);
    assert_eq!(result.coords.height, 70);

    let mut data = vec![0; 40 * 70 * 4];
    data[4 * (32 * 40 + 35) + 0] = 5;
    data[4 * (32 * 40 + 35) + 1] = 6;
    data[4 * (32 * 40 + 35) + 2] = 7;
    data[4 * (32 * 40 + 35) + 3] = 8;
    let result = rgba_bounding_box(&data, 40, 70);
    assert_eq!(result.coords.x_offset, 35);
    assert_eq!(result.coords.y_offset, 32);
    assert_eq!(result.coords.width, 1);
    assert_eq!(result.coords.height, 1);
    assert_eq!(result.data, &[5, 6, 7, 8]);

    data[4 * (2 * 40 + 5) + 0] = 50;
    data[4 * (2 * 40 + 5) + 1] = 60;
    data[4 * (2 * 40 + 5) + 2] = 70;
    data[4 * (2 * 40 + 5) + 3] = 80;
    let result = rgba_bounding_box(&data, 40, 70);
    assert_eq!(result.coords.x_offset, 5);
    assert_eq!(result.coords.y_offset, 2);
    assert_eq!(result.coords.width, 31);
    assert_eq!(result.coords.height, 31);
    let out_len = result.data.len();
    assert_eq!(&result.data[..4], &[50, 60, 70, 80]);
    assert_eq!(&result.data[out_len - 4..], &[5, 6, 7, 8]);
    for &x in &result.data[4..out_len - 4] {
        assert_eq!(x, 0);
    }
}

struct Bounded {
    data: Vec<u8>,
    coords: anim_encoder::FrameCoords,
}
