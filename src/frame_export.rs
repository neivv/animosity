use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs::File;
use std::io::BufWriter;
use std::path::{Path};

use anyhow::Context;

use crate::anim::{Frame, RgbaTexture};
use crate::files;
use crate::frame_info::{self, FrameInfo, FrameType};
use crate::{SpriteType, Error};

// Won't export layers with None prefix,
// framedef_file is joined to path, as are the image names
pub fn export_frames<F: Fn(f32)>(
    file: &files::File<'_>,
    ty: SpriteType,
    path: &Path,
    framedef_file: &Path,
    layer_prefixes: &[Option<String>],
    single_image: bool,
    report_progress: F,
) -> Result<(), Error> {
    if !path.is_dir() {
        return Err(anyhow!("{} is not a directory", path.to_string_lossy()));
    }

    let scale_div = match ty {
        SpriteType::Hd2 => 2u32,
        _ => 1u32,
    };

    let frames = file.frames().ok_or_else(|| anyhow!("Unable to get frames"))?;
    let values = match file.sprite_values() {
        Some(s) => s,
        None => return Err(anyhow!("Couldn't get sprite values")),
    };
    let enum_prefixes =
        layer_prefixes.iter().enumerate().flat_map(|(i, x)| x.as_ref().map(|x| (i, x)));
    let x_base =
        frames.iter().map(|x| i32::from(x.x_off)).min().unwrap_or(0).min(0i32) / scale_div as i32;
    let y_base =
        frames.iter().map(|x| i32::from(x.y_off)).min().unwrap_or(0).min(0i32) / scale_div as i32;
    let x_max = frames.iter().map(|x| (i32::from(x.x_off) + i32::from(x.width)) / scale_div as i32)
        .max().unwrap_or(1);
    let y_max = frames.iter().map(|x| (i32::from(x.y_off) + i32::from(x.height)) / scale_div as i32)
        .max().unwrap_or(1);
    let frame_width = (x_max.max(i32::from(values.width) / scale_div as i32) - x_base) as u32;
    let frame_height = (y_max.max(i32::from(values.height) / scale_div as i32) - y_base) as u32;
    let mut multi_frame_images = Vec::new();
    let mut step = 1.0;
    let layer_count = layer_prefixes.iter().filter(|x| x.is_some()).count();
    let step_count = (layer_count * frames.len()) as f32;
    for (i, prefix) in enum_prefixes {
        let texture = file.texture(i)?;
        if single_image {
            assert!(frames.len() > 0);
            let image_width = frame_width * frames.len().min(16) as u32;
            let image_height = frame_height * (1 + frames.len() / 16) as u32;
            let path = &path.join(format!("{}.png", prefix));
            let out = File::create(path)
                .with_context(|| format!("Unable to create {}", path.to_string_lossy()))?;
            let out = BufWriter::new(out);
            let mut bytes = vec![0; (image_width * image_height * 4) as usize];
            for (n, frame) in frames.iter().enumerate() {
                let x = (n as u32 % 16) * frame_width;
                let y = (n as u32 / 16) * frame_height;
                decode_frame_to_buf(
                    &mut bytes,
                    image_width,
                    &texture,
                    &frame,
                    scale_div,
                    x,
                    y,
                    x_base,
                    y_base,
                ).with_context(|| format!("Writing frame {}", n))?;
                report_progress(step / step_count);
                step += 1.0;
            }

            let mut encoder = png::Encoder::new(out, image_width, image_height);
            encoder.set_color(png::ColorType::RGBA);
            let mut encoder = encoder.write_header()?;
            encoder.write_image_data(&bytes)?;

            multi_frame_images.push(frame_info::MultiFrameImage {
                first_frame: 0,
                frame_count: frames.len() as u32,
                layer: i as u32,
                path: path.to_str().ok_or_else(|| anyhow!("Bad PNG path"))?.into(),
                frame_width,
                frame_height,
                frame_size_overrides: HashMap::default(),
            });
        } else {
            for (n, frame) in frames.iter().enumerate() {
                let path = path.join(format!("{}_{:03}.png", prefix, n));
                write_frame(
                    &path,
                    &texture,
                    &frame,
                    scale_div,
                    frame_width,
                    frame_height,
                    x_base,
                    y_base,
                ).with_context(|| format!("Writing frame {}", n))?;
                report_progress(step / step_count);
                step += 1.0;
            }
        }
    }

    let mut frame_info_file = File::create(&path.join(framedef_file))
        .context("Can't create the frame info file")?;
    let mut frame_info = FrameInfo {
        frame_count: frames.len() as u32,
        offset_x: x_base,
        offset_y: y_base,
        layers: layer_prefixes.iter().enumerate()
            .filter_map(|(i, l)| l.as_ref().map(|x| (i, x)))
            .map(|(i, prefix)| (i as u32, prefix.clone()))
            .collect(),
        frame_types: Vec::new(),
        multi_frame_images,
    };
    let mut start = 0;
    let mut first_unk = frames.get(0).map(|x| x.unknown).unwrap_or(0);
    for (i, f) in frames.iter().enumerate() {
        if f.unknown != first_unk {
            frame_info.frame_types.push(FrameType {
                first_frame: start as u32,
                last_frame: i as u32,
                frame_type: first_unk,
            });
            start = i + 1;
            first_unk = frames.get(start).map(|x| x.unknown).unwrap_or(0);
        }
    }
    if start < frames.len() {
        frame_info.frame_types.push(FrameType {
            first_frame: start as u32,
            last_frame: frames.len() as u32 - 1,
            frame_type: first_unk,
        });
    }
    serde_json::to_writer_pretty(&mut frame_info_file, &frame_info)?;

    Ok(())
}

fn decode_frame_to_buf(
    bytes: &mut [u8],
    stride: u32,
    texture: &RgbaTexture,
    frame: &Frame,
    scale_div: u32,
    x: u32,
    y: u32,
    x_base: i32,
    y_base: i32,
) -> Result<(), Error> {
    let tex_x = frame.tex_x / scale_div as u16;
    let tex_y = frame.tex_y / scale_div as u16;
    let frame_width = u32::from(frame.width) / scale_div;
    let frame_height = u32::from(frame.height) / scale_div;

    let blank_left = u32::try_from(frame.x_off as i32 / scale_div as i32 - x_base)?;
    let blank_top = u32::try_from(frame.y_off as i32 / scale_div as i32 - y_base)?;

    let x = x + blank_left;
    let y = y + blank_top;
    let mut byte_pos = ((y * stride) + x) as usize * 4;
    for row in 0..frame_height {
        let tex_start = (
            (tex_y as u32 + row) * texture.width + tex_x as u32
        ) as usize * 4;
        let image_row = texture.data.get(tex_start..tex_start + frame_width as usize * 4);
        let image_row = match image_row {
            Some(s) => s,
            None => return Err(anyhow!("Bad frame data")),
        };
        (&mut bytes[byte_pos..byte_pos + frame_width as usize * 4]).copy_from_slice(image_row);
        byte_pos += stride as usize * 4;
    }
    Ok(())
}

fn write_frame(
    path: &Path,
    texture: &RgbaTexture,
    frame: &Frame,
    scale_div: u32,
    out_width: u32,
    out_height: u32,
    x_base: i32,
    y_base: i32,
) -> Result<(), Error> {
    let out = File::create(&path)
        .with_context(|| format!("Unable to create {}", path.to_string_lossy()))?;
    let out = BufWriter::new(out);

    let tex_x = frame.tex_x / scale_div as u16;
    let tex_y = frame.tex_y / scale_div as u16;
    let frame_width = u32::from(frame.width) / scale_div;
    let frame_height = u32::from(frame.height) / scale_div;

    let blank_left = u32::try_from(frame.x_off as i32 / scale_div as i32 - x_base)?;
    let blank_top = u32::try_from(frame.y_off as i32 / scale_div as i32 - y_base)?;
    let blank_right = out_width - (blank_left + frame_width);
    let blank_bottom = out_height - (blank_top + frame_height);

    let mut bytes = Vec::with_capacity((out_width * out_height * 4) as usize);
    bytes.extend((0..blank_top * out_width).flat_map(|_| [0, 0, 0, 0].iter().cloned()));
    for row in 0..(out_height - blank_top - blank_bottom) {
        let tex_start = (
            (tex_y as u32 + row) * texture.width + tex_x as u32
        ) as usize * 4;
        let image_row = texture.data.get(tex_start..tex_start + frame_width as usize * 4);
        let image_row = match image_row {
            Some(s) => s,
            None => return Err(anyhow!("Bad frame data")),
        };
        bytes.extend((0..blank_left).flat_map(|_| [0, 0, 0, 0].iter().cloned()));
        bytes.extend_from_slice(image_row);
        bytes.extend((0..blank_right).flat_map(|_| [0, 0, 0, 0].iter().cloned()));
    }
    bytes.extend(
        (0..blank_bottom * out_width).flat_map(|_| [0, 0, 0, 0].iter().cloned())
    );

    let mut encoder = png::Encoder::new(out, out_width, out_height);
    encoder.set_color(png::ColorType::RGBA);
    let mut encoder = encoder.write_header()?;
    encoder.write_image_data(&bytes)?;
    Ok(())
}

pub fn export_grp<F: Fn(f32)>(
    file: &files::File<'_>,
    path: &Path,
    prefix: &str,
    framedef_file: &Path,
    single_image: bool,
    report_progress: F,
) -> Result<(), Error> {
    if !path.is_dir() {
        return Err(anyhow!("{} is not a directory", path.to_string_lossy()));
    }

    let mut multi_frame_images = Vec::new();

    let layer_count = file.layer_count();
    let mut step = 1.0;
    if single_image {
        // Adding 20% for PNG encoding
        let step_count = layer_count as f32 * 1.25;
        assert!(layer_count > 0);
        let frame_width = (0..layer_count)
            .flat_map(|i| file.texture(i).ok())
            .map(|tex| tex.width)
            .max()
            .unwrap_or(0);
        let frame_height = (0..layer_count)
            .flat_map(|i| file.texture(i).ok())
            .map(|tex| tex.height)
            .max()
            .unwrap_or(0);
        let frames_per_row = match frame_width {
            x if x < 512 => 16,
            x if x < 1024 => 8,
            x if x < 2048 => 4,
            _ => 2,
        };
        let image_width = frame_width * layer_count.min(frames_per_row) as u32;
        let image_height = frame_height * (1 + layer_count / frames_per_row) as u32;
        let path = &path.join(format!("{}.png", prefix));
        let out = File::create(path)
            .with_context(|| format!("Unable to create {}", path.to_string_lossy()))?;
        let out = BufWriter::new(out);
        let mut bytes = vec![0; (image_width * image_height * 4) as usize];
        let mut frame_size_overrides = HashMap::new();
        for i in 0..layer_count {
            let texture = file.texture(i)?;
            let x = (i % frames_per_row) as u32 * frame_width;
            let y = (i / frames_per_row) as u32 * frame_height;


            let mut byte_pos = ((y * image_width) + x) as usize * 4;
            for row in 0..texture.height {
                let tex_start = (row * texture.width) as usize * 4;
                let image_row =
                    &texture.data[tex_start..tex_start + texture.width as usize * 4];
                (&mut bytes[byte_pos..byte_pos + texture.width as usize * 4])
                    .copy_from_slice(image_row);
                byte_pos += image_width as usize * 4;
            }
            if texture.width != frame_width || texture.height != frame_height {
                frame_size_overrides.insert(i as u32, (texture.width, texture.height));
            }
            report_progress(step / step_count);
            step += 1.0;
        }

        let mut encoder = png::Encoder::new(out, image_width, image_height);
        encoder.set_color(png::ColorType::RGBA);
        let mut encoder = encoder.write_header()?;
        encoder.write_image_data(&bytes)?;

        multi_frame_images.push(frame_info::MultiFrameImage {
            first_frame: 0,
            frame_count: layer_count as u32,
            layer: 0,
            path: path.to_str().ok_or_else(|| anyhow!("Bad PNG path"))?.into(),
            frame_width,
            frame_height,
            frame_size_overrides,
        });
    } else {
        let step_count = layer_count as f32;
        for i in 0..file.layer_count() {
            let texture = file.texture(i)?;

            let path = path.join(format!("{}_{:03}.png", prefix, i));
            let out = File::create(&path)
                .with_context(|| format!("Unable to create {}", path.to_string_lossy()))?;
            let out = BufWriter::new(out);

            let mut encoder = png::Encoder::new(out, texture.width, texture.height);
            encoder.set_color(png::ColorType::RGBA);
            let mut encoder = encoder.write_header()?;
            encoder.write_image_data(&texture.data)?;
            // Uh, multi-frame images which are single frame each =)
            multi_frame_images.push(frame_info::MultiFrameImage {
                first_frame: 0,
                frame_count: 1,
                layer: 0,
                path: path.to_str().ok_or_else(|| anyhow!("Bad PNG path"))?.into(),
                frame_width: texture.width,
                frame_height: texture.height,
                frame_size_overrides: HashMap::default(),
            });
            report_progress(step / step_count);
            step += 1.0;
        }
    }

    let mut frame_info_file = File::create(&path.join(framedef_file))
        .context("Can't create the frame info file")?;
    let frame_info = FrameInfo {
        frame_count: file.layer_count() as u32,
        offset_x: 0,
        offset_y: 0,
        layers: vec![(0, prefix.into())],
        frame_types: Vec::new(),
        multi_frame_images,
    };
    serde_json::to_writer_pretty(&mut frame_info_file, &frame_info)?;

    Ok(())
}
