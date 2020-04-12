use std::cell::RefCell;
use std::convert::TryFrom;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, Weak};

use anyhow::Context;
use image::{GenericImageView, RgbaImage};
use rayon::prelude::*;

use crate::anim;
use crate::anim_encoder;
use crate::ddsgrp;
use crate::files;
use crate::frame_info::{FrameInfo};
use crate::{SpriteType, Error};

pub fn import_frames_grp<F: Fn(f32) + Sync>(
    files: &mut files::Files,
    frame_info: &FrameInfo,
    dir: &Path,
    frame_scale: f32,
    format: anim::TextureFormat,
    sprite: usize,
    scale: u8,
    report_progress: F,
) -> Result<(), Error> {
    let image_data_cache = Mutex::new(ImageDataCache::default());
    let tls = thread_local::ThreadLocal::new();
    let step = AtomicUsize::new(1);
    let step_count = frame_info.frame_count as f32;

    let mut frames = (0..frame_info.frame_count).into_par_iter()
        .map(|i| {
            let tls_cache = tls.get_or(|| RefCell::new(TlsImageDataCache::default()));
            let mut tls_cache = tls_cache.borrow_mut();
            let mut frame_reader =
                FrameReader::new(dir, &image_data_cache, &mut tls_cache);

            let (data, width, height) = frame_reader.read_frame(frame_info, 0, i, frame_scale)?;
            let data = anim_encoder::encode(&data, width, height, format);
            let step = step.fetch_add(1, Ordering::Relaxed);
            report_progress((step as f32) / step_count);
            let frame = ddsgrp::Frame {
                unknown: 0,
                width: u16::try_from(width)
                    .map_err(|_| anyhow!("Frame {} width too large", i))?,
                height: u16::try_from(height)
                    .map_err(|_| anyhow!("Frame {} width too large", i))?,
                size: data.len() as u32,
                offset: !0,
            };
            Ok((i, (frame, data)))
        })
        .collect::<Result<Vec<_>, Error>>()?;
    frames.sort_by_key(|x| x.0);
    let frames = frames.into_iter().map(|x| x.1).collect();

    files.set_grp_changes(sprite, frames, scale);
    Ok(())
}

struct FrameReader<'a> {
    dir: &'a Path,
    image_data_cache: &'a Mutex<ImageDataCache>,
    tls_cache: &'a mut TlsImageDataCache,
}

// ImageDataCache is the "root" object, but it won't keep anything alive by itself.
// Instead load_png returns TlsImageDataCache which keeps the all loaded pngs,
// including but not limited to what the thread actually asked, alive.
//
// This won't work well if each frame read is done in a new thread (thread_local crate
// won't free the TLS data on thread end, ending up everything getting buffered),
// but rayon's worker threads should behave well.

#[derive(Default)]
struct ImageDataCache {
    loaded: Vec<Weak<(RgbaImage, PathBuf)>>,
}

#[derive(Default)]
struct TlsImageDataCache {
    loaded: Vec<Arc<(RgbaImage, PathBuf)>>,
}

impl TlsImageDataCache {
    fn get(&self, path: &Path) -> Option<&RgbaImage> {
        self.loaded.iter().find(|x| x.1 == path).map(|x| &x.0)
    }
}

impl ImageDataCache {
    fn load_png(&mut self, filename: &Path) -> Result<TlsImageDataCache, Error> {
        let mut strong_loaded =
            self.loaded.iter().filter_map(|x| x.upgrade()).collect::<Vec<_>>();
        // Free any memory that was droped in all TLS caches but had weak pointers here
        self.loaded.retain(|x| x.strong_count() != 0);
        if !strong_loaded.iter().any(|x| x.1 == filename) {
            let file = File::open(&filename)
                .with_context(|| format!("Unable to open {}", filename.to_string_lossy()))?;
            let image = load_png(BufReader::new(file))
                .with_context(|| format!("Unable to load PNG {}", filename.to_string_lossy()))?;
            let arc = Arc::new((image, filename.into()));
            self.loaded.push(Arc::downgrade(&arc));
            strong_loaded.push(arc);
        }
        Ok(TlsImageDataCache {
            loaded: strong_loaded,
        })
    }
}

fn load_png<R: Read>(reader: BufReader<R>) -> Result<RgbaImage, Error> {
    let decoder = png::Decoder::new(reader);
    let (info, mut reader) = decoder.read_info()?;
    let mut buf = vec![0; info.buffer_size()];
    reader.next_frame(&mut buf)?;
    let rgba = arbitrary_png_to_rgba(buf, &info)?;
    image::ImageBuffer::from_raw(info.width, info.height, rgba)
        .ok_or_else(|| anyhow!("Couldn't create image from raw bytes"))
}

fn arbitrary_png_to_rgba(buf: Vec<u8>, info: &png::OutputInfo) -> Result<Vec<u8>, Error> {
    if info.bit_depth != png::BitDepth::Eight {
        return Err(anyhow!("Bit depth {:?} not supported", info.bit_depth));
    }
    match info.color_type {
        png::ColorType::RGBA => Ok(buf),
        png::ColorType::RGB => {
            if buf.len() != (info.width * info.height) as usize * 3 {
                return Err(anyhow!("RGB buffer size isn't 3 * w * h?"));
            }
            let mut out = vec![0; (info.width * info.height) as usize * 4];
            for (out, input) in out.chunks_mut(4).zip(buf.chunks(3)) {
                out[0] = input[0];
                out[1] = input[1];
                out[2] = input[2];
                out[3] = 0xff;
            }
            Ok(out)
        }
        png::ColorType::Grayscale => {
            if buf.len() != (info.width * info.height) as usize {
                return Err(anyhow!("Grayscale buffer size isn't w * h?"));
            }
            let mut out = vec![0; (info.width * info.height) as usize * 4];
            for (out, input) in out.chunks_mut(4).zip(buf.chunks(1)) {
                out[0] = input[0];
                out[1] = input[0];
                out[2] = input[0];
                out[3] = 0xff;
            }
            Ok(out)
        }
        png::ColorType::GrayscaleAlpha => {
            if buf.len() != (info.width * info.height) as usize * 2 {
                return Err(anyhow!("Grayscale + alpha buffer size isn't 2 * w * h?"));
            }
            let mut out = vec![0; (info.width * info.height) as usize * 4];
            for (out, input) in out.chunks_mut(4).zip(buf.chunks(2)) {
                out[0] = input[0];
                out[1] = input[0];
                out[2] = input[0];
                out[3] = input[1];
            }
            Ok(out)
        }
        _ => Err(anyhow!("Unsupported color type {:?}", info.color_type)),
    }
}

impl<'a> FrameReader<'a> {
    fn new(
        dir: &'a Path,
        image_data_cache: &'a Mutex<ImageDataCache>,
        tls_cache: &'a mut TlsImageDataCache,
    ) -> FrameReader<'a> {
        FrameReader {
            dir,
            image_data_cache,
            tls_cache,
        }
    }

    fn read_frame(
        &mut self,
        frame_info: &FrameInfo,
        layer: u32,
        frame: u32,
        frame_scale: f32,
    ) -> Result<(Vec<u8>, u32, u32), Error> {
        let layer_prefix = frame_info.layers.iter()
            .find(|x| x.0 == layer)
            .map(|x| &x.1)
            .ok_or_else(|| anyhow!("No layer {}", layer))?;
        let multi_frame_image = frame_info.multi_frame_images.iter()
            .filter(|x| x.layer == layer)
            .find(|x| frame >= x.first_frame && frame < x.first_frame + x.frame_count);
        let filename = if let Some(multi_frame) = multi_frame_image {
            (&multi_frame.path).into()
        } else {
            self.dir.join(format!("{}_{:03}.png", layer_prefix, frame))
        };
        let image = match self.tls_cache.get(&filename) {
            Some(s) => s,
            None => {
                // Lock the main cache before loading PNG.
                // Reduces parallelism but prevents issues cases
                // where 8 threads load a same 300MB PNG at once
                // and 7 of them end up being discarded.
                let mut main_cache = self.image_data_cache.lock().unwrap();
                *self.tls_cache = main_cache.load_png(&filename)?;
                self.tls_cache.get(&filename)
                    .ok_or_else(|| {
                        anyhow!("{} didn't load properly to cache???", filename.display())
                    })?
            }
        };
        let frame_view = if let Some(multi_frame) = multi_frame_image {
            let index = frame - multi_frame.first_frame;
            let frames_per_row = image.width() / multi_frame.frame_width;
            if frames_per_row * multi_frame.frame_width != image.width() {
                return Err(anyhow!(
                    "Image width {} not multiple of frame width {}",
                    image.width(), multi_frame.frame_width,
                ));
            }
            let x = (index % frames_per_row) * multi_frame.frame_width;
            let y = (index / frames_per_row) * multi_frame.frame_height;
            let (width, height) = match multi_frame.frame_size_overrides.get(&frame) {
                Some(&s) => s,
                None => (multi_frame.frame_width, multi_frame.frame_height),
            };
            image.view(x, y, width, height)
        } else {
            image.view(0, 0, image.width(), image.height())
        }.to_image();

        let buffer = if frame_scale != 1.0 {
            let new_width = (frame_view.width() as f32 * frame_scale) as u32;
            let new_height = (frame_view.height() as f32 * frame_scale) as u32;
            image::imageops::resize(
                &frame_view,
                new_width,
                new_height,
                image::imageops::FilterType::Lanczos3,
            )
        } else {
            frame_view
        };
        let (width, height) = buffer.dimensions();
        let data = buffer.into_raw();
        Ok((data, width, height))
    }
}

pub fn import_frames<F: Fn(f32) + Sync>(
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
    grp_path: Option<&Path>,
    report_progress: F,
) -> Result<(), Error> {
    fn add_layers<F: Fn(f32) + Sync>(
        layout: &mut anim_encoder::Layout,
        frame_info: &FrameInfo,
        dir: &Path,
        first_layer: usize,
        frame_scale: f32,
        scale: u32,
        report_progress: F,
    ) -> Result<(u32, u32), Error> {
        let step = AtomicUsize::new(1);
        let step_count = frame_info.layers.len() as f32 * frame_info.frame_count as f32;
        let mut image_width = 0;
        let mut image_height = 0;
        // Try to minimize amount of memory used by keeping PNGs loaded,
        // so never parallelize layers (as they are expected to always be
        // separate files)
        for &(i, _) in &frame_info.layers {
            let image_data_cache = Mutex::new(ImageDataCache::default());
            let tls = thread_local::ThreadLocal::new();
            let layer = first_layer + i as usize;
            let frames = (0..frame_info.frame_count).into_par_iter()
                .map(|f| {
                    let tls_cache = tls.get_or(|| RefCell::new(TlsImageDataCache::default()));
                    let mut tls_cache = tls_cache.borrow_mut();
                    let mut frame_reader =
                        FrameReader::new(dir, &image_data_cache, &mut tls_cache);

                    let (data, width, height) =
                        frame_reader.read_frame(frame_info, i, f, frame_scale)?;
                    let step = step.fetch_add(1, Ordering::Relaxed);
                    report_progress((step as f32) / step_count);
                    Ok((f, data, width, height))
                })
                .collect::<Result<Vec<_>, Error>>()?;
            for (f, data, width, height) in frames {
                image_width = image_width.max(width);
                image_height = image_height.max(height);
                let mut bounded = rgba_bounding_box(&data, width, height);

                let x_offset = (frame_info.offset_x as f32 * frame_scale) as i32;
                let y_offset = (frame_info.offset_y as f32 * frame_scale) as i32;
                bounded.coords.x_offset =
                    bounded.coords.x_offset.saturating_add(x_offset) * scale as i32;
                bounded.coords.y_offset =
                    bounded.coords.y_offset.saturating_add(y_offset) * scale as i32;
                bounded.coords.width *= scale;
                bounded.coords.height *= scale;
                layout.add_frame(layer, f as usize, bounded.data, bounded.coords);
            }
        }
        Ok((image_width, image_height))
    }

    let hd2_frame_info = match (hd2_frame_info, hd2_dir) {
        (Some(a), Some(b)) => Some((a, b)),
        _ => None,
    };

    let layer_count = formats.len();
    let mut layout = anim_encoder::Layout::new();
    let progress_mul = match hd2_frame_info.is_some() {
        true => 0.5,
        false => 1.0,
    };
    let (width, height) = add_layers(
        &mut layout,
        frame_info,
        dir,
        0,
        frame_scale,
        1,
        |step| report_progress(step * progress_mul),
    )?;
    if let Some((hd2, dir)) = hd2_frame_info {
        add_layers(
            &mut layout,
            hd2,
            dir,
            layer_count,
            hd2_frame_scale.unwrap_or(1.0),
            2,
            |step| report_progress(0.5 + step * 0.5),
        )?;
    }
    if let Some(grp_path) = grp_path {
        if let Some(parent) = grp_path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        let mut file = std::fs::File::create(grp_path)
            .with_context(|| format!("Couldn't create {}", grp_path.display()))?;
        let width = u16::try_from(width)
            .context("Sprite dimensions too large")?;
        let height = u16::try_from(height)
            .context("Sprite dimensions too large")?;
        layout.write_grp(&mut file, width, height)
            .context("Couldn't write GRP")?;
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

    let mut changes = layout_result.encode(0, &formats, 1);
    let frame_count = changes.frames.len() as u32;
    for f in &mut changes.frames {
        f.tex_x *= scale_mul;
        f.tex_y *= scale_mul;
        f.x_off *= scale_mul as i16;
        f.y_off *= scale_mul as i16;
        f.width = f.width * scale_mul;
        f.height = f.height * scale_mul;
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
        for ty in &hd2.frame_types {
            for f in ty.first_frame..ty.last_frame + 1 {
                if let Some(f) = changes.frames.get_mut(f as usize) {
                    f.unknown = ty.frame_type;
                }
            }
        }
        files.set_tex_changes(sprite, SpriteType::Hd2, changes);
    }

    // Resize lit frames if lit exists
    if ty == SpriteType::Hd {
        if let Some(lit) = files.lit() {
            if let Some(sprite) = lit.sprite_mut(sprite) {
                sprite.set_frame_count(frame_count);
            }
        }
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
