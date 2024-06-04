use std::cell::RefCell;
use std::convert::TryFrom;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, Weak};

use anyhow::Context;
use image::{GenericImageView, RgbaImage};
use rayon::prelude::*;

use crate::anim;
use crate::anim_encoder;
use crate::ddsgrp;
use crate::files;
use crate::frame_info::{self, FrameInfo};
use crate::grp::GrpWriter;
use crate::grp_decode;
use crate::normal_encoding;
use crate::{SpriteType, Error};

// If `format` isn't set it is assumed to be paletted, in which case the first image must
// have one in it.
pub fn import_frames_grp<F: Fn(f32) + Sync>(
    files: &mut files::Files,
    frame_info: &FrameInfo,
    dir: &Path,
    frame_scale: f32,
    format: Option<anim::TextureFormat>,
    sprite: usize,
    scale: u8,
    // For writing grp for SD ddsgrp cmdicon imports
    linked_grp_path: Option<&Path>,
    report_progress: F,
) -> Result<(), Error> {
    let image_data_cache = Mutex::new(ImageDataCache::new());
    let tls = thread_local::ThreadLocal::new();
    let step = AtomicUsize::new(1);
    let step_count = frame_info.frame_count as f32;
    let needs_palette = format.is_none();
    // Palette uses just palette of the first frame.
    let palette = Mutex::new(None);
    let palette_set = AtomicBool::new(false);

    let write_grp = linked_grp_path.is_some();
    let mut frames = (0..frame_info.frame_count).into_par_iter()
        .map(|i| {
            let tls_cache = tls.get_or(|| RefCell::new(TlsImageDataCache::default()));
            let mut tls_cache = tls_cache.borrow_mut();
            let mut frame_reader =
                FrameReader::new(dir, &image_data_cache, &mut tls_cache, needs_palette);

            let (data, width, height, frame_palette) =
                frame_reader.read_frame(frame_info, 0, i, frame_scale)?;
            let uncompressed = match write_grp {
                // Not too worried about this clone, when writing SD grps the frames are small.
                true => Some(data.clone()),
                false => None,
            };
            let data = if let Some(format) = format {
                anim_encoder::encode(&data, width, height, format)
            } else {
                data
            };
            if needs_palette && palette_set.swap(true, Ordering::Relaxed) == false {
                *palette.lock().unwrap() = Some(
                    (*frame_palette
                        .ok_or_else(|| anyhow!("Palette is required"))?)
                        .clone()
                );
            }
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
            Ok((i, (frame, data), uncompressed))
        })
        .collect::<Result<Vec<_>, Error>>()?;
    frames.sort_by_key(|x| x.0);

    // Write cmdicons.grp for SD cmdicons etc
    if let Some(linked_grp_path) = linked_grp_path {
        let frame_count = u16::try_from(frame_info.frame_count).context("Too many frames")?;
        let (width, height) = frames
            .iter()
            .map(|(_, (frame, _encoded), _rgba)| (frame.width, frame.height))
            .fold((0, 0), |old, new| (old.0.max(new.0), old.1.max(new.1)));

        let mut writer = GrpWriter::new(frame_count, width, height);
        for &(frame_n, (ref frame, _), ref data) in frames.iter() {
            let data = data.as_deref().expect("Should always be Some");
            // Note: x/y are 0 as ddsgrp don't store that per-frame
            // SC:R doesn't seem to need them, at least with cmdicons.
            let frame_w = u8::try_from(frame.width)
                .with_context(|| format!("Frame {} is too wide", frame_n))?;
            let frame_h = u8::try_from(frame.height)
                .with_context(|| format!("Frame {} is too tall", frame_n))?;
            writer.add_frame(frame_n as u16, 0, 0, frame_w, frame_h, data);
        }
        let grp = writer.finish();
        std::fs::write(linked_grp_path, &grp)
            .with_context(|| format!("Couldn't write {}", linked_grp_path.display()))?;
    }

    let frames = frames.into_iter().map(|x| x.1).collect();
    let palette = palette.into_inner().unwrap();
    files.set_grp_changes(sprite, frames, scale, palette);
    Ok(())
}

struct FrameReader<'a> {
    dir: &'a Path,
    image_data_cache: &'a Mutex<ImageDataCache>,
    tls_cache: &'a mut TlsImageDataCache,
    use_palette: bool,
}

// ImageDataCache is the "root" object, but it won't keep anything alive by itself.
// Instead load_png returns TlsImageDataCache which keeps the all loaded pngs,
// including but not limited to what the thread actually asked, alive.
//
// This won't work well if each frame read is done in a new thread (thread_local crate
// won't free the TLS data on thread end, ending up everything getting buffered),
// but rayon's worker threads should behave well.

/// Paletted images can't be scaled without de/repaletting them.
/// Use raw data for palette and image::RgbaImage otherwise
enum ImageData {
    Paletted(Vec<u8>, u32, u32, Arc<Vec<u8>>),
    Image(RgbaImage),
}

impl ImageData {
    pub fn width(&self) -> u32 {
        match *self {
            ImageData::Paletted(_, width, _, _) => width,
            ImageData::Image(ref image) => image.width(),
        }
    }

    pub fn height(&self) -> u32 {
        match *self {
            ImageData::Paletted(_, _, height, _) => height,
            ImageData::Image(ref image) => image.height(),
        }
    }
}

struct ImageDataCache {
    /// (data, path, last cache hit)
    loaded: Vec<Weak<(ImageData, PathBuf, AtomicUsize)>>,
    load_count: usize,
}

#[derive(Default)]
struct TlsImageDataCache {
    /// (data, path, last cache hit)
    loaded: Vec<Arc<(ImageData, PathBuf, AtomicUsize)>>,
}

impl TlsImageDataCache {
    fn get(&self, path: &Path) -> Option<&ImageData> {
        match self.loaded.iter().find(|x| x.1 == path) {
            Some(s) => {
                // This isn't super sensible but whatever, approximates somewhat how
                // recently this got used
                s.2.fetch_add(1, Ordering::Relaxed);
                Some(&s.0)
            },
            None => None,
        }
    }
}

impl ImageDataCache {
    fn new() -> ImageDataCache {
        ImageDataCache {
            loaded: Vec::new(),
            load_count: 0,
        }
    }

    fn load_png(
        &mut self,
        filename: &Path,
        paletted: bool,
    ) -> Result<TlsImageDataCache, Error> {
        let mut strong_loaded =
            self.loaded.iter().filter_map(|x| x.upgrade()).collect::<Vec<_>>();
        // Free any memory that was droped in all TLS caches but had weak pointers here
        self.loaded.retain(|x| x.strong_count() != 0);
        if strong_loaded.len() > 10 && self.load_count > 20 {
            // Don't include ones with low hitcounts to keep memory usage lower
            // (Often includes none of them then when using single image per frame mode)
            // Quite hackfix tbh though, won't help with single images, should instead
            // lazily bound the data on use.
            let last_keep = self.load_count - 20;
            strong_loaded.retain(|x| x.2.load(Ordering::Relaxed) < last_keep && x.1 != filename);
        }
        let matching = strong_loaded.iter().find(|x| x.1 == filename);
        self.load_count += 1;
        if let Some(matching) = matching {
            matching.2.store(self.load_count, Ordering::Relaxed);
        } else {
            let file = File::open(&filename)
                .with_context(|| format!("Unable to open {}", filename.to_string_lossy()))?;
            let image = load_png(BufReader::new(file), paletted)
                .with_context(|| format!("Unable to load PNG {}", filename.to_string_lossy()))?;
            let arc = Arc::new((image, filename.into(), AtomicUsize::new(self.load_count)));
            self.loaded.push(Arc::downgrade(&arc));
            strong_loaded.push(arc);
        }
        Ok(TlsImageDataCache {
            loaded: strong_loaded,
        })
    }
}

fn load_png<R: Read>(reader: BufReader<R>, paletted: bool) -> Result<ImageData, Error> {
    let mut decoder = png::Decoder::new(reader);
    if !paletted {
        // If we don't want palette, expand it to RGB
        decoder.set_transformations(png::Transformations::EXPAND);
    } else {
        // Explicitly no transformations; older version of PNG had EXPAND,
        // currently not but going to keep this.
        decoder.set_transformations(png::Transformations::IDENTITY);
    }
    let mut reader = decoder.read_info()?;
    let mut buf = vec![0; reader.output_buffer_size()];
    reader.next_frame(&mut buf)?;
    if paletted {
        let info = reader.info();
        let palette = match info.palette {
            Some(ref s) => Arc::new(rgb_to_rgb0(&s)),
            None => return Err(anyhow!("Imported image must be paletted")),
        };
        Ok(ImageData::Paletted(buf, info.width, info.height, palette))
    } else {
        let info = reader.info();
        let rgba = arbitrary_png_to_rgba(buf, &info)?;
        let image = image::ImageBuffer::from_raw(info.width, info.height, rgba)
            .ok_or_else(|| anyhow!("Couldn't create image from raw bytes"))?;
        Ok(ImageData::Image(image))
    }
}

fn rgb_to_rgb0(input: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(input.len() / 3 * 4);
    for x in input.chunks_exact(3) {
        out.extend_from_slice(&[x[0], x[1], x[2], 0]);
    }
    out
}

fn arbitrary_png_to_rgba(buf: Vec<u8>, info: &png::Info) -> Result<Vec<u8>, Error> {
    if info.bit_depth != png::BitDepth::Eight {
        return Err(anyhow!("Bit depth {:?} not supported", info.bit_depth));
    }
    match info.color_type {
        png::ColorType::Rgba => Ok(buf),
        png::ColorType::Rgb => {
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
        use_palette: bool,
    ) -> FrameReader<'a> {
        FrameReader {
            dir,
            image_data_cache,
            tls_cache,
            use_palette,
        }
    }

    fn read_frame(
        &mut self,
        frame_info: &FrameInfo,
        layer: u32,
        frame: u32,
        frame_scale: f32,
    ) -> Result<(Vec<u8>, u32, u32, Option<Arc<Vec<u8>>>), Error> {
        self.read_frame_sublayer(frame_info, layer, 0, frame, frame_scale)
    }

    fn read_ao_depth_merged_frame(
        &mut self,
        frame_info: &FrameInfo,
        layer: u32,
        frame: u32,
        frame_scale: f32,
    ) -> Result<(Vec<u8>, u32, u32, Option<Arc<Vec<u8>>>), Error> {
        let (mut data, width, height, palette) =
            self.read_frame_sublayer(frame_info, layer, 0, frame, frame_scale)
                .context("Failed to read AO layer")?;
        let (depth_data, depth_width, depth_height, depth_palette) =
            self.read_frame_sublayer(frame_info, layer, 1, frame, frame_scale)
                .context("Failed to read depth layer")?;
        if palette.is_some() || depth_palette.is_some() {
            return Err(anyhow!("Cannot merge ao_depth with paletted images"));
        }
        if width != depth_width || height != depth_height || data.len() != depth_data.len() {
            return Err(anyhow!("Cannot merge ao_depth with images of different size"));
        }
        for (ao_pixel, depth_pixel) in data.chunks_exact_mut(4).zip(depth_data.chunks_exact(4)) {
            let ao = ao_pixel[0];
            let depth = depth_pixel[0];
            ao_pixel[0] = 0;
            ao_pixel[1] = ao;
            ao_pixel[2] = 0;
            ao_pixel[3] = depth;
        }
        Ok((data, width, height, palette))
    }

    fn read_frame_sublayer(
        &mut self,
        frame_info: &FrameInfo,
        layer_id: u32,
        sublayer: u32,
        frame: u32,
        frame_scale: f32,
    ) -> Result<(Vec<u8>, u32, u32, Option<Arc<Vec<u8>>>), Error> {
        let layer = frame_info.layers.iter()
            .find(|x| x.id == layer_id && x.sub_id == sublayer)
            .ok_or_else(|| anyhow!("No layer {}:{}", layer_id, sublayer))?;
        let layer_prefix = &layer.filename_prefix;
        let multi_frame_image = frame_info.multi_frame_images.iter()
            .filter(|x| x.layer == layer_id && x.sublayer == sublayer)
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
                *self.tls_cache = main_cache.load_png(&filename, self.use_palette)?;
                self.tls_cache.get(&filename)
                    .ok_or_else(|| {
                        anyhow!("{} didn't load properly to cache???", filename.display())
                    })?
            }
        };
        let (x, y, width, height) = if let Some(multi_frame) = multi_frame_image {
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
            (x, y, width, height)
        } else {
            (0, 0, image.width(), image.height())
        };

        match *image {
            ImageData::Paletted(ref data, in_width, _in_height, ref palette) => {
                if frame_scale != 1.0 {
                    return Err(anyhow!("Cannot scale when encoding paletted"));
                }
                let in_width = usize::try_from(in_width)?;
                let width_usz = usize::try_from(width)?;
                let height_usz = usize::try_from(height)?;
                let size = width_usz.checked_mul(height_usz)
                    .ok_or_else(|| anyhow!("Too large image"))?;
                let mut buffer = Vec::with_capacity(size);
                let x = usize::try_from(x)?;
                let y = usize::try_from(y)?;
                for y in (y..).take(height_usz) {
                    let start = y.checked_mul(in_width)
                        .and_then(|r| r.checked_add(x))
                        .ok_or_else(|| anyhow!("Internal error {}:{}", file!(), line!()))?;
                    let line = data.get(start..)
                        .and_then(|x| x.get(..width_usz))
                        .ok_or_else(|| anyhow!("Internal error {}:{}", file!(), line!()))?;
                    buffer.extend_from_slice(line);
                }
                Ok((buffer, width, height, Some(palette.clone())))
            }
            ImageData::Image(ref image) => {
                let frame_view = image.view(x, y, width, height).to_image();

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
                let mut data = buffer.into_raw();
                if layer.encoding == frame_info::LayerEncoding::Normal {
                    for pixel in data.chunks_exact_mut(4) {
                        let (x, y) = normal_encoding::encode_normal(pixel[0], pixel[1], pixel[2]);
                        pixel[0] = x;
                        pixel[1] = 0;
                        pixel[2] = 0;
                        pixel[3] = y;
                    }
                }
                Ok((data, width, height, None))
            }
        }
    }
}

fn layer_has_alpha_bounding_box(name: &str) -> bool {
    name == "diffuse" || name == "bright"
}

struct LayerAddCtx<'a, F: Fn(f32) + Sync> {
    // Will be updated as layers are added to be max w/h that a frame will use
    // (For GRP creation)
    image_width: u32,
    image_height: u32,

    // Used for determining of size alpha data layers since their bounds
    // cannot be determined just by looking for alpha == 0.
    // Technically could check for rgba == 0, but normal and depth have non-zero
    // "neutral value". While exported images fill the out of bounds data with zeroes,
    // the user may want to use paint bucket tool to just fill all of the canvas with the
    // neutral color, so don't try to determine image bounds at all for those
    // Collected as max coords of non-alpha data layers.
    max_frame_bounds: Vec<Option<Bounds>>,

    step: AtomicUsize,

    // Immutable input params
    step_count: f32,
    frame_info: &'a FrameInfo,
    layout: &'a mut anim_encoder::Layout,
    first_layer: usize,
    dir: &'a Path,
    frame_scale: f32,
    scale: u32,
    report_progress: &'a F,
}

impl<'a, F: Fn(f32) + Sync> LayerAddCtx<'a, F> {
    fn add_layer(
        &mut self,
        i: u32,
        alpha_bounding_box: bool,
        merge_ao_depth: bool,
    ) -> Result<(), Error> {
        let frame_info = self.frame_info;
        let frame_scale = self.frame_scale;
        let scale = self.scale;
        let dir = self.dir;
        let report_progress = self.report_progress;
        let step = &self.step;
        let step_count = self.step_count;

        let image_data_cache = Mutex::new(ImageDataCache::new());
        let tls = thread_local::ThreadLocal::new();
        let layer = self.first_layer + i as usize;
        let frames = (0..frame_info.frame_count).into_par_iter()
            .map(|f| {
                let tls_cache = tls.get_or(|| RefCell::new(TlsImageDataCache::default()));
                let mut tls_cache = tls_cache.borrow_mut();
                let mut frame_reader =
                    FrameReader::new(dir, &image_data_cache, &mut tls_cache, false);

                let (data, width, height, _palette) = if merge_ao_depth {
                    frame_reader.read_ao_depth_merged_frame(frame_info, i, f, frame_scale)
                } else {
                    frame_reader.read_frame(frame_info, i, f, frame_scale)
                }.with_context(|| format!("Reading frame #{}", f))?;
                let step = step.fetch_add(1, Ordering::Relaxed);
                report_progress((step as f32) / step_count);
                Ok((f, data, width, height))
            })
            .collect::<Result<Vec<_>, Error>>()?;
        for (f, data, width, height) in frames {
            self.image_width = self.image_width.max(width);
            self.image_height = self.image_height.max(height);
            let bounds = if alpha_bounding_box {
                let bounds = rgba_bounds(&data, width, height);
                if bounds.right > bounds.left && bounds.bottom > bounds.top {
                    while self.max_frame_bounds.len() <= f as usize {
                        self.max_frame_bounds.push(None);
                    }
                    let old = self.max_frame_bounds.get(f as usize)
                        .cloned()
                        .flatten()
                        .unwrap_or_else(|| Bounds {
                            left: u32::MAX,
                            top: u32::MAX,
                            right: 0,
                            bottom: 0,
                        });
                    let new = Bounds {
                        left: old.left.min(bounds.left),
                        top: old.top.min(bounds.top),
                        right: old.right.max(bounds.right),
                        bottom: old.bottom.max(bounds.bottom),
                    };
                    self.max_frame_bounds[f as usize] = Some(new);
                }
                bounds
            } else {
                let mut bounds = self.max_frame_bounds.get(f as usize)
                    .cloned()
                    .flatten()
                    .filter(|b| b.left < width && b.top < height)
                    .unwrap_or_else(|| Bounds {
                        left: 0,
                        top: 0,
                        right: 0,
                        bottom: 0,
                    });
                if bounds.right >= width {
                    bounds.right = width;
                }
                if bounds.bottom >= height {
                    bounds.bottom = height;
                }
                bounds
            };

            let mut bounded = bound_data(&data, width, height, &bounds);
            let x_offset = (frame_info.offset_x as f32 * frame_scale) as i32;
            let y_offset = (frame_info.offset_y as f32 * frame_scale) as i32;
            bounded.coords.x_offset =
                bounded.coords.x_offset.saturating_add(x_offset) * scale as i32;
            bounded.coords.y_offset =
                bounded.coords.y_offset.saturating_add(y_offset) * scale as i32;
            bounded.coords.width *= scale;
            bounded.coords.height *= scale;
            self.layout.add_frame(layer, f as usize, bounded.data, bounded.coords);
        }
        Ok(())
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
        // Try to minimize amount of memory used by keeping PNGs loaded,
        // so never parallelize layers (as they are expected to always be
        // separate files)
        let mut ctx = LayerAddCtx {
            step: AtomicUsize::new(1),
            step_count: frame_info.layers.len() as f32 * frame_info.frame_count as f32,
            image_width: 0,
            image_height: 0,
            max_frame_bounds: Vec::with_capacity(frame_info.frame_count as usize),
            layout,
            frame_info,
            first_layer,
            dir,
            frame_scale,
            scale,
            report_progress: &report_progress,
        };
        fn is_merge_ao_depth(
            layer: &frame_info::Layer,
        ) -> bool {
            layer.name == "ao_depth" && layer.encoding == frame_info::LayerEncoding::SingleChannel
        }

        for layer in &frame_info.layers {
            let alpha_used = layer_has_alpha_bounding_box(&layer.name);
            if layer.sub_id == 0 && alpha_used {
                let merge_ao_depth = is_merge_ao_depth(layer);
                ctx.add_layer(layer.id, true, merge_ao_depth)?;
            }
        }
        for layer in &frame_info.layers {
            let alpha_used = layer_has_alpha_bounding_box(&layer.name);
            if layer.sub_id == 0 && !alpha_used {
                let merge_ao_depth = is_merge_ao_depth(layer);
                ctx.add_layer(layer.id, false, merge_ao_depth)?;
            }
        }

        Ok((ctx.image_width, ctx.image_height))
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
        let width = u16::try_from(width)
            .context("Sprite dimensions too large")?;
        let height = u16::try_from(height)
            .context("Sprite dimensions too large")?;
        let grp = layout.write_grp(width, height)
            .context("Couldn't write GRP")?;
        std::fs::write(grp_path, &grp)
            .with_context(|| format!("Couldn't write {}", grp_path.display()))?;
    }
    let layout_result = layout.layout();

    let formats = formats.iter().enumerate().map(|(i, &f)| {
        if frame_info.layers.iter().any(|x| x.id as usize == i) {
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

    let mut changes = layout_result.encode(0, &formats, 1);
    let frame_count = changes.frames.len() as u32;
    for ty in &frame_info.frame_types {
        for f in ty.first_frame..ty.last_frame + 1 {
            if let Some(f) = changes.frames.get_mut(f as usize) {
                f.unknown = ty.frame_type;
            }
        }
    }
    // width and height are already scaled by frame_scale
    let wh_scaled = (width as u16, height as u16);
    files.set_tex_changes(sprite, ty, changes, wh_scaled);
    if let Some((hd2, _dir)) = hd2_frame_info {
        let mut changes = layout_result.encode(layer_count, &formats, 2);
        for ty in &hd2.frame_types {
            for f in ty.first_frame..ty.last_frame + 1 {
                if let Some(f) = changes.frames.get_mut(f as usize) {
                    f.unknown = ty.frame_type;
                }
            }
        }
        files.set_tex_changes(sprite, SpriteType::Hd2, changes, wh_scaled);
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

pub fn import_grp_to_anim<F: Fn(f32) + Sync>(
    files: &mut files::Files,
    sprite: usize,
    grp: &[u8],
    palette: &[u8],
    format: anim::TextureFormat,
    use_teamcolor: bool,
    report_progress: F,
) -> Result<(), Error> {
    let variants = [
        (SpriteType::Sd, 1),
        (SpriteType::Hd2, 2),
        (SpriteType::Hd, 4),
    ];

    let frame_count = grp_decode::frame_count(grp)?;
    let (width, height) = grp_decode::width_height(grp)?;
    let mut layout = anim_encoder::Layout::new();
    let formats = [
        Some(format),
        Some(anim::TextureFormat::Monochrome).filter(|_| use_teamcolor),
    ];
    let step = AtomicUsize::new(1);
    let frames = (0..frame_count).into_par_iter()
        .map(|frame| {
            let (frame_data, teamcolor) = if use_teamcolor {
                let (frame_data, teamcolor) =
                    grp_decode::decode_grp_to_rgba_and_teamcolor(grp, frame, palette)
                        .with_context(|| format!("Invalid GRP, cannot decode frame {}", frame))?;
                (frame_data, Some(teamcolor))
            } else {
                let frame_data = grp_decode::decode_grp_to_rgba(grp, frame, palette)
                    .with_context(|| format!("Invalid GRP, cannot decode frame {}", frame))?;
                (frame_data, None)
            };

            let mut entries = Vec::new();
            for (i, &(_sprite_type, scale)) in variants.iter().enumerate() {
                let reverse_scale = 4 / scale;
                let data =
                    scale_rgba(&frame_data.data, frame_data.width, frame_data.height, scale);
                let width = frame_data.width * scale;
                let height = frame_data.height * scale;
                let bounds = rgba_bounds(&data, width, height);
                let mut bounded = bound_data(&data, width, height, &bounds);
                bounded.coords.x_offset *= reverse_scale as i32;
                bounded.coords.y_offset *= reverse_scale as i32;
                bounded.coords.width *= reverse_scale;
                bounded.coords.height *= reverse_scale;
                let layer = i * 2;
                entries.push((layer, frame as usize, bounded.data, bounded.coords));
                if let Some(ref teamcolor) = teamcolor {
                    let data = scale_rgba(&teamcolor, frame_data.width, frame_data.height, scale);
                    let mut bounded = bound_data(&data, width, height, &bounds);
                    bounded.coords.x_offset *= reverse_scale as i32;
                    bounded.coords.y_offset *= reverse_scale as i32;
                    bounded.coords.width *= reverse_scale;
                    bounded.coords.height *= reverse_scale;
                    entries.push((layer + 1, frame as usize, bounded.data, bounded.coords));
                }
            }
            let step = step.fetch_add(1, Ordering::Relaxed);
            report_progress(step as f32 / frame_count as f32);
            Ok(entries)
        })
        .collect::<Result<Vec<_>, Error>>()?;
    for (layer, frame, data, coords) in frames.into_iter().flatten() {
        layout.add_frame(layer, frame, data, coords);
    }
    let layout_result = layout.layout();
    for (i, &(sprite_type, scale)) in variants.iter().enumerate() {
        let reverse_scale = 4 / scale;
        let mut changes = layout_result.encode(i * 2, &formats, reverse_scale);
        if sprite_type == SpriteType::Sd {
            for f in &mut changes.frames {
                f.tex_x /= 4;
                f.tex_y /= 4;
                f.x_off /= 4;
                f.y_off /= 4;
                f.width /= 4;
                f.height /= 4;
            }
        }
        // Frame type to 0
        for f in 0..frame_count {
            if let Some(f) = changes.frames.get_mut(f as usize) {
                f.unknown = 0;
            }
        }
        // Fix tex change layers from diffuse/teamcolor to match
        // layer_names
        let ordered_textures = {
            let file = files.file(sprite, sprite_type)
                .ok()
                .flatten()
                .ok_or_else(|| anyhow!("Can't access Sprite {}/{:?}", sprite, sprite_type))?;
            let layer_names = file.layer_names();
            layer_names.iter().map(|name| {
                if name == "diffuse" {
                    changes.textures[0].take()
                } else if name == "teamcolor" {
                    changes.textures[1].take()
                } else {
                    None
                }
            }).collect()
        };
        changes.textures = ordered_textures;
        let wh_scaled = (
            width.saturating_mul(scale as u16),
            height.saturating_mul(scale as u16),
        );
        files.set_tex_changes(sprite, sprite_type, changes, wh_scaled);
    }

    // Resize lit frames if lit exists
    if let Some(lit) = files.lit() {
        if let Some(sprite) = lit.sprite_mut(sprite) {
            sprite.set_frame_count(u32::from(frame_count));
        }
    }

    Ok(())
}

/// Uses format: None for paletted
pub fn import_grp_to_ddsgrp<F: Fn(f32) + Sync>(
    files: &mut files::Files,
    sprite: usize,
    grp: &[u8],
    palette: &[u8],
    format: Option<anim::TextureFormat>,
    scale: u8,
    report_progress: F,
) -> Result<(), Error> {
    if palette.len() != 0x400 {
        return Err(anyhow!("Invalid palette"));
    }
    let frame_count = grp_decode::frame_count(grp)?;

    let frames = (0..frame_count).map(|frame| {
        let (data, width, height) = if let Some(format) = format {
            let result = grp_decode::decode_grp_to_rgba(grp, frame, palette)
                .with_context(|| format!("Invalid GRP, cannot decode frame {}", frame))?;
            let width = result.width;
            let height = result.height;
            let data = anim_encoder::encode(&result.data, width, height, format);
            (data, width, height)
        } else {
            let result = grp_decode::decode_grp_to_paletted(grp, frame)
                .with_context(|| format!("Invalid GRP, cannot decode frame {}", frame))?;
            (result.data, result.width, result.height)
        };
        report_progress(frame as f32 / frame_count as f32);
        let frame = ddsgrp::Frame {
            unknown: 0,
            width: u16::try_from(width)
                .map_err(|_| anyhow!("Frame {} width too large", frame))?,
            height: u16::try_from(height)
                .map_err(|_| anyhow!("Frame {} width too large", frame))?,
            size: data.len() as u32,
            offset: !0,
        };
        Ok((frame, data))
    }).collect::<Result<Vec<_>, Error>>()?;

    let palette = if format.is_none() {
        Some(palette.into())
    } else {
        None
    };
    files.set_grp_changes(sprite, frames, scale, palette);

    Ok(())
}

fn scale_rgba(input: &[u8], width: u32, height: u32, scale: u32) -> Vec<u8> {
    let vec = input.into();
    if scale == 1 {
        return vec;
    }
    let image = match image::RgbaImage::from_raw(width, height, vec) {
        Some(s) => s,
        None => panic!("Cannot create image::ImageBuffer"),
    };
    let result = image::imageops::resize(
        &image,
        width * scale,
        height * scale,
        image::imageops::FilterType::Lanczos3,
    );
    result.into_raw()
}

fn rgba_bounds(data: &[u8], width: u32, height: u32) -> Bounds {
    assert_eq!(data.len(), 4 * (width * height) as usize);
    let top = match data.chunks(width as usize * 4)
        .position(|x| !x.chunks(4).all(|x| x[3] == 0))
    {
        Some(s) => s as u32,
        None => return Bounds {
            left: 0,
            top: 0,
            right: 0,
            bottom: 0,
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
    Bounds {
        top,
        bottom,
        left,
        right,
    }
}

fn bound_data(data: &[u8], width: u32, _height: u32, bounds: &Bounds) -> Bounded {
    let Bounds {
        left,
        right,
        top,
        bottom,
    } = *bounds;
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

#[cfg(test)]
fn rgba_bounding_box(data: &[u8], width: u32, height: u32) -> Bounded {
    let bounds = rgba_bounds(data, width, height);
    bound_data(data, width, height, &bounds)
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

#[test]
fn test_empty_rgba_bounding_box() {
    let data = vec![0; 40 * 70 * 4];
    let result = rgba_bounding_box(&data, 40, 70);
    assert_eq!(result.coords.x_offset, 0);
    assert_eq!(result.coords.y_offset, 0);
    assert_eq!(result.coords.width, 0);
    assert_eq!(result.coords.height, 0);
    assert_eq!(result.data.len(), 0);
}

struct Bounded {
    data: Vec<u8>,
    coords: anim_encoder::FrameCoords,
}

#[derive(Copy, Clone, Debug)]
struct Bounds {
    left: u32,
    top: u32,
    right: u32,
    bottom: u32,
}
