use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs;
use std::io::{self, BufReader, BufWriter, Cursor, Seek, Write, Read};
use std::path::{Path, PathBuf};

use anyhow::Context;
use byteorder::{ByteOrder, ReadBytesExt, LE, LittleEndian};

use crate::anim::{self, SpriteValues};
use crate::anim_encoder::{self};
use crate::anim_lit::{self, Lit};
use crate::arc_error::ArcError;
use crate::ddsgrp;
use crate::{Error, SpriteType};

pub static DEFAULT_HD_LAYER_NAMES: &[&str] = &[
    "diffuse",
    "bright",
    "teamcolor",
    "emissive",
    "normal",
    "specular",
    "ao_depth",
];

pub static DEFAULT_SD_LAYER_NAMES: &[&str] = &[
    "diffuse",
    "teamcolor",
];

fn default_hd_layer_names() -> Vec<String> {
    DEFAULT_HD_LAYER_NAMES.iter().map(|&x| String::from(x)).collect()
}

fn default_sd_layer_names() -> Vec<String> {
    DEFAULT_SD_LAYER_NAMES.iter().map(|&x| String::from(x)).collect()
}

pub struct Files {
    sprites: Vec<SpriteFiles>,
    mainsd_anim: Option<(PathBuf, anim::Anim)>,
    file_root: Option<FileRoot>,
    open_files: OpenFiles,
    sd_grp_sizes: SdGrpSizes,
    edits: HashMap<(usize, SpriteType), Edit>,
    images_dat: ImagesDat,
    images_tbl: Vec<u8>,
    lit: Option<LitFile>,
    images_rel: Option<ImagesRel>,
    new_entry_count: Option<u16>,

    /// Default layer names.
    ///
    /// The layer names are part of the files and usually should be read from there,
    /// but when creating new entries we need some defaults.
    /// (Names are taken from first available sprite)
    sd_layer_names: Vec<String>,
    hd_layer_names: Vec<String>,
}

pub struct ImagesRel {
    original: Vec<u8>,
    editable: Vec<u8>,
    dirty: bool,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct ImageRelation {
    pub mode: u32,
    pub image: u16,
}

impl ImagesRel {
    fn has_changes(&self) -> bool {
        self.dirty
    }

    pub fn get(&self, image: u16) -> ImageRelation {
        self.editable.get((image as usize * 8)..)
            .and_then(|x| x.get(..8))
            .map(|x| ImageRelation {
                mode: LittleEndian::read_u32(x),
                image: LittleEndian::read_u16(&x[4..]),
            })
            .unwrap_or_else(|| ImageRelation {
                mode: 1,
                image: 0,
            })
    }

    pub fn resize(&mut self, new_size: u16) {
        for _ in (self.editable.len() / 8)..(new_size as usize) {
            self.editable.extend_from_slice(&[1, 0, 0, 0, 0, 0, 0, 0]);
        }
        self.dirty = true;
    }

    pub fn set(&mut self, image: u16, value: ImageRelation) {
        let pos = (image as usize * 8)..(image as usize * 8 + 8);
        if let Some(out) = self.editable.get_mut(pos.clone()) {
            LittleEndian::write_u32(&mut out[0..], value.mode);
            let image = if value.image == 65535 { -1 } else {value.image as i32 };
            LittleEndian::write_i32(&mut out[4..], image);
            self.dirty = if self.original.get(pos.clone()) != self.editable.get(pos) {
                true
            } else {
                self.original == self.editable
            };
        }
    }

    fn write<W: Write + Seek>(&self, out: &mut W) -> Result<(), Error> {
        out.write_all(&self.editable)?;
        Ok(())
    }

    fn reset_original(&mut self) {
        self.dirty = false;
        self.original = self.editable.clone();
    }
}

pub struct LitFile {
    /// This tracks which sprites have lighting enabled.
    /// On load only enabled sprites are ones where Lit.sprites is Some,
    /// but those sprites won't ever be changed to None so that
    /// toggling enable off/on restores the old values until program exit.
    enabled: Vec<bool>,
    editable: Lit,
    original: Lit,
    path: PathBuf,
}

impl LitFile {
    fn has_changes(&self) -> bool {
        let editable = self.editable.sprites();
        let original = self.original.sprites();
        if editable.len() != original.len() {
            return true;
        }
        assert_eq!(editable.len(), self.enabled.len());
        self.enabled.iter().enumerate()
            .any(|(i, &enabled)| {
                if enabled {
                    editable[i] != original[i]
                } else {
                    original[i].has_data()
                }
            })
    }

    fn write<W: Write + Seek>(&self, out: &mut W) -> Result<(), Error> {
        let mut new_original = self.editable.clone();
        for (i, enabled) in self.enabled.iter().cloned().enumerate() {
            if !enabled {
                new_original.remove_sprite(i);
            }
        }
        new_original.write(out)?;
        Ok(())
    }

    fn reset_original(&mut self) {
        let mut new_original = self.editable.clone();
        for (i, enabled) in self.enabled.iter().cloned().enumerate() {
            if !enabled {
                new_original.remove_sprite(i);
            }
        }
        self.original = new_original;
    }

    pub fn enable_sprite(&mut self, index: usize, frame_count: u32) -> &mut anim_lit::Sprite {
        self.enabled[index] = true;
        let sprite = self.editable.sprite_mut(index).unwrap();
        sprite.set_frame_count(frame_count);
        sprite
    }

    pub fn disable_sprite(&mut self, index: usize) {
        self.enabled[index] = false;
    }

    pub fn sprite(&self, index: usize) -> Option<&anim_lit::Sprite> {
        if self.enabled[index] {
            self.editable.sprite(index)
        } else {
            None
        }
    }

    pub fn sprite_mut(&mut self, index: usize) -> Option<&mut anim_lit::Sprite> {
        if self.enabled[index] {
            self.editable.sprite_mut(index)
        } else {
            None
        }
    }

    pub fn sprite_count(&self) -> u32 {
        self.editable.sprites().len() as u32
    }

    fn resize(&mut self, new_size: u16) {
        self.enabled.resize_with(new_size as usize, || false);
        self.editable.resize(new_size);
    }
}

static DEFAULT_IMAGES_TBL: &[u8] = include_bytes!("../arr/images.tbl");
static DEFAULT_IMAGES_DAT: &[u8] = include_bytes!("../arr/images.dat");
static DEFAULT_IMAGES_REL: &[u8] = include_bytes!("../arr/images.rel");

/// Cache reads of grps for SD frame sizes
struct SdGrpSizes {
    /// Sprite -> Read results.
    results: HashMap<usize, Result<(u16, u16), ArcError>>,
}

impl SdGrpSizes {
    fn new() -> SdGrpSizes {
        SdGrpSizes {
            results: Default::default(),
        }
    }
}

struct OpenFiles {
    anim: Vec<(anim::Anim, usize, SpriteType)>,
    grp: Vec<(ddsgrp::DdsGrp, usize, SpriteType)>,
}

impl OpenFiles {
    fn new() -> OpenFiles {
        OpenFiles {
            anim: Vec::new(),
            grp: Vec::new(),
        }
    }

    fn clear(&mut self) {
        self.anim.clear();
        self.grp.clear();
    }
}

struct ImagesDat {
    data: Vec<u8>,
    is_ext: bool,
    entries: u32,
    fields: u32,
}

impl ImagesDat {
    fn empty() -> ImagesDat {
        ImagesDat {
            data: Vec::new(),
            is_ext: false,
            entries: 0,
            fields: 0,
        }
    }

    fn from_data(data: Vec<u8>) -> Result<ImagesDat, Error> {
        let mut pos = &data[..];
        let magic = pos.read_u32::<LE>()?;
        if magic != 0x2b746144 {
            if data.len() == 37962 {
                Ok(ImagesDat {
                    data,
                    is_ext: false,
                    entries: 999,
                    fields: 0xe,
                })
            } else {
                Err(anyhow!("Expected 37962 bytes or extended dat"))
            }
        } else {
            let major_ver = pos.read_u16::<LE>()?;
            let minor_ver = pos.read_u16::<LE>()?;
            if major_ver != 1 || minor_ver < 1 {
                return Err(anyhow!("Invalid version {}, {}", major_ver, minor_ver));
            }
            let entries = pos.read_u32::<LE>()?;
            let fields = pos.read_u32::<LE>()?;
            Ok(ImagesDat {
                data,
                is_ext: true,
                entries,
                fields,
            })
        }
    }

    fn get_field(&self, field: u16, entry: u32) -> Result<u32, Error> {
        struct FieldDecl {
            field_id: u16,
            flags: u16,
            offset: u32,
            size: u32,
        }

        static IMAGES_DAT_FIELD_SIZES: &[u8] = &[
            4, 1, 1, 1, 1, 1, 1, 4,
            4, 4, 4, 4, 4, 4,
        ];
        if entry >= self.entries {
            return Err(
                anyhow!("Cannot read dat entry {}, there are {} entries", entry, self.entries)
            );
        }
        if self.is_ext {
            let field_decl = (0..self.fields)
                .map(|i| i as usize)
                .filter_map(|i| self.data.get(0x10 + i..)?.get(..0xc))
                .map(|slice| {
                    let field_id = LittleEndian::read_u16(&slice[0..]);
                    let flags = LittleEndian::read_u16(&slice[2..]);
                    let offset = LittleEndian::read_u32(&slice[4..]);
                    let size = LittleEndian::read_u32(&slice[8..]);
                    FieldDecl {
                        field_id,
                        flags,
                        offset,
                        size,
                    }
                })
                .find(|x| x.field_id == field);
            let field_decl = field_decl
                .ok_or_else(|| anyhow!("Field 0x{:x} is not available", field))?;
            let slice = self.data.get((field_decl.offset as usize)..)
                .and_then(|x| x.get(..(field_decl.size as usize)))
                .ok_or_else(|| anyhow!("Corrupt field decl"))?;
            let result = match field_decl.flags & 0x3 {
                0 => slice.get(entry as usize).map(|&x| x as u32),
                1 => slice.get((entry as usize * 2)..)
                    .and_then(|mut x| x.read_u16::<LE>().ok())
                    .map(|x| x as u32),
                2 => slice.get((entry as usize * 4)..)
                    .and_then(|mut x| x.read_u32::<LE>().ok()),
                3 | _ => slice.get((entry as usize * 8)..)
                    .and_then(|mut x| x.read_u16::<LE>().ok())
                    .and_then(|x| u32::try_from(x).ok()),
            };
            result.ok_or_else(|| anyhow!("Corrupt field decl, reading entry 0x{:x}", entry))
        } else {
            let field_size = IMAGES_DAT_FIELD_SIZES.get(field as usize)
                .map(|&x| x as usize)
                .ok_or_else(|| anyhow!("Field 0x{:x} is not available", field))?;
            let field_offset = IMAGES_DAT_FIELD_SIZES
                .iter()
                .take(field as usize)
                .fold(0usize, |x, &size| x + size as usize * 999);
            let entry_offset = field_offset + field_size * entry as usize;
            let result = match field_size {
                1 => self.data.get(entry_offset).map(|&x| x as u32),
                4 => self.data.get((entry as usize * 4)..)
                    .and_then(|mut x| x.read_u32::<LE>().ok()),
                _ => return Err(anyhow!("Invalid field size for field 0x{:x}", field)),
            };
            result.ok_or_else(|| anyhow!("Corrupt field decl, reading entry 0x{:x}", entry))
        }
    }
}

#[derive(Clone, Debug)]
enum Edit {
    Ref(u16),
    Values(EditValues),
    // Frames, scale, palette
    Grp(Vec<(ddsgrp::Frame, Vec<u8>)>, u8, Option<Vec<u8>>),
}

#[derive(Clone, Debug)]
struct EditValues {
    values: SpriteValues,
    tex_changes: Option<anim::TexChanges>,
}

#[derive(Debug, Clone)]
pub enum SpriteFiles {
    AnimSet(AnimFiles),
    DdsGrp(PathBuf),
    MainSdOnly {
        #[allow(dead_code)]
        image_id: u32,
        name: String,
    }
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct AnimFiles {
    pub image_id: u32,
    pub hd_filename: PathBuf,
    pub hd2_filename: PathBuf,
    pub name: String,
}

pub struct File<'a> {
    location: FileLocation<'a>,
    sprite_type: SpriteType,
    sprite_values: Option<SpriteValues>,
    frames: Option<&'a [anim::Frame]>,
    // Two variations since it can be patched or original, and they are in differently typed
    // vecs :l
    textures: Option<&'a [Option<(anim::Texture, Vec<u8>)>]>,
    texture_sizes: Option<&'a [Option<anim::Texture>]>,
    // Sigh, third
    grp_textures: Option<&'a [(ddsgrp::Frame, Vec<u8>)]>,
    // Set when palette is edited.
    // Outer option is None if not edited, inner option is None if edited to have
    // no palette.
    palette: Option<Option<&'a [u8]>>,
    image_ref: Option<Option<u16>>,
    /// Filesystem path to the file
    path: &'a Path,
    /// Some for SD sprites, None otherwise
    /// Contains dimensions read from the corresponding GRP set in images.dat,
    /// or an error if it could not be read.
    grp_dimensions: Option<Result<(u16, u16), ArcError>>,
    image_rel: Option<ImageRelation>,
}

pub enum FileLocation<'a> {
    /// Index to a single sprite in larger anim (Mainsd)
    Multiple(usize, &'a anim::Anim),
    /// A single-sprite anim
    Separate(&'a anim::Anim),
    DdsGrp(&'a ddsgrp::DdsGrp),
}

impl<'a> File<'a> {
    /// Filesystem path to the file. Note that with mainsd different files
    /// share the file path.
    pub fn path(&self) -> &Path {
        self.path
    }

    /// Few SD ddsgrps use the original .grp for dimensions, return path
    /// to that if it exists. Well, cmdicons is the only one I know of,
    /// wireframe does not use grp.
    /// (Currently just assumes that it's SD grp if its parent directiories match,
    /// as no root data is really stored if ddsgrp is opened)
    pub fn ddsgrp_linked_grp(&self) -> Option<PathBuf> {
        ddsgrp_linked_grp(self.path())
    }

    pub fn is_anim(&self) -> bool {
        match self.location {
            FileLocation::Multiple(..) | FileLocation::Separate(..) => true,
            FileLocation::DdsGrp(..) => false,
        }
    }

    pub fn grp(&self) -> Option<&ddsgrp::DdsGrp> {
        match self.location {
            FileLocation::Multiple(..) | FileLocation::Separate(..) => None,
            FileLocation::DdsGrp(s) => Some(s),
        }
    }

    pub fn texture(&self, layer: usize) -> Result<anim::RawTexture, Error> {
        if let Some(ref tex) = self.textures {
            let tex = tex.get(layer).and_then(|x| x.as_ref())
                .ok_or_else(|| anyhow!("No texture for layer {}", layer))?;
            return Ok(anim::read_texture(Cursor::new(&tex.1), &tex.0)?.into());
        }
        if let Some(ref tex) = self.grp_textures {
            let tex = tex.get(layer).ok_or_else(|| anyhow!("No frame {}", layer))?;
            if self.palette().is_some() {
                return Ok(anim::RawTexture {
                    data: tex.1.clone(),
                    width: tex.0.width.into(),
                    height: tex.0.height.into(),
                    is_paletted: true,
                });
            } else {
                let anim_tex = tex.0.to_anim_texture_coords();
                return Ok(anim::read_texture(Cursor::new(&tex.1), &anim_tex)?.into());
            }
        }
        if let Some(Some(img_ref)) = self.image_ref {
            Ok(match self.location {
                FileLocation::Multiple(_, ref mainsd) => {
                    mainsd.texture(img_ref as usize, layer)?.into()
                }
                FileLocation::Separate(..) => {
                    return Err(anyhow!("Ref in HD sprite"));
                }
                FileLocation::DdsGrp(..) => {
                    return Err(anyhow!("Ref in ddsgrp"));
                }
            })
        } else {
            Ok(match self.location {
                FileLocation::Multiple(sprite, ref mainsd) => {
                    mainsd.texture(sprite, layer)?.into()
                }
                FileLocation::Separate(ref file) => {
                    file.texture(0, layer)?.into()
                }
                FileLocation::DdsGrp(ref grp) => grp.frame(layer)?,
            })
        }
    }

    /// Gets the palette if the file has any
    /// (Only SD tileset vr4 usually has them)
    /// RGB0 format
    pub fn palette(&self) -> Option<&[u8]> {
        if let Some(palette) = self.palette {
            palette
        } else {
            match self.location {
                FileLocation::Multiple(..) => None,
                FileLocation::Separate(..) => None,
                FileLocation::DdsGrp(ref grp) => grp.palette(),
            }
        }
    }

    /// Dimensions that apply for all of the frames.
    /// Anim only.
    /// SD sprites use values from GRPs, HD from SpriteValues.
    pub fn dimensions(&self) -> Result<(u16, u16), ArcError> {
        if self.sprite_type == SpriteType::Sd {
            if let FileLocation::Multiple(..) = self.location {
                if let Some(ref result) = self.grp_dimensions {
                    return result.clone();
                }
            }
            return Err(anyhow!("Not applicable").into());
        } else {
            self.sprite_values()
                .ok_or_else(|| anyhow!("Not applicable").into())
                .map(|x| (x.width, x.height))
        }
    }

    pub fn sprite_values(&self) -> Option<SpriteValues> {
        if let Some(ref edit) = self.sprite_values {
            return Some(edit.clone());
        }
        self.location.sprite_values()
    }

    pub fn frames(&self) -> Option<&[anim::Frame]> {
        if let Some(ref f) = self.frames {
            return Some(f);
        }
        self.location.frames()
    }

    pub fn texture_size(&self, layer: usize) -> Option<anim::Texture> {
        if let Some(ref tex) = self.textures {
            return Some(tex.get(layer)?.as_ref()?.0.clone());
        }
        if let Some(ref tex) = self.texture_sizes {
            return Some(tex.get(layer)?.as_ref()?.clone());
        }
        self.location.texture_size(layer)
    }

    pub fn texture_formats(&self) -> Vec<Result<Option<anim::TextureFormat>, Error>> {
        if let Some(ref tex) = self.textures {
            return tex.iter().map(|x| {
                match x {
                    Some(ref x) => {
                        let cursor = ::std::io::Cursor::new(&x.1);
                        let format = anim::texture_format(cursor, x.1.len() as u32)?;
                        Ok(Some(format))
                    }
                    None => Ok(None),
                }
            }).collect();
        }
        let formats = if let Some(Some(img_ref)) = self.image_ref {
            match self.location {
                FileLocation::Multiple(_, mainsd) => mainsd.texture_formats(img_ref as usize),
                FileLocation::Separate(..) => {
                    warn!("Ref in HD sprite??");
                    Vec::new()
                }
                FileLocation::DdsGrp(..) => {
                    warn!("Ref in grp??");
                    Vec::new()
                }
            }
        } else {
            match self.location {
                FileLocation::Multiple(sprite, mainsd) => mainsd.texture_formats(sprite),
                FileLocation::Separate(file) => file.texture_formats(0),
                FileLocation::DdsGrp(grp) => grp.texture_formats(),
            }
        };
        formats.into_iter()
            .map(|result| result.map_err(|e| e.into()))
            .collect()
    }

    pub fn layer_names(&self) -> Cow<'a, [String]> {
        match self.location {
            FileLocation::Multiple(_sprite, mainsd) => mainsd.layer_names().into(),
            FileLocation::Separate(file) => file.layer_names().into(),
            FileLocation::DdsGrp(grp) => {
                (0..grp.frames.len()).map(|i| format!("#{}", i)).collect::<Vec<_>>().into()
            }
        }
    }

    pub fn layer_count(&self) -> usize {
        match self.location {
            FileLocation::Multiple(_sprite, mainsd) => mainsd.layer_names().len(),
            FileLocation::Separate(file) => file.layer_names().len(),
            FileLocation::DdsGrp(grp) => grp.frames.len(),
        }
    }

    pub fn image_ref(&self) -> Option<u16> {
        if let Some(img_ref) = self.image_ref {
            return img_ref;
        }
        self.location.image_ref()
    }

    pub fn image_rel(&self) -> Option<ImageRelation> {
        self.image_rel
    }
}

impl<'a> FileLocation<'a> {
    pub fn values_or_ref(&self) -> Option<anim::ValuesOrRef> {
        match *self {
            FileLocation::Multiple(sprite, mainsd) => mainsd.values_or_ref(sprite),
            FileLocation::Separate(file) => {
                Some(anim::ValuesOrRef::Values(file.sprite_values(0)?))
            }
            FileLocation::DdsGrp(_) => None,
        }
    }

    pub fn frames(&self) -> Option<&'a [anim::Frame]> {
        Some(match *self {
            FileLocation::Multiple(sprite, mainsd) => mainsd.frames(sprite)?,
            FileLocation::Separate(file) => file.frames(0)?,
            FileLocation::DdsGrp(_) => return None,
        })
    }

    pub fn texture_size(&self, layer: usize) -> Option<anim::Texture> {
        Some(match *self {
            FileLocation::Multiple(sprite, mainsd) => {
                mainsd.texture_sizes(sprite)?.get(layer)?.clone()?
            },
            FileLocation::Separate(file) => file.texture_sizes(0)?.get(layer)?.clone()?,
            FileLocation::DdsGrp(grp) => grp.texture_size(layer)?,
        })
    }

    pub fn sprite_values(&self) -> Option<SpriteValues> {
        Some(match *self {
            FileLocation::Multiple(sprite, mainsd) => mainsd.sprite_values(sprite)?,
            FileLocation::Separate(file) => file.sprite_values(0)?,
            FileLocation::DdsGrp(_) => return None,
        })
    }

    pub fn image_ref(&self) -> Option<u16> {
        match *self {
            FileLocation::Multiple(sprite, ref mainsd) => {
                mainsd.sprites().get(sprite).and_then(|x| match *x {
                    anim::SpriteType::Ref(x) => Some(x),
                    anim::SpriteType::Data(_) => None,
                })
            }
            FileLocation::Separate(..) => None,
            FileLocation::DdsGrp(_) => None,
        }
    }
}

fn load_mainsd(path: &Path) -> Result<anim::Anim, Error> {
    let file = fs::File::open(path)?;
    Ok(anim::Anim::read(file)?)
}

fn anim_index_from_filename(filename: &str) -> Option<u16> {
    let prefix = "main_";
    let suffix = ".anim";
    if filename.get(..prefix.len())?.eq_ignore_ascii_case(prefix) == false {
        return None;
    }
    let suffix_start = filename.len().checked_sub(suffix.len())?;
    if filename.get(suffix_start..)?.eq_ignore_ascii_case(suffix) == false {
        return None;
    }
    let index_str = filename.get(prefix.len()..suffix_start)?;
    // Zero padded to 3 chars, otherwise no zeroes
    if index_str.len() < 3 {
        return None;
    }
    if index_str.len() > 3 && index_str.starts_with("0") {
        return None;
    }
    index_str.parse::<u16>().ok()
}

#[test]
fn test_anim_index_from_filename() {
    assert_eq!(anim_index_from_filename("asdf.anim"), None);
    assert_eq!(anim_index_from_filename("main.anim"), None);
    assert_eq!(anim_index_from_filename("main_.anim"), None);
    assert_eq!(anim_index_from_filename("main_4.anim"), None);
    assert_eq!(anim_index_from_filename("main_44.anim"), None);
    assert_eq!(anim_index_from_filename("main_05.anim"), None);
    assert_eq!(anim_index_from_filename("main_000.anim"), Some(0));
    assert_eq!(anim_index_from_filename("main_044.anim"), Some(44));
    assert_eq!(anim_index_from_filename("main_444.anim"), Some(444));
    assert_eq!(anim_index_from_filename("main_1444.anim"), Some(1444));
    assert_eq!(anim_index_from_filename("main_0444.anim"), None);
}

/// Default HD anim layer names.
///
/// (Layer names of first anim that exists on disk, if any)
fn hd_layer_names_from_root(
    root: &Path,
    sprite_count: u16,
) -> Result<Vec<String>, Error> {
    let files = std::fs::read_dir(root.join("anim"))?
        .filter_map(|entry| {
            entry.map(|entry| {
                let file_name = entry.file_name();
                let index = file_name.to_str()
                    .and_then(|x| anim_index_from_filename(x))?;
                Some((index, entry.path()))
            })
            .transpose()
        })
        .collect::<Result<HashMap<u16, _>, _>>()?;
    for i in 0..sprite_count {
        if let Some(path) = files.get(&i) {
            let file = fs::File::open(path)
                .with_context(|| format!("Accessing {}", path.display()))?;
            let anim = anim::Anim::read(file)
                .with_context(|| format!("Reading {}", path.display()))?;
            return Ok(anim.layer_names().into());
        }
    }
    Ok(default_hd_layer_names())
}

/// Generates sprite list for case when only mainsd (-like anim) is loaded.
fn mainsd_sprites(sprite_count: u16) -> Vec<SpriteFiles> {
    (0..sprite_count).map(|i| {
        SpriteFiles::MainSdOnly {
            image_id: i as u32,
            name: image_name(i as u32),
        }
    }).collect()
}

/// Generates sprite list for case when entire mod dir is loaded
fn anim_set_sprites(root: &FileRoot, sprite_count: u16) -> Vec<SpriteFiles> {
    let hd_dir = root.hd_anim_dir();
    let hd2_dir = root.hd2_anim_dir();
    (0..sprite_count).map(|i| {
        let filename = format!("main_{:03}.anim", i);
        let hd_filename = hd_dir.join(&filename);
        let hd2_filename = hd2_dir.join(&filename);
        SpriteFiles::AnimSet(AnimFiles {
            image_id: i as u32,
            hd_filename,
            hd2_filename,
            name: image_name(i as u32),
        })
    }).collect()
}

impl Files {
    pub fn empty() -> Files {
        Files {
            sprites: Vec::new(),
            mainsd_anim: None,
            file_root: None,
            open_files: OpenFiles::new(),
            sd_grp_sizes: SdGrpSizes::new(),
            edits: HashMap::new(),
            images_dat: ImagesDat::empty(),
            images_tbl: Vec::new(),
            lit: None,
            images_rel: None,
            new_entry_count: None,
            sd_layer_names: default_sd_layer_names(),
            hd_layer_names: default_hd_layer_names(),
        }
    }

    pub fn root_path(&self) -> Option<&Path> {
        self.file_root.as_ref().map(|x| Path::new(&x.root))
    }

    /// Tries to load an entire anim tree structure, if files seem to be laid out like that.
    /// Otherwise just opens the file given.
    ///
    /// Returns sprite index if the filename is in anim/ or hd2/anim
    pub fn init(one_filename: &Path) -> Result<(Files, Option<usize>), Error> {
        if let Some(file_root) = file_root_from_file(one_filename) {
            let root = &file_root.root;
            let index = file_root.index;
            let mainsd_anim = {
                if let Some(mainsd_path) = file_root.mainsd_path() {
                    if mainsd_path.exists() && mainsd_path.is_file() {
                        let mainsd = load_mainsd(&mainsd_path)?;
                        Some((mainsd_path, mainsd))
                    } else {
                        None
                    }
                } else {
                    None
                }
            };
            let sprite_count = mainsd_anim.as_ref().map(|x| x.1.sprites().len())
                .unwrap_or(999);
            let images_dat = std::fs::read(root.join("arr/images.dat"))
                .unwrap_or_else(|_| DEFAULT_IMAGES_DAT.into());
            let images_tbl = std::fs::read(root.join("arr/images.tbl"))
                .unwrap_or_else(|_| DEFAULT_IMAGES_TBL.into());
            let images_rel = std::fs::read(root.join("images.rel"))
                .unwrap_or_else(|_| DEFAULT_IMAGES_REL.into());
            let primary_anim = file_root.region.is_none() && file_root.skin.is_none();
            let lit_path = root.join("anim/main.lit");
            let lit = if lit_path.exists() && lit_path.is_file() && primary_anim {
                let file = fs::File::open(&lit_path)
                    .with_context(|| format!("Opening {} failed", lit_path.display()))?;
                let mut read = BufReader::new(file);
                let lit = Lit::read(&mut read)
                    .with_context(|| format!("Reading {} failed", lit_path.display()))?;
                Some(LitFile {
                    enabled: lit.sprites().iter().map(|x| x.has_data()).collect(),
                    editable: lit.clone(),
                    original: lit,
                    path: lit_path,
                })
            } else {
                None
            };
            let sd_layer_names = mainsd_anim.as_ref()
                .map(|x| x.1.layer_names().into())
                .unwrap_or_else(|| default_sd_layer_names());
            let hd_layer_names = hd_layer_names_from_root(&root, sprite_count as u16)?;
            Ok((Files {
                sprites: anim_set_sprites(&file_root, sprite_count as u16),
                mainsd_anim,
                file_root: Some(file_root),
                open_files: OpenFiles::new(),
                sd_grp_sizes: SdGrpSizes::new(),
                edits: HashMap::new(),
                images_dat: ImagesDat::from_data(images_dat)
                    .context("Invalid images.dat")?,
                images_tbl,
                lit,
                images_rel: Some(ImagesRel {
                    editable: images_rel.clone(),
                    original: images_rel,
                    dirty: false,
                }),
                new_entry_count: None,
                sd_layer_names,
                hd_layer_names,
            }, index))
        } else {
            match one_filename.extension().map(|x| x == "anim").unwrap_or(false) {
                true => {
                    let mainsd = load_mainsd(one_filename)?;
                    let sd_layer_names = mainsd.layer_names().into();
                    Ok((Files {
                        sprites: mainsd_sprites(mainsd.sprites().len() as u16),
                        mainsd_anim: Some((one_filename.into(), mainsd)),
                        file_root: None,
                        open_files: OpenFiles::new(),
                        sd_grp_sizes: SdGrpSizes::new(),
                        edits: HashMap::new(),
                        images_dat: ImagesDat::empty(),
                        images_tbl: Vec::new(),
                        lit: None,
                        images_rel: None,
                        new_entry_count: None,
                        sd_layer_names: sd_layer_names,
                        hd_layer_names: default_hd_layer_names(),
                    }, None))
                }
                false => {
                    Ok((Files {
                        sprites: vec![SpriteFiles::DdsGrp(one_filename.into())],
                        mainsd_anim: None,
                        file_root: None,
                        open_files: OpenFiles::new(),
                        sd_grp_sizes: SdGrpSizes::new(),
                        edits: HashMap::new(),
                        images_dat: ImagesDat::empty(),
                        images_tbl: Vec::new(),
                        lit: None,
                        images_rel: None,
                        new_entry_count: None,
                        sd_layer_names: default_sd_layer_names(),
                        hd_layer_names: default_hd_layer_names(),
                    }, None))
                }
            }
        }
    }

    pub fn file<'a>(
        &'a mut self,
        sprite: usize,
        ty: SpriteType
    ) -> Result<Option<File<'a>>, Error> {
        let location;
        let sprite_values;
        let frames;
        let textures;
        let mut texture_sizes = None;
        let mut grp_textures = None;
        let mut palette = None;
        let image_rel = self.images_rel().as_ref().map(|x| x.get(sprite as u16));
        let image_ref;
        let grp_dimensions = if ty == SpriteType::Sd {
            Some(self.grp_dimensions_for_sprite(sprite))
        } else {
            None
        };
        let path = file_path(self.mainsd_anim.as_ref().map(|x| &*x.0), &self.sprites, sprite, ty);
        let path = match path {
            Some(s) => s,
            None => {
                warn!("No path for {}/{:?}?", sprite, ty);
                return Ok(None);
            }
        };
        let edit_values = self.edits.get(&(sprite, ty));

        match edit_values {
            Some(x) => match *x {
                Edit::Values(ref x) => {
                    let loc = file_location(
                        self.mainsd_anim.as_ref().map(|x| &x.1),
                        &mut self.open_files,
                        &self.sprites,
                        sprite,
                        ty,
                        &self.hd_layer_names,
                        &self.edits,
                    );
                    location = match loc {
                        Ok(Some(x)) => x,
                        Ok(None) => return Ok(None),
                        Err(e) => return Err(e),
                    };
                    sprite_values = Some(x.values);
                    frames = x.tex_changes.as_ref().map(|x| &x.frames[..]);
                    textures = x.tex_changes.as_ref().map(|x| &x.textures[..]);
                    image_ref = Some(None);
                }
                Edit::Ref(img_id) => {
                    let ref_edits = self.edits.get(&(img_id as usize, ty));
                    assert_eq!(ty, SpriteType::Sd);
                    let mainsd = match self.mainsd_anim {
                        Some(ref x) => &x.1,
                        None => return Ok(None),
                    };
                    image_ref = Some(Some(img_id));
                    match ref_edits {
                        Some(Edit::Values(ref x)) => {
                            sprite_values = Some(x.values);
                            frames = x.tex_changes.as_ref().map(|x| &x.frames[..]);
                            textures = x.tex_changes.as_ref().map(|x| &x.textures[..]);
                        }
                        Some(Edit::Ref(_)) => {
                            //warn!("Double ref for {}", sprite);
                            sprite_values = mainsd.sprite_values(img_id as usize);
                            frames = mainsd.frames(img_id as usize);
                            textures = None;
                            texture_sizes = mainsd.texture_sizes(img_id as usize);
                        }
                        None | Some(Edit::Grp(..)) => {
                            sprite_values = mainsd.sprite_values(img_id as usize);
                            frames = mainsd.frames(img_id as usize);
                            textures = None;
                            texture_sizes = mainsd.texture_sizes(img_id as usize);
                        }
                    }
                    location = FileLocation::Multiple(sprite, mainsd);
                }
                Edit::Grp(ref grp_edits, _scale, ref edit_palette) => {
                    let loc = file_location(
                        self.mainsd_anim.as_ref().map(|x| &x.1),
                        &mut self.open_files,
                        &self.sprites,
                        sprite,
                        ty,
                        &self.hd_layer_names,
                        &self.edits,
                    );
                    location = match loc {
                        Ok(Some(x)) => x,
                        Ok(None) => return Ok(None),
                        Err(e) => return Err(e),
                    };
                    sprite_values = None;
                    frames = None;
                    textures = None;
                    image_ref = None;
                    grp_textures = Some(&**grp_edits);
                    palette = Some(edit_palette.as_ref().map(|x| &**x));
                }
            },
            None => {
                image_ref = None;
                let loc = file_location(
                    self.mainsd_anim.as_ref().map(|x| &x.1),
                    &mut self.open_files,
                    &self.sprites,
                    sprite,
                    ty,
                    &self.hd_layer_names,
                    &self.edits,
                );
                location = match loc {
                    Ok(Some(x)) => x,
                    Ok(None) => return Ok(None),
                    Err(e) => return Err(e),
                };
                if let Some(ref_img) = location.image_ref() {
                    match self.edits.get(&(ref_img as usize, ty)) {
                        Some(Edit::Values(ref s)) => {
                            sprite_values = Some(s.values);
                            frames = s.tex_changes.as_ref().map(|x| &x.frames[..]);
                            textures = s.tex_changes.as_ref().map(|x| &x.textures[..]);
                        }
                        _ => {
                            sprite_values = None;
                            frames = None;
                            textures = None;
                        }
                    }
                } else {
                    sprite_values = None;
                    frames = None;
                    textures = None;
                };
            }
        };

        Ok(Some(File {
            location,
            sprite_type: ty,
            sprite_values,
            frames,
            textures,
            texture_sizes,
            grp_textures,
            palette,
            image_ref,
            path,
            grp_dimensions,
            image_rel,
        }))
    }

    pub fn close_opened(&mut self) {
        self.open_files.clear();
    }

    pub fn is_anim(&self) -> bool {
        match self.sprites.get(0) {
            Some(SpriteFiles::DdsGrp(..)) => false,
            _ => true,
        }
    }

    pub fn sprites(&self) -> &[SpriteFiles] {
        &self.sprites[..]
    }

    pub fn mainsd(&self) -> Option<&anim::Anim> {
        self.mainsd_anim.as_ref().map(|x| &x.1)
    }

    pub fn mainsd_entries(&self) -> Option<u16> {
        let mainsd = self.mainsd()?;
        if let Some(new_count) = self.new_entry_count {
            Some(new_count)
        } else {
            Some(mainsd.sprites().len() as u16)
        }
    }

    pub fn set_ref_enabled(&mut self, sprite: usize, ty: SpriteType, enabled: bool) {
        if ty != SpriteType::Sd {
            warn!("Can only enable ref on SD sprites");
            return;
        }

        let orig_enabled = {
            let file = file_location(
                self.mainsd_anim.as_ref().map(|x| &x.1),
                &mut self.open_files,
                &self.sprites,
                sprite,
                ty,
                &self.hd_layer_names,
                &self.edits,
            ).ok().and_then(|x| x);
            let orig = match file.as_ref().and_then(|x| x.values_or_ref()) {
                Some(s) => s,
                None => {
                    warn!("Tried to update nonexisting sprite {}/{:?}", sprite, ty);
                    return;
                }
            };
            match orig {
                anim::ValuesOrRef::Ref(..) => true,
                anim::ValuesOrRef::Values(..) => false,
            }
        };
        if orig_enabled == enabled {
            self.edits.remove(&(sprite, ty));
        } else {
            let value = match enabled {
                true => Edit::Ref(0),
                false => {
                    let values = SpriteValues {
                        width: 0,
                        height: 0,
                    };
                    Edit::Values(EditValues {
                        values,
                        tex_changes: None,
                    })
                }
            };
            self.edits.insert((sprite, ty), value);
        }
    }

    pub fn set_ref_img(&mut self, sprite: usize, ty: SpriteType, image: u16) {
        let unchanged = {
            let file = file_location(
                self.mainsd_anim.as_ref().map(|x| &x.1),
                &mut self.open_files,
                &self.sprites,
                sprite,
                ty,
                &self.hd_layer_names,
                &self.edits,
            ).ok().and_then(|x| x);
            match file {
                Some(s) => s.image_ref() == Some(image),
                None => {
                    warn!("Tried to update nonexisting sprite {}/{:?}", sprite, ty);
                    return;
                }
            }
        };
        if unchanged {
            self.edits.remove(&(sprite, ty));
        } else {
            self.edits.insert((sprite, ty), Edit::Ref(image));
        }
    }

    pub fn set_tex_changes(
        &mut self,
        sprite: usize,
        ty: SpriteType,
        changes: anim::TexChanges,
        (width, height): (u16, u16),
    ) {
        let file = file_location(
            self.mainsd_anim.as_ref().map(|x| &x.1),
            &mut self.open_files,
            &self.sprites,
            sprite,
            ty,
            &self.hd_layer_names,
            &self.edits,
        ).ok().and_then(|x| x);
        let values = match file.as_ref().and_then(|x| x.values_or_ref()) {
            Some(anim::ValuesOrRef::Values(s)) => s,
            _ => {
                SpriteValues {
                    width,
                    height,
                }
            }
        };
        let entry = self.edits.entry((sprite, ty));

        let values = entry.or_insert_with(|| Edit::Values(EditValues {
            values,
            tex_changes: None,
        }));
        if let Edit::Values(ref mut vals) = values {
            vals.tex_changes = Some(changes);
        }
    }

    pub fn set_grp_changes(
        &mut self,
        sprite: usize,
        changes: Vec<(ddsgrp::Frame, Vec<u8>)>,
        scale: u8,
        palette: Option<Vec<u8>>,
    ) {
        self.edits.insert(
            (sprite, SpriteType::Sd),
            Edit::Grp(changes, scale, palette),
        );
    }

    /// Does nothing if sprite/ty is currently Ref
    pub fn update_file<F>(&mut self, sprite: usize, ty: SpriteType, fun: F)
    where F: FnOnce(&mut SpriteValues)
    {
        let unchanged = {
            let file = file_location(
                self.mainsd_anim.as_ref().map(|x| &x.1),
                &mut self.open_files,
                &self.sprites,
                sprite,
                ty,
                &self.hd_layer_names,
                &self.edits,
            ).ok().and_then(|x| x);
            let entry = self.edits.entry((sprite, ty));
            let orig = match file.as_ref().and_then(|x| x.values_or_ref()) {
                Some(s) => s,
                None => {
                    warn!("Tried to update nonexisting sprite {}/{:?}", sprite, ty);
                    return;
                }
            };
            let values = entry.or_insert_with(|| match orig {
                anim::ValuesOrRef::Values(orig) => Edit::Values(EditValues {
                    values: orig,
                    tex_changes: None,
                }),
                anim::ValuesOrRef::Ref(i) => Edit::Ref(i),
            });
            if let Edit::Values(ref mut values) = *values {
                fun(&mut values.values);
            }
            match *values {
                Edit::Ref(i) => match orig {
                    anim::ValuesOrRef::Ref(j) => i == j,
                    _ => false,
                },
                Edit::Values(ref vals) => {
                    vals.tex_changes.is_none() && match orig {
                        anim::ValuesOrRef::Values(ref o) => vals.values == *o,
                        _ => false,
                    }
                }
                Edit::Grp(..) => {
                    warn!("Tried to update a grp");
                    return;
                }
            }
        };
        if unchanged {
            self.edits.remove(&(sprite, ty));
        }
    }

    pub fn has_changes(&self) -> bool {
        !self.edits.is_empty() ||
            self.lit.as_ref().map(|x| x.has_changes()).unwrap_or(false) ||
            self.images_rel.as_ref().map(|x| x.has_changes()).unwrap_or(false) ||
            self.new_entry_count.is_some()
    }

    pub fn save(&mut self) -> Result<(), Error> {
        let mut result = Ok(());
        {
            let mut temp_files = Vec::new();
            let mut sd_edits = Vec::new();
            let mut sd_textures = Vec::new();
            for (&(sprite, ty), edit) in self.edits.iter() {
                let is_anim = match edit {
                    Edit::Grp(..) => false,
                    Edit::Ref(..) | Edit::Values(..) => true,
                };
                let is_mainsd_edit = ty == SpriteType::Sd && is_anim;
                if !is_mainsd_edit {
                    let path = match separate_file_path(&self.sprites, sprite, ty) {
                        Some(s) => s,
                        None => {
                            return Err(anyhow!("No path for sprite {}/{:?}", sprite, ty));
                        }
                    };
                    let out_path = temp_file_path(&path);
                    if let Some(parent) = out_path.parent() {
                        if !parent.exists() {
                            fs::create_dir_all(&parent)
                                .with_context(|| {
                                    format!("Unable to create {}", parent.display())
                                })?;
                        }
                    }
                    let mut out = fs::File::create(&out_path).with_context(|| {
                        format!("Unable to create {}", out_path.display())
                    })?;
                    temp_files.push((out_path, path.into()));
                    if is_anim {
                        let edit = match *edit {
                            Edit::Ref(_) => {
                                return Err(
                                    anyhow!(
                                        "Ref edit for a separate sprite {}/{:?}",
                                        sprite, ty,
                                    )
                                );
                            }
                            Edit::Values(ref v) => v,
                            Edit::Grp(..) => unreachable!(),
                        };
                        let scale = match ty {
                            SpriteType::Sd => 1,
                            SpriteType::Hd2 => 2,
                            SpriteType::Hd => 4,
                        };
                        match fs::File::open(path) {
                            Ok(file) => {
                                let anim = anim::Anim::read(file)
                                    .with_context(|| format!("Reading {}", path.display()))?;
                                let layer_names = anim.layer_names();
                                let buf;
                                let tex_edits = match edit.tex_changes {
                                    Some(ref s) => {
                                        buf = [(0, s)];
                                        &buf[..]
                                    },
                                    None => &[],
                                };
                                anim.write_patched(
                                    &mut out,
                                    scale,
                                    1,
                                    &layer_names,
                                    &[(0, anim::ValuesOrRef::Values(edit.values))],
                                    tex_edits
                                ).with_context(|| format!("Writing {}", path.display()))?;
                            }
                            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                                let layer_names = if ty == SpriteType::Sd {
                                    &self.sd_layer_names[..]
                                } else {
                                    &self.hd_layer_names[..]
                                };
                                let tex_edit = edit.tex_changes.as_ref()
                                    .ok_or_else(|| {
                                        anyhow!(
                                            "Cannot write new sprite {} without textures",
                                            sprite,
                                        )
                                    })?;
                                let sprites = [
                                    (anim::ValuesOrRef::Values(edit.values), tex_edit),
                                ];
                                anim::Anim::write_new(
                                    &mut out,
                                    scale,
                                    &layer_names,
                                    &sprites,
                                ).with_context(|| format!("Writing {}", path.display()))?;
                            }
                            Err(e) => {
                                return Err(e)
                                    .with_context(|| format!("Cannot open {}", path.display()));
                            }
                        }
                    } else {
                        if let Edit::Grp(ref edits, scale, ref palette) = *edit {
                            ddsgrp::DdsGrp::write(&mut out, scale, &edits, palette.as_deref())?;
                        }
                    }
                } else {
                    match *edit {
                        Edit::Ref(r) => {
                            sd_edits.push((sprite, anim::ValuesOrRef::Ref(r)));
                        }
                        Edit::Values(ref e) => {
                            sd_edits.push((sprite, anim::ValuesOrRef::Values(e.values)));
                            if let Some(ref tex) = e.tex_changes {
                                sd_textures.push((sprite, tex));
                            }
                        }
                        Edit::Grp(..) => unreachable!(),
                    }
                }
            }
            if !sd_edits.is_empty() {
                if let Some((ref sd_path, ref sd)) = self.mainsd_anim {
                    let sprite_count = self.new_entry_count
                        .unwrap_or_else(|| sd.sprites().len() as u16);
                    let layer_names = sd.layer_names();
                    let out_path = temp_file_path(&sd_path);
                    let mut out = fs::File::create(&out_path).with_context(|| {
                        format!("Unable to create {}", out_path.to_string_lossy())
                    })?;
                    sd.write_patched(
                        &mut out,
                        sd.scale(),
                        sprite_count,
                        &layer_names,
                        &sd_edits,
                        &sd_textures,
                    )?;
                    temp_files.push((out_path, sd_path.clone()));
                }
            }
            if let Some(lit) = self.lit() {
                if lit.has_changes() {
                    let out_path = temp_file_path(&lit.path);
                    let out = fs::File::create(&out_path).with_context(|| {
                        format!("Unable to create {}", out_path.to_string_lossy())
                    })?;
                    let mut out = BufWriter::new(out);
                    lit.write(&mut out)?;
                    temp_files.push((out_path, lit.path.clone()));
                }
            }
            if let Some(ref mut images_rel) = self.images_rel {
                if images_rel.has_changes() {
                    let file_root = match self.file_root {
                        Some(ref s) => s,
                        None => return Err(anyhow!("Can't save images.rel without root path")),
                    };
                    let root = &file_root.root;
                    let _ = fs::create_dir(root.join("SD"));
                    // There are two images.rel files, SC:R seems to randomly select one of
                    // them so they need to be similar in things which matter <.<
                    // (They're not originally 100% similar)
                    let path1 = root.join("images.rel");
                    let path2 = root.join("SD/images.rel");
                    for &path in &[&path1, &path2] {
                        let out_path = temp_file_path(&path);
                        let out = fs::File::create(&out_path).with_context(|| {
                            format!("Unable to create {}", out_path.to_string_lossy())
                        })?;
                        let mut out = BufWriter::new(out);
                        images_rel.write(&mut out)?;
                        temp_files.push((out_path, path.clone()));
                    }
                }
            }
            self.open_files.clear();
            let mut sd_path = None;
            if !sd_edits.is_empty() {
                // Closing mainsd
                sd_path = self.mainsd_anim.take().map(|x| x.0);
            }
            for (temp, dest) in temp_files {
                result = fs::rename(temp, dest);
                if result.is_err() {
                    break;
                }
            }
            if !sd_edits.is_empty() {
                if let Some(sd_path) = sd_path {
                    let mainsd = load_mainsd(&sd_path)?;
                    self.mainsd_anim = Some((sd_path, mainsd));
                }
            }
        }
        if result.is_ok() {
            self.edits.clear();
            self.new_entry_count = None;
            if let Some(lit) = self.lit() {
                if lit.has_changes() {
                    lit.reset_original();
                }
            }
            if let Some(ref mut images_rel) = self.images_rel {
                if images_rel.has_changes() {
                    images_rel.reset_original();
                }
            }
        }

        Ok(result?)
    }

    /// Returns width/height of the grp that is referenced in images.dat.
    /// (E.g. The return value is only meaningful for SD sprites)
    fn grp_dimensions_for_sprite(&mut self, sprite: usize) -> Result<(u16, u16), ArcError> {
        let file_root = self.file_root
            .as_ref()
            .ok_or_else(|| {
                anyhow!("Cannot get GRP dimensions if the files are not in \
                    placed in same layout as in CASC")
            })?;
        let root = &file_root.root;
        let images_dat = &self.images_dat;
        let images_tbl = &self.images_tbl;
        self.sd_grp_sizes.results.entry(sprite).or_insert_with(|| {
            let grp_path = image_grp_path(images_dat, images_tbl, sprite as u32)?;
            let path = root.join(&grp_path);
            if path.is_file() {
                let mut file = fs::File::open(&path)
                    .with_context(|| format!("Unable to open {}", path.display()))?;
                let mut buffer = [0u8; 6];
                file.read_exact(&mut buffer[..])
                    .with_context(|| format!("Unable to read {}", path.display()))?;
                Ok((LittleEndian::read_u16(&buffer[2..]), LittleEndian::read_u16(&buffer[4..])))
            } else {
                if let Some(size) = crate::default_grp_sizes::grp_default_size(&grp_path) {
                    Ok(size)
                } else {
                    Err(anyhow!("File {} doesn't exist", path.display()).into())
                }
            }
        }).clone()
    }

    pub fn image_grp_path(&self, image_id: usize) -> Option<String> {
        image_grp_path(&self.images_dat, &self.images_tbl, image_id as u32).ok()
    }

    pub fn images_dat_entries(&self) -> u32 {
        self.images_dat.entries
    }

    pub fn lit(&mut self) -> Option<&mut LitFile> {
        self.lit.as_mut()
    }

    pub fn images_rel(&mut self) -> Option<&mut ImagesRel> {
        self.images_rel.as_mut()
    }

    pub fn resize_entry_counts(&mut self, new_size: u16) -> Result<(), Error> {
        if let Some((_, ref mut mainsd)) = self.mainsd_anim {
            let sd_layer_names = &self.sd_layer_names[..];
            let hd_layer_names = &self.hd_layer_names[..];
            let old_size = match self.new_entry_count {
                Some(s) => s,
                None => mainsd.sprites().len() as u16,
            };
            for i in old_size..new_size {
                let edit = empty_sd_edit(&sd_layer_names);
                self.edits.insert((i as usize, SpriteType::Sd), Edit::Values(edit));
                let edit = empty_hd_edit(&hd_layer_names);
                self.edits.insert((i as usize, SpriteType::Hd2), Edit::Values(edit.clone()));
                self.edits.insert((i as usize, SpriteType::Hd), Edit::Values(edit));
            }
        }
        if let Some(ref mut lit) = self.lit {
            lit.resize(new_size);
        }
        if let Some(ref mut rel) = self.images_rel {
            rel.resize(new_size);
        }
        self.new_entry_count = Some(new_size);
        self.sprites = if let Some(ref root) = self.file_root {
            anim_set_sprites(&root, new_size)
        } else {
            mainsd_sprites(new_size)
        };
        Ok(())
    }
}

/// 4x4 empty RGBA buffer
static EMPTY_RGBA: &[u8] = &[0u8; 4 * 4 * 4];

/// Empty SD sprite that can be used as a default when adding new sprite entries.
fn empty_sd_edit(layer_names: &[String]) -> EditValues {
    empty_edit(layer_names, 1, 1)
}

/// Empty HD/HD2 sprite that can be used as a default when adding new sprite entries.
fn empty_hd_edit(layer_names: &[String]) -> EditValues {
    empty_edit(layer_names, 4, 4)
}

fn empty_edit(layer_names: &[String], width: u16, height: u16) -> EditValues {
    assert!(width <= 4 && height <= 4);
    EditValues {
        values: SpriteValues {
            width: 0,
            height: 0,
        },
        tex_changes: Some(anim::TexChanges {
            frames: vec![anim::Frame {
                tex_x: 0,
                tex_y: 0,
                x_off: 0,
                y_off: 0,
                width,
                height,
                unknown: 0,
            }],
            textures: layer_names.iter()
                .map(|name| {
                    if name == "diffuse" {
                        let data =
                            anim_encoder::encode(EMPTY_RGBA, 4, 4, anim::TextureFormat::Dxt1);
                        let texture = anim::Texture {
                            offset: 0,
                            size: data.len() as u32,
                            width: 4,
                            height: 4,
                        };
                        Some((texture, data))
                    } else {
                        None
                    }
                })
                .collect()
        }),
    }
}

fn image_grp_path(
    images_dat: &ImagesDat,
    images_tbl: &[u8],
    image_id: u32,
) -> Result<String, Error> {
    let tbl_index = images_dat.get_field(0, image_id)? as usize;
    let string = Some(())
        .and_then(|()| {
            let tbl_offset = images_tbl.get(tbl_index.checked_mul(2)?..)?
                .read_u16::<LE>().ok()? as usize;
            let string_data = images_tbl.get(tbl_offset..)?;
            let string_len = string_data.iter().position(|&x| x == 0)?;
            Some(format!("unit\\{}", String::from_utf8_lossy(&string_data[..string_len])))
        })
        .ok_or_else(|| anyhow!("Unable to read images.tbl index {}", tbl_index))?;
    Ok(string)
}

fn temp_file_path(orig_file: &Path) -> PathBuf {
    let mut buf: PathBuf = orig_file.into();
    let temp_name = {
        let orig_name = buf.file_name()
        .map(|x| x.to_string_lossy())
        .unwrap_or("".into());
        format!("__temp__{}", orig_name)
    };
    buf.set_file_name(temp_name);
    buf
}

fn file_location<'a>(
    mainsd_anim: Option<&'a anim::Anim>,
    open_files: &'a mut OpenFiles,
    sprites: &[SpriteFiles],
    sprite: usize,
    ty: SpriteType,
    hd_layer_names: &[String],
    edits: &HashMap<(usize, SpriteType), Edit>,
) -> Result<Option<FileLocation<'a>>, Error> {
    match sprites.get(sprite) {
        Some(&SpriteFiles::AnimSet(_)) | Some(&SpriteFiles::MainSdOnly { .. }) => {
            match ty {
                SpriteType::Sd => {
                    Ok(mainsd_anim.map(|x| FileLocation::Multiple(sprite, x)))
                }
                SpriteType::Hd | SpriteType::Hd2 => {
                    file_location_hd(open_files, sprites, sprite, ty, hd_layer_names, edits)
                }
            }
        }
        Some(&SpriteFiles::DdsGrp(ref f)) => {
            let file = fs::File::open(f)
                .with_context(|| format!("Opening {}", f.display()))?;
            let grp = ddsgrp::DdsGrp::read(file)
                .with_context(|| format!("Parsing {}", f.display()))?;
            open_files.grp.push((grp, sprite, ty));
            Ok(Some(FileLocation::DdsGrp(&open_files.grp.last().unwrap().0)))
        }
        None => Ok(None),
    }
}

fn file_path<'a>(
    mainsd_path: Option<&'a Path>,
    sprites: &'a [SpriteFiles],
    sprite: usize,
    ty: SpriteType,
) -> Option<&'a Path> {
    if ty == SpriteType::Sd {
        match sprites.get(sprite)? {
            SpriteFiles::AnimSet(_) => return mainsd_path,
            SpriteFiles::MainSdOnly { .. } => return mainsd_path,
            _ => (),
        }
    }
    separate_file_path(sprites, sprite, ty)
}

fn separate_file_path(sprites: &[SpriteFiles], sprite: usize, ty: SpriteType) -> Option<&Path> {
    let path = sprites.get(sprite)
        .and_then(|s| match *s {
            SpriteFiles::AnimSet(ref files) => match ty == SpriteType::Hd2 {
                false => Some(&files.hd_filename),
                true => Some(&files.hd2_filename),
            },
            SpriteFiles::DdsGrp(ref f) => Some(f),
            _ => None,
        })?;

    Some(&path)
}

fn file_location_hd<'a>(
    open_files: &'a mut OpenFiles,
    sprites: &[SpriteFiles],
    sprite: usize,
    ty: SpriteType,
    layer_names: &[String],
    edits: &HashMap<(usize, SpriteType), Edit>,
) -> Result<Option<FileLocation<'a>>, Error> {
    if let Some(index) = open_files.anim.iter().position(|x| x.1 == sprite && x.2 == ty) {
        return Ok(Some(FileLocation::Separate(&open_files.anim[index].0)));
    }
    let path = match separate_file_path(sprites, sprite, ty) {
        Some(p) => p,
        None => return Ok(None),
    };
    match fs::File::open(path) {
        Ok(file) => {
            let anim = anim::Anim::read(file)
                .with_context(|| format!("Reading {}", path.display()))?;
            open_files.anim.push((anim, sprite, ty));
            Ok(Some(FileLocation::Separate(&open_files.anim.last_mut().unwrap().0)))
        }
        Err(e) if e.kind() == io::ErrorKind::NotFound => {
            // May be reached if anims have been resized before written to disk
            // Create a in-memory anim that then is read - somewhat messy workaround
            // that makes state management even worse :l
            if let Some(&Edit::Values(ref edit)) = edits.get(&(sprite, ty)) {
                if let Some(ref tex_edit) = edit.tex_changes {
                    let scale = match ty {
                        SpriteType::Sd => 1,
                        SpriteType::Hd2 => 2,
                        SpriteType::Hd => 4,
                    };
                    let mut out = io::Cursor::new(Vec::new());
                    let sprites = [
                        (anim::ValuesOrRef::Values(edit.values), tex_edit),
                    ];
                    anim::Anim::write_new(
                        &mut out,
                        scale,
                        &layer_names,
                        &sprites,
                    ).with_context(|| format!("Writing {}", path.display()))?;
                    out.set_position(0);
                    let anim = anim::Anim::read(out)?;
                    open_files.anim.push((anim, sprite, ty));
                    return Ok(Some(
                        FileLocation::Separate(&open_files.anim.last_mut().unwrap().0)
                    ))
                }
            }
            Ok(None)
        }
        Err(e) => Err(e.into()),
    }
}

struct FileRoot {
    root: PathBuf,
    /// "CN"
    region: Option<PathBuf>,
    /// "carbot" or "presale"
    skin: Option<PathBuf>,
    index: Option<usize>,
}

impl FileRoot {
    fn hd_anim_dir(&self) -> PathBuf {
        let mut path = self.root.clone();
        if let Some(ref region) = self.region {
            path.push(region);
        }
        path.push("anim");
        if let Some(ref skin) = self.skin {
            path.push(skin);
        }
        path
    }

    fn hd2_anim_dir(&self) -> PathBuf {
        let mut path = self.root.join("HD2");
        if let Some(ref region) = self.region {
            path.push(region);
        }
        path.push("anim");
        if let Some(ref skin) = self.skin {
            path.push(skin);
        }
        path
    }

    fn mainsd_path(&self) -> Option<PathBuf> {
        if self.skin.is_some() {
            return None;
        }
        let mut path = self.root.join("SD");
        if let Some(ref region) = self.region {
            path.push(region);
        }
        path.push("mainSD.anim");
        Some(path)
    }
}

fn file_root_from_file(file: &Path) -> Option<FileRoot> {
    let filename = file.file_name()
        .and_then(|f| f.to_str())?;
    let parent_path = file.parent()?;

    let valid_skins = ["PreSale", "Carbot"];
    let valid_regions = ["CN"];

    if filename.eq_ignore_ascii_case("mainsd.anim") {
        let mut path = parent_path;
        let path_str = path.file_name()?.to_str()?;
        let region = if let Some(region) = valid_regions.iter()
            .find(|x| path_str.eq_ignore_ascii_case(x))
        {
            path = path.parent()?;
            Some(region.into())
        } else {
            None
        };
        let path_str = path.file_name()?.to_str()?;
        if !path_str.eq_ignore_ascii_case("sd") {
            return None;
        }
        path = path.parent()?;
        Some(FileRoot {
            root: path.into(),
            region,
            skin: None,
            index: None,
        })
    } else if filename.ends_with(".anim") && filename.starts_with("main_") {
        let digit_len = filename.get(5..)
            .map(|x| x.chars().take_while(|c| c.is_numeric()).count())
            .filter(|&digit_len| digit_len >= 3)?;
        let digit_str = &filename[5..][..digit_len];
        let digit = digit_str.parse::<u32>().ok()? as usize;
        if filename.len() - digit_len != "main_.anim".len() {
            // main_.anim is 10 chars, anything else should be part of the anim number.
            return None;
        }

        // Path components are in the following order:
        // [HD2/][CN/]anim/[carbot/]main_000.anim
        let mut path = parent_path;
        // Check skin
        let path_str = path.file_name()?.to_str()?;
        let skin = if let Some(skin) = valid_skins.iter()
            .find(|x| path_str.eq_ignore_ascii_case(x))
        {
            path = path.parent()?;
            Some(skin.into())
        } else {
            None
        };
        let path_str = path.file_name()?.to_str()?;
        if !path_str.eq_ignore_ascii_case("anim") {
            return None;
        }
        path = path.parent()?;
        let path_str = path.file_name()?.to_str()?;
        let region = if let Some(region) = valid_regions.iter()
            .find(|x| path_str.eq_ignore_ascii_case(x))
        {
            path = path.parent()?;
            Some(region.into())
        } else {
            None
        };
        let path_str = path.file_name()?.to_str()?;
        if path_str.eq_ignore_ascii_case("hd2") {
            path = path.parent()?;
        }

        Some(FileRoot {
            root: path.into(),
            region,
            skin,
            index: Some(digit),
        })
    } else {
        None
    }
}

fn image_name(image_id: u32) -> String {
    format!("#{:03}", image_id)
}

fn path_remove_suffix_casei<'a>(path: &'a Path, suffix: &Path) -> Option<&'a Path> {
    let mut path = path;
    let mut suffix = suffix;
    while let Some(part) = suffix.file_name() {
        let part2 = path.file_name()?;
        if part.to_str()?.eq_ignore_ascii_case(part2.to_str()?) == false {
            return None;
        }
        path = path.parent()?;
        suffix = suffix.parent()?;
    }
    Some(path)
}

fn ddsgrp_linked_grp(path: &Path) -> Option<PathBuf> {
    // SD/unit/cmdicons/cmdicons.dds.grp -> unit/cmdicons/cmdicons.grp
    // SD/unit/wirefram/{grpwire, tranwire, wirefram}.dds.grp ->
    //     unit/wirefram/{grpwire, tranwire, wirefram}.grp
    let mapping = [
        ("SD/unit/cmdicons/cmdicons.dds.grp", "unit/cmdicons/cmdicons.grp"),
    ];
    mapping
        .iter()
        .find_map(|(base, new)| {
            Some(path_remove_suffix_casei(path, Path::new(base))?.join(new))
        })
}

#[test]
fn test_ddsgrp_linked_grp() {
    let normalize = |x: PathBuf| x.display().to_string().to_ascii_lowercase().replace("\\", "/");
    assert_eq!(
        ddsgrp_linked_grp(Path::new("asd/dir/SD/unit/cmdicons/cmdicons.dds.grp"))
            .map(normalize),
        Some(String::from("asd/dir/unit/cmdicons/cmdicons.grp")),
    );
    assert_eq!(
        ddsgrp_linked_grp(Path::new("asd/dir/HD2/unit/cmdicons/cmdicons.dds.grp")),
        None,
    );
    assert_eq!(
        ddsgrp_linked_grp(Path::new("asd/dir/unit/cmdicons/cmdicons.dds.grp")),
        None,
    );
    assert_eq!(
        ddsgrp_linked_grp(Path::new("asd/dir/SD/unit/cmdicons.dds.grp")),
        None,
    );
    assert_eq!(
        ddsgrp_linked_grp(Path::new("asd/dir/SD/unit/wirefram/cmdicons.dds.grp")),
        None,
    );
}

#[test]
fn test_file_root_from_file() {
    let root_path = Path::new("a/b/c");
    let check_root = |path: &str, idx: Option<usize>| {
        let root = file_root_from_file(Path::new(path)).unwrap();
        assert_eq!(root.root, root_path);
        assert_eq!(root.index, idx);
        assert_eq!(root.region, None);
        assert_eq!(root.skin, None);
    };
    check_root("a/b/c/sd/mainsd.anim", None);
    check_root("a/b/c/sd/mainsd.anim", None);
    check_root("a/b/c/anim/main_000.anim", Some(0));
    check_root("a/b/c/hd2/anim/main_000.anim", Some(0));
    check_root("a/b/c/hd2/anim/main_003.anim", Some(3));
    assert!(file_root_from_file(Path::new("a/b/c/mainsd.anim")).is_none());
    assert!(file_root_from_file(Path::new("a/b/c/a/main_000.anim")).is_none());
    assert!(file_root_from_file(Path::new("a/b/c/anim/main_nonstandard_name_000.anim")).is_none());
    assert!(file_root_from_file(Path::new("a/b/c/anim/main_000_nonstandard_name.anim")).is_none());
    check_root("a/b/c/anim/main_1000.anim", Some(1000));
    let root = file_root_from_file(Path::new("a/b/c/anim/carbot/main_1000.anim")).unwrap();
    assert_eq!(root.root, root_path);
    assert_eq!(root.index, Some(1000));
    assert_eq!(root.region, None);
    assert_eq!(root.skin, Some(PathBuf::from("Carbot")));
    let root = file_root_from_file(Path::new("a/b/c/anim/presale/main_1000.anim")).unwrap();
    assert_eq!(root.root, root_path);
    assert_eq!(root.index, Some(1000));
    assert_eq!(root.region, None);
    assert_eq!(root.skin, Some(PathBuf::from("PreSale")));
    let root = file_root_from_file(Path::new("a/b/c/cn/anim/main_1000.anim")).unwrap();
    assert_eq!(root.root, root_path);
    assert_eq!(root.index, Some(1000));
    assert_eq!(root.region, Some(PathBuf::from("CN")));
    assert_eq!(root.skin, None);
    let root = file_root_from_file(Path::new("a/b/c/hd2/anim/carbot/main_1000.anim")).unwrap();
    assert_eq!(root.root, root_path);
    assert_eq!(root.index, Some(1000));
    assert_eq!(root.region, None);
    assert_eq!(root.skin, Some(PathBuf::from("Carbot")));
    let root = file_root_from_file(Path::new("a/b/c/hd2/anim/presale/main_1000.anim")).unwrap();
    assert_eq!(root.root, root_path);
    assert_eq!(root.index, Some(1000));
    assert_eq!(root.region, None);
    assert_eq!(root.skin, Some(PathBuf::from("PreSale")));
    let root = file_root_from_file(Path::new("a/b/c/hd2/cn/anim/main_1000.anim")).unwrap();
    assert_eq!(root.root, root_path);
    assert_eq!(root.index, Some(1000));
    assert_eq!(root.region, Some(PathBuf::from("CN")));
    assert_eq!(root.skin, None);
    let root = file_root_from_file(Path::new("a/b/c/sd/cn/mainsd.anim")).unwrap();
    assert_eq!(root.root, root_path);
    assert_eq!(root.index, None);
    assert_eq!(root.region, Some(PathBuf::from("CN")));
    assert_eq!(root.skin, None);
}
