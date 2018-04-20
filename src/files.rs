use std::borrow::Cow;
use std::collections::HashMap;
use std::fs;
use std::io::Cursor;
use std::path::{Path, PathBuf};

use failure::{Error, ResultExt};

use anim::{self, SpriteValues};
use ddsgrp;
use ::SpriteType;

pub struct Files {
    sprites: Vec<SpriteFiles>,
    mainsd_anim: Option<(PathBuf, anim::Anim)>,
    root_path: Option<PathBuf>,
    open_files: OpenFiles,
    edits: HashMap<(usize, SpriteType), Edit>,
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

#[derive(Clone, Debug)]
enum Edit {
    Ref(u32),
    Values(EditValues),
    Grp(Vec<(ddsgrp::Frame, Vec<u8>)>),
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
        image_id: u32,
        name: String,
    }
}

#[derive(Debug, Clone)]
pub struct AnimFiles {
    pub image_id: u32,
    pub hd_filename: PathBuf,
    pub hd2_filename: PathBuf,
    pub name: String,
}

pub struct File<'a> {
    location: FileLocation<'a>,
    sprite_values: Option<SpriteValues>,
    frames: Option<&'a [anim::Frame]>,
    // Two variations since it can be patched or original, and they are in differently typed
    // vecs :l
    textures: Option<&'a [Option<(anim::Texture, Vec<u8>)>]>,
    texture_sizes: Option<&'a [Option<anim::Texture>]>,
    // Sigh, third
    grp_textures: Option<&'a [(ddsgrp::Frame, Vec<u8>)]>,
    image_ref: Option<Option<u32>>,
    path: &'a Path,
}

pub enum FileLocation<'a> {
    Multiple(usize, &'a anim::Anim),
    Separate(&'a anim::Anim),
    DdsGrp(&'a ddsgrp::DdsGrp),
}

impl<'a> File<'a> {
    pub fn path(&self) -> &Path {
        self.path
    }

    pub fn is_anim(&self) -> bool {
        match self.location {
            FileLocation::Multiple(..) | FileLocation::Separate(..) => true,
            FileLocation::DdsGrp(..) => false,
        }
    }

    pub fn texture(&self, layer: usize) -> Result<anim::RgbaTexture, Error> {
        if let Some(ref tex) = self.textures {
            let tex = tex.get(layer).and_then(|x| x.as_ref())
                .ok_or_else(|| format_err!("No texture for layer {}", layer))?;
            return anim::read_texture(Cursor::new(&tex.1), &tex.0);
        }
        if let Some(ref tex) = self.grp_textures {
            let tex = tex.get(layer).ok_or_else(|| format_err!("No frame {}", layer))?;
            let anim_tex = tex.0.to_anim_texture_coords();
            return anim::read_texture(Cursor::new(&tex.1), &anim_tex);
        }
        if let Some(Some(img_ref)) = self.image_ref {
            Ok(match self.location {
                FileLocation::Multiple(_, ref mainsd) => mainsd.texture(img_ref as usize, layer)?,
                FileLocation::Separate(..) => {
                    return Err(format_err!("Ref in HD sprite"));
                }
                FileLocation::DdsGrp(..) => {
                    return Err(format_err!("Ref in ddsgrp"));
                }
            })
        } else {
            Ok(match self.location {
                FileLocation::Multiple(sprite, ref mainsd) => mainsd.texture(sprite, layer)?,
                FileLocation::Separate(ref file) => file.texture(0, layer)?,
                FileLocation::DdsGrp(ref grp) => grp.frame(layer)?,
            })
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
                        anim::texture_format(cursor).map(|x| Some(x))
                    }
                    None => Ok(None),
                }
            }).collect();
        }
        if let Some(Some(img_ref)) = self.image_ref {
            match self.location {
                FileLocation::Multiple(_, mainsd) => mainsd.texture_formats(img_ref as usize),
                FileLocation::Separate(..) => {
                    warn!("Ref in HD sprite??");
                    return Vec::new()
                }
                FileLocation::DdsGrp(..) => {
                    warn!("Ref in grp??");
                    return Vec::new()
                }
            }
        } else {
            match self.location {
                FileLocation::Multiple(sprite, mainsd) => mainsd.texture_formats(sprite),
                FileLocation::Separate(file) => file.texture_formats(0),
                FileLocation::DdsGrp(grp) => grp.texture_formats(),
            }
        }
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

    pub fn image_ref(&self) -> Option<u32> {
        if let Some(img_ref) = self.image_ref {
            return img_ref;
        }
        self.location.image_ref()
    }
}

impl<'a> FileLocation<'a> {
    pub fn values_or_ref(&self) -> Option<anim::ValuesOrRef> {
        Some(match *self {
            FileLocation::Multiple(sprite, mainsd) => mainsd.values_or_ref(sprite),
            FileLocation::Separate(file) => anim::ValuesOrRef::Values(file.sprite_values(0)?),
            FileLocation::DdsGrp(_) => return None,
        })
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

    pub fn image_ref(&self) -> Option<u32> {
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
    anim::Anim::read(file)
}

impl Files {
    pub fn empty() -> Files {
        Files {
            sprites: Vec::new(),
            mainsd_anim: None,
            root_path: None,
            open_files: OpenFiles::new(),
            edits: HashMap::new(),
        }
    }

    pub fn root_path(&self) -> Option<&Path> {
        self.root_path.as_ref().map(|x| &**x)
    }

    /// Tries to load an entire anim tree structure, if files seem to be laid out like that.
    /// Otherwise just opens the file given.
    pub fn init(one_filename: &Path) -> Result<Files, Error> {
        if let Some(root) = file_root_from_file(one_filename) {
            let mainsd_path = root.join("SD/mainSD.anim");
            let mainsd_anim = {
                if mainsd_path.exists() && mainsd_path.is_file() {
                    let mainsd = load_mainsd(&mainsd_path)?;
                    Some((mainsd_path, mainsd))
                } else {
                    None
                }
            };
            let sprite_count = mainsd_anim.as_ref().map(|x| x.1.sprites().len())
                .unwrap_or(999);
            Ok(Files {
                sprites: (0..sprite_count as u32).map(|i| {
                    let hd_filename = |i: u32, prefix: &str| {
                        let mut dir: PathBuf = root.into();
                        dir.push(prefix);
                        dir.push(format!("main_{:03}.anim", i));
                        dir
                    };
                    SpriteFiles::AnimSet(AnimFiles {
                        image_id: i,
                        hd_filename: hd_filename(i, "anim"),
                        hd2_filename: hd_filename(i, "HD2/anim"),
                        name: image_name(i),
                    })
                }).collect(),
                mainsd_anim,
                root_path: Some(root.into()),
                open_files: OpenFiles::new(),
                edits: HashMap::new(),
            })
        } else {
            match one_filename.extension().map(|x| x == "anim").unwrap_or(false) {
                true => {
                    let mainsd = load_mainsd(one_filename)?;
                    Ok(Files {
                        sprites: (0..mainsd.sprites().len()).map(|i| {
                            SpriteFiles::MainSdOnly {
                                image_id: i as u32,
                                name: image_name(i as u32),
                            }
                        }).collect(),
                        mainsd_anim: Some((one_filename.into(), mainsd)),
                        root_path: Some(one_filename.into()),
                        open_files: OpenFiles::new(),
                        edits: HashMap::new(),
                    })
                }
                false => {
                    Ok(Files {
                        sprites: vec![SpriteFiles::DdsGrp(one_filename.into())],
                        mainsd_anim: None,
                        root_path: Some(one_filename.into()),
                        open_files: OpenFiles::new(),
                        edits: HashMap::new(),
                    })
                }
            }
        }
    }

    pub fn file<'a>(
        &'a mut self,
        sprite: usize,
        ty: SpriteType
    ) -> Result<Option<File<'a>>, Error> {
        let edit_values = self.edits.get(&(sprite, ty));
        let location;
        let sprite_values;
        let frames;
        let textures;
        let mut texture_sizes = None;
        let mut grp_textures = None;
        let image_ref;
        let path = file_path(self.mainsd_anim.as_ref().map(|x| &*x.0), &self.sprites, sprite, ty);
        let path = match path {
            Some(s) => s,
            None => {
                warn!("No path for {}/{:?}?", sprite, ty);
                return Ok(None);
            }
        };

        match edit_values {
            Some(x) => match *x {
                Edit::Values(ref x) => {
                    let loc = file_location(
                        self.mainsd_anim.as_ref().map(|x| &x.1),
                        &mut self.open_files,
                        &self.sprites,
                        sprite,
                        ty,
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
                        None | Some(Edit::Grp(_)) => {
                            sprite_values = mainsd.sprite_values(img_id as usize);
                            frames = mainsd.frames(img_id as usize);
                            textures = None;
                            texture_sizes = mainsd.texture_sizes(img_id as usize);
                        }
                    }
                    location = FileLocation::Multiple(sprite, mainsd);
                }
                Edit::Grp(ref grp_edits) => {
                    let loc = file_location(
                        self.mainsd_anim.as_ref().map(|x| &x.1),
                        &mut self.open_files,
                        &self.sprites,
                        sprite,
                        ty,
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
            sprite_values,
            frames,
            textures,
            texture_sizes,
            grp_textures,
            image_ref,
            path,
        }))
    }

    pub fn close_opened(&mut self) {
        self.open_files.clear();
    }

    pub fn sprites(&self) -> &[SpriteFiles] {
        &self.sprites[..]
    }

    pub fn mainsd(&self) -> Option<&anim::Anim> {
        self.mainsd_anim.as_ref().map(|x| &x.1)
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
                        unk2: !0,
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

    pub fn set_ref_img(&mut self, sprite: usize, ty: SpriteType, image: u32) {
        let unchanged = {
            let file = file_location(
                self.mainsd_anim.as_ref().map(|x| &x.1),
                &mut self.open_files,
                &self.sprites,
                sprite,
                ty,
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

    pub fn set_tex_changes(&mut self, sprite: usize, ty: SpriteType, changes: anim::TexChanges) {
        let entry = self.edits.entry((sprite, ty));
        let file = file_location(
            self.mainsd_anim.as_ref().map(|x| &x.1),
            &mut self.open_files,
            &self.sprites,
            sprite,
            ty,
        ).ok().and_then(|x| x);
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
        if let Edit::Values(ref mut vals) = values {
            vals.tex_changes = Some(changes);
        }
    }

    pub fn set_grp_changes(
        &mut self,
        sprite: usize,
        ty: SpriteType,
        changes: Vec<(ddsgrp::Frame, Vec<u8>)>,
    ) {
        let entry = self.edits.entry((sprite, ty));
        let values = entry.or_insert_with(|| Edit::Grp(Vec::new()));
        *values = Edit::Grp(changes);
    }

    /// Does nothing if sprite/ty is currently Ref
    pub fn update_file<F>(&mut self, sprite: usize, ty: SpriteType, fun: F)
    where F: FnOnce(&mut SpriteValues)
    {
        let unchanged = {
            let entry = self.edits.entry((sprite, ty));
            let file = file_location(
                self.mainsd_anim.as_ref().map(|x| &x.1),
                &mut self.open_files,
                &self.sprites,
                sprite,
                ty,
            ).ok().and_then(|x| x);
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
        !self.edits.is_empty()
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
                            return Err(format_err!("No path for sprite {}/{:?}", sprite, ty));
                        }
                    };
                    let file = fs::File::open(path)?;
                    let out_path = temp_file_path(&path);
                    let mut out = fs::File::create(&out_path).with_context(|_| {
                        format!("Unable to create {}", out_path.to_string_lossy())
                    })?;
                    temp_files.push((out_path, path.into()));
                    if is_anim {
                        let anim = anim::Anim::read(file)?;

                        let layer_names = anim.layer_names();
                        let edit = match *edit {
                            Edit::Ref(_) => {
                                return Err(
                                    format_err!(
                                        "Ref edit for a separate sprite {}/{:?}",
                                        sprite, ty,
                                    )
                                );
                            }
                            Edit::Values(ref v) => v,
                            Edit::Grp(..) => unreachable!(),
                        };
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
                            anim.scale(),
                            1,
                            &layer_names,
                            &[(0, anim::ValuesOrRef::Values(edit.values))],
                            tex_edits
                        )?;
                    } else {
                        let scale = match ty {
                            SpriteType::Sd => 1,
                            SpriteType::Hd2 => 1,
                            SpriteType::Hd => 4,
                        };
                        if let Edit::Grp(ref edits) = *edit {
                            ddsgrp::DdsGrp::write(&mut out, scale, &edits)?;
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
                    let sprite_count = sd.sprites().len() as u16;
                    let layer_names = sd.layer_names();
                    let out_path = temp_file_path(&sd_path);
                    let mut out = fs::File::create(&out_path).with_context(|_| {
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
            self.open_files.clear();
            // Closing mainsd
            let sd_path = self.mainsd_anim.take().map(|x| x.0);
            for (temp, dest) in temp_files {
                result = fs::rename(temp, dest);
                if result.is_err() {
                    break;
                }
            }
            if let Some(sd_path) = sd_path {
                let mainsd = load_mainsd(&sd_path)?;
                self.mainsd_anim = Some((sd_path, mainsd));
            }
        }
        if result.is_ok() {
            self.edits.clear();
        }

        Ok(result?)
    }
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
    ty: SpriteType
) -> Result<Option<FileLocation<'a>>, Error> {
    match sprites.get(sprite) {
        Some(&SpriteFiles::AnimSet(_)) | Some(&SpriteFiles::MainSdOnly { .. }) => {
            match ty {
                SpriteType::Sd => {
                    Ok(mainsd_anim.map(|x| FileLocation::Multiple(sprite, x)))
                }
                SpriteType::Hd | SpriteType::Hd2 => {
                    file_location_hd(open_files, sprites, sprite, ty)
                }
            }
        }
        Some(&SpriteFiles::DdsGrp(ref f)) => {
            let file = fs::File::open(f)?;
            let grp = ddsgrp::DdsGrp::read(file)?;
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

    if path.is_file() {
        Some(&path)
    } else {
        None
    }
}

fn file_location_hd<'a>(
    open_files: &'a mut OpenFiles,
    sprites: &[SpriteFiles],
    sprite: usize,
    ty: SpriteType,
) -> Result<Option<FileLocation<'a>>, Error> {
    if let Some(index) = open_files.anim.iter().position(|x| x.1 == sprite && x.2 == ty) {
        return Ok(Some(FileLocation::Separate(&open_files.anim[index].0)));
    }
    let path = match separate_file_path(sprites, sprite, ty) {
        Some(p) => p,
        None => return Ok(None),
    };
    let file = fs::File::open(path)?;

    let anim = anim::Anim::read(file)?;
    open_files.anim.push((anim, sprite, ty));
    Ok(Some(FileLocation::Separate(&open_files.anim.last_mut().unwrap().0)))
}

fn file_root_from_file(file: &Path) -> Option<&Path> {
    let filename = file.file_name()
        .and_then(|f| f.to_str())?;
    let parent_path = file.parent()?;
    let parent = parent_path.file_name()?.to_str()?;
    if filename.eq_ignore_ascii_case("mainsd.anim") {
        if parent.eq_ignore_ascii_case("sd") {
            parent_path.parent()
        } else {
            None
        }
    } else if filename.ends_with(".anim") && filename.starts_with("main_") {
        // Nice that the valid images go from 000 to 999
        let is_standard_name = filename.len() == 13 &&
            filename.get(5..8).map(|x| x.chars().all(|x| x.is_numeric())).unwrap_or(false);
        if is_standard_name && parent.eq_ignore_ascii_case("anim") {
            let l2 = parent_path.parent()?;
            if l2.file_name()?.to_str()?.eq_ignore_ascii_case("hd2") {
                l2.parent()
            } else {
                Some(l2)
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn image_name(image_id: u32) -> String {
    format!("#{:03}", image_id)
}

#[test]
fn test_file_root_from_file() {
    let root = Path::new("a/b/c");
    assert_eq!(file_root_from_file(Path::new("a/b/c/sd/mainsd.anim")), Some(root));
    assert_eq!(file_root_from_file(Path::new("a/b/c/anim/main_000.anim")), Some(root));
    assert_eq!(file_root_from_file(Path::new("a/b/c/hd2/anim/main_000.anim")), Some(root));
    assert_eq!(file_root_from_file(Path::new("a/b/c/mainsd.anim")), None);
    assert_eq!(file_root_from_file(Path::new("a/b/c/a/main_000.anim")), None);
    assert_eq!(file_root_from_file(Path::new("a/b/c/anim/main_nonstandard_name_000.anim")), None);
    assert_eq!(file_root_from_file(Path::new("a/b/c/anim/main_1000.anim")), None);
}
