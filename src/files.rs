use std::fs;
use std::path::{Path, PathBuf};

use failure::Error;

use anim;
use ::SpriteType;

pub struct Files {
    sprites: Vec<SpriteFiles>,
    mainsd_anim: Option<(PathBuf, anim::MainSd)>,
    root_path: Option<PathBuf>,
    open_files: Vec<(anim::Anim, usize, bool)>,
}

#[derive(Debug, Clone)]
pub enum SpriteFiles {
    AnimSet(AnimFiles),
    SingleFile(PathBuf),
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
}

pub enum FileLocation<'a> {
    MainSd(usize, &'a mut anim::MainSd),
    Separate(&'a mut anim::Anim),
}

impl<'a> File<'a> {
    pub fn texture(&mut self, layer: usize) -> Result<anim::RgbaTexture, Error> {
        Ok(match self.location {
            FileLocation::MainSd(sprite, ref mut mainsd) => mainsd.texture(sprite, layer)?,
            FileLocation::Separate(ref mut file) => file.texture(layer)?,
        })
    }

    pub fn sprite_data(&self) -> Option<&anim::SpriteData> {
        match self.location {
            FileLocation::MainSd(sprite, ref mainsd) => mainsd.sprite_data(sprite),
            FileLocation::Separate(ref file) => Some(file.sprite_data()),
        }
    }

    pub fn layer_names(&self) -> &[String] {
        match self.location {
            FileLocation::MainSd(_sprite, ref mainsd) => mainsd.layer_names(),
            FileLocation::Separate(ref file) => file.layer_names(),
        }
    }

    pub fn image_ref(&self) -> Option<u32> {
        match self.location {
            FileLocation::MainSd(sprite, ref mainsd) => {
                mainsd.sprites().get(sprite).and_then(|x| match *x {
                    anim::SpriteType::Ref(x) => Some(x),
                    anim::SpriteType::Data(_) => None,
                })
            }
            FileLocation::Separate(..) => None,
        }
    }
}

fn load_mainsd(path: &Path) -> Result<anim::MainSd, Error> {
    let file = fs::File::open(path)?;
    anim::MainSd::read(file)
}

impl Files {
    pub fn empty() -> Files {
        Files {
            sprites: Vec::new(),
            mainsd_anim: None,
            root_path: None,
            open_files: Vec::new(),
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
                open_files: Vec::new(),
            })
        } else {
            match is_mainsd(one_filename) {
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
                        open_files: Vec::new(),
                    })
                }
                false => {
                    Ok(Files {
                        sprites: vec![SpriteFiles::SingleFile(one_filename.into())],
                        mainsd_anim: None,
                        root_path: Some(one_filename.into()),
                        open_files: Vec::new(),
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
        match ty {
            SpriteType::Sd => Ok(self.mainsd_anim.as_mut().map(|x| File {
                location: FileLocation::MainSd(sprite, &mut x.1)
            })),
            SpriteType::Hd => self.hd(sprite),
            SpriteType::Hd2 => self.hd2(sprite),
        }
    }

    pub fn close_opened(&mut self) {
        self.open_files.clear();
    }

    pub fn sprites(&self) -> &[SpriteFiles] {
        &self.sprites[..]
    }

    pub fn mainsd(&self) -> Option<&anim::MainSd> {
        self.mainsd_anim.as_ref().map(|x| &x.1)
    }

    pub fn has_sprite(&mut self, sprite: usize, ty: SpriteType) -> bool {
        self.sprites.get(sprite)
            .map(|s| match *s {
                SpriteFiles::AnimSet(ref files) => match ty {
                    SpriteType::Hd => files.hd_filename.is_file(),
                    SpriteType::Hd2 => files.hd2_filename.is_file(),
                    SpriteType::Sd => self.mainsd_anim.is_some(),
                },
                _ => false,
            }).unwrap_or(false)
    }

    fn hd_or_hd2<'a>(&'a mut self, sprite: usize, hd2: bool) -> Result<Option<File<'a>>, Error> {
        if let Some(index) = self.open_files.iter().position(|x| x.1 == sprite && x.2 == hd2) {
            return Ok(Some(File {
                location: FileLocation::Separate(&mut self.open_files[index].0),
            }));
        }
        let file = {
            let path = self.sprites.get(sprite)
                .and_then(|s| match *s {
                    SpriteFiles::AnimSet(ref files) => match hd2 {
                        false => Some(&files.hd_filename),
                        true => Some(&files.hd2_filename),
                    },
                    _ => None,
                });
            let path = match path {
                Some(s) => match s.is_file() {
                    true => s,
                    false => return Ok(None),
                },
                None => return Ok(None)
            };
            fs::File::open(path)?
        };
        let anim = anim::Anim::read(file)?;
        self.open_files.push((anim, sprite, hd2));
        Ok(Some(File {
            location: FileLocation::Separate(&mut self.open_files.last_mut().unwrap().0)
        }))
    }

    pub fn hd<'a>(&'a mut self, sprite: usize) -> Result<Option<File<'a>>, Error> {
        self.hd_or_hd2(sprite, false)
    }

    pub fn hd2<'a>(&'a mut self, sprite: usize) -> Result<Option<File<'a>>, Error> {
        self.hd_or_hd2(sprite, true)
    }
}

fn is_mainsd(path: &Path) -> bool {
    path.file_name()
        .and_then(|f| f.to_str())
        .map(|f| f.eq_ignore_ascii_case("mainsd.anim"))
        .unwrap_or(false)
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
        if parent.eq_ignore_ascii_case("anim") {
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
}
