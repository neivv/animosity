use std::cell::{Cell, RefCell};
use std::fs;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;

use gio::prelude::*;
use gtk::prelude::*;
use serde_derive::{Deserialize, Serialize};

use crate::anim;
use crate::combo_box_enum::ComboBoxEnum;
use crate::frame_export_dialog::SavedCheckbox;
use crate::frame_import;
use crate::frame_info::{FrameInfo, parse_frame_info};
use crate::int_entry::{IntSize, IntEntry};
use crate::select_dir::{self, read_config_entry, set_config_entry};
use crate::{
    label_section, lookup_action, error_msg_box, info_msg_box, SpriteInfo, SpriteType, Error,
    error_from_panic,
};

use crate::ui_helpers::*;

enum Progress {
    Done(Result<u32, Error>),
    Progress(f32),
}

pub fn frame_import_dialog(sprite_info: &Arc<SpriteInfo>, parent: &gtk::ApplicationWindow) {
    let tex_id = sprite_info.tex_id();
    let mut files = match sprite_info.files.try_lock() {
        Ok(o) => o,
        _ => return,
    };
    let layer_names;
    let tex_formats;
    let is_anim;
    let path;
    let ddsgrp_linked_grp;
    let grp_scale;
    let had_palette;
    {
        let file = match files.file(tex_id.0, tex_id.1) {
            Ok(Some(o)) => o,
            _ => return,
        };
        is_anim = file.is_anim();
        layer_names = file.layer_names().into_owned();
        tex_formats = file.texture_formats();
        had_palette = file.palette().is_some();
        path = file.path().to_owned();
        ddsgrp_linked_grp = file.ddsgrp_linked_grp();
        grp_scale = file.grp().map(|x| x.scale);
    }
    let is_sd_anim = is_anim && tex_id.1 == SpriteType::Sd;
    let has_hd2 = if is_anim && tex_id.1 != SpriteType::Sd {
        let other_ty = match tex_id.1 {
            SpriteType::Hd => SpriteType::Hd2,
            SpriteType::Hd2 | _ => SpriteType::Hd,
        };
        match files.file(tex_id.0, other_ty) {
            Ok(Some(_)) => true,
            _ => false,
        }
    } else {
        false
    };

    let sprite_type_str = match tex_id.1 {
        SpriteType::Hd => "hd",
        SpriteType::Hd2 => "hd2",
        SpriteType::Sd => "sd",
    };

    let window = gtk::Window::new(gtk::WindowType::Toplevel);

    let framedef_status = gtk::Label::new(None);
    framedef_status.set_halign(gtk::Align::Start);
    let framedef;
    let framedef_scale;

    let mut hd2_framedef_filename = None;
    let mut hd2_framedef = None;
    let mut hd2_framedef_bx = None;
    let hd2_framedef_status = gtk::Label::new(None);
    hd2_framedef_status.set_halign(gtk::Align::Start);
    let hd2_scale = ScaleChooser::new(format!("import_scale_{}_hd2", sprite_type_str));

    let framedef_bx = if is_anim {
        framedef = Rc::new(
            select_dir::SelectFile::new(&window, "import_frames", "Text files", "*.json")
        );
        framedef_scale = ScaleChooser::new(format!("import_scale_{}", sprite_type_str));
        let framedef_inner_bx = box_vertical(&[
            framedef.widget(),
            framedef_scale.widget(),
            &framedef_status,
        ]);
        if has_hd2 {
            let hd2_framedef_ = Rc::new(select_dir::SelectFile::new(
                &window, "import_frames_hd2", "Text files", "*.json"
            ));
            let bx = box_vertical(&[
                hd2_framedef_.widget(),
                hd2_scale.widget(),
                &hd2_framedef_status,
            ]);
            hd2_framedef_filename = Some(hd2_framedef_.text()).filter(|x| !x.is_empty());
            hd2_framedef = Some(hd2_framedef_);
            hd2_framedef_bx = Some(label_section("HD2 Frame info file", &bx));
            label_section("HD Frame info file", &framedef_inner_bx)
        } else {
            label_section("Frame info file", &framedef_inner_bx)
        }
    } else {
        framedef = Rc::new(
            select_dir::SelectFile::new(&window, "import_grp_frames", "Text files", "*.json")
        );
        framedef_scale = ScaleChooser::new(format!("import_scale_grp_{}", sprite_type_str));
        let inner_bx = box_vertical(&[
            framedef.widget(),
            framedef_scale.widget(),
            &framedef_status,
        ]);
        label_section("Frame info file", &inner_bx)
    };
    let framedef_filename = Some(framedef.text()).filter(|x| !x.is_empty());

    let mut checkboxes = Vec::with_capacity(layer_names.len());
    let mut grp_format = None;
    static FORMATS_ANIM: &[(anim::TextureFormat, &str)] = &[
        (anim::TextureFormat::Dxt1, "DXT1"),
        (anim::TextureFormat::Dxt5, "DXT5"),
        (anim::TextureFormat::Monochrome, "Monochrome"),
    ];
    static FORMATS_DDSGRP: &[(Option<anim::TextureFormat>, &str)] = &[
        (Some(anim::TextureFormat::Dxt1), "DXT1"),
        (Some(anim::TextureFormat::Dxt5), "DXT5"),
        (Some(anim::TextureFormat::Monochrome), "Monochrome"),
        (None, "Paletted"),
    ];
    let layers_bx = if is_anim {
        let grid = gtk::Grid::new();
        grid.set_column_spacing(5);
        grid.set_row_spacing(5);

        for (i, name) in layer_names.iter().enumerate() {
            let row = i as i32 + 1;

            let checkbox = gtk::CheckButton::new();
            grid.attach(&checkbox, 0, row, 1, 1);
            checkbox.set_sensitive(false);

            let label = gtk::Label::new(Some(&**name));
            grid.attach(&label, 1, row, 1, 1);
            label.set_halign(gtk::Align::Start);
            let format = ComboBoxEnum::new(FORMATS_ANIM);
            grid.attach(format.widget(), 2, row, 1, 1);

            checkboxes.push((checkbox, format));
        }
        label_section("Layers", &grid)
    } else {
        let format = ComboBoxEnum::new(FORMATS_DDSGRP);
        if had_palette {
            format.set_active(&None);
        } else {
            if let Some(Ok(Some(tex_f))) = tex_formats.get(0) {
                format.set_active(&Some(*tex_f));
            }
        }

        let bx = label_section("Encode format", format.widget());
        grp_format = Some(format);
        bx
    };
    layers_bx.set_tooltip_text(Some(encoding_tooltip_text()));

    // Checkbox to create cmdicons / wirefram / tranwire grp for sd
    let ddsgrp_make_linked_grp;
    if let Some(ref linked_grp_path) = ddsgrp_linked_grp {
        let check = SavedCheckbox::new_with_default(
            "ddsgrp_make_sd_grp",
            &format!("Create {}", linked_grp_path.display()),
            true,
        );
        check.widget().set_tooltip_text(Some(&format!(
            "This SD .dds.grp file determines frame dimensions from {}.\n\
            If this setting is enabled, a new .grp will be created with dimensions \
            from imported frames.\n\
            \n\
            Keeping this setting enabled is recommended.",
            linked_grp_path.display(),
        )));
        ddsgrp_make_linked_grp = Some(check);
    } else {
        ddsgrp_make_linked_grp = None;
    }

    let grp_scale_entry;
    let grp_scale_bx;
    if is_anim {
        grp_scale_entry = None;
        grp_scale_bx = None;
    } else {
        let entry = IntEntry::new(IntSize::Int8);
        entry.set_value(grp_scale.unwrap_or(0).into());
        let labeled = label_section("Ingame scale", &entry.frame);
        labeled.set_tooltip_text(Some("\
            Selects the scale value saved within file.\n\
            Generally the correct setting is to use 1 for SD, 2 for HD2, and 4 for HD,\n\
            which should be what this value gets set automatically anyway.\n\
            \n\
            (It is mainly visible to let you fix any issues if the file happened to have wrong \
            values in the first place)"));
        grp_scale_bx = Some(labeled);
        grp_scale_entry = Some(entry);
    };

    let sd_anim_grp_bx;
    let sd_anim_grp_radios;
    let sd_anim_grp_filename;
    if is_sd_anim {
        let no_grp = gtk::RadioButton::with_label("Don't create");
        let default_name = gtk::RadioButton::with_label_from_widget(&no_grp, "Default path");
        let default_grp_path = files.image_grp_path(tex_id.0);
        if default_grp_path.is_some() {
            default_name.set_tooltip_text(Some(
                "Uses a path read from images.dat and images.tbl"
            ));
        } else {
            default_name.set_sensitive(false);
            default_name.set_tooltip_text(Some("\
                Uses a path derived from images.dat and images.tbl\n\
                (Unable to get GRP path)"));
        }
        let custom_name = gtk::RadioButton::with_label_from_widget(&no_grp, "Custom path");
        let (filename_entry, filename_frame) = crate::int_entry::entry();
        filename_entry.set_sensitive(false);
        let bx = box_vertical(&[
            &no_grp,
            &default_name,
            &custom_name,
            &filename_frame,
        ]);
        let filename_entry2 = filename_entry.clone();
        no_grp.connect_toggled(move |this| {
            if this.get_active() {
                filename_entry2.set_sensitive(false);
                filename_entry2.set_text("");
            }
        });
        let filename_entry2 = filename_entry.clone();
        default_name.connect_toggled(move |this| {
            if this.get_active() {
                filename_entry2.set_sensitive(false);
                if let Some(ref path) = default_grp_path {
                    filename_entry2.set_text(path);
                }
            }
        });
        let filename_entry2 = filename_entry.clone();
        custom_name.connect_toggled(move |this| {
            if this.get_active() {
                filename_entry2.set_sensitive(true);
                let name = read_config_entry("sd_grp_custom_name")
                    .unwrap_or_else(|| "something.grp".into());
                filename_entry2.set_text(&name);
            }
        });
        sd_anim_grp_filename = Some(filename_entry);
        sd_anim_grp_bx = Some(label_section("GRP creation", &bx));
        sd_anim_grp_radios = vec![no_grp, default_name, custom_name];
        let last_selected = read_config_entry("sd_grp_last_selected")
            .and_then(|x| x.parse::<u8>().ok())
            .and_then(|x| sd_anim_grp_radios.get(x as usize))
            .or_else(|| sd_anim_grp_radios.get(0));
        if let Some(last_selected) = last_selected {
            last_selected.set_active(true);
        }
    } else {
        sd_anim_grp_bx = None;
        sd_anim_grp_radios = vec![];
        sd_anim_grp_filename = None;
    }

    let button_bx = gtk::Box::new(gtk::Orientation::Horizontal, 15);
    let ok_button = gtk::Button::with_label("Import");
    ok_button.set_sensitive(true);
    let cancel_button = gtk::Button::with_label("Cancel");
    let w = window.clone();
    cancel_button.connect_clicked(move |_| {
        w.close();
    });
    let sprite_info = sprite_info.clone();
    let w = window.clone();
    let frame_info: Rc<RefCell<Option<FrameInfo>>> = Rc::new(RefCell::new(None));
    let hd2_frame_info: Rc<RefCell<Option<FrameInfo>>> = Rc::new(RefCell::new(None));
    let fi = frame_info.clone();
    let hd2_fi = hd2_frame_info.clone();
    let fi_entry = framedef.clone();
    let hd2_fi_entry = hd2_framedef.clone();
    let checkboxes = Rc::new(checkboxes);
    let checkboxes2 = checkboxes.clone();

    let progress = gtk::ProgressBar::new();
    let progress2 = progress.clone();
    let waiting_for_thread = Rc::new(Cell::new(false));
    let waiting_for_thread2 = waiting_for_thread.clone();
    let rest_of_ui: Rc<RefCell<Vec<gtk::Box>>> = Rc::new(RefCell::new(Vec::new()));
    let rest_of_ui2 = rest_of_ui.clone();
    let ddsgrp_make_linked_grp2 = ddsgrp_make_linked_grp.clone();
    let files_root: Option<PathBuf> = files.root_path().map(|x| x.into());
    ok_button.connect_clicked(move |_| {
        if waiting_for_thread.get() {
            return;
        }
        // Used for grps
        let dir = {
            let mut buf: PathBuf = fi_entry.text().into();
            buf.pop();
            if !buf.is_dir() {
                return;
            }
            buf
        };
        let hd2_dir = hd2_fi_entry.as_ref().and_then(|s| {
            let mut buf: PathBuf = s.text().into();
            buf.pop();
            if !buf.is_dir() {
                None
            } else {
                Some(buf)
            }
        });
        let fi = fi.borrow();
        let frame_info = match *fi {
            Some(ref s) => (*s).clone(),
            None => return,
        };
        let (send, recv) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);
        let files_arc = sprite_info.files.clone();
        if is_anim {
            let formats = checkboxes2.iter().map(|x| {
                Ok(match x.1.get_active() {
                    Some(x) => x,
                    None => {
                        if x.0.get_active() {
                            return Err(());
                        }
                        // Just a dummy value since the layer is unused
                        anim::TextureFormat::Monochrome
                    }
                })
            }).collect::<Result<Vec<_>, ()>>();
            let formats = match formats {
                Ok(o) => o,
                Err(()) => {
                    error_msg_box(&w, "Format not specified for every layer");
                    return;
                }
            };
            let hd2_fi = (*hd2_fi.borrow()).clone();
            let frame_scale = match framedef_scale.get_active() {
                Some(s) => s.to_float(),
                None => {
                    error_msg_box(&w, "Scale not set");
                    return;
                }
            };
            let hd2_scale = if hd2_fi.is_some() {
                match hd2_scale.get_active() {
                    Some(s) => Some(s.to_float()),
                    None => {
                        error_msg_box(&w, "HD2 Scale not set");
                        return;
                    }
                }
            } else {
                None
            };

            let frame_count = if hd2_fi.is_some() { 2 } else { 1 } *
                frame_info.layers.len() as u32 * frame_info.frame_count;

            // Hackfix: If importing SD and there's a layer named teamcolor somewhere
            // else than index 1, move it to index 1 (Since HD sprites usually have it
            // at index 2)
            let frame_info = if tex_id.1 == SpriteType::Sd {
                let mut frame_info = frame_info;
                let teamcolor_index = frame_info.layers.iter()
                    .position(|x| x.id > 1 && x.filename_prefix.ends_with("teamcolor"))
                    .filter(|&pos| pos > 1);
                if let Some(index) = teamcolor_index {
                    frame_info.layers = vec![
                        frame_info.layers[0].clone(),
                        frame_info.layers[index].clone(),
                    ];
                    frame_info.layers[0].id = 0;
                    frame_info.layers[1].id = 1;
                    for mfi in &mut frame_info.multi_frame_images {
                        if mfi.layer == 1 {
                            mfi.layer = 2;
                        } else if mfi.layer == index as u32 {
                            mfi.layer = 1;
                        }
                    }
                }
                frame_info
            } else {
                frame_info
            };

            let active_grp_radio_index = sd_anim_grp_radios.iter().position(|x| x.get_active());
            let grp_filename = if let Some(index) = active_grp_radio_index {
                set_config_entry("sd_grp_last_selected", &index.to_string());
                if index == 2 {
                    if let Some(ref entry) = sd_anim_grp_filename {
                        set_config_entry("sd_grp_custom_name", &entry.get_text());
                    }
                }
                if index == 0 {
                    None
                } else {
                    sd_anim_grp_filename.as_ref()
                        .and_then(|entry| {
                            let text = entry.get_text();
                            Some(files_root.as_ref()?.join(&*text))
                        })
                }
            } else {
                None
            };
            let layer_names = layer_names.clone();
            std::thread::spawn(move || {
                let send2 = send.clone();
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(move || {
                    let mut files = files_arc.lock();
                    frame_import::import_frames(
                        &mut files,
                        &frame_info,
                        hd2_fi.as_ref(),
                        &dir,
                        hd2_dir.as_ref().map(|x| &**x),
                        frame_scale,
                        hd2_scale,
                        &formats,
                        &layer_names,
                        tex_id.0,
                        tex_id.1,
                        grp_filename.as_ref().map(|x| &**x),
                        |step| send.send(Progress::Progress(step)).unwrap(),
                    )
                })).unwrap_or_else(|e| Err(error_from_panic(e)));
                let _ = send2.send(Progress::Done(result.map(|()| frame_count)));
            });
        } else {
            let format = match grp_format {
                Some(ref s) => s.get_active(),
                None => return,
            };
            let format = match format {
                Some(o) => o,
                None => {
                    error_msg_box(&w, "Format not specified");
                    return;
                }
            };
            let scale = grp_scale_entry.as_ref().unwrap().get_value() as u8;
            let frame_scale = match framedef_scale.get_active() {
                Some(s) => s.to_float(),
                None => {
                    error_msg_box(&w, "Scale not set");
                    return;
                }
            };
            let make_linked_grp = ddsgrp_make_linked_grp2
                .as_ref()
                .map(|x| x.get_active())
                .unwrap_or(false);
            let linked_grp_path = match make_linked_grp {
                true => ddsgrp_linked_grp.clone(),
                false => None,
            };
            std::thread::spawn(move || {
                let send2 = send.clone();
                let frame_count = frame_info.frame_count;
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(move || {
                    if let Some(ref path) = linked_grp_path {
                        if let Some(dir) = path.parent() {
                            fs::create_dir_all(dir)?;
                        }
                    }
                    let mut files = files_arc.lock();
                    frame_import::import_frames_grp(
                        &mut files,
                        &frame_info,
                        &dir,
                        frame_scale,
                        format,
                        tex_id.0,
                        scale,
                        linked_grp_path.as_deref(),
                        |step| send.send(Progress::Progress(step)).unwrap(),
                    )
                })).unwrap_or_else(|e| Err(error_from_panic(e)));
                let _ = send2.send(Progress::Done(result.map(|()| frame_count)));
            });
        }
        let rest_of_ui = rest_of_ui2.clone();
        let window = w.clone();
        let progress = progress2.clone();
        waiting_for_thread.set(true);
        for part in rest_of_ui.borrow().iter() {
            part.set_sensitive(false);
        }
        let waiting_for_thread = waiting_for_thread.clone();
        let sprite_info = sprite_info.clone();
        let files_arc = sprite_info.files.clone();
        recv.attach(None, move |status| match status {
            Progress::Done(result) => {
                waiting_for_thread.set(false);
                for part in rest_of_ui.borrow().iter() {
                    part.set_sensitive(true);
                }
                match result {
                    Ok(frame_count) => {
                        let mut files = files_arc.lock();
                        sprite_info.draw_clear_all();
                        if let Ok(mut file) = files.file(tex_id.0, tex_id.1) {
                            sprite_info.changed_ty(tex_id, &mut file);
                        }
                        drop(files);
                        if let Some(a) = lookup_action(&sprite_info.sprite_actions, "is_dirty") {
                            a.activate(Some(&true.to_variant()));
                        }

                        info_msg_box(&window, format!("Imported {} frames", frame_count));
                        sprite_info.lighting.select_sprite(tex_id.0);
                        window.close();
                    }
                    Err(e) => {
                        let msg = format!("Unable to import frames: {:?}", e);
                        error_msg_box(&window, msg);
                    }
                }
                glib::Continue(false)
            }
            Progress::Progress(step) => {
                progress.set_fraction(step as f64);
                glib::Continue(true)
            }
        });
    });

    let ok = ok_button.clone();
    // The second entry is used for hd2
    let framedef_set = Rc::new(move |filename: &str, hd2: bool, status: &gtk::Label| {
        match parse_frame_info(Path::new(filename)) {
            Ok(o) => {
                ok.set_sensitive(true);
                for &(ref check, ref format) in checkboxes.iter() {
                    check.set_active(false);
                    format.set_sensitive(false);
                    format.clear_active();
                }
                for layer in &o.layers {
                    if let Some(&(ref check, ref format)) = checkboxes.get(layer.id as usize) {
                        check.set_active(true);
                        format.set_sensitive(true);
                        let tex_f = tex_formats.get(layer.id as usize)
                            .and_then(|x| x.as_ref().ok())
                            .and_then(|x| x.as_ref());
                        if let Some(tex_f) = tex_f {
                            format.set_active(tex_f);
                        }
                    }
                }
                if hd2 {
                    *hd2_frame_info.borrow_mut() = Some(o);
                } else {
                    *frame_info.borrow_mut() = Some(o);
                }
                status.set_text("");
            }
            Err(e) => {
                ok.set_sensitive(false);
                for &(ref check, ref format) in checkboxes.iter() {
                    check.set_active(false);
                    format.set_sensitive(false);
                    format.clear_active();
                }
                let msg = format!("Frame info invalid: {:?}", e);
                if hd2 {
                    *hd2_frame_info.borrow_mut() = None;
                } else {
                    *frame_info.borrow_mut() = None;
                }
                status.set_text(&msg);
            }
        }
    });
    if let Some(filename) = framedef_filename {
        framedef_set(&filename, false, &framedef_status);
    }
    let fun = framedef_set.clone();
    framedef.on_change(move |filename| {
        fun(filename, false, &framedef_status);
    });
    if let Some(filename) = hd2_framedef_filename {
        framedef_set(&filename, true, &hd2_framedef_status);
    }
    if let Some(ref fdef) = hd2_framedef {
        let fun = framedef_set.clone();
        fdef.on_change(move |filename| {
            fun(filename, true, &hd2_framedef_status);
        });
    }

    button_bx.pack_end(&cancel_button, false, false, 0);
    button_bx.pack_end(&ok_button, false, false, 0);
    let rest_bx = gtk::Box::new(gtk::Orientation::Vertical, 10);
    rest_bx.pack_start(&framedef_bx, false, false, 0);
    if let Some(hd2) = hd2_framedef_bx {
        rest_bx.pack_start(&hd2, false, false, 0);
    }
    rest_bx.pack_start(&layers_bx, false, false, 0);
    if let Some(sd_grp) = ddsgrp_make_linked_grp {
        rest_bx.pack_start(sd_grp.widget(), false, false, 0);
    }
    if let Some(scale) = grp_scale_bx {
        rest_bx.pack_start(&scale, false, false, 0);
    }
    if let Some(sd_grp) = sd_anim_grp_bx {
        rest_bx.pack_start(&sd_grp, false, false, 0);
    }
    let bx = box_vertical(&[
        &rest_bx,
        &progress,
        &button_bx,
    ]);
    *rest_of_ui.borrow_mut() = vec![rest_bx, button_bx];
    window.add(&bx);
    window.set_border_width(10);
    window.set_property_default_width(350);
    if is_anim {
        window.set_title(&format!("Import frames for {:?} image {}", tex_id.1, tex_id.0));
    } else {
        if let Some(filename) = path.file_name() {
            window.set_title(&format!("Import frames of {}", filename.to_string_lossy()));
        }
    }
    window.connect_delete_event(move |_, _| {
        Inhibit(waiting_for_thread2.get())
    });
    window.set_modal(true);
    window.set_transient_for(Some(parent));
    window.show_all();
}

#[derive(Eq, PartialEq, Copy, Clone, Debug, Deserialize, Serialize)]
enum ScaleValue {
    Scale1,
    Scale050,
    Scale025,
    Scale2,
    Scale4,
}

impl ScaleValue {
    fn to_float(self) -> f32 {
        use self::ScaleValue::*;
        match self {
            Scale1 => 1.0,
            Scale050 => 0.5,
            Scale025 => 0.25,
            Scale2 => 2.0,
            Scale4 => 4.0,
        }
    }
}

struct ScaleChooser {
    bx: gtk::Box,
    combo_box: ComboBoxEnum<ScaleValue>,
}

impl ScaleChooser {
    fn new<S: Into<String>>(config_cache: S) -> ScaleChooser {
        use self::ScaleValue::*;
        static SCALES: &[(ScaleValue, &str)] = &[
            (Scale1, "1x"),
            (Scale050, "0.50x"),
            (Scale025, "0.25x"),
            (Scale2, "2x"),
            (Scale4, "4x"),
        ];
        let combo_box = ComboBoxEnum::new(SCALES);
        let config_cache = config_cache.into();
        let cached_value = read_config_entry(&config_cache)
            .and_then(|x| serde_json::from_str(&x).ok());
        if let Some(value) = cached_value {
            combo_box.set_active(&value)
        } else {
            combo_box.set_active(&ScaleValue::Scale1);
        }
        combo_box.connect_changed(move |new| {
            if let Some(new) = new {
                if let Ok(new) = serde_json::to_string(&new) {
                    set_config_entry(&config_cache, &new);
                }
            }
        });
        let bx = label_section("Scale", combo_box.widget());
        bx.set_tooltip_text(Some("\
            Down/upscales the PNG frames. 1x for no change.\n\
            \n\
            E.g.\n\
            To import HD-resolution frames to HD2, set scale to 0.50x,\n\
            to import SD-resolution frames to HD, set scale to 4x."));
        ScaleChooser {
            bx,
            combo_box,
        }
    }

    pub fn widget(&self) -> &gtk::Widget {
        self.bx.upcast_ref()
    }

    fn get_active(&self) -> Option<ScaleValue> {
        self.combo_box.get_active()
    }
}

pub fn encoding_tooltip_text() -> &'static str {
    "\
    Selects encoding format used for the graphics\n\
    \n\
    DXT1 is the most efficient format, using 4 bits per pixel.\n\
    Its drawback is that it only supports 1-bit transparency \
    (Each pixel is either fully transparent or fully opaque).\n\
    It is probably best choice unless you need partial transparency for explosion effects or \
    to reduce aliasing.\n\
    \n\
    DXT5 supports full transparency at cost of using twice as much memory (8bpp).\n\
    It should be used for explosions or similar effects which are partially see-through.\n\
    It is slightly less lossy than DXT1 around areas that have transparent pixels,\n\
    but the quality improvement is likely not noticeable.\n\
    \n\
    SC:R does not seem to have support for a lossless 32bpp RGBA file format that could \
    be used for higher quality images at cost of memory usage.\n\
    \n\
    Monochrome is intended to be used with player color mask layers.\n\
    It is lossless 8bpp format, and could take advantage of 256 values per pixel,\n\
    but the default shaders assume that values are either 0 or 255,\n\
    so Animosity will clip any input pixels to those values.
    \n\
    Paletted is another lossless 8bpp format, which is only (intented to be) \
    used by SD tileset .dds.vr4 files.\n\
    The imported image must have palette and every frame must be same size in order for \
    it to be usable."
}
