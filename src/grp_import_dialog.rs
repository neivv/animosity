use std::cell::{Cell, RefCell};
use std::rc::Rc;
use std::sync::Arc;

use gio::prelude::*;
use gtk::prelude::*;

use crate::anim;
use crate::combo_box_enum::ComboBoxEnum;
use crate::frame_export_dialog::SavedCheckbox;
use crate::frame_import;
use crate::frame_import_dialog;
use crate::grp_decode;
use crate::select_dir::{self};
use crate::{
    error_from_panic, label_section, lookup_action, error_msg_box, info_msg_box, SpriteInfo,
    Error,
};

use crate::ui_helpers::*;

#[derive(Eq, PartialEq, Copy, Clone)]
pub struct Palette(&'static [u8]);

mod palette {
    use super::Palette;

    pub static BADLANDS: Palette = Palette(include_bytes!("../palettes/badlands.wpe"));
    pub static BEXPL: Palette = Palette(include_bytes!("../palettes/bexpl.wpe"));
    pub static BFIRE: Palette = Palette(include_bytes!("../palettes/bfire.wpe"));
    pub static GFIRE: Palette = Palette(include_bytes!("../palettes/gfire.wpe"));
    pub static OFIRE: Palette = Palette(include_bytes!("../palettes/ofire.wpe"));
}

enum Progress {
    Done(Result<u32, Error>),
    Progress(f32),
}

pub fn grp_import_dialog(sprite_info: &Arc<SpriteInfo>, parent: &gtk::ApplicationWindow) {
    let tex_id = sprite_info.tex_id();
    let mut files = match sprite_info.files.try_lock() {
        Ok(o) => o,
        _ => return,
    };
    let tex_formats;
    let is_anim;
    let path;
    let grp_scale;
    let had_palette;
    {
        let file = match files.file(tex_id.0, tex_id.1) {
            Ok(Some(o)) => o,
            _ => return,
        };
        is_anim = file.is_anim();
        tex_formats = file.texture_formats();
        had_palette = file.palette().is_some();
        path = file.path().to_owned();
        grp_scale = file.grp().map(|x| x.scale).unwrap_or(1);
    }

    let window = gtk::Window::new(gtk::WindowType::Toplevel);

    let grp_select = Rc::new(
        select_dir::SelectFile::new(&window, "import_grp", "StarCraft GRP sprite", "*.grp")
    );
    let grp_section = label_section("Input GRP", grp_select.widget());

    static PALETTES: &[(Option<Palette>, &str)] = &[
        (Some(palette::BADLANDS), "Regular (badlands.wpe)"),
        (Some(palette::BEXPL), "bexpl.pcx"),
        (Some(palette::BFIRE), "bfire.pcx"),
        (Some(palette::GFIRE), "gfire.pcx"),
        (Some(palette::OFIRE), "ofire.pcx"),
        (None, "Custom"),
    ];
    let palette_combo_box = ComboBoxEnum::new(PALETTES);
    let custom_palette_select = Rc::new(
        select_dir::SelectFile::new(&window, "import_grp_palette", "Palette", "*.wpe")
    );
    palette_combo_box.set_active(&Some(palette::BADLANDS));
    custom_palette_select.widget().set_sensitive(false);
    let custom_palette_select2 = custom_palette_select.clone();
    palette_combo_box.connect_changed(move |new| {
        let is_custom = new == Some(None);
        custom_palette_select2.widget().set_sensitive(is_custom);
    });
    let palette_bx = box_vertical(&[
        palette_combo_box.widget(),
        custom_palette_select.widget(),
    ]);
    let palette_section = label_section("Input palette", &palette_bx);

    static FORMATS_ANIM: &[(Option<anim::TextureFormat>, &str)] = &[
        (Some(anim::TextureFormat::Dxt1), "DXT1"),
        (Some(anim::TextureFormat::Dxt5), "DXT5"),
    ];
    static FORMATS_DDSGRP: &[(Option<anim::TextureFormat>, &str)] = &[
        (Some(anim::TextureFormat::Dxt1), "DXT1"),
        (Some(anim::TextureFormat::Dxt5), "DXT5"),
        (None, "Paletted"),
    ];
    let format_combo_box = if is_anim {
        ComboBoxEnum::new(FORMATS_ANIM)
    } else {
        ComboBoxEnum::new(FORMATS_DDSGRP)
    };
    let encode_format_bx = {
        if had_palette {
            format_combo_box.set_active(&None);
        } else {
            if let Some(Ok(Some(tex_f))) = tex_formats.get(0) {
                format_combo_box.set_active(&Some(*tex_f));
            }
        }

        let bx = label_section("Encode format", format_combo_box.widget());
        bx
    };
    encode_format_bx.set_tooltip_text(Some(frame_import_dialog::encoding_tooltip_text()));

    let teamcolor_check = if is_anim {
        Some(SavedCheckbox::new("grp_import_teamcolor", "Create teamcolor layer"))
    } else {
        None
    };

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

    let progress = gtk::ProgressBar::new();
    let progress2 = progress.clone();
    let waiting_for_thread = Rc::new(Cell::new(false));
    let waiting_for_thread2 = waiting_for_thread.clone();
    let rest_of_ui: Rc<RefCell<Vec<gtk::Box>>> = Rc::new(RefCell::new(Vec::new()));
    let rest_of_ui2 = rest_of_ui.clone();
    let teamcolor_check2 = teamcolor_check.clone();
    let palette_combo_box2 = palette_combo_box.clone();
    let custom_palette_select2 = custom_palette_select.clone();
    let grp_select2 = grp_select.clone();
    ok_button.connect_clicked(move |_| {
        if waiting_for_thread.get() {
            return;
        }
        let (send, recv) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);
        let files_arc = sprite_info.files.clone();
        let format = match format_combo_box.active() {
            Some(o) => o,
            None => {
                error_msg_box(&w, "Format not specified");
                return;
            }
        };
        let grp_path = grp_select2.text();
        let grp = match std::fs::read(&grp_path) {
            Ok(o) => o,
            Err(e) => {
                error_msg_box(&w, &format!("Can't read palette from {}: {}", grp_path, e));
                return;
            }
        };
        let mut palette: Vec<u8> = match palette_combo_box2.active() {
            Some(Some(palette)) => palette.0.into(),
            Some(None) => {
                let path = custom_palette_select2.text();
                let palette = match std::fs::read(&path) {
                    Ok(o) => o,
                    Err(e) => {
                        error_msg_box(&w, &format!("Can't read palette from {}: {}", path, e));
                        return;
                    }
                };
                if palette.len() != 0x400 {
                    let msg = format!("Palette {} is not valid (.wpa format is required)", path);
                    error_msg_box(&w, &msg);
                    return;
                }
                palette
            }
            None => {
                error_msg_box(&w, "Palette not specified");
                return;
            }
        };
        // Fix palette alpha.
        // The alpha field wasn't used by BW but AlphaEdit generated .wpe from remap palettes
        // have valid values there, and those .wpe files are commonly used (Including being
        // embedded here).
        // Though they encode alpha as `255 - alpha` (0 = fully opaque) so fix that up.
        for pixel in palette.chunks_exact_mut(4) {
            pixel[3] = 255 - pixel[3];
        }

        let frame_count = u32::from(grp_decode::frame_count(&grp).unwrap_or(0));
        if is_anim {
            let format = match format {
                Some(o) => o,
                None => {
                    error_msg_box(&w, "Cannot import paletted anim");
                    return;
                }
            };
            let use_teamcolor = teamcolor_check2.as_ref()
                .map(|x| x.is_active())
                .unwrap_or(false);
            std::thread::spawn(move || {
                let send2 = send.clone();
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(move || {
                    let mut files = files_arc.lock();
                    frame_import::import_grp_to_anim(
                        &mut files,
                        tex_id.0,
                        &grp,
                        &palette,
                        format,
                        use_teamcolor,
                        |step| send2.send(Progress::Progress(step)).unwrap(),
                    )
                })).unwrap_or_else(|e| Err(error_from_panic(e)));
                let _ = send.send(Progress::Done(result.map(|()| frame_count)));
            });
        } else {
            std::thread::spawn(move || {
                let send2 = send.clone();
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(move || {
                    let mut files = files_arc.lock();
                    frame_import::import_grp_to_ddsgrp(
                        &mut files,
                        tex_id.0,
                        &grp,
                        &palette,
                        format,
                        grp_scale,
                        |step| send2.send(Progress::Progress(step)).unwrap(),
                    )
                })).unwrap_or_else(|e| Err(error_from_panic(e)));
                let _ = send.send(Progress::Done(result.map(|()| frame_count)));
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
    let grp_select2 = grp_select.clone();
    let custom_palette_select2 = custom_palette_select.clone();
    let palette_combo_box2 = palette_combo_box.clone();
    update_ok_sensitive(&ok, &grp_select2, &custom_palette_select2, &palette_combo_box2);
    grp_select.on_change(move |_| {
        update_ok_sensitive(&ok, &grp_select2, &custom_palette_select2, &palette_combo_box2);
    });

    button_bx.pack_end(&cancel_button, false, false, 0);
    button_bx.pack_end(&ok_button, false, false, 0);
    let rest_bx = gtk::Box::new(gtk::Orientation::Vertical, 10);
    rest_bx.pack_start(&grp_section, false, false, 0);
    rest_bx.pack_start(&palette_section, false, false, 0);
    rest_bx.pack_start(&encode_format_bx, false, false, 0);
    if let Some(teamcolor_check) = teamcolor_check {
        rest_bx.pack_start(teamcolor_check.widget(), false, false, 0);
    }
    let bx = box_vertical(&[
        &rest_bx,
        &progress,
        &button_bx,
    ]);
    *rest_of_ui.borrow_mut() = vec![rest_bx, button_bx];
    window.add(&bx);
    window.set_border_width(10);
    window.set_default_width(350);
    if is_anim {
        window.set_title(&format!("Import GRP to {:?} image {}", tex_id.1, tex_id.0));
    } else {
        if let Some(filename) = path.file_name() {
            window.set_title(&format!("Import GRP to {}", filename.to_string_lossy()));
        }
    }
    window.connect_delete_event(move |_, _| {
        Inhibit(waiting_for_thread2.get())
    });
    window.set_modal(true);
    window.set_transient_for(Some(parent));
    window.show_all();
}

fn update_ok_sensitive(
    ok: &gtk::Button,
    grp_select: &select_dir::SelectFile,
    custom_palette_select: &select_dir::SelectFile,
    palette_combo_box: &ComboBoxEnum<Option<Palette>>,
) {
    let has_grp = grp_select.text().is_empty() == false;
    let has_palette = palette_combo_box.active().is_some() ||
        custom_palette_select.text().is_empty() == false;
    ok.set_sensitive(has_grp && has_palette);
}
