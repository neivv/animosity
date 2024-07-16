use std::cell::{Cell, RefCell};
use std::convert::{TryFrom};
use std::fs;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;

use anyhow::Context;
use gio::prelude::*;
use gtk::prelude::*;
use serde_derive::{Deserialize, Serialize};

use crate::anim;
use crate::combo_box_enum::ComboBoxEnum;
use crate::files::Files;
use crate::frame_export_dialog::SavedCheckbox;
use crate::frame_import;
use crate::frame_info::{self, FrameInfo, parse_frame_info};
use crate::int_entry::{IntSize, IntEntry};
use crate::select_dir::{
    self, read_config_entry, set_config_entry, read_config_entry_int,
};
use crate::{
    label_section, lookup_action, error_msg_box, info_msg_box, SpriteInfo, SpriteType, Error,
    error_from_panic,
};
use crate::files::{DEFAULT_HD_LAYER_NAMES, DEFAULT_SD_LAYER_NAMES};

use crate::ui_helpers::*;
use crate::util::{OptionExt, SliceExt};

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
    let tex_formats;
    let is_anim = files.is_anim();
    let ddsgrp_path;
    let ddsgrp_linked_grp;
    let grp_scale;
    let had_palette;
    {
        if is_anim {
            {
                let file = match files.file(tex_id.0, SpriteType::Hd) {
                    Ok(Some(o)) => o,
                    _ => match files.file(tex_id.0, tex_id.1) {
                        Ok(Some(o)) => o,
                        _ => return,
                    }
                };
                tex_formats = file.texture_formats();
            }
            had_palette = false;
            ddsgrp_path = None;
            ddsgrp_linked_grp = None;
            grp_scale = None;
        } else {
            let file = match files.file(tex_id.0, tex_id.1) {
                Ok(Some(o)) => o,
                _ => return,
            };
            tex_formats = file.texture_formats();
            ddsgrp_path = Some(file.path().to_owned());
            had_palette = file.palette().is_some();
            ddsgrp_linked_grp = file.ddsgrp_linked_grp();
            grp_scale = file.grp().map(|x| x.scale);
        }
    }

    let window = gtk::Window::new(gtk::WindowType::Toplevel);

    let inputs = FrameInputs::new(window.clone());

    let checkboxes = OutLayerCheckboxes::new();
    let mut grp_format = None;
    static FORMATS_DDSGRP: &[(Option<anim::TextureFormat>, &str)] = &[
        (Some(anim::TextureFormat::Dxt1), "DXT1"),
        (Some(anim::TextureFormat::Dxt5), "DXT5"),
        (Some(anim::TextureFormat::Monochrome), "Monochrome"),
        (None, "Paletted"),
    ];
    let grp_layers_bx;
    let layers_bx = if is_anim {
        checkboxes.widget()
    } else {
        let format = ComboBoxEnum::new(FORMATS_DDSGRP);
        if had_palette {
            format.set_active(&None);
        } else {
            if let Some(Ok(Some(tex_f))) = tex_formats.get(0) {
                format.set_active(&Some(*tex_f));
            }
        }

        grp_layers_bx = label_section("Encode format", format.widget());
        grp_format = Some(format);
        grp_layers_bx.upcast_ref()
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

    let import_hd_checkbox = if is_anim {
        Some(SavedCheckbox::new_with_default(
            "import_hd",
            "Import HD/HD2",
            true,
        ))
    } else {
        None
    };
    let sd_anim_grp_widget = match is_anim {
        true => Some(Rc::new(SdAnimGrpWidget::new(&mut files, tex_id.0))),
        false => None,
    };
    let import_sd_checkbox = if let Some(ref grp_widget) = sd_anim_grp_widget {
        Some(label_section_with_enable_check(
            "Import SD",
            grp_widget.widget(),
            "import_sd",
            true,
        ))
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
    let checkboxes2 = checkboxes.clone();
    let ok_button2 = ok_button.clone();

    inputs.on_frame_info_updated(move |this| {
        let frame_info = this.frame_info(0);
        match frame_info {
            Some(frame_info) => {
                ok_button2.set_sensitive(true);
                checkboxes2.disable();
                for layer in &frame_info.layers {
                    checkboxes2.enable(layer, &tex_formats);
                }
            }
            None => {
                ok_button2.set_sensitive(false);
                checkboxes2.disable();
            }
        }
    });

    let checkboxes2 = checkboxes.clone();

    let progress = gtk::ProgressBar::new();
    let progress2 = progress.clone();
    let waiting_for_thread = Rc::new(Cell::new(false));
    let waiting_for_thread2 = waiting_for_thread.clone();
    let rest_of_ui: Rc<RefCell<Vec<gtk::Box>>> = Rc::new(RefCell::new(Vec::new()));
    let rest_of_ui2 = rest_of_ui.clone();
    let ddsgrp_make_linked_grp2 = ddsgrp_make_linked_grp.clone();
    let import_hd_checkbox2 = import_hd_checkbox.clone();
    let import_sd_checkbox2 = import_sd_checkbox.clone();
    let files_root: Option<PathBuf> = files.root_path().map(|x| x.into());
    let inputs2 = inputs.clone();
    ok_button.connect_clicked(move |_| {
        if waiting_for_thread.get() {
            return;
        }
        let inputs = &inputs2;
        // Used for grps too
        let dir = match inputs.frame_def_dir(0).should() {
            Some(s) => s,
            None => return,
        };
        let frame_info = match inputs.frame_info(0).should() {
            Some(s) => s,
            None => return,
        };
        let (hd_fi, sd_fi) = split_frame_info_hd_sd(&frame_info, &checkboxes2);

        let (send, recv) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);
        let files_arc = sprite_info.files.clone();
        let frame_scales = match inputs.scales().should() {
            Some(s) => s,
            None => return,
        };
        if is_anim {
            let import_sd = import_sd_checkbox2
                .as_ref()
                .map(|x| x.is_active())
                .unwrap_or(false);
            let import_hd = import_hd_checkbox2
                .as_ref()
                .map(|x| x.is_active())
                .unwrap_or(false);
            let formats = checkboxes2.get_formats();
            let (hd_formats, sd_formats) = match formats {
                Ok(o) => o,
                Err(()) => {
                    error_msg_box(&w, "Format not specified for every layer");
                    return;
                }
            };
            let sd_grp_widget = match sd_anim_grp_widget {
                Some(ref s) => s,
                None => return,
            };

            if !import_sd && !import_hd {
                info_msg_box(&w, "Nothing to do, select at least HD or SD import.");
                return;
            }

            let frame_count = frame_info.frame_count;

            let grp_filename = sd_grp_widget.grp_filename(&files_root);
            std::thread::spawn(move || {
                let send2 = send.clone();
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(move || {
                    let mut files = files_arc.lock();
                    let (hd_weight, sd_weight) = match (import_sd, import_hd) {
                        (true, true) => (2.0 / 3.0, 1.0 / 3.0),
                        (true, false) => (1.0, 0.0),
                        (false, true) => (0.0, 1.0),
                        (false, false) => (1.0, 0.0),
                    };
                    let hd_step = |step: f32| (step * hd_weight).clamp(0.0, 1.0);
                    let sd_step = |step: f32| (hd_weight + step * sd_weight).clamp(0.0, 1.0);
                    // HD / HD2
                    if import_hd {
                        frame_import::import_frames(
                            &mut files,
                            &hd_fi,
                            Some(&hd_fi),
                            &dir,
                            Some(&dir),
                            frame_scales.0,
                            Some(frame_scales.1),
                            &hd_formats,
                            tex_id.0,
                            SpriteType::Hd,
                            None,
                            |step| send.send(Progress::Progress(hd_step(step))).unwrap(),
                        ).context("Import HD frames")?;
                    }
                    // SD
                    if import_sd {
                        frame_import::import_frames(
                            &mut files,
                            &sd_fi,
                            None,
                            &dir,
                            None,
                            frame_scales.2,
                            None,
                            &sd_formats,
                            tex_id.0,
                            SpriteType::Sd,
                            grp_filename.as_ref().map(|x| &**x),
                            |step| send.send(Progress::Progress(sd_step(step))).unwrap(),
                        ).context("Import SD frames")?;
                    }
                    Ok(())
                })).unwrap_or_else(|e| Err(error_from_panic(e)));
                let _ = send2.send(Progress::Done(result.map(|()| frame_count)));
            });
        } else {
            // Ddsgrp
            let format = match grp_format {
                Some(ref s) => s.active(),
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
            let make_linked_grp = ddsgrp_make_linked_grp2
                .as_ref()
                .map(|x| x.is_active())
                .unwrap_or(false);
            let linked_grp_path = match make_linked_grp {
                true => ddsgrp_linked_grp.clone(),
                false => None,
            };
            let frame_info = FrameInfo::clone(&frame_info);
            std::thread::spawn(move || {
                let send2 = send.clone();
                let frame_count = frame_info.frame_count;
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(move || {
                    if let Some(ref path) = linked_grp_path {
                        if let Some(dir) = path.parent() {
                            fs::create_dir_all(dir)?;
                        }
                    }
                    let frame_scale = match scale {
                        4 => frame_scales.0,
                        2 => frame_scales.1,
                        1 => frame_scales.2,
                        _ => return Err(anyhow!("Unsupported scale value")),
                    };
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
                    )?;
                    Ok(())
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

    button_bx.pack_end(&cancel_button, false, false, 0);
    button_bx.pack_end(&ok_button, false, false, 0);
    let rest_bx = gtk::Box::new(gtk::Orientation::Vertical, 10);
    rest_bx.pack_start(inputs.widget(), false, false, 0);
    rest_bx.pack_start(layers_bx, false, false, 0);
    if let Some(sd_grp) = ddsgrp_make_linked_grp {
        rest_bx.pack_start(sd_grp.widget(), false, false, 0);
    }
    if let Some(scale) = grp_scale_bx {
        rest_bx.pack_start(&scale, false, false, 0);
    }
    if let Some(ref check) = import_hd_checkbox {
        rest_bx.pack_start(check.widget(), false, false, 0);
    }
    if let Some(ref check) = import_sd_checkbox {
        rest_bx.pack_start(check.widget(), false, false, 0);
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
        window.set_title(&format!("Import frames for image {}", tex_id.0));
    } else {
        if let Some(filename) = ddsgrp_path.as_ref().and_then(|x| x.file_name()) {
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

/// Choice for SD anim -> grp generation
/// - No
/// - Default path
/// - Custom path
#[derive(Clone)]
struct SdAnimGrpWidget {
    bx: gtk::Box,
    radios: Vec<gtk::RadioButton>,
    filename_entry: gtk::Entry,
}

impl SdAnimGrpWidget {
    pub fn new(files: &mut Files, sprite_id: usize) -> SdAnimGrpWidget {
        let no_grp = gtk::RadioButton::with_label("Don't create");
        let default_name = gtk::RadioButton::with_label_from_widget(&no_grp, "Default path");
        let default_grp_path = files.image_grp_path(sprite_id);
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
            if this.is_active() {
                filename_entry2.set_sensitive(false);
                filename_entry2.set_text("");
            }
        });
        let filename_entry2 = filename_entry.clone();
        default_name.connect_toggled(move |this| {
            if this.is_active() {
                filename_entry2.set_sensitive(false);
                if let Some(ref path) = default_grp_path {
                    filename_entry2.set_text(path);
                }
            }
        });
        let filename_entry2 = filename_entry.clone();
        custom_name.connect_toggled(move |this| {
            if this.is_active() {
                filename_entry2.set_sensitive(true);
                let name = read_config_entry("sd_grp_custom_name")
                    .unwrap_or_else(|| "something.grp".into());
                filename_entry2.set_text(&name);
            }
        });
        let bx = label_section("GRP creation", &bx);
        let radios = vec![no_grp, default_name, custom_name];
        let last_selected = read_config_entry_int("sd_grp_last_selected")
            .and_then(|x| u8::try_from(x).ok())
            .and_then(|x| radios.get(x as usize))
            .or_else(|| radios.get(0));
        if let Some(last_selected) = last_selected {
            last_selected.set_active(true);
        }
        SdAnimGrpWidget {
            bx,
            radios,
            filename_entry,
        }
    }

    pub fn widget(&self) -> &gtk::Widget {
        self.bx.upcast_ref()
    }

    pub fn grp_filename(&self, files_root: &Option<PathBuf>) -> Option<PathBuf> {
        let active_radio_index = self.radios.iter().position(|x| x.is_active())?;
        set_config_entry("sd_grp_last_selected", active_radio_index);
        if active_radio_index == 2 {
            let text = String::from(self.filename_entry.text());
            set_config_entry("sd_grp_custom_name", text.as_str());
        }
        if active_radio_index == 0 {
            None
        } else {
            let text = String::from(self.filename_entry.text());
            Some(files_root.as_ref()?.join(&*text))
        }
    }
}

/// Widget for at least one to at most `self.limit`
/// frame_info / input scale choices.
#[derive(Clone)]
struct FrameInputs(Rc<FrameInputsInner>);

struct FrameInputsInner {
    limit: Cell<u32>,
    count: Cell<u32>,
    bx: gtk::Box,
    input_controls: RefCell<Vec<FrameInputControls>>,
    frame_info_update_callbacks: RefCell<Vec<Box<dyn FnMut(&FrameInputs)>>>,
    input_state: FrameInputState,
    window: gtk::Window,
}

/// Controls for single input choice
struct FrameInputControls {
    file_select: Rc<select_dir::SelectFile>,
    input_scale: ScaleChooser,
    framedef_status: gtk::Label,
}

struct FrameInputState {
    list: RefCell<Vec<Option<Rc<FrameInfo>>>>,
}

impl FrameInputs {
    pub fn new(window: gtk::Window) -> FrameInputs {
        let count = read_config_entry_int("frame_inputs_count")
            .unwrap_or(1)
            .clamp(1, 3) as u32;
        let bx = gtk::Box::new(gtk::Orientation::Vertical, 10);
        let this = Rc::new(FrameInputsInner {
            limit: Cell::new(3),
            count: Cell::new(0),
            bx,
            input_controls: RefCell::new(Vec::new()),
            frame_info_update_callbacks: RefCell::new(Vec::new()),
            input_state: FrameInputState::new(),
            window,
        });
        let this = FrameInputs(this);
        while this.0.count.get() < count {
            this.add_new();
        }
        this
    }

    fn add_new(&self) {
        let this = &self.0;
        if this.count.get() >= this.limit.get() {
            return;
        }
        this.count.set(this.count.get() + 1);

        let i = this.count.get();
        let file_select_id = format!("import_frames_{i}");
        let file_select = Rc::new(
            select_dir::SelectFile::new(&this.window, file_select_id, "Text files", "*.json")
        );

        let input_scale = ScaleChooser::new(format!("import_scale_{i}"));
        input_scale.widget().set_tooltip_text(Some("\
            Selects resolution of input frames.\n\
            If different from output scale, the frames will be resized\n\
            when imported"));
        let label = if i == 1 {
            String::from("Frame info file")
        } else {
            format!("Frame info file {i}")
        };
        let framedef_status = gtk::Label::new(None);
        framedef_status.set_halign(gtk::Align::Start);

        let file_select_labeled = label_section(&label, file_select.widget());
        let inner_bx = box_vertical(&[
            &file_select_labeled,
            input_scale.widget(),
            &framedef_status,
        ]);
        this.bx.pack_start(&inner_bx, false, false, 0);

        this.input_state.add_new();
        this.input_controls.borrow_mut().push(FrameInputControls {
            file_select: file_select.clone(),
            input_scale,
            framedef_status,
        });

        let index = i as usize - 1;
        if let Some(filename) = Some(file_select.text()).filter(|x| !x.is_empty()) {
            self.new_framedef_filename(index, &filename);
        }
        let s = self.clone();
        file_select.on_change(move |filename| {
            s.new_framedef_filename(index, filename);
        });
    }

    fn new_framedef_filename(&self, index: usize, filename: &str) {
        let status = match self.0.input_controls.borrow().should_get(index) {
            Some(s) => s.framedef_status.clone(),
            None => return,
        };
        let frame_info = match parse_frame_info(Path::new(filename)) {
            Ok(o) => {
                status.set_text("");
                Some(o)
            }
            Err(e) => {
                let msg = format!("Frame info invalid: {:?}", e);
                status.set_text(&msg);
                None
            }
        };
        self.0.input_state.set(index, frame_info);
        self.frame_info_updated();
    }

    /// Called once any of the state in input FrameInfos is replaced
    fn frame_info_updated(&self) {
        let mut cbs = self.0.frame_info_update_callbacks.replace(Vec::new());
        for cb in &mut cbs {
            cb(self);
        }
        self.0.frame_info_update_callbacks.replace(cbs);
    }

    fn on_frame_info_updated(&self, mut cb: impl FnMut(&FrameInputs) + 'static) {
        cb(self);
        self.0.frame_info_update_callbacks.borrow_mut().push(Box::new(cb));
    }

    fn frame_def_dir(&self, index: usize) -> Option<PathBuf> {
        let text = self.0.input_controls.borrow().get(index)?.file_select.text();
        let mut buf = PathBuf::from(text);
        buf.pop();
        if !buf.is_dir() {
            None
        } else {
            Some(buf)
        }
    }

    fn frame_info(&self, index: usize) -> Option<Rc<FrameInfo>> {
        self.0.input_state.get(index)
    }

    pub fn widget(&self) -> &gtk::Widget {
        self.0.bx.upcast_ref()
    }

    pub fn scales(&self) -> Option<(f32, f32, f32)> {
        let scale = self.0.input_controls.borrow().should_get(0)?.input_scale.active()?;
        Some(match scale {
            ScaleValue::Scale4 => (1.0, 0.5, 0.25),
            ScaleValue::Scale2 => (2.0, 1.0, 0.5),
            ScaleValue::Scale1 => (4.0, 2.0, 1.0),
        })
    }
}

impl FrameInputState {
    fn new() -> FrameInputState {
        FrameInputState {
            list: RefCell::new(Vec::new()),
        }
    }

    fn set(&self, index: usize, value: Option<FrameInfo>) {
        if let Some(out) = self.list.borrow_mut().should_get_mut(index) {
            *out = value.map(Rc::new);
        }
    }

    fn get(&self, index: usize) -> Option<Rc<FrameInfo>> {
        self.list.borrow().get(index)?.clone()
    }

    fn add_new(&self) {
        self.list.borrow_mut().push(None);
    }
}

fn split_frame_info_hd_sd(
    frame_info: &FrameInfo,
    checkboxes: &OutLayerCheckboxes,
) -> (FrameInfo, FrameInfo) {
    // Create hd frameinfo using layers with name matching hd layers,
    // sd layers with name matching sd layers
    let mut layer_to_hd = Vec::new();
    let mut layer_to_sd = Vec::new();
    for layer in frame_info.layers.iter() {
        if !checkboxes.layer_enabled(&layer.name) {
            continue;
        }
        if let Some(hd_idx) = DEFAULT_HD_LAYER_NAMES.iter().position(|&x| x == layer.name) {
            layer_to_hd.push((layer.id, hd_idx as u32));
        }
        if let Some(sd_idx) = DEFAULT_SD_LAYER_NAMES.iter().position(|&x| x == layer.name) {
            layer_to_sd.push((layer.id, sd_idx as u32));
        }
    }

    let mut hd = frame_info.clone();
    let mut sd = frame_info.clone();
    hd.layers.clear();
    sd.layers.clear();
    hd.multi_frame_images.clear();
    sd.multi_frame_images.clear();
    for layer in frame_info.layers.iter() {
        if let Some(&(_, hd_id)) = layer_to_hd.iter().find(|x| x.0 == layer.id) {
            let mut layer = layer.clone();
            layer.id = hd_id;
            hd.layers.push(layer);
        }
        if let Some(&(_, sd_id)) = layer_to_sd.iter().find(|x| x.0 == layer.id) {
            let mut layer = layer.clone();
            layer.id = sd_id;
            sd.layers.push(layer);
        }
    }
    for mfi in &frame_info.multi_frame_images {
        if let Some(&(_, hd_id)) = layer_to_hd.iter().find(|x| x.0 == mfi.layer) {
            let mut mfi = mfi.clone();
            mfi.layer = hd_id;
            hd.multi_frame_images.push(mfi);
        }
        if let Some(&(_, sd_id)) = layer_to_sd.iter().find(|x| x.0 == mfi.layer) {
            let mut mfi = mfi.clone();
            mfi.layer = sd_id;
            sd.multi_frame_images.push(mfi);
        }
    }
    (hd, sd)
}

#[derive(Clone)]
struct OutLayerCheckboxes(Rc<OutLayerCheckboxesInner>);

struct OutLayerCheckboxesInner {
    bx: gtk::Box,
    checkboxes: Vec<(gtk::CheckButton, ComboBoxEnum<anim::TextureFormat>, &'static str)>,
}

impl OutLayerCheckboxes {
    pub fn new() -> OutLayerCheckboxes {
        let layer_names = DEFAULT_HD_LAYER_NAMES;
        static FORMATS_ANIM: &[(anim::TextureFormat, &str)] = &[
            (anim::TextureFormat::Dxt1, "DXT1"),
            (anim::TextureFormat::Dxt5, "DXT5"),
            (anim::TextureFormat::Monochrome, "Monochrome"),
        ];

        let grid = gtk::Grid::new();
        grid.set_column_spacing(5);
        grid.set_row_spacing(5);

        let mut checkboxes = Vec::new();

        for (i, &name) in layer_names.iter().enumerate() {
            let row = i as i32 + 1;

            let checkbox = gtk::CheckButton::new();
            grid.attach(&checkbox, 0, row, 1, 1);

            let label = gtk::Label::new(Some(name));
            grid.attach(&label, 1, row, 1, 1);
            label.set_halign(gtk::Align::Start);
            let format = ComboBoxEnum::new(FORMATS_ANIM);
            grid.attach(format.widget(), 2, row, 1, 1);

            checkboxes.push((checkbox, format, name));
        }
        let bx = label_section("Layers", &grid);

        let this = Rc::new(OutLayerCheckboxesInner {
            bx,
            checkboxes,
        });
        OutLayerCheckboxes(this)
    }

    pub fn widget(&self) -> &gtk::Widget {
        self.0.bx.upcast_ref()
    }

    pub fn layer_enabled(&self, name: &str) -> bool {
        self.0.checkboxes.iter()
            .find(|x| x.2 == name)
            .map(|x| x.0.is_active())
            .unwrap_or(false)
    }

    pub fn get_formats(&self) -> Result<(Vec<anim::TextureFormat>, Vec<anim::TextureFormat>), ()> {
        let mut hd: Vec<anim::TextureFormat> =
            DEFAULT_HD_LAYER_NAMES.iter().map(|_| anim::TextureFormat::Monochrome).collect();
        let mut sd: Vec<anim::TextureFormat> =
            DEFAULT_SD_LAYER_NAMES.iter().map(|_| anim::TextureFormat::Monochrome).collect();
        for &(ref check, ref format, name) in self.0.checkboxes.iter() {
            let format = if let Some(format) = format.active() {
                format
            } else {
                if check.is_active() {
                    return Err(());
                }
                // Just a dummy value since the layer is unused
                anim::TextureFormat::Monochrome
            };
            if let Some(idx) = DEFAULT_HD_LAYER_NAMES.iter().position(|&x| x == name) {
                hd[idx] = format;
            }
            if let Some(idx) = DEFAULT_SD_LAYER_NAMES.iter().position(|&x| x == name) {
                sd[idx] = format;
            }
        }
        Ok((hd, sd))
    }

    pub fn disable(&self) {
        for &(ref check, ref format, _name) in self.0.checkboxes.iter() {
            check.set_sensitive(false);
            check.set_active(false);
            format.set_sensitive(false);
            format.clear_active();
        }
    }

    pub fn enable(
        &self,
        layer: &frame_info::Layer,
        tex_formats: &[Result<Option<anim::TextureFormat>, Error>],
    ) {
        for (i, &(ref check, ref format, name)) in self.0.checkboxes.iter().enumerate() {
            if name == layer.name {
                check.set_sensitive(true);
                check.set_active(true);
                format.set_sensitive(true);
                let tex_f = tex_formats.get(i)
                    .and_then(|x| x.as_ref().ok())
                    .and_then(|x| x.as_ref());
                if let Some(tex_f) = tex_f {
                    format.set_active(tex_f);
                }
            }
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug, Deserialize, Serialize)]
enum ScaleValue {
    Scale1,
    Scale2,
    Scale4,
}

struct ScaleChooser {
    bx: gtk::Box,
    combo_box: ComboBoxEnum<ScaleValue>,
}

impl ScaleChooser {
    fn new<S: Into<String>>(config_cache: S) -> ScaleChooser {
        use self::ScaleValue::*;
        static SCALES: &[(ScaleValue, &str)] = &[
            (Scale4, "HD (4x)"),
            (Scale2, "HD2 (2x)"),
            (Scale1, "SD (1x)"),
        ];
        let combo_box = ComboBoxEnum::new(SCALES);
        let config_cache = config_cache.into();
        let cached_value = read_config_entry(&config_cache)
            .and_then(|x| serde_json::from_str(&x).ok());
        if let Some(value) = cached_value {
            combo_box.set_active(&value)
        } else {
            combo_box.set_active(&ScaleValue::Scale4);
        }
        combo_box.connect_changed(move |new| {
            if let Some(new) = new {
                if let Ok(new) = serde_json::to_string(&new) {
                    set_config_entry(&config_cache, &*new);
                }
            }
        });
        let bx = label_section("Input scale", combo_box.widget());
        ScaleChooser {
            bx,
            combo_box,
        }
    }

    pub fn widget(&self) -> &gtk::Widget {
        self.bx.upcast_ref()
    }

    fn active(&self) -> Option<ScaleValue> {
        self.combo_box.active()
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
