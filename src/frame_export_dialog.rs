use std::cell::{Cell, RefCell};
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;

use anyhow::Context;
use gtk;
use gtk::prelude::*;

use crate::frame_export::{self, LayerExportMode};
use crate::int_entry::{self, TextEntry};
use crate::select_dir;
use crate::ui_helpers::*;
use crate::{
    Error, error_from_panic, error_msg_box, info_msg_box, label_section, SpriteInfo, SpriteType,
};

struct LayerCheckboxState {
    check: gtk::CheckButton,
    label: gtk::Label,
    entry: TextEntry,
    layer: u32,
    sublayer: u32,
    export_mode: LayerExportMode,
}

pub fn frame_export_dialog(this: &Arc<SpriteInfo>, parent: &gtk::ApplicationWindow) {
    enum Progress {
        Done(Result<(), Error>),
        Progress(f32),
    }

    let tex_id = this.tex_id();
    let mut files = match this.files.try_lock() {
        Ok(o) => o,
        _ => return,
    };
    let file = match files.file(tex_id.0, tex_id.1) {
        Ok(Some(o)) => o,
        _ => return,
    };
    let layer_names = file.layer_names();

    let window = gtk::Window::new(gtk::WindowType::Toplevel);

    let dir_select = select_dir::SelectDir::new(&window, "export_frames");
    let filename_bx = label_section("Output directory", &dir_select.widget());

    let type_lowercase = match tex_id.1 {
        SpriteType::Sd => "sd",
        SpriteType::Hd => "hd",
        SpriteType::Hd2 => "hd2",
    };

    let is_anim = file.is_anim();
    let single_image_check = if is_anim {
        SavedCheckbox::new("frame_export_single_image", "One image per layer")
    } else {
        SavedCheckbox::new("frame_export_single_image", "Single image")
    };

    // Sprite dimensions are only used for anim;
    // if it errors display the error as a warning.
    let dimensions_result: Option<Result<(u16, u16), _>> = if is_anim {
        Some(
            file.dimensions()
                .context("WARNING: Cannot get sprite dimensions. \n\
                    Exported frames may be incorrectly aligned.")
        )
    } else {
        None
    };
    let dimensions = dimensions_result.as_ref()
        .and_then(|result| result.as_ref().ok())
        .map(|&x| x)
        .unwrap_or_else(|| (0, 0));
    let checkboxes = Rc::new(RefCell::new(Vec::with_capacity(layer_names.len())));
    let mut grp_prefix = None;
    let mut grp_prefix_text = String::new();
    let layers_bx = if is_anim {
        let grid = gtk::Grid::new();
        grid.set_column_spacing(5);
        grid.set_row_spacing(5);
        let prefix_label = gtk::Label::new(Some("Filename prefix"));
        let prefix_prefix = format!("{:03}_{}", tex_id.0, type_lowercase);
        prefix_label.set_halign(gtk::Align::Start);
        grid.attach(&prefix_label, 2, 0, 1, 1);
        let mut row = 0;
        for (i, name) in layer_names.iter().enumerate() {
            row += 1;
            let tex_size = file.texture_size(i);

            fn make_checkbox(
                prefix_prefix: &str,
                name: &str,
                usable: bool,
            ) -> (gtk::CheckButton, gtk::Label, TextEntry) {
                let checkbox = gtk::CheckButton::new();
                let label = gtk::Label::new(Some(name));
                label.set_halign(gtk::Align::Start);

                let entry = TextEntry::new();
                entry.widget().set_hexpand(true);

                if usable {
                    checkbox.set_active(true);
                    entry.set_text(&format!("{}_{}", prefix_prefix, name));
                } else {
                    checkbox.set_sensitive(false);
                    label.set_sensitive(false);
                    entry.widget().set_sensitive(false);
                    checkbox.set_active(false);
                }

                let e = entry.clone();
                checkbox.connect_toggled(move |s| {
                    e.widget().set_sensitive(s.get_active());
                });

                (checkbox, label, entry)
            }
            let (checkbox, label, entry) =
                make_checkbox(&prefix_prefix, &name, tex_size.is_some());

            grid.attach(&checkbox, 0, row, 1, 1);
            grid.attach(&label, 1, row, 1, 1);
            grid.attach(entry.widget(), 2, row, 1, 1);
            let layer_id = i as u32;
            checkboxes.borrow_mut().push(LayerCheckboxState {
                check: checkbox,
                entry,
                label,
                layer: layer_id,
                sublayer: 0,
                export_mode: LayerExportMode::Rgba,
            });
            if tex_size.is_some() {
                if name == "normal" {
                    row += 1;
                    let check = SavedCheckbox::new_with_default(
                        "export_decode_normal",
                        "Decode normals",
                        true,
                    );
                    check.widget().set_tooltip_text(Some("\
                        Decodes the normal layer to X/Y/Z components.\n\
                        See readme.txt for more information."));
                    grid.attach(check.widget(), 1, row, 2, 1);
                    let check2 = check.clone();
                    let checkboxes2 = checkboxes.clone();
                    check.connect_toggled(move || {
                        checkboxes_update_normal(&checkboxes2, check2.get_active(), layer_id);
                    });
                    checkboxes_update_normal(&checkboxes, check.get_active(), layer_id);
                } else if name == "ao_depth" {
                    let (checkbox, label, entry) =
                        make_checkbox(&prefix_prefix, "ao", true);
                    row += 1;
                    grid.attach(&checkbox, 0, row, 1, 1);
                    grid.attach(&label, 1, row, 1, 1);
                    grid.attach(entry.widget(), 2, row, 1, 1);
                    checkboxes.borrow_mut().push(LayerCheckboxState {
                        check: checkbox,
                        entry,
                        label,
                        layer: layer_id,
                        sublayer: 0,
                        export_mode: LayerExportMode::Green,
                    });

                    let (checkbox, label, entry) =
                        make_checkbox(&prefix_prefix, "depth", true);
                    row += 1;
                    grid.attach(&checkbox, 0, row, 1, 1);
                    grid.attach(&label, 1, row, 1, 1);
                    grid.attach(entry.widget(), 2, row, 1, 1);
                    checkboxes.borrow_mut().push(LayerCheckboxState {
                        check: checkbox,
                        entry,
                        label,
                        layer: layer_id,
                        sublayer: 1,
                        export_mode: LayerExportMode::Alpha,
                    });

                    row += 1;
                    let check = SavedCheckbox::new_with_default(
                        "export_decode_ao_depth",
                        "Split to AO + depth",
                        true,
                    );
                    check.widget().set_tooltip_text(Some("\
                        Splits ambient occlusion and depth data to separate files.\n\
                        See readme.txt for more information."));
                    grid.attach(check.widget(), 1, row, 2, 1);

                    let check2 = check.clone();
                    let checkboxes2 = checkboxes.clone();
                    check.connect_toggled(move || {
                        checkboxes_update_ao_depth(&checkboxes2, check2.get_active(), layer_id);
                    });
                    let check2 = check.clone();
                    let checkboxes2 = checkboxes.clone();
                    grid.connect_map(move |_| {
                        checkboxes_update_ao_depth(&checkboxes2, check2.get_active(), layer_id);
                    });
                }
            }
        }
        label_section("Layers to export", &grid)
    } else {
        let (entry, frame) = int_entry::entry();
        frame.set_hexpand(true);
        let text;
        let prefix = match file.path().file_name() {
            Some(x) => {
                text = x.to_string_lossy();
                match text.find(".") {
                    Some(x) => &text[..x],
                    None => &text,
                }
            }
            None => "Unk".into(),
        };
        entry.set_text(&prefix);
        grp_prefix_text = prefix.into();
        grp_prefix = Some(entry.clone());
        label_section("Filename prefix", &frame)
    };

    let (framedef_entry, framedef_frame) = int_entry::entry();
    let framedef_name = if is_anim {
        format!("frames_{:03}_{}.json", tex_id.0, type_lowercase)
    } else {
        format!("frames_{}.json", grp_prefix_text)
    };
    framedef_entry.set_text(&framedef_name);
    let framedef_bx = label_section("Write miscellaneous frame info to..", &framedef_frame);

    let button_bx = gtk::Box::new(gtk::Orientation::Horizontal, 15);
    let ok_button = gtk::Button::with_label("Export");
    let cancel_button = gtk::Button::with_label("Cancel");
    let w = window.clone();
    cancel_button.connect_clicked(move |_| {
        w.close();
    });
    let s = this.clone();
    let w = window.clone();
    let single_image_check2 = single_image_check.clone();
    let progress = gtk::ProgressBar::new();
    let progress2 = progress.clone();
    let waiting_for_thread = Rc::new(Cell::new(false));
    let waiting_for_thread2 = waiting_for_thread.clone();
    let rest_of_ui: Rc<RefCell<Vec<gtk::Box>>> = Rc::new(RefCell::new(Vec::new()));
    let rest_of_ui2 = rest_of_ui.clone();
    ok_button.connect_clicked(move |_| {
        if waiting_for_thread.get() {
            return;
        }
        let path: PathBuf = dir_select.text().into();

        let tex_id = s.tex_id();
        let mut files = match s.files.try_lock() {
            Ok(o) => o,
            _ => return,
        };
        let file = match files.file(tex_id.0, tex_id.1) {
            Ok(Some(o)) => o,
            _ => return,
        };

        let framedef: PathBuf = String::from(framedef_entry.get_text()).into();
        let (send, recv) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);
        let files_arc = s.files.clone();
        let frame_count;
        let path2 = path.clone();
        if is_anim {
            let layers_to_export = checkboxes
                .borrow()
                .iter()
                .filter_map(|layer| {
                    if !layer.check.get_active() || !layer.check.is_visible() {
                        return None;
                    }
                    let prefix = layer.entry.get_text();
                    Some(frame_export::ExportLayer {
                        prefix,
                        id: layer.layer,
                        sub_id: layer.sublayer,
                        mode: layer.export_mode,
                    })
                })
                .collect::<Vec<_>>();
            frame_count = layers_to_export.len() *
                file.frames().map(|x| x.len()).unwrap_or(0);
            let single_image = single_image_check2.get_active();
            std::thread::spawn(move || {
                let send2 = send.clone();
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(move || {
                    let mut files = files_arc.lock();
                    let file = files.file(tex_id.0, tex_id.1)?
                        .ok_or_else(|| anyhow!("No file?"))?;

                    let (width, height) = dimensions;
                    frame_export::export_frames(
                        &file,
                        tex_id.1,
                        i32::from(width),
                        i32::from(height),
                        &path2,
                        &framedef,
                        &layers_to_export,
                        single_image,
                        |step| send.send(Progress::Progress(step)).unwrap(),
                    )
                })).unwrap_or_else(|e| Err(error_from_panic(e)));
                let _ = send2.send(Progress::Done(result));
            });
        } else {
            let prefix = grp_prefix.as_ref()
                .map(|x| x.get_text().into())
                .unwrap_or_else(String::new);
            frame_count = file.layer_count();
            let single_image = single_image_check2.get_active();
            std::thread::spawn(move || {
                let send2 = send.clone();
                let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(move || {
                    let mut files = files_arc.lock();
                    let file = files.file(tex_id.0, tex_id.1)?
                        .ok_or_else(|| anyhow!("No file?"))?;

                    frame_export::export_grp(
                        &file,
                        &path2,
                        &prefix,
                        &framedef,
                        single_image,
                        |step| send.send(Progress::Progress(step)).unwrap(),
                    )
                })).unwrap_or_else(|e| Err(error_from_panic(e)));
                let _ = send2.send(Progress::Done(result));
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
        recv.attach(None, move |status| match status {
            Progress::Done(result) => {
                waiting_for_thread.set(false);
                for part in rest_of_ui.borrow().iter() {
                    part.set_sensitive(true);
                }
                match result {
                    Ok(()) => {
                        let msg = format!(
                            "Wrote {} frames to {}",
                            frame_count, path.to_string_lossy(),
                        );
                        info_msg_box(&window, &msg);
                        window.close();
                    }
                    Err(e) => {
                        let msg = format!("Unable to export frames: {:?}", e);
                        error_msg_box(&window, &msg);
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
    let opt_error_label;
    let mut input_parts: Vec<&dyn BoxableWidget>  = vec![
        &filename_bx,
        &framedef_bx,
        single_image_check.widget(),
        &layers_bx,
    ];
    if let Some(Err(ref error)) = dimensions_result {
        opt_error_label = gtk::Label::new(Some(&format!("{:?}", error)));
        input_parts.push(&opt_error_label);
    }
    let rest_bx = box_vertical(&input_parts);
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
        window.set_title(&format!("Export frames of {:?} image {}", tex_id.1, tex_id.0));
    } else {
        if let Some(filename) = file.path().file_name() {
            window.set_title(&format!("Export frames of {}", filename.to_string_lossy()));
        }
    }
    window.connect_delete_event(move |_, _| {
        Inhibit(waiting_for_thread2.get())
    });
    window.set_modal(true);
    window.set_transient_for(Some(parent));
    window.show_all();
}

#[derive(Clone)]
struct SavedCheckbox {
    check: gtk::CheckButton,
}

impl SavedCheckbox {
    pub fn new<S: Into<String>>(save_entry: S, label: &str) -> SavedCheckbox {
        SavedCheckbox::new_with_default(save_entry, label, false)
    }

    pub fn new_with_default<S: Into<String>>(
        save_entry: S,
        label: &str,
        default: bool,
    ) -> SavedCheckbox {
        let check = gtk::CheckButton::with_label(label);
        let save_entry = save_entry.into();
        if select_dir::read_config_entry(&save_entry).map(|x| x == "y").unwrap_or(default) {
            check.set_active(true);
        } else {
            check.set_active(false);
        }
        check.connect_toggled(move |check| {
            let state = match check.get_active() {
                true => "y",
                false => "n",
            };
            select_dir::set_config_entry(&save_entry, state);
        });
        SavedCheckbox {
            check,
        }
    }

    pub fn widget(&self) -> &gtk::Widget {
        self.check.upcast_ref()
    }

    pub fn get_active(&self) -> bool {
        self.check.get_active()
    }

    pub fn connect_toggled<F: Fn() + 'static>(&self, func: F) {
        self.check.connect_toggled(move |_| func());
    }
}

fn checkboxes_update_normal(
    checkboxes: &RefCell<Vec<LayerCheckboxState>>,
    activate: bool,
    layer_id: u32,
) {
    let mut checkboxes = match checkboxes.try_borrow_mut() {
        Ok(o) => o,
        _ => return,
    };
    for check in &mut *checkboxes {
        if check.layer == layer_id {
            if activate {
                check.export_mode = LayerExportMode::Normal;
            } else {
                check.export_mode = LayerExportMode::Rgba;
            }
        }
    }
}

fn checkboxes_update_ao_depth(
    checkboxes: &RefCell<Vec<LayerCheckboxState>>,
    activate_split: bool,
    layer_id: u32,
) {
    // Note: Hidden checkboxes are treated as unchecked with ok button
    fn set_visible(check: &LayerCheckboxState, visible: bool) {
        check.check.set_visible(visible);
        check.label.set_visible(visible);
        check.entry.widget().set_visible(visible);
    }

    let checkboxes = match checkboxes.try_borrow() {
        Ok(o) => o,
        _ => return,
    };
    for check in &*checkboxes {
        if check.layer == layer_id {
            if activate_split {
                set_visible(&check, check.export_mode != LayerExportMode::Rgba);
            } else {
                set_visible(&check, check.export_mode == LayerExportMode::Rgba);
            }
        }
    }
}
