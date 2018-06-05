use std::cell::RefCell;
use std::fs;
use std::io::{self, Seek};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use app_dirs::{self, AppDataType, AppInfo};
use serde_json;

use gtk;

use gtk::prelude::*;

use int_entry;

const APP_INFO: AppInfo = AppInfo {
    name: "Animosity",
    author: "Animosity",
};

fn config_filename() -> Option<PathBuf> {
    let dir = app_dirs::app_root(AppDataType::UserData, &APP_INFO).ok()?;
    Some(dir.join("select_dir.json"))
}

pub fn read_config_entry(id: &str) -> Option<String> {
    let mut file = fs::File::open(config_filename()?).ok()?;
    let json: serde_json::Value = serde_json::from_reader(&mut file).ok()?;
    Some(json.as_object()?.get(id)?.as_str()?.into())
}

// Nice return value
pub fn set_config_entry(id: &str, value: &str) -> Option<()> {
    fn update_json(file: &mut fs::File, id: &str, value: &str) -> Option<serde_json::Value> {
        let mut json: serde_json::Value = serde_json::from_reader(file).ok()?;
        {
            let obj = json.as_object_mut()?;
            obj.insert(id.into(), value.into());
        }
        Some(json)
    }

    let mut file = fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(config_filename()?).ok()?;

    let json = match update_json(&mut file, id, value) {
        Some(o) => o,
        None => {
            let mut map = serde_json::map::Map::new();
            map.insert(id.into(), value.into());
            map.into()
        }
    };
    file.seek(io::SeekFrom::Start(0)).ok()?;
    serde_json::to_writer_pretty(&mut file, &json).ok()?;
    let len = file.seek(io::SeekFrom::Current(0)).ok()?;
    file.set_len(len).ok()?;
    Some(())
}

pub struct SelectDir {
    pub entry: gtk::Entry,
    pub bx: gtk::Box,
}

pub struct SelectFile {
    pub entry: gtk::Entry,
    pub bx: gtk::Box,
    on_change_handlers: Rc<RefCell<Vec<Box<FnMut(&str) + 'static>>>>,
}

fn create_common() -> (gtk::Box, gtk::Entry, gtk::Button) {
    let bx = gtk::Box::new(gtk::Orientation::Horizontal, 15);
    let button = gtk::Button::new_with_label("Select...");
    let (entry, frame) = int_entry::entry();
    //entry.set_sensitive(false);
    let _ = entry.set_property("editable", &false);
    frame.set_vexpand(false);
    frame.set_valign(gtk::Align::End);
    bx.pack_start(&frame, true, true, 0);
    bx.pack_start(&button, false, false, 0);
    (bx, entry, button)
}

impl SelectDir {
    pub fn new(window: &gtk::Window, select_id: &'static str) -> SelectDir {
        let filename = read_config_entry(select_id);

        let (bx, entry, button) = create_common();
        if let Some(name) = filename {
            entry.set_text(&name);
            entry.emit_move_cursor(gtk::MovementStep::BufferEnds, 1, false);
        }

        let e = entry.clone();
        let w = window.clone();
        button.connect_clicked(move |_| {
            let dir = e.get_text();
            if let Some(path) = choose_dir_dialog(&w, &dir) {
                let val = path.to_string_lossy();
                e.set_text(&val);
                e.emit_move_cursor(gtk::MovementStep::BufferEnds, 1, false);
                set_config_entry(select_id, &val);
            }
        });

        SelectDir {
            entry,
            bx,
        }
    }

    pub fn widget(&self) -> gtk::Widget {
        self.bx.clone().upcast()
    }

    pub fn text(&self) -> Option<String> {
        self.entry.get_text()
    }
}

impl SelectFile {
    pub fn new(
        window: &gtk::Window,
        select_id: &'static str,
        filter_name: &'static str,
        filter_pattern: &'static str,
    ) -> SelectFile {
        let filename = read_config_entry(select_id);

        let (bx, entry, button) = create_common();
        if let Some(name) = filename {
            entry.set_text(&name);
            entry.emit_move_cursor(gtk::MovementStep::BufferEnds, 1, false);
        }

        let on_change_handlers: Rc<RefCell<Vec<Box<FnMut(&str) + 'static>>>> =
            Rc::new(RefCell::new(Vec::new()));
        let e = entry.clone();
        let w = window.clone();
        let o = on_change_handlers.clone();
        button.connect_clicked(move |_| {
            let dir = e.get_text().and_then(|x| {
                Path::new(&x).parent().map(|x| x.to_string_lossy().into_owned())
            });
            if let Some(path) = choose_file_dialog(&w, &dir, filter_name, filter_pattern) {
                let val = path.to_string_lossy();
                e.set_text(&val);
                e.emit_move_cursor(gtk::MovementStep::BufferEnds, 1, false);
                set_config_entry(select_id, &val);
                let mut handlers = o.borrow_mut();
                for h in handlers.iter_mut() {
                    h(&val);
                }
            }
        });

        SelectFile {
            entry,
            bx,
            on_change_handlers,
        }
    }

    pub fn widget(&self) -> gtk::Widget {
        self.bx.clone().upcast()
    }

    pub fn text(&self) -> Option<String> {
        self.entry.get_text()
    }

    pub fn on_change<F: FnMut(&str) + 'static>(&self, fun: F) {
        self.on_change_handlers.borrow_mut().push(Box::new(fun));
    }
}

fn choose_file_dialog(
    parent: &gtk::Window,
    dir: &Option<String>,
    name: &str,
    pattern: &str,
) -> Option<PathBuf> {
    let dialog = gtk::FileChooserNative::new(
        Some("Open..."),
        Some(parent),
        gtk::FileChooserAction::Open,
        Some("Open"),
        Some("Cancel")
    );
    if let Some(ref dir) = *dir {
        dialog.set_current_folder(&dir);
    }
    dialog.set_select_multiple(false);
    let filter = gtk::FileFilter::new();
    filter.add_pattern(pattern);
    gtk::FileFilterExt::set_name(&filter, name);
    dialog.add_filter(&filter);
    let filter = gtk::FileFilter::new();
    filter.add_pattern("*.*");
    gtk::FileFilterExt::set_name(&filter, "All files");
    dialog.add_filter(&filter);
    let result: gtk::ResponseType = dialog.run().into();
    let result = if result == gtk::ResponseType::Accept {
        dialog.get_filename()
    } else {
        None
    };
    dialog.destroy();
    result
}

fn choose_dir_dialog(parent: &gtk::Window, dir: &Option<String>) -> Option<PathBuf> {
    let dialog = gtk::FileChooserNative::new(
        Some("Select folder..."),
        Some(parent),
        gtk::FileChooserAction::SelectFolder,
        Some("Select"),
        Some("Cancel")
    );
    if let Some(ref dir) = *dir {
        dialog.set_current_folder(&dir);
    }
    dialog.set_select_multiple(false);
    let result: gtk::ResponseType = dialog.run().into();
    let result = if result == gtk::ResponseType::Accept {
        dialog.get_filename()
    } else {
        None
    };
    dialog.destroy();
    result
}
