use std::borrow::Cow;
use std::cell::RefCell;
use std::fs;
use std::io::{self, Seek};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use app_dirs::{self, AppDataType, AppInfo};
use serde_json;

use gtk::prelude::*;
use gtk::{glib, gio};

use crate::int_entry;

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

pub fn read_config_entry_int(id: &str) -> Option<i64> {
    let mut file = fs::File::open(config_filename()?).ok()?;
    let json: serde_json::Value = serde_json::from_reader(&mut file).ok()?;
    Some(json.as_object()?.get(id)?.as_i64()?)
}

// Nice return value
pub fn set_config_entry<V: Into<serde_json::Value>> (id: &str, value: V) -> Option<()> {
    fn update_json(
        file: &mut fs::File,
        id: &str,
        value: serde_json::Value,
    ) -> Option<serde_json::Value> {
        let mut json: serde_json::Value = serde_json::from_reader(file).ok()?;
        {
            let obj = json.as_object_mut()?;
            obj.insert(id.into(), value);
        }
        Some(json)
    }

    let mut file = fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(config_filename()?).ok()?;

    let value: serde_json::Value = value.into();
    let json = match update_json(&mut file, id, value.clone()) {
        Some(o) => o,
        None => {
            let mut map = serde_json::map::Map::new();
            map.insert(id.into(), value);
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
    on_change_handlers: Rc<RefCell<Vec<Box<dyn FnMut(&str) + 'static>>>>,
}

fn create_common() -> (gtk::Box, gtk::Entry, gtk::Button) {
    let bx = gtk::Box::new(gtk::Orientation::Horizontal, 15);
    let button = gtk::Button::with_label("Select...");
    let (entry, frame) = int_entry::entry();
    //entry.set_sensitive(false);
    let _ = entry.set_property("editable", &false);
    frame.set_vexpand(false);
    frame.set_hexpand(true);
    frame.set_valign(gtk::Align::End);
    bx.append(&frame);
    bx.append(&button);
    (bx, entry, button)
}

impl SelectDir {
    pub fn new<Id: Into<Cow<'static, str>>>(window: &gtk::Window, select_id: Id) -> SelectDir {
        let id = select_id.into();
        Self::new_(window, id)
    }

    fn new_(window: &gtk::Window, select_id: Cow<'static, str>) -> SelectDir {
        let filename = read_config_entry(&select_id);

        let (bx, entry, button) = create_common();
        if let Some(name) = filename {
            entry.set_text(&name);
            move_cursor_to_end(&entry);
        }

        let e = entry.clone();
        let w = window.clone();
        let select_id = Rc::new(select_id);
        button.connect_clicked(move |_| {
            let dir = e.text();
            let e = e.clone();
            let w = w.clone();
            let select_id = select_id.clone();
            let task = async move {
                if let Some(path) = choose_dir_dialog(&w, &dir).await {
                    let val = path.to_string_lossy();
                    e.set_text(&val);
                    move_cursor_to_end(&e);
                    set_config_entry(&select_id, &*val);
                }
            };
            glib::MainContext::default().spawn_local(task);
        });

        SelectDir {
            entry,
            bx,
        }
    }

    pub fn widget(&self) -> gtk::Widget {
        self.bx.clone().upcast()
    }

    pub fn text(&self) -> String {
        self.entry.text().into()
    }
}

impl SelectFile {
    pub fn new<Id: Into<Cow<'static, str>>>(
        window: &gtk::Window,
        select_id: Id,
        filter_name: &'static str,
        filter_pattern: &'static str,
    ) -> SelectFile {
        let id = select_id.into();
        Self::new_(window, id, filter_name, filter_pattern)
    }

    fn new_(
        window: &gtk::Window,
        select_id: Cow<'static, str>,
        filter_name: &'static str,
        filter_pattern: &'static str,
    ) -> SelectFile {
        let filename = read_config_entry(&select_id);

        let (bx, entry, button) = create_common();
        if let Some(name) = filename {
            entry.set_text(&name);
            move_cursor_to_end(&entry);
        }

        let on_change_handlers: Rc<RefCell<Vec<Box<dyn FnMut(&str) + 'static>>>> =
            Rc::new(RefCell::new(Vec::new()));
        let e = entry.clone();
        let w = window.clone();
        let o = on_change_handlers.clone();
        let select_id = Rc::new(select_id);
        button.connect_clicked(move |_| {
            let dir = e.text();
            let dir = Path::new(&*dir).parent().map(|x| x.to_string_lossy().into_owned());
            let e = e.clone();
            let o = o.clone();
            let w = w.clone();
            let select_id = select_id.clone();
            let task = async move {
                if let Some(path) =
                    choose_file_dialog(&w, &dir, filter_name, filter_pattern).await
                {
                    let val = path.to_string_lossy();
                    e.set_text(&val);
                    move_cursor_to_end(&e);
                    set_config_entry(&select_id, &*val);
                    let mut handlers = o.borrow_mut();
                    for h in handlers.iter_mut() {
                        h(&val);
                    }
                }
            };
            glib::MainContext::default().spawn_local(task);
        });

        SelectFile {
            entry,
            bx,
            on_change_handlers,
        }
    }

    pub fn widget(&self) -> &gtk::Widget {
        self.bx.upcast_ref()
    }

    pub fn text(&self) -> String {
        self.entry.text().into()
    }

    pub fn on_change<F: FnMut(&str) + 'static>(&self, fun: F) {
        self.on_change_handlers.borrow_mut().push(Box::new(fun));
    }
}

fn move_cursor_to_end(entry: &gtk::Entry) {
    entry.set_position(-1);
}

async fn choose_file_dialog(
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
        let file = gio::File::for_path(&dir);
        let _ = dialog.set_current_folder(Some(&file));
    }
    dialog.set_select_multiple(false);
    let filter = gtk::FileFilter::new();
    filter.add_pattern(pattern);
    filter.set_name(Some(name));
    dialog.add_filter(&filter);
    let filter = gtk::FileFilter::new();
    filter.add_pattern("*.*");
    filter.set_name(Some("All files"));
    dialog.add_filter(&filter);

    let (send, recv) = async_channel::unbounded();
    dialog.connect_response(move |_, response| {
        let _ = send.send_blocking(response);
    });
    dialog.show();
    let res = recv.recv().await.ok()?;

    let result = if res == gtk::ResponseType::Accept {
        let file = dialog.file();
        file.and_then(|x| x.path())
    } else {
        None
    };
    result
}

async fn choose_dir_dialog(parent: &gtk::Window, dir: &str) -> Option<PathBuf> {
    let dialog = gtk::FileChooserNative::new(
        Some("Select folder..."),
        Some(parent),
        gtk::FileChooserAction::SelectFolder,
        Some("Select"),
        Some("Cancel")
    );
    let file = gio::File::for_path(&dir);
    let _ = dialog.set_current_folder(Some(&file));
    dialog.set_select_multiple(false);

    let (send, recv) = async_channel::unbounded();
    dialog.connect_response(move |_, response| {
        let _ = send.send_blocking(response);
    });
    dialog.show();
    let res = recv.recv().await.ok()?;

    let result = if res == gtk::ResponseType::Accept {
        let file = dialog.file();
        file.and_then(|x| x.path())
    } else {
        None
    };
    result
}
