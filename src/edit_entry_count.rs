use std::sync::Arc;

use gtk::gio::prelude::*;
use gtk::prelude::*;

use crate::int_entry::{IntSize, IntEntry};
use crate::ui_helpers::*;
use crate::{SpriteInfo, error_msg_box, info_msg_box};

pub fn dialog(sprite_info: &Arc<SpriteInfo>, parent: &gtk::ApplicationWindow) {

    // Please see the section about "Adding more sprites..." in readme.txt first to make
    // sure all necessary steps are taken.
    // ---
    // Current entry amounts:
    // images.dat: xxx
    // mainsd.anim: xxx
    // main.lit: xxx
    // [NOTE: It is recommended to extract main.lit before extending anim entries]
    // ---
    // New entry amount: ____
    // ---
    // Ok | Cancel

    let mut files = match sprite_info.files.try_lock() {
        Ok(o) => o,
        _ => return,
    };
    let mainsd_entries;
    let images_dat_entries;
    let lit_entries;
    {
        lit_entries = files.lit().map(|x| x.sprite_count());
        mainsd_entries = match files.mainsd_entries() {
            Some(s) => s as u32,
            None => return,
        };
        images_dat_entries = files.images_dat_entries();
    }

    let window = gtk::Window::new();

    let readme_msg = gtk::Label::new(Some("\
    Please see the section about \"Adding more sprites [...]\" in readme.txt first to make \
    sure the other necessary steps are taken.\
    "));
    readme_msg.set_wrap(true);
    readme_msg.set_halign(gtk::Align::Start);

    let lit_entries_num = lit_entries.unwrap_or(999);
    let mut entry_msg = format!("\
        Current entry amounts:\n\
        images.dat: {}\n\
        mainsd.anim: {}\n\
        main.lit: {}",
        images_dat_entries, mainsd_entries, lit_entries_num);
    if lit_entries.is_none() {
        entry_msg.push_str(&format!("\n\
        NOTE: It is recommended to extract main.lit before extending anim entries."));
    }

    let entry_msg = gtk::Label::new(Some(&entry_msg));
    entry_msg.set_wrap(true);
    entry_msg.set_halign(gtk::Align::Start);

    let entry_count_label = gtk::Label::new(Some("New entry amount"));
    let entry_count_entry = IntEntry::new(IntSize::Int16);
    entry_count_entry.set_value(mainsd_entries);

    let ok_button = gtk::Button::with_label("Ok");
    ok_button.set_sensitive(true);
    let cancel_button = gtk::Button::with_label("Cancel");
    let w = window.clone();
    cancel_button.connect_clicked(move |_| {
        w.close();
    });
    let sprite_info = sprite_info.clone();
    let entry_count_entry2 = entry_count_entry.clone();
    let w = window.clone();
    ok_button.connect_clicked(move |_| {
        let mut files = match sprite_info.files.try_lock() {
            Ok(o) => o,
            _ => return,
        };
        let new_count = entry_count_entry2.get_value() as u16;
        if new_count == 0 {
            error_msg_box(&w, "Invalid new entry count");
            return;
        }
        if let Err(e) = files.resize_entry_counts(new_count) {
            error_msg_box(&w, &format!("Failed to resize: {:?}", e));
            return;
        }
        info_msg_box(&w, &format!("Resized to {} sprites", new_count));
        crate::ui().files_changed(&files);
        let dirty = files.has_changes();
        drop(files);
        if let Some(a) = crate::lookup_action(&sprite_info.sprite_actions, "is_dirty") {
            a.activate(Some(&dirty.to_variant()));
        }
        w.close();
    });

    let bx = box_vertical(&[
        &readme_msg,
        &gtk::Separator::new(gtk::Orientation::Horizontal),
        &entry_msg,
        &box_horizontal(&[
            &entry_count_label,
            entry_count_entry.widget(),
        ]),
        &gtk::Separator::new(gtk::Orientation::Horizontal),
        &box_horizontal(&[
            &ok_button,
            &cancel_button,
        ]),
    ]);
    window.set_child(Some(&bx));
    window.set_default_width(350);
    window.set_title(Some("Edit sprite count"));
    window.set_modal(true);
    window.set_transient_for(Some(parent));
    window.show();
}
