use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use gio::prelude::*;
use gtk::prelude::*;

use crate::lookup_action;

pub struct IntEntry {
    pub entry: gtk::Entry,
    pub frame: gtk::Frame,
    disable_edit_events: AtomicUsize,
}

#[derive(Clone)]
pub struct TextEntry(Arc<TextEntryInner>);

struct TextEntryInner {
    entry: gtk::Entry,
    frame: gtk::Frame,
}

pub enum IntSize {
    Int8,
    Int16,
    Int32,
}

fn fix_text(text: &str) -> Option<String> {
    let text = text.trim();
    if text.len() == 0 {
        return Some("0".into());
    }
    if text.chars().any(|x| !x.is_numeric()) {
        return Some("0".into());
    }
    None
}

pub fn entry() -> (gtk::Entry, gtk::Frame) {
    let entry = gtk::Entry::new();
    entry.set_has_frame(false);
    let mut hints = entry.get_input_hints();
    hints.remove(gtk::InputHints::EMOJI);
    hints.insert(gtk::InputHints::NO_EMOJI);
    entry.set_input_hints(hints);
    let frame = gtk::Frame::new(None);
    <_ as gtk::WidgetExt>::set_widget_name(&frame, "entry_frame");
    frame.add(&entry);
    let style_ctx = frame.get_style_context();
    let css = crate::get_css_provider();
    style_ctx.add_provider(&css, 600 /* GTK_STYLE_PROVIDER_PRIORITY_APPLICATION */);
    (entry, frame)
}

impl IntEntry {
    pub fn new(size: IntSize) -> Arc<IntEntry> {
        let max_len = match size {
            IntSize::Int8 => 3,
            IntSize::Int16 => 5,
            IntSize::Int32 => 10,
        };
        let (entry, frame) = entry();
        entry.set_max_length(max_len);
        entry.set_width_chars(max_len);
        Arc::new(IntEntry {
            entry,
            frame,
            disable_edit_events: AtomicUsize::new(0),
        })
    }

    pub fn set_value(&self, val: u32) {
        self.entry.set_text(&val.to_string());
    }

    pub fn get_value(&self) -> u32 {
        self.entry.get_text().parse::<u32>().unwrap_or(0)
    }

    pub fn connect_actions<A: IsA<gio::ActionMap>>(
        this: &Arc<IntEntry>,
        actions: &A,
        init_action: &str,
        edit_action: &str,
    ) {
        this.entry.connect_focus_out_event(|s, _| {
            let s = match s.clone().downcast::<gtk::Entry>() {
                Ok(o) => o,
                Err(_) => return Inhibit(false),
            };
            if let Some(fix) = fix_text(&s.get_text()) {
                s.set_text(&fix);
            }
            Inhibit(false)
        });
        this.entry.connect_key_press_event(|_s, key| {
            use gdk::keys::constants;
            use glib::translate::ToGlib;

            let modifier = key.get_state();
            let key = key.get_keyval();
            let acceptable = key == constants::BackSpace ||
                key == constants::Delete ||
                key.to_unicode()
                    .filter(|&c| c as u32 >= b'0' as u32 && c as u32 <= b'9' as u32)
                    .is_some() ||
                modifier.contains(gdk::ModifierType::CONTROL_MASK);
            if acceptable {
                // EMIT CHANGE
                Inhibit(false)
            } else {
                if key.to_glib() > 65000 {
                    return Inhibit(false);
                }
                Inhibit(true)
            }
        });
        let s = this.clone();
        if let Some(a) = lookup_action(actions, init_action) {
            a.connect_activate(move |_, param| {
                if let Some(val) = param.as_ref().and_then(|x| x.get::<u32>()) {
                    s.disable_edit_events.fetch_add(1, Ordering::Relaxed);
                    s.entry.set_text(&val.to_string());
                    s.disable_edit_events.fetch_sub(1, Ordering::Relaxed);
                }
            });
        } else {
            println!("NO ACTION {}", init_action);
        }
        if let Some(a) = lookup_action(actions, edit_action) {
            let t = this.clone();
            this.entry.connect_property_text_notify(move |s| {
                if t.disable_edit_events.load(Ordering::Relaxed) == 0 {
                    if let Ok(i) = s.get_text().parse::<u32>() {
                        a.activate(Some(&i.into()));
                    }
                }
            });
        } else {
            println!("NO ACTION {}", edit_action);
        }
    }

    pub fn widget(&self) -> &gtk::Widget {
        self.frame.upcast_ref()
    }

    pub fn clear(&self) {
        self.entry.delete_text(0, -1);
    }
}

impl TextEntry {
    pub fn new() -> TextEntry {
        let (entry, frame) = entry();
        TextEntry(Arc::new(TextEntryInner {
            entry,
            frame,
        }))
    }

    pub fn widget(&self) -> &gtk::Widget {
        self.0.frame.upcast_ref()
    }

    pub fn get_text(&self) -> String {
        self.0.entry.get_text().into()
    }

    pub fn set_text(&self, text: &str) {
        self.0.entry.set_text(text)
    }
}
