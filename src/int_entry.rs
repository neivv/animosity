use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use gdk;
use gio;
use gtk;

use gio::prelude::*;
use gtk::prelude::*;

use ::lookup_action;

pub struct IntEntry {
    pub entry: gtk::Entry,
    pub frame: gtk::Frame,
    disable_edit_events: AtomicUsize,
}

pub enum IntSize {
    Int8,
    Int16,
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
    <_ as gtk::WidgetExt>::set_name(&frame, "entry_frame");
    frame.add(&entry);
    if let Some(style_ctx) = frame.get_style_context() {
        let css = ::get_css_provider();
        style_ctx.add_provider(&css, 600 /* GTK_STYLE_PROVIDER_PRIORITY_APPLICATION */);
    }
    (entry, frame)
}

impl IntEntry {
    pub fn new(size: IntSize) -> Arc<IntEntry> {
        let max_len = match size {
            IntSize::Int8 => 3,
            IntSize::Int16 => 5,
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
        self.entry.get_text().and_then(|x| x.parse::<u32>().ok()).unwrap_or(0)
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
            if let Some(fix) = fix_text(&s.get_text().unwrap_or_else(|| "".into())) {
                s.set_text(&fix);
            }
            Inhibit(false)
        });
        this.entry.connect_key_press_event(|_s, key| {
            use gdk::enums::key;

            let modifier = key.get_state();
            let key = key.get_keyval();
            let acceptable = key == key::BackSpace ||
                key == key::Delete ||
                key >= '0' as u8 as u32 && key <= '9' as u8 as u32 ||
                modifier.contains(gdk::ModifierType::CONTROL_MASK);
            if acceptable {
                // EMIT CHANGE
                Inhibit(false)
            } else {
                if key > 65000 {
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
                    if let Some(text) = s.get_text() {
                        if let Ok(i) = text.parse::<u32>() {
                            a.activate(&i.into());
                        }
                    }
                }
            });
        } else {
            println!("NO ACTION {}", edit_action);
        }
    }

    pub fn widget(&self) -> gtk::Widget {
        self.frame.clone().upcast()
    }

    pub fn clear(&self) {
        self.entry.delete_text(0, -1);
    }
}
