use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use gtk::prelude::*;
use gtk::glib::signal::Propagation;
use gtk::{gdk, glib, gio};

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
    let mut hints = entry.input_hints();
    hints.remove(gtk::InputHints::EMOJI);
    hints.insert(gtk::InputHints::NO_EMOJI);
    entry.set_input_hints(hints);
    let frame = gtk::Frame::new(None);
    <_ as gtk::prelude::WidgetExt>::set_widget_name(&frame, "entry_frame");
    frame.set_child(Some(&entry));
    let style_ctx = frame.style_context();
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
        entry.set_max_width_chars(max_len + 1);
        Arc::new(IntEntry {
            entry,
            frame,
            disable_edit_events: AtomicUsize::new(0),
        })
    }

    pub fn set_value(&self, val: u32) {
        self.entry.buffer().set_text(&val.to_string());
    }

    pub fn get_value(&self) -> u32 {
        self.entry.buffer().text().parse::<u32>().unwrap_or(0)
    }

    pub fn connect_actions<A: IsA<gio::ActionMap>>(
        this: &Arc<IntEntry>,
        actions: &A,
        init_action: &str,
        edit_action: &str,
    ) {
        let focus_ctrl = gtk::EventControllerFocus::new();
        let key_ctrl = gtk::EventControllerKey::new();
        key_ctrl.set_propagation_phase(gtk::PropagationPhase::Capture);
        let weak = Arc::downgrade(&this);
        focus_ctrl.connect_leave(move |_s| {
            if let Some(this) = weak.upgrade() {
                let buffer = this.entry.buffer();
                if let Some(fix) = fix_text(&buffer.text()) {
                    buffer.set_text(&fix);
                }
            }
        });
        key_ctrl.connect_key_pressed(|_s, key, _keycode, modifier| {
            use glib::translate::IntoGlib;

            let acceptable = key == gdk::Key::BackSpace ||
                key == gdk::Key::Delete ||
                key.to_unicode()
                    .filter(|&c| c as u32 >= b'0' as u32 && c as u32 <= b'9' as u32)
                    .is_some() ||
                modifier.contains(gdk::ModifierType::CONTROL_MASK);
            if acceptable {
                Propagation::Proceed
            } else {
                if key.into_glib() > 65000 {
                    return Propagation::Proceed;
                }
                Propagation::Stop
            }
        });
        this.entry.add_controller(focus_ctrl);
        this.entry.add_controller(key_ctrl);
        let weak = Arc::downgrade(&this);
        if let Some(a) = lookup_action(actions, init_action) {
            a.connect_activate(move |_, param| {
                if let Some(s) = weak.upgrade() {
                    if let Some(val) = param.as_ref().and_then(|x| x.get::<u32>()) {
                        s.disable_edit_events.fetch_add(1, Ordering::Relaxed);
                        s.entry.set_text(&val.to_string());
                        s.disable_edit_events.fetch_sub(1, Ordering::Relaxed);
                    }
                }
            });
        } else {
            println!("NO ACTION {}", init_action);
        }
        if let Some(a) = lookup_action(actions, edit_action) {
            let weak = Arc::downgrade(&this);
            this.entry.connect_text_notify(move |s| {
                if let Some(t) = weak.upgrade() {
                    if t.disable_edit_events.load(Ordering::Relaxed) == 0 {
                        if let Ok(i) = s.text().parse::<u32>() {
                            a.activate(Some(&i.to_variant()));
                        }
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

    pub fn text(&self) -> String {
        self.0.entry.text().into()
    }

    pub fn set_text(&self, text: &str) {
        self.0.entry.set_text(text)
    }
}
