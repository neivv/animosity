use gdk;
use gio;
use gtk;

use gio::prelude::*;
use gtk::prelude::*;

use ::lookup_action;

#[derive(Clone)]
pub struct IntEntry {
    pub entry: gtk::Entry,
    pub frame: gtk::Frame,
}

pub enum IntSize {
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

impl IntEntry {
    pub fn new(size: IntSize) -> IntEntry {
        let max_len = match size {
            IntSize::Int16 => 5,
            IntSize::Int32 => 10,
        };
        let entry = gtk::Entry::new();
        entry.set_has_frame(false);
        entry.set_max_length(max_len);
        entry.set_width_chars(max_len);
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
        IntEntry {
            entry,
            frame,
        }
    }

    pub fn connect_actions<A: IsA<gio::ActionMap>>(
        &self,
        actions: &A,
        init_action: &str,
        edit_action: &str,
    ) {
        self.entry.connect_focus_out_event(|s, _| {
            let s = match s.clone().downcast::<gtk::Entry>() {
                Ok(o) => o,
                Err(_) => return Inhibit(false),
            };
            if let Some(fix) = fix_text(&s.get_text().unwrap_or_else(|| "".into())) {
                s.set_text(&fix);
            }
            Inhibit(false)
        });
        self.entry.connect_key_press_event(|_s, key| {
            use gdk::enums::key;

            let modifier = key.get_state();
            let key = key.get_keyval();
            match key {
                key::Tab | key::ISO_Left_Tab => return Inhibit(false),
                _ => (),
            }
            let acceptable = key == key::BackSpace ||
                key == key::Delete ||
                key >= '0' as u8 as u32 && key <= '9' as u8 as u32 ||
                modifier.contains(gdk::ModifierType::CONTROL_MASK);
            if acceptable {
                // EMIT CHANGE
                Inhibit(false)
            } else {
                println!("{}", key);
                Inhibit(true)
            }
        });
        let entry = self.entry.clone();
        if let Some(a) = lookup_action(actions, init_action) {
            a.connect_activate(move |_, param| {
                if let Some(val) = param.as_ref().and_then(|x| x.get::<u32>()) {
                    entry.set_text(&val.to_string());
                }
            });
        } else {
            println!("NO ACTION {}", init_action);
        }
        if let Some(a) = lookup_action(actions, edit_action) {
            self.entry.connect_property_text_notify(move |s| {
                if let Some(text) = s.get_text() {
                    if let Ok(i) = text.parse::<u32>() {
                        a.change_state(&i.into());
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
