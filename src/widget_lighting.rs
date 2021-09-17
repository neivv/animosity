use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

use glib::types::Type;
use glib::value::Value;
use gio::prelude::*;
use gtk::prelude::*;

use crate::anim_lit;
use crate::files::{Files, SpriteFiles};
use crate::recurse_checked_mutex::Mutex;
use crate::SpriteType;

pub struct SpriteLighting {
    bx: gtk::Box,
    enabled: gtk::CheckButton,
    tree: gtk::TreeView,
    // Columns: frame idx, color hex string, x, y, intensity, radius
    store: gtk::ListStore,
    files: Arc<Mutex<Files>>,
    active_sprite: AtomicUsize,
    listening_input: AtomicBool,
    sprite_actions: gio::SimpleActionGroup,
}

impl SpriteLighting {
    pub fn new(
        files: &Arc<Mutex<Files>>,
        sprite_actions: gio::SimpleActionGroup,
    ) -> Arc<SpriteLighting> {
        use crate::ui_helpers::*;

        let enabled = gtk::CheckButton::with_label("Enabled");
        let columns = &[Type::U32, Type::STRING, Type::I32, Type::I32, Type::U32, Type::U32];
        let store = gtk::ListStore::new(columns);
        let tree = gtk::TreeView::with_model(&store);
        for i in 0..6 {
            let col = gtk::TreeViewColumn::new();
            let a;
            let b;
            let renderer: &gtk::CellRendererText = if i == 1 {
                a = gtk_ext::CellRendererColor::new();
                a.upcast_ref()
            } else {
                b = gtk::CellRendererText::new();
                &b
            };
            col.set_title(match i {
                0 => "Frame",
                1 => "Color",
                2 => "X",
                3 => "Y",
                4 => "Intensity",
                5 | _ => "Radius",
            });
            if i == 0 {
                renderer.set_editable(false);
            } else {
                renderer.set_editable(true);
            }
            let store2 = store.clone();
            renderer.connect_edited(move |_, path, value| {
                if let Some(iter) = store2.iter(&path) {
                    let value = match i {
                        1 => {
                            if rgb_color_from_string(value).is_none() {
                                return;
                            }
                            Value::from(value)
                        }
                        _ => match columns[i as usize] {
                            Type::U32 => match value.parse::<u32>() {
                                Ok(o) => Value::from(&o),
                                Err(_) => return,
                            },
                            Type::I32 => match value.parse::<i32>() {
                                Ok(o) => Value::from(&o),
                                Err(_) => return,
                            },
                            _ => return,
                        }
                    };
                    store2.set_value(&iter, i as u32, &value);
                }
            });
            col.pack_end(renderer, true);
            col.add_attribute(renderer, "text", i);
            col.set_sizing(gtk::TreeViewColumnSizing::Fixed);
            tree.append_column(&col);
        }
        tree.set_fixed_height_mode(true);
        tree.set_activate_on_single_click(true);
        tree.selection().set_mode(gtk::SelectionMode::Multiple);
        let none: Option<&gtk::Adjustment> = None;
        let tree_scroll = gtk::ScrolledWindow::new(none, none);
        tree_scroll.add(&tree);
        tree_scroll.set_overlay_scrolling(false);
        tree_scroll.set_vscrollbar_policy(gtk::PolicyType::Always);
        tree_scroll.set_hscrollbar_policy(gtk::PolicyType::Never);
        tree_scroll.set_propagate_natural_width(true);
        tree_scroll.set_propagate_natural_height(true);
        tree_scroll.set_min_content_height(100);
        let bx = box_vertical(&[
            &enabled as &dyn BoxableWidget,
            &box_expand(&tree_scroll),
        ]);
        let bx = crate::label_section("Sprite lighting", &bx);
        bx.set_margin_start(5);
        bx.set_margin_end(5);
        let result = Arc::new(SpriteLighting {
            bx,
            files: files.clone(),
            active_sprite: AtomicUsize::new(0),
            enabled,
            tree,
            store,
            listening_input: AtomicBool::new(true),
            sprite_actions,
        });
        let this = result.clone();
        result.enabled.connect_toggled(move |check| {
            if this.listening_input.load(Ordering::Relaxed) == false {
                return;
            }
            this.enable_for_current(check.is_active());
        });
        let this = result.clone();
        result.store.connect_row_changed(move |store, path, iter| {
            if this.listening_input.load(Ordering::Relaxed) == false {
                return;
            }
            || -> Option<()> {
                let dirty;
                let &frame_id = path.indices().get(0)?;
                let frame = lit_frame_from_store_iter(&store, &iter)?;
                {
                    let mut files = this.files.lock();
                    let lit = files.lit()?;
                    let sprite_index = this.active_sprite.load(Ordering::Relaxed);
                    let sprite = lit.sprite_mut(sprite_index)?;
                    *sprite.frames_mut().get_mut(frame_id as usize)? = frame;
                    dirty = files.has_changes();
                }
                if let Some(a) = crate::lookup_action(&this.sprite_actions, "is_dirty") {
                    a.activate(Some(&dirty.to_variant()));
                }
                Some(())
            }();
        });
        let this = result.clone();
        result.tree.connect_key_press_event(move |_, event| {
            if this.listening_input.load(Ordering::Relaxed) == false {
                return Inhibit(false);
            }
            if event.state().intersects(gdk::ModifierType::CONTROL_MASK) {
                match event.keyval() {
                    gdk::keys::constants::c => {
                        this.copy_row();
                        return Inhibit(true);
                    }
                    gdk::keys::constants::v => {
                        this.paste_row();
                        return Inhibit(true);
                    }
                    _ => (),
                }
            }
            Inhibit(false)
        });
        let this = result.clone();
        result.tree.connect_button_release_event(move |_, event| {
            // Rclick
            if event.button() == 3 {
                let menu = this.clone().make_rclick_menu();
                menu.popup_at_pointer(Some(event));
                Inhibit(true)
            } else {
                Inhibit(false)
            }
        });
        result
    }

    pub fn widget(&self) -> gtk::Widget {
        self.bx.clone().upcast()
    }

    pub fn select_sprite(&self, index: usize) {
        self.active_sprite.store(index, Ordering::Relaxed);
        self.reload_data();
    }

    fn copy_row(&self) {
        use std::fmt::Write;
        let mut result = String::new();
        let frames = self.tree.selection()
            .selected_rows().0
            .into_iter()
            .filter_map(|path| self.store.iter(&path))
            .filter_map(|iter| lit_frame_from_store_iter(&self.store, &iter));
        for f in frames {
            writeln!(result, "{}, {}, {}, {}, {}", f.x, f.y, f.color, f.intensity, f.radius)
                .unwrap();
        }
        if !result.is_empty() {
            let atom = gdk::Atom::intern("CLIPBOARD");
            let clip = gtk::Clipboard::get(&atom);
            clip.set_text(&result);
        }
    }

    fn paste_row(&self) {
        let frames = match frames_from_clipboard() {
            Some(s) => s,
            None => return,
        };
        let rows = self.tree.selection().selected_rows().0;
        if rows.len() == 0 {
            return;
        }
        if rows.len() == 1 {
            // Paste starting from current row
            let index = match rows.get(0).and_then(|path| path.indices().get(0).cloned()) {
                Some(s) => s,
                None => return,
            };
            for (frame, index) in frames.iter().zip(index..) {
                let path = gtk::TreePath::from_indicesv(&[index]);
                if let Some(iter) = self.store.iter(&path) {
                    set_lit_frame_in_store(&self.store, &iter, frame);
                }
            }
        } else {
            // Paste over any selected rows
            for (frame, path) in frames.iter().zip(rows.iter()) {
                if let Some(iter) = self.store.iter(path) {
                    set_lit_frame_in_store(&self.store, &iter, frame);
                }
            }
        }
    }

    fn make_rclick_menu(self: Arc<Self>) -> gtk::Menu {
        let menu = gtk::Menu::new();
        let item = gtk::MenuItem::with_label("Copy");
        let this = self.clone();
        item.connect_activate(move |_| {
            this.copy_row();
        });
        item.show();
        menu.append(&item);
        let item = gtk::MenuItem::with_label("Paste");
        item.connect_activate(move |_| {
            self.paste_row();
        });
        item.show();
        if frames_from_clipboard().is_none() {
            item.set_sensitive(false);
        }
        menu.append(&item);
        menu
    }

    fn enable_for_current(&self, enable: bool) {
        let dirty;
        {
            let sprite_index = self.active_sprite.load(Ordering::Relaxed);
            let mut files = self.files.lock();
            let frame_count = {
                match files.file(sprite_index, SpriteType::Hd) {
                    Ok(Some(file)) => match file.frames() {
                        Some(s) => s.len(),
                        None => return,
                    },
                    _ => return,
                }
            };
            let lit = match files.lit() {
                Some(s) => s,
                None => return,
            };
            if enable {
                lit.enable_sprite(sprite_index, frame_count as u32);
            } else {
                lit.disable_sprite(sprite_index);
            }
            dirty = files.has_changes();
        }
        if let Some(a) = crate::lookup_action(&self.sprite_actions, "is_dirty") {
            a.activate(Some(&dirty.to_variant()));
        }
        self.reload_data();
    }

    fn reload_data(&self) {
        self.listening_input.store(false, Ordering::Relaxed);
        self.reload_data_inner();
        self.listening_input.store(true, Ordering::Relaxed);
    }

    fn reload_data_inner(&self) {
        let index = self.active_sprite.load(Ordering::Relaxed);
        let mut files = self.files.lock();
        let anim_sprite_frame_count = {
            match files.file(index, SpriteType::Hd) {
                Ok(Some(file)) => file.frames().map(|f| f.len()),
                _ => None,
            }
        };
        let lit = match files.lit() {
            Some(s) => s,
            None => {
                self.bx.set_sensitive(false);
                let is_sprite_anim_set = match files.sprites().get(0) {
                    Some(SpriteFiles::AnimSet(..)) => true,
                    _ => false,
                };
                let tooltip = match is_sprite_anim_set {
                    true => "No lighting data loaded.\n\
                        You can edit sprite lighting by adding the file anim\\main.lit.",
                    false => "Lighting data is only for images.dat sprites.",
                };
                self.bx.set_tooltip_text(Some(tooltip));
                return;
            }
        };
        self.bx.set_has_tooltip(false);
        self.store.clear();
        // Resize lit frames to match sprite frames.
        // They may mismatch if the sprite was edited without updating lit.
        // It would be nice to do this on sprite load, but that would require
        // opening all HD anim files (which have lighting info) to get their
        // frame counts, so it is done here lazily instead.
        let needs_resize = if let Some(lit_sprite) = lit.sprite(index) {
            if let Some(anim_sprite_frame_count) = anim_sprite_frame_count {
                lit_sprite.frame_count() as usize == anim_sprite_frame_count
            } else {
                false
            }
        } else {
            false
        };
        if needs_resize {
            if let Some(new_size) = anim_sprite_frame_count {
                lit.sprite_mut(index)
                    .expect("Index was supposed to be ok")
                    .set_frame_count(new_size as u32);
            }
        }

        if let Some(sprite) = lit.sprite_mut(index) {
            self.enabled.set_active(true);
            let store = &self.store;
            for (i, frame) in sprite.frames_mut().iter().enumerate() {
                let iter = store.append();
                store.set_value(&iter, 0, &Value::from(&(i as u32)));
                set_lit_frame_in_store(store, &iter, &frame);
            }
        } else {
            self.enabled.set_active(false);
        }
    }
}

fn frames_from_clipboard() -> Option<Vec<anim_lit::Frame>> {
    let atom = gdk::Atom::intern("CLIPBOARD");
    let clip = gtk::Clipboard::get(&atom);
    clip.wait_for_text()
        .and_then(|x| {
            let text = x.as_str();
            text.split("\n")
                .filter(|text| !text.is_empty())
                .map(|text| {
                    let mut tokens = text.split(", ");
                    let frame = anim_lit::Frame {
                        x: tokens.next()?.parse().ok()?,
                        y: tokens.next()?.parse().ok()?,
                        color: tokens.next()?.parse().ok()?,
                        intensity: tokens.next()?.parse().ok()?,
                        radius: tokens.next()?.parse().ok()?,
                    };
                    if tokens.next().is_some() {
                        None
                    } else {
                        Some(frame)
                    }
                }).collect::<Option<Vec<_>>>()
        })
        .filter(|vec| !vec.is_empty())
}

fn lit_frame_from_store_iter(
    store: &gtk::ListStore,
    iter: &gtk::TreeIter,
) -> Option<anim_lit::Frame> {
    let color = store.value(iter, 1).get::<String>().ok()?;
    let color = rgb_color_from_string(&color)?;
    let x = store.value(iter, 2).get::<i32>().ok()?;
    let y = store.value(iter, 3).get::<i32>().ok()?;
    let intensity = store.value(iter, 4).get::<u32>().ok()?;
    let radius = store.value(iter, 5).get::<u32>().ok()?;
    Some(anim_lit::Frame {
        x,
        y,
        color,
        intensity,
        radius,
    })
}

fn set_lit_frame_in_store(
    store: &gtk::ListStore,
    iter: &gtk::TreeIter,
    frame: &anim_lit::Frame,
) {
    store.set_value(iter, 1, &Value::from(&rgb_color_string(frame.color)));
    store.set_value(iter, 2, &Value::from(&frame.x));
    store.set_value(iter, 3, &Value::from(&frame.y));
    store.set_value(iter, 4, &Value::from(&frame.intensity));
    store.set_value(iter, 5, &Value::from(&frame.radius));
}

fn rgb_color_string(color: u32) -> String {
    format!("#{:02x}{:02x}{:02x}", color & 0xff, (color >> 8) & 0xff, (color >> 16) & 0xff)
}

fn rgb_color_from_string(mut text: &str) -> Option<u32> {
    text = text.trim();
    // Allow also just 123456 if the user loses the hash when editing
    if text.starts_with("#") {
        text = &text[1..]
    };
    if text.len() != 6 {
        return None;
    }
    let red = u32::from_str_radix(&text[0..2], 16).ok()?;
    let green = u32::from_str_radix(&text[2..4], 16).ok()?;
    let blue = u32::from_str_radix(&text[4..6], 16).ok()?;
    Some(red | (green << 8) | (blue << 16) | 0xff00_0000)
}

mod gtk_ext {
    use gtk::subclass::{prelude::*};
    use gtk::prelude::*;

    #[derive(Default)]
    pub struct CellRendererColorC {
    }

    #[glib::object_subclass]
    impl ObjectSubclass for CellRendererColorC {
        const NAME: &'static str = "anim_CellRendererColor";
        type Type = CellRendererColor;
        type ParentType = gtk::CellRendererText;
    }

    impl ObjectImpl for CellRendererColorC {
    }

    impl CellRendererImpl for CellRendererColorC {
        fn preferred_width<P: IsA<gtk::Widget>>(
            &self,
            renderer: &CellRendererColor,
            widget: &P,
        ) -> (i32, i32) {
            let (min, natural) = self.parent_preferred_width(renderer, widget);
            let (_, natural_h) = self.parent_preferred_height_for_width(renderer, widget, natural);
            let color_block_size = natural_h;
            (min + color_block_size, natural + color_block_size)
        }

        fn render<P: glib::IsA<gtk::Widget>>(
            &self,
            renderer: &CellRendererColor,
            cr: &cairo::Context,
            widget: &P,
            background_area: &gdk::Rectangle,
            cell_area: &gdk::Rectangle,
            flags: gtk::CellRendererState,
        ) {
            let text = match renderer.text() {
                Some(s) => s,
                None => return,
            };
            let color_block_size = cell_area.height;
            let text_width = cell_area.width - color_block_size;
            if let Some(color) = super::rgb_color_from_string(text.as_str()) {
                let (x_padding, y_padding) = renderer.padding();
                let result = cr.save()
                    .and_then(|()| {
                        cr.new_path();
                        cr.set_line_width(1.0);
                        cr.set_source_rgb(0.0, 0.0, 0.0);
                        cr.rectangle(
                            (cell_area.x + text_width + x_padding) as f64,
                            (cell_area.y + y_padding) as f64,
                            (color_block_size - x_padding * 2) as f64,
                            (color_block_size - y_padding * 2) as f64,
                        );
                        cr.stroke()?;
                        cr.new_path();
                        cr.set_line_width(1.0);
                        cr.set_source_rgb(
                            (color & 0xff) as f64 / 255.0,
                            ((color >> 8) & 0xff) as f64 / 255.0,
                            ((color >> 16) & 0xff) as f64 / 255.0,
                        );
                        cr.rectangle(
                            (cell_area.x + text_width + x_padding) as f64 + 1.0,
                            (cell_area.y + y_padding) as f64 + 1.0,
                            (color_block_size - x_padding * 2) as f64 - 2.0,
                            (color_block_size - y_padding * 2) as f64 - 2.0,
                        );
                        cr.fill()?;
                        cr.restore()
                    });
                if let Err(e) = result {
                    println!("Cairo error {}", e);
                }
            }
            let text_area = gdk::Rectangle {
                x: cell_area.x,
                y: cell_area.y,
                width: text_width,
                height: cell_area.height,
            };
            self.parent_render(renderer, cr, widget, background_area, &text_area, flags);
        }
    }

    impl CellRendererTextImpl for CellRendererColorC {
    }

    glib::wrapper! {
        pub struct CellRendererColor(ObjectSubclass<CellRendererColorC>)
            @extends gtk::CellRendererText, gtk::CellRenderer;
    }

    impl CellRendererColor {
        pub fn new() -> CellRendererColor {
            glib::Object::new(&[])
                .expect("Failed to create CellRendererColor")
        }
    }
}
