use std::cell::Cell;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

use gtk::glib::prelude::*;
use gtk::prelude::*;
use gtk::subclass::prelude::*;
use gtk::{glib, gio, gdk};
use gtk::glib::signal::Propagation;
use gtk::glib::Properties;

use crate::anim_lit;
use crate::files::{Files, SpriteFiles};
use crate::recurse_checked_mutex::Mutex;
use crate::SpriteType;
use crate::util::*;

pub struct SpriteLighting {
    bx: gtk::Box,
    enabled: gtk::CheckButton,
    tree: gtk::ColumnView,
    // Columns: frame idx, color hex string, x, y, intensity, radius
    model: gtk::MultiSelection,
    files: Arc<Mutex<Files>>,
    active_sprite: AtomicUsize,
    listening_input: AtomicBool,
    sprite_actions: gio::SimpleActionGroup,
    lighting_actions: gio::SimpleActionGroup,
}

impl SpriteLighting {
    pub fn new(
        files: &Arc<Mutex<Files>>,
        sprite_actions: gio::SimpleActionGroup,
    ) -> Arc<SpriteLighting> {
        use crate::ui_helpers::*;

        let lighting_actions = gio::SimpleActionGroup::new();

        let click_ctrl = gtk::GestureClick::new();
        click_ctrl.set_propagation_phase(gtk::PropagationPhase::Capture);
        click_ctrl.set_button(3); // Rclick

        let enabled = gtk::CheckButton::with_label("Enabled");
        let inner_model = gio::ListStore::new::<LitFrameObject>();
        let model = gtk::MultiSelection::new(Some(inner_model));
        let tree = gtk::ColumnView::new(Some(model.clone()));
        for i in 0..6 {
            let title = match i {
                0 => "Frame",
                1 => "Color",
                // Spaces are a hack to make things not resize when there are no frames
                2 => "X　　",
                3 => "Y　　",
                4 => "Intensity",
                5 | _ => "Radius",
            };
            let factory = gtk::SignalListItemFactory::new();
            let count = Cell::new(0);
            factory.connect_bind(move |_, item| {
                /*
                let item = match item.downcast_ref::<gtk::ColumnViewCell>() {
                    Some(s) => s,
                    None => {
                        debug_assert!(false);
                        return;
                    }
                };
                let item = item.child().unwrap();
                let p = item.parent().unwrap();
                let p = p.parent().unwrap();
                */
                if i == 0 {
                //std::mem::forget(p);
                    //println!("Bind {p:?} {} {}", p.ref_count(), count.get());
                    count.set(count.get() + 1);
                }
            });
            let count = Cell::new(0);
            factory.connect_unbind(move |_, item| {
                if i == 0 {
                    println!("Unbind {}", count.get());
                    count.set(count.get() + 1);
                }
            });
            let count = Cell::new(0);
            factory.connect_teardown(move |_, item| {
                if i == 0 {
                    println!("Teardown {}", count.get());
                    count.set(count.get() + 1);
                }
            });
            let count = Cell::new(0);
            factory.connect_setup(move |_, item| {
                let item = match item.downcast_ref::<gtk::ColumnViewCell>() {
                    Some(s) => s,
                    None => {
                        debug_assert!(false);
                        return;
                    }
                };
                item.set_selectable(true);
                item.set_activatable(true);
                let prop_name = match i {
                    0 => "index",
                    1 => "color",
                    2 => "x",
                    3 => "y",
                    4 => "intensity",
                    5 | _ => "radius",
                };
                println!("Create col cell {i} {}", count.get());
                count.set(count.get() + 1);
                match i {
                    99 => {
                        let label = gtk::Label::new(None);
                        label.set_valign(gtk::Align::Start);
                        item.set_child(Some(&label));
                        item.property_expression("item")
                            .chain_property::<LitFrameObject>(prop_name)
                            .bind(&label, "label", gtk::Widget::NONE);
                    }
                    _ => {
                        let entry = gtk::Entry::new();
                        entry.set_has_frame(false);
                        //let entry = gtk::Text::new();
                        entry.set_width_chars(6);
                        entry.set_max_width_chars(6);
                        if i == 0 {
                            entry.set_sensitive(false);
                            entry.add_css_class("cell-entry-disabled");
                        }
                        let buffer = entry.buffer();
                        item.set_child(Some(&entry));
                        /*
                        item.property_expression("item")
                            .chain_property::<LitFrameObject>(prop_name)
                            .bind(&buffer, "text", gtk::Widget::NONE);
                        */
                    }
                }
            });
            let col = gtk::ColumnViewColumn::new(Some(title), Some(factory));
            tree.append_column(&col);
        }
        tree.set_single_click_activate(false);
        tree.set_enable_rubberband(false);
        tree.set_reorderable(false);
        let tree_scroll = gtk::ScrolledWindow::new();
        tree_scroll.set_child(Some(&tree));
        //tree_scroll.set_overlay_scrolling(false);
        tree_scroll.set_vscrollbar_policy(gtk::PolicyType::Always);
        tree_scroll.set_hscrollbar_policy(gtk::PolicyType::Never);
        //tree_scroll.set_propagate_natural_width(true);
        //tree_scroll.set_propagate_natural_height(true);
        tree_scroll.set_min_content_height(100);

        let menu = {
            use crate::menu_item_with_accel as with_accel;
            let model = gio::Menu::new();
            model.append_item(&with_accel("Copy", "lighting.copy", "<Ctrl>C"));
            model.append_item(&with_accel("Paste", "lighting.paste", "<Ctrl>V"));
            let menu = gtk::PopoverMenu::from_model(Some(&model));
            menu.set_has_arrow(false);
            menu.set_halign(gtk::Align::Start);
            menu.set_valign(gtk::Align::Start);
            menu.set_parent(&tree);
            menu
        };
        let bx = box_vertical(&[
            &enabled as &dyn BoxableWidget,
            &box_expand(&tree_scroll),
        ]);
        let bx = crate::label_section("Sprite lighting", &bx);
        bx.set_margin_start(5);
        bx.set_margin_end(5);
        bx.insert_action_group("lighting", Some(&lighting_actions));
        let result = Arc::new(SpriteLighting {
            bx,
            files: files.clone(),
            active_sprite: AtomicUsize::new(0),
            enabled,
            tree,
            model,
            listening_input: AtomicBool::new(true),
            sprite_actions,
            lighting_actions,
        });
        result.create_lighting_actions();
        let this = result.clone();
        result.enabled.connect_toggled(move |check| {
            if this.listening_input.load(Ordering::Relaxed) == false {
                return;
            }
            this.enable_for_current(check.is_active());
        });
        let this = result.clone();
        result.model.connect_items_changed(move |model, pos, _removed, _added| {
            let this = &this;
            if this.listening_input.load(Ordering::Relaxed) == false {
                return;
            }
            || -> Option<()> {
                let frame_id = pos;
                let frame = lit_frame_from_model(&model, pos)?;
                let dirty;
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
        let key_ctrl = gtk::EventControllerKey::new();
        key_ctrl.set_propagation_phase(gtk::PropagationPhase::Capture);
        let this = result.clone();
        key_ctrl.connect_key_pressed(move |_s, key, _keycode, modifier| {
            if this.listening_input.load(Ordering::Relaxed) == false {
                return Propagation::Proceed;
            }
            if modifier.intersects(gdk::ModifierType::CONTROL_MASK) {
                match key {
                    gdk::Key::C => {
                        this.copy_row();
                        return Propagation::Stop;
                    }
                    gdk::Key::V => {
                        SpriteLighting::paste_row(&this);
                        return Propagation::Stop;
                    }
                    _ => (),
                }
            }
            Propagation::Proceed
        });
        let this = result.clone();
        click_ctrl.set_exclusive(true);
        let tree = result.tree.clone();
        click_ctrl.connect_pressed(move |_, _, x, y| {
            if let Some(widget) = tree.pick(x, y, gtk::PickFlags::INSENSITIVE) {
                println!("Got");
                /*
                if let Some(cell) = widget.ancestor(gtk::ColumnViewCell::static_type()) {
                    if let Some(cell) = cell.downcast_ref::<gtk::ColumnViewCell>().should() {
                        println!("Got cell {}", cell.position());
                    }
                }
                */
            }
            this.update_paste_state();
            menu.set_pointing_to(Some(&gdk::Rectangle::new(x as i32, y as i32, 1, 1)));
            menu.popup();
        });

        result.tree.add_controller(key_ctrl);
        result.tree.add_controller(click_ctrl);
        result
    }

    fn create_lighting_actions(self: &Arc<Self>) {
        fn action<F>(
            group: &gio::ActionMap,
            name: &str,
            enabled: bool,
            param_ty: Option<&str>,
            fun: F,
        ) -> gio::SimpleAction
        where F: Fn(&gio::SimpleAction, Option<&glib::Variant>) + 'static
        {
            let param_ty = param_ty.map(|x| {
                glib::VariantTy::new(x)
                    .unwrap_or_else(|_| panic!("Invalid variant type string {}", x))
            });
            let action = gio::SimpleAction::new(name, param_ty);
            action.set_enabled(enabled);
            action.connect_activate(fun);
            group.add_action(&action);
            action
        }
        let group = self.lighting_actions.upcast_ref();
        let this = self.clone();
        action(group, "copy", true, None, move |_, _| {
            this.copy_row();
        });
        let this = self.clone();
        action(group, "paste", true, None, move |_, _| {
            Self::paste_row(&this);
        });
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
        let selection = self.model.selection();
        let frames = iter_bitset(&selection)
            .filter_map(|n| lit_frame_from_model(&self.model, n));
        for f in frames {
            writeln!(result, "{}, {}, {}, {}, {}", f.x, f.y, f.color, f.intensity, f.radius)
                .unwrap();
        }
        if !result.is_empty() {
            if let Some(display) = gdk::Display::default() {
                let clip = display.clipboard();
                clip.set_text(&result);
            }
        }
    }

    fn paste_row(this: &Arc<Self>) {
        let this = this.clone();
        let paste_task = async move {
            let frames = match frames_from_clipboard().await {
                Some(s) => s,
                None => return,
            };
            let selection = this.model.selection();
            let size = selection.size();
            if size == 0 {
                return;
            }
            if size == 1 {
                // Paste starting from current row
                let index = selection.minimum();
                for (frame, index) in frames.iter().zip(index..) {
                    set_lit_frame_in_model(&this.model, index, frame);
                }
            } else {
                // Paste over any selected rows
                let mut iter = iter_bitset(&selection);
                'outer: loop {
                    for frame in frames.iter() {
                        let index = match iter.next() {
                            Some(s) => s,
                            None => break 'outer,
                        };
                        set_lit_frame_in_model(&this.model, index, frame);
                    }
                }
            }
        };
        glib::MainContext::default().spawn_local(paste_task);
    }

    fn update_paste_state(self: &Arc<Self>) {
        let this = self.clone();
        let set_sensitivity_task = async move {
            let frames = frames_from_clipboard().await;
            if let Some(a) = crate::lookup_action(&this.lighting_actions, "paste") {
                a.set_enabled(frames.is_some());
            }
        };
        glib::MainContext::default().spawn_local(set_sensitivity_task);
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
        let model = self.model.model();
        let list_store_model = model.as_ref()
            .and_then(|x| x.downcast_ref::<gio::ListStore>())
            .should();
        if let Some(model) = list_store_model {
            model.remove_all();
        }
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
            if let Some(model) = list_store_model {
                let frames = sprite.frames_mut().iter().enumerate()
                    .map(|(i, frame)| LitFrameObject::new(frame, i as u32))
                    .collect::<Vec<_>>();
                model.extend_from_slice(&frames);
            }
        } else {
            self.enabled.set_active(false);
        }
    }
}

fn iter_bitset(bitset: &gtk::Bitset) -> impl Iterator<Item = u32> + use<'_> {
    let (mut iter, mut first) = match gtk::BitsetIter::init_first(bitset) {
        Some(s) => (Some(s.0), Some(s.1)),
        None => (None, None),
    };
    std::iter::from_fn(move || {
        if let Some(next) = first.take() {
            return Some(next);
        }
        iter.as_mut()?.next()
    })
}

async fn frames_from_clipboard() -> Option<Vec<anim_lit::Frame>> {
    let display = gdk::Display::default()?;
    let clip = display.clipboard();
    let text = clip.read_text_future().await.ok()??;
    let text = text.as_str();
    let result = text.split("\n")
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
        })
        .collect::<Option<Vec<_>>>()?;
    if result.is_empty() {
        None
    } else {
        Some(result)
    }
}

#[derive(Properties, Default)]
#[properties(wrapper_type = LitFrameObject)]
pub struct LitFrameObjectImpl {
    #[property(get, set)]
    color: Cell<u32>,
    #[property(get, set)]
    x: Cell<i32>,
    #[property(get, set)]
    y: Cell<i32>,
    #[property(get, set)]
    intensity: Cell<u32>,
    #[property(get, set)]
    radius: Cell<u32>,
    #[property(get, set)]
    index: Cell<u32>,
}

#[glib::object_subclass]
impl ObjectSubclass for LitFrameObjectImpl {
    const NAME: &'static str = "AnimosityLitFrame";
    type Type = LitFrameObject;
}

#[glib::derived_properties]
impl ObjectImpl for LitFrameObjectImpl {}

glib::wrapper! {
    pub struct LitFrameObject(ObjectSubclass<LitFrameObjectImpl>);
}

impl LitFrameObject {
    pub fn new(frame: &anim_lit::Frame, index: u32) -> Self {
        glib::Object::builder()
            .property("x", frame.x)
            .property("y", frame.y)
            .property("color", frame.color)
            .property("intensity", frame.intensity)
            .property("radius", frame.radius)
            .property("index", index)
            .build()
    }
}

fn lit_frame_from_model(
    model: &gtk::MultiSelection,
    pos: u32,
) -> Option<anim_lit::Frame> {
    let item = model.item(pos)?;
    let lit = item.downcast_ref::<LitFrameObject>()?;
    Some(anim_lit::Frame {
        x: lit.x(),
        y: lit.y(),
        color: lit.color(),
        intensity: lit.intensity(),
        radius: lit.radius(),
    })
}

fn set_lit_frame_in_model(
    model: &gtk::MultiSelection,
    pos: u32,
    frame: &anim_lit::Frame,
) -> Option<()> {
    let item = model.item(pos)?;
    let lit = item.downcast_ref::<LitFrameObject>()?;
    lit.set_x(frame.x);
    lit.set_y(frame.y);
    lit.set_color(frame.color);
    lit.set_intensity(frame.intensity);
    lit.set_radius(frame.radius);
    Some(())
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

/*
mod gtk_ext {
    use gtk::subclass::prelude::*;
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
            widget: &P,
        ) -> (i32, i32) {
            let (min, natural) = self.parent_preferred_width(widget);
            let (_, natural_h) = self.parent_preferred_height_for_width(widget, natural);
            let color_block_size = natural_h;
            (min + color_block_size, natural + color_block_size)
        }

        fn render<P: IsA<gtk::Widget>>(
            &self,
            cr: &cairo::Context,
            widget: &P,
            background_area: &gdk::Rectangle,
            cell_area: &gdk::Rectangle,
            flags: gtk::CellRendererState,
        ) {
            let obj = self.obj();
            let text = match obj.text() {
                Some(s) => s,
                None => return,
            };
            let color_block_size = cell_area.height();
            let text_width = cell_area.width() - color_block_size;
            if let Some(color) = super::rgb_color_from_string(text.as_str()) {
                let (x_padding, y_padding) = obj.padding();
                let result = cr.save()
                    .and_then(|()| {
                        cr.new_path();
                        cr.set_line_width(1.0);
                        cr.set_source_rgb(0.0, 0.0, 0.0);
                        cr.rectangle(
                            (cell_area.x() + text_width + x_padding) as f64,
                            (cell_area.y() + y_padding) as f64,
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
                            (cell_area.x() + text_width + x_padding) as f64 + 1.0,
                            (cell_area.y() + y_padding) as f64 + 1.0,
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
            let text_area = gdk::Rectangle::new(
                cell_area.x(),
                cell_area.y(),
                text_width,
                cell_area.height(),
            );
            self.parent_render(cr, widget, background_area, &text_area, flags);
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
        }
    }
}
*/
