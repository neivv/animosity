#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

#[macro_use] extern crate anyhow;
#[macro_use] extern crate glium;
#[macro_use] extern crate log;

mod anim;
mod anim_lit;
mod anim_encoder;
mod arc_error;
mod combo_box_enum;
mod ddsgrp;
mod default_grp_sizes;
mod frame_export;
mod frame_import;
mod frame_import_dialog;
mod frame_info;
mod gl;
mod int_entry;
mod files;
mod recurse_checked_mutex;
mod select_dir;
mod shaders;
mod widget_lighting;
#[allow(dead_code)] mod ui_helpers;

use std::borrow::Cow;
use std::cell::{Cell, RefCell};
use std::fs::File;
use std::path::{Path, PathBuf};
use std::sync::{Arc};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::rc::Rc;

use gio::prelude::*;
use gtk::prelude::*;

use anyhow::{Context, Error};
use cgmath::conv::array4x4;
use cgmath::{Matrix4, vec4};
use glium::backend::glutin::headless::Headless;
use glium::framebuffer::SimpleFrameBuffer;
use glium::index::{IndexBuffer, PrimitiveType};
use glium::texture::{self, Texture2d};
use glium::vertex::VertexBuffer;

use crate::files::SpriteFiles;
use crate::int_entry::{IntEntry, IntSize};
use crate::recurse_checked_mutex::Mutex;

fn init_log() -> Result<(), fern::InitError> {
    if cfg!(debug_assertions) {
        fern::Dispatch::new()
            .format(|out, message, record| {
                out.finish(format_args!(
                    "[{}][{}] {}",
                    record.target(),
                    record.level(),
                    message
                ))
            })
            .level(log::LevelFilter::Debug)
            .chain(std::io::stdout())
            .apply()?;
    }
    Ok(())
}

fn init_panic_handler() {
    std::panic::set_hook(Box::new(|info| {
        use std::fmt::Write;

        let mut msg = String::new();
        match info.location() {
            Some(s) => writeln!(msg, "Panic at {}:{}", s.file(), s.line()).unwrap(),
            None => writeln!(msg, "Panic at unknown location").unwrap()
        }
        let payload = info.payload();
        let panic_msg = match payload.downcast_ref::<&str>() {
            Some(s) => s,
            None => match payload.downcast_ref::<String>() {
                Some(s) => &s[..],
                None => "(???)",
            },
        };
        writeln!(msg, "{}", panic_msg).unwrap();
        error!("{}", msg);
        let dialog = gtk::MessageDialog::new::<gtk::Window>(
            None,
            gtk::DialogFlags::MODAL,
            gtk::MessageType::Error,
            gtk::ButtonsType::None,
            &msg,
        );
        dialog.add_button("Ok", gtk::ResponseType::Ok);
        dialog.set_title("Crash");
        dialog.run();
        dialog.close();
        std::process::exit(3);
    }));
}

fn main() {
    if !cfg!(debug_assertions) {
        init_panic_handler();
    }
    let _ = init_log();
    let name = format!("animosity.pid_{}", std::process::id());
    let app = gtk::Application::new(Some(&*name), gio::ApplicationFlags::empty())
        .unwrap_or_else(|e| panic!("Couldn't create app: {}", e));
    app.connect_startup(|app| {
        let ui = create_ui(app);
        create_actions(app, &ui.main_window.clone().upcast());
        ui.main_window.show_all();
        ui.info.lighting_expander.emit_activate();
        UI.with(|x| {
            *x.borrow_mut() = Some(Rc::new(ui));
        });
        if let Some(path) = ::std::env::args_os().nth(1) {
            open(Path::new(&path));
        }
    });
    app.connect_activate(|_| {
    });
    app.run(&[]);
}

struct State {
    files: Arc<Mutex<files::Files>>,
}

struct Ui {
    app: gtk::Application,
    main_window: gtk::ApplicationWindow,
    list: SpriteList,
    info: Arc<SpriteInfo>,
}

thread_local! {
    static UI: RefCell<Option<Rc<Ui>>> = RefCell::new(None);
    static CSS: gtk::CssProvider = init_css_provider();
    static STATE: RefCell<State> = RefCell::new(State {
        files: Arc::new(Mutex::new(files::Files::empty())),
    });
}

fn ui() -> Rc<Ui> {
    UI.with(|x| {
        x.borrow_mut().as_ref().expect("UI not initialized").clone()
    })
}

impl Ui {
    fn message(&self, msg: &str) {
        error_msg_box(&self.main_window, msg);
    }

    fn files_changed(&self, files: &files::Files) {
        self.list.list.clear();
        for sprite in files.sprites() {
            let name: Cow<'_, str> = match *sprite {
                SpriteFiles::AnimSet(ref s) => (&*s.name).into(),
                SpriteFiles::DdsGrp(_) => "(File)".into(),
                SpriteFiles::MainSdOnly { ref name, .. } => (&**name).into(),
            };
            self.list.list.push(&name);
        }
        self.list.list.columns_autosize();
        self.main_window.set_title(&title(files.root_path(), false));
    }
}

fn title(path: Option<&Path>, dirty: bool) -> String {
    if let Some(path) = path {
        if dirty {
            format!("{}* - Animosity {}", path.to_string_lossy(), env!("CARGO_PKG_VERSION"))
        } else {
            format!("{} - Animosity {}", path.to_string_lossy(), env!("CARGO_PKG_VERSION"))
        }
    } else {
        format!("Animosity {}", env!("CARGO_PKG_VERSION"))
    }
}

struct ScrolledList {
    root: gtk::ScrolledWindow,
    list: gtk::TreeView,
    store: gtk::ListStore,
}

impl ScrolledList {
    fn new() -> ScrolledList {
        let store = gtk::ListStore::new(&[glib::Type::String]);
        let list = gtk::TreeView::with_model(&store);
        let col = gtk::TreeViewColumn::new();
        let renderer = gtk::CellRendererText::new();
        col.pack_end(&renderer, true);
        col.add_attribute(&renderer, "text", 0);
        list.append_column(&col);
        list.set_headers_visible(false);

        let none: Option<&gtk::Adjustment> = None;
        let root = gtk::ScrolledWindow::new(none, none);
        root.add(&list);
        root.set_overlay_scrolling(false);
        ScrolledList {
            root,
            list,
            store,
        }
    }

    fn clear(&self) {
        self.store.clear();
    }

    fn select(&self, index: usize) {
        let path = gtk::TreePath::from_indicesv(&[index as i32]);
        let none: Option<&gtk::TreeViewColumn> = None;
        self.list.set_cursor(&path, none, false);
    }

    fn columns_autosize(&self) {
        self.list.columns_autosize();
    }

    fn push(&self, value: &str) {
        let iter = self.store.append();
        self.store.set_value(&iter, 0, &value.to_value());
    }
}

struct SpriteList {
    list: ScrolledList,
}

impl SpriteList {
    fn new(linked_info: Arc<SpriteInfo>) -> SpriteList {
        let list = ScrolledList::new();
        list.root.set_min_content_width(80);

        let info = linked_info.clone();
        list.list.connect_cursor_changed(move |s| {
            let sprite = s.get_selection().get_selected()
                .and_then(|(model, iter)| model.get_path(&iter))
                .and_then(|path| path.get_indices().get(0).cloned());
            if let Some(index) = sprite {
                info.select_sprite(index as usize);
            }
        });
        SpriteList {
            list,
        }
    }

    fn widget(&self) -> gtk::Widget {
        self.list.root.clone().upcast()
    }
}

/// The various integers etc that are associated with a sprite.
struct SpriteValues {
    bx: gtk::Box,
    ref_enable: gtk::CheckButton,
    ref_index: Arc<IntEntry>,
    unk2: Arc<IntEntry>,
    width: Arc<IntEntry>,
    height: Arc<IntEntry>,
    texture_dimensions: gtk::Label,
    frame_count_label: gtk::Label,
}

impl SpriteValues {
    fn new() -> SpriteValues {
        let bx = gtk::Box::new(gtk::Orientation::Vertical, 0);
        let ref_enable = gtk::CheckButton::with_label("References image");
        ref_enable.set_sensitive(false);
        let ref_index = IntEntry::new(IntSize::Int16);
        ref_index.frame.set_sensitive(false);
        let texture_dimensions = gtk::Label::new(Some("Texture size: 0x0"));
        texture_dimensions.set_width_chars(20);
        let frame_count_label = gtk::Label::new(Some("0 frames"));
        let unk2_label = gtk::Label::new(Some("Unknown2"));
        let unk2 = IntEntry::new(IntSize::Int16);
        let unk3_label = gtk::Label::new(Some("Dimensions"));
        let unk3_bx = gtk::Box::new(gtk::Orientation::Horizontal, 0);
        let width = IntEntry::new(IntSize::Int16);
        let height = IntEntry::new(IntSize::Int16);
        bx.set_sensitive(false);
        bx.pack_start(&ref_enable, false, false, 0);
        bx.pack_start(&ref_index.widget(), false, false, 0);
        bx.pack_start(&texture_dimensions, false, false, 0);
        bx.pack_start(&frame_count_label, false, false, 0);
        bx.pack_start(&unk2_label, false, false, 0);
        bx.pack_start(&unk2.widget(), false, false, 0);
        bx.pack_start(&unk3_label, false, false, 0);
        unk3_bx.pack_start(&width.widget(), true, true, 0);
        unk3_bx.pack_start(&height.widget(), true, true, 0);
        bx.pack_start(&unk3_bx, false, false, 0);
        SpriteValues {
            bx,
            ref_index,
            ref_enable,
            unk2,
            width,
            height,
            texture_dimensions,
            frame_count_label,
        }
    }

    fn connect_actions(&self, sprite_actions: &gio::SimpleActionGroup) {
        let disable_check = Rc::new(Cell::new(false));
        if let Some(a) = lookup_action(sprite_actions, "enable_ref") {
            let check = self.ref_enable.clone();
            let i = self.ref_index.clone();
            let d = disable_check.clone();
            a.connect_property_enabled_notify(move |s| {
                let enabled = s.get_enabled();
                check.set_sensitive(enabled);
                i.frame.set_sensitive(enabled);
                if !enabled {
                    d.set(true);
                    check.set_active(false);
                    d.set(false);
                    i.clear();
                }
            });
            let i = self.ref_index.clone();
            let check = self.ref_enable.clone();
            let u2 = self.unk2.clone();
            let u3a = self.width.clone();
            let u3b = self.height.clone();
            let disable_check = disable_check.clone();
            a.connect_activate(move |_, param| {
                if let Some(enabled) = param.as_ref().and_then(|x| x.get::<bool>()) {
                    if check.get_active() != enabled {
                        disable_check.set(true);
                        check.set_active(enabled);
                        disable_check.set(false);
                    }
                    i.frame.set_sensitive(enabled);
                    if !enabled {
                        i.clear();
                    }
                    u2.frame.set_sensitive(!enabled);
                    u3a.frame.set_sensitive(!enabled);
                    u3b.frame.set_sensitive(!enabled);
                }
            });
        }
        if let Some(a) = lookup_action(sprite_actions, "edit_enable_ref") {
            self.ref_enable.connect_toggled(move |s| {
                if disable_check.get() == false {
                    let enabled: bool = s.get_active();
                    let variant = enabled.to_variant();
                    a.activate(Some(&variant));
                }
            });
        }
        IntEntry::connect_actions(
            &self.ref_index,
            sprite_actions,
            "init_ref_img",
            "edit_ref_img",
        );
        IntEntry::connect_actions(&self.unk2, sprite_actions, "init_unk2", "edit_unk2");
        IntEntry::connect_actions(&self.width, sprite_actions, "init_unk3a", "edit_unk3a");
        IntEntry::connect_actions(&self.height, sprite_actions, "init_unk3b", "edit_unk3b");
        let i = self.ref_index.clone();
        let u2 = self.unk2.clone();
        let u3a = self.width.clone();
        let u3b = self.height.clone();
        let bx = self.bx.clone();
        if let Some(a) = lookup_action(sprite_actions, "sprite_exists") {
            a.connect_activate(move |_, param| {
                if let Some(exists) = param.as_ref().and_then(|x| x.get::<bool>()) {
                    bx.set_sensitive(exists);
                    if !exists {
                        u2.clear();
                        u3a.clear();
                        u3b.clear();
                        i.clear();
                    }
                }
            });
        }
        let l = self.texture_dimensions.clone();
        if let Some(a) = lookup_action(sprite_actions, "texture_size") {
            a.connect_activate(move |_, param| {
                if let Some(text) = param.as_ref().and_then(|x| x.get_str()) {
                    l.set_text(&format!("Texture size: {}", text));
                }
            });
        }
        let l = self.frame_count_label.clone();
        if let Some(a) = lookup_action(sprite_actions, "frame_count") {
            a.connect_activate(move |_, param| {
                if let Some(val) = param.as_ref().and_then(|x| x.get::<u32>()) {
                    if val == 1 {
                        l.set_text(&"1 frame");
                    } else {
                        l.set_text(&format!("{} frames", val));
                    }
                }
            });
        }
    }

    fn widget(&self) -> gtk::Widget {
        self.bx.clone().upcast()
    }
}

struct SpriteSelector {
    bx: gtk::Box,
    list: ScrolledList,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub enum SpriteType {
    Sd,
    Hd,
    Hd2,
}

impl SpriteSelector {
    fn new(sprite_actions: gio::ActionGroup) -> SpriteSelector {
        let bx = gtk::Box::new(gtk::Orientation::Vertical, 0);
        let sd = gtk::RadioButton::with_label("SD");
        let hd = gtk::RadioButton::with_label_from_widget(&sd, "HD");
        let hd2 = gtk::RadioButton::with_label_from_widget(&sd, "HD2");
        let list = ScrolledList::new();
        list.root.set_min_content_height(200);
        list.root.set_min_content_width(80);
        list.list.connect_cursor_changed(move |s| {
            let index = s.get_selection().get_selected()
                .and_then(|(model, iter)| model.get_path(&iter))
                .and_then(|path| path.get_indices().get(0).cloned());
            if let Some(index) = index {
                let variant = (index as u32).to_variant();
                sprite_actions.activate_action("select_layer", Some(&variant));
            }
        });
        sd.set_action_name(Some("sprite.select_sd"));
        hd.set_action_name(Some("sprite.select_hd"));
        hd2.set_action_name(Some("sprite.select_hd2"));
        bx.pack_start(&sd, false, false, 0);
        bx.pack_start(&hd, false, false, 0);
        bx.pack_start(&hd2, false, false, 0);
        bx.pack_start(&list.root, false, false, 0);
        SpriteSelector {
            bx,
            list,
        }
    }

    fn widget(&self) -> gtk::Widget {
        self.bx.clone().upcast()
    }
}

struct DrawParams {
    vertices: VertexBuffer<gl::Vertex>,
    indices: IndexBuffer<u32>,
    lines: DrawLines,
    program: glium::program::Program,
    cached_textures: Vec<(Texture2d, TextureId)>,
}

/// sprite_id, type, layer
#[derive(Eq, Copy, Clone, PartialEq, Debug)]
struct TextureId(usize, SpriteType, usize);

struct DrawLines {
    pub texture_lines: TextureLines,
    program: glium::program::Program,
}

struct TextureLines(Vec<(TextureId, LineBuffer)>);

impl TextureLines {
    fn buffer_for_texture<F: FnOnce() -> Vec<(Rect, Color, u8)>>(
        &mut self,
        facade: &Headless,
        tex_id: &TextureId,
        init: F,
    ) -> &mut LineBuffer {
        match self.0.iter().position(|x| x.0 == *tex_id) {
            Some(s) => &mut self.0[s].1,
            None => {
                let rects = init();
                let mut vertices = Vec::with_capacity(rects.len() * 4);
                for &(ref rect, color, ty) in rects.iter() {
                    let color = [color.0, color.1, color.2, color.3];
                    let left = rect.x as f32;
                    let top = rect.y as f32;
                    let right = left + rect.width as f32;
                    let bottom = top + rect.height as f32;
                    vertices.extend([
                        gl::LineVertex {
                            pos: [left, top],
                            color,
                            ty,
                        },
                        gl::LineVertex {
                            pos: [right, top],
                            color,
                            ty,
                        },
                        gl::LineVertex {
                            pos: [left, bottom],
                            color,
                            ty,
                        },
                        gl::LineVertex {
                            pos: [right, bottom],
                            color,
                            ty,
                        },
                    ].iter().cloned());
                }
                let mut indices = Vec::with_capacity(rects.len() * 8);
                for i in 0..rects.len() {
                    let i = i as u32 * 4;
                    indices.extend(
                        [i, i + 1, i + 1, i + 3, i + 3, i + 2, i + 2, i].iter().cloned()
                    );
                }
                let vertices = VertexBuffer::new(facade, &vertices)
                    .expect("Couldn't create vertex buffer");
                let indices = IndexBuffer::new(facade, PrimitiveType::LinesList, &indices)
                    .expect("Couldn't create vertex buffer");

                // Hacky, clear cache when sprite id changes, so the sprite can be reloaded
                // by clicking away and back.
                let clear = self.0.first().map(|x| (x.0).0 != tex_id.0).unwrap_or(false);
                if clear {
                    self.0.clear();
                }

                self.0.push((tex_id.clone(), LineBuffer {
                    vertices,
                    indices,
                }));
                let pos = self.0.len() - 1;
                &mut self.0[pos].1
            }
        }
    }
}

struct LineBuffer {
    vertices: VertexBuffer<gl::LineVertex>,
    indices: IndexBuffer<u32>,
}

impl DrawLines {
    fn new(gl: &mut gl::Context) -> DrawLines {
        let program = glium::program::Program::from_source(
            gl.facade(),
            shaders::LINE_VERTEX,
            shaders::LINE_FRAGMENT,
            None,
        ).expect("GL line program creation failed");
        DrawLines {
            texture_lines: TextureLines(Vec::new()),
            program,
        }
    }
}

#[derive(Copy, Clone)]
struct Color(f32, f32, f32, f32);

#[derive(Copy, Clone, Eq, PartialEq)]
struct Rect {
    x: u32,
    y: u32,
    width: u32,
    height: u32,
}

impl Rect {
    fn new(x: u32, y: u32, width: u32, height: u32) -> Rect {
        Rect {
            x,
            y,
            width,
            height,
        }
    }
}

fn sprite_render_program(gl: &mut gl::Context) -> glium::program::Program {
    glium::program::Program::from_source(
        gl.facade(),
        shaders::SPRITE_VERTEX,
        shaders::SPRITE_FRAGMENT,
        None,
    ).expect("GL sprite program creation failed")
}

pub struct SpriteInfo {
    bx: gtk::Box,
    file_list: gtk::TextBuffer,
    files: Arc<Mutex<files::Files>>,
    sprite_actions: gio::SimpleActionGroup,
    sprite_index: AtomicUsize,
    selected_layer: AtomicUsize,
    selector: SpriteSelector,
    selected_type: Cell<SpriteType>,
    draw_area: gtk::DrawingArea,
    draw_clear_requests: RefCell<Vec<TextureId>>,
    lighting: Arc<widget_lighting::SpriteLighting>,
    lighting_expander: gtk::Expander,
}

fn lookup_action<G: IsA<gio::ActionMap>>(group: &G, name: &str) -> Option<gio::SimpleAction> {
    group.lookup_action(name).and_then(|x| x.downcast::<gio::SimpleAction>().ok())
}

impl SpriteInfo {
    fn new(file_shared: &Arc<Mutex<files::Files>>) -> Arc<SpriteInfo> {
        let bx = gtk::Box::new(gtk::Orientation::Vertical, 0);
        let bx1 = gtk::Box::new(gtk::Orientation::Horizontal, 0);
        let sprite_actions = gio::SimpleActionGroup::new();
        bx.insert_action_group("sprite", Some(&sprite_actions));
        let sprite_bx = gtk::Box::new(gtk::Orientation::Horizontal, 0);
        let data_bx = gtk::Box::new(gtk::Orientation::Vertical, 0);
        let selector = SpriteSelector::new(sprite_actions.clone().upcast());
        let values = SpriteValues::new();
        let draw_area = gtk::DrawingArea::new();
        data_bx.pack_start(&selector.widget(), false, false, 0);
        data_bx.pack_start(&values.widget(), false, false, 0);
        sprite_bx.pack_start(&data_bx, false, false, 0);
        sprite_bx.pack_start(&draw_area, true, true, 0);
        let files = gtk::TextView::new();
        let none: Option<&gtk::TextTagTable> = None;
        let file_list = gtk::TextBuffer::new(none);
        file_list.set_text("\n\n\n");
        files.set_buffer(Some(&file_list));
        files.set_editable(false);
        files.set_wrap_mode(gtk::WrapMode::None);
        let lighting = widget_lighting::SpriteLighting::new(file_shared, sprite_actions.clone());
        let expander = gtk::Expander::new(None);
        // There is expander.set_resize_toplevel, but it assumes the expander is being used
        // vertically ;_;
        let light2 = lighting.clone();
        let expander_toggled = Rc::new(move |expander: &gtk::Expander, grow| {
            use gdk::WindowExt;
            let (_, size) = light2.widget().get_preferred_size();
            let (_, expander_width) = expander.get_preferred_width();
            if let Some(window) = expander.get_window() {
                let width = window.get_width();
                let height = window.get_height();
                let new_w = if grow {
                    width + (size.width - expander_width).max(0)
                } else {
                    width - (size.width - expander_width).max(0)
                };
                window.resize(new_w, height);
            }
        });
        let expander_toggled2 = expander_toggled.clone();
        expander.connect_activate(move |expander| {
            // This is ran before the state changes from activation
            if expander.get_expanded() {
                return;
            }
            expander_toggled2(expander, true);
        });
        expander.connect_property_expanded_notify(move |expander| {
            // This is ran after state change
            if expander.get_expanded() {
                return;
            }
            expander_toggled(expander, false);
        });
        expander.add(&lighting.widget());

        bx.pack_start(&sprite_bx, true, true, 0);
        bx.pack_start(&files, false, false, 0);
        bx1.pack_start(&bx, true, true, 0);
        bx1.pack_start(&expander, false, false, 0);
        let result = Arc::new(SpriteInfo {
            bx: bx1,
            file_list,
            files: file_shared.clone(),
            sprite_actions,
            sprite_index: AtomicUsize::new(0),
            selected_layer: AtomicUsize::new(0),
            selector,
            selected_type: Cell::new(SpriteType::Sd),
            draw_area: draw_area.clone(),
            draw_clear_requests: RefCell::new(Vec::new()),
            lighting,
            lighting_expander: expander,
        });
        SpriteInfo::create_sprite_actions(&result, &result.sprite_actions.clone().upcast());
        values.connect_actions(&result.sprite_actions);

        let this = result.clone();
        let gl: Rc<RefCell<Option<(gl::Context, DrawParams)>>> = Rc::new(RefCell::new(None));
        draw_area.connect_draw(move |s, cairo| {
            let mut gl = gl.borrow_mut();
            let rect = s.get_allocation();
            let &mut (ref mut gl, ref mut draw_params) = gl.get_or_insert_with(|| {
                let mut gl = gl::Context::new(rect.width as u32, rect.height as u32);
                let vertices = gl.set_vertices(&[
                    gl::Vertex { pos: [-1.0, 1.0], tex: [0.0, 1.0] },
                    gl::Vertex { pos: [1.0, 1.0], tex: [1.0, 1.0] },
                    gl::Vertex { pos: [-1.0, -1.0], tex: [0.0, 0.0] },
                    gl::Vertex { pos: [1.0, -1.0], tex: [1.0, 0.0] },
                ]);
                let indices = glium::index::IndexBuffer::new(
                    gl.facade(),
                    PrimitiveType::TrianglesList,
                    &[0, 1, 2, 1, 3, 2],
                ).expect("Unable to create index buffer");
                let program = sprite_render_program(&mut gl);
                let lines = DrawLines::new(&mut gl);
                (gl, DrawParams {
                    vertices,
                    indices,
                    program,
                    cached_textures: Vec::new(),
                    lines,
                })
            });
            {
                let mut clear_reqs = this.draw_clear_requests.borrow_mut();
                for tex_id in clear_reqs.drain(..) {
                    if tex_id.0 == !0 {
                        // Hack for clear all
                        draw_params.cached_textures.clear();
                        draw_params.lines.texture_lines.0.clear();
                    } else {
                        draw_params.cached_textures.retain(|x| x.1 != tex_id);
                        draw_params.lines.texture_lines.0.retain(|x| x.0 != tex_id);
                    }
                }
            }
            gl.resize_buf(rect.width as u32, rect.height as u32);
            let result = {
                let size = gl.buf_dimensions();
                let (mut buf, facade) = gl.framebuf();
                this.render_sprite(
                    &mut buf,
                    facade,
                    draw_params,
                    size,
                    gl.stride(),
                )
            };
            match result {
                Ok(()) => {
                    let (data, width, height) = gl.framebuf_bytes();
                    let surface = cairo::ImageSurface::create_for_data(
                        data.into_boxed_slice(),
                        cairo::Format::ARgb32,
                        width as i32,
                        height as i32,
                        width as i32 * 4,
                    ).expect("Couldn't create cairo image surface");
                    // Could recycle the surface?
                    cairo.set_source_surface(&surface, 0.0, 0.0);
                    cairo.paint();
                }
                Err(e) => {
                    cairo.set_source_rgb(0.0, 0.0, 0.0);
                    cairo.set_font_size(15.0);
                    cairo.move_to(0.0, 20.0);
                    cairo.show_text(&e.to_string());
                }
            }
            Inhibit(true)
        });

        result
    }

    fn draw_clear_all(&self) {
        self.draw_clear_requests.borrow_mut().push(TextureId(!0, SpriteType::Sd, !0));
    }

    fn frame_export_dialog(this: &Arc<SpriteInfo>, parent: &gtk::ApplicationWindow) {
        use crate::ui_helpers::*;

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
        let mut checkboxes = Vec::with_capacity(layer_names.len());
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
            for (i, name) in layer_names.iter().enumerate() {
                let row = i as i32 + 1;
                let tex_size = file.texture_size(i);

                let checkbox = gtk::CheckButton::new();
                grid.attach(&checkbox, 0, row, 1, 1);
                let label = gtk::Label::new(Some(&**name));
                grid.attach(&label, 1, row, 1, 1);
                label.set_halign(gtk::Align::Start);

                let (entry, frame) = int_entry::entry();
                frame.set_hexpand(true);

                if tex_size.is_none() {
                    checkbox.set_sensitive(false);
                    label.set_sensitive(false);
                    entry.set_sensitive(false);
                    checkbox.set_active(false);
                } else {
                    checkbox.set_active(true);
                    entry.set_text(&format!("{}_{}", prefix_prefix, name));
                }
                let e = entry.clone();
                checkbox.connect_toggled(move |s| {
                    e.set_sensitive(s.get_active());
                });

                grid.attach(&frame, 2, row, 1, 1);
                checkboxes.push((checkbox, entry));
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
                let layers_to_export = checkboxes.iter().map(|(check, entry)| {
                    if check.get_active() {
                        Some(entry.get_text().into())
                    } else {
                        None
                    }
                }).collect::<Vec<_>>();
                frame_count = layers_to_export.iter().filter(|x| x.is_some()).count() *
                    file.frames().map(|x| x.len()).unwrap_or(0);
                let single_image = single_image_check2.get_active();
                std::thread::spawn(move || {
                    let mut files = files_arc.lock();
                    let file = match files.file(tex_id.0, tex_id.1) {
                        Ok(Some(o)) => o,
                        _ => return,
                    };

                    let (width, height) = dimensions;
                    let result = frame_export::export_frames(
                        &file,
                        tex_id.1,
                        i32::from(width),
                        i32::from(height),
                        &path2,
                        &framedef,
                        &layers_to_export,
                        single_image,
                        |step| send.send(Progress::Progress(step)).unwrap(),
                    );
                    let _ = send.send(Progress::Done(result));
                });
            } else {
                let prefix = grp_prefix.as_ref()
                    .map(|x| x.get_text().into())
                    .unwrap_or_else(String::new);
                frame_count = file.layer_count();
                let single_image = single_image_check2.get_active();
                std::thread::spawn(move || {
                    let mut files = files_arc.lock();
                    let file = match files.file(tex_id.0, tex_id.1) {
                        Ok(Some(o)) => o,
                        _ => return,
                    };

                    let result = frame_export::export_grp(
                        &file,
                        &path2,
                        &prefix,
                        &framedef,
                        single_image,
                        |step| send.send(Progress::Progress(step)).unwrap(),
                    );
                    let _ = send.send(Progress::Done(result));
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
        let mut input_parts: Vec<&dyn ui_helpers::BoxableWidget>  = vec![
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

    fn on_dirty_update<F: Fn(bool) + 'static>(&self, fun: F) {
        if let Some(a) = lookup_action(&self.sprite_actions, "is_dirty") {
            a.connect_activate(move |_, param| {
                if let Some(val) = param.as_ref().and_then(|x| x.get::<bool>()) {
                    fun(val);
                }
            });
        }
    }

    fn tex_id(&self) -> TextureId {
        let index = self.sprite_index.load(Ordering::SeqCst);
        let selected_type = self.selected_type.get();
        let layer = self.selected_layer.load(Ordering::SeqCst);
        TextureId(index, selected_type, layer)
    }

    fn sprite_texture<'a>(
        &self,
        facade: &Headless,
        cached_textures: &'a mut Vec<(Texture2d, TextureId)>,
        cache_file: &mut files::File<'_>,
    ) -> Result<Option<&'a Texture2d>, Error> {
        let tex_id = self.tex_id();
        let cached = cached_textures.iter().position(|x| x.1 == tex_id);
        if let Some(index) = cached {
            Ok(Some(&cached_textures[index].0))
        } else {
            let image = cache_file.texture(tex_id.2)?;
            let image = glium::texture::RawImage2d::from_raw_rgba(
                image.data,
                (image.width, image.height),
            );
            let texture = Texture2d::with_format(
                facade,
                image,
                texture::UncompressedFloatFormat::U8U8U8U8,
                texture::MipmapsOption::AutoGeneratedMipmaps,
            )?;
            // Hacky, clear cache when sprite id changes, so the sprite can be reloaded
            // by clicking away and back.
            let clear = cached_textures.first().map(|x| (x.1).0 != tex_id.0).unwrap_or(false);
            if clear {
                cached_textures.clear();
            }
            cached_textures.push((texture, tex_id));
            Ok(Some(&cached_textures.last().unwrap().0))
        }
    }

    fn render_sprite(
        &self,
        buf: &mut SimpleFrameBuffer<'_>,
        facade: &Headless,
        draw_params: &mut DrawParams,
        (buf_width, buf_height): (u32, u32),
        buf_stride: u32,
    ) -> Result<(), Error> {
        use glium::Surface;

        buf.clear_color(0.0, 0.0, 0.0, 1.0);
        let tex_id = self.tex_id();
        let mut files = match self.files.try_lock() {
            Ok(o) => o,
            Err(_) => return Ok(()),
        };
        let mut file = match files.file(tex_id.0, tex_id.1)? {
            Some(s) => s,
            None => return Ok(()),
        };

        let texture = self.sprite_texture(facade, &mut draw_params.cached_textures, &mut file)?;
        if let Some(texture) = texture {
            let glium_params = glium::draw_parameters::DrawParameters {
                blend: glium::Blend::alpha_blending(),
                ..Default::default()
            };
            let sampler = glium::uniforms::Sampler::new(texture)
                .magnify_filter(glium::uniforms::MagnifySamplerFilter::Nearest)
                .minify_filter(glium::uniforms::MinifySamplerFilter::Linear);

            // scale to view, scale + transform view to
            let tex_width = texture.width() as f32;
            let tex_height = texture.height() as f32;
            let mut render_width = tex_width.min(buf_width as f32);
            let mut render_height = tex_height.min(buf_height as f32);
            // Keep aspect ratio
            if render_width / tex_width < render_height / tex_height {
                render_height = (render_width / tex_width) * tex_height;
            } else {
                render_width = (render_height / tex_height) * tex_width;
            }
            // (render_width / buf_width) * (buf_width / buf_stride)
            let scale_x = render_width / buf_stride as f32;
            let scale_y = render_height / buf_height as f32;
            let shift_x = -1.0 + buf_width as f32 / buf_stride as f32;
            let shift_y = 0.0;
            let tex_to_window = Matrix4::from_cols(
                vec4(scale_x,   0.0,        0.0,    0.0),
                vec4(0.0,       scale_y,    0.0,    0.0),
                vec4(0.0,       0.0,        1.0,    0.0),
                vec4(shift_x,   shift_y,    0.0,    1.0),
            );
            let uniforms = uniform! {
                transform: array4x4(tex_to_window),
                tex: sampler,
            };
            buf.draw(
                &draw_params.vertices,
                &draw_params.indices,
                &draw_params.program,
                &uniforms,
                &glium_params,
            )?;
            let lines = draw_params.lines.texture_lines.buffer_for_texture(facade, &tex_id, || {
                let div = match tex_id.1 {
                    // Hd2 has Hd coordinates?? BW seems to divide them too
                    SpriteType::Hd2 => 2,
                    _ => 1,
                };
                let mut result = Vec::with_capacity(32);
                let red = Color(1.0, 0.0, 0.0, 1.0);
                let green = Color(0.0, 1.0, 0.0, 1.0);
                result.push((Rect::new(0, 0, texture.width(), texture.height()), red, 0));
                if let Some(frames) = file.frames() {
                    for f in frames {
                        let rect = Rect::new(
                            f.tex_x as u32 / div,
                            f.tex_y as u32 / div,
                            f.width as u32 / div,
                            f.height as u32 / div,
                        );
                        result.push((rect, green, 1));
                    }
                }

                result
            });
            let pixel_to_tex = Matrix4::from_cols(
                vec4(2.0 / tex_width,   0.0,                0.0,    0.0),
                vec4(0.0,               2.0 / tex_height,   0.0,    0.0),
                vec4(0.0,               0.0,                1.0,    0.0),
                vec4(-1.0,              -1.0,               0.0,    1.0),
            );
            let uniforms = uniform! {
                //transform: array4x4(pixel_to_tex * tex_to_window),
                transform: array4x4(tex_to_window * pixel_to_tex),
            };
            buf.draw(
                &lines.vertices,
                &lines.indices,
                &draw_params.lines.program,
                &uniforms,
                &glium_params,
            )?;
        }
        Ok(())
    }

    fn create_sprite_actions(this: &Arc<SpriteInfo>, group: &gio::ActionMap) {
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
        let s = this.clone();
        action(group, "select_sd", false, None, move |_, _| {
            s.selected_type.set(SpriteType::Sd);
            s.changed_type_from_event();
            s.draw_area.queue_draw();
        });
        let s = this.clone();
        action(group, "select_hd", false, None, move |_, _| {
            s.selected_type.set(SpriteType::Hd);
            s.changed_type_from_event();
            s.draw_area.queue_draw();
        });
        let s = this.clone();
        action(group, "select_hd2", false, None, move |_, _| {
            s.selected_type.set(SpriteType::Hd2);
            s.changed_type_from_event();
            s.draw_area.queue_draw();
        });
        let s = this.clone();
        action(group, "select_layer", true, Some("u"), move |_, param| {
            if let Some(layer) = param.and_then(|x| x.get::<u32>()) {
                s.selected_layer.store(layer as usize, Ordering::SeqCst);
                {
                    let tex_id = s.tex_id();
                    let mut files = match s.files.try_lock() {
                        Ok(o) => o,
                        _ => return,
                    };
                    let file = files.file(tex_id.0, tex_id.1).unwrap_or_else(|e| {
                        error!("Couldn't open {:?}: {}", tex_id, e);
                        None
                    });
                    if let Some(mut file) = file {
                        s.update_tex_size(&mut file);
                    }
                }
                s.draw_area.queue_draw();
            }
        });
        let s = this.clone();
        action(group, "edit_enable_ref", true, Some("b"), move |_, param| {
            if let Some(value) = param.and_then(|x| x.get::<bool>()) {
                s.set_ref_enabled(value);
            }
        });
        action(group, "enable_ref", false, Some("b"), move |_, _| {
        });
        action(group, "init_ref_img", true, Some("u"), move |_, _| {
        });
        let s = this.clone();
        action(group, "edit_ref_img", true, Some("u"), move |_, param| {
            if let Some(value) = param.and_then(|x| x.get::<u32>()) {
                s.set_ref_img(value);
            }
        });
        action(group, "init_unk2", true, Some("u"), move |_, _| {
        });
        action(group, "init_unk3a", true, Some("u"), move |_, _| {
        });
        action(group, "init_unk3b", true, Some("u"), move |_, _| {
        });
        let s = this.clone();
        action(group, "edit_unk2", true, Some("u"), move |_, param| {
            if let Some(value) = param.and_then(|x| x.get::<u32>()) {
                s.update_active_file(|x, _| {
                    x.unk2 = value as u16;
                });
            }
        });
        let s = this.clone();
        action(group, "edit_unk3a", true, Some("u"), move |_, param| {
            if let Some(value) = param.and_then(|x| x.get::<u32>()) {
                s.update_active_file(|x, _| {
                    x.width = value as u16;
                });
            }
        });
        let s = this.clone();
        action(group, "edit_unk3b", true, Some("u"), move |_, param| {
            if let Some(value) = param.and_then(|x| x.get::<u32>()) {
                s.update_active_file(|x, _| {
                    x.height = value as u16;
                });
            }
        });
        action(group, "sprite_exists", true, Some("b"), move |_, _| {
        });
        action(group, "texture_size", true, Some("s"), move |_, _| {
        });
        action(group, "frame_count", true, Some("u"), move |_, _| {
        });
        action(group, "is_dirty", true, Some("b"), move |_, _| {
        });
    }

    fn set_ref_enabled(&self, enabled: bool) {
        let dirty;
        {
            let tex_id = self.tex_id();
            if tex_id.1 != SpriteType::Sd {
                warn!("Changing ref for non-sd sprite");
                return;
            }
            let mut files = match self.files.try_lock() {
                Ok(o) => o,
                _ => return,
            };
            files.set_ref_enabled(tex_id.0, tex_id.1, enabled);
            dirty = files.has_changes();
            self.draw_clear_requests.borrow_mut().push(tex_id);
            // To re-read the other fields
            let mut file = files.file(tex_id.0, tex_id.1).unwrap_or_else(|e| {
                error!("Couldn't open {:?}: {}", tex_id, e);
                None
            });
            self.changed_ty(tex_id, &mut file);
        }
        if let Some(a) = lookup_action(&self.sprite_actions, "is_dirty") {
            a.activate(Some(&dirty.to_variant()));
        }
    }

    fn set_ref_img(&self, image: u32) {
        let dirty;
        {
            let tex_id = self.tex_id();
            if tex_id.1 != SpriteType::Sd {
                warn!("Changing ref for non-sd sprite");
                return;
            }
            let mut files = match self.files.try_lock() {
                Ok(o) => o,
                _ => return,
            };
            files.set_ref_img(tex_id.0, tex_id.1, image);
            dirty = files.has_changes();
            self.draw_clear_requests.borrow_mut().push(tex_id);
            let mut file = files.file(tex_id.0, tex_id.1).unwrap_or_else(|e| {
                error!("Couldn't open {:?}: {}", tex_id, e);
                None
            });
            self.changed_ty(tex_id, &mut file);
        }
        if let Some(a) = lookup_action(&self.sprite_actions, "is_dirty") {
            a.activate(Some(&dirty.to_variant()));
        }
    }

    /// Should be only called from global event handling context.
    /// The usize is layer id
    fn update_active_file<F: FnOnce(&mut anim::SpriteValues, usize)>(&self, fun: F) {
        let dirty;
        {
            let tex_id = self.tex_id();
            let mut files = match self.files.try_lock() {
                Ok(o) => o,
                _ => return,
            };
            files.update_file(tex_id.0, tex_id.1, |f| fun(f, tex_id.2));
            dirty = files.has_changes();
        }
        if let Some(a) = lookup_action(&self.sprite_actions, "is_dirty") {
            a.activate(Some(&dirty.to_variant()));
        }
    }

    fn changed_type_from_event(&self) {
        let tex_id = self.tex_id();
        let mut files = match self.files.try_lock() {
            Ok(o) => o,
            _ => return,
        };
        let mut file = files.file(tex_id.0, tex_id.1).unwrap_or_else(|e| {
            error!("Couldn't open {:?}: {}", tex_id, e);
            None
        });
        self.changed_ty(tex_id, &mut file);
    }

    fn update_tex_size(&self, file: &mut files::File<'_>) {
        let tex_id = self.tex_id();
        let variant = {
            let tex_sizes = file.texture_size(tex_id.2);
            if let Some(t) = tex_sizes {
                format!("{}x{}", t.width, t.height).to_variant()
            } else {
                "0x0".to_variant()
            }
        };
        self.sprite_actions.activate_action("texture_size", Some(&variant));
    }

    fn changed_ty(&self, tex_id: TextureId, file: &mut Option<files::File<'_>>) {
        let ty = tex_id.1;
        self.set_layers(file);
        if let Some(ref mut file) = *file {
            let is_anim = file.is_anim();
            // sprite_exists is a bit poorly chosen name
            self.sprite_actions.activate_action("sprite_exists", Some(&is_anim.to_variant()));
            let sprite_data = file.sprite_values();
            let sprite_data = sprite_data.as_ref();
            if let Some(a) = lookup_action(&self.sprite_actions, "enable_ref") {
                if ty == SpriteType::Sd && is_anim {
                    a.set_enabled(true);
                    if let Some(img) = file.image_ref() {
                        a.activate(Some(&true.to_variant()));
                        if let Some(a) = lookup_action(&self.sprite_actions, "init_ref_img") {
                            a.activate(Some(&img.to_variant()));
                        }
                    } else {
                        a.activate(Some(&false.to_variant()));
                    }
                } else {
                    a.activate(Some(&false.to_variant()));
                    a.set_enabled(false);
                }
            }
            self.update_tex_size(file);
            if let Some(data) = sprite_data {
                let variant = (data.unk2 as u32).to_variant();
                self.sprite_actions.activate_action("init_unk2", Some(&variant));
                let variant = (data.width as u32).to_variant();
                self.sprite_actions.activate_action("init_unk3a", Some(&variant));
                let variant = (data.height as u32).to_variant();
                self.sprite_actions.activate_action("init_unk3b", Some(&variant));
            }
            let frame_count = if is_anim {
                file.frames().map(|x| x.len() as u32).unwrap_or(0)
            } else {
                file.layer_count() as u32
            };
            let variant = frame_count.to_variant();
            self.sprite_actions.activate_action("frame_count", Some(&variant));
        } else {
            let variant = false.to_variant();
            self.sprite_actions.activate_action("sprite_exists", Some(&variant));
            if let Some(a) = lookup_action(&self.sprite_actions, "enable_ref") {
                a.activate(Some(&false.to_variant()));
                a.set_enabled(false);
            }
            let variant = "0x0".to_variant();
            self.sprite_actions.activate_action("texture_size", Some(&variant));
            let variant = 0u32.to_variant();
            self.sprite_actions.activate_action("frame_count", Some(&variant));
        }
    }

    fn set_layers(&self, file: &Option<files::File<'_>>) {
        let old_layer = self.selected_layer.load(Ordering::SeqCst);
        self.selector.list.clear();
        let layer_count;
        match *file {
            Some(ref file) => {
                let names = file.layer_names();
                for name in names.iter() {
                    self.selector.list.push(name);
                }
                layer_count = names.len();
            }
            None => {
                layer_count = 0;
            }
        }
        self.selector.list.columns_autosize();
        let new_layer = if old_layer >= layer_count {
            0
        } else {
            old_layer
        };
        self.selected_layer.store(new_layer, Ordering::SeqCst);
        self.selector.list.select(new_layer);
    }

    fn widget(&self) -> gtk::Widget {
        self.bx.clone().upcast()
    }

    fn set_enable_animset_actions(&self, enable: bool) {
        if let Some(a) = lookup_action(&self.sprite_actions, "select_hd") {
            a.set_enabled(enable);
        }
        if let Some(a) = lookup_action(&self.sprite_actions, "select_hd2") {
            a.set_enabled(enable);
        }
        if let Some(a) = lookup_action(&self.sprite_actions, "select_sd") {
            a.set_enabled(enable);
        }
    }

    fn select_sprite(&self, index: usize) {
        let has_mainsd;
        let sprite = {
            let mut files = match self.files.try_lock() {
                Ok(o) => o,
                _ => return,
            };
            files.close_opened();
            has_mainsd = files.mainsd().is_some();
            files.sprites().get(index).cloned()
        };
        let sprite = match sprite {
            Some(s) => s,
            None => {
                warn!("Invalid sprite index {:x} selected", index);
                self.set_enable_animset_actions(false);
                return;
            }
        };
        self.sprite_index.store(index, Ordering::SeqCst);
        self.draw_area.queue_draw();
        match sprite {
            SpriteFiles::AnimSet(ref s) => {
                use std::fmt::Write;
                let mut buf = String::new();
                writeln!(buf, "HD: {}", s.hd_filename.to_string_lossy()).unwrap();
                writeln!(buf, "HD2: {}", s.hd2_filename.to_string_lossy()).unwrap();
                self.set_enable_animset_actions(true);
                if let Some(a) = lookup_action(&self.sprite_actions, "select_sd") {
                    a.set_enabled(has_mainsd);
                }
                self.file_list.set_text(&buf);
            }
            SpriteFiles::DdsGrp(_) => {
                self.set_enable_animset_actions(false);
            }
            SpriteFiles::MainSdOnly { .. } => {
                self.set_enable_animset_actions(false);
                let buf = format!("\n\n");
                self.file_list.set_text(&buf);
            }
        }
        self.lighting.select_sprite(index);
        let tex_id = self.tex_id();
        let mut files = self.files.lock();
        let mut file = files.file(tex_id.0, tex_id.1).unwrap_or_else(|e| {
            error!("Couldn't open {:?}: {}", tex_id, e);
            None
        });
        self.changed_ty(tex_id, &mut file);
    }
}

fn create_menu() -> gio::Menu {
    let with_accel = |name: &str, action: &str, accel: &str| {
        let item = gio::MenuItem::new(Some(name), Some(action));
        if accel != "" {
            item.set_attribute_value("accel", Some(&accel.to_variant()));
        }
        item
    };

    let menu = gio::Menu::new();
    let file_menu = {
        let menu = gio::Menu::new();
        let file_actions = {
            let menu = gio::Menu::new();
            menu.append_item(&with_accel("_Open...", "app.open", "<Ctrl>O"));
            menu.append_item(&with_accel("_Save", "app.save", "<Ctrl>S"));
            menu
        };
        menu.append_section(None, &file_actions);
        let exit = {
            let menu = gio::Menu::new();
            menu.append_item(&with_accel("E_xit...", "app.exit", "<Alt>F4"));
            menu
        };
        menu.append_section(None, &exit);
        menu
    };
    // Gtk is dumb and doesn't like underscores w/ accel actions
    let sprite_menu = {
        let menu = gio::Menu::new();
        let export_actions = {
            let menu = gio::Menu::new();
            menu.append_item(&with_accel("_Export frames...", "app.exportFrames", "<Ctrl>E"));
            menu
        };
        menu.append_section(None, &export_actions);
        let import_actions = {
            let menu = gio::Menu::new();
            menu.append_item(&with_accel("_Import frames...", "app.importFrames", "<Ctrl>I"));
            menu
        };
        menu.append_section(None, &import_actions);
        menu
    };
    menu.append_submenu(Some("_File"), &file_menu);
    menu.append_submenu(Some("_Sprite"), &sprite_menu);
    if cfg!(debug_assertions) {
        let debug_menu = {
            let menu = gio::Menu::new();
            menu.append_item(&with_accel("Write test", "app.debug_write", ""));
            menu.append_item(&with_accel("Dump frame info", "app.debug_dump_frames", ""));
            menu
        };
        menu.append_submenu(Some("_Debug"), &debug_menu);
    }
    menu.freeze();
    menu
}

// Requires state to not be borrowed
fn save() -> Result<(), Error> {
    let files = STATE.with(|x| {
        let state = x.borrow();
        state.files.clone()
    });
    let result = {
        let mut files = files.lock();
        files.save()
    };
    if let Err(ref e) = result {
        let msg = format!("Unable to save: {:?}", e);
        ui().message(&msg);
    } else {
        let ui = ui();
        if let Some(a) = lookup_action(&ui.info.sprite_actions, "is_dirty") {
            a.activate(Some(&false.to_variant()));
        }
    }
    result
}

// Return true if the user didn't press cancel
fn check_unsaved_files() -> bool {
    let has_changes = {
        let files = STATE.with(|x| {
            let state = x.borrow();
            state.files.clone()
        });
        let files = files.lock();
        files.has_changes()
    };
    if has_changes {
        let ui = ui();
        let msg = format!("Save changes made to open files?");
        let dialog = gtk::MessageDialog::new(
            Some(&ui.main_window),
            gtk::DialogFlags::MODAL,
            gtk::MessageType::Question,
            gtk::ButtonsType::None,
            &msg,
        );
        dialog.add_button("Save", gtk::ResponseType::Other(1));
        dialog.add_button("Discard changes", gtk::ResponseType::Other(2));
        dialog.add_button("Cancel", gtk::ResponseType::Cancel);
        let result = dialog.run();
        dialog.close();
        match result {
            gtk::ResponseType::Other(1) => {
                let result = save();
                result.is_ok()
            }
            gtk::ResponseType::Other(2) => true,
            _ => false,
        }
    } else {
        true
    }
}

fn create_actions(app: &gtk::Application, main_window: &gtk::Window) {
    fn action<F>(app: &gtk::Application, name: &str, enabled: bool, fun: F) -> gio::SimpleAction
    where F: Fn(&gio::SimpleAction, Option<&glib::Variant>) + 'static
    {
        let action = gio::SimpleAction::new(name, None);
        action.set_enabled(enabled);
        action.connect_activate(fun);
        app.add_action(&action);
        action
    }
    main_window.connect_delete_event(|_, _| {
        if check_unsaved_files() {
            Inhibit(false)
        } else {
            Inhibit(true)
        }
    });
    let a = app.clone();
    action(app, "exit", true, move |_, _| {
        if check_unsaved_files() {
            a.quit()
        }
    });
    let w = main_window.clone();
    action(app, "open", true, move |_, _| {
        if check_unsaved_files() {
            if let Some(filename) = open_file_dialog(&w) {
                open(&filename);
            }
        }
    });
    action(app, "save", false, move |_, _| {
        let _ = save();
    });
    action(app, "exportFrames", false, move |_, _| {
        let ui = ui();
        SpriteInfo::frame_export_dialog(&ui.info, &ui.main_window);
    });
    action(app, "importFrames", false, move |_, _| {
        let ui = ui();
        frame_import_dialog::frame_import_dialog(&ui.info, &ui.main_window);
    });
    if cfg!(debug_assertions) {
        action(app, "debug_write", true, move |_, _| {
            println!("Write test finished");
        });
        action(app, "debug_dump_frames", true, move |_, _| {
            use std::io::Write;

            fn write_frames<W: Write>(file: files::File<'_>, out: &mut W) -> Result<(), Error> {
                if let Some(i) = file.sprite_values() {
                    writeln!(
                        out,
                        "Unk2 {:x} Unk3 {}:{}",
                        i.unk2, i.width, i.height,
                    )?
                }
                if let Some(frames) = file.frames() {
                    for (i, f) in frames.iter().enumerate() {
                        writeln!(
                            out,
                            "Frame {} Tex {}:{} Sprite {}:{} Size {}:{} Unk {:x}",
                            i, f.tex_x, f.tex_y, f.x_off, f.y_off, f.width, f.height, f.unknown,
                        )?
                    }
                }
                Ok(())
            }
            let files = STATE.with(|x| {
                let state = x.borrow();
                state.files.clone()
            });
            let mut files = files.lock();
            let mut out = std::io::BufWriter::new(File::create("frames.txt").unwrap());
            for i in 0..files.sprites().len() {
                if let Some(file) = files.file(i, SpriteType::Sd).unwrap() {
                    writeln!(out, "Sd image {}", i).unwrap();
                    write_frames(file, &mut out).unwrap();
                }
                if let Some(file) = files.file(i, SpriteType::Hd).unwrap() {
                    writeln!(out, "Hd image {}", i).unwrap();
                    write_frames(file, &mut out).unwrap();
                }
                if let Some(file) = files.file(i, SpriteType::Hd2).unwrap() {
                    writeln!(out, "Hd2 image {}", i).unwrap();
                    write_frames(file, &mut out).unwrap();
                }
            }
            println!("Frames dumped");
        });
    }
}

fn enable_file_actions(app: &gtk::Application) {
    if let Some(a) = lookup_action(app, "save") {
        a.set_enabled(true);
    }
    if let Some(a) = lookup_action(app, "importFrames") {
        a.set_enabled(true);
    }
    if let Some(a) = lookup_action(app, "exportFrames") {
        a.set_enabled(true);
    }
}

fn open(filename: &Path) {
    let ui = ui();
    match files::Files::init(filename) {
        Ok((f, index)) => {
            ui.files_changed(&f);
            {
                STATE.with(|x| {
                    let state = x.borrow();
                    let mut files = state.files.lock();
                    *files = f;
                });
            }
            ui.info.draw_clear_all();
            ui.info.sprite_actions.activate_action("select_sd", None);
            let index = index.unwrap_or(0);
            ui.info.select_sprite(index);
            ui.list.list.select(index);
            enable_file_actions(&ui.app);
        }
        Err(e) => {
            let msg = format!("Unable to open file: {:?}", e);
            ui.message(&msg);
        }
    }
}

fn open_file_dialog(parent: &gtk::Window) -> Option<PathBuf> {
    let dialog = gtk::FileChooserNative::new(
        Some("Open..."),
        Some(parent),
        gtk::FileChooserAction::Open,
        Some("Open"),
        Some("Cancel")
    );
    if let Some(path) = select_dir::read_config_entry("open_file") {
        dialog.set_current_folder(&path);
    }
    dialog.set_select_multiple(false);
    let filter = gtk::FileFilter::new();
    filter.add_pattern("*.anim");
    filter.add_pattern("*.dds.grp");
    filter.add_pattern("*.dds.vr4");
    filter.set_name(Some("Valid files"));
    dialog.add_filter(&filter);
    let filter = gtk::FileFilter::new();
    filter.add_pattern("*.*");
    filter.set_name(Some("All files"));
    dialog.add_filter(&filter);
    //dialog.add_button("Open", gtk::ResponseType::Accept.into());
    //dialog.add_button("Cancel", gtk::ResponseType::Cancel.into());
    let result: gtk::ResponseType = dialog.run().into();
    let result = if result == gtk::ResponseType::Accept {
        if let Some(path) = dialog.get_filename() {
            if let Some(parent) = path.parent() {
                select_dir::set_config_entry("open_file", &parent.to_string_lossy());
            }
        }
        dialog.get_filename()
    } else {
        None
    };
    dialog.destroy();
    result
}

fn get_css_provider() -> gtk::CssProvider {
    CSS.with(|x| x.clone())
}

fn init_css_provider() -> gtk::CssProvider {
    let css = gtk::CssProvider::new();
    if ::std::path::Path::new("animosity.css").is_file() {
        let errors = ::std::rc::Rc::new(::std::cell::RefCell::new(Vec::new()));

        let errs = errors.clone();
        css.connect_parsing_error(move |_, _, e| {
            errs.borrow_mut().push(e.to_string());
        });
        let file = gio::File::new_for_path("animosity.css");
        let _ = css.load_from_file(&file);
        let mut errors = errors.borrow_mut();
        if !errors.is_empty() {
            let mut msg = format!("CSS parsing failed:\n");
            for e in errors.drain(..) {
                msg.push_str(&e);
                msg.push_str("\n");
            }
            panic!(msg);
        }
    }
    css
}

fn create_ui(app: &gtk::Application) -> Ui {
    app.set_menubar(Some(&create_menu()));

    let window = gtk::ApplicationWindow::new(app);

    let box1 = gtk::Box::new(gtk::Orientation::Horizontal, 0);
    let files = {
        STATE.with(|x| x.borrow().files.clone())
    };
    let info = SpriteInfo::new(&files);
    let list = SpriteList::new(info.clone());
    box1.pack_start(&list.widget(), false, false, 0);
    box1.pack_start(&info.widget(), true, true, 0);
    window.add(&box1);

    let w = window.clone();
    info.on_dirty_update(move |dirty| {
        STATE.with(|x| {
            let state = x.borrow();
            let files = state.files.lock();
            w.set_title(&title(files.root_path(), dirty));
        });
    });
    window.set_title(&title(None, false));
    window.resize(800, 600);

    let style_ctx = window.get_style_context();
    let css = crate::get_css_provider();
    style_ctx.add_provider(&css, 600 /* GTK_STYLE_PROVIDER_PRIORITY_APPLICATION */);

    Ui {
        app: app.clone(),
        main_window: window,
        list,
        info,
    }
}

fn label_section<O: IsA<gtk::Widget>>(name: &str, obj: &O) -> gtk::Box {
    let bx = gtk::Box::new(gtk::Orientation::Vertical, 0);
    let label = gtk::Label::new(Some(name));
    label.set_halign(gtk::Align::Start);
    bx.pack_start(&label, false, false, 0);
    bx.pack_start(obj, false, false, 0);
    bx
}

fn info_msg_box<W: IsA<gtk::Window>, S: AsRef<str>>(window: &W, msg: S) {
    let dialog = gtk::MessageDialog::new(
        Some(window),
        gtk::DialogFlags::MODAL,
        gtk::MessageType::Info,
        gtk::ButtonsType::None,
        msg.as_ref(),
    );
    dialog.add_button("Ok", gtk::ResponseType::Ok);
    dialog.run();
    dialog.close();
}

fn error_msg_box<W: IsA<gtk::Window>, S: AsRef<str>>(window: &W, msg: S) {
    let dialog = gtk::MessageDialog::new(
        Some(window),
        gtk::DialogFlags::MODAL,
        gtk::MessageType::Error,
        gtk::ButtonsType::None,
        msg.as_ref(),
    );
    dialog.add_button("Ok", gtk::ResponseType::Ok);
    dialog.run();
    dialog.close();
}

#[derive(Clone)]
struct SavedCheckbox {
    check: gtk::CheckButton,
}

impl SavedCheckbox {
    pub fn new<S: Into<String>>(save_entry: S, label: &str) -> SavedCheckbox {
        let check = gtk::CheckButton::with_label(label);
        let save_entry = save_entry.into();
        if select_dir::read_config_entry(&save_entry).map(|x| x == "y").unwrap_or(false) {
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
}
