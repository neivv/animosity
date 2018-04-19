#![windows_subsystem = "windows"]

extern crate app_dirs;
extern crate byteorder;
extern crate cairo;
extern crate cgmath;
extern crate ddsfile;
#[macro_use] extern crate failure;
extern crate fern;
extern crate gdk;
extern crate gio;
extern crate glib;
#[macro_use] extern crate glium;
extern crate gtk;
#[macro_use] extern crate log;
extern crate png;
#[macro_use] extern crate serde_derive;
extern crate serde_json;

mod anim;
mod anim_encoder;
mod ddsgrp;
mod gl;
mod int_entry;
mod files;
mod select_dir;
mod shaders;

use std::borrow::Cow;
use std::cell::{Cell, RefCell};
use std::convert::TryFrom;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::rc::Rc;

use gio::prelude::*;
use gtk::prelude::*;

use cgmath::conv::array4x4;
use cgmath::{Matrix4, vec4};
use failure::{Error, ResultExt};
use glium::backend::glutin::headless::Headless;
use glium::framebuffer::SimpleFrameBuffer;
use glium::index::{IndexBuffer, PrimitiveType};
use glium::texture::{self, Texture2d};
use glium::vertex::VertexBuffer;

use files::SpriteFiles;
use int_entry::{IntEntry, IntSize};

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

fn main() {
    let _ = init_log();
    let app = gtk::Application::new("a.b", gio::ApplicationFlags::empty())
        .unwrap_or_else(|e| panic!("Couldn't create app: {}", e));
    app.connect_startup(|app| {
        let ui = create_ui(app);
        create_actions(app, &ui.main_window.clone().upcast());
        ui.main_window.show_all();
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
    files: Rc<RefCell<files::Files>>,
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
        files: Rc::new(RefCell::new(files::Files::empty())),
    });
}

fn ui() -> Rc<Ui> {
    UI.with(|x| {
        x.borrow_mut().as_ref().expect("UI not initialized").clone()
    })
}

impl Ui {
    fn message(&self, msg: &str) {
        let dialog = gtk::MessageDialog::new(
            Some(&self.main_window),
            gtk::DialogFlags::MODAL,
            gtk::MessageType::Error,
            gtk::ButtonsType::Ok,
            msg
        );
        dialog.run();
        dialog.destroy();
    }

    fn files_changed(&self, files: &files::Files) {
        self.list.list.clear();
        for sprite in files.sprites() {
            let name: Cow<str> = match *sprite {
                SpriteFiles::AnimSet(ref s) => (&*s.name).into(),
                SpriteFiles::SingleFile(_) => "(File)".into(),
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
        let store = gtk::ListStore::new(&[gtk::Type::String]);
        let list = gtk::TreeView::new_with_model(&store);
        let col = gtk::TreeViewColumn::new();
        let renderer = gtk::CellRendererText::new();
        col.pack_end(&renderer, true);
        col.add_attribute(&renderer, "text", 0);
        list.append_column(&col);
        list.set_headers_visible(false);

        let root = gtk::ScrolledWindow::new(None, None);
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
        let path = gtk::TreePath::new_from_indicesv(&[index as i32]);
        self.list.set_cursor(&path, None, false);
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
        let ref_enable = gtk::CheckButton::new_with_label("References image");
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
            <_ as gio::SimpleActionExt>::connect_property_enabled_notify(&a, move |s| {
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
        let sd = gtk::RadioButton::new_with_label("SD");
        let hd = gtk::RadioButton::new_with_label_from_widget(&sd, "HD");
        let hd2 = gtk::RadioButton::new_with_label_from_widget(&sd, "HD2");
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

struct SpriteInfo {
    bx: gtk::Box,
    file_list: gtk::TextBuffer,
    files: Rc<RefCell<files::Files>>,
    sprite_actions: gio::SimpleActionGroup,
    sprite_index: AtomicUsize,
    selected_layer: AtomicUsize,
    selector: SpriteSelector,
    selected_type: Cell<SpriteType>,
    draw_area: gtk::DrawingArea,
    draw_clear_requests: RefCell<Vec<TextureId>>,
}

fn lookup_action<G: IsA<gio::ActionMap>>(group: &G, name: &str) -> Option<gio::SimpleAction> {
    group.lookup_action(name).and_then(|x| x.downcast::<gio::SimpleAction>().ok())
}

impl SpriteInfo {
    fn new(file_shared: Rc<RefCell<files::Files>>) -> Arc<SpriteInfo> {
        let bx = gtk::Box::new(gtk::Orientation::Vertical, 0);
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
        let file_list = gtk::TextBuffer::new(None);
        file_list.set_text("\n\n\n");
        files.set_buffer(Some(&file_list));
        files.set_editable(false);
        files.set_wrap_mode(gtk::WrapMode::None);
        bx.pack_start(&sprite_bx, true, true, 0);
        bx.pack_start(&files, false, false, 0);
        let result = Arc::new(SpriteInfo {
            bx,
            file_list,
            files: file_shared,
            sprite_actions,
            sprite_index: AtomicUsize::new(0),
            selected_layer: AtomicUsize::new(0),
            selector,
            selected_type: Cell::new(SpriteType::Sd),
            draw_area: draw_area.clone(),
            draw_clear_requests: RefCell::new(Vec::new()),
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
                        |_| {},
                        cairo::Format::ARgb32,
                        width as i32,
                        height as i32,
                        width as i32 * 4,
                    ).expect("Couldn't create cairo image surface");
                    // Could recycle the surface?
                    let pattern = cairo::SurfacePattern::create(&surface);
                    cairo.set_source(&pattern);
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
        let tex_id = this.tex_id();
        let mut files = match this.files.try_borrow_mut() {
            Ok(o) => o,
            _ => return,
        };
        let file = match files.file(tex_id.0, tex_id.1) {
            Ok(Some(o)) => o,
            _ => return,
        };
        let layer_names = file.layer_names();

        let window = gtk::Window::new(gtk::WindowType::Toplevel);
        let bx = gtk::Box::new(gtk::Orientation::Vertical, 10);

        let dir_select = select_dir::SelectDir::new(&window, "export_frames");
        let filename_bx = label_section("Output directory", &dir_select.widget());

        let type_lowercase = match tex_id.1 {
            SpriteType::Sd => "sd",
            SpriteType::Hd => "hd",
            SpriteType::Hd2 => "hd2",
        };

        let framedef_bx = gtk::Box::new(gtk::Orientation::Vertical, 0);
        let framedef_label = gtk::Label::new(Some("Write miscellaneous frame info to.."));
        let (framedef_entry, framedef_frame) = int_entry::entry();
        let framedef_name = format!("frames_{:03}_{}.json", tex_id.0, type_lowercase);
        framedef_entry.set_text(&framedef_name);
        framedef_label.set_halign(gtk::Align::Start);
        framedef_bx.pack_start(&framedef_label, false, false, 0);
        framedef_bx.pack_start(&framedef_frame, true, true, 0);

        let mut checkboxes = Vec::with_capacity(layer_names.len());
        let mut grp_prefix = None;
        let is_anim = file.is_anim();
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
            grp_prefix = Some(entry.clone());
            label_section("Filename prefix", &frame)
        };

        let button_bx = gtk::Box::new(gtk::Orientation::Horizontal, 15);
        let ok_button = gtk::Button::new_with_label("Export");
        let cancel_button = gtk::Button::new_with_label("Cancel");
        let w = window.clone();
        cancel_button.connect_clicked(move |_| {
            w.destroy();
        });
        let s = this.clone();
        let w = window.clone();
        ok_button.connect_clicked(move |_| {
            let path: PathBuf = match dir_select.text() {
                Some(s) => s.into(),
                None => return,
            };

            let tex_id = s.tex_id();
            let mut files = match s.files.try_borrow_mut() {
                Ok(o) => o,
                _ => return,
            };
            let file = match files.file(tex_id.0, tex_id.1) {
                Ok(Some(o)) => o,
                _ => return,
            };

            let layers_to_export;
            let result = if is_anim {
                let framedef: PathBuf = match framedef_entry.get_text() {
                    Some(s) => s.into(),
                    None => return,
                };

                layers_to_export = checkboxes.iter().map(|(check, entry)| {
                    if check.get_active() {
                        Some(entry.get_text().unwrap_or_else(|| String::new()))
                    } else {
                        None
                    }
                }).collect::<Vec<_>>();
                export_frames(&file, tex_id.1, &path, &framedef, &layers_to_export)
            } else {
                let prefix = grp_prefix.as_ref()
                    .and_then(|x| x.get_text()).unwrap_or_else(String::new);
                layers_to_export = Vec::new();
                export_grp(&file, &path, &prefix)
            };
            match result {
                Ok(()) => {
                    let frame_count = if is_anim {
                        layers_to_export.iter().filter(|x| x.is_some()).count() *
                        file.frames().map(|x| x.len()).unwrap_or(0)
                    } else {
                        file.layer_count()
                    };
                    let msg =
                        format!("Wrote {} frames to {}", frame_count, path.to_string_lossy());
                    let dialog = gtk::MessageDialog::new(
                        Some(&w),
                        gtk::DialogFlags::MODAL,
                        gtk::MessageType::Info,
                        gtk::ButtonsType::Ok,
                        &msg
                    );
                    dialog.run();
                    dialog.destroy();
                    w.destroy();
                }
                Err(e) => {
                    use std::fmt::Write;
                    let mut msg = format!("Unable to export frames:\n");
                    for c in e.causes() {
                        writeln!(msg, "{}", c).unwrap();
                    }
                    // Remove last newline
                    msg.pop();
                    let dialog = gtk::MessageDialog::new(
                        Some(&w),
                        gtk::DialogFlags::MODAL,
                        gtk::MessageType::Error,
                        gtk::ButtonsType::Ok,
                        &msg
                    );
                    dialog.run();
                    dialog.destroy();
                }
            };
        });
        button_bx.pack_end(&cancel_button, false, false, 0);
        button_bx.pack_end(&ok_button, false, false, 0);
        bx.pack_start(&filename_bx, false, false, 0);
        if is_anim {
            bx.pack_start(&framedef_bx, false, false, 0);
        }
        bx.pack_start(&layers_bx, false, false, 0);
        bx.pack_start(&button_bx, false, false, 0);
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
        window.set_modal(true);
        window.set_transient_for(Some(parent));
        window.show_all();
    }

    fn frame_import_dialog(this: &Arc<SpriteInfo>, parent: &gtk::ApplicationWindow) {
        let tex_id = this.tex_id();
        let mut files = match this.files.try_borrow_mut() {
            Ok(o) => o,
            _ => return,
        };
        let file = match files.file(tex_id.0, tex_id.1) {
            Ok(Some(o)) => o,
            _ => return,
        };
        let layer_names = file.layer_names();

        let window = gtk::Window::new(gtk::WindowType::Toplevel);
        let bx = gtk::Box::new(gtk::Orientation::Vertical, 10);

        let is_anim = file.is_anim();
        let mut framedef_filename = None;
        let framedef_status = gtk::Label::new(None);
        let framedef;
        let framedef_bx = if is_anim {
            framedef = Rc::new(
                select_dir::SelectFile::new(&window, "import_frames", "Text files", "*.json")
            );
            let framedef_inner_bx = gtk::Box::new(gtk::Orientation::Vertical, 0);
            framedef_inner_bx.pack_start(&framedef.widget(), false, false, 0);
            framedef_status.set_halign(gtk::Align::Start);
            framedef_inner_bx.pack_start(&framedef_status, false, false, 5);
            framedef_filename = framedef.text().and_then(|x| match x.is_empty() {
                true => None,
                false => Some(x),
            });
            label_section("Frame info file", &framedef_inner_bx)
        } else {
            framedef = Rc::new(
                select_dir::SelectFile::new(&window, "import_frames_grp", "PNG files", "*.png")
            );
            let inner_bx = gtk::Box::new(gtk::Orientation::Vertical, 0);
            inner_bx.pack_start(&framedef.widget(), false, false, 0);
            label_section("First frame", &inner_bx)
        };

        let mut checkboxes = Vec::with_capacity(layer_names.len());
        let mut grp_format = None;
        let tex_formats = file.texture_formats();
        let layers_bx = if is_anim {
            let grid = gtk::Grid::new();
            grid.set_column_spacing(5);
            grid.set_row_spacing(5);

            for (i, name) in layer_names.iter().enumerate() {
                let row = i as i32 + 1;

                let checkbox = gtk::CheckButton::new();
                grid.attach(&checkbox, 0, row, 1, 1);
                checkbox.set_sensitive(false);

                let label = gtk::Label::new(Some(&**name));
                grid.attach(&label, 1, row, 1, 1);
                label.set_halign(gtk::Align::Start);
                let format = gtk::ComboBoxText::new();
                format.append_text("DXT1");
                format.append_text("DXT5");
                format.append_text("Monochrome");
                grid.attach(&format, 2, row, 1, 1);

                checkboxes.push((checkbox, format));
            }
            label_section("Layers", &grid)
        } else {
            let format = gtk::ComboBoxText::new();
            format.append_text("DXT1");
            format.append_text("DXT5");
            format.append_text("Monochrome");
            if let Some(Ok(Some(tex_f))) = tex_formats.get(0) {
                format.set_active(match tex_f {
                    anim::TextureFormat::Dxt1 => 0,
                    anim::TextureFormat::Dxt5 => 1,
                    anim::TextureFormat::Monochrome => 2,
                });
            }

            let bx = label_section("Encode format", &format);
            grp_format = Some(format);
            bx
        };

        let button_bx = gtk::Box::new(gtk::Orientation::Horizontal, 15);
        let ok_button = gtk::Button::new_with_label("Import");
        ok_button.set_sensitive(!is_anim);
        let cancel_button = gtk::Button::new_with_label("Cancel");
        let w = window.clone();
        cancel_button.connect_clicked(move |_| {
            w.destroy();
        });
        let s = this.clone();
        let w = window.clone();
        let frame_info: Rc<RefCell<Option<FrameInfo>>> = Rc::new(RefCell::new(None));
        let fi = frame_info.clone();
        let fi_entry = framedef.clone();
        let c = checkboxes.clone();
        ok_button.connect_clicked(move |_| {
            // Used for grps
            let filename_prefix;
            let dir = match fi_entry.text() {
                Some(s) => {
                    let mut buf: PathBuf = s.into();
                    filename_prefix = match buf.file_name() {
                        Some(s) => {
                            let text = s.to_string_lossy();
                            match text.find("_000.") {
                                Some(s) => Some((&text[..s]).to_string()),
                                None => None,
                            }
                        }
                        None => None,
                    };
                    buf.pop();
                    if !buf.is_dir() {
                        return;
                    }
                    buf
                }
                None => return,
            };
            let mut files = s.files.borrow_mut();
            let result = if is_anim {
                let formats = c.iter().map(|x| {
                    Ok(match x.1.get_active() {
                        0 => anim::TextureFormat::Dxt1,
                        1 => anim::TextureFormat::Dxt5,
                        2 => anim::TextureFormat::Monochrome,
                        _ => {
                            if x.0.get_active() {
                                return Err(());
                            }
                            // Just a dummy value since the layer is unused
                            anim::TextureFormat::Monochrome
                        }
                    })
                }).collect::<Result<Vec<_>, ()>>();
                let formats = match formats {
                    Ok(o) => o,
                    Err(()) => {
                        let dialog = gtk::MessageDialog::new(
                            Some(&w),
                            gtk::DialogFlags::MODAL,
                            gtk::MessageType::Error,
                            gtk::ButtonsType::Ok,
                            "Format not specified for every layer",
                        );
                        dialog.run();
                        dialog.destroy();
                        return;
                    }
                };
                let fi = fi.borrow();
                let frame_info = match *fi {
                    Some(ref s) => s,
                    None => return,
                };

                let result =
                    import_frames(&mut files, frame_info, &dir, &formats, tex_id.0, tex_id.1);
                result.map(|()| frame_info.layers.len() as u32 * frame_info.frame_count)
            } else {
                let format = match grp_format {
                    Some(ref s) => match s.get_active() {
                        0 => Ok(anim::TextureFormat::Dxt1),
                        1 => Ok(anim::TextureFormat::Dxt5),
                        2 => Ok(anim::TextureFormat::Monochrome),
                        _ => Err(()),
                    },
                    None => return,
                };
                let format = match format {
                    Ok(o) => o,
                    Err(()) => {
                        let dialog = gtk::MessageDialog::new(
                            Some(&w),
                            gtk::DialogFlags::MODAL,
                            gtk::MessageType::Error,
                            gtk::ButtonsType::Ok,
                            "Format not specified",
                        );
                        dialog.run();
                        dialog.destroy();
                        return;
                    }
                };
                let filename_prefix = match filename_prefix {
                    Some(o) => o,
                    None => {
                        let dialog = gtk::MessageDialog::new(
                            Some(&w),
                            gtk::DialogFlags::MODAL,
                            gtk::MessageType::Error,
                            gtk::ButtonsType::Ok,
                            "Invalid first frame filename",
                        );
                        dialog.run();
                        dialog.destroy();
                        return;
                    }
                };
                let result = import_frames_grp(
                    &mut files,
                    &dir,
                    &filename_prefix,
                    format,
                    tex_id.0,
                    tex_id.1,
                );
                result
            };
            match result {
                Ok(frame_count) => {
                    s.draw_clear_all();
                    if let Ok(mut file) = files.file(tex_id.0, tex_id.1) {
                        s.changed_ty(tex_id, &mut file);
                    }
                    drop(files);
                    if let Some(a) = lookup_action(&s.sprite_actions, "is_dirty") {
                        a.activate(Some(&true.to_variant()));
                    }

                    let msg = format!("Imported {} frames", frame_count);
                    let dialog = gtk::MessageDialog::new(
                        Some(&w),
                        gtk::DialogFlags::MODAL,
                        gtk::MessageType::Info,
                        gtk::ButtonsType::Ok,
                        &msg
                    );
                    dialog.run();
                    dialog.destroy();
                    w.destroy();
                }
                Err(e) => {
                    drop(files);
                    use std::fmt::Write;
                    let mut msg = format!("Unable to import frames:\n");
                    for c in e.causes() {
                        writeln!(msg, "{}", c).unwrap();
                    }
                    // Remove last newline
                    msg.pop();
                    let dialog = gtk::MessageDialog::new(
                        Some(&w),
                        gtk::DialogFlags::MODAL,
                        gtk::MessageType::Error,
                        gtk::ButtonsType::Ok,
                        &msg
                    );
                    dialog.run();
                    dialog.destroy();
                }
            }
        });

        let ok = ok_button.clone();
        if is_anim {
            let framedef_set = Rc::new(move |filename: &str| {
                match parse_frame_info(Path::new(filename)) {
                    Ok(o) => {
                        ok.set_sensitive(true);
                        for &(ref check, ref format) in &checkboxes {
                            check.set_active(false);
                            format.set_sensitive(false);
                            format.set_active(-1);
                        }
                        for &(i, _) in &o.layers {
                            if let Some(&(ref check, ref format)) = checkboxes.get(i as usize) {
                                check.set_active(true);
                                let tex_f = tex_formats.get(i as usize)
                                    .and_then(|x| x.as_ref().ok())
                                    .and_then(|x| x.as_ref());
                                if let Some(tex_f) = tex_f {
                                    format.set_sensitive(true);
                                    format.set_active(match tex_f {
                                        anim::TextureFormat::Dxt1 => 0,
                                        anim::TextureFormat::Dxt5 => 1,
                                        anim::TextureFormat::Monochrome => 2,
                                    });
                                }
                            }
                        }
                        *frame_info.borrow_mut() = Some(o);
                        framedef_status.set_text("");
                    }
                    Err(e) => {
                        ok.set_sensitive(false);
                        for &(ref check, ref format) in &checkboxes {
                            check.set_active(false);
                            format.set_sensitive(false);
                            format.set_active(-1);
                        }
                        let mut msg = format!("Frame info invalid:\n");
                        for c in e.causes() {
                            use std::fmt::Write;
                            writeln!(msg, "{}", c).unwrap();
                        }
                        // Remove last newline
                        msg.pop();
                        *frame_info.borrow_mut() = None;
                        framedef_status.set_text(&msg);
                    }
                }
            });
            if let Some(filename) = framedef_filename {
                framedef_set(&filename);
            }
            let fun = framedef_set.clone();
            framedef.on_change(move |filename| {
                fun(filename);
            });
        }

        button_bx.pack_end(&cancel_button, false, false, 0);
        button_bx.pack_end(&ok_button, false, false, 0);
        bx.pack_start(&framedef_bx, false, false, 0);
        bx.pack_start(&layers_bx, false, false, 0);
        bx.pack_start(&button_bx, false, false, 0);
        window.add(&bx);
        window.set_border_width(10);
        window.set_property_default_width(350);
        if is_anim {
            window.set_title(&format!("Import frames for {:?} image {}", tex_id.1, tex_id.0));
        } else {
            if let Some(filename) = file.path().file_name() {
                window.set_title(&format!("Import frames of {}", filename.to_string_lossy()));
            }
        }
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
        cache_file: &mut files::File,
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
        buf: &mut SimpleFrameBuffer,
        facade: &Headless,
        draw_params: &mut DrawParams,
        (buf_width, buf_height): (u32, u32),
        buf_stride: u32,
    ) -> Result<(), Error> {
        use glium::Surface;

        buf.clear_color(0.0, 0.0, 0.0, 1.0);
        let tex_id = self.tex_id();
        let mut files = self.files.try_borrow_mut()?;
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
        where F: Fn(&gio::SimpleAction, &Option<glib::Variant>) + 'static
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
            if let Some(layer) = param.as_ref().and_then(|x| x.get::<u32>()) {
                s.selected_layer.store(layer as usize, Ordering::SeqCst);
                s.draw_area.queue_draw();
            }
        });
        let s = this.clone();
        action(group, "edit_enable_ref", true, Some("b"), move |_, param| {
            if let Some(value) = param.as_ref().and_then(|x| x.get::<bool>()) {
                s.set_ref_enabled(value);
            }
        });
        action(group, "enable_ref", false, Some("b"), move |_, _| {
        });
        action(group, "init_ref_img", true, Some("u"), move |_, _| {
        });
        let s = this.clone();
        action(group, "edit_ref_img", true, Some("u"), move |_, param| {
            if let Some(value) = param.as_ref().and_then(|x| x.get::<u32>()) {
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
            if let Some(value) = param.as_ref().and_then(|x| x.get::<u32>()) {
                s.update_active_file(|x, _| {
                    x.unk2 = value as u16;
                });
            }
        });
        let s = this.clone();
        action(group, "edit_unk3a", true, Some("u"), move |_, param| {
            if let Some(value) = param.as_ref().and_then(|x| x.get::<u32>()) {
                s.update_active_file(|x, _| {
                    x.width = value as u16;
                });
            }
        });
        let s = this.clone();
        action(group, "edit_unk3b", true, Some("u"), move |_, param| {
            if let Some(value) = param.as_ref().and_then(|x| x.get::<u32>()) {
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
            let mut files = match self.files.try_borrow_mut() {
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
            let mut files = match self.files.try_borrow_mut() {
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
            let mut files = match self.files.try_borrow_mut() {
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
        let mut files = match self.files.try_borrow_mut() {
            Ok(o) => o,
            _ => return,
        };
        let mut file = files.file(tex_id.0, tex_id.1).unwrap_or_else(|e| {
            error!("Couldn't open {:?}: {}", tex_id, e);
            None
        });
        self.changed_ty(tex_id, &mut file);
    }

    fn changed_ty(&self, tex_id: TextureId, file: &mut Option<files::File>) {
        let ty = tex_id.1;
        self.set_layers(file);
        if let Some(ref mut file) = *file {
            let variant = true.to_variant();
            self.sprite_actions.activate_action("sprite_exists", Some(&variant));
            let sprite_data = file.sprite_values();
            let sprite_data = sprite_data.as_ref();
            if let Some(a) = lookup_action(&self.sprite_actions, "enable_ref") {
                if ty == SpriteType::Sd {
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
            let variant = {
                let tex_sizes = file.texture_size(tex_id.2);
                if let Some(t) = tex_sizes {
                    format!("{}x{}", t.width, t.height).to_variant()
                } else {
                    "0x0".to_variant()
                }
            };
            self.sprite_actions.activate_action("texture_size", Some(&variant));
            if let Some(data) = sprite_data {
                let variant = (data.unk2 as u32).to_variant();
                self.sprite_actions.activate_action("init_unk2", Some(&variant));
                let variant = (data.width as u32).to_variant();
                self.sprite_actions.activate_action("init_unk3a", Some(&variant));
                let variant = (data.height as u32).to_variant();
                self.sprite_actions.activate_action("init_unk3b", Some(&variant));
            }
            let frame_count = if file.is_anim() {
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

    fn set_layers(&self, file: &Option<files::File>) {
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
            let mut files = match self.files.try_borrow_mut() {
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

                let tex_id = self.tex_id();
                let mut files = match self.files.try_borrow_mut() {
                    Ok(o) => o,
                    _ => return,
                };
                let mut file = files.file(tex_id.0, tex_id.1).unwrap_or_else(|e| {
                    error!("Couldn't open {:?}: {}", tex_id, e);
                    None
                });
                self.changed_ty(tex_id, &mut file);
            }
            SpriteFiles::SingleFile(_) => {
                self.set_enable_animset_actions(false);
            }
            SpriteFiles::MainSdOnly { .. } => {
                self.set_enable_animset_actions(false);
                let buf = format!("\n\n");
                self.file_list.set_text(&buf);
            }
        }
    }
}

fn create_menu() -> gio::Menu {
    use gio::MenuExt;
    use gio::MenuItemExt;

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
            menu.append_item(&with_accel("_Save...", "app.save", "<Ctrl>S"));
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

fn create_actions(app: &gtk::Application, main_window: &gtk::Window) {
    fn action<F>(app: &gtk::Application, name: &str, enabled: bool, fun: F) -> gio::SimpleAction
    where F: Fn(&gio::SimpleAction, &Option<glib::Variant>) + 'static
    {
        let action = gio::SimpleAction::new(name, None);
        action.set_enabled(enabled);
        action.connect_activate(fun);
        app.add_action(&action);
        action
    }
    let a = app.clone();
    action(app, "exit", true, move |_, _| a.quit());
    let w = main_window.clone();
    action(app, "open", true, move |_, _| {
        // TODO SAVE IF CHANGED
        println!("TODO CHECK SAVE");
        if let Some(filename) = open_file_dialog(&w) {
            open(&filename);
        }
    });
    action(app, "save", false, move |_, _| {
        let files = STATE.with(|x| {
            let state = x.borrow();
            state.files.clone()
        });
        let result = {
            let mut files = files.borrow_mut();
            files.save()
        };
        if let Err(e) = result {
            use std::fmt::Write;
            let mut msg = format!("Unable to save:\n");
            for c in e.causes() {
                writeln!(msg, "{}", c).unwrap();
            }
            // Remove last newline
            msg.pop();
            ui().message(&msg);
        } else {
            let ui = ui();
            if let Some(a) = lookup_action(&ui.info.sprite_actions, "is_dirty") {
                a.activate(Some(&false.to_variant()));
            }
        }
    });
    action(app, "exportFrames", false, move |_, _| {
        let ui = ui();
        SpriteInfo::frame_export_dialog(&ui.info, &ui.main_window);
    });
    action(app, "importFrames", false, move |_, _| {
        let ui = ui();
        SpriteInfo::frame_import_dialog(&ui.info, &ui.main_window);
    });
    if cfg!(debug_assertions) {
        action(app, "debug_write", true, move |_, _| {
            let files = STATE.with(|x| {
                let state = x.borrow();
                state.files.clone()
            });
            let mut files = files.borrow_mut();
            let out = std::fs::File::create("out/mainsd.anim").unwrap();
            files.write_mainsd(out).unwrap();
            let out = std::fs::File::create("out/main_028.anim").unwrap();
            files.write_separate(out, 28, SpriteType::Hd).unwrap();
            println!("Write test finished");
        });
        action(app, "debug_dump_frames", true, move |_, _| {
            use std::io::Write;

            fn write_frames<W: Write>(file: files::File, out: &mut W) -> Result<(), Error> {
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
            let mut files = files.borrow_mut();
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
        Ok(f) => {
            ui.files_changed(&f);
            {
                STATE.with(|x| {
                    let state = x.borrow();
                    let mut files = state.files.borrow_mut();
                    *files = f;
                });
            }
            ui.info.draw_clear_all();
            ui.info.sprite_actions.activate_action("select_sd", None);
            ui.info.select_sprite(0);
            enable_file_actions(&ui.app);
        }
        Err(e) => {
            use std::fmt::Write;
            let mut msg = format!("Unable to open file:\n");
            for c in e.causes() {
                writeln!(msg, "{}", c).unwrap();
            }
            // Remove last newline
            msg.pop();
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
    dialog.set_select_multiple(false);
    let filter = gtk::FileFilter::new();
    filter.add_pattern("*.anim");
    filter.add_pattern("*.dds.grp");
    filter.add_pattern("*.dds.vr4");
    gtk::FileFilterExt::set_name(&filter, "Valid files");
    dialog.add_filter(&filter);
    let filter = gtk::FileFilter::new();
    filter.add_pattern("*.*");
    gtk::FileFilterExt::set_name(&filter, "All files");
    dialog.add_filter(&filter);
    //dialog.add_button("Open", gtk::ResponseType::Accept.into());
    //dialog.add_button("Cancel", gtk::ResponseType::Cancel.into());
    let result: gtk::ResponseType = dialog.run().into();
    let result = if result == gtk::ResponseType::Accept {
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
    let info = SpriteInfo::new(files);
    let list = SpriteList::new(info.clone());
    box1.pack_start(&list.widget(), false, false, 0);
    box1.pack_start(&info.widget(), true, true, 0);
    window.add(&box1);

    let w = window.clone();
    info.on_dirty_update(move |dirty| {
        STATE.with(|x| {
            let state = x.borrow();
            let files = state.files.borrow();
            w.set_title(&title(files.root_path(), dirty));
        });
    });
    window.set_title(&title(None, false));
    window.resize(800, 600);
    if let Some(style_ctx) = window.get_style_context() {
        let css = ::get_css_provider();
        style_ctx.add_provider(&css, 600 /* GTK_STYLE_PROVIDER_PRIORITY_APPLICATION */);
    }
    Ui {
        app: app.clone(),
        main_window: window,
        list,
        info,
    }
}

fn export_grp(file: &files::File, path: &Path, prefix: &str) -> Result<(), Error> {
    use std::io::BufWriter;

    use png::HasParameters;

    if !path.is_dir() {
        return Err(format_err!("{} is not a directory", path.to_string_lossy()));
    }

    for i in 0..file.layer_count() {
        let texture = file.texture(i)?;

        let path = path.join(format!("{}_{:03}.png", prefix, i));
        let out = File::create(&path)
            .with_context(|_| format!("Unable to create {}", path.to_string_lossy()))?;
        let mut out = BufWriter::new(out);

        let mut encoder = png::Encoder::new(out, texture.width, texture.height);
        encoder.set(png::ColorType::RGBA);
        let mut encoder = encoder.write_header()?;
        encoder.write_image_data(&texture.data)?;
    }

    Ok(())
}

// Won't export layers with None prefix,
// framedef_file is joined to path, as are the image names
fn export_frames(
    file: &files::File,
    ty: SpriteType,
    path: &Path,
    framedef_file: &Path,
    layer_prefixes: &[Option<String>],
) -> Result<(), Error> {
    use std::io::BufWriter;

    use png::HasParameters;

    if !path.is_dir() {
        return Err(format_err!("{} is not a directory", path.to_string_lossy()));
    }

    let scale_div = match ty {
        SpriteType::Hd2 => 2u32,
        _ => 1u32,
    };

    let frames = file.frames().ok_or_else(|| format_err!("Unable to get frames"))?;
    let values = match file.sprite_values() {
        Some(s) => s,
        None => return Err(format_err!("Couldn't get sprite values")),
    };
    let enum_prefixes =
        layer_prefixes.iter().enumerate().flat_map(|(i, x)| x.as_ref().map(|x| (i, x)));
    let x_base =
        frames.iter().map(|x| x.x_off as i32).min().unwrap_or(0).min(0i32) / scale_div as i32;
    let y_base =
        frames.iter().map(|x| x.x_off as i32).min().unwrap_or(0).min(0i32) / scale_div as i32;
    let x_max = frames.iter().map(|x| (x.x_off as i32 + x.width as i32) / scale_div as i32)
        .max().unwrap_or(1);
    let y_max = frames.iter().map(|x| (x.y_off as i32 + x.height as i32) / scale_div as i32)
        .max().unwrap_or(1);
    let out_width = (x_max.max(values.width as i32 / scale_div as i32) - x_base) as u32;
    let out_height = (y_max.max(values.height as i32 / scale_div as i32) - y_base) as u32;
    for (i, prefix) in enum_prefixes {
        let texture = file.texture(i)?;
        for (n, frame) in frames.iter().enumerate() {
            let path = path.join(format!("{}_{:03}.png", prefix, n));
            let out = File::create(&path)
                .with_context(|_| format!("Unable to create {}", path.to_string_lossy()))?;
            let mut out = BufWriter::new(out);

            let tex_x = frame.tex_x / scale_div as u16;
            let tex_y = frame.tex_y / scale_div as u16;
            let frame_width = frame.width as u32 / scale_div;
            let frame_height = frame.height as u32 / scale_div;

            let blank_left = u32::try_from(frame.x_off as i32 / scale_div as i32 - x_base)?;
            let blank_top = u32::try_from(frame.y_off as i32 / scale_div as i32 - y_base)?;
            let blank_right = out_width - (blank_left + frame_width);
            let blank_bottom = out_height - (blank_top + frame_height);

            let mut bytes = Vec::with_capacity((out_width * out_height * 4) as usize);
            bytes.extend((0..blank_top * out_width).flat_map(|_| [0, 0, 0, 0].iter().cloned()));
            for row in 0..(out_height - blank_top - blank_bottom) {
                let tex_start = (
                    (tex_y as u32 + row) * texture.width + tex_x as u32
                ) as usize * 4;
                let image_row = texture.data.get(tex_start..tex_start + frame_width as usize * 4);
                let image_row = match image_row {
                    Some(s) => s,
                    None => return Err(format_err!("Bad frame data for frame {}", n)),
                };
                bytes.extend((0..blank_left).flat_map(|_| [0, 0, 0, 0].iter().cloned()));
                bytes.extend_from_slice(image_row);
                bytes.extend((0..blank_right).flat_map(|_| [0, 0, 0, 0].iter().cloned()));
            }
            bytes.extend(
                (0..blank_bottom * out_width).flat_map(|_| [0, 0, 0, 0].iter().cloned())
            );

            let mut encoder = png::Encoder::new(out, out_width, out_height);
            encoder.set(png::ColorType::RGBA);
            let mut encoder = encoder.write_header()?;
            encoder.write_image_data(&bytes)?;
        }
    }

    let mut frame_info_file = File::create(&path.join(framedef_file))
        .context("Can't create the frame info file")?;
    let mut frame_info = FrameInfo {
        frame_count: frames.len() as u32,
        offset_x: x_base,
        offset_y: y_base,
        layers: layer_prefixes.iter().enumerate()
            .filter_map(|(i, l)| l.as_ref().map(|x| (i, x)))
            .map(|(i, prefix)| (i as u32, prefix.clone()))
            .collect(),
        frame_types: Vec::new(),
    };
    let mut start = 0;
    let mut first_unk = frames.get(0).map(|x| x.unknown).unwrap_or(0);
    for (i, f) in frames.iter().enumerate() {
        if f.unknown != first_unk {
            frame_info.frame_types.push(FrameType {
                first_frame: start as u32,
                last_frame: i as u32,
                frame_type: first_unk,
            });
            start = i + 1;
            first_unk = frames.get(start).map(|x| x.unknown).unwrap_or(0);
        }
    }
    if start < frames.len() {
        frame_info.frame_types.push(FrameType {
            first_frame: start as u32,
            last_frame: frames.len() as u32 - 1,
            frame_type: first_unk,
        });
    }
    serde_json::to_writer_pretty(&mut frame_info_file, &frame_info)?;

    Ok(())
}

fn import_frames_grp(
    files: &mut files::Files,
    dir: &Path,
    filename_prefix: &str,
    format: anim::TextureFormat,
    sprite: usize,
    ty: SpriteType,
) -> Result<u32, Error> {
    use png::HasParameters;

    let mut frame_count = 0;
    let mut frames = Vec::new();
    for i in 0.. {
        let path = dir.join(format!("{}_{:03}.png", filename_prefix, i));
        if !path.is_file() {
            frame_count = i;
            break;
        }

        let file = File::open(&path)
            .with_context(|_| format!("Unable to open {}", path.to_string_lossy()))?;
        let mut decoder = png::Decoder::new(file);
        decoder.set(png::Transformations::IDENTITY);
        let (info, mut reader) = decoder.read_info()
            .with_context(|_| format!("Unable to read {}", path.to_string_lossy()))?;
        let mut buf = vec![0; info.buffer_size()];
        reader.next_frame(&mut buf)
            .with_context(|_| format!("Unable to read {}", path.to_string_lossy()))?;
        let data = anim_encoder::encode(&buf, info.width, info.height, format);
        frames.push((ddsgrp::Frame {
            unknown: 0,
            width: u16::try_from(info.width)
                .map_err(|_| format_err!("Frame {} width too large", i))?,
            height: u16::try_from(info.height)
                .map_err(|_| format_err!("Frame {} width too large", i))?,
            size: data.len() as u32,
            offset: !0,
        }, data));
    }
    files.set_grp_changes(sprite, ty, frames);
    Ok(frame_count)
}

fn import_frames(
    files: &mut files::Files,
    frame_info: &FrameInfo,
    dir: &Path,
    formats: &[anim::TextureFormat],
    sprite: usize,
    ty: SpriteType,
) -> Result<(), Error> {
    use png::HasParameters;

    let mut encoder = anim_encoder::AnimEncoder::new();
    for &(i, ref layer_prefix) in &frame_info.layers {
        let layer = i as usize;
        let format = formats.get(layer)
            .ok_or_else(|| format_err!("No layer format for layer {}", layer))?
            .clone();
        encoder.layer_format(layer, format);
        for f in 0..frame_info.frame_count {
            let path = dir.join(format!("{}_{:03}.png", layer_prefix, f));
            let file = File::open(&path)
                .with_context(|_| format!("Unable to open {}", path.to_string_lossy()))?;
            let mut decoder = png::Decoder::new(file);
            decoder.set(png::Transformations::IDENTITY);
            let (info, mut reader) = decoder.read_info()
                .with_context(|_| format!("Unable to read {}", path.to_string_lossy()))?;
            let mut buf = vec![0; info.buffer_size()];
            reader.next_frame(&mut buf)
                .with_context(|_| format!("Unable to read {}", path.to_string_lossy()))?;
            let mut bounded = rgba_bounding_box(&buf, info.width, info.height);
            bounded.coords.x_offset = bounded.coords.x_offset.saturating_add(frame_info.offset_x);
            bounded.coords.y_offset = bounded.coords.y_offset.saturating_add(frame_info.offset_y);
            encoder.add_frame(layer, f as usize, bounded.data, bounded.coords);
        }
    }
    let mut changes = encoder.finish();

    let scale_mul = match ty {
        SpriteType::Hd2 => 2u16,
        _ => 1,
    };
    for f in &mut changes.frames {
        f.tex_x *= scale_mul;
        f.tex_y *= scale_mul;
        f.x_off *= scale_mul as i16;
        f.y_off *= scale_mul as i16;
        f.width *= scale_mul;
        f.height *= scale_mul;
    }
    for ty in &frame_info.frame_types {
        for f in ty.first_frame..ty.last_frame + 1 {
            if let Some(f) = changes.frames.get_mut(f as usize) {
                f.unknown = ty.frame_type;
            }
        }
    }
    files.set_tex_changes(sprite, ty, changes);
    Ok(())
}

struct Bounded {
    data: Vec<u8>,
    coords: anim_encoder::FrameCoords,
}

fn rgba_bounding_box(data: &[u8], width: u32, height: u32) -> Bounded {
    assert_eq!(data.len(), 4 * (width * height) as usize);
    let top = match data.chunks(width as usize * 4)
        .position(|x| !x.chunks(4).all(|x| x[3] == 0))
    {
        Some(s) => s as u32,
        None => return Bounded {
            data: vec![],
            coords: anim_encoder::FrameCoords {
                x_offset: 0,
                y_offset: 0,
                width: 0,
                height: 0,
            },
        },
    };
    let bottom = height - data.chunks(width as usize * 4).rev()
        .position(|x| !x.chunks(4).all(|x| x[3] == 0)).unwrap() as u32;
    let left = (0..width)
        .find(|x| !(top..bottom).all(|y| data[(y * width + x) as usize * 4 + 3] == 0))
        .unwrap();
    let right = 1 + (0..width).rev()
        .find(|x| !(top..bottom).all(|y| data[(y * width + x) as usize * 4 + 3] == 0))
        .unwrap();
    let out_width = right - left;
    let out_height = bottom - top;
    let mut out = vec![0; (out_width * out_height) as usize * 4];
    let mut out_pos = 0;
    let mut in_pos = (top * width + left) as usize * 4;
    for _ in 0..out_height {
        (&mut out[out_pos..out_pos + out_width as usize * 4])
            .copy_from_slice(&data[in_pos..in_pos + out_width as usize * 4]);
        out_pos += out_width as usize * 4;
        in_pos += width as usize * 4;
    }
    Bounded {
        data: out,
        coords: anim_encoder::FrameCoords {
            x_offset: left as i32,
            y_offset: top as i32,
            width: out_width,
            height: out_height,
        },
    }
}

#[test]
fn test_rgba_bounding_box() {
    let data = vec![1; 40 * 70 * 4];
    let result = rgba_bounding_box(&data, 40, 70);
    assert_eq!(result.coords.x_offset, 0);
    assert_eq!(result.coords.y_offset, 0);
    assert_eq!(result.coords.width, 40);
    assert_eq!(result.coords.height, 70);

    let mut data = vec![0; 40 * 70 * 4];
    data[4 * (32 * 40 + 35) + 0] = 5;
    data[4 * (32 * 40 + 35) + 1] = 6;
    data[4 * (32 * 40 + 35) + 2] = 7;
    data[4 * (32 * 40 + 35) + 3] = 8;
    let result = rgba_bounding_box(&data, 40, 70);
    assert_eq!(result.coords.x_offset, 35);
    assert_eq!(result.coords.y_offset, 32);
    assert_eq!(result.coords.width, 1);
    assert_eq!(result.coords.height, 1);
    assert_eq!(result.data, &[5, 6, 7, 8]);

    data[4 * (2 * 40 + 5) + 0] = 50;
    data[4 * (2 * 40 + 5) + 1] = 60;
    data[4 * (2 * 40 + 5) + 2] = 70;
    data[4 * (2 * 40 + 5) + 3] = 80;
    let result = rgba_bounding_box(&data, 40, 70);
    assert_eq!(result.coords.x_offset, 5);
    assert_eq!(result.coords.y_offset, 2);
    assert_eq!(result.coords.width, 31);
    assert_eq!(result.coords.height, 31);
    let out_len = result.data.len();
    assert_eq!(&result.data[..4], &[50, 60, 70, 80]);
    assert_eq!(&result.data[out_len - 4..], &[5, 6, 7, 8]);
    for &x in &result.data[4..out_len - 4] {
        assert_eq!(x, 0);
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

#[derive(Clone, Serialize, Deserialize)]
struct FrameType {
    first_frame: u32,
    last_frame: u32,
    frame_type: u32,
}

#[derive(Clone, Serialize, Deserialize)]
struct FrameInfo {
    frame_count: u32,
    offset_x: i32,
    offset_y: i32,
    layers: Vec<(u32, String)>,
    frame_types: Vec<FrameType>
}

fn parse_frame_info(path: &Path) -> Result<FrameInfo, Error> {
    let mut file = File::open(path)?;
    Ok(serde_json::from_reader(&mut file)?)
}
