extern crate byteorder;
extern crate cairo;
extern crate ddsfile;
#[macro_use] extern crate failure;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate log;
extern crate gio;
extern crate glib;
#[macro_use] extern crate glium;
extern crate gtk;

mod anim;
mod gl;
mod files;

use std::cell::RefCell;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::rc::Rc;

use gio::prelude::*;
use gtk::prelude::*;

use failure::Error;
use glium::backend::glutin::headless::Headless;
use glium::framebuffer::SimpleFrameBuffer;
use glium::texture::{self, Texture2d};

use files::SpriteFiles;

fn main() {
    let app = gtk::Application::new("a.b", gio::ApplicationFlags::empty())
        .unwrap_or_else(|e| panic!("Couldn't create app: {}", e));
    app.connect_startup(|app| {
        let ui = create_ui(app);
        create_actions(app, &ui.main_window.clone().upcast());
        ui.main_window.show_all();
        UI.with(|x| {
            *x.borrow_mut() = Some(Rc::new(ui));
        })
    });
    app.connect_activate(|_| {
    });
    app.run(&[]);
}

struct State {
    files: Arc<Mutex<files::Files>>,
}

struct Ui {
    main_window: gtk::ApplicationWindow,
    list: SpriteList,
    info: Arc<SpriteInfo>,
}

lazy_static! {
    static ref STATE: Mutex<State> = Mutex::new(State {
        files: Arc::new(Mutex::new(files::Files::empty())),
    });
}

thread_local! {
    static UI: RefCell<Option<Rc<Ui>>> = RefCell::new(None);
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
        self.list.store.clear();
        for sprite in files.sprites() {
            let name = match *sprite {
                SpriteFiles::AnimSet(ref s) => (&*s.name).into(),
                SpriteFiles::SingleFile(ref p) => (&*p.to_string_lossy()).into(),
                SpriteFiles::MainSdOnly { ref name, .. } => (&*name).into(),
            };
            let iter = self.list.store.append();
            self.list.store.set_value(&iter, 0, &name);
        }
        self.list.list.columns_autosize();
    }
}

struct SpriteList {
    root: gtk::ScrolledWindow,
    list: gtk::TreeView,
    store: gtk::ListStore,
    linked_info: Arc<SpriteInfo>,
}

impl SpriteList {
    fn new(linked_info: Arc<SpriteInfo>) -> SpriteList {
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
        root.set_min_content_width(80);
        root.set_overlay_scrolling(false);

        let info = linked_info.clone();
        list.connect_cursor_changed(move |s| {
            let sprite = s.get_selection().get_selected()
                .and_then(|(model, iter)| model.get_path(&iter))
                .and_then(|path| path.get_indices().get(0).cloned());
            if let Some(index) = sprite {
                info.select_sprite(index as usize);
            }
        });
        SpriteList {
            root,
            list,
            store,
            linked_info,
        }
    }

    fn widget(&self) -> gtk::Widget {
        self.root.clone().upcast()
    }
}

struct SpriteSelector {
    bx: gtk::Box,
    sd: gtk::RadioButton,
    hd: gtk::RadioButton,
    hd2: gtk::RadioButton,
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum SpriteType {
    Sd,
    Hd,
    Hd2,
}

impl SpriteSelector {
    fn new() -> SpriteSelector {
        let bx = gtk::Box::new(gtk::Orientation::Vertical, 0);
        let sd = gtk::RadioButton::new_with_label("SD");
        let hd = gtk::RadioButton::new_with_label_from_widget(&sd, "HD");
        let hd2 = gtk::RadioButton::new_with_label_from_widget(&sd, "HD2");
        sd.set_action_name(Some("sprite.select_sd"));
        hd.set_action_name(Some("sprite.select_hd"));
        hd2.set_action_name(Some("sprite.select_hd2"));
        bx.pack_start(&sd, false, false, 0);
        bx.pack_start(&hd, false, false, 0);
        bx.pack_start(&hd2, false, false, 0);
        SpriteSelector {
            bx,
            sd,
            hd,
            hd2,
        }
    }

    fn widget(&self) -> gtk::Widget {
        self.bx.clone().upcast()
    }
}

struct SpriteInfo {
    bx: gtk::Box,
    file_list: gtk::TextBuffer,
    files: Arc<Mutex<files::Files>>,
    sprite_actions: gio::ActionMap,
    sprite_index: AtomicUsize,
    selector: SpriteSelector,
    selected_type: Mutex<SpriteType>,
    draw_area: gtk::DrawingArea,
}

fn lookup_action(group: &gio::ActionMap, name: &str) -> Option<gio::SimpleAction> {
    group.lookup_action(name).and_then(|x| x.downcast::<gio::SimpleAction>().ok())
}

struct DrawParams {
    vertices: glium::vertex::VertexBuffer<gl::Vertex>,
    indices: glium::index::IndexBuffer<u32>,
    program: glium::program::Program,
    cached_textures: Vec<(Texture2d, usize, SpriteType, usize)>,
}

fn sprite_render_program(gl: &mut gl::Context) -> glium::program::Program {
    glium::program::Program::from_source(
        gl.facade(),
        r#"
            #version 130

            in vec2 pos;
            in vec2 tex;
            out vec2 v_tex_coords;

            uniform mat4 transform;

            void main() {
                gl_Position = transform * vec4(pos.xy, 0.0, 1.0);
                v_tex_coords = tex;
            }
        "#,
        r#"
            #version 130

            in vec2 v_tex_coords;
            out vec4 color;

            uniform sampler2D tex;

            void main() {
                color = texture(tex, v_tex_coords);
            }
        "#,
        None,
    ).expect("GL program creation failed")
}

impl SpriteInfo {
    fn new(file_shared: Arc<Mutex<files::Files>>) -> Arc<SpriteInfo> {
        let bx = gtk::Box::new(gtk::Orientation::Vertical, 0);
        let sprite_actions = gio::SimpleActionGroup::new();
        bx.insert_action_group("sprite", Some(&sprite_actions));
        let sprite_bx = gtk::Box::new(gtk::Orientation::Horizontal, 0);
        let selector = SpriteSelector::new();
        let draw_area = gtk::DrawingArea::new();
        sprite_bx.pack_start(&selector.widget(), false, false, 0);
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
            sprite_actions: sprite_actions.upcast(),
            sprite_index: AtomicUsize::new(!0),
            selector,
            selected_type: Mutex::new(SpriteType::Sd),
            draw_area: draw_area.clone(),
        });
        SpriteInfo::create_sprite_actions(&result, &result.sprite_actions);

        let this = result.clone();
        let gl: Arc<Mutex<Option<(gl::Context, DrawParams)>>> = Arc::new(Mutex::new(None));
        draw_area.connect_draw(move |s, cairo| {
            let mut gl = gl.lock().unwrap();;
            let rect = s.get_allocation();
            println!("RECT {:?}", rect);
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
                    glium::index::PrimitiveType::TrianglesList,
                    &[0, 1, 2, 1, 3, 2],
                ).expect("Unable to create index buffer");
                let program = sprite_render_program(&mut gl);
                (gl, DrawParams {
                    vertices,
                    indices,
                    program,
                    cached_textures: Vec::new(),
                })
            });
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
                        // TODO: This is premultiplied alpha, is the glium format too?
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

    fn sprite_texture<'a>(
        &self,
        facade: &Headless,
        cached_textures: &'a mut Vec<(Texture2d, usize, SpriteType, usize)>,
    ) -> Result<Option<&'a Texture2d>, Error> {
        let index = self.sprite_index.load(Ordering::SeqCst);
        let selected_type = self.selected_type.lock().unwrap().clone();
        let layer = 0;
        let cached = cached_textures.iter().position(|x| {
            x.1 == index && x.2 == selected_type && x.3 == layer
        });
        if let Some(index) = cached {
            Ok(Some(&cached_textures[index].0))
        } else {
            let image = {
                let mut files = self.files.lock().unwrap();
                match selected_type {
                    SpriteType::Sd => {
                        match files.mainsd_mut() {
                            Some(s) => Some(s.texture(index, layer)?),
                            None => None,
                        }
                    }
                    SpriteType::Hd => {
                        match files.hd(index)? {
                            Some(mut s) => Some(s.texture(layer)?),
                            None => None,
                        }
                    }
                    SpriteType::Hd2 => {
                        match files.hd2(index)? {
                            Some(mut s) => Some(s.texture(layer)?),
                            None => None,
                        }
                    }
                }
            };
            if let Some(image) = image {
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
                let clear = cached_textures.first().map(|x| x.1 != index).unwrap_or(false);
                if clear {
                    cached_textures.clear();
                }
                cached_textures.push((texture, index, selected_type, layer));
                Ok(Some(&cached_textures.last().unwrap().0))
            } else {
                Ok(None)
            }
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
        let texture = self.sprite_texture(facade, &mut draw_params.cached_textures)?;
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
            let uniforms = uniform! {
                transform: [
                    [scale_x,   0.0,        0.0,    0.0],
                    [0.0,       scale_y,    0.0,    0.0],
                    [0.0,       0.0,        1.0,    0.0],
                    [shift_x,   shift_y,    0.0,    1.0],
                ],
                tex: sampler,
            };
            buf.draw(
                &draw_params.vertices,
                &draw_params.indices,
                &draw_params.program,
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
            fun: F,
        ) -> gio::SimpleAction
        where F: Fn(&gio::SimpleAction, &Option<glib::Variant>) + 'static
        {
            let action = gio::SimpleAction::new(name, None);
            action.set_enabled(enabled);
            action.connect_activate(fun);
            group.add_action(&action);
            action
        }
        let s = this.clone();
        action(group, "select_sd", false, move |_, _| {
            *s.selected_type.lock().unwrap() = SpriteType::Sd;
            s.draw_area.queue_draw();
        });
        let s = this.clone();
        action(group, "select_hd", false, move |_, _| {
            *s.selected_type.lock().unwrap() = SpriteType::Hd;
            s.draw_area.queue_draw();
        });
        let s = this.clone();
        action(group, "select_hd2", false, move |_, _| {
            *s.selected_type.lock().unwrap() = SpriteType::Hd2;
            s.draw_area.queue_draw();
        });
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
            let files = self.files.lock().unwrap();
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
            SpriteFiles::SingleFile(_) => {
                self.set_enable_animset_actions(false);
                println!("TODO");
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
        item.set_attribute_value("accel", Some(&accel.to_variant()));
        item
    };

    let menu = gio::Menu::new();
    let file_menu = {
        let menu = gio::Menu::new();
        let file_actions = {
            let menu = gio::Menu::new();
            menu.append_item(&with_accel("_Open...", "app.open", "<Ctrl>O"));
            menu.append_item(&with_accel("_Save...", "app.save", "<Ctrl>S"));
            menu.append(Some("Save _As..."), Some("app.saveas"));
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
    menu.append_submenu(Some("_File"), &file_menu);
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
            let ui = ui();
            match files::Files::init(&filename) {
                Ok(f) => {
                    ui.files_changed(&f);
                    {
                        let state = STATE.lock().unwrap();
                        let mut files = state.files.lock().unwrap();
                        *files = f;
                    }
                }
                Err(e) => {
                    use std::fmt::Write;
                    let mut msg = format!("Unable to open file:\n");
                    for c in e.causes() {
                        writeln!(msg, "{}", c).unwrap();
                    }
                    ui.message(&msg);
                }
            }
        }
    });
}

fn open_file_dialog(parent: &gtk::Window) -> Option<PathBuf> {
    let dialog =
        gtk::FileChooserDialog::new(Some("Open..."), Some(parent), gtk::FileChooserAction::Open);
    dialog.set_select_multiple(false);
    let filter = gtk::FileFilter::new();
    filter.add_pattern("*.anim");
    filter.add_pattern("*.dds.grp");
    gtk::FileFilterExt::set_name(&filter, "Valid files");
    dialog.add_filter(&filter);
    dialog.add_button("Open", gtk::ResponseType::Accept.into());
    dialog.add_button("Cancel", gtk::ResponseType::Cancel.into());
    let result = dialog.run();
    let result = if result == gtk::ResponseType::Accept.into() {
        dialog.get_filename()
    } else {
        None
    };
    dialog.destroy();
    result
}

fn create_ui(app: &gtk::Application) -> Ui {
    app.set_menubar(Some(&create_menu()));

    let window = gtk::ApplicationWindow::new(app);

    let box1 = gtk::Box::new(gtk::Orientation::Horizontal, 0);
    let files = {
        STATE.lock().unwrap().files.clone()
    };
    let info = SpriteInfo::new(files);
    let list = SpriteList::new(info.clone());
    box1.pack_start(&list.widget(), false, false, 0);
    box1.pack_start(&info.widget(), true, true, 0);
    window.add(&box1);

    window.set_title(&format!("Animosity {}", env!("CARGO_PKG_VERSION")));
    window.resize(800, 600);
    Ui {
        main_window: window,
        list,
        info,
    }
}
