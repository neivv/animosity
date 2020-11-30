use std::cell::RefCell;
use std::rc::Rc;

use gtk;
use gtk::prelude::*;

use crate::combo_box_enum::ComboBoxEnum;
use crate::ui_helpers::*;
use crate::label_section;

pub struct RenderSettingsWidget {
    root: gtk::Box,
    settings: RefCell<RenderSettings>,
}

#[derive(Copy, Clone)]
pub struct RenderSettings {
    pub decode_normal: bool,
    pub ao_depth_mode: AoDepth,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum AoDepth {
    Raw,
    Ao,
    Depth,
}

impl RenderSettingsWidget {
    pub fn new() -> Rc<RenderSettingsWidget> {
        let normal_decode = gtk::CheckButton::with_label("Decode normal layer");
        let ao_depth = ComboBoxEnum::new(&[
            (AoDepth::Raw, "Raw"),
            (AoDepth::Ao, "Ambient Occlusion"),
            (AoDepth::Depth, "Depth"),
        ]);
        ao_depth.set_active(&AoDepth::Raw);
        let ao_depth_mode = label_section("ao_depth mode", ao_depth.widget());
        normal_decode.set_valign(gtk::Align::Start);
        normal_decode.set_tooltip_text(Some("\
            When checked, unpacks data from normal layer to x/y/z components and displays \
            them as a normal map image.\n\
            See readme.txt for details on how the data is encoded."));
        ao_depth_mode.set_tooltip_text(Some("\
            Selects how ao_depth layer is displayed.\n\
            - Raw: Displays raw pixels of the texture\n\
            - Ambient Occlusion: Displays only AO data\n\
            - Depth: Displays only depth data\n\
            See readme.txt for details on how the data is encoded."));
        let bx = box_horizontal(&[
            &normal_decode,
            &ao_depth_mode,
        ]);
        let root = label_section("Rendering settings", &bx);
        let this = Rc::new(RenderSettingsWidget {
            root,
            settings: RefCell::new(RenderSettings {
                decode_normal: false,
                ao_depth_mode: AoDepth::Raw,
            }),
        });
        let this2 = this.clone();
        ao_depth.connect_changed(move |new| {
            if let Some(new) = new {
                this2.settings.borrow_mut().ao_depth_mode = new;
                crate::ui().info.draw_area.queue_draw();
            }
        });
        let this2 = this.clone();
        normal_decode.connect_toggled(move |s| {
            this2.settings.borrow_mut().decode_normal = s.get_active();
            crate::ui().info.draw_area.queue_draw();
        });

        this
    }

    pub fn widget(&self) -> &gtk::Widget {
        self.root.upcast_ref()
    }

    pub fn settings(&self) -> RenderSettings {
        self.settings.borrow().clone()
    }
}

