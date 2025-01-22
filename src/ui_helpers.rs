use gtk::{glib};
use gtk::prelude::*;

use crate::frame_export_dialog::SavedCheckbox;

pub trait BoxableWidget {
    fn expands(&self) -> bool {
        false
    }

    fn padding(&self) -> u32 {
        0
    }

    fn widget(&self) -> &gtk::Widget;
}

impl<T: IsA<gtk::Widget> + glib::object::Cast> BoxableWidget for T {
    fn widget(&self) -> &gtk::Widget {
        self.upcast_ref()
    }
}

pub struct Expand<'a, T: BoxableWidget>(&'a T);

impl<'a, T: BoxableWidget> BoxableWidget for Expand<'a, T> {
    fn expands(&self) -> bool {
        true
    }

    fn widget(&self) -> &gtk::Widget {
        self.0.widget()
    }
}

pub fn box_expand<T: BoxableWidget>(widget: &T) -> Expand<'_, T> {
    Expand(widget)
}

pub struct AddPadding<'a, T: BoxableWidget>(&'a T, u32);

impl<'a, T: BoxableWidget> BoxableWidget for AddPadding<'a, T> {
    fn padding(&self) -> u32 {
        self.1
    }

    fn widget(&self) -> &gtk::Widget {
        self.0.widget()
    }
}

pub fn box_add_padding<T: BoxableWidget>(widget: &T, padding: u32) -> AddPadding<'_, T> {
    AddPadding(widget, padding)
}

pub fn box_vertical(widgets: &[&dyn BoxableWidget]) -> gtk::Box {
    use gtk::prelude::WidgetExt;
    let bx = gtk::Box::new(gtk::Orientation::Vertical, 5);
    for widget in widgets {
        let expand = widget.expands();
        if expand {
            widget.widget().set_vexpand(true);
        }
        bx.append(widget.widget());
    }
    bx
}

pub fn box_horizontal(widgets: &[&dyn BoxableWidget]) -> gtk::Box {
    use gtk::prelude::WidgetExt;
    let bx = gtk::Box::new(gtk::Orientation::Horizontal, 5);
    for widget in widgets {
        let expand = widget.expands();
        if expand {
            widget.widget().set_hexpand(true);
        }
        bx.append(widget.widget());
    }
    bx
}

pub fn pane_horizontal(first: &dyn BoxableWidget, second: &dyn BoxableWidget) -> gtk::Paned {
    use gtk::prelude::WidgetExt;
    let pane = gtk::Paned::new(gtk::Orientation::Horizontal);
    if first.expands() {
        first.widget().set_hexpand(true);
    }
    if second.expands() {
        second.widget().set_hexpand(true);
    }
    pane.set_start_child(Some(first.widget()));
    pane.set_end_child(Some(second.widget()));
    pane
}

pub trait WidgetExt {
    fn tooltip(&self, tip: &str) -> &Self;
}

impl<T: IsA<gtk::Widget> + glib::object::Cast> WidgetExt for T {
    fn tooltip(&self, tip: &str) -> &Self {
        use gtk::prelude::*;
        self.set_tooltip_text(Some(tip));
        self
    }
}

#[derive(Clone)]
pub struct CheckEnabledSection {
    check: SavedCheckbox,
    bx: gtk::Box,
}

impl CheckEnabledSection {
    pub fn widget(&self) -> &gtk::Widget {
        self.bx.upcast_ref()
    }

    pub fn is_active(&self) -> bool {
        self.check.is_active()
    }
}

pub fn label_section_with_enable_check<O: IsA<gtk::Widget>>(
    name: &str,
    obj: &O,
    check_save_key: &str,
    check_default_value: bool,
) -> CheckEnabledSection {
    use gtk::prelude::*;
    let check = SavedCheckbox::new_with_default(
        check_save_key,
        name,
        check_default_value,
    );
    let frame = gtk::Frame::new(None);
    obj.set_margin_top(obj.margin_top() + 2);
    obj.set_margin_bottom(obj.margin_bottom() + 2);
    obj.set_margin_start(obj.margin_start() + 2);
    obj.set_margin_end(obj.margin_end() + 2);
    frame.set_child(Some(obj));

    let frame2 = frame.clone();
    let check2 = check.clone();
    check.connect_toggled(move || {
        frame2.set_sensitive(check2.is_active());
    });

    let bx = gtk::Box::new(gtk::Orientation::Vertical, 0);
    bx.append(check.widget());
    bx.append(&frame);
    CheckEnabledSection {
        check,
        bx,
    }
}
