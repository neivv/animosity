use gtk;
use gtk::prelude::*;

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
    let bx = gtk::Box::new(gtk::Orientation::Vertical, 5);
    for widget in widgets {
        let expand = widget.expands();
        bx.pack_start(widget.widget(), expand, expand, widget.padding());
    }
    bx
}

pub fn box_horizontal(widgets: &[&dyn BoxableWidget]) -> gtk::Box {
    let bx = gtk::Box::new(gtk::Orientation::Horizontal, 5);
    for widget in widgets {
        let expand = widget.expands();
        bx.pack_start(widget.widget(), expand, expand, widget.padding());
    }
    bx
}

pub fn pane_horizontal(first: &dyn BoxableWidget, second: &dyn BoxableWidget) -> gtk::Paned {
    let pane = gtk::Paned::new(gtk::Orientation::Horizontal);
    pane.pack1(first.widget(), first.expands(), false);
    pane.pack2(second.widget(), second.expands(), false);
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
