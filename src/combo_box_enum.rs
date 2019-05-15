use gtk;
use gtk::prelude::*;

pub struct ComboBoxEnum<E: Copy + Clone + Eq + PartialEq + 'static> {
    combo_box: gtk::ComboBoxText,
    cases: &'static [(E, &'static str)],
}

impl<E: Copy + Clone + Eq + PartialEq + 'static> ComboBoxEnum<E> {
    pub fn new(cases: &'static [(E, &'static str)]) -> ComboBoxEnum<E> {
        let combo_box = gtk::ComboBoxText::new();
        for case in cases {
            combo_box.append_text(case.1);
        }
        ComboBoxEnum {
            combo_box,
            cases,
        }
    }

    pub fn clear_active(&self) {
        self.combo_box.set_active(None);
    }

    pub fn set_active(&self, value: &E) {
        if let Some(i) = self.cases.iter().enumerate().find(|x| (x.1).0 == *value).map(|x| x.0) {
            self.combo_box.set_active(i as u32);
        }
    }

    pub fn get_active(&self) -> Option<E> {
        self.combo_box.get_active().and_then(|x| self.cases.get(x as usize)).map(|x| x.0)
    }

    pub fn widget(&self) -> &gtk::Widget {
        self.combo_box.upcast_ref()
    }

    pub fn set_sensitive(&self, yes: bool) {
        self.combo_box.set_sensitive(yes)
    }
}
