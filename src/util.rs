use std::fmt::Debug;
use std::panic::Location;
use std::slice::SliceIndex;


pub trait SliceExt {
    /// Like get(), but logs a warning if returning None.
    /// For cases where panics aren't nice but still the
    /// index being out of bounds is programmer error.
    #[track_caller]
    fn should_get<S: SliceIndex<Self> + Clone + Debug>(&self, index: S) -> Option<&S::Output>;
    #[track_caller]
    fn should_get_mut<S: SliceIndex<Self> + Clone + Debug>(&mut self, index: S) -> Option<&mut S::Output>;
}

pub trait OptionExt {
    /// Logs warning if `None`, but returns `self` unchanged.
    #[track_caller]
    fn should(self) -> Self;
}

impl<T> SliceExt for [T] {
    fn should_get<S: SliceIndex<Self> + Clone + Debug>(&self, index: S) -> Option<&S::Output> {
        match self.get(index.clone()) {
            Some(s) => Some(s),
            None => {
                warn!(
                    "Index out of bounds at {}, index {:?}, length is {}",
                    Location::caller(), index, self.len(),
                );
                None
            }
        }
    }

    fn should_get_mut<S: SliceIndex<Self> + Clone + Debug>(&mut self, index: S) -> Option<&mut S::Output> {
        let len = self.len();
        match self.get_mut(index.clone()) {
            Some(s) => Some(s),
            None => {
                warn!(
                    "Index out of bounds at {}, index {:?}, length is {}",
                    Location::caller(), index, len,
                );
                None
            }
        }
    }
}

impl<T> OptionExt for Option<T> {
    fn should(self) -> Self {
        if self.is_none() {
            warn!("Option at {} is None", Location::caller());
        }
        self
    }
}
