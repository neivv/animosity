use std::fmt;
use std::sync::Arc;

use anyhow::Error;

#[derive(Clone)]
pub struct ArcError(Arc<Error>);

impl From<Error> for ArcError {
    fn from(val: Error) -> ArcError {
        ArcError(Arc::new(val))
    }
}

impl fmt::Debug for ArcError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for ArcError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl std::error::Error for ArcError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.source()
    }
}
