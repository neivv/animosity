use std::fs::File;
use std::path::Path;

use crate::Error;

#[derive(Clone, Serialize, Deserialize)]
pub struct FrameInfo {
    pub frame_count: u32,
    pub offset_x: i32,
    pub offset_y: i32,
    pub layers: Vec<(u32, String)>,
    pub frame_types: Vec<FrameType>
}

pub fn parse_frame_info(path: &Path) -> Result<FrameInfo, Error> {
    let mut file = File::open(path)?;
    Ok(serde_json::from_reader(&mut file)?)
}

#[derive(Clone, Serialize, Deserialize)]
pub struct FrameType {
    pub first_frame: u32,
    pub last_frame: u32,
    pub frame_type: u32,
}
