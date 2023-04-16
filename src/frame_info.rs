use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use anyhow::{anyhow, Context};
use serde_derive::{Serialize, Deserialize};
use serde::Deserialize;

use crate::Error;

#[derive(Clone, Serialize)]
pub struct FrameInfo {
    pub frame_count: u32,
    pub offset_x: i32,
    pub offset_y: i32,
    pub layers: Vec<Layer>,
    pub frame_types: Vec<FrameType>,
    pub multi_frame_images: Vec<MultiFrameImage>,
}

#[derive(Clone, Deserialize)]
pub struct FrameInfoDeserialize {
    pub frame_count: u32,
    pub offset_x: i32,
    pub offset_y: i32,
    pub layers: Vec<serde_json::Value>,
    pub frame_types: Vec<FrameType>,
    #[serde(default)]
    pub multi_frame_images: Vec<MultiFrameImage>,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Layer {
    pub id: u32,
    /// Only meaningful for ao_depth; 0 = ao, 1 = depth if encoding is SingleChannel
    /// Otherwise 0 is the entire layer.
    pub sub_id: u32,
    pub filename_prefix: String,
    pub encoding: LayerEncoding,
    // Will use filename_prefix when not set
    #[serde(default)]
    pub name: String,
}

#[derive(Eq, PartialEq, Copy, Clone, Serialize, Deserialize)]
pub enum LayerEncoding {
    Raw,
    Normal,
    SingleChannel,
}

pub fn parse_frame_info(path: &Path) -> Result<FrameInfo, Error> {
    let mut file = File::open(path)?;
    parse_from_reader(&mut file)
}

fn layer_name_from_prefix(prefix: &str) -> String {
    if prefix.ends_with("ao_depth") {
        String::from("ao_depth")
    } else {
        String::from(prefix.rsplit_once('_').map(|x| x.1).unwrap_or(prefix))
    }
}

fn parse_from_reader<R: Read>(r: &mut R) -> Result<FrameInfo, Error> {
    // layers used to be (u32, String) instead of Layer,
    // accept both
    let base: FrameInfoDeserialize = serde_json::from_reader(r)?;
    let layers = base.layers.iter().enumerate().map(|(i, x)| {
        fn parse(x: &serde_json::Value) -> Result<Layer, Error> {
            match x {
                serde_json::Value::Array(ref vals) => {
                    if vals.len() != 2 {
                        Err(anyhow!("Layer array must have 2 values"))
                    } else {
                        let id = u32::deserialize(&vals[0])?;
                        let filename_prefix = String::deserialize(&vals[1])?;
                        let name = layer_name_from_prefix(&filename_prefix);
                        Ok(Layer {
                            id,
                            sub_id: 0,
                            filename_prefix,
                            encoding: LayerEncoding::Raw,
                            name,
                        })
                    }
                }
                serde_json::Value::Object(..) => {
                    Ok(Layer::deserialize(x)?)
                }
                _ => Err(anyhow!("Layer must be array or object")),
            }
        }
        let mut layer = parse(x)
            .with_context(|| anyhow!("Layer {}", i))?;
        if layer.name.is_empty() {
            layer.name = layer_name_from_prefix(&layer.filename_prefix);
        }
        Ok(layer)
    }).collect::<Result<Vec<_>, Error>>()?;
    Ok(FrameInfo {
        frame_count: base.frame_count,
        offset_x: base.offset_x,
        offset_y: base.offset_y,
        layers,
        frame_types: base.frame_types,
        multi_frame_images: base.multi_frame_images,
    })
}

#[derive(Clone, Serialize, Deserialize)]
pub struct FrameType {
    pub first_frame: u32,
    pub last_frame: u32,
    pub frame_type: u32,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct MultiFrameImage {
    pub layer: u32,
    // For ao_depth split
    #[serde(default)]
    pub sublayer: u32,
    pub first_frame: u32,
    pub frame_count: u32,
    pub path: String,
    pub frame_width: u32,
    pub frame_height: u32,
    // Only relevant to grps
    #[serde(default)]
    pub frame_size_overrides: HashMap<u32, (u32, u32)>,
}

#[test]
fn backwards_compat() {
    let text = r#"{
  "frame_count": 230,
  "offset_x": -11,
  "offset_y": 0,
  "layers": [
    [
      0,
      "944_hd2_diffuse"
    ],
    [
      1,
      "944_hd2_bright"
    ],
    [
      2,
      "944_hd2_teamcolor"
    ],
    [
      3,
      "944_hd2_emissive"
    ],
    [
      4,
      "944_hd2_normal"
    ],
    [
      5,
      "944_hd2_specular"
    ],
    [
      6,
      "944_hd2_ao_depth"
    ]
  ],
  "frame_types": [
    {
      "first_frame": 0,
      "last_frame": 229,
      "frame_type": 1
    }
  ],
  "multi_frame_images": [
    {
      "layer": 0,
      "first_frame": 0,
      "frame_count": 230,
      "path": "C:\\Jotakin\\src\\animosity\\reimport_tmp\\944_hd2_diffuse.png",
      "frame_width": 161,
      "frame_height": 133,
      "frame_size_overrides": {}
    },
    {
      "layer": 1,
      "first_frame": 0,
      "frame_count": 230,
      "path": "C:\\Jotakin\\src\\animosity\\reimport_tmp\\944_hd2_bright.png",
      "frame_width": 161,
      "frame_height": 133,
      "frame_size_overrides": {}
    },
    {
      "layer": 2,
      "first_frame": 0,
      "frame_count": 230,
      "path": "C:\\Jotakin\\src\\animosity\\reimport_tmp\\944_hd2_teamcolor.png",
      "frame_width": 161,
      "frame_height": 133,
      "frame_size_overrides": {}
    },
    {
      "layer": 3,
      "first_frame": 0,
      "frame_count": 230,
      "path": "C:\\Jotakin\\src\\animosity\\reimport_tmp\\944_hd2_emissive.png",
      "frame_width": 161,
      "frame_height": 133,
      "frame_size_overrides": {}
    },
    {
      "layer": 4,
      "first_frame": 0,
      "frame_count": 230,
      "path": "C:\\Jotakin\\src\\animosity\\reimport_tmp\\944_hd2_normal.png",
      "frame_width": 161,
      "frame_height": 133,
      "frame_size_overrides": {}
    },
    {
      "layer": 5,
      "first_frame": 0,
      "frame_count": 230,
      "path": "C:\\Jotakin\\src\\animosity\\reimport_tmp\\944_hd2_specular.png",
      "frame_width": 161,
      "frame_height": 133,
      "frame_size_overrides": {}
    },
    {
      "layer": 6,
      "first_frame": 0,
      "frame_count": 230,
      "path": "C:\\Jotakin\\src\\animosity\\reimport_tmp\\944_hd2_ao_depth.png",
      "frame_width": 161,
      "frame_height": 133,
      "frame_size_overrides": {}
    }
  ]
}"#;
    let result = parse_from_reader(&mut text.as_bytes()).unwrap();
    assert_eq!(result.frame_count, 230);
    assert_eq!(result.offset_x, -11);
    assert_eq!(result.offset_y, 0);
    assert_eq!(result.layers.len(), 7);
    assert_eq!(result.multi_frame_images.len(), 7);
    assert_eq!(result.frame_types.len(), 1);
    assert_eq!(result.layers[4].name, "normal");
    assert_eq!(result.layers[6].name, "ao_depth");
}
