use std::convert::TryInto;
use std::fmt;

use byteorder::{ByteOrder, LittleEndian};

#[derive(Debug)]
pub struct Error;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("GRP Decode Error")
    }
}

impl std::error::Error for Error {}

struct GrpHeader<'a> {
    width: u16,
    height: u16,
    x: u8,
    y: u8,
    frame_width: u16,
    frame_height: u16,
    frame_lines: &'a [u8],
}

pub struct GrpFrame {
    /// May be either paletted or RGBA depending on function called.
    pub data: Vec<u8>,
    pub left: u32,
    pub top: u32,
    pub right: u32,
    pub bottom: u32,
    pub width: u32,
    pub height: u32,
}

fn parse_header(grp: &[u8], frame: u16) -> Result<GrpHeader<'_>, Error> {
    if grp.len() < 6 {
        return Err(Error);
    }
    let frame_count = LittleEndian::read_u16(grp);
    let width = LittleEndian::read_u16(&grp[2..]);
    let height = LittleEndian::read_u16(&grp[4..]);
    if frame_count < frame {
        return Err(Error);
    }
    let frame_header = grp.get((6 + frame as usize * 8)..)
        .and_then(|x| x.get(..8))
        .ok_or_else(|| Error)?;
    let offset = LittleEndian::read_u32(&frame_header[4..]);
    let x = frame_header[0];
    let y = frame_header[1];
    let frame_width = match offset & 0x8000_0000 != 0 {
        true => frame_header[2] as u16 + 0x100,
        false => frame_header[2] as u16,
    };
    let frame_height = frame_header[3] as u16;
    let frame_lines = grp.get((offset & 0x7fff_ffff) as usize..)
        .ok_or_else(|| Error)?;
    Ok(GrpHeader {
        width,
        height,
        x,
        y,
        frame_width,
        frame_height,
        frame_lines,
    })
}

impl<'a> GrpHeader<'a> {
    /// callback is (pixel offset in width * height buffer), color
    /// x/y are relative to frame topleft, not sprite.
    fn decode<F: FnMut(usize, u8)>(&self, mut set_color: F) -> Result<(), Error> {
        let width = self.frame_width as usize;
        let mut line_start = self.x as usize + self.y as usize * self.width as usize;
        for y in 0..(self.frame_height as usize) {
            let line_offset = self.frame_lines.get(y.wrapping_mul(2)..)
                .and_then(|x| x.get(..2))
                .map(|x| LittleEndian::read_u16(x))
                .ok_or_else(|| Error)?;
            let mut line_data = self.frame_lines.get((line_offset as usize)..)
                .ok_or_else(|| Error)?;
            let mut x = 0;
            while x < width {
                let max_width = width - x;
                let (&byte, rest) = line_data.split_first()
                    .ok_or_else(|| Error)?;
                if byte & 0x80 != 0 {
                    // Transparent
                    x = x.wrapping_add((byte & 0x7f) as usize);
                    line_data = rest;
                } else if byte & 0x40 != 0 {
                    // memset
                    let length = (byte & 0x3f) as usize;
                    let length = max_width.min(length);
                    let (&color, rest) = rest.split_first()
                        .ok_or_else(|| Error)?;
                    let start = line_start.wrapping_add(x);
                    let end = start.wrapping_add(length);
                    for pos in start..end {
                        set_color(pos, color);
                    }
                    x = x.wrapping_add(length);
                    line_data = rest;
                } else {
                    // memcpy
                    let length = (byte & 0x3f) as usize;
                    let length = max_width.min(length);
                    let data = rest.get(..length)
                        .ok_or_else(|| Error)?;
                    let start = line_start.wrapping_add(x);
                    let end = start.wrapping_add(length);
                    for (pos, color) in (start..end).zip(data.iter().copied()) {
                        set_color(pos, color);
                    }

                    x = x.wrapping_add(length);
                    line_data = rest.get(length..)
                        .ok_or_else(|| Error)?;
                }
            }
            line_start = line_start.wrapping_add(self.width as usize);
        }
        Ok(())
    }
}

pub fn frame_count(grp: &[u8]) -> Result<u16, Error> {
    if grp.len() < 2 {
        Err(Error)
    } else {
        Ok(LittleEndian::read_u16(grp))
    }
}

pub fn width_height(grp: &[u8]) -> Result<(u16, u16), Error> {
    if grp.len() < 6 {
        return Err(Error);
    }
    let width = LittleEndian::read_u16(&grp[2..]);
    let height = LittleEndian::read_u16(&grp[4..]);
    Ok((width, height))
}

/// This differentiates between transparent 0/0/0/0 and whatever palette[0] is.
/// (Usually 0/0/0/255)
pub fn decode_grp_to_rgba(grp: &[u8], frame: u16, palette: &[u8]) -> Result<GrpFrame, Error> {
    let palette: &[u8; 0x400] = palette.try_into().map_err(|_| Error)?;

    let header = parse_header(grp, frame)?;
    let mut buffer = vec![0; header.width as usize * header.height as usize * 4];
    header.decode(|pos, color| {
        let pos = pos.wrapping_mul(4);
        if let Some(out) = buffer.get_mut(pos..(pos.wrapping_add(4))) {
            let color = &palette[(color as usize * 4)..][..4];
            out.copy_from_slice(color);
        }
    })?;
    Ok(GrpFrame {
        data: buffer,
        left: header.x.into(),
        top: header.y.into(),
        right: u32::from(header.x) + u32::from(header.frame_width),
        bottom: u32::from(header.y) + u32::from(header.frame_height),
        width: header.width.into(),
        height: header.height.into(),
    })
}

/// Returns (diffuse, teamcolor)
/// Teamcolor is also rgba (Even if just 255/255/255/255 or 0/0/0/0)
/// Diffuse teamcolor pixels get replaced by a grayscale pixel that roughly
/// is equal to teamcolor strength.
pub fn decode_grp_to_rgba_and_teamcolor(
    grp: &[u8],
    frame: u16,
    palette: &[u8],
) -> Result<(GrpFrame, Vec<u8>), Error> {
    let palette: &[u8; 0x400] = palette.try_into().map_err(|_| Error)?;

    let header = parse_header(grp, frame)?;
    let mut buffer = vec![0; header.width as usize * header.height as usize * 4];
    let mut teamcolor = vec![0; header.width as usize * header.height as usize * 4];
    header.decode(|pos, color| {
        let pos = pos.wrapping_mul(4);
        if let Some(out) = buffer.get_mut(pos..(pos.wrapping_add(4))) {
            if color >= 8 && color < 0x10 {
                // scale to 1.0, 8/9, 7/9, ..., 1/9 (No 0.0)
                let strength = (((1.0 - ((color.wrapping_sub(8)) as f32) / 9.0)) * 255.0) as u8;
                for i in 0..3 {
                    out[i] = strength;
                }
                out[3] = 255;
                if let Some(tc) = teamcolor.get_mut(pos..(pos.wrapping_add(4))) {
                    for i in 0..4 {
                        tc[i] = 255;
                    }
                }
            } else {
                let color = &palette[(color as usize * 4)..][..4];
                out.copy_from_slice(color);
            }
        }
    })?;
    Ok((GrpFrame {
        data: buffer,
        left: header.x.into(),
        top: header.y.into(),
        right: u32::from(header.x) + u32::from(header.frame_width),
        bottom: u32::from(header.y) + u32::from(header.frame_height),
        width: header.width.into(),
        height: header.height.into(),
    }, teamcolor))
}

pub fn decode_grp_to_paletted(grp: &[u8], frame: u16) -> Result<GrpFrame, Error> {
    let header = parse_header(grp, frame)?;
    let mut buffer = vec![0; header.width as usize * header.height as usize];
    header.decode(|pos, color| {
        if let Some(out) = buffer.get_mut(pos) {
            *out = color;
        }
    })?;
    Ok(GrpFrame {
        data: buffer,
        left: header.x.into(),
        top: header.y.into(),
        right: u32::from(header.x) + u32::from(header.frame_width),
        bottom: u32::from(header.y) + u32::from(header.frame_height),
        width: header.width.into(),
        height: header.height.into(),
    })
}

#[cfg(test)]
mod test {
    use super::*;

    static GRP: &[u8] = include_bytes!("../test_inputs/test.grp");

    #[test]
    fn test_decode_paletted() {
        let frame = decode_grp_to_paletted(GRP, 0).unwrap();
        let data = &[
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x80, 0x80, 0x80, 0x80, 0x00, 0x00,
            0x00, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00,
            0x00, 0x00, 0x80, 0x80, 0x80, 0x80, 0x00, 0x00,

            0x00, 0x00, 0x00, 0x80, 0x80, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x80, 0x80, 0x80, 0x80, 0x00, 0x00,
            0x00, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00,
            0x00, 0x00, 0x80, 0x80, 0x80, 0x80, 0x00, 0x00,
        ];
        assert_eq!(frame.left, 1);
        assert_eq!(frame.top, 1);
        assert_eq!(frame.right, 7);
        assert_eq!(frame.bottom, 8);
        assert_eq!(frame.width, 8);
        assert_eq!(frame.height, 8);
        assert_eq!(&frame.data, &data);

        let frame = decode_grp_to_paletted(GRP, 1).unwrap();
        let data = &[
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x80, 0x81, 0x81, 0x80, 0x81, 0x81,
            0x00, 0x00, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,

            0x00, 0x00, 0x00, 0x80, 0x80, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x80, 0x80, 0x80, 0x80, 0x82, 0x82,
            0x00, 0x00, 0x80, 0x81, 0x81, 0x80, 0x80, 0x80,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];
        assert_eq!(frame.left, 2);
        assert_eq!(frame.top, 2);
        assert_eq!(frame.right, 8);
        assert_eq!(frame.bottom, 7);
        assert_eq!(frame.width, 8);
        assert_eq!(frame.height, 8);
        assert_eq!(&frame.data, &data);

        let frame = decode_grp_to_paletted(GRP, 2).unwrap();
        let data = &[
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x80, 0x00, 0x00, 0x00, 0x00, 0x80, 0x00,
            0x00, 0x00, 0x80, 0x00, 0x00, 0x80, 0x00, 0x00,

            0x00, 0x00, 0x00, 0x80, 0x80, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x80, 0x00, 0x00, 0x80, 0x00, 0x00,
            0x00, 0x80, 0x00, 0x81, 0x81, 0x00, 0x80, 0x00,
            0x00, 0x00, 0x80, 0x00, 0x00, 0x80, 0x00, 0x00,
        ];
        assert_eq!(frame.left, 1);
        assert_eq!(frame.top, 2);
        assert_eq!(frame.right, 7);
        assert_eq!(frame.bottom, 8);
        assert_eq!(frame.width, 8);
        assert_eq!(frame.height, 8);
        assert_eq!(&frame.data, &data);

        let frame = decode_grp_to_paletted(GRP, 3).unwrap();
        let data = &[
            0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x80, 0x80, 0x00, 0x00, 0x00,

            0x00, 0x00, 0x00, 0x80, 0x80, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80,
        ];
        assert_eq!(frame.left, 0);
        assert_eq!(frame.top, 0);
        assert_eq!(frame.right, 8);
        assert_eq!(frame.bottom, 8);
        assert_eq!(frame.width, 8);
        assert_eq!(frame.height, 8);
        assert_eq!(&frame.data, &data);
    }
}
