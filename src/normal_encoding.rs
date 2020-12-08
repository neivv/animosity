//! SCR decodes normal Z as min(sqrt(0.1), sqrt(1.0 - x * x - y * y))
//! in shaders. While the forced minimum z of sqrt(0.1) ~ 0.31 could
//! be taken into account there, I think it's better to just not do that
//! (Though the sprite renderer of this program is doing that)

/// Decodes normal x/y/z with SC:R's algorithm
/// from stored x/y.
/// Note: Depending on values stored returned x/y aren't necessarily
/// equivalent to input values.
/// (While I think that they are intended to be stored as normalized x/y,
/// the fact that SC:R's shader normalizes the normal vector after determining z
/// ends up allowing x/y to represent slightly smaller z than the forced minimum)
///
/// If the stored values aren't normalized, assume z = 0.0 (128) and normalize x/y.
pub fn decode_normal(x_in: u8, y_in: u8) -> (u8, u8, u8) {
    let x = u8_to_f32(x_in);
    let y = u8_to_f32(y_in);
    let x_y_sq = x * x + y * y;
    if x_y_sq > 1.0 {
        let len = x_y_sq.sqrt();
        (
            f32_to_u8(x / len),
            f32_to_u8(y / len),
            128,
        )
    } else {
        let z_sq = 1.0 - x_y_sq;
        (x_in, y_in, f32_to_u8(z_sq.sqrt()))
    }
}

/// Input values don't have to be normalized.
/// This will just return normalized x/y.
/// While returning non-normalized values could be used to encode slightly smaller
/// Z values than the shader-forced sqrt(0.1), not going to implement that.
/// People can edit the shaders if those weirdly angled normals are desired for some reason.
pub fn encode_normal(x: u8, y: u8, z: u8) -> (u8, u8) {
    let x = u8_to_f32(x);
    let y = u8_to_f32(y);
    let z = u8_to_f32(z);
    let len = (x * x + y * y + z * z).sqrt();
    (f32_to_u8(x / len), f32_to_u8(y / len))
}

/// 0 ..= 255 => -1.0 ..= 1.0
fn u8_to_f32(val: u8) -> f32 {
    ((val as f32) * (1.0 / 255.0) * 2.0) - 1.0
}

fn f32_to_u8(val: f32) -> u8 {
    let val = ((val + 1.0) / 2.0) * 255.0;
    // Technically this is implicit in as conversion nowadays but i'll make it explicit
    if val < 0.0 {
        0
    } else if val > 255.0 {
        255
    } else {
        val as u8
    }
}

#[test]
fn test_conversions() {
    assert!((u8_to_f32(0) - (-1.0)).abs() < 0.05);
    assert!((u8_to_f32(128) - (0.0)).abs() < 0.05);
    assert!((u8_to_f32(255) - (1.0)).abs() < 0.05);
    assert!((f32_to_u8(0.0) as i32 - 128).abs() < 2);
    assert!((f32_to_u8(1.0) as i32 - 255).abs() < 2);
    assert!((f32_to_u8(-1.0) as i32 - 0).abs() < 2);
}
