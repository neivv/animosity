use byteorder::{ByteOrder, LittleEndian, WriteBytesExt};

pub struct GrpWriter {
    buffer: Vec<u8>,
    frames: u16,
}

impl GrpWriter {
    /// You must know the number of frames that will be added, though you're not
    /// required to call add_frame for each of them. Frames just default to 0x0 dummies.
    pub fn new(frames: u16, width: u16, height: u16) -> GrpWriter {
        let mut buffer = Vec::with_capacity(frames as usize * 32);
        buffer.write_u16::<LittleEndian>(frames).unwrap();
        buffer.write_u16::<LittleEndian>(width).unwrap();
        buffer.write_u16::<LittleEndian>(height).unwrap();
        buffer.resize_with(buffer.len() + frames as usize * 8, || 0);
        GrpWriter {
            buffer,
            frames,
        }
    }

    pub fn add_frame(
        &mut self,
        frame: u16,
        x: u8,
        y: u8,
        width: u8,
        height: u8,
        rgba: &[u8],
    ) {
        assert!(frame < self.frames);
        assert_eq!(rgba.len(), width as usize * height as usize * 4);
        let frame_header_offset = frame as usize * 8 + 6;
        let frame_start_offset = self.buffer.len();
        LittleEndian::write_u32(
            &mut self.buffer[(frame_header_offset + 4)..],
            frame_start_offset as u32,
        );
        self.buffer[frame_header_offset + 0] = x;
        self.buffer[frame_header_offset + 1] = y;
        self.buffer[frame_header_offset + 2] = width;
        self.buffer[frame_header_offset + 3] = height;

        self.buffer.resize_with(self.buffer.len() + 2 * height as usize, || 0);
        let mut line_offset_pos = frame_start_offset;

        let mut offset = height as u16 * 2;
        for line in rgba.chunks_exact(width as usize * 4) {
            LittleEndian::write_u16(&mut self.buffer[line_offset_pos..], offset);
            line_offset_pos += 2;
            let mut pos = line;
            while pos.len() >= 4 {
                let len;
                if pos[3] == 0 {
                    // Write transparent
                    len = pos.chunks_exact(4).take(0x7f).take_while(|x| x[3] == 0).count();
                    self.buffer.push(0x80 | len as u8);
                    offset += 1;
                } else {
                    len = pos.chunks_exact(4).take(0x3f).take_while(|x| x[3] != 0).count();
                    self.buffer.push(0x40 | len as u8);
                    self.buffer.push(0x01);
                    offset += 2;
                }
                pos = &pos[4 * len..];
            }
        }
    }

    pub fn finish(self) -> Vec<u8> {
        self.buffer
    }
}
