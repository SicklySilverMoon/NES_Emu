pub struct Ram {
    cpu_data: [u8; 0x800],
    //todo: PPU data
}

impl Ram {
    pub fn new() -> Ram {
        Ram {
            cpu_data: [0; 0x800]
        }
    }

    pub fn read_cpu(&self, addr: u16) -> u8 {
        self.cpu_data[addr as usize]
    }

    pub fn write_cpu(&mut self, addr: u16, val: u8) {
        self.cpu_data[addr as usize] = val;
    }
}