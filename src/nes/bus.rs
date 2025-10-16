use std::{env, fs};
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use crate::nes::cpu::Cpu;
use crate::nes::ppu::Ppu;

pub struct Bus {
    ram: [u8; 0x800],
    rom: [u8; 0x8000],
    last_read: u8,

    cpu: Weak<RefCell<Cpu>>,
    ppu: Weak<RefCell<Ppu>>,
}

impl Bus {
    pub fn new() -> Bus {
        Bus {
            ram: [0; 0x800],
            rom: [0; 0x8000],
            last_read: 0,

            cpu: Default::default(),
            ppu: Default::default(),
        }
    }

    pub fn set_cpu(&mut self, cpu: Weak<RefCell<Cpu>>) {
        self.cpu = cpu;
    }

    pub fn set_ppu(&mut self, ppu: Weak<RefCell<Ppu>>) {
        self.ppu = ppu;
    }

    pub fn read(&mut self, addr: u16) -> u8 {
        if addr < 0x2000 {
            return self.ram[addr as usize % 0x800]
        }
        if addr >= 0x8000 {
            return self.rom[addr as usize - 0x8000]
        }

        return 0
    }

    pub fn write(&mut self, addr: u16, data: u8) {
        if addr < 0x2000 {
            self.ram[addr as usize % 0x800] = data;
        }
        todo!("rest of it")
    }

    pub fn reset(&mut self) {
        let rom_path = env::args().nth(1).unwrap();
        let rom_data = fs::read(rom_path).unwrap();
        let splits = rom_data.split_at(0x10);
        let header: [u8; 0x10] = splits.0.try_into().unwrap();

        let mut rom_data = splits.1.to_vec();
        rom_data.resize(0x8000, 0);
        self.rom[..0x8000].copy_from_slice(&rom_data);
    }
}