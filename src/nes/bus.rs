use std::cell::RefCell;
use std::rc::Weak;
use std::{env, fs};

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

    pub fn reset(&mut self) {
        let rom_path = env::args().nth(1).unwrap();
        let rom_data = fs::read(rom_path).unwrap();
        let splits = rom_data.split_at(0x10);
        let header: [u8; 0x10] = splits.0.try_into().unwrap();

        let mut rom_data = splits.1.to_vec();
        rom_data.resize(0x8000, 0);
        self.rom[..0x8000].copy_from_slice(&rom_data);
    }

    pub fn read_inc(&mut self, addr: u16, pc: Option<&mut u16>) -> u8 {
        pc.map(|pc| *pc = pc.wrapping_add(1)); //technically not what map should be used for, but its fineee
        if addr < 0x2000 {
            self.last_read = self.ram[addr as usize % 0x800];
            return self.last_read
        }
        if addr >= 0x8000 { //todo: yeah this will be a fun one, gonna have to make a Cartridge/Mapper class
            self.last_read = self.rom[addr as usize - 0x8000];
            return self.last_read
        }

        return 0
    }

    pub fn read(&mut self, addr: u16) -> u8 {
        self.read_inc(addr, None)
    }

    pub fn read_16_inc(&mut self, addr: u16, pc: Option<&mut u16>) -> u16 {
        pc.map(|pc| *pc = pc.wrapping_add(2));
        let low = self.read(addr);
        let high = self.read(addr.wrapping_add(1));
        return (high as u16) << 8 | low as u16;
    }

    pub fn read_16(&mut self, addr: u16)  -> u16 {
        self.read_16_inc(addr, None)
    }

    pub fn write(&mut self, addr: u16, data: u8) {
        if addr < 0x2000 {
            self.ram[addr as usize % 0x800] = data;
        }
        //notably we do not write to ROM
        todo!("rest of it")
    }
}
