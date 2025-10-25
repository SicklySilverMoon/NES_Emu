use std::cell::RefCell;
use std::rc::Weak;
use std::{env, fs};
use sdl3::pixels::Color;
use sdl3::rect::Point;
use sdl3::render::{WindowCanvas};
use crate::nes::cpu::Cpu;
use crate::nes::ppu::Ppu;

pub struct Bus {
    ram: [u8; 0x800],
    rom_header: [u8; 0x10],
    rom: Vec<u8>,
    chrrom: Vec<u8>,
    last_read: u8,

    cpu: Weak<RefCell<Cpu>>,
    ppu: Weak<RefCell<Ppu>>,
}

impl Bus {
    pub fn new() -> Bus {
        Bus {
            ram: [0; 0x800],
            rom_header: [0; 0x10],
            rom: Vec::new(),
            chrrom: Vec::new(),
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
        let splits = rom_data.split_at(0x10); //split into header and combined ROM data
        self.rom_header = splits.0.try_into().unwrap();

        let rom_size = 0x4000usize * self.rom_header[4] as usize;
        let splits = splits.1.split_at(rom_size); //split into ROM and CHRROM data.
        self.rom = splits.1.to_vec();
        self.rom.resize(rom_size, 0);

        let chrrom_size = 0x2000usize * self.rom_header[5] as usize;
        self.chrrom = splits.1.to_vec();
        self.chrrom.resize(chrrom_size, 0);
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
        } else if addr < 0x4000 {
            todo!("PPU registers")
        }
        //notably we do not write to ROM
        //todo rest of it
    }

    pub fn draw_chrrom(&self, canvas: &mut WindowCanvas) { //temp function for testing and drawing CHRROM
        for table in 0..2 {
            for row in 0..16 {
                for col in 0..16 {
                    for y in 0..8 {
                        let low_byte = self.chrrom[y + col * 0x10 + row * 0x100 + table * 0x1000];
                        let high_byte = self.chrrom[8 + y + col * 0x10 + row * 0x100 + table * 0x1000];
                        for x in 0..8 {
                            let mut two_bit = (low_byte >> (7 - x)) & 1; //0b0 or 0b1
                            two_bit |= ((high_byte >> (7 - x)) & 1) << 1;    //0b0X or 0b1X
                            canvas.set_draw_color(Color::RGB(two_bit * 85, two_bit * 85, two_bit * 85));
                            canvas.draw_point(Point::new(
                                (x + col * 8 + table * 128) as i32,
                                (y + row * 8) as i32)).expect("if this fires something bad has occurred!");
                        }
                    }
                }
            }
        }
    }
}
