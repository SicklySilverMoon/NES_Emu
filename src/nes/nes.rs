use std::cell::RefCell;
use std::ops::DerefMut;
use std::rc::Rc;
use sdl3::render::WindowCanvas;
use crate::nes::bus::Bus;
use crate::nes::cpu;
use cpu::{Cpu, CpuStage};
use crate::nes::mapper::mapper;
use mapper::Mapper;
use crate::nes::ppu::Ppu;

pub struct Nes {
    cpu: Cpu,
    ppu: Ppu,
    mapper: Box<dyn Mapper>,
}

impl Nes {
    pub fn new(rom_data: Vec<u8>) -> Nes {
        let splits = rom_data.split_at(0x10); //split into header and combined ROM data
        let header: [u8; 0x10] = splits.0.try_into().unwrap();
        let rom_size = 0x4000usize * header[4] as usize;
        let splits = splits.1.split_at(rom_size); //split into ROM and CHRROM data.
        let rom = splits.1.to_vec();
        // rom.resize(rom_size, 0);
        let chrrom_size = 0x2000usize * header[5] as usize;
        let chrrom = splits.1.to_vec(); //there could be something about a footer here that could mess something up in future
        // chrrom.resize(chrrom_size, 0);

        let mut nes = Nes {
            mapper: mapper::create_mapper(&header, &rom, &chrrom),
            cpu: Cpu::new(),
            ppu: Ppu::new(),
        };
        nes.reset();
        return nes;
    }

    pub fn is_halted(&self) -> bool {
        self.cpu.is_halted()
    }

    pub fn reset(&mut self) {
        // self.bus.borrow_mut().reset();
        let reset_vector = self.mapper.read_cpu(0xFFFC) as u16 | (self.mapper.read_cpu(0xFFFD) << 8) as u16;
        self.cpu.reset(reset_vector);
        self.ppu.reset();
    }

    pub fn read_cpu(&mut self, addr: u16) -> u8 {
        return self.mapper.read_cpu(addr);
    }

    pub fn step(&mut self) {
        let closure = self.cpu.step();


        // if !self.is_halted() {
        //     let cpu_cycles = self.cpu.step();
        //     for _ in 0..(cpu_cycles * 3) {
        //         self.ppu.step();
        //     }
        // }
    }

    pub fn draw_chrrom(&self, canvas: &mut WindowCanvas) { //temp function for testing and drawing CHRROM
        // self.bus.borrow_mut().draw_chrrom(canvas);
    }
}
