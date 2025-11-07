use std::cell::RefCell;
use std::ops::DerefMut;
use std::rc::Rc;
use sdl3::render::WindowCanvas;
use crate::nes::bus::Bus;
use crate::nes::cpu;
use cpu::Cpu;
use crate::nes::mapper::mapper;
use mapper::Mapper;
use crate::nes::cpu::CpuReturnAction;
use crate::nes::ppu::{Ppu, PpuReturnAction};

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

    pub fn write_cpu(&mut self, addr: u16, val: u8) {
        self.mapper.write_cpu(addr, val);
        //todo: extend to other components
    }

    pub fn read_ppu(&mut self, addr: u16) -> u8 {
        return self.mapper.read_ppu(addr & 0x3FFF); //ppu has a 14 bit address space
    }

    pub fn write_ppu(&mut self, addr: u16, val: u8) {
        self.mapper.write_ppu(addr & 0x3FFF, val);
        //todo: extend to other components
    }

    pub fn step(&mut self) { //processes one full frame
        let mut cpu_val: Option<u8> = Option::None;
        while self.ppu.is_frame_ready() {
            let cpu_action = self.cpu.step(cpu_val);
            while cpu_action != CpuReturnAction::None {
                match cpu_action {
                    CpuReturnAction::None => {}
                    CpuReturnAction::Read(addr) => {
                        cpu_val = Option::from(self.read_cpu(addr));
                    }
                    CpuReturnAction::Write(addr, write_val) => {
                        self.write_cpu(addr, write_val);
                        cpu_val = Option::None;
                    }
                }
            }
            let mut ppu_val: Option<u8> = Option::None;
            for _ in 0..3 {
                let ppu_action = self.ppu.step(ppu_val);
                match ppu_action {
                    PpuReturnAction::None => {}
                    PpuReturnAction::Read(addr) => {
                        ppu_val = Option::from(self.read_ppu(addr));
                    }
                    PpuReturnAction::Write(addr, write_val) => {
                        self.write_ppu(addr, write_val);
                        ppu_val = Option::None;
                    }
                }
            }
        }

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
