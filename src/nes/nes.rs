use std::cell::RefCell;
use std::ops::DerefMut;
use std::rc::Rc;
use sdl3::render::WindowCanvas;
use crate::nes::bus::Bus;
use crate::nes::cpu::Cpu;
use crate::nes::mapper::mapper;
use mapper::Mapper;
use crate::nes::ppu::Ppu;

pub struct Nes {
    cpu: Rc<RefCell<Cpu>>,
    ppu: Rc<RefCell<Ppu>>,
    bus: Rc<RefCell<Bus>>,

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

        let bus = Rc::new(RefCell::new(Bus::new()));
        let mut nes = Nes {
            bus: bus.clone(),
            cpu: Cpu::new(bus.clone()),
            ppu: Ppu::new(bus.clone()),
            mapper: mapper::create_mapper(&header, &rom, &chrrom),
        };
        nes.reset();
        return nes;
    }

    pub fn is_halted(&self) -> bool {
        self.cpu.borrow().is_halted()
    }

    pub fn reset(&mut self) {
        self.bus.borrow_mut().reset();
        self.cpu.borrow_mut().reset();
        self.ppu.borrow_mut().reset();
    }

    pub fn step(&mut self) {
        if !self.is_halted() {
            let cpu_cycles = self.cpu.borrow_mut().step();
            let mut ppu = self.ppu.borrow_mut();
            for _ in 0..(cpu_cycles * 3) {
                ppu.step();
            }
        }
    }

    pub fn draw_chrrom(&self, canvas: &mut WindowCanvas) { //temp function for testing and drawing CHRROM
        self.bus.borrow_mut().draw_chrrom(canvas);
    }
}
