use std::cell::RefCell;
use std::ops::DerefMut;
use std::rc::Rc;
use sdl3::render::WindowCanvas;
use crate::nes::bus::Bus;
use crate::nes::cpu::Cpu;
use crate::nes::ppu::Ppu;

pub struct Nes {
    cpu: Rc<RefCell<Cpu>>,
    ppu: Rc<RefCell<Ppu>>,
    bus: Rc<RefCell<Bus>>,
}

impl Nes {
    pub fn new() -> Nes {
        let bus = Rc::new(RefCell::new(Bus::new()));
        Nes {
            bus: bus.clone(),
            cpu: Cpu::new(bus.clone()),
            ppu: Ppu::new(bus.clone()),
        }
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
