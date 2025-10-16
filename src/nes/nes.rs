use std::cell::RefCell;
use std::rc::{Rc, Weak};

use crate::nes::cpu::Cpu;
use crate::nes::ppu::Ppu;
use crate::nes::bus::Bus;

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

    pub fn reset(&mut self) {
        self.bus.borrow_mut().reset();
        self.cpu.borrow_mut().reset();
        self.ppu.borrow_mut().reset();
    }

    pub fn step(&mut self) {
        
    }
}
