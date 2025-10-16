use std::cell::RefCell;
use std::rc::Rc;
use crate::nes::bus::Bus;

pub struct Ppu {
    bus: Rc<RefCell<Bus>>,
}

impl Ppu {
    pub fn new(bus: Rc<RefCell<Bus>>) -> Rc<RefCell<Ppu>> {
        let ppu = Ppu {
            bus
        };
        let ppu = Rc::new(RefCell::new(ppu));
        ppu.borrow_mut().bus.borrow_mut().set_ppu(Rc::downgrade(&ppu));
        return ppu;
    }

    pub fn reset(&mut self) {
        todo!("Yeah")
    }
}