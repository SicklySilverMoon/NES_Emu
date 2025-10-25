use std::cell::RefCell;
use std::rc::Rc;
use crate::nes::bus::Bus;

pub struct Ppu {
    bus: Rc<RefCell<Bus>>,

    //internal registers
    w: bool, //w register/write latch
    t: u16, //t register/transfer address
    v: u16, //v register/vram addr
    v_temp: u16, //temp vram addr
}

impl Ppu {
    pub fn new(bus: Rc<RefCell<Bus>>) -> Rc<RefCell<Ppu>> {
        let ppu = Ppu {
            bus,

            w: false,
            t: 0,
            v: 0,
            v_temp: 0,
        };
        let ppu = Rc::new(RefCell::new(ppu));
        ppu.borrow_mut().bus.borrow_mut().set_ppu(Rc::downgrade(&ppu));
        return ppu;
    }

    pub fn reset(&mut self) {
        //todo: yeah

        self.w = false;
        self.t = 0;
        self.v = 0;
        self.v_temp = 0;
    }

    pub fn step(&mut self) {
        //todo: yeah
    }

    pub fn write(&mut self, addr: u16, data: u8) {
        let addr = addr & 0x2007; //redirects all writes into [0x2000, 0x2007]
        match addr {
            0x2000 => { //PPUCTRL

            },
            0x2001 => { //PPUMASK

            },
            0x2002 => { //PPUSTATUS

            },
            0x2003 => { //OAMADDR

            },
            0x2004 => { //OAMDATA

            },
            0x2005 => { //PPUSCROLL

            },
            0x2006 => { //PPUADDR
                if !self.w {
                    self.v_temp = ((data & 0x3F) as u16) << 8;
                } else {
                    self.v = self.v_temp | (data as u16);
                }
                self.w = !self.w;
            },
            0x2007 => { //PPUDATA
                if self.v < 0x2000 {
                    if self.bus.borrow().header[5] == 0 { //todo: shunt this over to an eventual cartridge/mapper class
                        self.chr_ram[self.v as usize] = data; //CHR-RAM
                    }
                } else if self.v < 0x3F00 {
                    //todo: write to the nametables
                } else {
                    //todo: write to palette OAM
                }
            },
            _ => { unreachable!("impossible value range somehow") }
        }
    }
}
