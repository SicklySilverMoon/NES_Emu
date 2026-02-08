use std::cell::RefCell;
use std::rc::Rc;
use crate::nes::bus::Bus;

// #[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PpuReturnAction {
    None,
    Read(u16),
    ReadCallback(u16, Box<dyn Fn(u8, &mut Ppu)>),
    Write(u16, u8),
    WriteRead(u16, u8, u16),
}

pub struct Ppu {
    //internal registers
    w: bool, //w register/write latch
    t: u16, //t register/transfer address
    v: u16, //v register/vram addr
    v_temp: u16, //temp vram addr

    frame_ready: bool,
}

impl Ppu {
    pub fn new() -> Ppu {
        return Ppu {
            w: false,
            t: 0,
            v: 0,
            v_temp: 0,

            frame_ready: false,
        }
    }

    pub fn is_frame_ready(&self) -> bool {
        return self.frame_ready; //todo
    }

    pub fn reset(&mut self) {
        //todo: yeah

        self.w = false;
        self.t = 0;
        self.v = 0;
        self.v_temp = 0;
    }

    pub fn step(&mut self, val: Option<u8>) -> PpuReturnAction {
        //todo: yeah
        return PpuReturnAction::None;
    }

    pub fn write(&mut self, addr: u16, data: u8) -> PpuReturnAction {
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
                    self.v = (self.v_temp | (data as u16)) & 0x3FFF; //PPU space is 14 bits
                }
                self.w = !self.w;
            },
            0x2007 => { //PPUDATA
                if self.v < 0x2000 {
                    return PpuReturnAction::Write(self.v, data);
                } else if self.v < 0x3F00 {
                    //todo: write to the nametables
                } else {
                    //todo: write to palette OAM
                }
            },
            _ => { unreachable!("impossible value range somehow") }
        }
        return PpuReturnAction::None;
    }
}
