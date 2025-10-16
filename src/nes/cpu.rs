use std::cell::RefCell;
use std::rc::Rc;
use std::unreachable;

use crate::nes::bus::Bus;

pub struct Cpu {
    bus: Rc<RefCell<Bus>>,

    pc: u16,
    x: u8,
    y: u8,
    a: u8,

    s: u8,
    c: u8,
    z: u8,
    i: u8,
    d: u8,
    b: u8,
    v: u8,
    n: u8,
}

impl Cpu {
    pub fn new(bus: Rc<RefCell<Bus>>) -> Rc<RefCell<Cpu>> {
        let cpu = Cpu {
            bus,

            pc: 0,
            x: 0,
            y: 0,
            a: 0,

            s: 0,
            c: 0,
            z: 0,
            i: 0,
            d: 0,
            b: 0,
            v: 0,
            n: 0,
        };
        let cpu = Rc::new(RefCell::new(cpu));
        cpu.borrow_mut().bus.borrow_mut().set_cpu(Rc::downgrade(&cpu));
        return cpu;
    }

    pub fn reset(&mut self) {
        let pc_high = (self.bus.borrow_mut().read(0xFFFD) as u16) << 8;
        let pc_low = self.bus.borrow_mut().read(0xFFFC) as u16;
        self.pc = pc_high | pc_low
    }

    pub fn read(&mut self, addr: u16) -> u8 {
        let val = self.bus.borrow_mut().read(addr);
        self.pc += 1;
        return val;
    }

    pub fn step(&mut self) {
        let op = self.read(self.pc);

        match (op & 0x1F) {
            0x00 | 0x04 | 0x08 | 0x0C | 0x10 | 0x14 | 0x18 | 0x1C => {
                self.handle_control_instr(op);
            },
            0x01 | 0x05 | 0x09 | 0x0D | 0x11 | 0x15 | 0x19 | 0x1D => {
                self.handle_alu_instr(op);
            },
            0x02 | 0x06 | 0x0A | 0x0E | 0x12 | 0x16 | 0x1A | 0x1E => {
                self.handle_rmw_instr(op);
            },
            0x03 | 0x07 | 0x0B | 0x0F | 0x13 | 0x17 | 0x1B | 0x1F => {
                self.handle_rmw_alu_instr(op);
            },
            _ => unreachable!("impossible value range somehow")
        }
    }

    fn handle_control_instr(&mut self, op: u8) {

    }

    fn handle_alu_instr(&mut self, op: u8) {
        let mut val: u8;
        match (op & 0x1F) {
            0x01 => {
                val = self.get_x_indirect();
            },
            0x05 => {
                val = self.get_zero_page();
            },
            0x09 => {
                val = self.get_immediate();
            },
            0x0D => {
                val = self.get_absolute();
            },
            0x11 => {
                val = self.get_indirect_y();
            },
            0x15 => {
                val = self.get_zero_page_x();
            },
            0x19 => {
                val = self.get_absolute_y();
            },
            0x1D => {
                val = self.get_absolute_x();
            },
            _ => unreachable!("impossible value range somehow")
        }

        match (op & 0xE0) {
            0x00 => { //ORA
                self.a = self.a | val;
                self.z = u8::from(self.a == 0);
                self.n = u8::from(self.a & 0x80 == 0x80);
            },
            0x20 => { //AND
                self.a = self.a & val;
                self.z = u8::from(self.a == 0);
                self.n = u8::from(self.a & 0x80 == 0x80);
            },
            0x40 => { //EOR/XOR
                self.a = self.a ^ val;
                self.z = u8::from(self.a == 0);
                self.n = u8::from(self.a & 0x80 == 0x80);
            },
            0x60 => { //ADC
                let pre_a = self.a;
                self.a = self.a.wrapping_add(val).wrapping_add(self.c);

                self.c = u8::from((pre_a as u16 + val as u16 + self.c as u16) > 0xFF); //todo: stupid way of doing this, fix that
                self.z = u8::from(self.a == 0);
                self.n = u8::from(self.a & 0x80 == 0x80);
                self.v = u8::from((self.a ^ pre_a) & (self.a ^ val) & 0x80 != 0)
            },
            0x80 => { //STA
                if op == 0x89 {
                    return; //0x89 is a NOP with an immediate operand (so 2 byte NOP)
                } else {
                    todo!("Store it, need to basically invert the get functions");
                }
            }
            0xA0 => { //LDA
                self.a = val;
                self.z = u8::from(self.a == 0);
                self.n = u8::from(self.a & 0x80 == 0x80);
            }
            0xC0 => { //CMP
                self.c = u8::from(self.a >= val);
                self.z = u8::from(self.a == val);
                self.n = u8::from((self.a - val) & 0x80 == 0x80);
            }
            0xE0 => { //SBC
                let pre_a = self.a;
                self.a = self.a.wrapping_sub(val).wrapping_sub(!self.c);

                self.c = u8::from(!(pre_a as i16 - val as i16 - (!self.c) as i16) < 0);
                self.z = u8::from(self.a == 0);
                self.n = u8::from(self.a & 0x80 == 0x80);
                self.v = u8::from((self.a ^ pre_a) & (self.a ^ !val) & 0x80 != 0)
            }
            _ => unreachable!("impossible value range somehow")
        }
    }

    fn handle_rmw_instr(&mut self, op: u8) {

    }

    fn handle_rmw_alu_instr(&mut self, op: u8) {

    }

    fn get_x_indirect(&mut self) -> u8 {
        let zp = self.read(self.pc);
        let zp = zp + self.x;

        let low = self.bus.borrow_mut().read(zp as u16) as u16;
        let high = self.bus.borrow_mut().read((zp + 1) as u16) as u16;
        return self.bus.borrow_mut().read(high << 8 | low);
    }

    fn get_indirect_y(&mut self) -> u8 {
        let zp = self.read(self.pc);

        let low = self.bus.borrow_mut().read(zp as u16) as u16;
        let high = self.bus.borrow_mut().read((zp + 1) as u16) as u16;
        return self.bus.borrow_mut().read(high << 8 | low + self.y as u16);
    }

    fn get_immediate(&mut self) -> u8 {
        return self.read(self.pc)
    }

    fn get_zero_page(&mut self) -> u8 {
        let zp = self.read(self.pc) as u16;
        return self.bus.borrow_mut().read(zp);
    }

    fn get_zero_page_x(&mut self) -> u8 {
        let zp = (self.read(self.pc) + self.x) as u16;
        return self.bus.borrow_mut().read(zp);
    }

    fn get_zero_page_y(&mut self) -> u8 {
        let zp = (self.read(self.pc) + self.y) as u16;
        return self.bus.borrow_mut().read(zp);
    }

    fn get_absolute(&mut self) -> u8 {
        let low = self.read(self.pc) as u16;
        let high = self.read(self.pc) as u16;
        return self.bus.borrow_mut().read(high << 8 | low);
    }

    fn get_absolute_x(&mut self) -> u8 {
        let low = self.read(self.pc) as u16;
        let high = self.read(self.pc) as u16;
        return self.bus.borrow_mut().read(high << 8 | low + self.x as u16);
    }

    fn get_absolute_y(&mut self) -> u8 {
        let low = self.read(self.pc) as u16;
        let high = self.read(self.pc) as u16;
        return self.bus.borrow_mut().read(high << 8 | low + self.y as u16);
    }
}
