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
        return self.bus.borrow_mut().read_inc(addr, Option::from(&mut self.pc));
    }

    pub fn read_16(&mut self, addr: u16) -> u16 {
        return self.bus.borrow_mut().read_16_inc(addr, Option::from(&mut self.pc));
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
        let (read, write) = Cpu::opcode_needs_read_write(op);

        let mut val: u8 = 0; //it should never actually get used with this, safeguard
        if read {
            match op & 0x1F {
                0x00 => {
                    if (op & 0xE0) >= 0x80 { //NOP, LDY, CPY, CPX
                        val = self.read_immediate();
                    }
                    if (op & 0xE0) == 0x20 { //JSR
                        val = self.read_absolute();
                    }
                },
                0x04 => {
                    if (op == 0x6C) {
                        val = self.read_absolute();
                    }
                    val = self.read_zero_page();
                }
                _ => unreachable!("impossible value range somehow")
            }
        }
    }

    fn handle_alu_instr(&mut self, op: u8) {
        let (read, write) = Cpu::opcode_needs_read_write(op);

        let mut val: u8 = 0; //it should never actually get used with this, safeguard
        if read {
            match op & 0x1F {
                0x01 => {
                    val = self.read_x_indirect();
                },
                0x05 => {
                    val = self.read_zero_page();
                },
                0x09 => {
                    val = self.read_immediate();
                },
                0x0D => {
                    val = self.read_absolute();
                },
                0x11 => {
                    val = self.read_indirect_y();
                },
                0x15 => {
                    val = self.read_zero_page_x();
                },
                0x19 => {
                    val = self.read_absolute_y();
                },
                0x1D => {
                    val = self.read_absolute_x();
                },
                _ => unreachable!("impossible value range somehow")
            }
        }

        match op & 0xE0 {
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
                val = self.a;
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

        if write {
            match op & 0x1F {
                0x01 => {
                    self.write_x_indirect(val);
                },
                0x05 => {
                    self.write_zero_page(val);
                },
                0x09 => {
                    self.write_immediate(val);
                },
                0x0D => {
                    self.write_absolute(val);
                },
                0x11 => {
                    self.write_indirect_y(val);
                },
                0x15 => {
                    self.write_zero_page_x(val);
                },
                0x19 => {
                    self.write_absolute_y(val);
                },
                0x1D => {
                    self.write_absolute_x(val);
                },
                _ => unreachable!("impossible value range somehow")
            }
        }
    }

    fn handle_rmw_instr(&mut self, op: u8) {

    }

    fn handle_rmw_alu_instr(&mut self, op: u8) {

    }

    fn opcode_needs_read_write(op: u8) -> (bool, bool) { //read, write
        //https://www.nesdev.org/wiki/CPU_unofficial_opcodes used as reference
        if op == 0x84 | 0x9C { //control block, STY, SHY
            return (false, true)
        }
        if op == 0x8E | 0x96 | 0x9E { //control block, STY, SHY
            return (false, true)
        }
        match (op & 0x1F) {
            0x00 | 0x04 | 0x08 | 0x0C | 0x10 | 0x14 | 0x18 | 0x1C => { //control instructions
                match (op & 0x1F) {
                    0x04 | 0x0C | 0x10 | 0x14 | 0x1C => {
                        return (true, false); //notably, this ends up not covering earlier exceptions
                    }
                    0x00 => {
                        if (op & 0x0E >= 0x80) || (op & 0x0E == 0x20) { //bunch of loads and a JSR
                            return (true, false);
                        }
                        return (false, false);
                    }
                    _ => unreachable!("impossible value range somehow")
                }
            },
            0x01 | 0x05 | 0x09 | 0x0D | 0x11 | 0x15 | 0x19 | 0x1D => { //alu instructions
                match (op & 0x1F) {
                    0x00 | 0x20 | 0x40 | 0x60 | 0xA0 | 0xC0 | 0xE0 => { //the rest, which all read
                        return (true, false);
                    },
                    0x80 => { //STA
                        return (false, true);
                    },
                    _ => unreachable!("impossible value range somehow")
                }
            },
            0x02 | 0x06 | 0x0A | 0x0E | 0x12 | 0x16 | 0x1A | 0x1E => { //rmw instructions
                match (op & 0x1F) {
                    0x06 | 0x0E | 0x16 | 0x1E => {
                        return (true, false);
                    }
                    0x02 => {
                        if (op & 0x0E >= 0x80) || (op & 0x0E == 0x20) { //bunch of loads and a JSR
                            return (true, false);
                        }
                        return (false, false);
                    }
                    _ => unreachable!("impossible value range somehow")
                }
            },
            0x03 | 0x07 | 0x0B | 0x0F | 0x13 | 0x17 | 0x1B | 0x1F => { //rmw alu instructions
                //todo: agony agony agony agony
            },
            _ => unreachable!("impossible value range somehow")
        }
        return (false, false);
    }

    fn get_x_indirect_addr(&mut self) -> u16 {
        let zp = self.read(self.pc).wrapping_add(self.x);
        return self.bus.borrow_mut().read_16(zp as u16);
    }

    fn read_x_indirect(&mut self) -> u8 {
        let addr = self.get_x_indirect_addr();
        return self.bus.borrow_mut().read(addr);
    }

    fn write_x_indirect(&mut self, val: u8) {
        let addr = self.get_x_indirect_addr();
        self.bus.borrow_mut().write(addr, val);
    }

    fn get_indirect_y_addr(&mut self) -> u16 {
        let zp = self.read(self.pc);
        return self.bus.borrow_mut().read_16(zp as u16).wrapping_add(self.y as u16);
    }

    fn read_indirect_y(&mut self) -> u8 {
        let addr = self.get_indirect_y_addr();
        return self.bus.borrow_mut().read(addr);
    }

    fn write_indirect_y(&mut self, val: u8) {
        let addr = self.get_indirect_y_addr();
        self.bus.borrow_mut().write(addr, val);
    }

    //notably, immediate mode does not get a get since it's not fetching an address

    fn read_immediate(&mut self) -> u8 {
        return self.read(self.pc);
    }

    fn write_immediate(&mut self, val: u8) {
        self.pc += 1;
        //that's it, just the increment
    }

    fn get_zero_page_addr(&mut self) -> u16 {
        return self.read(self.pc) as u16;
    }

    fn read_zero_page(&mut self) -> u8 {
        let addr = self.get_zero_page_addr();
        return self.bus.borrow_mut().read(addr);
    }

    fn write_zero_page(&mut self, val: u8) {
        let addr = self.get_zero_page_addr();
        self.bus.borrow_mut().write(addr, val);
    }

    fn get_zero_page_x_addr(&mut self) -> u16 {
        return self.read(self.pc).wrapping_add(self.x) as u16;
    }

    fn read_zero_page_x(&mut self) -> u8 {
        let addr = self.get_zero_page_x_addr();
        return self.bus.borrow_mut().read(addr);
    }

    fn write_zero_page_x(&mut self, val: u8) {
        let addr = self.get_zero_page_x_addr();
        self.bus.borrow_mut().write(addr, val);
    }

    fn get_zero_page_y_addr(&mut self) -> u16 {
        return self.read(self.pc).wrapping_add(self.y) as u16;
    }

    fn read_zero_page_y(&mut self) -> u8 {
        let addr = self.get_zero_page_y_addr();
        return self.bus.borrow_mut().read(addr);
    }

    fn write_zero_page_y(&mut self, val: u8) {
        let addr = self.get_zero_page_y_addr();
        self.bus.borrow_mut().write(addr, val);
    }

    fn get_absolute_addr(&mut self) -> u16 {
        return self.read_16(self.pc);
    }

    fn read_absolute(&mut self) -> u8 {
        let addr = self.get_absolute_addr();
        return self.bus.borrow_mut().read(addr);
    }

    fn write_absolute(&mut self, val: u8) {
        let addr = self.get_absolute_addr();
        self.bus.borrow_mut().write(addr, val);
    }

    fn get_absolute_x_addr(&mut self) -> u16 {
        return self.read_16(self.pc).wrapping_add(self.x as u16);
    }

    fn read_absolute_x(&mut self) -> u8 {
        let addr = self.get_absolute_x_addr();
        return self.bus.borrow_mut().read(addr);
    }

    fn write_absolute_x(&mut self, val: u8) {
        let addr = self.get_absolute_x_addr();
        return self.bus.borrow_mut().write(addr, val);
    }

    fn get_absolute_y_addr(&mut self) -> u16 {
        return self.read_16(self.pc).wrapping_add(self.y as u16);
    }

    fn read_absolute_y(&mut self) -> u8 {
        let addr = self.get_absolute_y_addr();
        return self.bus.borrow_mut().read(addr);
    }

    fn write_absolute_y(&mut self, val: u8) {
        let addr = self.get_absolute_y_addr();
        return self.bus.borrow_mut().write(addr, val);
    }
}
