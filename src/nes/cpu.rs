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

    s: u8, //stack pointer
    c: bool, //carry flag
    z: bool, //zero flag
    i: bool, //interrupt disable flag
    d: bool, //decimal mode flag (lol)
    b: bool, //break command flag
    v: bool, //overflow flag
    n: bool, //negative flag
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
            c: false,
            z: false,
            i: false,
            d: false,
            b: false,
            v: false,
            n: false,
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

        match op & 0x1F {
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
        let mut val_16: u16 = 0;
        if read {
            match op & 0x1F {
                0x00 => {
                    if (op & 0xE0) >= 0x80 { //NOP, LDY, CPY, CPX
                        val = self.read_immediate();
                    }
                    if (op & 0xE0) == 0x20 { //JSR
                        val_16 = self.get_absolute_addr();
                    }
                },
                0x04 => {
                    val = self.read_zero_page();
                },
                0x0C => {
                    if op == 0x6C { //JMP
                        val_16 = self.read_indirect_16();
                    } else if op == 0x4C { //JMP
                        val_16 = self.get_absolute_addr();
                    } else {
                        val = self.read_absolute();
                    }
                },
                0x10 => {
                    val = self.read_immediate(); //gonna have to transform these to a signed value for most branches
                },
                0x14 => {
                    val = self.read_zero_page_x();
                },
                0x1C => {
                    val = self.read_absolute_x();
                },
                _ => unreachable!("impossible value range somehow")
            }
        }

        let mut matched = false;
        match op {
            0x04 | 0x0C | 0x14 | 0x1C |
            0x34 | 0x3C |
            0x44 | 0x54 | 0x5C |
            0x64 | 0x74 | 0x7C |
            0x80 |
            0xD4 | 0xDC |
            0xF4 | 0xFC => {
                matched = true; //the NOPs (note: some of these have differing cycle lengths, which might need to be dealt with eventually)
            },
            0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => { //LDY
                matched = true;
                self.y = val;
                self.z = self.y == 0;
                self.n = self.y & 0x80 == 0x80;
            },
            0xC0 | 0xC4 | 0xCC => { //CPY
                matched = true;
                let result = self.y - val;
                self.c = self.y >= val;
                self.z = result == 0;
                self.n = result & 0x80 == 0x80;
            },
            0xE0 | 0xE4 | 0xEC => { //CPX
                matched = true;
                let result = self.x - val;
                self.c = self.x >= val;
                self.z = result == 0;
                self.n = result & 0x80 == 0x80;
            },
            0x24 | 0x2C => { //BIT
                let result = self.a & val;
                self.z = result == 0;
                self.v = result & 0x40 == 0x40;
                self.n = result & 0x80 == 0x80;
            },
            _ => (),
        }
        if !matched {
            if op & 0x1F == 0x00 {
                if op == 0x00 { //BRK
                    self.push_stack_16(self.pc.wrapping_add(1)); //it's weird but BRK does just fully skip a byte
                    self.push_stack_flags();
                    self.pc = 0xFFFE;
                } else if op == 0x20 { //JSR
                    self.push_stack_16(self.pc.wrapping_sub(1)); //notably, JSR points to one before the next instruction, conflicting with our method of reading operands first
                    self.pc = val_16;
                } else if op == 0x40 { //RTI
                    self.pop_stack_flags();
                    self.pc = self.pop_stack_16();
                } else if op == 0x60 { //RTS
                    self.pc = self.pop_stack_16().wrapping_add(1);
                }
            } //0x04 is covered by earlier cases (NOP, BIT, STY, LDY, CPY, CPX)
            else if op & 0x1F == 0x08 {
                if op == 0x08 { //PHP
                    self.push_stack_flags();
                } else if op == 0x28 { //PLP
                    self.pop_stack_flags();
                } else if op == 0x48 { //PHA
                    self.push_stack(self.a);
                } else if op == 0x68 { //PLA
                    self.a = self.pop_stack();
                } else if op == 0x88 { //DEY
                    self.y = self.y.wrapping_sub(1);
                    self.z = self.y == 0;
                    self.n = self.y & 0x80 == 0x80;
                } else if op == 0xA8 { //TAY
                    self.y = self.a;
                    self.z = self.y == 0;
                    self.n = self.y & 0x80 == 0x80;
                } else if op == 0xC8 { //INY
                    self.y = self.y.wrapping_add(1);
                    self.z = self.y == 0;
                    self.n = self.y & 0x80 == 0x80;
                } else if op == 0xE8 { //INX
                    self.x = self.x.wrapping_add(1);
                    self.z = self.x == 0;
                    self.n = self.x & 0x80 == 0x80;
                }
            } else if op & 0x1F == 0x0A {
                self.pc = val_16; //JMP
            } else if op & 0x1F == 0x10 {
                if op == 0x10 && !self.n {
                    self.pc = self.pc.wrapping_add_signed(val as i8 as i16); //BPL
                } else if op == 0x30 && self.n {
                    self.pc = self.pc.wrapping_add_signed(val as i8 as i16); //BMI
                } else if op == 0x50 && self.v {
                    self.pc = self.pc.wrapping_add_signed(val as i8 as i16); //BVC
                } else if op == 0x70 && !self.v {
                    self.pc = self.pc.wrapping_add_signed(val as i8 as i16); //BVS
                } else if op == 0x90 && self.c {
                    self.pc = self.pc.wrapping_add_signed(val as i8 as i16); //BCC
                } else if op == 0xB0 && !self.c {
                    self.pc = self.pc.wrapping_add_signed(val as i8 as i16); //BCS
                } else if op == 0xD0 && self.z {
                    self.pc = self.pc.wrapping_add_signed(val as i8 as i16); //BNE
                } else if op == 0xF0 && !self.z {
                    self.pc = self.pc.wrapping_add_signed(val as i8 as i16); //BEQ
                }
            } //0x14 is covered by earlier cases (NOP, STY, LDY)
            else if op & 0x1F == 0x18 {
                let shifted = (op & 0x0E) >> 4;
                if shifted <= 0x2 {
                    self.c = ((shifted >> 1) & 0b1) == 1; //CLC and SEC
                } else if shifted <= 0x6 {
                    self.i = ((shifted >> 1) & 0b1) == 1; //CLI and SEI
                } else if shifted <= 0xA {
                    if op == 0x98 {
                        self.a = self.y; //TYA
                    } else {
                        self.v = false; //CLV
                    }
                } else if shifted <= 0xE {
                    self.d = ((shifted >> 1) & 0b1) == 1; //CLD and SED
                }
            } else if op & 0x1F == 0x1C {
                todo!("SHY executed!") //SHY, lol
            }
        }

        if write {
            match op & 0x1F {
                0x04 => {
                    self.write_zero_page(val);
                },
                0x0C => {
                    self.write_absolute(val);
                },
                0x14 => {
                    self.write_zero_page_x(val);
                },
                0x1C => {
                    self.write_absolute_x(val);
                },
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

        match op & 0xE0 { //actual implementations
            0x00 => { //ORA
                self.a = self.a | val;
                self.z = self.a == 0;
                self.n = self.a & 0x80 == 0x80;
            },
            0x20 => { //AND
                self.a = self.a & val;
                self.z = self.a == 0;
                self.n = self.a & 0x80 == 0x80;
            },
            0x40 => { //EOR/XOR
                self.a = self.a ^ val;
                self.z = self.a == 0;
                self.n = self.a & 0x80 == 0x80;
            },
            0x60 => { //ADC
                let pre_a = self.a;
                self.a = self.a.wrapping_add(val).wrapping_add(u8::from(self.c));

                self.c = (pre_a as u16 + val as u16 + self.c as u16) > 0xFF; //todo: stupid way of doing this, fix that
                self.z = self.a == 0;
                self.n = self.a & 0x80 == 0x80;
                self.v = (self.a ^ pre_a) & (self.a ^ val) & 0x80 != 0
            },
            0x80 => { //STA
                val = self.a;
            }
            0xA0 => { //LDA
                self.a = val;
                self.z = self.a == 0;
                self.n = self.a & 0x80 == 0x80;
            }
            0xC0 => { //CMP
                self.c = self.a >= val;
                self.z = self.a == val;
                self.n = (self.a - val) & 0x80 == 0x80;
            }
            0xE0 => { //SBC
                let pre_a = self.a;
                self.a = self.a.wrapping_sub(val).wrapping_sub(!self.c as u8);

                self.c = !(pre_a as i16 - val as i16 - (!self.c) as i16) < 0;
                self.z = self.a == 0;
                self.n = self.a & 0x80 == 0x80;
                self.v = (self.a ^ pre_a) & (self.a ^ !val) & 0x80 != 0
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
        match op & 0x1F {
            0x00 | 0x04 | 0x08 | 0x0C | 0x10 | 0x14 | 0x18 | 0x1C => { //control instructions
                match op & 0x1F {
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
                match op & 0x1F {
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
                match op & 0x1F {
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

    fn push_stack(&mut self, val: u8) {
        let addr = 0x0100u16 + self.s as u16;
        self.s = self.s.wrapping_sub(1);
        self.bus.borrow_mut().write(addr, val);
    }

    fn push_stack_flags(&mut self) {
        let val: u8 = (self.n as u8) << 7 | (self.v as u8) << 6 | 1 << 5 | 1 << 4 | (self.d as u8) << 3 | (self.i as u8) << 2 | (self.z as u8) << 1 | (self.c as u8) << 0;
        self.push_stack(val);
    }

    fn push_stack_16(&mut self, val: u16) {
        self.push_stack((val & 0xFF) as u8);
        self.push_stack((val >> 8) as u8);
    }

    fn pop_stack(&mut self) -> u8 {
        let addr = 0x0100u16 + self.s as u16;
        self.s = self.s.wrapping_add(1);
        return self.bus.borrow_mut().read(addr);
    }

    fn pop_stack_flags(&mut self) {
        let flags = self.pop_stack();
        self.n = (flags & 0b10000000) != 0;
        self.v = (flags & 0b01000000) != 0;
        //note the two values missing, that's intentional
        self.d = (flags & 0b00001000) != 0;
        self.i = (flags & 0b00000100) != 0;
        self.z = (flags & 0b00000010) != 0;
        self.c = (flags & 0b00000001) != 0;
    }

    fn pop_stack_16(&mut self) -> u16 {
        let low = self.pop_stack() as u16;
        let high = self.pop_stack() as u16;
        return (high << 8) | low;
    }

    fn read_indirect_16(&mut self) -> u16 {
        let low_1 = self.read(self.pc);
        let high_1 = self.read(self.pc.wrapping_add(1));
        let addr_1 = (high_1 as u16) << 8 | low_1 as u16;
        let addr_2 = (high_1 as u16) << 8 | low_1.wrapping_add(1) as u16; //weird bug where only the lower byte is incremented

        let low_2 = self.bus.borrow_mut().read(addr_1);
        let high_2 = self.bus.borrow_mut().read(addr_2);
        return (high_2 as u16) << 8 | low_2 as u16;
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
