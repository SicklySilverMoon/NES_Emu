use std::cell::RefCell;
use std::rc::Rc;
use std::unreachable;

use crate::nes::bus::Bus;
use crate::nes::nes::Nes;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CpuReturnAction {
    None,
    Read(u16),
    Write(u16, u8),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum CpuStage {
    FetchIns,
    Decode,
    FetchOp1,
    FetchOp2,
    FetchAddrAdjust, //I think I messed something up, todo: please fix
    FetchAddrAdjust2,
    FetchAddrAdjust3,
    FetchAddrAdjust4,
    FetchAddrAdjust5,
    Execute,
    WriteBack1,
    WriteBack2,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum AddrMode {
    Implied,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndirectX,
    IndirectY,
    Accumulator, //That's not even an address mode
}

pub struct Cpu {
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

    halted: bool, //if the CPU is halted

    stage: CpuStage,
    instruction: u8, //For storage across cycles/stages
    operand1: u8, //ditto
    operand2: u8, //ditto ditto
    val: u8, //ditto ditto ditto
    addr: u16, //used for storing addresses for some instructions
    cycles: u8, //how many cycles this instruction took
}

impl Cpu {
    pub fn new() -> Cpu {
        return Cpu {
            pc: 0,
            x: 0,
            y: 0,
            a: 0,

            s: 0x00, //since reset is USUALLY called after creation this *should* be fine

            c: false,
            z: false,
            i: true,
            d: false,
            b: false,
            v: false,
            n: false,

            halted: false,

            stage: CpuStage::FetchIns,
            instruction: 0,
            operand1: 0,
            operand2: 0,
            val: 0,
            addr: 0,
            cycles: 0,
        }
    }

    pub fn is_halted(&self) -> bool {
        return self.halted;
    }

    pub fn reset(&mut self, reset_vector: u16) {
        self.pc = reset_vector;

        self.s = self.s.wrapping_sub(3);
        self.i = true;
        self.halted = false;
        self.stage = CpuStage::FetchIns;
    }

    pub fn step(&mut self, val: Option<u8>) -> CpuReturnAction { //return an address that the NES must read, or an address to write, or nothing
        if self.stage == CpuStage::FetchIns {
            self.stage = CpuStage::Decode;
            return CpuReturnAction::Read(self.pc);
        } else if self.stage == CpuStage::Decode {
            self.instruction = val.unwrap(); //shouldn't panic
            let (read, write) = Cpu::instruction_needs_read_write(self.instruction);
            return if read || write {
                self.stage = CpuStage::FetchOp1;
                let ret = CpuReturnAction::Read(self.pc);
                self.pc = self.pc.wrapping_add(1);
                ret
            } else {
                self.stage = CpuStage::Execute;
                CpuReturnAction::None
            }
        } else if self.stage == CpuStage::FetchOp1 {
            self.operand1 = val.unwrap();

            let mode = Cpu::instruction_read_write_type(self.instruction);
            return match mode { //todo: start splitting on read and writes, as some stuff just reads, some stuff just needs ops for writes, and some weird ones (indexed stuff) do both
                AddrMode::ZeroPage => {
                    self.stage = CpuStage::Execute;
                    CpuReturnAction::Read(self.operand1 as u16)
                }
                AddrMode::ZeroPageX | AddrMode::ZeroPageY => {
                    self.stage = CpuStage::FetchAddrAdjust;
                    CpuReturnAction::Read(self.operand1 as u16)
                }
                AddrMode::Absolute | AddrMode::AbsoluteX | AddrMode::AbsoluteY |
                AddrMode::Indirect => {
                    self.stage = CpuStage::FetchOp2;
                    let ret = CpuReturnAction::Read(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    ret
                }
                AddrMode::IndirectX | AddrMode::IndirectY => {
                    self.stage = CpuStage::FetchAddrAdjust;
                    let ret = CpuReturnAction::Read(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    ret
                }
                _ => {
                    println!("Fallthrough on FetchOp1 with {:?}", self.stage);
                    self.stage = CpuStage::Execute;
                    CpuReturnAction::None
                }
            }
        } else if self.stage == CpuStage::FetchOp2 {
            self.operand2 = val.unwrap();

            let (read, write) = Cpu::instruction_needs_read_write(self.instruction);
            let mode = Cpu::instruction_read_write_type(self.instruction);
            match mode { //todo: some must fetch more, some must go right to execute, pretty easy to determine what's what
                AddrMode::Absolute => {
                    self.stage = CpuStage::Execute;
                    return CpuReturnAction::Read((self.operand1 as u16) | ((self.operand2 as u16) << 8));
                }
                AddrMode::AbsoluteX | AddrMode::AbsoluteY => {
                    self.stage = CpuStage::Execute;
                    return CpuReturnAction::Read((self.operand1 as u16) | ((self.operand2 as u16) << 8));
                }
                AddrMode::Indirect => {
                    //full indirect is only ever used for JMP
                    self.stage = CpuStage::FetchAddrAdjust;
                    return CpuReturnAction::Read((self.operand1 as u16) | ((self.operand2 as u16) << 8));
                }
                _ => { println!("Fallthrough on FetchOp2 with {:?}", self.stage); self.stage = CpuStage::Execute; return CpuReturnAction::None }
            }
        } else if self.stage == CpuStage::FetchAddrAdjust {
            //do nothing with the value, it's a dummy read
            let (read, write) = Cpu::instruction_needs_read_write(self.instruction);
            let mode = Cpu::instruction_read_write_type(self.instruction);
            match mode {
                AddrMode::ZeroPageX => {
                    self.operand1 = self.operand1.wrapping_add(self.x);
                    self.stage = CpuStage::Execute;
                    return CpuReturnAction::Read(self.operand1 as u16);
                }
                AddrMode::ZeroPageY => {
                    self.operand1 = self.operand1.wrapping_add(self.y);
                    self.stage = CpuStage::Execute;
                    return CpuReturnAction::Read(self.operand1 as u16);
                }
                AddrMode::AbsoluteX => {
                    let (op1, wrapped) = self.operand1.overflowing_add(self.x);
                    let ret = CpuReturnAction::Read((op1 as u16) | ((self.operand2 as u16) << 8));
                    if wrapped || (read && write) {
                        self.stage = CpuStage::FetchAddrAdjust2;
                    } else {
                        self.stage = CpuStage::Execute;
                    }
                    return ret;
                }
                AddrMode::AbsoluteY => {
                    let (op1, wrapped) = self.operand1.overflowing_add(self.x);
                    let ret = CpuReturnAction::Read((op1 as u16) | ((self.operand2 as u16) << 8));
                    if wrapped || (read && write) {
                        self.stage = CpuStage::FetchAddrAdjust2;
                    } else {
                        self.stage = CpuStage::Execute;
                    }
                    return ret;
                }
                AddrMode::Indirect => {
                    //again just JMP
                    self.addr = val.unwrap() as u16; //load lower byte
                    self.stage = CpuStage::Execute;
                    return CpuReturnAction::Read((self.operand1.wrapping_add(1) as u16) | ((self.operand2 as u16) << 8));
                }
                AddrMode::IndirectX => {
                    self.stage = CpuStage::FetchAddrAdjust2;
                    return CpuReturnAction::Read(self.operand1.wrapping_add(self.x) as u16);
                }
                AddrMode::IndirectY => {
                    self.stage = CpuStage::FetchAddrAdjust2;
                    return CpuReturnAction::Read(self.operand1 as u16);
                }
                _ => { println!("Fallthrough on FetchAddrAdjust with {:?}", self.stage); self.stage = CpuStage::Execute; return CpuReturnAction::None }
            }
        } else if self.stage == CpuStage::FetchAddrAdjust2 {
            //do nothing with the value, it's usually a dummy read
            let mode = Cpu::instruction_read_write_type(self.instruction);
            match mode {
                AddrMode::AbsoluteX | AddrMode::AbsoluteY => {
                    self.stage = CpuStage::Execute;
                    let mut addr = (self.operand1 as u16) | ((self.operand2.wrapping_add(1) as u16) << 8);
                    if mode == AddrMode::AbsoluteX {
                        addr = addr.wrapping_add(self.x as u16);
                    } else {
                        addr = addr.wrapping_add(self.y as u16);
                    }
                    return CpuReturnAction::Read(addr);
                }
                AddrMode::IndirectX => {
                    self.addr = val.unwrap() as u16;
                    self.stage = CpuStage::FetchAddrAdjust3;
                    return CpuReturnAction::Read(self.operand1.wrapping_add(self.x).wrapping_add(1) as u16);
                }
                AddrMode::IndirectY => {
                    self.addr = val.unwrap() as u16;
                    self.stage = CpuStage::FetchAddrAdjust3;
                    return CpuReturnAction::Read(self.operand1.wrapping_add(1) as u16);
                }
                _ => { println!("Fallthrough on FetchAddrAdjust2 with {:?}", self.stage); self.stage = CpuStage::Execute; return CpuReturnAction::None }
            }
        } else if self.stage == CpuStage::FetchAddrAdjust3 {
            let mode = Cpu::instruction_read_write_type(self.instruction);
            match mode {
                AddrMode::IndirectX => {
                    self.addr |= (val.unwrap() as u16) << 8; //load high byte
                    self.stage = CpuStage::FetchAddrAdjust4;
                    return CpuReturnAction::Read(self.addr); //todo: do branch on reading and writing here
                }
                AddrMode::IndirectY => {
                    let (_, wrapped) = (self.addr as u8).overflowing_add(self.y);
                    self.addr |= (val.unwrap() as u16) << 8; //load high byte
                    if wrapped {
                        self.stage = CpuStage::FetchAddrAdjust4;
                        let pre_addr = self.addr;
                        self.addr = self.addr.wrapping_add(self.y as u16);
                        return CpuReturnAction::Read(pre_addr);
                    } else {
                        self.stage = CpuStage::Execute;
                        return CpuReturnAction::Read(self.addr);
                    }
                }
                _ => { println!("Fallthrough on FetchAddrAdjust3 with {:?}", self.stage); self.stage = CpuStage::Execute; return CpuReturnAction::None }
            }
        } else if self.stage == CpuStage::FetchAddrAdjust4 {
            self.val = val.unwrap();
            let mode = Cpu::instruction_read_write_type(self.instruction);
            match mode {
                AddrMode::IndirectX => {
                    self.stage = CpuStage::Execute;
                }
                AddrMode::IndirectY => {
                    self.stage = CpuStage::FetchAddrAdjust5;
                    return CpuReturnAction::Read(self.addr);
                }
                _ => { println!("Fallthrough on FetchAddrAdjust4 with {:?}", self.stage); self.stage = CpuStage::Execute; return CpuReturnAction::None }
            }
        } else if self.stage == CpuStage::FetchAddrAdjust5 {
            self.val = val.unwrap();
            let mode = Cpu::instruction_read_write_type(self.instruction);
            match mode {
                AddrMode::IndirectY => {
                    self.stage = CpuStage::Execute;
                }
                _ => { println!("Fallthrough on FetchAddrAdjust5 with {:?}", self.stage); self.stage = CpuStage::Execute; return CpuReturnAction::None }
            }
        }

        if self.stage == CpuStage::Execute {
            if val.is_some() {
                self.val = val.unwrap();
            }
            let action: CpuReturnAction;
            match self.instruction & 0x1F {
                0x00 | 0x04 | 0x08 | 0x0C | 0x10 | 0x14 | 0x18 | 0x1C => {
                    action = self.handle_control_instr(self.instruction);
                },
                0x01 | 0x05 | 0x09 | 0x0D | 0x11 | 0x15 | 0x19 | 0x1D => {
                    action = self.handle_alu_instr(self.instruction);
                },
                0x02 | 0x06 | 0x0A | 0x0E | 0x12 | 0x16 | 0x1A | 0x1E => {
                    action = self.handle_rmw_instr(self.instruction);
                },
                0x03 | 0x07 | 0x0B | 0x0F | 0x13 | 0x17 | 0x1B | 0x1F => {
                    action = self.handle_rmw_alu_instr(self.instruction);
                },
                _ => unreachable!("impossible value range somehow")
            }
            return action;
        }

        if self.stage == CpuStage::WriteBack1 {
            //todo
        }
        // return self.cycles;
        return CpuReturnAction::None;
    }

    fn handle_control_instr(&mut self, op: u8) -> CpuReturnAction {
        let (read, write) = Cpu::instruction_needs_read_write(op);

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
                self.y = self.val;
                self.z = self.y == 0;
                self.n = self.y & 0x80 == 0x80;
            },
            0x84 | 0x94 | 0x8C => { //STY
                matched = true;
                self.val = self.y;
            }
            0xC0 | 0xC4 | 0xCC => { //CPY
                matched = true;
                let result = self.y.wrapping_sub(self.val);
                self.c = self.y >= self.val;
                self.z = result == 0;
                self.n = result & 0x80 == 0x80;
            },
            0xE0 | 0xE4 | 0xEC => { //CPX
                matched = true;
                let result = self.x.wrapping_sub(self.val);
                self.c = self.x >= self.val;
                self.z = result == 0;
                self.n = result & 0x80 == 0x80;
            },
            0x24 | 0x2C => { //BIT
                let result = self.a & self.val;
                self.z = result == 0;
                self.v = self.val & 0x40 == 0x40; //these use the value from mem for whatever reason?
                self.n = self.val & 0x80 == 0x80;
            },
            _ => (),
        }
        if !matched {
            if op & 0x1F == 0x00 {
                if op == 0x00 { //BRK
                    self.push_stack_16(self.pc.wrapping_add(1)); //it's weird but BRK does just fully skip a byte
                    self.push_stack_flags();
                    self.i = true;
                    self.pc = self.bus.borrow_mut().read_16(0xFFFE); //todo
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
            } else if op & 0x1F == 0x0C {
                self.pc = self.addr; //JMP //todo
            } else if op & 0x1F == 0x10 {
                if op == 0x10 && !self.n {
                    self.pc = self.pc.wrapping_add_signed(self.val as i8 as i16); //BPL
                } else if op == 0x30 && self.n {
                    self.pc = self.pc.wrapping_add_signed(self.val as i8 as i16); //BMI
                } else if op == 0x50 && !self.v {
                    self.pc = self.pc.wrapping_add_signed(self.val as i8 as i16); //BVC
                } else if op == 0x70 && self.v {
                    self.pc = self.pc.wrapping_add_signed(self.val as i8 as i16); //BVS
                } else if op == 0x90 && !self.c {
                    self.pc = self.pc.wrapping_add_signed(self.val as i8 as i16); //BCC
                } else if op == 0xB0 && self.c {
                    self.pc = self.pc.wrapping_add_signed(self.val as i8 as i16); //BCS
                } else if op == 0xD0 && !self.z {
                    self.pc = self.pc.wrapping_add_signed(self.val as i8 as i16); //BNE
                } else if op == 0xF0 && self.z {
                    self.pc = self.pc.wrapping_add_signed(self.val as i8 as i16); //BEQ
                }
            } //0x14 is covered by earlier cases (NOP, STY, LDY)
            else if op & 0x1F == 0x18 {
                let shifted = (op & 0xE0) >> 4;
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
        return CpuReturnAction::None;
    }

    fn handle_alu_instr(&mut self, op: u8) -> CpuReturnAction {
        let (read, write) = Cpu::instruction_needs_read_write(op);

        match op & 0xE0 { //actual implementations
            0x00 => { //ORA
                self.a = self.a | self.val;
                self.z = self.a == 0;
                self.n = self.a & 0x80 == 0x80;
            },
            0x20 => { //AND
                self.a = self.a & self.val;
                self.z = self.a == 0;
                self.n = self.a & 0x80 == 0x80;
            },
            0x40 => { //EOR/XOR
                self.a = self.a ^ self.val;
                self.z = self.a == 0;
                self.n = self.a & 0x80 == 0x80;
            },
            0x60 => { //ADC
                let pre_a = self.a;
                self.a = self.a.wrapping_add(self.val).wrapping_add(u8::from(self.c));

                self.c = (pre_a as u16 + self.val as u16 + self.c as u16) > 0xFF; //todo: stupid way of doing this, fix that
                self.z = self.a == 0;
                self.n = self.a & 0x80 == 0x80;
                self.v = (self.a ^ pre_a) & (self.a ^ self.val) & 0x80 != 0;
            },
            0x80 => { //STA
                self.val = self.a;
            }
            0xA0 => { //LDA
                self.a = self.val;
                self.z = self.a == 0;
                self.n = self.a & 0x80 == 0x80;
            }
            0xC0 => { //CMP
                self.c = self.a >= self.val;
                self.z = self.a == self.val;
                self.n = (self.a - self.val) & 0x80 == 0x80;
            }
            0xE0 => { //SBC
                let pre_a = self.a;
                self.a = self.a.wrapping_sub(self.val).wrapping_sub(!self.c as u8);

                self.c = !(pre_a as i16 - self.val as i16 - (!self.c) as i16) < 0;
                self.z = self.a == 0;
                self.n = self.a & 0x80 == 0x80;
                self.v = (self.a ^ pre_a) & (self.a ^ !self.val) & 0x80 != 0;
            }
            _ => unreachable!("impossible value range somehow")
        }
        return CpuReturnAction::None;
    }

    fn handle_rmw_instr(&mut self, op: u8) -> CpuReturnAction {
        let (read, write) = Cpu::instruction_needs_read_write(op);
        match op & 0x1F { //just to catch STPs
            0x02 => {
                if op & 0xE0 <= 0x60 { //STP
                    self.halted = true;
                    return CpuReturnAction::None;
                }
            },
            0x12 => { //STP
                self.halted = true;
                return CpuReturnAction::None;
            }
            _ => (),
        }

        match op & 0xE0 { //actual implementations
            0x00 => { //ASL
                if op & 0x1F == 0x0A { //accum version
                    self.val = self.a; //todo: missing cycle counts on accum versions
                }
                self.c = self.val & 0x80 == 0x80;
                self.val <<= 1;
                self.z = self.val == 0;
                self.n = self.val & 0x80 == 0x80;
                if op & 0x1F == 0x0A { //accum version
                    self.a = self.val;
                }
            },
            0x20 => { //ROL
                if op & 0x1F == 0x0A {
                    self.val = self.a;
                }
                let to_c = self.val & 0x80 == 0x80;
                self.val <<= 1;
                self.val |= self.c as u8;
                self.c = to_c;
                if op & 0x1F == 0x0A {
                    self.a = self.val;
                }
            },
            0x40 => { //LSR
                if op & 0x1F == 0x0A {
                    self.val = self.a;
                }
                self.c = self.val & 0x01 == 0x01;
                self.val >>= 1;
                self.z = self.val == 0;
                self.n = false; //lol
                if op & 0x1F == 0x0A {
                    self.a = self.val;
                }
            },
            0x60 => { //ROR
                if op & 0x1F == 0x0A {
                    self.val = self.a;
                }
                let to_c = self.val & 0x01 == 0x01;
                self.val >>= 1;
                self.val |= (self.c as u8) << 7;
                self.c = to_c;
                self.z = self.val == 0;
                self.n = self.val & 0x80 == 0x80;
                if op & 0x1F == 0x0A {
                    self.a = self.val;
                }
            },
            0x80 => {
                if op & 0x1F == 0x06 || op & 0x1F == 0x0E || op & 0x1F == 0x16 { //STX
                    self.val = self.x;
                } else if op & 0x1F == 0x0A { //TXA
                    self.a = self.x;
                    self.z = self.a == 0;
                    self.n = self.a & 0x80 == 0x80;
                } else if op & 0x1F == 0x1A { //TXS
                    self.s = self.x;
                } else if op & 0x1F == 0x1E {
                    todo!("SHX hit!")
                }
            },
            0xA0 => {
                if op & 0x1F == 0x02 || op & 0x1F == 0x06 || op & 0x1F == 0x0E || op & 0x1F == 0x16 || op & 0x1F == 0x1E { //LDX
                    self.x = self.val;
                    self.z = self.x == 0;
                    self.n = self.x & 0x80 == 0x80;
                } else if op & 0x1F == 0x0A { //TAX
                    self.x = self.a;
                    self.z = self.x == 0;
                    self.n = self.x & 0x80 == 0x80;
                } else if op & 0x1F == 0x1A { //TSX
                    self.x = self.s;
                    self.z = self.x == 0;
                    self.n = self.x & 0x80 == 0x80;
                }
            },
            0xC0 => { //DEC, DEX
                if op & 0x1F == 0x0A { //X instead of A lol
                    self.val = self.x;
                }
                self.val = self.val.wrapping_sub(1);
                self.z = self.val == 0;
                self.n = self.val & 0x80 == 0x80;
                if op & 0x1F == 0x0A {
                    self.x = self.val;
                }
            },
            0xE0 => { //INC
                //No accum version
                self.val = self.val.wrapping_add(1);
                self.z = self.val == 0;
                self.n = self.val & 0x80 == 0x80;
            }
            _ => unreachable!("impossible value range somehow")
        }

        if write {
            return CpuReturnAction::Write((self.operand1 as u16) | ((self.operand2 as u16) << 8), self.val);
        }
        return CpuReturnAction::None;
        // if write && !read {
        //     match op & 0x1F {
        //         0x02 => {
        //             self.write_immediate(val);
        //         },
        //         0x06 => {
        //             self.write_zero_page(val);
        //         },
        //         0x0E => {
        //             self.write_absolute(val);
        //         },
        //         0x16 => {
        //             self.write_zero_page_x(val);
        //         },
        //         0x1E => {
        //             self.write_absolute_x(val);
        //         },
        //         _ => unreachable!("impossible value range somehow")
        //     }
        // } else if write && read {
        //     self.bus.borrow_mut().write(addr, val); //write back to the earlier grabbed value
        // }
    }

    fn handle_rmw_alu_instr(&mut self, op: u8) -> CpuReturnAction {
        todo!("RMW ALU combined instructions hit!")
    }

    fn instruction_needs_read_write(op: u8) -> (bool, bool) { //read, write
        //https://www.nesdev.org/wiki/CPU_unofficial_opcodes used as reference
        if op == 0x84 || op == 0x8C || op == 0x94 || op == 0x9C { //control block, STY, SHY
            return (false, true)
        }
        if op == 0x86 || op == 0x8E || op == 0x96 || op == 0x9E { //control block, STX, SHX
            return (false, true)
        }
        match op & 0x1F {
            0x00 | 0x04 | 0x08 | 0x0C | 0x10 | 0x14 | 0x18 | 0x1C => { //control instructions
                match op & 0x1F {
                    0x04 | 0x0C | 0x10 | 0x14 | 0x1C => {
                        return (true, false); //notably, this ends up not covering earlier exceptions
                    }
                    0x00 => {
                        if (op & 0xE0 >= 0x80) || (op & 0xE0 == 0x20) { //bunch of loads and a JSR
                            return (true, false);
                        }
                        return (false, false);
                    }
                    _ => (),
                }
            },
            0x01 | 0x05 | 0x09 | 0x0D | 0x11 | 0x15 | 0x19 | 0x1D => { //alu instructions
                match op & 0xE0 {
                    0x00 | 0x20 | 0x40 | 0x60 | 0xA0 | 0xC0 | 0xE0 => { //the rest, which all read
                        return (true, false);
                    },
                    0x80 => { //STA
                        return (false, true);
                    },
                    _ => (),
                }
            },
            0x02 | 0x06 | 0x0A | 0x0E | 0x12 | 0x16 | 0x1A | 0x1E => { //rmw instructions
                match op & 0xE0 {
                    0x00 | 0x20 | 0x40 | 0x60 => { //ASL, ROL, LSR, ROR
                        if op & 0x1F != 0x02 && op & 0x1F != 0x0A && op & 0x1F != 0x12 && op & 0x1F != 0x1A {
                            return (true, true);
                        }
                        return (false, false);
                    },
                    _ => (),
                }
                match op & 0x1F {
                    0x02 => {
                        if op & 0xE0 >= 0x80 {
                            return (true, false); //LDX and NOPs
                        }
                        return (false, false);
                    },
                    0x06 | 0x0E | 0x16 | 0x1E => {
                        if op & 0xE0 == 0x80 {
                            return (false, true); //STX
                        } else if op & 0xE0 == 0xA0 {
                            return (true, false); //LDX
                        } else if op & 0xE0 >= 0xC0 {
                            return (true, true); //DEC, INC
                        }
                        return (false, false)
                    },
                    _ => (),
                }
            },
            0x03 | 0x07 | 0x0B | 0x0F | 0x13 | 0x17 | 0x1B | 0x1F => { //rmw alu instructions
                //todo: agony agony agony agony
            },
            _ => unreachable!("impossible value range somehow")
        }
        return (false, false);
    }

    fn instruction_read_write_type(instr: u8) -> AddrMode {
        let (read, write) = Cpu::instruction_needs_read_write(instr);

        match instr & 0x1F {
            0x00 | 0x04 | 0x08 | 0x0C | 0x10 | 0x14 | 0x18 | 0x1C => { //control instructions
                if read {
                    match instr & 0x1F {
                        0x00 => {
                            if (instr & 0xE0) >= 0x80 { //NOP, LDY, CPY, CPX
                                return AddrMode::Immediate;
                            }
                            if (instr & 0xE0) == 0x20 { //JSR
                                return AddrMode::Immediate;
                                // val_16 = self.get_absolute_addr();
                                // self.cycles += 2; //JSR takes 6, adding the extra 2 here
                            }
                        },
                        0x04 => {
                            return AddrMode::ZeroPage;
                        },
                        0x0C => {
                            if instr == 0x6C { //JMP
                                return AddrMode::Indirect;
                                // val_16 = self.read_indirect_16();
                            } else if instr == 0x4C { //JMP
                                return AddrMode::Absolute;
                                // val_16 = self.get_absolute_addr();
                                // self.cycles -= 1; //JMP absolute takes 3 not 4, but abs addr adds 2 cycles, so sub 1 off
                            } else {
                                return AddrMode::Absolute;
                            }
                        },
                        0x10 => {
                            return AddrMode::Immediate; //gonna have to transform these to a signed value for most branches
                        },
                        0x14 => {
                            return AddrMode::ZeroPageX;
                        },
                        0x1C => {
                            return AddrMode::AbsoluteX;
                        },
                        _ => unreachable!("impossible value range somehow")
                    }
                } else if write {
                    match instr & 0x1F {
                        0x04 => {
                            return AddrMode::ZeroPage;
                        },
                        0x0C => {
                            return AddrMode::Absolute;
                        },
                        0x14 => {
                            return AddrMode::ZeroPageX;
                        },
                        0x1C => {
                            return AddrMode::AbsoluteX;
                        },
                        _ => unreachable!("impossible value range somehow")
                    }
                }
            },
            0x01 | 0x05 | 0x09 | 0x0D | 0x11 | 0x15 | 0x19 | 0x1D => { //ALU instructions
                if read || write {
                    match instr & 0x1F {
                        0x01 => {
                            return AddrMode::IndirectX;
                        },
                        0x05 => {
                            return AddrMode::ZeroPage;
                        },
                        0x09 => {
                            return AddrMode::Immediate;
                        },
                        0x0D => {
                            return AddrMode::Absolute;
                        },
                        0x11 => {
                            return AddrMode::IndirectY;
                        },
                        0x15 => {
                            return AddrMode::ZeroPageX;
                        },
                        0x19 => {
                            return AddrMode::AbsoluteY;
                        },
                        0x1D => {
                            return AddrMode::AbsoluteX;
                        },
                        _ => unreachable!("impossible value range somehow")
                    }
                }
            },
            0x02 | 0x06 | 0x0A | 0x0E | 0x12 | 0x16 | 0x1A | 0x1E => { //RMW instructions
                return match instr & 0x1F {
                    0x02 => {
                        AddrMode::Immediate
                    },
                    0x06 => {
                        AddrMode::ZeroPage
                    },
                    0x0E => {
                        AddrMode::Absolute
                    },
                    0x16 => {
                        if instr == 0x96 || instr == 0xB6 {
                            AddrMode::ZeroPageY
                        } else {
                            AddrMode::ZeroPageX
                        }
                    },
                    0x1E => {
                        if instr == 0x9E || instr == 0xBE {
                            AddrMode::AbsoluteY
                        } else {
                            AddrMode::AbsoluteX
                        }
                    },
                    _ => unreachable!("impossible value range somehow")
                }
            },
            0x03 | 0x07 | 0x0B | 0x0F | 0x13 | 0x17 | 0x1B | 0x1F => { //RMW ALU instructions
                todo!()
            },
            _ => unreachable!("impossible value range somehow")
        }
        return AddrMode::Implied;
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
        self.s = self.s.wrapping_add(1);
        let addr = 0x0100u16 + self.s as u16;
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
        let high = self.pop_stack() as u16;
        let low = self.pop_stack() as u16;
        return (high << 8) | low;
    }

    fn read_indirect_16(&mut self) -> u16 {
        self.cycles += 3; //just for indirect JMP
        let low_1 = self.read(self.pc);
        let high_1 = self.read(self.pc.wrapping_add(1));
        let addr_1 = (high_1 as u16) << 8 | low_1 as u16;
        let addr_2 = (high_1 as u16) << 8 | low_1.wrapping_add(1) as u16; //weird bug where only the lower byte is incremented

        let low_2 = self.bus.borrow_mut().read(addr_1);
        let high_2 = self.bus.borrow_mut().read(addr_2);
        return (high_2 as u16) << 8 | low_2 as u16;
    }

    fn get_x_indirect_addr(&mut self) -> u16 {
        self.cycles += 4;
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
        self.cycles += 3; //todo: get the actual timing depending on page crossings and such
        //todo: and the dummy read
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
        self.cycles += 1; //todo RMWs take 5 not 6, need to subtract 1 later
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
        self.cycles += 2; //todo RMWs take 6 not 8, need to subtract 2 later
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
        self.cycles += 2;
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
        self.cycles += 2;
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
        self.cycles += 2; //todo: determine page crossing stuff
        //todo: RMWs take exactly 7 always, need to mess around with subtracting 1 and page crossing to ensure correct timing
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
        self.cycles += 2; //todo: determine page crossing stuff, and whatever is up with STA on this and abs x
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
