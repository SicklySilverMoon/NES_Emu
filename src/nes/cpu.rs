use std::cell::RefCell;
use std::rc::Rc;
use std::unreachable;

use crate::nes::bus::Bus;
use crate::nes::nes::Nes;

// #[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CpuReturnAction {
    None,
    Read(u16),
    ReadCallback(u16, Box<dyn Fn(u8, &mut Cpu)>),
    Write(u16, u8),
    WriteRead(u16, u8, u16),
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
    a: u8,
    x: u8,
    y: u8,

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

    total_cycles: u64, //how many total cycles have been executed since reset
    temp_cycles: u64, //tracelogging
}

impl Cpu {
    pub fn new() -> Cpu {
        return Cpu {
            pc: 0,
            a: 0,
            x: 0,
            y: 0,

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

            total_cycles: 0,
            temp_cycles: 0,
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

        self.total_cycles = 7; //todo
    }

    fn tracelog(&mut self) {
        if self.stage == CpuStage::FetchIns {
            self.temp_cycles = self.total_cycles;
            print!("${:04X}\t", self.pc);
            return;
        }

        let (read, write) = Cpu::instruction_needs_read_write(self.instruction);
        let mode = Cpu::instruction_read_write_type(self.instruction);

        let instr_str: &str;
        match self.instruction {
            0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71 => {
                instr_str = "ADC";
            },
            0x29 | 0x25 | 0x35 | 0x2D | 0x3D | 0x39 | 0x21 | 0x31 => {
                instr_str = "AND";
            },
            0x0A | 0x06 | 0x16 | 0x0E | 0x1E => {
                instr_str = "ASL";
            },
            0x90 => {
                instr_str = "BCC";
            },
            0xB0 => {
                instr_str = "BCS";
            },
            0xF0 => {
                instr_str = "BEQ";
            },
            0x24 | 0x2C => {
                instr_str = "BIT";
            },
            0x30 => {
                instr_str = "BMI";
            },
            0xD0 => {
                instr_str = "BNE";
            },
            0x10 => {
                instr_str = "BPL";
            },
            0x00 => {
                instr_str = "BRK";
            },
            0x50 => {
                instr_str = "BVC";
            },
            0x70 => {
                instr_str = "BVS";
            },
            0x18 => {
                instr_str = "CLC";
            },
            0xD8 => {
                instr_str = "CLD";
            },
            0x58 => {
                instr_str = "CLI";
            },
            0xB8 => {
                instr_str = "CLV";
            },
            0xC9 | 0xC5 | 0xD5 | 0xCD | 0xDD | 0xD9 | 0xC1 | 0xD1 => {
                instr_str = "CMP";
            },
            0xE0 | 0xE4 | 0xEC => {
                instr_str = "CPX";
            },
            0xC0 | 0xC4 | 0xCC => {
                instr_str = "CPY";
            },
            0xC6 | 0xD6 | 0xCE | 0xDE => {
                instr_str = "DEC";
            },
            0xCA => {
                instr_str = "DEX";
            },
            0x88 => {
                instr_str = "DEY";
            },
            0x49 | 0x45 | 0x55 | 0x4D | 0x5D | 0x59 | 0x41 | 0x51 => {
                instr_str = "EOR";
            },
            0xE6 | 0xF6 | 0xEE | 0xFE => {
                instr_str = "INC";
            },
            0xE8 => {
                instr_str = "INX";
            },
            0xC8 => {
                instr_str = "INY";
            },
            0x4C | 0x6C => {
                instr_str = "JMP";
            },
            0x20 => {
                instr_str = "JSR";
            },
            0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => {
                instr_str = "LDA";
            },
            0xA2 | 0xA6 | 0xB6 | 0xAE | 0xBE => {
                instr_str = "LDX";
            },
            0xA0 | 0xA4 | 0xB4 | 0xAC | 0xBC => {
                instr_str = "LDY";
            },
            0x4A | 0x46 | 0x56 | 0x4E | 0x5E => {
                instr_str = "LSR";
            },
            0xEA => {
                instr_str = "NOP";
            },
            0x09 | 0x05 | 0x15 | 0x0D | 0x1D | 0x19 | 0x01 | 0x11 => {
                instr_str = "ORA";
            },
            0x48 => {
                instr_str = "PHA";
            },
            0x08 => {
                instr_str = "PHP";
            },
            0x68 => {
                instr_str = "PLA";
            },
            0x28 => {
                instr_str = "PLP";
            },
            0x2A | 0x26 | 0x36 | 0x2E | 0x3E => {
                instr_str = "ROL";
            },
            0x6A | 0x66 | 0x76 | 0x6E | 0x7E => {
                instr_str = "ROR";
            },
            0x40 => {
                instr_str = "RTI";
            },
            0x60 => {
                instr_str = "RTS";
            },
            0xE9 | 0xE5 | 0xF5 | 0xED | 0xFD | 0xF9 | 0xE1 | 0xF1 => {
                instr_str = "SBC";
            },
            0x38 => {
                instr_str = "SEC";
            },
            0xF8 => {
                instr_str = "SED";
            },
            0x78 => {
                instr_str = "SEI";
            },
            0x85 | 0x95 | 0x8D | 0x9D | 0x99 | 0x81 | 0x91 => {
                instr_str = "STA";
            },
            0x86 | 0x96 | 0x8E => {
                instr_str = "STX";
            },
            0x84 | 0x94 | 0x8C => {
                instr_str = "STY";
            },
            0xAA => {
                instr_str = "TAX";
            },
            0xA8 => {
                instr_str = "TAY";
            },
            0xBA => {
                instr_str = "TSX";
            },
            0x8A => {
                instr_str = "TXA";
            },
            0x9A => {
                instr_str = "TXS";
            },
            0x98 => {
                instr_str = "TYA";
            },
            0x02 => {
                instr_str = "STP";
            }
            _ => {
                todo!("unimplemented instruction: {:02X}", self.instruction);
            }
        }
        match mode {
            AddrMode::Implied => {
                print!("{:02X}\t\t{}\t\t", self.instruction, instr_str);
            }
            AddrMode::Immediate => {
                print!("{:02X} {:02X}\t\t{} #{:02X}\t\t", self.instruction, self.operand1, instr_str, self.operand1);
            }
            AddrMode::ZeroPage => {
                print!("{:02X} {:02X}\t\t{} <{:02X}\t\t", self.instruction, self.operand1, instr_str, self.operand1);
            }
            AddrMode::ZeroPageX => {
                print!("{:02X} {:02X}\t\t{} <{:02X}, X\t", self.instruction, self.operand1, instr_str, self.operand1);
            }
            AddrMode::ZeroPageY => {
                print!("{:02X} {:02X}\t\t{} <{:02X}, Y\t", self.instruction, self.operand1, instr_str, self.operand1);
            }
            AddrMode::Absolute => {
                print!("{:02X}\t\t{} ${:04X}\t", self.instruction, instr_str, self.addr);
            }
            AddrMode::AbsoluteX => {
                print!("{:02X}\t\t{} ${:04X}, X\t", self.instruction, instr_str, self.addr);
            }
            AddrMode::AbsoluteY => {
                print!("{:02X}\t\t{} ${:04X}, Y\t", self.instruction, instr_str, self.addr);
            }
            AddrMode::Indirect => {
                print!("{:02X} {:02X} {:02X}\t{} (${:04X})\t", self.instruction, self.operand1, self.operand2, instr_str, self.addr);
            }
            AddrMode::IndirectX => {
                print!("{:02X}\t\t{} (${:04X}, X)\t", self.instruction, instr_str, self.addr);
            }
            AddrMode::IndirectY => {
                print!("{:02X}\t\t{} (${:04X}), Y\t", self.instruction, instr_str, self.addr);
            }
            AddrMode::Accumulator => {
                print!("{:02X}\t\t{} A\t\t", self.instruction, instr_str);
            }
        }
        print!("A: {:02X} X: {:02X} Y: {:02X} SP: {:02X}\t", self.a, self.x, self.y, self.s);
        if self.n {
            print!("N");
        } else {
            print!("n");
        }
        if self.v {
            print!("V");
        } else {
            print!("v");
        }
        print!("--");
        if self.d {
            print!("D");
        } else {
            print!("d");
        }
        if self.i {
            print!("I");
        } else {
            print!("i");
        }
        if self.z {
            print!("Z");
        } else {
            print!("z");
        }
        if self.c {
            print!("C");
        } else {
            print!("c");
        }
        println!("\tCycle: {}", self.temp_cycles);
    }

    pub fn step(&mut self, val: Option<u8>) -> CpuReturnAction { //return an address that the NES must read, or an address to write, or nothing
        if self.stage == CpuStage::FetchIns {
            self.tracelog();
        }
        self.total_cycles += 1;

        if self.stage == CpuStage::FetchIns {
            self.cycles = 0; //Couple instruction impls use this to track their internal stage, so we need to reset it here
            self.stage = CpuStage::Decode;
            let ret = CpuReturnAction::Read(self.pc);
            self.pc = self.pc.wrapping_add(1);
            return ret;
        } else if self.stage == CpuStage::Decode {
            self.instruction = val.unwrap(); //shouldn't panic
            let (read, write) = Cpu::instruction_needs_read_write(self.instruction);
            let mode = Cpu::instruction_read_write_type(self.instruction);
            return if read || write {
                self.stage = CpuStage::FetchOp1;
                let ret = CpuReturnAction::Read(self.pc);
                self.pc = self.pc.wrapping_add(1);
                ret
            } else if mode == AddrMode::Implied {
                self.stage = CpuStage::FetchOp1;
                CpuReturnAction::Read(self.pc)
            } else {
                self.stage = CpuStage::Execute;
                CpuReturnAction::None
            }
        } else if self.stage == CpuStage::FetchOp1 {
            self.operand1 = val.unwrap();

            let (read, write) = Cpu::instruction_needs_read_write(self.instruction);
            let mode = Cpu::instruction_read_write_type(self.instruction);
            match mode { //todo: start splitting on read and writes, as some stuff just reads, some stuff just needs ops for writes, and some weird ones (indexed stuff) do both
                AddrMode::Implied => { //do nothing with the read value
                    self.stage = CpuStage::Execute;
                    return CpuReturnAction::None;
                }
                AddrMode::Immediate => {
                    self.stage = CpuStage::Execute;
                    //notably no return, go straight to execute
                }
                AddrMode::ZeroPage => {
                    self.stage = CpuStage::Execute;
                    self.addr = self.operand1 as u16;
                    if read {
                        return CpuReturnAction::Read(self.addr);
                    }
                }
                AddrMode::ZeroPageX | AddrMode::ZeroPageY => {
                    self.stage = CpuStage::FetchAddrAdjust;
                    return CpuReturnAction::Read(self.operand1 as u16);
                }
                AddrMode::Absolute | AddrMode::AbsoluteX | AddrMode::AbsoluteY |
                AddrMode::Indirect => {
                    self.stage = CpuStage::FetchOp2;
                    let ret = CpuReturnAction::Read(self.pc);
                    self.pc = self.pc.wrapping_add(1);
                    return ret;
                }
                AddrMode::IndirectX | AddrMode::IndirectY => {
                    self.stage = CpuStage::FetchAddrAdjust;
                    let ret = CpuReturnAction::Read(self.pc);
                    // self.pc = self.pc.wrapping_add(1);
                    return ret;
                }
                _ => {
                    println!("Fallthrough on FetchOp1 with {:?}", self.instruction);
                    self.stage = CpuStage::Execute;
                    return CpuReturnAction::None;
                }
            }
        } else if self.stage == CpuStage::FetchOp2 {
            self.operand2 = val.unwrap();

            let (read, write) = Cpu::instruction_needs_read_write(self.instruction);
            let mode = Cpu::instruction_read_write_type(self.instruction);
            match mode { //todo: some must fetch more, some must go right to execute, pretty easy to determine what's what
                AddrMode::Absolute => {
                    self.stage = CpuStage::Execute;
                    self.addr = (self.operand1 as u16) | ((self.operand2 as u16) << 8);
                    return CpuReturnAction::Read(self.addr);
                }
                AddrMode::AbsoluteX | AddrMode::AbsoluteY => {
                    self.stage = CpuStage::Execute;
                    self.addr = (self.operand1 as u16) | ((self.operand2 as u16) << 8);
                    return CpuReturnAction::Read(self.addr);
                }
                AddrMode::Indirect => {
                    //full indirect is only ever used for JMP
                    self.stage = CpuStage::FetchAddrAdjust;
                    self.addr = (self.operand1 as u16) | ((self.operand2 as u16) << 8);
                    return CpuReturnAction::Read(self.addr);
                }
                _ => { println!("Fallthrough on FetchOp2 with {:?}", self.instruction); self.stage = CpuStage::Execute; return CpuReturnAction::None }
            }
        } else if self.stage == CpuStage::FetchAddrAdjust {
            //do nothing with the value, it's a dummy read
            let (read, write) = Cpu::instruction_needs_read_write(self.instruction);
            let mode = Cpu::instruction_read_write_type(self.instruction);
            match mode {
                AddrMode::ZeroPageX => {
                    self.addr = self.operand1.wrapping_add(self.x) as u16;
                    self.stage = CpuStage::Execute;
                    if read {
                        return CpuReturnAction::Read(self.addr);
                    }
                }
                AddrMode::ZeroPageY => {
                    self.addr = self.operand1.wrapping_add(self.y) as u16;
                    self.stage = CpuStage::Execute;
                    if read {
                        return CpuReturnAction::Read(self.addr);
                    }
                }
                AddrMode::AbsoluteX => {
                    let (op1, wrapped) = self.operand1.overflowing_add(self.x);
                    self.addr = (op1 as u16) | ((self.operand2 as u16) << 8);
                    let ret = CpuReturnAction::Read(self.addr);
                    if wrapped || (read && write) {
                        self.stage = CpuStage::FetchAddrAdjust2;
                    } else {
                        self.stage = CpuStage::Execute;
                    }
                    return ret;
                }
                AddrMode::AbsoluteY => {
                    let (op1, wrapped) = self.operand1.overflowing_add(self.x);
                    self.addr = (op1 as u16) | ((self.operand2 as u16) << 8);
                    let ret = CpuReturnAction::Read(self.addr);
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
                    self.stage = CpuStage::FetchAddrAdjust2;
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
                _ => { println!("Fallthrough on FetchAddrAdjust with {:?}", self.instruction); self.stage = CpuStage::Execute; return CpuReturnAction::None }
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
                    self.addr = addr;
                    return CpuReturnAction::Read(self.addr);
                }
                AddrMode::Indirect => { //Once more it is just JMP
                    self.addr |= (val.unwrap() as u16) << 8;
                    self.stage = CpuStage::Execute;
                    //No return, straight to execute
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
                _ => { println!("Fallthrough on FetchAddrAdjust2 with {:?}", self.instruction); self.stage = CpuStage::Execute; return CpuReturnAction::None }
            }
        } else if self.stage == CpuStage::FetchAddrAdjust3 {
            //do nothing with the value, it's a dummy read
            let (read, write) = Cpu::instruction_needs_read_write(self.instruction);
            let mode = Cpu::instruction_read_write_type(self.instruction);
            match mode {
                AddrMode::IndirectX => {
                    self.addr |= (val.unwrap() as u16) << 8; //load high byte
                    self.stage = CpuStage::Execute;
                    if read {
                        return CpuReturnAction::Read(self.addr); //todo: do branch on reading and writing here
                    } else {
                        return CpuReturnAction::None;
                    }
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
                _ => { println!("Fallthrough on FetchAddrAdjust3 with {:?}", self.instruction); self.stage = CpuStage::Execute; return CpuReturnAction::None }
            }
        } else if self.stage == CpuStage::FetchAddrAdjust4 {
            self.val = val.unwrap();
            let mode = Cpu::instruction_read_write_type(self.instruction);
            match mode {
                AddrMode::IndirectY => {
                    self.stage = CpuStage::Execute;
                    return CpuReturnAction::Read(self.addr);
                }
                _ => { println!("Fallthrough on FetchAddrAdjust4 with {:?}", self.instruction); self.stage = CpuStage::Execute; return CpuReturnAction::None }
            }
        }

        if self.stage == CpuStage::Execute {
            if self.cycles == 0 {
                self.tracelog();
            }
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
                    if self.cycles == 0 { //todo: add hijacking lol
                        self.cycles += 1;
                        return self.push_stack((self.pc >> 8) as u8);
                    } else if self.cycles == 1 {
                        self.cycles += 1;
                        return self.push_stack(self.pc as u8);
                    } else if self.cycles == 2 {
                        self.cycles += 1;
                        if let CpuReturnAction::Write(addr, val) = self.push_stack_flags() {
                            return CpuReturnAction::WriteRead(addr, val, 0xFFFE);
                        } else {
                            unreachable!("BRK should always push flags");
                        }
                    } else if self.cycles == 3 {
                        self.cycles += 1;
                        self.addr = self.val as u16;
                        return CpuReturnAction::Read(0xFFFF);
                    } else if self.cycles == 4 {
                        self.cycles += 1;
                        self.addr |= (self.val as u16) << 8;
                        self.i = true;
                        self.pc = self.addr;
                    }
                } else if op == 0x20 { //JSR
                    if self.cycles == 0 {
                        self.cycles += 1;
                        return CpuReturnAction::Read(self.get_stack_addr());
                    } else if self.cycles == 1 {
                        self.cycles += 1;
                        return self.push_stack((self.pc >> 8) as u8);
                    } else if self.cycles == 2 {
                        self.cycles += 1;
                        return self.push_stack((self.pc) as u8);
                    } else if self.cycles == 3 {
                        self.cycles += 1;
                        let pc_old = self.pc;
                        self.pc = self.val as u16;
                        self.stage = CpuStage::FetchIns;
                        return CpuReturnAction::ReadCallback(pc_old, Box::new(|val, cpu: &mut Cpu| {cpu.pc |= (val as u16) << 8})); //fetch straight into PCH
                    }
                } else if op == 0x40 { //RTI
                    if self.cycles == 0 {
                        self.cycles += 1;
                        return self.pop_stack();
                    } else if self.cycles == 1 {
                        self.cycles += 1;
                        self.restore_flags(self.val);
                        return self.pop_stack();
                    } else if self.cycles == 2 {
                        self.cycles += 1;
                        self.addr = self.val as u16;
                        return self.pop_stack();
                    } else if self.cycles == 3 {
                        self.pc = self.addr | ((self.val as u16) << 8);
                    }
                } else if op == 0x60 { //RTS
                    if self.cycles == 0 {
                        self.cycles += 1;
                        return self.pop_stack();
                    } else if self.cycles == 1 {
                        self.cycles += 1;
                        self.addr = self.val as u16;
                        return self.pop_stack();
                    } else if self.cycles == 2 {
                        self.cycles += 1;
                        self.addr |= (self.val as u16) << 8;
                        return CpuReturnAction::None;
                    } else if self.cycles == 3 {
                        self.pc = self.addr.wrapping_add(1);
                    }
                }
            } //0x04 is covered by earlier cases (NOP, BIT, STY, LDY, CPY, CPX)
            else if op & 0x1F == 0x08 {
                if op == 0x08 { //PHP
                    self.stage = CpuStage::FetchIns;
                    return self.push_stack_flags();
                } else if op == 0x28 { //PLP
                    if self.cycles == 0 {
                        self.cycles += 1;
                        return self.pop_stack();
                    } else if self.cycles == 1 {
                        self.restore_flags(self.val);
                    }
                } else if op == 0x48 { //PHA
                    self.stage = CpuStage::FetchIns;
                    return self.push_stack(self.a);
                } else if op == 0x68 { //PLA
                    if self.cycles == 0 {
                        self.cycles += 1;
                        return self.pop_stack();
                    } else if self.cycles == 1 {
                        self.a = self.val;
                        self.z = self.a == 0;
                        self.n = self.a & 0x80 == 0x80;
                    }
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
                self.pc = self.addr; //JMP
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
        self.stage = CpuStage::FetchIns;
        if write {
            return CpuReturnAction::Write(self.addr, self.val);
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
        self.stage = CpuStage::FetchIns;
        if write {
            return CpuReturnAction::Write(self.addr, self.val);
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
        if read && write {
            if self.cycles == 0 {
                self.cycles += 1;
                return CpuReturnAction::Write(self.addr, self.val);
            }
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

        self.stage = CpuStage::FetchIns;
        if write {
            return CpuReturnAction::Write(self.addr, self.val);
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
                        if (op & 0xE0 >= 0x80) || (op & 0xE0 == 0x20) || op == 0x00 { //bunch of loads, a JSR, and BRK
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
                            return AddrMode::Immediate; //BRK
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
                    0x0A | 0x12 | 0x1A => {
                        AddrMode::Implied
                    }
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

    fn get_stack_addr(&self) -> u16 {
        return 0x0100u16 + self.s as u16;
    }

    fn push_stack(&mut self, val: u8) -> CpuReturnAction {
        let addr = self.get_stack_addr();
        self.s = self.s.wrapping_sub(1);
        return CpuReturnAction::Write(addr, val);
    }

    fn push_stack_flags(&mut self) -> CpuReturnAction {
        let val: u8 = (self.n as u8) << 7 | (self.v as u8) << 6 | 1 << 5 | 1 << 4 | (self.d as u8) << 3 | (self.i as u8) << 2 | (self.z as u8) << 1 | (self.c as u8) << 0;
        return self.push_stack(val);
    }

    fn pop_stack(&mut self) -> CpuReturnAction {
        self.s = self.s.wrapping_add(1);
        let addr = self.get_stack_addr();
        return CpuReturnAction::Read(addr);
    }

    fn restore_flags(&mut self, flags: u8) {
        self.n = (flags & 0b10000000) != 0;
        self.v = (flags & 0b01000000) != 0;
        //note the two values missing, that's intentional
        self.d = (flags & 0b00001000) != 0;
        self.i = (flags & 0b00000100) != 0;
        self.z = (flags & 0b00000010) != 0;
        self.c = (flags & 0b00000001) != 0;
    }
}
