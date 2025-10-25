use crate::nes::mapper::mapper::Mapper;

#[derive(PartialEq, Eq)]
enum NromType {
    NROM128,
    NROM256,
}

struct NROM {
    nrom_type: NromType,
    has_chr_ram: bool,
    prg_rom_first: [u8; 0x4000],
    prg_rom_second: [u8; 0x4000],
    chr_rom: [u8; 0x2000], //can double as RAM if written to and supported by the header!
    prg_ram: [u8; 0x2000], //lol iNES extension, or used by family basic but that gets into iNES 2.0
}

impl NROM {
    pub fn new(header: &[u8; 0x10], prg_rom: &Vec<u8>, chr_rom: &Vec<u8>) -> NROM {
        let nromtype;
        if header[4] <= 1 {
            nromtype = NromType::NROM128;
        } else {
            nromtype = NromType::NROM256;
        }
        let splits = prg_rom.split_at(0x4000);
        let first: [u8; 0x4000] = splits.0.try_into().unwrap();
        let second: [u8; 0x4000] = match nromtype {
            NromType::NROM128 => { [0; 0x4000] }
            NromType::NROM256 => { splits.1.try_into().unwrap() }
        };
        NROM {
            nrom_type: nromtype,
            has_chr_ram: header[5] == 0,
            prg_rom_first: first,
            prg_rom_second: second,
            chr_rom: chr_rom.split_at(0x2000).0.try_into().unwrap(),
            prg_ram: [0; 0x2000],
        }
    }
}

impl Mapper for NROM {
    fn read_cpu(&mut self, addr: u16) -> u8 {
        if (0x6000..=0x7FFF).contains(&addr) {
            return self.prg_ram[addr as usize - 0x6000];
        } else if (0x8000..=0xBFFF).contains(&addr) {
            return self.prg_rom_first[addr as usize - 0x8000];
        } else if (0xC000..=0xFFFF).contains(&addr) {
            return match self.nrom_type {
                NromType::NROM128 => { self.prg_rom_second[addr as usize - 0xC000] }
                NromType::NROM256 => { self.prg_rom_first[addr as usize - 0xC000] }
            }
        }
        unreachable!("address outside NROM range")
    }

    fn write_cpu(&mut self, addr: u16, data: u8) {
        if (0x6000..=0x7FFF).contains(&addr) {
            self.prg_ram[addr as usize - 0x6000] = data;
        }
    }

    fn read_ppu(&mut self, addr: u16) -> u8 {
        if (0x0000..=0x1FFF).contains(&addr) {
            return self.chr_rom[addr as usize];
        }
        unreachable!("address outside NROM range")
    }

    fn write_ppu(&mut self, addr: u16, data: u8) {
        if self.has_chr_ram && (0x0000..=0x1FFF).contains(&addr) {
            self.chr_rom[addr as usize] = data;
        }
    }

    fn addr_mapped_cpu(&self, addr: u16) -> bool {
        return (0x6000..=0xFFFF).contains(&addr);
    }

    fn addr_mapped_ppu(&self, addr: u16) -> bool {
        return (0x0000..=0x1FFF).contains(&addr);
    }
}
