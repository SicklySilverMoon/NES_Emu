use crate::nes::mapper::nrom::NROM;

pub trait Mapper {
    fn read_cpu(&mut self, addr: u16) -> u8;
    fn write_cpu(&mut self, addr: u16, data: u8);
    fn read_ppu(&mut self, addr: u16) -> u8;
    fn write_ppu(&mut self, addr: u16, data: u8);

    fn addr_mapped_cpu(&self, addr: u16) -> bool; //returns true if the address is mapped
    fn addr_mapped_ppu(&self, addr: u16) -> bool; //ditto but PPU space
}

pub fn create_mapper(header: &[u8; 0x10], prg_rom: &Vec<u8>, chr_rom: &Vec<u8>) -> Box<dyn Mapper> {
    match (header[7] & 0xF0) | (header[6] >> 4) { //yeah the mapper is split across 2 flags, great isn't it?
        0 => { return Box::new(NROM::new(header, prg_rom, chr_rom)) }
        _ => unreachable!("Mapper not implemented!")
    }
}
