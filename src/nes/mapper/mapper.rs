pub trait Mapper {
    fn read_cpu(&mut self, addr: u16) -> u8;
    fn write_cpu(&mut self, addr: u16, data: u8);
    fn read_ppu(&mut self, addr: u16) -> u8;
    fn write_ppu(&mut self, addr: u16, data: u8);

    fn addr_mapped_cpu(&self, addr: u16) -> bool; //returns true if the address is mapped
    fn addr_mapped_ppu(&self, addr: u16) -> bool; //ditto but PPU space
}
