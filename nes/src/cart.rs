
// iNES format ROM
pub struct INES {
    nMapperId: usize,
    nPRGBanks: usize,
    vPRGMem: Box<[u8]>,
    vCHRMem: Box<[u8]>,
}

impl INES {
    pub fn new() -> INES {
	INES {
	    nMapperId: 0,
	    nPRGBanks: 1,
	    vPRGMem: Box::new([0; 1]),
	    vCHRMem: Box::new([0; 1])
	}
    }
}

#[derive(Clone, Copy, Debug)]
enum ReadOrWrite { Read, Write }

impl ReadOrWrite {
    fn is_read(&self) -> bool {
	match self {
	    ReadOrWrite::Read => true,
	    ReadOrWrite::Write => false,
	}
    }
}

#[derive(Clone, Copy, Debug)]
pub enum CpuOrPpu { Cpu, Ppu }

trait Mapper where {
    fn map(&self, cp: CpuOrPpu, rw: ReadOrWrite, addr: u16) -> Option<u16>;
}

struct Mapper000 {
    twoPRGBanks: bool
}

impl Mapper000 {
    fn new(twoPRGBanks: bool) -> Mapper000 {
	Mapper000 {
	    twoPRGBanks: twoPRGBanks
	}
    }
}

fn assert(b : bool) -> Option<()> {
    if b { Some(()) } else { None }
}

impl Mapper for Mapper000 {
    fn map(&self, cp: CpuOrPpu, rw: ReadOrWrite, addr: u16) -> Option<u16> {
	match cp {
	    CpuOrPpu::Cpu => {
		assert(addr >= 0x8000)?;
		Some(addr & (if self.twoPRGBanks {0x7FFF} else {0x3FFF}))
	    }
	    CpuOrPpu::Ppu => {
		assert(rw.is_read())?;
		assert(addr >= 0x8000)?;
		Some(addr)
	    }
	}
    }
}

pub struct Cart {
    rom: INES,
    mapper: Box<dyn Mapper>,
}

impl Cart {
    fn select_mapper(rom: &INES) -> Option<Box<dyn Mapper>> {
	match rom.nMapperId {
	    0 => Some(Box::new(Mapper000::new(rom.nPRGBanks > 1))),
	    _ => None
	}
    }
    pub fn new(rom: INES) -> Option<Cart> {
	let mapper = Cart::select_mapper(&rom)?;
	Some(Cart {
	    rom: rom,
	    mapper: mapper,
	})
    }
    pub fn read(&self, cp: CpuOrPpu, addr: u16) -> Option<u8> {
	let mapped_addr = self.mapper.map(cp, ReadOrWrite::Read, addr)? as usize;
	match cp {
	    CpuOrPpu::Cpu => Some(self.rom.vPRGMem[mapped_addr]),
	    CpuOrPpu::Ppu => Some(self.rom.vCHRMem[mapped_addr]),
	}
    }
    pub fn write(&mut self, cp: CpuOrPpu, addr: u16, byte: u8) -> Option<()> {
	let mapped_addr = self.mapper.map(cp, ReadOrWrite::Write, addr)? as usize;
	match cp {
	    CpuOrPpu::Cpu => self.rom.vPRGMem[mapped_addr] = byte,
	    CpuOrPpu::Ppu => self.rom.vCHRMem[mapped_addr] = byte,
	}
	Some(())
    }
}
