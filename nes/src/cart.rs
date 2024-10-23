
// struct INESHeader {
//     szPRGROM: usize,
//     szCHRROM: usize,    
// }

#[derive(Clone, Debug)]
enum Arrangement { Horizontal, Vertical }

#[derive(Clone, Debug)]
enum TVSystem { NTSC, PAL, Dual }

// iNES format ROM
#[derive(Clone, Debug)]
pub struct INES {
    szPRGROM: u8,
    szCHRROM: u8,
    nameTblArrangement: Arrangement,
    batteryPRGRAM: bool,
    altNameTblLayout: bool,
    mapperId: u8,
    vsUnisystem: bool,
    playChoice: bool,
    nes2: bool,

    trainer: Option<Box<[u8; 512]>>,
    pub prgROM: Vec<u8>,
    pub chrROM: Vec<u8>,
}

impl From<Vec<u8>> for INES {
    fn from(bytes: Vec<u8>) -> INES {
	let szPRGROM = bytes[4];
	let szCHRROM = bytes[5];
	
	let flags6 = bytes[6];
	let nameTblArrangement = if flags6 & 0b00000001 > 0 {
	    Arrangement::Vertical
	} else {
	    Arrangement::Horizontal
	};
	let batteryPRGRAM = flags6 & 0b00000010 > 1;
	let hasTrainer = flags6 & 0b00000100 > 1;
	let altNameTblLayout = flags6 & 0b00001000 > 1;
	let mapperId_lo = flags6 & 0xF0;

	let flags7 = bytes[7];
	let vsUnisystem = flags7 & 0b00000001 > 1;
	let playChoice = flags7 & 0b00000010 > 1;
	let nes2 = flags7 & 0b00001100 == 2;
	let mapperId_hi = flags7 & 0xF0;
	let mapperId = (mapperId_hi << 4) | mapperId_lo;

	let szPRGRAM = bytes[8];

	// Ignore bytes[9]

	let flags10 = bytes[10];
	let tvSystem = match bytes[10] {
	    0 => TVSystem::NTSC,
	    2 => TVSystem::PAL,
	    _ => TVSystem::Dual,
	};

	// Ignore last four bytes of header for now

	let mut offset: usize = 16;

	let trainer: Option<Box<[u8; 512]>> = if hasTrainer {
	    let mut a = [0 as u8; 512];
	    a.copy_from_slice(&bytes[16..16+512]);
	    offset += 512;
	    Some(Box::new(a))
	} else {
	    None
	};

	let prgROM: Vec<u8> =
	    bytes[offset .. offset + 16384 * szPRGROM as usize].to_vec();
	offset += 16384 * szPRGROM as usize;

	let chrROM: Vec<u8> =
	    bytes[offset .. offset + 8192 * szCHRROM as usize].to_vec();
	// offset += 16384 * szPRGROM as usize;

	INES {
	    szPRGROM: szPRGROM,
	    szCHRROM: szCHRROM,
	    nameTblArrangement: nameTblArrangement,
	    batteryPRGRAM: batteryPRGRAM,
	    altNameTblLayout: altNameTblLayout,
	    mapperId: mapperId,
	    vsUnisystem: vsUnisystem,
	    playChoice: playChoice,
	    nes2: nes2,
	    trainer: trainer,
	    prgROM: prgROM,
	    chrROM: chrROM,
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
	match rom.mapperId {
	    0 => Some(Box::new(Mapper000::new(rom.szCHRROM > 1))),
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
	    CpuOrPpu::Cpu => Some(self.rom.prgROM[mapped_addr]),
	    CpuOrPpu::Ppu => Some(self.rom.chrROM[mapped_addr]),
	}
    }
    pub fn write(&mut self, cp: CpuOrPpu, addr: u16, byte: u8) -> Option<()> {
	let mapped_addr = self.mapper.map(cp, ReadOrWrite::Write, addr)? as usize;
	match cp {
	    CpuOrPpu::Cpu => self.rom.prgROM[mapped_addr] = byte,
	    CpuOrPpu::Ppu => self.rom.chrROM[mapped_addr] = byte,
	}
	Some(())
    }
}
