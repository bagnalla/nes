use cpu::IO;

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
    pub sz_prgrom: u8,
    sz_chrrom: u8,
    name_tbl_arrangement: Arrangement,
    battery_prgram: bool,
    alt_name_tbl_layout: bool,
    pub mapper_id: u8,
    vs_unisystem: bool,
    play_choice: bool,
    nes2: bool,
    trainer: Option<Box<[u8; 512]>>,
    pub prgrom: Vec<u8>,
    pub chrrom: Vec<u8>,
}

impl From<Vec<u8>> for INES {
    fn from(bytes: Vec<u8>) -> INES {
	let sz_prgrom = bytes[4];
	let sz_chrrom = bytes[5];
	
	let flags6 = bytes[6];
	let name_tbl_arrangement = if flags6 & 0b00000001 > 0 {
	    Arrangement::Vertical
	} else {
	    Arrangement::Horizontal
	};
	let battery_prgram = flags6 & 0b00000010 > 1;
	let has_trainer = flags6 & 0b00000100 > 1;
	let alt_name_tbl_layout = flags6 & 0b00001000 > 1;
	let mapper_id_lo = flags6 & 0xF0;

	let flags7 = bytes[7];
	let vs_unisystem = flags7 & 0b00000001 > 1;
	let play_choice = flags7 & 0b00000010 > 1;
	let nes2 = flags7 & 0b00001100 == 2;
	let mapper_id_hi = flags7 & 0xF0;
	let mapper_id = (mapper_id_hi << 4) | mapper_id_lo;

	let _sz_prgram = bytes[8];

	// Ignore bytes[9]

	let flags10 = bytes[10];
	let _tv_system = match flags10 {
	    0 => TVSystem::NTSC,
	    2 => TVSystem::PAL,
	    _ => TVSystem::Dual,
	};

	// Ignore last four bytes of header for now

	let mut offset: usize = 16;

	let trainer: Option<Box<[u8; 512]>> = if has_trainer {
	    let mut a = [0 as u8; 512];
	    a.copy_from_slice(&bytes[16..16+512]);
	    offset += 512;
	    Some(Box::new(a))
	} else {
	    None
	};

	let prgrom: Vec<u8> =
	    bytes[offset .. offset + 16384 * sz_prgrom as usize].to_vec();
	offset += 16384 * sz_prgrom as usize;

	let chrrom: Vec<u8> =
	    bytes[offset .. offset + 8192 * sz_chrrom as usize].to_vec();
	// offset += 16384 * szPRGROM as usize;

	INES {
	    sz_prgrom: sz_prgrom,
	    sz_chrrom: sz_chrrom,
	    name_tbl_arrangement: name_tbl_arrangement,
	    battery_prgram: battery_prgram,
	    alt_name_tbl_layout: alt_name_tbl_layout,
	    mapper_id: mapper_id,
	    vs_unisystem: vs_unisystem,
	    play_choice: play_choice,
	    nes2: nes2,
	    trainer: trainer,
	    prgrom: prgrom,
	    chrrom: chrrom,
	}
    }
}

// #[derive(Clone, Copy, Debug)]
// pub enum ReadOrWrite { Read, Write }

// impl ReadOrWrite {
//     fn is_read(&self) -> bool {
// 	match self {
// 	    ReadOrWrite::Read => true,
// 	    ReadOrWrite::Write => false,
// 	}
//     }
// }

// impl From<IO> for ReadOrWrite {
//     fn from(event_type: IO) -> ReadOrWrite {
// 	match event_type {
// 	    IO::Read => ReadOrWrite::Read,
// 	    IO::Write => ReadOrWrite::Write,
// 	}
//     }
// }

// impl From<PpuEventType> for ReadOrWrite {
//     fn from(event_type: PpuEventType) -> ReadOrWrite {
// 	match event_type {
// 	    PpuEventType::Read => ReadOrWrite::Read,
// 	    PpuEventType::Write(_) => ReadOrWrite::Write,
// 	}
//     }
// }

fn assert(b : bool) -> Option<()> {
    if b { Some(()) } else { None }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum CpuOrPpu { Cpu, Ppu }

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum MapTarget { Prg, Chr }

// #[derive(Clone, Copy, Debug)]
// pub enum MapTarget { Default, Cartridge(PrgOrChr) }

pub trait Mapper where {
    fn try_map(&self, cp: CpuOrPpu, rw: IO, addr: u16) -> Option<(MapTarget, u16)>;
    fn map(&self, cp: CpuOrPpu, rw: IO, addr: u16) -> Option<(MapTarget, u16)> {
	// Reads/writes to 0x4015 don't even get to this point.
	// if cp == CpuOrPpu::Cpu && addr == 0x4015 {
	//     return Some((MapTarget::Default, addr))
	// }

	// println!("{:?} {:?} {:04x}", cp, rw, addr);

	// Pass the address through the mapper. Even if it doesn't map
	// to anything, the mapper can change its internal state via
	// passive observation of the read/write.
	let mapped = self.try_map(cp, rw, addr);

	// Palette table read/writes not remappable.
	assert(!(cp == CpuOrPpu::Ppu && 0x3F00 <= addr && addr < 0x4000))?;

	// if addr < 0x4013 || addr == 0x4017 {
	//     return Some((MapTarget::Default, addr))

	mapped
    }
}

struct Mapper000 {
    two_prg_banks: bool
}

impl Mapper000 {
    fn new(two_prg_banks: bool) -> Mapper000 {
	Mapper000 {
	    two_prg_banks: two_prg_banks
	}
    }
}

impl Mapper for Mapper000 {
    fn try_map(&self, cp: CpuOrPpu, rw: IO, addr: u16)
	       -> Option<(MapTarget, u16)> {
	match cp {
	    CpuOrPpu::Cpu => {
		// if addr < 0x4013 || addr == 0x4017 {
		//     return Some((MapTarget::Default, addr))
		// }
		assert(addr >= 0x8000)?;
		Some((MapTarget::Prg,
		      addr & (if self.two_prg_banks { 0x7FFF } else { 0x3FFF })))
	    }
	    _ => None
	    }
	    // CpuOrPpu::Ppu => {
	    // 	assert(rw.is_read())?;
	    // 	// assert(addr >= 0x8000)?;
	    // 	Some((MapTarget::Default, addr))
	// }
    }
}

pub struct Cart {
    pub rom: INES,
    pub mapper: Box<dyn Mapper>,
}

impl Cart {
    fn select_mapper(rom: &INES) -> Option<Box<dyn Mapper>> {
	match rom.mapper_id {
	    0 => Some(Box::new(Mapper000::new(rom.sz_chrrom > 1))),
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
    pub fn read(&self, tgt: MapTarget, addr: u16) -> Result<u8, String> {
	match tgt {
	    MapTarget::Prg => self.rom.prgrom.get(addr as usize)
		.ok_or(format!("PRGROM read out of bounds: {:04x}", addr)).copied(),
	    MapTarget::Chr => self.rom.chrrom.get(addr as usize)
		.ok_or(format!("CHRROM read out of bounds: {:04x}", addr)).copied(),
	}
    }
    pub fn write(&mut self, cp: MapTarget, addr: u16, byte: u8)
		 -> Result<(), String> {
	match cp {
	    MapTarget::Prg => {
		if (addr as usize) < self.rom.prgrom.len() {
		    self.rom.prgrom[addr as usize] = byte
		} else {
		    return Err(format!("PRGROM write out of bounds: {:04x}", addr))
		}
	    }
	    MapTarget::Chr => {
		if (addr as usize) < self.rom.chrrom.len() {
		    self.rom.chrrom[addr as usize] = byte
		} else {
		    return Err(format!("CHRROM write out of bounds: {:04x}", addr))
		}
	    }
	}
	Ok(())
    }
}
