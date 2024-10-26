#![feature(coroutines, coroutine_trait, stmt_expr_attributes)]

use std::ops::{Coroutine};
use std::fmt;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicU16, Ordering};

use atomic_bitfield::AtomicBitField as _;
use bitflags::bitflags;

use crate::cart::{Cart, CpuOrPpu, MapTarget};
use cpu::IO;

// #[derive(Debug)]
// pub enum Bus {
//     Cpu,
//     Ppu
// }

// #[derive(Debug)]
// pub enum PpuEventType {
//     Read,
//     Write(u8),
// }

// // Target bus
// pub enum PpuEventTarget {
//     Cpu,
//     Ppu,
// }

// // Every PPU event contains a target bus, an address on that bus, and
// // is either a read or a write to it.
// pub type PpuEvent = (PpuEventTarget, u16, PpuEventType);

// https://www.nesdev.org/wiki/PPU_palettes "Note that entry 0 of
// each palette is also unique in that its color value is shared
// between the background and sprite palettes, so writing to
// either one updates the same internal storage."
fn paletteTblAddr(addr: u16) -> u16 {
    match addr {
	0x0010 => 0x0000,
	0x0014 => 0x0004,
	0x0018 => 0x0008,
	0x001C => 0x000C,
	_ => addr,
    }
}

bitflags! {
    #[derive(Clone, Copy, Debug)]
    pub struct PpuStatus: u8 {
	const SpriteOverflow = 1 << 5;
	const SpriteZeroHit = 1 << 6;
	const VerticalBlank = 1 << 7;
    }
}

impl From<PpuStatus> for usize {
    fn from(f: PpuStatus) -> usize {
	f.bits() as usize
    }
}

bitflags! {
    #[derive(Clone, Copy, Debug)]
    pub struct PpuMask: u8 {
	const Greyscale = 1 << 0;
	const RenderBackgroundLeft = 1 << 1;
	const RenderSpritesLeft = 1 << 2;
	const RenderBackground = 1 << 3;
	const RenderSprites = 1 << 4;
	const EnhanceRed = 1 << 5;
	const EnhanceGreen = 1 << 6;
	const EnhanceBlue = 1 << 7;
    }
}

impl From<PpuMask> for usize {
    fn from(f: PpuMask) -> usize {
	f.bits() as usize
    }
}

bitflags! {
    #[derive(Clone, Copy, Debug)]
    pub struct PpuCtrl: u8 {
	const NameTblX = 1 << 0;
	const NameTblY = 1 << 1;
	const IncrementMode = 1 << 2;
	const PatternSprite = 1 << 3;
	const PatternBackground = 1 << 4;
	const SpriteSize = 1 << 5;
	const SlaveMode = 1 << 6;
	const EnableNmi = 1 << 7;
    }
}

impl From<PpuCtrl> for usize {
    fn from(f: PpuCtrl) -> usize {
	f.bits() as usize
    }
}

#[derive(Clone, Debug)]
pub struct PpuRegs {
    ctrl:    Arc<AtomicU8>,
    mask:    Arc<AtomicU8>,
    status:  Arc<AtomicU8>,
    oamaddr: Arc<AtomicU8>,
    oamdata: Arc<AtomicU8>,
    scroll:  Arc<AtomicU16>,
    addr:    Arc<AtomicU16>,
    data:    Arc<AtomicU8>,
    oamdma:  Arc<AtomicU8>,

    // Internal registers
    v: Arc<AtomicU16>,
    t: Arc<AtomicU16>,
    x: Arc<AtomicU8>,
    w: Arc<AtomicBool>,
    
    read_buf: Arc<AtomicU8>,

    // For emulating open bus behavior
    io_bus: Arc<AtomicU16>,
}

impl PpuRegs {
    pub fn new() -> Self {
	PpuRegs {
	    ctrl:    Arc::new(AtomicU8::new(0)),
	    mask:    Arc::new(AtomicU8::new(0)),
	    status:  Arc::new(AtomicU8::new(0)),
	    oamaddr: Arc::new(AtomicU8::new(0)),
	    oamdata: Arc::new(AtomicU8::new(0)),
	    scroll:  Arc::new(AtomicU16::new(0)),
	    addr:    Arc::new(AtomicU16::new(0)),
	    data:    Arc::new(AtomicU8::new(0)),
	    oamdma:  Arc::new(AtomicU8::new(0)),
	    v:       Arc::new(AtomicU16::new(0)),
	    t:       Arc::new(AtomicU16::new(0)),
	    x:       Arc::new(AtomicU8::new(0)),
	    w:       Arc::new(AtomicBool::new(false)),
	    read_buf: Arc::new(AtomicU8::new(0)),
	    io_bus:   Arc::new(AtomicU16::new(0)),
	}
    }

    fn store_io_bus(&self, val: u16) {
	self.io_bus.store(val, Ordering::Relaxed);
    }
    fn store_io_bus_lo(&self, byte: u8) {
	self.io_bus.store((self.io_bus.load(Ordering::Relaxed) & 0xFF00)
			  | (byte as u16), Ordering::Relaxed);
    }
    fn store_io_bus_hi(&self, byte: u8) {
	self.io_bus.store(((byte as u16) << 8) |
			  (self.io_bus.load(Ordering::Relaxed) & 0x00FF),
			  Ordering::Relaxed);
    }
    fn load_io_bus(&self) -> u16 {
	self.io_bus.load(Ordering::Relaxed)
    }

    pub fn read_bus(&self) -> u8 {
	self.load_io_bus() as u8
    }
    pub fn read_ctrl(&self) -> u8 {
	let ctrl = self.ctrl.load(Ordering::Relaxed);
	self.store_io_bus_lo(ctrl);
	ctrl
    }
    pub fn write_ctrl(&self, byte: u8) {
	self.store_io_bus_lo(byte);
	self.ctrl.store(byte, Ordering::Relaxed)
    }
    pub fn read_mask(&self) -> u8 {
	let mask = self.mask.load(Ordering::Relaxed);
	self.store_io_bus_lo(mask);
	mask
    }
    pub fn write_mask(&self, byte: u8) {
	self.store_io_bus_lo(byte);
	self.mask.store(byte, Ordering::Relaxed)
    }
    pub fn read_status(&self) -> u8 {
	self.store_w(false); // clear w on status read
	let status = self.status.load(Ordering::Relaxed);
	self.io_bus.store(
	    (self.io_bus.load(Ordering::Relaxed) & 0b1111111111111000)
		| (status as u16), Ordering::Relaxed);
	status
    }
    pub fn write_status(&self, byte: u8) {
	self.store_io_bus_lo(byte);
	self.status.store(byte, Ordering::Relaxed)
    }
    pub fn read_oamaddr(&self) -> u8 {
	let oamaddr = self.oamaddr.load(Ordering::Relaxed);
	self.store_io_bus_lo(oamaddr);
	oamaddr
    }
    pub fn write_oamaddr(&self, byte: u8) {
	self.store_io_bus_lo(byte);
	self.oamaddr.store(byte, Ordering::Relaxed)
    }
    pub fn read_oamdata(&self) -> u8 {
	let oamdata = self.oamdata.load(Ordering::Relaxed);
	self.store_io_bus_lo(oamdata);
	oamdata
    }
    pub fn write_oamdata(&self, byte: u8) {
	self.store_io_bus_lo(byte);
	self.oamdata.store(byte, Ordering::Relaxed)
    }
    // pub fn read_scroll(&self) -> u16 {
    // 	self.scroll.load(Ordering::Relaxed)
    // }
    // pub fn write_scroll(&self, scroll: u16) {
    // 	self.io_bus.store(scroll, Ordering::Relaxed);
    // 	self.scroll.store(scroll, Ordering::Relaxed)
    // }
    // pub fn read_addr(&self) -> u8 {
    // 	self.io_bus.load(Ordering::Relaxed) as u8
    // }
    pub fn write_addr(&self, byte: u8) {
	if self.load_w() {
	    self.store_t((self.load_t() & 0xFF00) | byte as u16);
	    self.store_io_bus_lo(byte);
	    self.addr.store(
		(self.addr.load(Ordering::Relaxed) & 0xFF00) | byte as u16,
		Ordering::Relaxed);
	    self.store_w(true)
	} else {
	    // TODO: During raster effects, if the second write to
	    // PPUADDR happens at specific times, at most one axis of
	    // scrolling will be set to the bitwise AND of the written
	    // value and the current value. The only safe time to
	    // finish the second write is during blanking; see PPU
	    // scrolling for more specific timing.
	    self.store_t(((byte as u16) << 8) | (self.load_t() & 0x00FF));
	    self.store_io_bus_hi(byte);
	    self.addr.store(
		((byte as u16) << 8) | (self.addr.load(Ordering::Relaxed) & 0x00FF),
		Ordering::Relaxed);
	    self.store_w(false)
	}
	// self.io_bus.store(scroll, Ordering::Relaxed);
	// self.scroll.store(scroll, Ordering::Relaxed)
    }
    pub fn read_data(&self) -> u8 {
	let data = self.data.load(Ordering::Relaxed);
	self.store_io_bus_lo(data);
	data
    }

    fn load_w(&self) -> bool {
	self.w.load(Ordering::Relaxed)
    }
    fn store_w(&self, b: bool) {
	self.w.store(b, Ordering::Relaxed)
    }
    fn load_t(&self) -> u16 {
	self.t.load(Ordering::Relaxed)
    }
    fn store_t(&self, val: u16) {
	self.t.store(val, Ordering::Relaxed)
    }

    // Perform a read or write on the PPU bus. Assumes argument
    // address has already been passed through the mapper.
    pub fn do_io(&self,
	     cart: &mut Cart,
	     vram: &mut [u8],
	     paletteTbl: &mut [u8],
	     io: IO,	     
    ) -> Result<(), String> {

	if io == IO::Read {
	    // Transfer read buf to PPUDATA register
	    self.data.store(self.read_buf.load(Ordering::Relaxed),
			    Ordering::Relaxed);
	}

	let mapped = cart.mapper.map(CpuOrPpu::Ppu, io.into(),
				     self.addr.load(Ordering::Relaxed));

	// If read, load something from vram or cartridge (depending
	// on mapper) into read_buf. If write, copy it into PPUDATA
	// and also wherever the mapper wants to put it.
		
	match mapped {
	    None => {
		match io {
		    IO::Read => {
			// Read from open bus (unmapped address)
			// returns low byte of the address.
			self.read_buf.store(self.addr.load(Ordering::Relaxed) as u8,
					    Ordering::Relaxed)
		    }
		    IO::Write(_) => () // Write to unmapped memory??
		}
	    }
	    Some((MapTarget::Default, mapped_addr)) => {
		// Pattern memory / "CHR ROM"
		if mapped_addr <= 0x1FFF {
		    match io {
			IO::Read => {
			    let byte = cart.rom.chrROM[mapped_addr as usize];
			    self.read_buf.store(byte, Ordering::Relaxed)
			}
			IO::Write(byte) => {
			    // Is it possible to check if CHR is RAM?
			    // To throw error here if not RAM...
			    cart.rom.chrROM[mapped_addr as usize] = byte;
			    self.data.store(byte, Ordering::Relaxed)
			}
		    }
		}
		// Nametable memory / internal VRAM
		else if mapped_addr <= 0x3EFF {
		    let wrapped_addr = mapped_addr & 0x800;
		    match io {
			IO::Read => {
			    let byte = vram[wrapped_addr as usize];
			    self.read_buf.store(byte, Ordering::Relaxed)
			}
			IO::Write(byte) => {
			    vram[wrapped_addr as usize] = byte;
			    self.data.store(byte, Ordering::Relaxed)
			}
		    }
		}
		// Palette memory
		else if mapped_addr <= 0x3FFF {
		    let wrapped_addr = mapped_addr & 0x001F;
		    let palette_addr = paletteTblAddr(wrapped_addr);
		    match io {
			IO::Read => {
			    let byte = paletteTbl[palette_addr as usize];
			    self.read_buf.store(byte, Ordering::Relaxed)
			}
			IO::Write(byte) => {
			    paletteTbl[palette_addr as usize] = byte;
			    self.data.store(byte, Ordering::Relaxed)
			}
		    }
		}
		else {
		    return Err(format!("{} out of PPU address space", mapped_addr))
		}
	    }
	    Some((MapTarget::Cartridge(tgt), mapped_addr)) => {
		match io {
		    IO::Read => {
			let byte = cart.read(tgt, mapped_addr)?;
			self.read_buf.store(byte, Ordering::Relaxed)
		    }
		    IO::Write(byte) => {
			cart.write(tgt, mapped_addr, byte)?;
			self.data.store(byte, Ordering::Relaxed)
		    }
		}
	    }
	}
	
	// Increment PPUADDR
	let incr = if self.ctrl.get_bit(2, Ordering::Relaxed) {
	    32
	} else {
	    1
	};
	self.addr.store(self.addr.load(Ordering::Relaxed).wrapping_add(incr),
			Ordering::Relaxed);
	
	// // "Later PPUs added an unreliable feature
	// // for reading palette data from
	// // $3F00-$3FFF ..."
	// if addr >= 0x3F00 {
	//     let palette_addr = self.paletteTblAddr(wrapped_addr);
	//     cpu_buf.store(self.paletteTbl[palette_addr as usize],
	// 		  Ordering::Relaxed)
	// } else {
	//     cpu_buf.store(data, Ordering::Relaxed)
	// }
	
	Ok(())
    }    
}

// Very tentative -- likely to change
#[derive(Clone, Debug)]
pub struct Ppu {
    pub regs: PpuRegs,
    pub frame_complete: Arc<AtomicBool>,
}

impl fmt::Display for Ppu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	writeln!(f, "")
    }
}

impl Ppu {
    pub fn new() -> Self {
	Ppu {
	    regs: PpuRegs::new(),
	    frame_complete: Arc::new(AtomicBool::new(false)),
	}
    }

    pub fn run(&mut self)
	       -> Box<dyn Coroutine<Yield = (), Return = String> + Unpin + '_> {
	let go = #[coroutine] move || {
	    let mut cycle: usize = 0;
	    let mut scanline: i64 = 0;
	    loop {
		cycle += 1;
		if cycle >= 341 {
		    cycle = 0;
		    scanline += 1;
		    if scanline >= 261 {
			scanline = -1;
			self.frame_complete.store(true, Ordering::Relaxed)
		    }
		}
		// yield (PpuEventTarget::Cpu, 0, PpuEventType::Read)
		yield ()
	    }
	};
	Box::new(go)
    }
}
