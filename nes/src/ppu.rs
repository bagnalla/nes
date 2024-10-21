#![feature(coroutines, coroutine_trait, stmt_expr_attributes)]

use std::ops::{Coroutine};
use std::fmt;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicU16, Ordering};

#[derive(Debug)]
pub enum Bus {
    Cpu,
    Ppu
}

#[derive(Debug)]
pub enum PpuEventType {
    Read,
    Write(u8),
}

// Target bus
pub enum PpuEventTarget {
    Cpu,
    Ppu,
}

// Every PPU event contains a target bus, an address on that bus, and
// is either a read or a write to it.
pub type PpuEvent = (PpuEventTarget, u16, PpuEventType);

// Very tentative -- likely to change
#[derive(Clone, Debug)]
pub struct Ppu {
    pub ppuctrl:   Arc<AtomicU8>,
    pub ppumask:   Arc<AtomicU8>,
    pub ppustatus: Arc<AtomicU8>,
    pub oamaddr:   Arc<AtomicU8>,
    pub oamdata:   Arc<AtomicU8>,
    pub ppuscroll: Arc<AtomicU16>,
    pub ppuaddr:   Arc<AtomicU16>,
    pub ppudata:   Arc<AtomicU8>,
    pub oamdma:    Arc<AtomicU8>,

    nameTbl: [[u8; 1024]; 2],
    paletteTbl: [u8; 32],

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
	    ppuctrl:   Arc::new(AtomicU8::new(0)),
	    ppumask:   Arc::new(AtomicU8::new(0)),
	    ppustatus: Arc::new(AtomicU8::new(0)),
	    oamaddr:   Arc::new(AtomicU8::new(0)),
	    oamdata:   Arc::new(AtomicU8::new(0)),
	    ppuscroll: Arc::new(AtomicU16::new(0)),
	    ppuaddr:   Arc::new(AtomicU16::new(0)),
	    ppudata:   Arc::new(AtomicU8::new(0)),
	    oamdma:    Arc::new(AtomicU8::new(0)),
	    
	    nameTbl: [[0; 1024]; 2],
	    paletteTbl: [0; 32],

	    frame_complete: Arc::new(AtomicBool::new(false)),
	}
    }

    pub fn run(&mut self)
	       -> Box<dyn Coroutine<Yield = PpuEvent, Return = String> + Unpin + '_> {
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
		yield (PpuEventTarget::Cpu, 0, PpuEventType::Read)
	    }
	};
	Box::new(go)
    }
}
