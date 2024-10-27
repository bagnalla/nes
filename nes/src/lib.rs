#![feature(coroutines, coroutine_trait, stmt_expr_attributes)]

pub mod ppu;
pub mod cart;

use std::ops::{Coroutine, CoroutineState};
use std::pin::Pin;
use std::ptr;
use std::sync::Arc;
use std::sync::atomic::{AtomicU8, Ordering};
// use std::time::{Duration, Instant};
// use std::thread::sleep;
// use std::time::{Instant};

// use atomic_bitfield::AtomicBitField as _;

use cpu::{Cpu, CpuEvent, IO};
use crate::cart::{Cart, CpuOrPpu, MapTarget, PrgOrChr};
use crate::ppu::{Ppu, PpuRegs};

pub fn yielded<Y, R: std::fmt::Debug>(state: CoroutineState<Y, R>)
				      -> Result<Y, String> {
    match state {
	CoroutineState::Yielded(event) => Ok(event),
	// CoroutineState::Complete(x) => panic!("{:?}", x)
	CoroutineState::Complete(x) => Err(format!("{:?}", x))
    }
}

pub struct Nes {
    // pub cpu: Cpu,
    // ppu: Ppu,
    // ram: [u8; 2048],
    // ram: [u8; 2_usize.pow(16)],
    ram: Vec<u8>,
    vram: [u8; 2048],
    // nameTbl: [[u8; 1024]; 2],
    pub palette_tbl: [u8; 32],
    pub cpu: *const Cpu,
    pub ppu: *mut Ppu,
}

impl Nes {
    pub fn new() -> Nes {
	Nes {
	    // cpu: Cpu::new(),
	    // ppu: Ppu::new(),
	    // ram: [0; 2048],
	    // ram: [0; 2_usize.pow(16)],
	    ram: vec![0; 2048],
	    vram: [0; 2048],
	    // nameTbl: [[0; 1024]; 2],
	    palette_tbl: [0; 32],
	    cpu: ptr::null(),
	    ppu: ptr::null_mut(),
	}
    }

    // // Perform a read or write on the PPU bus. Assumes argument
    // // address has already been passed through the mapper.
    // fn ppu_io(&mut self,
    // 	      cart: &mut Cart,
    // 	      rw: ReadOrWrite,
    // 	      // mapped_addr: Option<(MapTarget, u16)>,
    // ) -> Result<(), String> {

    // 	let vram_addr = ppu_regs.addr.load(Ordering::Relaxed);
    // 	let mapped_addr = cart.mapper.map(CpuOrPpu::Cpu, rw, addr);
    // 	let data = ppu_regs.read_buf.load(Ordering::Relaxed);
    // 	let wrapped_vram_addr =
    // 	    if vram_addr >= 0x3000 && vram_addr < 0x4000 {
    // 		vram_addr - 0x1000
    // 	    } else {
    // 		vram_addr
    // 	    };
    // 	// TODO: use mapper to transform wrapped_vram_addr here?
    // 	ppu_regs.read_buf.store(
    // 	    self.vram[wrapped_vram_addr as usize],
    // 	    Ordering::Relaxed);
    // 	let incr = if ppu_regs.ctrl.get_bit(2, Ordering::Relaxed) {
    // 	    32
    // 	} else {
    // 	    1
    // 	};
    // 	ppu_regs.addr.store(wrapped_vram_addr.wrapping_add(incr),
    // 			    Ordering::Relaxed);

    // 	// "Later PPUs added an unreliable feature
    // 	// for reading palette data from
    // 	// $3F00-$3FFF ..."
    // 	if addr >= 0x3F00 {
    // 	    let palette_addr = self.paletteTblAddr(wrapped_addr);
    // 	    cpu_buf.store(self.paletteTbl[palette_addr as usize],
    // 			  Ordering::Relaxed)
    // 	} else {
    // 	    cpu_buf.store(data, Ordering::Relaxed)
    // 	}
	
    // 	match mapped_addr {
    // 	    None => {
    // 		// Read from open bus (unmapped address) returns low
    // 		// byte of the address.
    // 		let data = addr as u8;
    // 		todo!();
    // 		return Ok(())
    // 	    }
    // 	    Some((MapTarget::Default, addr)) => {
    // 		match bus {
    // 		    PpuEventTarget::Cpu => {
    // 			match event_type {
    // 			    PpuEventType::Read => {
    // 				// todo!()
    // 				()
    // 			    }
    // 			    PpuEventType::Write(_byte) => {
    // 				todo!()
    // 			    }
    // 			}
    // 		    }
    // 		    PpuEventTarget::Ppu => {
    // 			// Pattern memory / "CHR ROM"
    // 			if addr <= 0x1FFF {
    // 			    match event_type {
    // 				PpuEventType::Read => {
    // 				    let data = cart.rom.chrROM[addr as usize];
    // 				    todo!()
    // 				}
    // 				PpuEventType::Write(byte) => {
    // 				    // Is it possible to check if CHR is RAM?
    // 				    // To throw error here if not RAM...
    // 				    cart.rom.chrROM[addr as usize] = byte
    // 				}
    // 			    }
    // 			}
    // 			// Nametable memory / VRAM
    // 			else if addr <= 0x3EFF {
    // 			    let wrapped_addr = addr & 0x800;
    // 			    todo!()
    // 			}
    // 			// Palette memory
    // 			else if addr <= 0x3FFF {
    // 			    let wrapped_addr = addr & 0x001F;
    // 			    let palette_addr = self.paletteTblAddr(wrapped_addr);
    // 			    match event_type {
    // 				PpuEventType::Read => {
    // 				    let data = self.paletteTbl[palette_addr as usize];
    // 				    todo!()
    // 				}
    // 				PpuEventType::Write(byte) => {
    // 				    self.paletteTbl[palette_addr as usize] = byte
    // 				}
    // 			    }
    // 			}
    // 			else {
    // 			    return Err(format!("{} out of PPU address space", addr))
    // 			}
    // 		    }
    // 		}
    // 	    }
    // 	    Some((MapTarget::Cartridge, addr)) => {
    // 		todo!()
    // 	    }
    // 	}
    // }

    fn handle_cpu_event(&mut self,
			cart: &mut Cart,
			cpu_buf: &Arc<AtomicU8>,
			ppu_regs: &PpuRegs,
			(addr, io) : CpuEvent)
			-> Result<(), String> {
	let mapped_addr = cart.mapper.map(CpuOrPpu::Cpu, io.into(), addr);

	// Main memory access
	if addr <= 0x1FFF {
	    let wrapped_addr = addr % 2048;
	    match io {
		IO::Read => {
		    // println!("{:04x}", fixed_addr);
		    cpu_buf.store(self.ram[wrapped_addr as usize],
			      Ordering::Relaxed)
		}
		IO::Write(byte) => {
		    self.ram[wrapped_addr as usize] = byte
		}
	    }
	}

	// PPU access
	else if addr <= 0x3FFF {
	    let wrapped_addr = addr % 8;
	    match io {
		IO::Read => {
		    match wrapped_addr {
			0 => (),
			1 => (),
			2 => ppu_regs.load_status(),
			3 => (),
			4 => ppu_regs.load_oamdata(),
			5 => (),
			6 => (),
			7 => { // PPUDATA
			    ppu_regs.do_io(cart,
					   &mut self.vram,
					   &mut self.palette_tbl,
					   IO::Read)?;
			    ppu_regs.load_data()
			}
			_ => todo!("{}", wrapped_addr)
		    }
		    cpu_buf.store(ppu_regs.read_bus(), Ordering::Relaxed)
		    // TODO: "Read conflict with DPCM samples" on PPU
		    // registers page.
		}
		IO::Write(byte) => {
		    ppu_regs.write_bus(byte);
		    match wrapped_addr {
			0 => ppu_regs.store_ctrl(),
			1 => ppu_regs.store_mask(),
			2 => (), // Can't write status. Do nothing or error?
			6 => ppu_regs.store_addr(),
			7 => { // PPUDATA
			    ppu_regs.do_io(cart,
					   &mut self.vram,
					   &mut self.palette_tbl,
					   IO::Write(byte))?;
			}
			_ => todo!()
		    }
		}
	    }
	}

	// Something else
	else {
	    match mapped_addr {
		None =>
		    match io {
			IO::Read => (), // Read open bus (leave cpu_buf unchanged)
			IO::Write(_) => (), // Write to unmapped memory?
		    }
		Some((tgt, adr)) => {
		    // Read/write cart
		    let prgorchr = match tgt {
			MapTarget::Default => PrgOrChr::Prg,
			MapTarget::Cartridge(x) => x,
		    };
		    let data = cart.read(prgorchr, adr)?;
		    cpu_buf.store(data, Ordering::Relaxed)
		}
	    }
	};

	Ok(())
    }

    // Writes to any PPU port, including the nominally read-only
    // status port at $2002, load a value onto the PPU's I/O
    // bus. Reading a value from $2004 or $2007 loads a byte from OAM,
    // video memory, or the palette onto this bus. Reading the PPU's
    // status port loads a value onto bits 7-5 of the bus, leaving the
    // rest unchanged. Reading any PPU port, including write-only
    // ports $2000, $2001, $2003, $2005, $2006, returns the PPU I/O
    // bus's value.

    // fn handle_ppu_event(&mut self,
    // 			cart: &mut Cart,
    // 			(bus, addr, event_type) : PpuEvent)
    // 			-> Result<(), String> {
    // 	let mapped_addr = cart.mapper.map(CpuOrPpu::Ppu, event_type.into(), addr);

    // 	match mapped_addr {
    // 	    None => {
    // 		// Read from open bus (unmapped address) returns
    // 		// low byte of the address.
    // 		let data = addr as u8;
    // 		todo!();
    // 		return Ok(())
    // 	    }
    // 	    Some((MapTarget::Default, addr)) => {
    // 		match bus {
    // 		    PpuEventTarget::Cpu => {
    // 			match event_type {
    // 			    PpuEventType::Read => {
    // 				// todo!()
    // 				()
    // 			    }
    // 			    PpuEventType::Write(_byte) => {
    // 				todo!()
    // 			    }
    // 			}
    // 		    }
    // 		    PpuEventTarget::Ppu => {
    // 			// Pattern memory / "CHR ROM"
    // 			if addr <= 0x1FFF {
    // 			    match event_type {
    // 				PpuEventType::Read => {
    // 				    let data = cart.rom.chrROM[addr as usize];
    // 				    todo!()
    // 				}
    // 				PpuEventType::Write(byte) => {
    // 				    // Is it possible to check if CHR is RAM?
    // 				    // To throw error here if not RAM...
    // 				    cart.rom.chrROM[addr as usize] = byte
    // 				}
    // 			    }
    // 			}
    // 			// Nametable memory / VRAM
    // 			else if addr <= 0x3EFF {
    // 			    let wrapped_addr = addr & 0x800;
    // 			    todo!()
    // 			}
    // 			// Palette memory
    // 			else if addr <= 0x3FFF {
    // 			    let wrapped_addr = addr & 0x001F;
    // 			    let palette_addr = self.paletteTblAddr(wrapped_addr);
    // 			    match event_type {
    // 				PpuEventType::Read => {
    // 				    let data = self.paletteTbl[palette_addr as usize];
    // 				    todo!()
    // 				}
    // 				PpuEventType::Write(byte) => {
    // 				    self.paletteTbl[palette_addr as usize] = byte
    // 				}
    // 			    }
    // 			}
    // 			else {
    // 			    return Err(format!("{} out of PPU address space", addr))
    // 			}
    // 		    }
    // 		}
    // 	    }
    // 	    Some((MapTarget::Cartridge, addr)) => {
    // 		todo!()
    // 	    }
    // 	}
    // 	Ok(())
    // }

    pub fn run(&mut self,
	       mut cart: Cart)
	       -> Box<dyn Coroutine<Yield = usize, Return = String> + Unpin + '_> {
	let go = #[coroutine] static move || {

	    // Set up CPU
	    let mut cpu = Cpu::new();
	    // cpu.pc = 0x8000;
	    cpu.pc = 0xc000;
	    self.cpu = &cpu as *const Cpu;
	    let cpu_buf = cpu.read_buf.clone();
	    let reset_signal = cpu.reset_signal.clone();
	    let _irq_signal = cpu.irq_signal.clone();
	    let _nmi_signal = cpu.nmi_signal.clone();
	    let mut cpu_process = cpu.run();

	    // Set up PPU
	    let mut ppu = Ppu::new();
	    self.ppu = &mut ppu as *mut Ppu;
	    let ppu_regs = ppu.regs.clone();
	    let mut ppu_process = ppu.run();

	    let mut cycle_counter: usize = 0;

	    // fn load_mem(path: &str, start_addr: u16)
	    // 		-> Result<Vec<u8>, Box<dyn std::error::Error>> {
	    // 	let bin = std::fs::read(path)?;
	    // 	let mut mem = [0; 65536];
	    // 	mem[start_addr as usize..start_addr as usize + bin.len()]
	    // 	    .copy_from_slice(&bin);
	    // 	Ok(mem.into())
	    // }

	    // const PROGRAM_START: u16 = 0x400;
	    // const SUCCESS_ADDR: u16 = 0x336d;
	    // let path = "/home/alex/Dropbox/6502_65C02_functional_tests/6502_functional_test.bin";
	    // self.ram = load_mem(path, 0x000A).expect("");

	    // let mut cpu = Cpu::new();
	    // self.cpu = &mut cpu as *mut Cpu;
	    // cpu.pc = PROGRAM_START;
	    // let cpu_buf = cpu.read_buf.clone();
	    // let mut cpu_process = cpu.run();

	    // reset_signal.store(true, Ordering::Relaxed);
	    loop {
		// Step PPU
		match yielded(Pin::new(&mut ppu_process).resume(())) {
		    Ok(()) => (),
		    Err(msg) => return msg
		}
		// let ppu_event = yielded(Pin::new(&mut ppu_process).resume(()));
		// match ppu_event.and_then(|event| {
		//     self.handle_ppu_event(&mut cart, event)
		// }) {
		//     Ok(()) => (),
		//     Err(msg) => return msg
		// }

		// Step CPU every third cycle
		if cycle_counter % 3 == 0 {
		    let cpu_event = yielded(Pin::new(&mut cpu_process).resume(()));
		    match cpu_event.and_then(|event| {
			self.handle_cpu_event(&mut cart, &cpu_buf, &ppu_regs, event)
		    }) {
			Ok(()) => (),
			Err(msg) => return msg
		    }
		}

		cycle_counter += 1;
		// yield (cpu_ptr, ppu_ptr)
		yield cycle_counter
	    }
	};

	Box::new(Box::pin(go))
    }
}
