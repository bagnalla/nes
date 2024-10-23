#![feature(coroutines, coroutine_trait, stmt_expr_attributes)]

pub mod ppu;
pub mod cart;

use std::ops::{Coroutine, CoroutineState};
use std::pin::Pin;
use std::ptr;
use std::sync::Arc;
use std::sync::atomic::{AtomicU8, Ordering};
use std::time::{Duration, Instant};
use std::thread::sleep;
// use std::time::{Instant};

use cpu::{Cpu, CpuEvent, CpuEventType};
use crate::cart::{Cart};
use crate::ppu::{Ppu, PpuEvent, PpuEventTarget, PpuEventType};

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
    nameTbl: [[u8; 1024]; 2],
    paletteTbl: [u8; 32],
    pub cpu: *const Cpu,
    pub ppu: *const Ppu,
}

impl Nes {
    pub fn new() -> Nes {
	Nes {
	    // cpu: Cpu::new(),
	    // ppu: Ppu::new(),
	    // ram: [0; 2048],
	    // ram: [0; 2_usize.pow(16)],
	    ram: Vec::new(),
	    vram: [0; 2048],
	    nameTbl: [[0; 1024]; 2],
	    paletteTbl: [0; 32],
	    cpu: ptr::null(),
	    ppu: ptr::null(),
	}
    }

    fn handle_cpu_event(&mut self,
			cart: &mut Cart,
			cpu_buf: &Arc<AtomicU8>,
			(addr, event_type) : CpuEvent) {
	
	match event_type {
	    CpuEventType::Read => {
		cpu_buf.store(self.ram[addr as usize],
			      Ordering::Relaxed)
	    }
	    CpuEventType::Write(byte) => {
		self.ram[addr as usize] = byte
	    }
	}
	
	// // Main memory access
	// if addr <= 0x1FFF {
	//     let wrapped_addr = addr % 2048;
	//     match event_type {
	// 	CpuEventType::Read => {
	// 	    // println!("{:04x}", fixed_addr);
	// 	    cpu_buf.store(self.ram[wrapped_addr as usize],
	// 		      Ordering::Relaxed)
	// 	}
	// 	CpuEventType::Write(byte) => {
	// 	    self.ram[wrapped_addr as usize] = byte
	// 	}
	//     }
	// }

	// // PPU access
	// else if addr <= 0x3FFF {
	//     let wrapped_addr = (addr % 8) as u8;
	//     match event_type {
	// 	CpuEventType::Read => {
	// 	    todo!()
	// 	}
	// 	CpuEventType::Write(byte) => {
	// 	    todo!()
	// 	}
	//     }
	// }

	// // Something else
	// else {
	//     // todo!()
	//     ()
	// };
    }

    fn handle_ppu_event(&mut self,
			(bus, addr, event_type) : PpuEvent) {
	match bus {
	    PpuEventTarget::Cpu => {
		match event_type {
		    PpuEventType::Read => {
			// todo!()
			()
		    }
		    PpuEventType::Write(_byte) => {
			todo!()
		    }
		}
	    }
	    PpuEventTarget::Ppu => {
		// Pattern memory / "CHR ROM"
		let data = if addr <= 0x1FFF {
		    todo!()
		}
		// Nametable memory / VRAM
		else if addr <= 0x3EFF {
		    let wrapped_addr = addr & 0x800;
		    todo!()
		}
		// Palette memory
		else if addr <= 0x3FFF {
		    let wrapped_addr = addr & 0x001F;
		    // https://www.nesdev.org/wiki/PPU_palettes "Note
		    // that entry 0 of each palette is also unique in
		    // that its color value is shared between the
		    // background and sprite palettes, so writing to
		    // either one updates the same internal storage."
		    let shared_addr = match wrapped_addr {
			0x0010 => 0x0000,
			0x0014 => 0x0004,
			0x0018 => 0x0008,
			0x001C => 0x000C,
			_ => wrapped_addr,
		    };
		    self.paletteTbl[shared_addr as usize]
		}
		else {
		    todo!()
		};

		match event_type {
		    PpuEventType::Read => {
			todo!()
		    }
		    PpuEventType::Write(_byte) => {
			todo!()
		    }
		}
	    }
	}
    }

    pub fn run(&mut self,
	       mut cart: Cart)
	       -> Box<dyn Coroutine<Yield = usize, Return = String> + Unpin + '_> {
	let go = #[coroutine] static move || {

	    // // Set up CPU
	    // let mut cpu = Cpu::new();
	    // cpu.pc = 0x400;
	    // let cpu_buf = cpu.read_buf.clone();
	    // let _reset_signal = cpu.reset_signal.clone();
	    // let _irq_signal = cpu.irq_signal.clone();
	    // let _nmi_signal = cpu.nmi_signal.clone();
	    // let mut cpu_process = cpu.run();
	    
	    // Set up PPU
	    let mut ppu = Ppu::new();
	    self.ppu = &ppu as *const Ppu;
	    let mut ppu_process = ppu.run();

	    let mut cycle_counter: usize = 0;

	    let mut err = String::new();
	    
	    fn load_mem(path: &str, start_addr: u16)
			-> Result<Vec<u8>, Box<dyn std::error::Error>> {
		let bin = std::fs::read(path)?;
		let mut mem = [0; 65536];
		mem[start_addr as usize..start_addr as usize + bin.len()].copy_from_slice(&bin);
		Ok(mem.into())
	    }

	    const PROGRAM_START: u16 = 0x400;
	    const SUCCESS_ADDR: u16 = 0x336d;
	    let path = "/home/alex/Dropbox/6502_65C02_functional_tests/6502_functional_test.bin";
	    self.ram = load_mem(path, 0x000A).expect("");

	    let mut cpu = Cpu::new();
	    self.cpu = &mut cpu as *mut Cpu;
	    cpu.pc = PROGRAM_START;
	    let cpu_buf = cpu.read_buf.clone();
	    let mut cpu_process = cpu.run();

	    loop {
		// Step PPU
		let ppu_event = yielded(Pin::new(&mut ppu_process).resume(()));
		match ppu_event {
		    Ok(event) => self.handle_ppu_event(event),
		    Err(msg) => return msg
		}

		// Step CPU every third cycle
		if cycle_counter % 3 == 0 {
		    let cpu_event = yielded(Pin::new(&mut cpu_process).resume(()));
		    match cpu_event {
			Ok(event) => self.handle_cpu_event(&mut cart, &cpu_buf, event),
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
