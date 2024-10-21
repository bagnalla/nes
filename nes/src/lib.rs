#![feature(coroutines, coroutine_trait, stmt_expr_attributes)]

pub mod ppu;
pub mod cart;

use std::ops::{Coroutine, CoroutineState};
use std::pin::Pin;
use std::sync::Arc;
use std::sync::atomic::{AtomicU8, Ordering};
use std::time::{Duration, Instant};
use std::thread::sleep;
// use std::time::{Instant};

use cpu::{Cpu, CpuEvent, CpuEventType};
use crate::cart::{Cart};
use crate::ppu::{Ppu, PpuEvent, PpuEventTarget, PpuEventType};

pub fn yielded<Y, R: std::fmt::Debug>(state: CoroutineState<Y, R>) -> Y {
    match state {
	CoroutineState::Yielded(event) => event,
	CoroutineState::Complete(x) => panic!("{:?}", x)
    }
}

pub struct Nes {
    cpu: Cpu,
    ppu: Ppu,
    ram: [u8; 2048],
    vram: [u8; 2048],
}

impl Nes {
    pub fn new() -> Nes {
	Nes {
	    cpu: Cpu::new(),
	    ppu: Ppu::new(),
	    ram: [0; 2048],
	    vram: [0; 2048],
	}
    }

    fn handle_cpu_event(&mut self,
			cart: &mut Cart,
			cpu_buf: &Arc<AtomicU8>,
			(addr, event_type) : CpuEvent) {
	// Main memory access
	if addr <= 0x1FFF {
	    let wrapped_addr = addr % 2048;
	    match event_type {
		CpuEventType::Read => {
		    // println!("{:04x}", fixed_addr);
		    cpu_buf.store(self.ram[wrapped_addr as usize],
			      Ordering::Relaxed)
		}
		CpuEventType::Write(byte) => {
		    self.ram[wrapped_addr as usize] = byte
		}
	    }
	}

	// PPU access
	else if addr <= 0x3FFF {
	    let wrapped_addr = (addr % 8) as u8;
	    match event_type {
		CpuEventType::Read => {
		    todo!()
		}
		CpuEventType::Write(byte) => {
		    todo!()
		}
	    }
	}

	// Something else
	else {
	    // todo!()
	    ()
	};
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

	    // Set up CPU
	    let mut cpu = Cpu::new();
	    let cpu_buf = cpu.read_buf.clone();
	    let _reset_signal = cpu.reset_signal.clone();
	    let _irq_signal = cpu.irq_signal.clone();
	    let _nmi_signal = cpu.nmi_signal.clone();
	    let mut cpu_process = cpu.run();

	    // Set up PPU
	    let mut ppu = Ppu::new();
	    let mut ppu_process = ppu.run();

	    let mut cycle_counter: usize = 0;

	    loop {
		// Step PPU
		let ppu_event = yielded(Pin::new(&mut ppu_process).resume(()));
		self.handle_ppu_event(ppu_event);

		// Step CPU every third cycle
		if cycle_counter % 3 == 0 {
		    let cpu_event = yielded(Pin::new(&mut cpu_process).resume(()));
		    self.handle_cpu_event(&mut cart, &cpu_buf, cpu_event);
		}

		cycle_counter += 1;
		yield cycle_counter
	    }
	};

	Box::new(Box::pin(go))
    }
}
