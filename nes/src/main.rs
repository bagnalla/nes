#![feature(coroutines, coroutine_trait, stmt_expr_attributes)]

use std::ops::{Coroutine, CoroutineState};
use std::pin::Pin;
use std::sync::atomic::{Ordering};
use std::time::{Duration, Instant};
use std::thread::sleep;
// use std::time::{Instant};

use cpu::{Cpu, CpuEventType};
use ppu::{Ppu, PpuEventTarget, PpuEventType};

fn yielded<Y, R: std::fmt::Debug>(state: CoroutineState<Y, R>) -> Y {
    match state {
	CoroutineState::Yielded(event) => event,
	CoroutineState::Complete(x) => panic!("{:?}", x)
    }
}

// TODO: stateful Nes struct with methods for initializing, running
// (perhaps not a coroutine but just running a cycle or frame at a
// time), loading a ROM, etc. The main creates the Nes, loads a ROM
// into it, and takes care of rendering graphics.

struct Rom {
    
}

trait Mapper where {
}

struct Nes<M: Mapper> {
    cpu: Cpu,
    ppu: Ppu,
    ram: [u8; 2048],
    vram: [u8; 2048],
    mapper: M
}

impl<M: Mapper> Nes<M> {
    // fn new() -> Nes {
    // }

    fn step(&mut self) {
    }
}

fn main() {
    const CYCLES_PER_SECOND: f64 = 1790000.0;
    const SECONDS_PER_FRAME: f64 = 1.0 / 60.0;
    const CYCLES_PER_FRAME: usize = 29833;
    assert!((CYCLES_PER_SECOND * SECONDS_PER_FRAME).floor() as usize
	    == CYCLES_PER_FRAME);

    // Set up CPU
    let mut cpu = Cpu::new();
    let buf = cpu.read_buf.clone();
    let _reset_signal = cpu.reset_signal.clone();
    let _irq_signal = cpu.irq_signal.clone();
    let _nmi_signal = cpu.nmi_signal.clone();
    let mut cpu_process = cpu.run();

    // Set up PPU
    let mut ppu = Ppu::new();
    let mut ppu_process = ppu.run();

    // Set up memories
    let mut main_memory: [u8; 2048] = [0; 2048];
    let mut _ppu_memory: [u8; 2_usize.pow(11)] = [0; 2_usize.pow(11)];

    // Set up window / graphics renderer
    // todo!(); (probably raylib)

    let mut cycle_count: usize = 0;
    let mut frame_count: usize = 0;
    // let start_time = Instant::now();
    
    loop {
	let frame_start = Instant::now();

	// Run all the cycles for the frame
	for _ in 0..CYCLES_PER_FRAME {
	    
	    // Run CPU for one cycle
	    let (addr, event_type) = yielded(Pin::new(&mut cpu_process).resume(()));

	    // Main memory access
	    if addr <= 0x1FFF {
		let wrapped_addr = addr % 2048;
		match event_type {
		    CpuEventType::Read => {
			// println!("{:04x}", fixed_addr);
			buf.store(main_memory[wrapped_addr as usize], Ordering::Relaxed)
		    }
		    CpuEventType::Write(byte) => {
			main_memory[wrapped_addr as usize] = byte
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
		todo!()
	    };

	    // Run PPU for three cycles
	    for _ in 0..3 {
		let (bus, _addr, event) = yielded(Pin::new(&mut ppu_process).resume(()));
		match bus {
		    PpuEventTarget::Cpu => {
			match event {
			    PpuEventType::Read => {
				todo!()
			    }
			    PpuEventType::Write(_byte) => {
				todo!()
			    }
			}
		    }
		    PpuEventTarget::Ppu => {
			match event {
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
	}

	// Render frame
	// todo!();

	cycle_count += CYCLES_PER_FRAME;
	// println!("{}", cycle_count);
	
	// Sleep for remaining time
	let elapsed = Instant::now().duration_since(frame_start);
	sleep(Duration::from_secs_f64(SECONDS_PER_FRAME) - elapsed);

	frame_count += 1;
	// println!("{}", frame_count as f64 /
	// 	 Instant::now().duration_since(start_time).as_secs_f64());
    }
}
