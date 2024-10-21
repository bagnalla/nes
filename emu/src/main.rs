#![feature(coroutines, coroutine_trait, stmt_expr_attributes)]

use std::ops::{Coroutine, CoroutineState};
use std::pin::Pin;
use std::sync::atomic::{Ordering};
use std::time::{Duration, Instant};
use std::thread::sleep;
// use std::time::{Instant};

use cpu::{Cpu, CpuEventType};
use nes::cart::{Cart, INES};
use nes::{Nes, yielded};
use nes::ppu::{Ppu, PpuEventTarget, PpuEventType};

use raylib::prelude::*;

fn main() {
    const CYCLES_PER_SECOND: f64 = 1790000.0 * 3.0;
    const SECONDS_PER_FRAME: f64 = 1.0 / 60.0;
    // const CYCLES_PER_FRAME: usize = 29833 * 3;
    const CYCLES_PER_FRAME: usize = 89500;
    assert_eq!((CYCLES_PER_SECOND * SECONDS_PER_FRAME).floor() as usize,
	       CYCLES_PER_FRAME);

    let cart = Cart::new(INES::new()).expect("couldn't load ROM");

    // Set up NES
    let mut nes = Nes::new();
    let mut nes_process = nes.run(cart);

    // Set up raylib
    let (mut rl, thread) = raylib::init()
        .size(640, 480)
        .title("NES")
        .build();
    rl.set_target_fps(60);

    while !rl.window_should_close() {
	let mut cycles = 0;
	// Run NES cycles for the frame
	for _ in 0 .. CYCLES_PER_FRAME {
	    cycles = yielded(Pin::new(&mut nes_process).resume(()));
	}
	println!("{}", cycles);
	
        let mut d = rl.begin_drawing(&thread);
         
        d.clear_background(Color::WHITE);
        // d.draw_text("Hello, world!", 12, 12, 20, Color::BLACK);
    }
}

    // // Set up CPU
    // let mut cpu = Cpu::new();
    // let buf = cpu.read_buf.clone();
    // let _reset_signal = cpu.reset_signal.clone();
    // let _irq_signal = cpu.irq_signal.clone();
    // let _nmi_signal = cpu.nmi_signal.clone();
    // let mut cpu_process = cpu.run();

    // // Set up PPU
    // let mut ppu = Ppu::new();
    // let mut ppu_process = ppu.run();

    // // Set up memories
    // let mut main_memory: [u8; 2048] = [0; 2048];
    // let mut _ppu_memory: [u8; 2_usize.pow(11)] = [0; 2_usize.pow(11)];

    // // Set up window / graphics renderer
    // // todo!(); (probably raylib)

    // let mut cycle_count: usize = 0;
    // let mut frame_count: usize = 0;
    // // let start_time = Instant::now();

//     // TODO: replace with raylib game loop
//     loop {
// 	let frame_start = Instant::now();

// 	// Run all the cycles for the frame
// 	for _ in 0..CYCLES_PER_FRAME {
	    
// 	    // Run CPU for one cycle
// 	    let (addr, event_type) = yielded(Pin::new(&mut cpu_process).resume(()));

// 	    // Main memory access
// 	    if addr <= 0x1FFF {
// 		let wrapped_addr = addr % 2048;
// 		match event_type {
// 		    CpuEventType::Read => {
// 			// println!("{:04x}", fixed_addr);
// 			buf.store(main_memory[wrapped_addr as usize], Ordering::Relaxed)
// 		    }
// 		    CpuEventType::Write(byte) => {
// 			main_memory[wrapped_addr as usize] = byte
// 		    }
// 		}
// 	    }

// 	    // PPU access
// 	    else if addr <= 0x3FFF {
// 		let wrapped_addr = (addr % 8) as u8;
// 		match event_type {
// 		    CpuEventType::Read => {
// 			todo!()
// 		    }
// 		    CpuEventType::Write(byte) => {
// 			todo!()
// 		    }
// 		}
// 	    }

// 	    // Something else
// 	    else {
// 		todo!()
// 	    };

// 	    // Run PPU for three cycles
// 	    for _ in 0..3 {
// 		let (bus, _addr, event) = yielded(Pin::new(&mut ppu_process).resume(()));
// 		match bus {
// 		    PpuEventTarget::Cpu => {
// 			match event {
// 			    PpuEventType::Read => {
// 				todo!()
// 			    }
// 			    PpuEventType::Write(_byte) => {
// 				todo!()
// 			    }
// 			}
// 		    }
// 		    PpuEventTarget::Ppu => {
// 			match event {
// 			    PpuEventType::Read => {
// 				todo!()
// 			    }
// 			    PpuEventType::Write(_byte) => {
// 				todo!()
// 			    }
// 			}
// 		    }
// 		}
// 	    }
// 	}

// 	// Render frame
// 	// todo!();

// 	cycle_count += CYCLES_PER_FRAME;
// 	// println!("{}", cycle_count);
	
// 	// Sleep for remaining time
// 	let elapsed = Instant::now().duration_since(frame_start);
// 	sleep(Duration::from_secs_f64(SECONDS_PER_FRAME) - elapsed);

// 	frame_count += 1;
// 	// println!("{}", frame_count as f64 /
// 	// 	 Instant::now().duration_since(start_time).as_secs_f64());
//     }
// }
