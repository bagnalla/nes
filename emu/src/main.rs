#![feature(coroutines, coroutine_trait, stmt_expr_attributes)]

use std::fs;
use std::ops::{Coroutine};
use std::pin::Pin;
use std::sync::atomic::{Ordering};
// use std::time::{Duration, Instant};
// use std::thread::sleep;
// use std::time::{Instant};

use cpu::{Cpu, IO, parse_instr_with_arg, PC_MONITOR};
use nes::cart::{Cart, CpuOrPpu, INES};
use nes::{Nes, yielded};
// use nes::ppu::{Ppu};

use raylib::prelude::*;

fn chrrom_texture(rl: &mut RaylibHandle,
		  thread: &RaylibThread,
		  ines: &INES,
		  palette: &[Color; 64],
		  i: usize) -> Result<Texture2D, String> {
    let mut image = Image::gen_image_color(128, 128, Color::BLACK);
    // for i in 0 .. ines.chrROM.len() / 2 {
    // 	let lo = ines.chrROM[2*i];
    // 	let hi = ines.chrROM[2*i+1];
    // 	for j in 0..8 {
    // 	    // (hi[j] << 1) | lo[j]
    // 	}
    // }

    for tile_y in 0..16 {
	for tile_x in 0..16 {
	    let tile_offset = tile_y * 256 + tile_x * 16;
	    
	    for row in 0..8 {
		let mut lsb = ines.chrrom[i * 0x1000 + tile_offset + row];
		let mut msb = ines.chrrom[i * 0x1000 + tile_offset + row + 8];
		for col in 0..8 {
		    let pixel = ((msb & 1) << 1) | (lsb & 1);
		    lsb >>= 1;
		    msb >>= 1;
		    image.draw_pixel((tile_x * 8 + (7 - col)) as i32,
				     (tile_y * 8 + row) as i32,
				     palette[pixel as usize]);
		}	
	    }
	}
    }
    
    let chrrom_texture = rl.load_texture_from_image(&thread, &image)?;
    Ok(chrrom_texture)
}

fn load_palette(path: &str) -> Result<[Color; 64], std::io::Error> {
    let bytes = fs::read(path)?;
    let mut colors = [Color::BLACK; 64];
    for i in 0..64 {
	colors[i] = Color {
	    r: bytes[3*i],
	    g: bytes[3*i+1],
	    b: bytes[3*i+2],
	    a: 255,
	}
    }
    Ok(colors)
}

// fn print_log(cpu: *const Cpu, prgrom: &[u8], cyc: usize) {
// fn print_log(nes: *const Nes, prgrom: &[u8], cyc: usize) {
fn print_log(nes: *const Nes, cart: *const Cart, cyc: usize) {
    unsafe {
	let pc = (*(*nes).cpu).pc;
	let a = (*(*nes).cpu).a;
	let x = (*(*nes).cpu).x;
	let y = (*(*nes).cpu).y;
	let p = (*(*nes).cpu).status;
	let sp = (*(*nes).cpu).sp;
	let Some((_, mapped_addr)) =
	    (*cart).mapper.map(CpuOrPpu::Cpu, IO::Read.into(), pc)
	else { panic!() };
	let (instr, _) = parse_instr_with_arg(&(*cart).rom.prgrom,
					      mapped_addr as usize);
	println!("{:04X}: {}, a:{:02X} x:{:02X} y:{:02X} p:{:02X} sp:{:02X} {}",
		 pc, instr, a, x, y, p, sp, cyc)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    const CYCLES_PER_SECOND: f64 = 1790000.0 * 3.0;
    const SECONDS_PER_FRAME: f64 = 1.0 / 60.0;
    // const CYCLES_PER_FRAME: usize = 29833 * 3;
    const CYCLES_PER_FRAME: usize = 89500;
    assert_eq!((CYCLES_PER_SECOND * SECONDS_PER_FRAME).floor() as usize,
	       CYCLES_PER_FRAME);

    let palette = load_palette("2C02G_wiki.pal")?;
    // println!("{:?}", palette);

    // let rom_data = fs::read("/home/alex/Dropbox/nes/roms/Super_mario_brothers.nes")?;
    let rom_data = fs::read(
	// "/home/alex/Dropbox/nes/roms/Super Mario Bros. (Japan, USA).nes")?;
	"/home/alex/source/nes-test-roms/other/nestest.nes")?;
    let ines: INES = rom_data.into();
    // println!("{:?}", ines.mapper_id);
    // println!("{:?}", ines.sz_prgrom);
    // std::process::exit(0);
    let cart = Cart::new(ines.clone()).expect("couldn't load ROM");
    let cart_ptr = &cart as *const Cart;

    // Set up NES
    let mut nes = Nes::new();
    let nes_ptr = &nes as *const Nes;
    let mut nes_process = nes.run(cart);

    // let cpu_ptr = unsafe { (*nes_ptr).cpu };
    // let _ppu_ptr = unsafe { (*nes_ptr).ppu };

    // Set up raylib
    let (mut rl, thread) = raylib::init()
        .size(640, 480)
        .title("NES")
        .build();

    rl.set_target_fps(60);

    let chrrom_left = chrrom_texture(&mut rl, &thread, &ines, &palette, 0)?;
    let chrrom_right = chrrom_texture(&mut rl, &thread, &ines, &palette, 1)?;

    let mut pc = PC_MONITOR.load(Ordering::Relaxed);

    while !rl.window_should_close() {
	let mut cycles = 7*3;
	// let mut cycles = 0;
	// Run NES cycles for the frame
	for _ in 0 .. CYCLES_PER_FRAME {
	    _ = yielded(Pin::new(&mut nes_process).resume(()))?;
	    let new_pc = PC_MONITOR.load(Ordering::Relaxed);
	    cycles += 1;
	    if new_pc != pc {
		// println!("{:04x}", new_pc);
		// unsafe { print_log(cpu_ptr, &(*cart_ptr).rom.prgrom, cycles) }
		print_log(nes_ptr, cart_ptr, cycles / 3);
		pc = new_pc;
	    }
	    if cycles / 3 > 199630 {
		return Ok(())
	    }
	    // println!("{}", cycles / 3);
	}
	
	// println!("{}", cycles);

	// println!("{}", PC_MONITOR.load(Ordering::Relaxed));
	// unsafe {
	//     println!("{}", (*(*nes_ptr).cpu).pc);
	// }
	
        let mut d = rl.begin_drawing(&thread);         
        d.clear_background(Color::WHITE);
	// d.draw_texture(&chrROM_texture, 0, 0, Color::WHITE);
	d.draw_texture_ex(&chrrom_left,
			  Vector2 { x: 341.0, y: 261.0 },
			  0.0, 1.0, Color::WHITE);
	d.draw_texture_ex(&chrrom_right,
			  Vector2 { x: 341.0+128.0, y: 261.0 },
			  0.0, 1.0, Color::WHITE);
        // d.draw_text("Hello, world!", 12, 12, 20, Color::BLACK);
	let palette_tbl = unsafe { &(*nes_ptr).palette_tbl };
	// println!("{:?}", palette_tbl);
	for pal in 0..4 {
	    
	}
    }

    Ok(())
}
