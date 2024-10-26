#![feature(coroutines, coroutine_trait, stmt_expr_attributes)]

use std::fs;
use std::ops::{Coroutine, CoroutineState};
use std::pin::Pin;
use std::sync::atomic::{Ordering};
use std::time::{Duration, Instant};
use std::thread::sleep;
// use std::time::{Instant};

use cpu::{Cpu, IO, PC_MONITOR};
use nes::cart::{Cart, INES};
use nes::{Nes, yielded};
use nes::ppu::{Ppu};

use raylib::prelude::*;

fn chrROMTexture(rl: &mut RaylibHandle,
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

    for tileY in 0..16 {
	for tileX in 0..16 {
	    let tileOffset = tileY * 256 + tileX * 16;
	    
	    for row in 0..8 {
		let mut lsb = ines.chrROM[i * 0x1000 + tileOffset + row];
		let mut msb = ines.chrROM[i * 0x1000 + tileOffset + row + 8];
		for col in 0..8 {
		    let pixel = ((msb & 1) << 1) | (lsb & 1);
		    lsb >>= 1;
		    msb >>= 1;
		    image.draw_pixel((tileX * 8 + (7 - col)) as i32,
				     (tileY * 8 + row) as i32,
				     palette[pixel as usize]);
		}	
	    }
	}
    }
    
    let chrROM_texture = rl.load_texture_from_image(&thread, &image)?;
    Ok(chrROM_texture)
}

fn loadPalette(path: &str) -> Result<[Color; 64], std::io::Error> {
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    const CYCLES_PER_SECOND: f64 = 1790000.0 * 3.0;
    const SECONDS_PER_FRAME: f64 = 1.0 / 60.0;
    // const CYCLES_PER_FRAME: usize = 29833 * 3;
    const CYCLES_PER_FRAME: usize = 89500;
    assert_eq!((CYCLES_PER_SECOND * SECONDS_PER_FRAME).floor() as usize,
	       CYCLES_PER_FRAME);

    let palette = loadPalette("2C02G_wiki.pal")?;
    // println!("{:?}", palette);

    // let rom_data = fs::read("/home/alex/Dropbox/nes/roms/Super_mario_brothers.nes")?;
    let rom_data = fs::read(
	"/home/alex/Dropbox/nes/roms/Super Mario Bros. (Japan, USA).nes")?;
    let ines: INES = rom_data.into();
    // println!("{:?}", ines);
    // std::process::exit(0);
    let cart = Cart::new(ines.clone()).expect("couldn't load ROM");
    let cart_ptr = &cart as *const Cart;

    // Set up NES
    let mut nes = Nes::new();
    let nes_ptr = &nes as *const Nes;
    let mut nes_process = nes.run(cart);

    // Set up raylib
    let (mut rl, thread) = raylib::init()
        .size(640, 480)
        .title("NES")
        .build();

    rl.set_target_fps(60);

    let chrROM_left = chrROMTexture(&mut rl, &thread, &ines, &palette, 0)?;
    let chrROM_right = chrROMTexture(&mut rl, &thread, &ines, &palette, 1)?;

    while !rl.window_should_close() {
	let mut cycles = 0;
	// Run NES cycles for the frame
	for _ in 0 .. CYCLES_PER_FRAME {
	    cycles = yielded(Pin::new(&mut nes_process).resume(()))?;
	}
	
	// println!("{}", cycles);

	println!("{}", PC_MONITOR.load(Ordering::Relaxed));
	unsafe {
	    println!("{}", (*(*nes_ptr).cpu).pc);
	}
	
        let mut d = rl.begin_drawing(&thread);         
        d.clear_background(Color::WHITE);
	// d.draw_texture(&chrROM_texture, 0, 0, Color::WHITE);
	d.draw_texture_ex(&chrROM_left,
			  Vector2 { x: 0.0, y: 0.0 },
			  0.0, 2.0, Color::WHITE);
	d.draw_texture_ex(&chrROM_right,
			  Vector2 { x: 256.0, y: 0.0 },
			  0.0, 2.0, Color::WHITE);
        // d.draw_text("Hello, world!", 12, 12, 20, Color::BLACK);
    }

    Ok(())
}
