#![feature(coroutines, coroutine_trait, stmt_expr_attributes)]

use std::fmt;
use std::fs;
use std::ops::{Coroutine};
use std::pin::Pin;
// use std::sync::atomic::{Ordering};
// use std::time::{Duration, Instant};
// use std::thread::sleep;
// use std::time::{Instant};

use cpu::{Cpu, IO, parse_instr_with_arg};
use nes::cart::{Cart, CpuOrPpu, INES};
use nes::{Nes, yielded};
// use nes::ppu::{Ppu};

use once_cell::sync::Lazy;
use raylib::prelude::*;
use regex::Regex;

#[derive(PartialEq)]
struct LogState {
    pc: u16,
    // opcode: u8,
    // args: Vec<u8>,
    a: u8,
    x: u8,
    y: u8,
    p: u8,
    sp: u8,
    cyc: usize,
}

impl fmt::Display for LogState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "pc:{:04x} ", self.pc)?;
        write!(f, "a:{:02x} ", self.a)?;
        write!(f, "x:{:02x} ", self.x)?;
        write!(f, "y:{:02x} ", self.y)?;
        write!(f, "p:{:02x} ", self.p)?;
        write!(f, "sp:{:02x} ", self.sp)?;
        write!(f, "cyc:{}", self.cyc)
    }
}

impl From<(&Cpu, usize)> for LogState {
    fn from((cpu, cycle): (&Cpu, usize)) -> Self {
        LogState {
            pc: cpu.pc,
            a: cpu.a,
            x: cpu.x,
            y: cpu.y,
            p: cpu.status.bits(),
            sp: cpu.sp,
            cyc: cycle,
        }
    }
}

impl std::str::FromStr for LogState {
    type Err = Box<dyn std::error::Error>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        static RE: Lazy<Regex> = Lazy::new(|| Regex::new(
            "(....) .*A:(..) X:(..) Y:(..) P:(..) SP:(..).*CYC:(.+)").unwrap());
        let (_, [pc, a, x, y, p, sp, cyc]) = RE.captures_iter(s).next()
            .ok_or("failed to parse log line".to_owned())?.extract();
        // println!("{} {} {} {} {} {} {}", pc, a, x, y, p, sp, cyc);
        Ok(LogState {
            pc: u16::from_str_radix(pc, 16)?,
            a: u8::from_str_radix(a, 16)?,
            x: u8::from_str_radix(x, 16)?,
            y: u8::from_str_radix(y, 16)?,
            p: u8::from_str_radix(p, 16)?,
            sp: u8::from_str_radix(sp, 16)?,
            cyc: cyc.parse::<usize>()?,
        })
    }
}

// From https://doc.rust-lang.org/rust-by-example/std_misc/file/read_lines.html
fn read_lines(filename: &str) -> Vec<String> {
    fs::read_to_string(filename)
        .unwrap()  // panic on possible file-reading errors
        .lines()  // split the string into an iterator of string slices
        .map(String::from)  // make each slice into a string
        .collect()  // gather them together into a vector
}

fn parse_log(path: &str) -> Result<Vec<LogState>, Box<dyn std::error::Error>> {
    read_lines(path).iter().map(|line| line.parse()).collect()
}

fn chrrom_texture(rl: &mut RaylibHandle,
                  thread: &RaylibThread,
                  ines: &INES,
                  palette: &[Color; 64],
                  i: usize) -> Result<Texture2D, String> {
    let mut image = Image::gen_image_color(128, 128, Color::BLACK);
    // for i in 0 .. ines.chrROM.len() / 2 {
    //  let lo = ines.chrROM[2*i];
    //  let hi = ines.chrROM[2*i+1];
    //  for j in 0..8 {
    //      // (hi[j] << 1) | lo[j]
    //  }
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

    rl.load_texture_from_image(&thread, &image)
        .map_err(|e| e.to_string())
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
        let mapped_addr = if let Some((_, addr)) =
            (*cart).mapper.map(CpuOrPpu::Cpu, IO::Read.into(), pc) {
                addr
            } else {
                pc
            };
        // let Some((_, mapped_addr)) =
        //     (*cart).mapper.map(CpuOrPpu::Cpu, IO::Read.into(), pc)
        // else { panic!("{:04x}", pc) };
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
    let mut nes_process = nes.run(cart, None);

    // Set up raylib
    let (mut rl, thread) = raylib::init()
        .size(640, 480)
        .title("NES")
        .build();

    rl.set_target_fps(60);

    let chrrom_left = chrrom_texture(&mut rl, &thread, &ines, &palette, 0)?;
    let chrrom_right = chrrom_texture(&mut rl, &thread, &ines, &palette, 1)?;

    _ = yielded(Pin::new(&mut nes_process).resume(()))?;

    let pc_monitor = unsafe { (*(*nes_ptr).cpu).pc_monitor.clone() };
    let mut pc = 0;
    let mut cycles = 7*3;

    while !rl.window_should_close() {
        // Run NES cycles for the frame
        for _ in 0 .. CYCLES_PER_FRAME {
            _ = yielded(Pin::new(&mut nes_process).resume(()))?;
            let new_pc = unsafe { (*(*nes_ptr).cpu).pc };

            cycles += 1;
            if new_pc != pc {
                print_log(nes_ptr, cart_ptr, cycles / 3);
                pc = new_pc;
            }
            // if cycles / 3 > 199630 {
            //  return Ok(())
            // }
        }

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

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    // use std::ops::{CoroutineState};
    use std::pin::Pin;
    use std::sync::atomic::{Ordering};

    // Compare execution against the nestest log.
    #[test]
    fn nestest() -> Result<(), Box<dyn std::error::Error>> {
        let rom_data = fs::read("../test/rom/nestest.nes")?;
        let ines: INES = rom_data.into();
        let cart = Cart::new(ines.clone()).expect("couldn't load ROM");
        let cart_ptr = &cart as *const Cart;

        // Set up NES
        let mut nes = Nes::new();
        let nes_ptr = &nes as *const Nes;
        let mut nes_process = nes.run(cart, Some((0xc000, 0xfd)));

        let expected_log = parse_log(
            "/home/alex/source/nes-test-roms/other/nestest.log")?;

        let mut pc = 0;
        let mut i = 0;
        let mut cycles = 7 * 3;

        _ = yielded(Pin::new(&mut nes_process).resume(()))?;
        let pc_monitor = unsafe { (*(*nes_ptr).cpu).pc_monitor.clone() };
        print_log(nes_ptr, cart_ptr, cycles / 3);

        while i < expected_log.len() {
            cycles += 1;
            let new_pc = pc_monitor.load(Ordering::Relaxed);
            if new_pc != pc {
                print_log(nes_ptr, cart_ptr, cycles / 3);
                unsafe {
                    let state = (&*(*nes_ptr).cpu, cycles / 3).into();
                    if expected_log[i] != state {
                        return Err(format!("expected: {}, actual: {}",
                                           expected_log[i], state).into());
                    }
                }
                i += 1;
            }
            pc = new_pc;
            _ = yielded(Pin::new(&mut nes_process).resume(()))?;
        }
        Ok(())
    }

}
