use std::ops::{Coroutine};
use std::fmt;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicU16, Ordering};

use atomic_bitfield::AtomicBitField as _;
use bitflags::bitflags;

use crate::cart::{Cart, CpuOrPpu};
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
fn palette_tbl_addr(addr: u16) -> u16 {
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

// TODO: refactor to take the following into account (remove the
// actual registers for PPUADDR and PPUSCROLL, not sure about
// PPUCTRL):

// PPUADDR itself is not storage. It is an interface for writing to t and v.
// PPUSCROLL is another such interface.
// The low 2 bits of PPUCTRL are, too.
// Just because hardware presents a register for the CPU to interact with doesn't mean that that register actually has a byte of storage in the hardware.

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

    // All reads and writes go through the IO bus.
    io_bus: Arc<AtomicU8>,
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
            io_bus:   Arc::new(AtomicU8::new(0)),
        }
    }

    pub fn read_bus(&self) -> u8 {
        self.io_bus.load(Ordering::Relaxed)
    }
    pub fn write_bus(&self, val: u8) {
        self.io_bus.store(val, Ordering::Relaxed)
    }

    pub fn load_ctrl(&self) {
        self.write_bus(self.ctrl.load(Ordering::Relaxed))
    }
    pub fn store_ctrl(&self) {
        self.ctrl.store(self.read_bus(), Ordering::Relaxed)
    }
    pub fn load_mask(&self) {
        self.write_bus(self.mask.load(Ordering::Relaxed))
    }
    pub fn store_mask(&self) {
        self.mask.store(self.read_bus(), Ordering::Relaxed)
    }
    pub fn load_status(&self) {
        let masked_status = self.status.load(Ordering::Relaxed) & 0b00000111;
        self.set_w(false); // clear w
        // self.status.clear_bit(7, Ordering::Relaxed); // clear vblank flag
        // self.write_bus((self.read_bus() & 0b11111000) | masked_status)

        self.status.set_bit(7, Ordering::Relaxed); // clear vblank flag
        self.write_bus((self.read_bus() & 0b11111000) | masked_status)
    }
    pub fn store_status(&self) {
        let bus_masked = self.read_bus() & 0b00000111;
        let old_status = self.status.load(Ordering::Relaxed) & 0b11111000;
        self.status.store(bus_masked | old_status, Ordering::Relaxed)
    }
    pub fn load_oamaddr(&self) {
        self.oamaddr.store(self.read_bus(), Ordering::Relaxed)
    }
    pub fn store_oamaddr(&self) {
        self.oamaddr.store(self.read_bus(), Ordering::Relaxed)
    }
    pub fn load_oamdata(&self) {
        self.oamdata.store(self.read_bus(), Ordering::Relaxed)
    }
    pub fn store_oamdata(&self) {
        self.oamdata.store(self.read_bus(), Ordering::Relaxed)
    }
    // pub fn read_scroll(&self) -> u16 {
    //  self.scroll.load(Ordering::Relaxed)
    // }
    // pub fn write_scroll(&self, scroll: u16) {
    //  self.io_bus.store(scroll, Ordering::Relaxed);
    //  self.scroll.store(scroll, Ordering::Relaxed)
    // }
    // pub fn read_addr(&self) -> u8 {
    //  self.io_bus.load(Ordering::Relaxed) as u8
    // }
    pub fn store_addr(&self) {
        let byte = self.read_bus();
        if self.get_w() {
            self.set_t(((byte as u16) << 8) | (self.get_t() & 0x00FF));
            self.set_v(((byte as u16) << 8) |
                       (self.addr.load(Ordering::Relaxed) & 0x00FF));
            // self.addr.store(
            //  ((byte as u16) << 8) | (self.addr.load(Ordering::Relaxed) & 0x00FF),
            //  Ordering::Relaxed);
            self.addr.store(self.get_v(), Ordering::Relaxed);
            self.set_w(true)
        } else {
            // TODO: During raster effects, if the second write to
            // PPUADDR happens at specific times, at most one axis of
            // scrolling will be set to the bitwise AND of the written
            // value and the current value. The only safe time to
            // finish the second write is during blanking; see PPU
            // scrolling for more specific timing.
            self.set_t((self.get_t() & 0xFF00) | byte as u16);
            self.addr.store(
                (self.addr.load(Ordering::Relaxed) & 0xFF00) | byte as u16,
                Ordering::Relaxed);
            self.set_w(false)
        }
    }
    pub fn load_data(&self) {
        self.data.store(self.read_bus(), Ordering::Relaxed)
    }

    fn get_v(&self) -> u16 {
        self.v.load(Ordering::Relaxed)
    }
    fn set_v(&self, val: u16) {
        self.v.store(val, Ordering::Relaxed)
    }
    fn get_t(&self) -> u16 {
        self.t.load(Ordering::Relaxed)
    }
    fn set_t(&self, val: u16) {
        self.t.store(val, Ordering::Relaxed)
    }
    fn get_x(&self) -> u8 {
        self.x.load(Ordering::Relaxed)
    }
    fn set_x(&self, byte: u8) {
        self.x.store(byte, Ordering::Relaxed)
    }
    fn get_w(&self) -> bool {
        self.w.load(Ordering::Relaxed)
    }
    fn set_w(&self, b: bool) {
        self.w.store(b, Ordering::Relaxed)
    }

    // Perform a read or write on the PPU bus.
    pub fn do_io(&self,
                 cart: &mut Cart,
                 vram: &mut [u8],
                 palette_tbl: &mut [u8],
                 io: IO,
    ) -> Result<(), String> {

        if io == IO::Read {
            // Transfer read buf to PPUDATA register
            self.data.store(self.read_buf.load(Ordering::Relaxed),
                            Ordering::Relaxed);
            self.load_data()
        }

        let addr = self.addr.load(Ordering::Relaxed);
        let mapped = cart.mapper.map(CpuOrPpu::Ppu, io.into(), addr);

        // If read, load something from vram or cartridge (depending
        // on mapper) into read_buf. If write, copy it into PPUDATA
        // and also wherever the mapper wants to put it.

        match mapped {
            None => {
                // Pattern memory / "CHR ROM"
                if addr <= 0x1FFF {
                    match io {
                        IO::Read => {
                            let byte = cart.rom.chrrom[addr as usize];
                            self.read_buf.store(byte, Ordering::Relaxed)
                        }
                        IO::Write => {
                            // Is it possible to check if CHR is RAM?
                            // To throw error here if not RAM...
                            let byte = self.read_bus();
                            cart.rom.chrrom[addr as usize] = byte;
                            self.data.store(byte, Ordering::Relaxed)
                        }
                    }
                }
                // Nametable memory / internal VRAM
                else if addr <= 0x3EFF {
                    let wrapped_addr = addr & 0x800;
                    match io {
                        IO::Read => {
                            let byte = vram[wrapped_addr as usize];
                            self.read_buf.store(byte, Ordering::Relaxed)
                        }
                        IO::Write => {
                            let byte = self.read_bus();
                            vram[wrapped_addr as usize] = byte;
                            self.data.store(byte, Ordering::Relaxed)
                        }
                    }
                }
                // Palette memory
                else if addr <= 0x3FFF {
                    let wrapped_addr = addr & 0x001F;
                    let palette_addr = palette_tbl_addr(wrapped_addr);
                    match io {
                        IO::Read => {
                            let byte = palette_tbl[palette_addr as usize];
                            self.read_buf.store(byte, Ordering::Relaxed)
                        }
                        IO::Write => {
                            let byte = self.read_bus();
                            palette_tbl[palette_addr as usize] = byte;
                            self.data.store(byte, Ordering::Relaxed)
                        }
                    }
                }
                else {
                    match io {
                        IO::Read => {
                            // Read from open bus (unmapped address)
                            // returns low byte of the address.
                            self.read_buf.store(addr as u8, Ordering::Relaxed)
                        }
                        IO::Write => () // Write to unmapped memory??
                    }
                }
            }
            Some((tgt, mapped_addr)) => {
                match io {
                    IO::Read => {
                        let byte = cart.read(tgt, mapped_addr)?;
                        self.read_buf.store(byte, Ordering::Relaxed)
                    }
                    IO::Write => {
                        let byte = self.read_bus();
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
        //        Ordering::Relaxed)
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
