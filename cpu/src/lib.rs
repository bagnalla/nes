#![feature(coroutines, coroutine_trait, stmt_expr_attributes)]

// BUGS not found by the harte tests:
// - jmp abs
// - branch with -128 relative offset
// - php setting B flag (obelisk guide doesn't mention)

// TODO: consider using threads and channels instead of coroutines? Or
// async/await with channels?

// use std::collections::HashMap;
use std::ops::{Coroutine};
use std::fmt;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicU16, Ordering};

use bitflags::bitflags;

pub static PC_MONITOR: AtomicU16 = AtomicU16::new(0);
// static SP_MONITOR: AtomicU8 = AtomicU8::new(0);
// static A_MONITOR: AtomicU8 = AtomicU8::new(0);
// static X_MONITOR: AtomicU8 = AtomicU8::new(0);
// static Y_MONITOR: AtomicU8 = AtomicU8::new(0);
// static P_MONITOR: AtomicU8 = AtomicU8::new(0);

bitflags! {
    #[derive(Clone, Copy, Debug)]
    pub struct Flags: u8 {
        const C = 1 << 0;
        const Z = 1 << 1;
        const I = 1 << 2;
	const D = 1 << 3;
	const B = 1 << 4;
	const U = 1 << 5;
	const V = 1 << 6;
	const N = 1 << 7;
    }
}

#[derive(Clone, Debug)]
pub struct Cpu {
    pub pc: u16,
    pub sp: u8,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub status: Flags,
    pub read_buf: Arc<AtomicU8>,
    pub reset_signal: Arc<AtomicBool>,
    pub irq_signal: Arc<AtomicBool>,
    pub nmi_signal: Arc<AtomicBool>,
}

impl fmt::Display for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	writeln!(f, "pc: {:04x}", self.pc)?;
	writeln!(f, "sp: {:02x}", self.sp)?;
	writeln!(f, "a: {:02x}", self.a)?;
	writeln!(f, "x: {:02x}", self.x)?;
	writeln!(f, "y: {:02x}", self.y)?;
	writeln!(f, "p: {:08b}", self.status)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum IO {
    Read,
    Write(u8),
}

// Every CPU event contains an address and either reads from that
// address or writes a byte to it.
pub type CpuEvent = (u16, IO);

// impl fmt::Display for CpuEvent {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
// 	match self {
// 	    CpuEvent::Read(adr) => write!(f, "Read({:04x})", adr),
// 	    CpuEvent::Write(adr, val) =>
// 		write!(f, "Write({:04x}, {:02x})", adr, val),
// 	}
//     }
// }

impl Cpu {
    pub fn new() -> Self {
	Cpu {
	    pc: 0,
	    sp: 0xFF,
	    a: 0,
	    x: 0,
	    y: 0,
	    status: Flags::empty(),
	    read_buf: Arc::new(AtomicU8::new(0)),
	    reset_signal: Arc::new(AtomicBool::new(false)),
	    irq_signal: Arc::new(AtomicBool::new(false)),
	    nmi_signal: Arc::new(AtomicBool::new(false)),
	}
    }
    fn set_flag(&mut self, f: Flags, b: bool) {
	self.status.set(f, b)
    }
    fn get_flag(&self, f: Flags) -> bool {
	self.status.contains(f)
    }
    fn reset(&mut self) {
	self.pc = 0;
	self.sp = 0xFF;
	self.a = 0;
	self.x = 0;
	self.y = 0;
	self.status = Flags::empty();
	// self.read_buf.store(0, Ordering::Relaxed);
    }

    fn decode(op: u8) -> &'static Instr {
	const fn i(opcode: Opcode, mode: AddrMode) -> Instr {
	    Instr { opcode: opcode, mode: mode }
	}
	static INSTR_TABLE: [Instr; 256] = [
	    i(Brk, Imp), i(Ora, Izx), i(Xxx, Imp), i(Xxx, Imp), i(Nop, Zp0), i(Ora, Zp0), i(Asl, Zp0), i(Xxx, Imp), i(Php, Imp), i(Ora, Imm), i(Asl, Imp), i(Xxx, Imp), i(Nop, Abs), i(Ora, Abs), i(Asl, Abs), i(Xxx, Imp),
	    i(Bpl, Rel), i(Ora, Izy), i(Xxx, Imp), i(Xxx, Imp), i(Nop, Zpx), i(Ora, Zpx), i(Asl, Zpx), i(Xxx, Imp), i(Clc, Imp), i(Ora, Aby), i(Nop, Imp), i(Xxx, Imp), i(Nop, Abx), i(Ora, Abx), i(Asl, Abx), i(Xxx, Imp),
	    i(Jsr, Abs), i(And, Izx), i(Xxx, Imp), i(Xxx, Imp), i(Bit, Zp0), i(And, Zp0), i(Rol, Zp0), i(Xxx, Imp), i(Plp, Imp), i(And, Imm), i(Rol, Imp), i(Xxx, Imp), i(Bit, Abs), i(And, Abs), i(Rol, Abs), i(Xxx, Imp),
	    i(Bmi, Rel), i(And, Izy), i(Xxx, Imp), i(Xxx, Imp), i(Nop, Zpx), i(And, Zpx), i(Rol, Zpx), i(Xxx, Imp), i(Sec, Imp), i(And, Aby), i(Nop, Imp), i(Xxx, Imp), i(Nop, Abx), i(And, Abx), i(Rol, Abx), i(Xxx, Imp),
	    i(Rti, Imp), i(Eor, Izx), i(Xxx, Imp), i(Xxx, Imp), i(Nop, Zp0), i(Eor, Zp0), i(Lsr, Zp0), i(Xxx, Imp), i(Pha, Imp), i(Eor, Imm), i(Lsr, Imp), i(Xxx, Imp), i(Jmp, Abs), i(Eor, Abs), i(Lsr, Abs), i(Xxx, Imp),
	    i(Bvc, Rel), i(Eor, Izy), i(Xxx, Imp), i(Xxx, Imp), i(Nop, Zpx), i(Eor, Zpx), i(Lsr, Zpx), i(Xxx, Imp), i(Cli, Imp), i(Eor, Aby), i(Nop, Imp), i(Xxx, Imp), i(Nop, Abx), i(Eor, Abx), i(Lsr, Abx), i(Xxx, Imp),
	    i(Rts, Imp), i(Adc, Izx), i(Xxx, Imp), i(Xxx, Imp), i(Nop, Zp0), i(Adc, Zp0), i(Ror, Zp0), i(Xxx, Imp), i(Pla, Imp), i(Adc, Imm), i(Ror, Imp), i(Xxx, Imp), i(Jmp, Ind), i(Adc, Abs), i(Ror, Abs), i(Xxx, Imp),
	    i(Bvs, Rel), i(Adc, Izy), i(Xxx, Imp), i(Xxx, Imp), i(Nop, Zpx), i(Adc, Zpx), i(Ror, Zpx), i(Xxx, Imp), i(Sei, Imp), i(Adc, Aby), i(Nop, Imp), i(Xxx, Imp), i(Nop, Abx), i(Adc, Abx), i(Ror, Abx), i(Xxx, Imp),
	    i(Nop, Imm), i(Sta, Izx), i(Xxx, Imp), i(Xxx, Imp), i(Sty, Zp0), i(Sta, Zp0), i(Stx, Zp0), i(Xxx, Imp), i(Dey, Imp), i(Xxx, Imp), i(Txa, Imp), i(Xxx, Imp), i(Sty, Abs), i(Sta, Abs), i(Stx, Abs), i(Xxx, Imp),
	    i(Bcc, Rel), i(Sta, Izy), i(Xxx, Imp), i(Xxx, Imp), i(Sty, Zpx), i(Sta, Zpx), i(Stx, Zpy), i(Xxx, Imp), i(Tya, Imp), i(Sta, Aby), i(Txs, Imp), i(Xxx, Imp), i(Xxx, Imp), i(Sta, Abx), i(Xxx, Imp), i(Xxx, Imp),
	    i(Ldy, Imm), i(Lda, Izx), i(Ldx, Imm), i(Xxx, Imp), i(Ldy, Zp0), i(Lda, Zp0), i(Ldx, Zp0), i(Xxx, Imp), i(Tay, Imp), i(Lda, Imm), i(Tax, Imp), i(Xxx, Imp), i(Ldy, Abs), i(Lda, Abs), i(Ldx, Abs), i(Xxx, Imp),
	    i(Bcs, Rel), i(Lda, Izy), i(Xxx, Imp), i(Xxx, Imp), i(Ldy, Zpx), i(Lda, Zpx), i(Ldx, Zpy), i(Xxx, Imp), i(Clv, Imp), i(Lda, Aby), i(Tsx, Imp), i(Xxx, Imp), i(Ldy, Abx), i(Lda, Abx), i(Ldx, Aby), i(Xxx, Imp),
	    i(Cpy, Imm), i(Cmp, Izx), i(Xxx, Imp), i(Xxx, Imp), i(Cpy, Zp0), i(Cmp, Zp0), i(Dec, Zp0), i(Xxx, Imp), i(Iny, Imp), i(Cmp, Imm), i(Dex, Imp), i(Xxx, Imp), i(Cpy, Abs), i(Cmp, Abs), i(Dec, Abs), i(Xxx, Imp),
	    i(Bne, Rel), i(Cmp, Izy), i(Xxx, Imp), i(Xxx, Imp), i(Nop, Zpx), i(Cmp, Zpx), i(Dec, Zpx), i(Xxx, Imp), i(Cld, Imp), i(Cmp, Aby), i(Nop, Imp), i(Xxx, Imp), i(Nop, Abx), i(Cmp, Abx), i(Dec, Abx), i(Xxx, Imp),
	    i(Cpx, Imm), i(Sbc, Izx), i(Xxx, Imp), i(Xxx, Imp), i(Cpx, Zp0), i(Sbc, Zp0), i(Inc, Zp0), i(Xxx, Imp), i(Inx, Imp), i(Sbc, Imm), i(Nop, Imp), i(Xxx, Imp), i(Cpx, Abs), i(Sbc, Abs), i(Inc, Abs), i(Xxx, Imp),
	    i(Beq, Rel), i(Sbc, Izy), i(Xxx, Imp), i(Xxx, Imp), i(Nop, Zpx), i(Sbc, Zpx), i(Inc, Zpx), i(Xxx, Imp), i(Sed, Imp), i(Sbc, Aby), i(Nop, Imp), i(Xxx, Imp), i(Nop, Abx), i(Sbc, Abx), i(Inc, Abx), i(Xxx, Imp),
	];
	&INSTR_TABLE[op as usize]
    }

    fn update_monitor(&self) {
	PC_MONITOR.store(self.pc, Ordering::Relaxed);
	// SP_MONITOR.store(self.sp, Ordering::Relaxed);
	// A_MONITOR.store(self.a, Ordering::Relaxed);
	// X_MONITOR.store(self.x, Ordering::Relaxed);
	// Y_MONITOR.store(self.y, Ordering::Relaxed);
	// P_MONITOR.store(self.status.bits(), Ordering::Relaxed);
    }

    pub fn run(&mut self)
	   -> Box<dyn Coroutine<Yield = CpuEvent, Return = String> + Unpin + '_> {
	
	macro_rules! fetch {
	    ( $adr:expr ) => {
		{
		    yield ($adr, IO::Read);
		    self.read_buf.load(Ordering::Relaxed)
		}
	    };
	}

	macro_rules! write {
	    ( $adr:expr, $val:expr ) => {
		{
		    yield ($adr, IO::Write($val))
		}
	    };
	}

	macro_rules! next_pc {
	    ( ) => {
		// {
		//     let val = fetch!(self.pc);
		//     self.pc = self.pc.wrapping_add(1);
		//     val
		// }
		{
		    yield (self.pc, IO::Read);
		    self.pc = self.pc.wrapping_add(1);
		    self.read_buf.load(Ordering::Relaxed)
		}
	    };
	}

	macro_rules! push {
	    ( $x:expr ) => {
		{
		    write!(0x0100 + self.sp as u16, $x);
		    self.sp = self.sp.wrapping_sub(1)
		}
	    };
	}

	// Maybe shouldn't call this peek since it reads the byte past
	// the end of the stack.
	macro_rules! peek {
	    ( ) => {
		{
		    fetch!(0x0100 + self.sp as u16)
		}
	    };
	}

	macro_rules! pop {
	    ( ) => {
		{
		    self.sp = self.sp.wrapping_add(1);
		    fetch!(0x0100 + self.sp as u16)
		}
	    };
	}

	// Branch to pc+rel if $flag is $b.
	macro_rules! branch {
	    ( $rel:expr, $flag:expr, $b:expr ) => {
		if self.get_flag($flag) == $b {
		    let pcl = self.pc as u8;
		    let pch = self.pc & 0xFF00;
		    let _ = next_pc!();
		    if $rel >= 0 {
			let (new_pcl, carry) = pcl.overflowing_add($rel as u8);
			self.pc = pch | new_pcl as u16; // Mask unnecessary?
			if carry {
			    let _ = fetch!(self.pc);
			    self.pc = self.pc.wrapping_add(256)
			}
		    } else {
			let (new_pcl, carry) = pcl.overflowing_sub($rel.unsigned_abs());
			self.pc = pch | new_pcl as u16;
			if carry {
			    let _ = fetch!(self.pc);
			    self.pc = self.pc.wrapping_sub(256)
			}
		    }
		}
	    };
	}	

	let go = #[coroutine] move || {
	    loop {
		// println!("{}", self.pc);
		self.update_monitor();

		// Poll reset/interrupt signals
		if self.reset_signal.load(Ordering::Relaxed) {
		    // Wait for signal to be cleared to actually proceed?
		    // while self.reset_signal.load(Ordering::Relaxed) {
		    // 	cycle!()
		    // }
		    let _ = fetch!(0x00FF);
		    let _ = fetch!(0x00FF);
		    let _ = fetch!(0x0100);
		    self.reset();
		    // let _ = pop!();
		    // let _ = pop!();
		    // let _ = pop!();
		    let pcl = fetch!(0xFFFC) as u16;
		    self.set_flag(Flags::I, true); // ?
		    let pch = fetch!(0xFFFD) as u16;
		    self.pc = (pch << 8) | pcl;
		    self.reset_signal.store(false, Ordering::Relaxed);
		}

		if self.nmi_signal.load(Ordering::Relaxed) {
		    // println!("NMI!");
		    let _ = fetch!(self.pc);
		    let _ = fetch!(self.pc);
		    self.set_flag(Flags::B, false);
		    push!(((self.pc & 0xFF00) >> 8) as u8); // Push PCH
		    push!((self.pc & 0x00FF) as u8); // Push PCL
		    self.set_flag(Flags::U, true);
		    push!(self.status.bits());
		    let pcl = fetch!(0xFFFA) as u16;
		    self.set_flag(Flags::I, true); // Conflicting documentation
		    let pch = fetch!(0xFFFB) as u16;
		    self.pc = (pch << 8) | pcl;
		    self.nmi_signal.store(false, Ordering::Relaxed);
		    continue
		}

		if self.irq_signal.load(Ordering::Relaxed) && !self.get_flag(Flags::I) {
		    // println!("IRQ!");
		    let _ = fetch!(self.pc);
		    let _ = fetch!(self.pc);
		    self.set_flag(Flags::B, false);
		    push!(((self.pc & 0xFF00) >> 8) as u8); // Push PCH
		    push!((self.pc & 0x00FF) as u8); // Push PCL
		    self.set_flag(Flags::U, true);
		    push!(self.status.bits());
		    if self.nmi_signal.load(Ordering::Relaxed) {
			let pcl = fetch!(0xFFFA) as u16;
			self.set_flag(Flags::I, true); // Conflicting documentation
			let pch = fetch!(0xFFFB) as u16;
			self.pc = (pch << 8) | pcl;
			self.nmi_signal.store(false, Ordering::Relaxed);
		    } else {
			let pcl = fetch!(0xFFFE) as u16;
			self.set_flag(Flags::I, true);
			let pch = fetch!(0xFFFF) as u16;
			self.pc = (pch << 8) | pcl;
			self.irq_signal.store(false, Ordering::Relaxed);
		    }
		    continue
		}
		let instr_code = next_pc!();
		let instr = Self::decode(instr_code);

		let mut lo_carry = false;
		let mut rel: i8 = 0;
		let fetch_addr: u16 = match instr.mode {
		    AddrMode::Imp => {
			let _ = fetch!(self.pc);
			0
		    }
		    AddrMode::Imm => {
			let adr = self.pc;
			self.pc = self.pc.wrapping_add(1);
			adr
		    }
		    AddrMode::Zp0 => next_pc!().into(),
		    AddrMode::Zpx => {
			let adr = next_pc!();
			let _ = fetch!(adr.into());
			(adr.wrapping_add(self.x)).into()
		    }
		    AddrMode::Zpy => {
			let adr = next_pc!();
			let _ = fetch!(adr.into());
			(adr.wrapping_add(self.y)).into()
		    }
		    AddrMode::Rel => {
			rel = next_pc!() as i8;
			0
		    }
		    AddrMode::Abs => match instr.opcode {
			Jsr => 0,
			_ => {
			    let (lo, hi) = (next_pc!(), next_pc!());
			    ((hi as u16) << 8) | lo as u16
			}
		    }
		    AddrMode::Abx => {
			let (lo, hi) = (next_pc!(), next_pc!());
			let (lo2, carry) = lo.overflowing_add(self.x);
			lo_carry = carry;
			((hi as u16) << 8) | lo2 as u16
		    }
		    AddrMode::Aby => {
			let (lo, hi) = (next_pc!(), next_pc!());
			let (lo2, carry) = lo.overflowing_add(self.y);
			lo_carry = carry;
			((hi as u16) << 8) | lo2 as u16
		    }
		    AddrMode::Ind => {
			let (lo, hi) = (next_pc!(), next_pc!());
			let loc = (hi as u16) << 8 | lo as u16;
			let ptr_lo = fetch!(loc);
			let ptr_hi = fetch!(if lo == 0x00FF { // Page boundary bug
			    loc & 0xFF00
			} else {
			    loc + 1
			});
			((ptr_hi as u16) << 8) | ptr_lo as u16
		    }
		    AddrMode::Izx => {
			let t = next_pc!();
			let _ = fetch!(t.into());
			let lo = fetch!(t.wrapping_add(self.x).into());
			let hi = fetch!(t.wrapping_add(self.x).wrapping_add(1).into());
			((hi as u16) << 8) | lo as u16
		    }
		    AddrMode::Izy => {
			let t = next_pc!();
			let lo = fetch!(t as u16);
			let hi = fetch!(t.wrapping_add(1).into());
			let (lo2, carry) = lo.overflowing_add(self.y);
			lo_carry = carry;
			((hi as u16) << 8) | lo2 as u16
		    }
		};

		let corrected_addr = if lo_carry {
		    fetch_addr.wrapping_add(256)
		} else {
		    fetch_addr
		};

		// Does second fetch with lo_carry applied when necessary.
		macro_rules! fetch_oops {
		    ( ) => {
			{
			    let val = fetch!(fetch_addr);
			    if lo_carry {
				fetch!(corrected_addr)
			    } else {
				val
			    }
			}
		    };
		}

		// Does second fetch when addressing mode is Abx or
		// Aby regardless of whether lo_carry needs to be
		// applied (thus results in a totally redundant extra
		// read when lo_carry is false).
		macro_rules! fetch_oops_store {
		    ( $mode:expr ) => {
			{
			    let val = fetch!(fetch_addr);
			    match $mode {
				AddrMode::Abx | AddrMode::Aby => {
				    fetch!(if lo_carry {
					fetch_addr.wrapping_add(256)
				    } else {
					fetch_addr
				    })
				}
				_ => val
			    }
			}
		    };
		}

		match instr.opcode {
		    Nop => match instr.mode {
			AddrMode::Imp => (),
			_ => {
			    let _ = fetch_oops!();
			}
		    }
		    And => {
			let val = fetch_oops!();
			self.a &= val;
			self.set_flag(Flags::Z, self.a == 0);
			self.set_flag(Flags::N, self.a & 0x80 != 0);
		    }
		    Adc => {
			let val = fetch_oops!();
			let res =
			    val as u16 + self.a as u16 + self.get_flag(Flags::C) as u16;
			let a_sign = self.a & 0b10000000;
			let val_sign = val & 0b10000000;
			let res_sign = res as u8 & 0b10000000;
			let signed_overflow = a_sign == val_sign && a_sign != res_sign;
			self.a = res as u8;
			self.set_flag(Flags::C, res & 0xFF00 != 0);
			self.set_flag(Flags::Z, self.a == 0);
			self.set_flag(Flags::V, signed_overflow);
			self.set_flag(Flags::N, res_sign > 0);
		    }
		    Sbc => {
			let val = fetch_oops!() ^ 0xFF;
			let res =
			    val as u16 + self.a as u16 + self.get_flag(Flags::C) as u16;
			let a_sign = self.a & 0b10000000;
			let val_sign = val & 0b10000000;
			let res_sign = res as u8 & 0b10000000;
			let signed_overflow = a_sign == val_sign && a_sign != res_sign;
			self.a = res as u8;
			self.set_flag(Flags::C, res & 0xFF00 != 0);
			self.set_flag(Flags::Z, self.a == 0);
			self.set_flag(Flags::V, signed_overflow);
			self.set_flag(Flags::N, res_sign > 0);
		    }

		    Asl => {
			match instr.mode {
			    AddrMode::Imp => {
				let shifted_bit = self.a & 0b10000000;
				self.a <<= 1;
				self.set_flag(Flags::C, shifted_bit != 0);
				self.set_flag(Flags::Z, self.a == 0);
				self.set_flag(Flags::N, self.a & 0b10000000 != 0);
			    }
			    _ => {
				let val = fetch_oops_store!(instr.mode);
				write!(corrected_addr, val);
				let shifted_bit = val & 0b10000000;
				let res = val << 1;
				self.set_flag(Flags::C, shifted_bit != 0);
				self.set_flag(Flags::Z, res == 0);
				self.set_flag(Flags::N, res & 0b10000000 != 0);
				write!(corrected_addr, res);
			    }
			}
		    }

		    Lsr => {
			match instr.mode {
			    AddrMode::Imp => {
				let shifted_bit = self.a & 1;
				self.a >>= 1;
				self.set_flag(Flags::C, shifted_bit != 0);
				self.set_flag(Flags::Z, self.a == 0);
				self.set_flag(Flags::N, false);
			    }
			    _ => {
				let val = fetch_oops_store!(instr.mode);
				write!(corrected_addr, val);
				let shifted_bit = val & 1;
				let res = val >> 1;
				self.set_flag(Flags::C, shifted_bit != 0);
				self.set_flag(Flags::Z, res == 0);
				self.set_flag(Flags::N, false);
				write!(corrected_addr, res);
			    }
			}
		    }

		    Rol => {
			match instr.mode {
			    AddrMode::Imp => {
				let shifted_bit = self.a & 0b10000000;
				self.a = (self.a << 1) | self.get_flag(Flags::C) as u8;
				self.set_flag(Flags::C, shifted_bit != 0);
				self.set_flag(Flags::Z, self.a == 0);
				self.set_flag(Flags::N, self.a & 0b10000000 != 0);
			    }
			    _ => {
				let val = fetch_oops_store!(instr.mode);
				write!(corrected_addr, val);
				let shifted_bit = val & 0b10000000;
				let res = (val << 1) | self.get_flag(Flags::C) as u8;
				self.set_flag(Flags::C, shifted_bit != 0);
				self.set_flag(Flags::Z, res == 0);
				self.set_flag(Flags::N, res & 0b10000000 != 0);
				write!(corrected_addr, res);
			    }
			}
		    }

		    Ror => {
			match instr.mode {
			    AddrMode::Imp => {
				let shifted_bit = self.a & 1;
				self.a =
				    (self.get_flag(Flags::C) as u8) << 7 | self.a >> 1;
				self.set_flag(Flags::C, shifted_bit != 0);
				self.set_flag(Flags::Z, self.a == 0);
				self.set_flag(Flags::N, self.a & 0b10000000 != 0);
			    }
			    _ => {
				let val = fetch_oops_store!(instr.mode);
				write!(corrected_addr, val);
				let shifted_bit = val & 1;
				let res =
				    (self.get_flag(Flags::C) as u8) << 7 | val >> 1;
				self.set_flag(Flags::C, shifted_bit != 0);
				self.set_flag(Flags::Z, res == 0);
				self.set_flag(Flags::N, res & 0b10000000 != 0);
				write!(corrected_addr, res);
			    }
			}
		    }

		    Bit => {
			let val = fetch!(fetch_addr);
			self.set_flag(Flags::Z, self.a & val == 0);
			self.set_flag(Flags::V, val & 0b01000000 != 0);
			self.set_flag(Flags::N, val & 0b10000000 != 0);
		    }

		    Bcc => branch!(rel, Flags::C, false),
		    Bcs => branch!(rel, Flags::C, true),
		    Bne => branch!(rel, Flags::Z, false),
		    Beq => branch!(rel, Flags::Z, true),
		    Bpl => branch!(rel, Flags::N, false),
		    Bmi => branch!(rel, Flags::N, true),
		    Bvc => branch!(rel, Flags::V, false),
		    Bvs => branch!(rel, Flags::V, true),
		    Clc => self.set_flag(Flags::C, false),
		    Cld => self.set_flag(Flags::D, false),
		    Cli => self.set_flag(Flags::I, false),
		    Clv => self.set_flag(Flags::V, false),
		    Sec => self.set_flag(Flags::C, true),
		    Sed => self.set_flag(Flags::D, true),
		    Sei => self.set_flag(Flags::I, true),

		    Cmp => {
			let val = fetch_oops!();
			self.set_flag(Flags::C, self.a >= val);
			self.set_flag(Flags::Z, self.a == val);
			let (res, _) = self.a.overflowing_sub(val);
			self.set_flag(Flags::N, res & 0b10000000 != 0);
		    }
		    Cpx => {
			let val = fetch_oops!();
			self.set_flag(Flags::C, self.x >= val);
			self.set_flag(Flags::Z, self.x == val);
			let (res, _) = self.x.overflowing_sub(val);
			self.set_flag(Flags::N, res & 0b10000000 != 0);
		    }
		    Cpy => {
			let val = fetch_oops!();
			self.set_flag(Flags::C, self.y >= val);
			self.set_flag(Flags::Z, self.y == val);
			let (res, _) = self.y.overflowing_sub(val);
			self.set_flag(Flags::N, res & 0b10000000 != 0);
		    }

		    Dec => {
			let val = fetch_oops_store!(instr.mode);
			write!(corrected_addr, val);
			let (res, _) = val.overflowing_sub(1);
			self.set_flag(Flags::Z, res == 0);
			self.set_flag(Flags::N, res & 0b10000000 != 0);
			write!(corrected_addr, res);
		    }
		    Dex => {
			let (res, _) = self.x.overflowing_sub(1);
			self.set_flag(Flags::Z, res == 0);
			self.set_flag(Flags::N, res & 0b10000000 != 0);
			self.x = res;
		    }
		    Dey => {
			let (res, _) = self.y.overflowing_sub(1);
			self.set_flag(Flags::Z, res == 0);
			self.set_flag(Flags::N, res & 0b10000000 != 0);
			self.y = res;
		    }

		    Eor => {
			let val = fetch_oops!();
			self.a ^= val;
			self.set_flag(Flags::Z, self.a == 0);
			self.set_flag(Flags::N, self.a & 0b10000000 != 0);
		    }

		    Ora => {
			let val = fetch_oops!();
			self.a |= val;
			self.set_flag(Flags::Z, self.a == 0);
			self.set_flag(Flags::N, self.a & 0b10000000 != 0);
		    }

		    Inc => {
			let val = fetch_oops_store!(instr.mode);
			write!(corrected_addr, val);
			let (res, _) = val.overflowing_add(1);
			self.set_flag(Flags::Z, res == 0);
			self.set_flag(Flags::N, res & 0b10000000 != 0);
			write!(corrected_addr, res);
		    }
		    Inx => {
			let (res, _) = self.x.overflowing_add(1);
			self.set_flag(Flags::Z, res == 0);
			self.set_flag(Flags::N, res & 0b10000000 != 0);
			self.x = res;
		    }
		    Iny => {
			let (res, _) = self.y.overflowing_add(1);
			self.set_flag(Flags::Z, res == 0);
			self.set_flag(Flags::N, res & 0b10000000 != 0);
			self.y = res;
		    }

		    Jmp => self.pc = fetch_addr,

		    Lda => {
			self.a = fetch_oops!();
			self.set_flag(Flags::Z, self.a == 0);
			self.set_flag(Flags::N, self.a & 0b10000000 != 0);
		    }
		    Ldx => {
			self.x = fetch_oops!();
			self.set_flag(Flags::Z, self.x == 0);
			self.set_flag(Flags::N, self.x & 0b10000000 != 0);
		    }
		    Ldy => {
			self.y = fetch_oops!();
			self.set_flag(Flags::Z, self.y == 0);
			self.set_flag(Flags::N, self.y & 0b10000000 != 0);
		    }

		    Sta => match instr.mode {
			Abx | Aby | Izy => {
			    let _ = fetch!(fetch_addr);
			    write!(corrected_addr, self.a)
			}
			_ => write!(fetch_addr, self.a),
		    }
		    Stx => write!(fetch_addr, self.x),
		    Sty => write!(fetch_addr, self.y),

		    Tax => {
			self.x = self.a;
			self.set_flag(Flags::Z, self.x == 0);
			self.set_flag(Flags::N, self.x & 0b10000000 != 0);
		    }
		    Tay => {
			self.y = self.a;
			self.set_flag(Flags::Z, self.y == 0);
			self.set_flag(Flags::N, self.y & 0b10000000 != 0);
		    }
		    Tsx => {
			self.x = self.sp;
			self.set_flag(Flags::Z, self.x == 0);
			self.set_flag(Flags::N, self.x & 0b10000000 != 0);
		    }
		    Txa => {
			self.a = self.x;
			self.set_flag(Flags::Z, self.a == 0);
			self.set_flag(Flags::N, self.a & 0b10000000 != 0);
		    }
		    Tya => {
			self.a = self.y;
			self.set_flag(Flags::Z, self.a == 0);
			self.set_flag(Flags::N, self.a & 0b10000000 != 0);
		    }
		    Txs => {
			self.sp = self.x;
		    }

		    Pha => push!(self.a),
		    Php => {
			self.set_flag(Flags::B, true);
			self.set_flag(Flags::U, true);
			push!(self.status.bits())
		    }

		    Pla => {
			let _ = peek!();
			self.a = pop!();
			self.set_flag(Flags::Z, self.a == 0);
			self.set_flag(Flags::N, self.a & 0b10000000 != 0);
		    }
		    Plp => {
			let _ = peek!();
			self.status = Flags::from_bits_retain(pop!())
		    }

		    Brk => {
			// Implied already did dummy read
			self.pc = self.pc.wrapping_add(1);
			self.set_flag(Flags::B, true);
			push!(((self.pc & 0xFF00) >> 8) as u8); // Push PCH
			push!((self.pc & 0x00FF) as u8); // Push PCL
			self.set_flag(Flags::U, true);
			push!(self.status.bits());
			// self.set_flag(Flags::I, true); // ???
			// Check for NMI hijack
			if self.nmi_signal.load(Ordering::Relaxed) {
			    let pcl = fetch!(0xFFFA) as u16;
			    self.set_flag(Flags::I, true);
			    let pch = fetch!(0xFFFB) as u16;
			    self.pc = (pch << 8) | pcl;
			} else {
			    let pcl = fetch!(0xFFFE) as u16;
			    self.set_flag(Flags::I, true);
			    let pch = fetch!(0xFFFF) as u16;
			    self.pc = (pch << 8) | pcl
			}
		    }
		    Rti => {
			let _ = peek!();
			self.status = Flags::from_bits_retain(pop!());
			let pcl = pop!() as u16;
			let pch = pop!() as u16;
			self.pc = (pch << 8) | pcl
		    }
		    Rts => {
			let _ = peek!();
			let pcl = pop!() as u16;
			let pch = pop!() as u16;
			self.pc = (pch << 8) | pcl;
			let _ = fetch!(self.pc as u16);
			self.pc = self.pc.wrapping_add(1);
		    }
		    Jsr => {
			let lo = next_pc!() as u16;
			let _ = peek!();
			push!(((self.pc & 0xFF00) >> 8) as u8); // Push PCH
			push!((self.pc & 0x00FF) as u8); // Push PCL
			let hi = next_pc!() as u16;
			self.pc = (hi << 8) | lo;
		    }

		    // Xxx => panic!("Unsupported opcode {:02x}", instr_code)
		    Xxx => return format!("Unsupported opcode {:02x}", instr_code)
		    // Xxx => (),
		}
	    }
	};
	Box::new(go)
    }
}

#[derive(Clone, Copy, Debug)]
enum AddrMode {
    Imp, Imm, Zp0, Zpx, Zpy, Rel, Abs, Abx, Aby, Ind, Izx, Izy
}
use AddrMode::*;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Opcode {
    Adc, And, Asl, Bcc, Bcs, Beq, Bit, Bmi, Bne, Bpl, Brk, Bvc, Bvs,
    Clc, Cld, Cli, Clv, Cmp, Cpx, Cpy, Dec, Dex, Dey, Eor, Inc, Inx,
    Iny, Jmp, Jsr, Lda, Ldx, Ldy, Lsr, Nop, Ora, Pha, Php, Pla, Plp,
    Rol, Ror, Rti, Rts, Sbc, Sec, Sed, Sei, Sta, Stx, Sty, Tax, Tay,
    Tsx, Txa, Txs, Tya, Xxx
}
use Opcode::*;

#[derive(Clone, Copy, Debug)]
struct Instr {
    opcode: Opcode,
    mode: AddrMode,
}

// fn show_prog(mem: &[u8],
// 	     pc_instr_map: &HashMap<u16, InstrWithArg>,
// 	     ctx_size: usize) {
//     let pc = PC_MONITOR.load(Ordering::Relaxed) as usize;
//     for i in (pc - std::cmp::min(pc, ctx_size))..
// 	(std::cmp::min(mem.len(), pc + ctx_size)) {
// 	    let _ = pc_instr_map.get(&(i as u16)).map(|instr|{
// 		println!("{:04x}: {}{}", i, instr, if i == pc { " <--" } else { "" })
// 	    });
//     }
// }

#[derive(Clone, Copy, Debug)]
pub enum Arg {
    Rel(i8),
    Abs(u16),
    Imm(u8),
}

#[derive(Clone, Copy, Debug)]
pub struct InstrWithArg {
    instr: Instr,
    arg: Option<Arg>
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	match self {
	    Arg::Rel(x) => write!(f, "Rel({:x})", x),
	    Arg::Abs(x) => write!(f, "Abs({:x})", x),
	    Arg::Imm(x) => write!(f, "Imm({:x})", x),
	}
    }
}

impl fmt::Display for InstrWithArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	match self.arg {
	    None => write!(f, "{:?}", self.instr),
	    Some(x) => write!(f, "{:?} {}", self.instr, x)
	}
    }
}

pub fn parse_instr_with_arg(mem: &[u8], pc: usize) -> (InstrWithArg, usize) {
    let instr_code = mem[pc];
    let instr = Cpu::decode(instr_code);
    match instr.mode {
	Imp => (InstrWithArg { instr: *instr,
			       arg: None }, 1),
	Rel => {
	    let byte = mem[pc+1];
	    (InstrWithArg { instr: *instr,
			    arg: Some(Arg::Rel(byte as i8)) }, 1)
	}
	Imm => {
	    let byte = mem[pc+1];
	    (InstrWithArg { instr: *instr,
			    arg: Some(Arg::Imm(byte)) }, 1)
	}
	_ => {
	    let lo = mem[pc+1] as u16;
	    let hi = mem[pc+2] as u16;
	    (InstrWithArg { instr: *instr,
			    arg: Some(Arg::Abs((hi << 8) | lo)) }, 2)
	}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::ops::{CoroutineState};
    use std::pin::Pin;
    use serde::{Deserialize, Serialize};

    #[derive(Clone, Copy, Debug)]
    enum Arg {
	Rel(i8),
	Abs(u16),
	Imm(u8),
    }

    #[derive(Debug, Serialize, Deserialize)]
    struct TestState {
	pc: u16,
	s: u8,
	a: u8,
	x: u8,
	y: u8,
	p: u8,
	ram: Vec<(u16, u8)>
    }

    #[derive(Debug, Serialize, Deserialize)]
    struct Test {
	name: String,
	initial: TestState,
	r#final: TestState,
	cycles: Vec<(u16, u8, String)>,
    }

    fn run_harte_test(test: Test) -> Result<(), String> {
	println!("{:?}", test);

	let mut cpu = Cpu::new();
	cpu.pc = test.initial.pc;
	cpu.sp = test.initial.s;
	cpu.a = test.initial.a;
	cpu.x = test.initial.x;
	cpu.y = test.initial.y;
	cpu.status = Flags::from_bits_retain(test.initial.p);
	let buf = cpu.read_buf.clone();
	let mut cpu_process = cpu.run();

	let mut mem = vec![0; 2_usize.pow(16)];
	for (addr, val) in test.initial.ram {
	    mem[addr as usize] = val;
	}

	for (test_addr, test_val, test_rw) in test.cycles {
	    match Pin::new(&mut cpu_process).resume(()) {
		CoroutineState::Yielded((addr, event)) => {
		    // println!("{}", event);
		    match event {
			IO::Read => {
			    if test_rw != "read" || addr != test_addr {
				return Err(format!("expected '{} {:04x}', got {:?} {}",
						   test_rw, test_addr, event, addr))
			    }
			    if mem[addr as usize] != test_val {
				return Err(format!(
				    "{:?} {} wrong memory value. \
				     expected {:x}, got {:x}",
				    event, addr, test_val, mem[addr as usize]))
			    }
			    buf.store(mem[addr as usize], Ordering::Relaxed)
			}
			IO::Write(byte) => {
			    if test_rw != "write" || addr != test_addr {
				return Err(format!("expected '{} {:04x}', got {:?} {}",
						   test_rw, test_addr, event, addr))
			    }
			    mem[addr as usize] = byte
			}
		    }
		}
		CoroutineState::Complete(msg) => {
		    return Err(msg)
		}
	    }
	}
	Ok(())
    }

    #[test]
    fn harte() -> Result<(), Box<dyn std::error::Error>> {
	for op in 0..=255 {
	    if Cpu::decode(op).opcode == Opcode::Xxx {
		continue
	    }
	    println!("{:02x}", op);
	    let path = format!("/home/alex/source/65x02/6502/v1/{:02x}.json", op);
	    let file = fs::File::open(path)?;
	    let tests: Vec<Test> = serde_json::from_reader(file)?;
	    for test in tests {
		run_harte_test(test)?
	    }
	}
	Ok(())
    }

    fn load_mem(path: &str, start_addr: u16)
		-> Result<Vec<u8>, Box<dyn std::error::Error>> {
	let bin = std::fs::read(path)?;
	let mut mem = [0; 65536];
	mem[start_addr as usize..start_addr as usize + bin.len()].copy_from_slice(&bin);
	Ok(mem.into())
    }

    #[test]
    fn klaus_functional() -> Result<(), Box<dyn std::error::Error>> {
	const PROGRAM_START: u16 = 0x400;
	const SUCCESS_ADDR: u16 = 0x336d;
	let path = "/home/alex/Dropbox/6502_65C02_functional_tests/6502_functional_test.bin";
	let mut mem: Vec<u8> = load_mem(path, 0x000A)?;

	let mut cpu = Cpu::new();
	cpu.pc = PROGRAM_START;
	let buf = cpu.read_buf.clone();
	let mut cpu_process = cpu.run();

	// let mut trace: Vec<(u16, InstrWithArg)> = Vec::new();
	// let mut pc_instr_map: HashMap<u16, InstrWithArg> = HashMap::new();
	// let mut break_count = 10;

	let mut n = 0;

	let mut old_pc = 1;
	let mut same_pc_counter = 0;
	loop {
	    n += 1;
	    let pc = PC_MONITOR.load(Ordering::Relaxed);
	    if pc == old_pc {
		same_pc_counter += 1;
		if same_pc_counter >= 10 {
		    break
		}
	    } else {
		same_pc_counter = 0;
	    }
	    old_pc = pc;

	    // let (cur_instr, _) = parse_instr_with_arg(&mem, pc as usize);
	    // println!("{:04x}: {}", pc, cur_instr);
	    // let a = A_MONITOR.load(Ordering::Relaxed);
	    // let x = X_MONITOR.load(Ordering::Relaxed);
	    // let y = Y_MONITOR.load(Ordering::Relaxed);
	    // if PC_MONITOR.load(Ordering::Relaxed) == 0x3477 {
	    // 	if break_count == 0 {
	    // 	    break
	    // 	} else {
	    // 	    break_count -= 1
	    // 	}
	    // }

	    match Pin::new(&mut cpu_process).resume(()) {
		CoroutineState::Yielded((addr, event)) => {
		    match event {
			IO::Read => {
			    buf.store(mem[addr as usize], Ordering::Relaxed)
			}
			IO::Write(byte) => {
			    mem[addr as usize] = byte
			}
		    }
		}
		CoroutineState::Complete(msg) => {
		    panic!("{}", msg)
		}
	    }
	}

	// show_prog(&mem, &pc_instr_map, 500);

	if old_pc == SUCCESS_ADDR {
	    Ok(())
	} else {
	    println!("{}", n);
	    let (instr, _) = parse_instr_with_arg(&mem, old_pc.into());
	    println!("{}", instr);
	    Err(format!("trapped at {:04x}", old_pc).into())
	}
    }

    #[test]
    fn klaus_interrupt() -> Result<(), Box<dyn std::error::Error>> {
	const PROGRAM_START: u16 = 0x400;
	const SUCCESS_ADDR: u16 = 0x06F5;
	const FEEDBACK_ADDR: usize = 0xBFFC;
	const IRQ_BIT: u8 = 1 << 0;
	const NMI_BIT: u8 = 1 << 1;
	let path = "/home/alex/Dropbox/6502_65C02_functional_tests/6502_interrupt_test.bin";
	let mut mem: Vec<u8> = load_mem(path, 0x000A)?;

	let mut cpu = Cpu::new();
	cpu.pc = PROGRAM_START;
	PC_MONITOR.store(PROGRAM_START, Ordering::Relaxed);
	let buf = cpu.read_buf.clone();
	let irq_signal = cpu.irq_signal.clone();
	let nmi_signal = cpu.nmi_signal.clone();
	let mut cpu_process = cpu.run();

	let mut n = 0;

	let mut old_pc = 1;
	let mut same_pc_counter = 0;
	let mut prev_irq = mem[FEEDBACK_ADDR] & IRQ_BIT != 0;
	let mut prev_nmi = mem[FEEDBACK_ADDR] & IRQ_BIT != 0;
	loop {
	    n += 1;

	    let irq = mem[FEEDBACK_ADDR] & IRQ_BIT != 0;
	    let nmi = mem[FEEDBACK_ADDR] & NMI_BIT != 0;
	    if nmi && !prev_nmi {
		println!("triggering nmi");
		nmi_signal.store(true, Ordering::Relaxed);
	    }
	    if irq && !prev_irq {
		println!("triggering irq");
		irq_signal.store(true, Ordering::Relaxed);
	    }
	    prev_irq = irq;
	    prev_nmi = nmi;

	    let pc = PC_MONITOR.load(Ordering::Relaxed);
	    if pc == old_pc {
		same_pc_counter += 1;
		if same_pc_counter >= 10 {
		    break
		}
	    } else {
		same_pc_counter = 0;
	    }
	    old_pc = pc;

	    // let (cur_instr, _) = parse_instr_with_arg(&mem, pc as usize);
	    // println!("{:04x}: {}", pc, cur_instr);

	    match Pin::new(&mut cpu_process).resume(()) {
		CoroutineState::Yielded((addr, event)) => {
		    match event {
			IO::Read => {
			    buf.store(mem[addr as usize], Ordering::Relaxed)
			}
			IO::Write(byte) => {
			    mem[addr as usize] = byte
			}
		    }
		}
		CoroutineState::Complete(msg) => {
		    panic!("{}", msg)
		}
	    }
	}

	println!("{}", n);
	if old_pc == SUCCESS_ADDR {
	    Ok(())
	} else {
	    let (instr, _) = parse_instr_with_arg(&mem, old_pc.into());
	    println!("{}", instr);
	    Err(format!("trapped at {:04x}", old_pc).into())
	}
    }
}
