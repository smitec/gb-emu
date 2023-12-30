mod cpu;
mod gpu;
mod helpers;
mod instructions;
mod memory;
mod registers;

use crate::cpu::*;

fn main() {
    let mut cpu = Cpu::new();
    cpu.step();
}
