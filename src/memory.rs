use crate::gpu::*;

pub struct Memory {
    memory: [u8; 0xFFFF],
    gpu: Gpu,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            memory: [0u8; 0xFFFF],
            gpu: Gpu::new(),
        }
    }

    pub fn read_byte(&self, address: u16) -> u8 {
        let address = address as usize;
        match address {
            VRAM_BEGIN..=VRAM_END => self.gpu.read_vram(address - VRAM_BEGIN),
            _ => self.memory[address],
        }
    }

    pub fn read_word(&self, address: u16) -> u16 {
        let lsb = self.memory[address as usize] as u16;
        let msb = self.memory[(address + 1) as usize] as u16;

        (msb << 8) | lsb
    }

    pub fn write_byte(&mut self, address: u16, byte: u8) {
        let address = address as usize;
        match address {
            VRAM_BEGIN..=VRAM_END => self.gpu.write_vram(address - VRAM_BEGIN, byte),
            _ => self.memory[address] = byte,
        }
    }
}
