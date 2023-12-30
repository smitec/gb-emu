pub const VRAM_BEGIN: usize = 0x8000;
pub const VRAM_END: usize = 0x9FFF;
const VRAM_SIZE: usize = VRAM_END - VRAM_BEGIN + 1;

#[derive(Copy, Clone)]
enum TilePixelValue {
    Zero,
    One,
    Two,
    Three,
}

type Tile = [[TilePixelValue; 8]; 8];

fn empty_tile() -> Tile {
    [[TilePixelValue::Zero; 8]; 8]
}

pub struct Gpu {
    vram: [u8; VRAM_SIZE],
    tile_set: [Tile; 384],
}

impl Gpu {
    pub fn new() -> Gpu {
        Gpu {
            vram: [0; VRAM_SIZE],
            tile_set: [empty_tile(); 384],
        }
    }

    pub fn read_vram(&self, address: usize) -> u8 {
        self.vram[address]
    }

    pub fn write_vram(&mut self, address: usize, byte: u8) {
        self.vram[address] = byte;

        // TODO: is this actually useful...
        // In fact, is any of this GPU code for memory management useful...
        if address >= 0x1800 {
            return;
        }

        let address_aligned = address & 0xFFFE;

        let first_byte = self.vram[address_aligned];
        let second_byte = self.vram[address_aligned + 1];

        let tile_index = address / 16;
        let row_index = (address % 16) / 2;

        for pixel in 0..8 {
            let mask = 1 << (7 - pixel);
            let lsb = first_byte & mask;
            let msb = second_byte & mask;

            let value = match (lsb != 0, msb != 0) {
                (true, true) => TilePixelValue::Three,
                (true, false) => TilePixelValue::Two,
                (false, true) => TilePixelValue::One,
                (false, false) => TilePixelValue::Zero,
            };

            self.tile_set[tile_index][row_index][pixel] = value;
        }
    }
}
