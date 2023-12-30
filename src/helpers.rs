// Set a <bit> in a <value> to 1 leaving all other bits unchanged.
pub fn set(value: u8, bit: u8) -> u8 {
    let mask = (0b1 << bit) as u8;
    value | mask
}

// Set a <bit> in a <value> to 0 leaving all other bits unchanged.
pub fn reset(value: u8, bit: u8) -> u8 {
    let mask = 0xFF - (0b1 << bit) as u8;
    value & mask
}

pub fn flip(value: u8, bit: u8) -> u8 {
    let mask = (0b1 << bit) as u8;
    value ^ mask
}

// Rotate left and return the new value and the carry.
//
// The carry is returned on it's own to account for the RLX and RLCX variants.
pub fn rot_left(value: u8) -> (u8, u8) {
    let value_new = (value as u16) << 1;
    let carry = ((value_new & 0x10) >> 8) as u8;
    (value_new as u8, carry)
}

// Rotate right and return the new value and the carry.
//
// The carry is returned on it's own to account for the RLX and RLCX variants.
pub fn rot_right(value: u8) -> (u8, u8) {
    let carry = value & 0b1;
    let value_new = value >> 1;
    (value_new, carry)
}
