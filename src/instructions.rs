type WithCarry = bool;
type ResetAddress = u16;

pub enum Instruction {
    Add(TargetRegister),
    AddC(TargetRegister),
    Sub(TargetRegister),
    SubC(TargetRegister),
    Compare(TargetRegister),
    Set(TargetRegister, u8),
    Reset(TargetRegister, u8),
    Inc(TargetRegister),
    Dec(TargetRegister),
    Inc16(TargetRegister16),
    Dec16(TargetRegister16),
    ShiftLeft(TargetRegister, ShiftMode, WithCarry),
    ShiftRight(TargetRegister, ShiftMode, WithCarry),
    BinaryOp(TargetRegister, BinaryOp),
    Swap(TargetRegister),
    LoadByte(LoadByteTarget, LoadByteSource),
    Jump(JumpTest),
    Push(StackTarget),
    Pop(StackTarget),
    Call(JumpTest),
    RST(ResetAddress),
    Return(JumpTest),
    Nop,
    Stop,
    Halt,
}

#[derive(Clone, Copy)]
pub enum JumpTest {
    NotZero,
    Zero,
    NotCarry,
    Carry,
    Always,
}

#[derive(Clone, Copy)]
pub enum ShiftMode {
    Logical,
    Rolling,
    Arithmetic,
}

#[derive(Clone, Copy)]
pub enum BinaryOp {
    AND,
    OR,
    XOR,
}

#[derive(Clone, Copy)]
pub enum TargetRegister {
    A,
    B,
    C,
    D,
    E,
    H,
    L, // Note: Not F
    D8,
    HLI,
}

fn extract_target_register_r8(byte: u8) -> TargetRegister {
    let target = (byte & 0b00111000) >> 3;
    match target {
        0 => TargetRegister::B,
        1 => TargetRegister::C,
        2 => TargetRegister::D,
        3 => TargetRegister::E,
        4 => TargetRegister::H,
        5 => TargetRegister::L,
        6 => TargetRegister::HLI,
        7 => TargetRegister::A,
        _ => panic!("Impossible Destination Rgister"),
    }
}

#[derive(Clone, Copy)]
pub enum TargetRegister16 {
    BC,
    DE,
    HL,
    SP,
}

fn extract_target_register_r16(byte: u8) -> TargetRegister16 {
    let target = (byte & 0b00110000) >> 4;
    match target {
        0 => TargetRegister16::BC,
        1 => TargetRegister16::DE,
        2 => TargetRegister16::HL,
        3 => TargetRegister16::SP,
        _ => panic!("Impossible Destination Rgister"),
    }
}

#[derive(Clone, Copy)]
pub enum StackTarget {
    BC,
    DE,
    HL,
    AF,
}

#[derive(Clone, Copy)]
pub enum LoadByteTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    BC,
    DE,
    HL,
    HLADD,
    HLDEC,
    SP,
    A16,
    HLI, // Load from the address HL
    BCI, // Load from the address BC
    DEI, // Load from the address DE
}

fn extract_dest_register_r8(byte: u8) -> LoadByteTarget {
    let target = (byte & 0b00111000) >> 3;
    match target {
        0 => LoadByteTarget::B,
        1 => LoadByteTarget::C,
        2 => LoadByteTarget::D,
        3 => LoadByteTarget::E,
        4 => LoadByteTarget::H,
        5 => LoadByteTarget::L,
        6 => LoadByteTarget::HLI,
        7 => LoadByteTarget::A,
        _ => panic!("Impossible Destination Rgister"),
    }
}

fn extract_dest_r16(byte: u8) -> LoadByteTarget {
    let target = (byte & 0b00110000) >> 4;
    match target {
        0 => LoadByteTarget::BC,
        1 => LoadByteTarget::DE,
        2 => LoadByteTarget::HL,
        3 => LoadByteTarget::SP,
        _ => panic!("Impossible Destination Rgister"),
    }
}

fn extract_dest_r16_memory(byte: u8) -> LoadByteTarget {
    let target = (byte & 0b00110000) >> 4;
    match target {
        0 => LoadByteTarget::BCI,
        1 => LoadByteTarget::DEI,
        2 => LoadByteTarget::HLADD,
        3 => LoadByteTarget::HLDEC,
        _ => panic!("Impossible Destination Rgister"),
    }
}

#[derive(Clone, Copy)]
pub enum LoadByteSource {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    D8,
    D16,
    SP,
    HLI, // High Low Immediate
    HLADD,
    HLDEC,
    BCI, // BC Immediate
    DEI, // DE Immediate
}

fn extract_source_register_r8(byte: u8) -> LoadByteSource {
    let target = byte & 0b00000111;
    match target {
        0 => LoadByteSource::B,
        1 => LoadByteSource::C,
        2 => LoadByteSource::D,
        3 => LoadByteSource::E,
        4 => LoadByteSource::H,
        5 => LoadByteSource::L,
        6 => LoadByteSource::HLI,
        7 => LoadByteSource::A,
        _ => panic!("Impossible Destination Rgister"),
    }
}

fn extract_source_r16_memory(byte: u8) -> LoadByteSource {
    let target = (byte & 0b00110000) >> 4;
    match target {
        0 => LoadByteSource::BCI,
        1 => LoadByteSource::DEI,
        2 => LoadByteSource::HLADD,
        3 => LoadByteSource::HLDEC,
        _ => panic!("Impossible Destination Rgister"),
    }
}

impl Instruction {
    pub fn from_byte(byte: u8, prefixed: bool) -> Option<Instruction> {
        if prefixed {
            Self::from_byte_prefixed(byte)
        } else {
            Self::from_byte_not_prefixed(byte)
        }
    }

    fn from_byte_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            0x00 => todo!(),
            _ => todo!(),
        }
    }

    fn from_byte_not_prefixed(byte: u8) -> Option<Instruction> {
        match byte {
            0x00 => Some(Self::Nop),

            0x01 | 0x11 | 0x21 | 0x31 => {
                Some(Self::LoadByte(extract_dest_r16(byte), LoadByteSource::D16))
            }
            0x02 | 0x12 | 0x22 | 0x32 => Some(Self::LoadByte(
                extract_dest_r16_memory(byte),
                LoadByteSource::A,
            )),
            0x03 | 0x13 | 0x23 | 0x33 => Some(Self::Inc16(extract_target_register_r16(byte))),
            0x04 | 0x14 | 0x24 | 0x34 | 0x0C | 0x1C | 0x2C | 0x3C => {
                Some(Self::Inc(extract_target_register_r8(byte)))
            }
            0x05 | 0x15 | 0x25 | 0x35 | 0x0D | 0x1D | 0x2D | 0x3D => {
                Some(Self::Dec(extract_target_register_r8(byte)))
            }
            0x06 | 0x16 | 0x26 | 0x36 | 0x0E | 0x1E | 0x2E | 0x3E => Some(Self::LoadByte(
                extract_dest_register_r8(byte),
                LoadByteSource::D8,
            )),

            0x07 => Some(Self::ShiftLeft(
                TargetRegister::A,
                ShiftMode::Arithmetic,
                false,
            )),

            0x08 => Some(Self::LoadByte(LoadByteTarget::A16, LoadByteSource::SP)),

            0x09 | 0x19 | 0x29 | 0x39 => todo!(), // ADD HL, r16 (1,8)

            0x0A | 0x1A | 0x2A | 0x3A => Some(Self::LoadByte(
                LoadByteTarget::A,
                extract_source_r16_memory(byte),
            )),
            0x0B | 0x1B | 0x2B | 0x3B => Some(Self::Dec16(extract_target_register_r16(byte))),

            0x0F => Some(Self::ShiftRight(
                TargetRegister::A,
                ShiftMode::Arithmetic,
                false,
            )),

            0x10 => Some(Self::Stop),

            0x17 => Some(Self::ShiftLeft(
                TargetRegister::A,
                ShiftMode::Arithmetic,
                true,
            )),
            0x18 => todo!(), // JR r8 (2, 12)
            0x1F => Some(Self::ShiftRight(
                TargetRegister::A,
                ShiftMode::Arithmetic,
                true,
            )),
            0x20 => todo!(), // JR NZ, r8 (2, 12/8)
            0x27 => todo!(), // DAA (1 4)
            0x28 => todo!(), // JR Z r8 (2, 12)
            0x2F => todo!(), // CPL (1, 4)
            0x30 => todo!(), // JR NC, r8 (2, 12/8)
            0x37 => todo!(), // SCF (1 4)
            0x38 => todo!(), // JR C r8 (2, 12)
            0x3F => todo!(), // CCF (1, 4)

            0x40..=0x75 | 0x77..=0x7F => Some(Self::LoadByte(
                extract_dest_register_r8(byte),
                extract_source_register_r8(byte),
            )),

            0x76 => todo!(), // HALT
            0x80..=0x87 => Some(Self::Add(extract_target_register_r8(byte << 3))),
            0x88..=0x8F => Some(Self::AddC(extract_target_register_r8(byte << 3))),
            0x90..=0x97 => Some(Self::Sub(extract_target_register_r8(byte << 3))),
            0x98..=0x9F => Some(Self::SubC(extract_target_register_r8(byte << 3))),
            0xA0..=0xA7 => Some(Self::BinaryOp(
                extract_target_register_r8(byte << 3),
                BinaryOp::AND,
            )),
            0xA8..=0xAF => Some(Self::BinaryOp(
                extract_target_register_r8(byte << 3),
                BinaryOp::XOR,
            )),
            0xB0..=0xB7 => Some(Self::BinaryOp(
                extract_target_register_r8(byte << 3),
                BinaryOp::OR,
            )),
            0xB8..=0xBF => Some(Self::Compare(extract_target_register_r8(byte << 3))),

            0xC0 => Some(Self::Return(JumpTest::NotZero)),
            0xC1 => Some(Self::Pop(StackTarget::BC)),
            0xC2 => Some(Self::Jump(JumpTest::NotZero)),
            0xC3 => Some(Self::Jump(JumpTest::Always)),
            0xC4 => Some(Self::Call(JumpTest::NotZero)),
            0xC5 => Some(Self::Push(StackTarget::BC)),
            0xC6 => Some(Self::Add(TargetRegister::D8)),
            0xC7 => Some(Self::RST(0x00)),
            0xC8 => Some(Self::Return(JumpTest::Zero)),
            0xC9 => Some(Self::Return(JumpTest::Always)),
            0xCA => Some(Self::Jump(JumpTest::Zero)),
            0xCB => None, // Handled by the prefix Check
            0xCC => Some(Self::Call(JumpTest::Zero)),
            0xCD => Some(Self::Call(JumpTest::Always)),
            0xCE => Some(Self::AddC(TargetRegister::D8)),
            0xCF => Some(Self::RST(0x08)),
            0xD0 => Some(Self::Return(JumpTest::NotCarry)),
            0xD1 => Some(Self::Pop(StackTarget::DE)),
            0xD2 => Some(Self::Jump(JumpTest::NotCarry)),
            0xD3 => None,
            0xD4 => Some(Self::Call(JumpTest::NotCarry)),
            0xD5 => Some(Self::Push(StackTarget::DE)),
            0xD6 => Some(Self::Sub(TargetRegister::D8)),
            0xD7 => Some(Self::RST(0x10)),
            0xD8 => Some(Self::Return(JumpTest::Carry)),
            0xD9 => todo!(), // RETI
            0xDA => Some(Self::Jump(JumpTest::Carry)),
            0xDB => None,
            0xDC => Some(Self::Call(JumpTest::Carry)),
            0xDD => None,
            0xDE => Some(Self::SubC(TargetRegister::D8)),
            0xDF => Some(Self::RST(0x18)),
            0xE0 => todo!(), // LDH (a8),A
            0xE1 => Some(Self::Pop(StackTarget::HL)),
            0xE2 => todo!(), // LD (C), A
            0xE3 => None,
            0xE4 => None,
            0xE5 => Some(Self::Push(StackTarget::HL)),
            0xE6 => Some(Self::BinaryOp(TargetRegister::D8, BinaryOp::AND)),
            0xE7 => Some(Self::RST(0x20)),
            0xE8 => todo!(), // ADD SP, R8 (2, 16)
            0xE9 => todo!(), // JP (HL)
            0xEA => todo!(), // LD (a16),A
            0xEB => None,
            0xEC => None,
            0xED => None,
            0xEE => Some(Self::BinaryOp(TargetRegister::D8, BinaryOp::XOR)),
            0xEF => Some(Self::RST(0x28)),
            0xF0 => todo!(), // LD A,(a8)
            0xF1 => Some(Self::Pop(StackTarget::AF)),
            0xF2 => todo!(), // LD A,(C)
            0xF3 => todo!(), // DI
            0xF4 => None,
            0xF5 => Some(Self::Push(StackTarget::AF)),
            0xF6 => Some(Self::BinaryOp(TargetRegister::D8, BinaryOp::OR)),
            0xF7 => Some(Self::RST(0x30)),
            0xF8 => todo!(), // LD HL,SP+r8
            0xF9 => todo!(), // LD SP,HL
            0xFA => todo!(), // LD A,(a16)
            0xFB => todo!(), // EI
            0xFC => None,
            0xFD => None,
            0xFE => Some(Self::Compare(TargetRegister::D8)),
            0xFF => Some(Self::RST(0x38)),
        }
    }
}
