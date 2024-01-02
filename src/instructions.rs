type WithCarry = bool;

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
    ShiftLeft(TargetRegister, ShiftMode, WithCarry),
    ShiftRight(TargetRegister, ShiftMode, WithCarry),
    BinaryOp(TargetRegister, BinaryOp),
    Swap(TargetRegister),
    LoadByte(LoadByteTarget, LoadByteSource),
    Jump(JumpTest),
    Push(StackTarget),
    Pop(StackTarget),
    Call(JumpTest),
    Return(JumpTest),
    Nop,
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
    HLI,
    BCI,
    DEI,
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
    HLI,
    BCI,
    DEI,
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
            0x01 => todo!(), // LD BC,d16 (3, 12)
            0x02 => Some(Self::LoadByte(LoadByteTarget::BCI, LoadByteSource::A)),
            0x03 => todo!(), // INC BC
            0x04 => Some(Self::Inc(TargetRegister::B)),
            0x05 => Some(Self::Dec(TargetRegister::B)),
            0x06 => Some(Self::LoadByte(LoadByteTarget::B, LoadByteSource::D8)),
            0x07 => Some(Self::ShiftLeft(
                TargetRegister::A,
                ShiftMode::Arithmetic,
                false,
            )),
            0x08 => todo!(), // LD (a16), SP (3, 20)
            0x09 => todo!(), // ADD HL, BC (1,8)
            0x0A => Some(Self::LoadByte(LoadByteTarget::A, LoadByteSource::BCI)),
            0x0B => todo!(), // DEC BC (1, 8)
            0x0C => Some(Self::Inc(TargetRegister::C)),
            0x0D => Some(Self::Dec(TargetRegister::C)),
            0x0E => todo!(),
            0x0F => Some(Self::ShiftRight(
                TargetRegister::A,
                ShiftMode::Arithmetic,
                false,
            )),
            0x10 => Some(Self::Halt),
            0x11 => todo!(), // LD DE,d16 (3, 12)
            0x12 => Some(Self::LoadByte(LoadByteTarget::DEI, LoadByteSource::A)),
            0x13 => todo!(), // INC DE (1 8)
            0x14 => Some(Self::Inc(TargetRegister::D)),
            0x15 => Some(Self::Dec(TargetRegister::D)),
            0x16 => Some(Self::LoadByte(LoadByteTarget::D, LoadByteSource::D8)),
            0x17 => Some(Self::ShiftLeft(
                TargetRegister::A,
                ShiftMode::Arithmetic,
                true,
            )),
            0x18 => todo!(), // JR r8 (2, 12)
            0x19 => todo!(), // ADD HL,DE (1, 8)
            0x1A => Some(Self::LoadByte(LoadByteTarget::A, LoadByteSource::DEI)),
            0x1B => todo!(), // DEC DE (1, 8)
            0x1C => Some(Self::Inc(TargetRegister::E)),
            0x1D => Some(Self::Dec(TargetRegister::E)),
            0x1E => Some(Self::LoadByte(LoadByteTarget::E, LoadByteSource::D8)),
            0x1F => Some(Self::ShiftRight(
                TargetRegister::A,
                ShiftMode::Arithmetic,
                true,
            )),
            0x20 => todo!(), // JR NZ, r8 (2, 12/8)
            0x21 => todo!(), // LD HL,d16 (3, 12)
            0x22 => todo!(), // LD (HL+),A (1, 8)
            0x23 => todo!(), // INC HL (1 8)
            0x24 => Some(Self::Inc(TargetRegister::H)),
            0x25 => Some(Self::Dec(TargetRegister::H)),
            0x26 => Some(Self::LoadByte(LoadByteTarget::H, LoadByteSource::D8)),
            0x27 => todo!(), // DAA (1 4)
            0x28 => todo!(), // JR Z r8 (2, 12)
            0x29 => todo!(), // ADD HL,HL (1, 8)
            0x2A => todo!(), // LD A,(HL+) (1, 8)
            0x2B => todo!(), // DEC HL (1, 8)
            0x2C => Some(Self::Inc(TargetRegister::L)),
            0x2D => Some(Self::Dec(TargetRegister::L)),
            0x2E => Some(Self::LoadByte(LoadByteTarget::L, LoadByteSource::D8)),
            0x2F => todo!(), // CPL (1, 4)
            0x30 => todo!(), // JR NC, r8 (2, 12/8)
            0x31 => todo!(), // LD SP,d16 (3, 12)
            0x32 => todo!(), // LD (HL-),A (1, 8)
            0x33 => todo!(), // INC SP (1 8)
            0x34 => todo!(), // INC (HL) (1, 12)
            0x35 => todo!(), // DEC (HL) (1, 12)
            0x36 => Some(Self::LoadByte(LoadByteTarget::HLI, LoadByteSource::D8)),
            0x37 => todo!(), // SCF (1 4)
            0x38 => todo!(), // JR C r8 (2, 12)
            0x39 => todo!(), // ADD HL,SP (1, 8)
            0x3A => todo!(), // LD A,(HL-) (1, 8)
            0x3B => todo!(), // DEC SP (1, 8)
            0x3C => Some(Self::Inc(TargetRegister::A)),
            0x3D => Some(Self::Dec(TargetRegister::A)),
            0x3E => Some(Self::LoadByte(LoadByteTarget::A, LoadByteSource::D8)),
            0x3F => todo!(), // CCF (1, 4)
            0x40 => Some(Self::LoadByte(LoadByteTarget::B, LoadByteSource::B)),
            0x41 => Some(Self::LoadByte(LoadByteTarget::B, LoadByteSource::C)),
            0x42 => Some(Self::LoadByte(LoadByteTarget::B, LoadByteSource::D)),
            0x43 => Some(Self::LoadByte(LoadByteTarget::B, LoadByteSource::E)),
            0x44 => Some(Self::LoadByte(LoadByteTarget::B, LoadByteSource::H)),
            0x45 => Some(Self::LoadByte(LoadByteTarget::B, LoadByteSource::L)),
            0x46 => Some(Self::LoadByte(LoadByteTarget::B, LoadByteSource::HLI)),
            0x47 => Some(Self::LoadByte(LoadByteTarget::B, LoadByteSource::A)),
            0x48 => Some(Self::LoadByte(LoadByteTarget::C, LoadByteSource::A)),
            0x49 => Some(Self::LoadByte(LoadByteTarget::C, LoadByteSource::C)),
            0x4A => Some(Self::LoadByte(LoadByteTarget::C, LoadByteSource::D)),
            0x4B => Some(Self::LoadByte(LoadByteTarget::C, LoadByteSource::E)),
            0x4C => Some(Self::LoadByte(LoadByteTarget::C, LoadByteSource::H)),
            0x4D => Some(Self::LoadByte(LoadByteTarget::C, LoadByteSource::L)),
            0x4E => Some(Self::LoadByte(LoadByteTarget::C, LoadByteSource::HLI)),
            0x4F => Some(Self::LoadByte(LoadByteTarget::C, LoadByteSource::A)),
            0x50 => Some(Self::LoadByte(LoadByteTarget::D, LoadByteSource::B)),
            0x51 => Some(Self::LoadByte(LoadByteTarget::D, LoadByteSource::C)),
            0x52 => Some(Self::LoadByte(LoadByteTarget::D, LoadByteSource::D)),
            0x53 => Some(Self::LoadByte(LoadByteTarget::D, LoadByteSource::E)),
            0x54 => Some(Self::LoadByte(LoadByteTarget::D, LoadByteSource::H)),
            0x55 => Some(Self::LoadByte(LoadByteTarget::D, LoadByteSource::L)),
            0x56 => Some(Self::LoadByte(LoadByteTarget::D, LoadByteSource::HLI)),
            0x57 => Some(Self::LoadByte(LoadByteTarget::D, LoadByteSource::A)),
            0x58 => Some(Self::LoadByte(LoadByteTarget::E, LoadByteSource::A)),
            0x59 => Some(Self::LoadByte(LoadByteTarget::E, LoadByteSource::C)),
            0x5A => Some(Self::LoadByte(LoadByteTarget::E, LoadByteSource::D)),
            0x5B => Some(Self::LoadByte(LoadByteTarget::E, LoadByteSource::E)),
            0x5C => Some(Self::LoadByte(LoadByteTarget::E, LoadByteSource::H)),
            0x5D => Some(Self::LoadByte(LoadByteTarget::E, LoadByteSource::L)),
            0x5E => Some(Self::LoadByte(LoadByteTarget::E, LoadByteSource::HLI)),
            0x5F => Some(Self::LoadByte(LoadByteTarget::E, LoadByteSource::A)),
            0x60 => Some(Self::LoadByte(LoadByteTarget::H, LoadByteSource::B)),
            0x61 => Some(Self::LoadByte(LoadByteTarget::H, LoadByteSource::C)),
            0x62 => Some(Self::LoadByte(LoadByteTarget::H, LoadByteSource::D)),
            0x63 => Some(Self::LoadByte(LoadByteTarget::H, LoadByteSource::E)),
            0x64 => Some(Self::LoadByte(LoadByteTarget::H, LoadByteSource::H)),
            0x65 => Some(Self::LoadByte(LoadByteTarget::H, LoadByteSource::L)),
            0x66 => Some(Self::LoadByte(LoadByteTarget::H, LoadByteSource::HLI)),
            0x67 => Some(Self::LoadByte(LoadByteTarget::H, LoadByteSource::A)),
            0x68 => Some(Self::LoadByte(LoadByteTarget::L, LoadByteSource::B)),
            0x69 => Some(Self::LoadByte(LoadByteTarget::L, LoadByteSource::C)),
            0x6A => Some(Self::LoadByte(LoadByteTarget::L, LoadByteSource::D)),
            0x6B => Some(Self::LoadByte(LoadByteTarget::L, LoadByteSource::E)),
            0x6C => Some(Self::LoadByte(LoadByteTarget::L, LoadByteSource::H)),
            0x6D => Some(Self::LoadByte(LoadByteTarget::L, LoadByteSource::L)),
            0x6E => Some(Self::LoadByte(LoadByteTarget::L, LoadByteSource::HLI)),
            0x6F => Some(Self::LoadByte(LoadByteTarget::L, LoadByteSource::A)),
            0x70 => Some(Self::LoadByte(LoadByteTarget::HLI, LoadByteSource::B)),
            0x71 => Some(Self::LoadByte(LoadByteTarget::HLI, LoadByteSource::C)),
            0x72 => Some(Self::LoadByte(LoadByteTarget::HLI, LoadByteSource::D)),
            0x73 => Some(Self::LoadByte(LoadByteTarget::HLI, LoadByteSource::E)),
            0x74 => Some(Self::LoadByte(LoadByteTarget::HLI, LoadByteSource::H)),
            0x75 => Some(Self::LoadByte(LoadByteTarget::HLI, LoadByteSource::L)),
            0x76 => todo!(), // HALT
            0x77 => Some(Self::LoadByte(LoadByteTarget::HLI, LoadByteSource::A)),
            0x78 => Some(Self::LoadByte(LoadByteTarget::A, LoadByteSource::B)),
            0x79 => Some(Self::LoadByte(LoadByteTarget::A, LoadByteSource::C)),
            0x7A => Some(Self::LoadByte(LoadByteTarget::A, LoadByteSource::D)),
            0x7B => Some(Self::LoadByte(LoadByteTarget::A, LoadByteSource::E)),
            0x7C => Some(Self::LoadByte(LoadByteTarget::A, LoadByteSource::H)),
            0x7D => Some(Self::LoadByte(LoadByteTarget::A, LoadByteSource::L)),
            0x7E => Some(Self::LoadByte(LoadByteTarget::A, LoadByteSource::HLI)),
            0x7F => Some(Self::LoadByte(LoadByteTarget::A, LoadByteSource::A)),
            0x80 => Some(Self::Add(TargetRegister::B)),
            0x81 => Some(Self::Add(TargetRegister::C)),
            0x82 => Some(Self::Add(TargetRegister::D)),
            0x83 => Some(Self::Add(TargetRegister::E)),
            0x84 => Some(Self::Add(TargetRegister::H)),
            0x85 => Some(Self::Add(TargetRegister::L)),
            0x86 => Some(Self::Add(TargetRegister::HLI)),
            0x87 => Some(Self::Add(TargetRegister::A)),
            0x88 => Some(Self::AddC(TargetRegister::B)),
            0x89 => Some(Self::AddC(TargetRegister::C)),
            0x8A => Some(Self::AddC(TargetRegister::D)),
            0x8B => Some(Self::AddC(TargetRegister::E)),
            0x8C => Some(Self::AddC(TargetRegister::H)),
            0x8D => Some(Self::AddC(TargetRegister::L)),
            0x8E => Some(Self::AddC(TargetRegister::HLI)),
            0x8F => Some(Self::AddC(TargetRegister::A)),
            0x90 => Some(Self::Sub(TargetRegister::B)),
            0x91 => Some(Self::Sub(TargetRegister::C)),
            0x92 => Some(Self::Sub(TargetRegister::D)),
            0x93 => Some(Self::Sub(TargetRegister::E)),
            0x94 => Some(Self::Sub(TargetRegister::H)),
            0x95 => Some(Self::Sub(TargetRegister::L)),
            0x96 => Some(Self::Sub(TargetRegister::HLI)),
            0x97 => Some(Self::Sub(TargetRegister::A)),
            0x98 => Some(Self::SubC(TargetRegister::B)),
            0x99 => Some(Self::SubC(TargetRegister::C)),
            0x9A => Some(Self::SubC(TargetRegister::D)),
            0x9B => Some(Self::SubC(TargetRegister::E)),
            0x9C => Some(Self::SubC(TargetRegister::H)),
            0x9D => Some(Self::SubC(TargetRegister::L)),
            0x9E => Some(Self::SubC(TargetRegister::HLI)),
            0x9F => Some(Self::SubC(TargetRegister::A)),
            0xA0 => Some(Self::BinaryOp(TargetRegister::B, BinaryOp::AND)),
            0xA1 => Some(Self::BinaryOp(TargetRegister::C, BinaryOp::AND)),
            0xA2 => Some(Self::BinaryOp(TargetRegister::D, BinaryOp::AND)),
            0xA3 => Some(Self::BinaryOp(TargetRegister::E, BinaryOp::AND)),
            0xA4 => Some(Self::BinaryOp(TargetRegister::H, BinaryOp::AND)),
            0xA5 => Some(Self::BinaryOp(TargetRegister::L, BinaryOp::AND)),
            0xA6 => Some(Self::BinaryOp(TargetRegister::HLI, BinaryOp::AND)),
            0xA7 => Some(Self::BinaryOp(TargetRegister::A, BinaryOp::AND)),
            0xA8 => Some(Self::BinaryOp(TargetRegister::B, BinaryOp::XOR)),
            0xA9 => Some(Self::BinaryOp(TargetRegister::C, BinaryOp::XOR)),
            0xAA => Some(Self::BinaryOp(TargetRegister::D, BinaryOp::XOR)),
            0xAB => Some(Self::BinaryOp(TargetRegister::E, BinaryOp::XOR)),
            0xAC => Some(Self::BinaryOp(TargetRegister::H, BinaryOp::XOR)),
            0xAD => Some(Self::BinaryOp(TargetRegister::L, BinaryOp::XOR)),
            0xAE => Some(Self::BinaryOp(TargetRegister::HLI, BinaryOp::XOR)),
            0xAF => Some(Self::BinaryOp(TargetRegister::A, BinaryOp::XOR)),
            0xB0 => Some(Self::BinaryOp(TargetRegister::B, BinaryOp::OR)),
            0xB1 => Some(Self::BinaryOp(TargetRegister::C, BinaryOp::OR)),
            0xB2 => Some(Self::BinaryOp(TargetRegister::D, BinaryOp::OR)),
            0xB3 => Some(Self::BinaryOp(TargetRegister::E, BinaryOp::OR)),
            0xB4 => Some(Self::BinaryOp(TargetRegister::H, BinaryOp::OR)),
            0xB5 => Some(Self::BinaryOp(TargetRegister::L, BinaryOp::OR)),
            0xB6 => Some(Self::BinaryOp(TargetRegister::HLI, BinaryOp::OR)),
            0xB7 => Some(Self::BinaryOp(TargetRegister::A, BinaryOp::OR)),
            0xB8 => Some(Self::Compare(TargetRegister::B)),
            0xB9 => Some(Self::Compare(TargetRegister::C)),
            0xBA => Some(Self::Compare(TargetRegister::D)),
            0xBB => Some(Self::Compare(TargetRegister::E)),
            0xBC => Some(Self::Compare(TargetRegister::H)),
            0xBD => Some(Self::Compare(TargetRegister::L)),
            0xBE => Some(Self::Compare(TargetRegister::HLI)),
            0xBF => Some(Self::Compare(TargetRegister::A)),
            0xC0 => Some(Self::Return(JumpTest::NotZero)),
            0xC1 => Some(Self::Pop(StackTarget::BC)),
            0xC2 => Some(Self::Jump(JumpTest::NotZero)),
            0xC3 => Some(Self::Jump(JumpTest::Always)),
            0xC4 => Some(Self::Call(JumpTest::NotZero)),
            0xC5 => Some(Self::Push(StackTarget::BC)),
            0xC6 => Some(Self::Add(TargetRegister::D8)),
            0xC7 => todo!(), // RST 00H
            0xC8 => Some(Self::Return(JumpTest::Zero)),
            0xC9 => Some(Self::Return(JumpTest::Always)),
            0xCA => Some(Self::Jump(JumpTest::Zero)),
            0xCB => None, // Handled by the prefix Check
            0xCC => Some(Self::Call(JumpTest::Zero)),
            0xCD => Some(Self::Call(JumpTest::Always)),
            0xCE => Some(Self::AddC(TargetRegister::D8)),
            0xCF => todo!(), // RST 08H
            0xD0 => Some(Self::Return(JumpTest::NotCarry)),
            0xD1 => Some(Self::Pop(StackTarget::DE)),
            0xD2 => Some(Self::Jump(JumpTest::NotCarry)),
            0xD3 => panic!("No Instruction 0xD3"),
            0xD4 => Some(Self::Call(JumpTest::NotCarry)),
            0xD5 => Some(Self::Push(StackTarget::DE)),
            0xD6 => Some(Self::Sub(TargetRegister::D8)),
            0xD7 => todo!(), // RST 10H
            0xD8 => Some(Self::Return(JumpTest::Carry)),
            0xD9 => todo!(), // RETI
            0xDA => Some(Self::Jump(JumpTest::Carry)),
            0xDB => panic!("No Instruction 0xDB"),
            0xDC => Some(Self::Call(JumpTest::Carry)),
            0xDD => panic!("No Instruction 0xDD"),
            0xDE => Some(Self::SubC(TargetRegister::D8)),
            0xDF => todo!(), // RST 18H
            0xE0 => todo!(), // LDH (a8),A
            0xE1 => Some(Self::Pop(StackTarget::HL)),
            0xE2 => todo!(), // LD (C), A
            0xE3 => panic!("No Instruction 0xE3"),
            0xE4 => panic!("No Instruction 0xE4"),
            0xE5 => Some(Self::Push(StackTarget::HL)),
            0xE6 => Some(Self::BinaryOp(TargetRegister::D8, BinaryOp::AND)),
            0xE7 => todo!(), // RST 20H
            0xE8 => todo!(), // ADD SP, R8 (2, 16)
            0xE9 => todo!(), // JP (HL)
            0xEA => todo!(), // LD (a16),A
            0xEB => panic!("No Instruction 0xEB"),
            0xEC => panic!("No Instruction 0xEC"),
            0xED => panic!("No Instruction 0xED"),
            0xEE => Some(Self::BinaryOp(TargetRegister::D8, BinaryOp::XOR)),
            0xEF => todo!(), // RST 28H
            0xF0 => todo!(), // LD A,(a8)
            0xF1 => Some(Self::Pop(StackTarget::AF)),
            0xF2 => todo!(), // LD A,(C)
            0xF3 => todo!(), // DI
            0xF4 => panic!("No Instruction 0xF4"),
            0xF5 => Some(Self::Push(StackTarget::AF)),
            0xF6 => Some(Self::BinaryOp(TargetRegister::D8, BinaryOp::OR)),
            0xF7 => todo!(), // RST 30H
            0xF8 => todo!(), // LD HL,SP+r8
            0xF9 => todo!(), // LD SP,HL
            0xFA => todo!(), // LD A,(a16)
            0xFB => todo!(), // EI
            0xFC => panic!("No Instruction 0xFC"),
            0xFD => panic!("No Instruction 0xFD"),
            0xFE => Some(Self::Compare(TargetRegister::D8)),
            0xFF => todo!(), // RST 38H
        }
    }
}
