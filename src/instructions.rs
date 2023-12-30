type WithCarry = bool;

pub enum Instruction {
    Add(TargetRegister),
    AddC(TargetRegister),
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
}

impl Instruction {
    pub fn from_byte(byte: u8, prefixed: bool) -> Option<Instruction> {
        if prefixed {
            Instruction::from_byte_prefixed(byte)
        } else {
            Instruction::from_byte_not_prefixed(byte)
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
            0x00 => Some(Instruction::Nop),
            0x01 => todo!(), // LD BC,d16 (3, 12)
            0x02 => todo!(), // LD (BC),A 1 8
            0x03 => todo!(), // INC BC
            0x04 => Some(Instruction::Inc(TargetRegister::B)),
            0x05 => Some(Instruction::Dec(TargetRegister::B)),
            0x06 => Some(Instruction::LoadByte(LoadByteTarget::B, LoadByteSource::D8)),
            0x07 => Some(Instruction::ShiftLeft(
                TargetRegister::A,
                ShiftMode::Arithmetic,
                false,
            )),
            0x08 => todo!(), // LD (a16), SP (3, 20)
            0x09 => todo!(), // ADD HL, BC (1,8)
            0x0A => todo!(), // LD A,(BC) (1, 8)
            0x0B => todo!(), // DEC BC (1, 8)
            0x0C => Some(Instruction::Inc(TargetRegister::C)),
            0x0D => Some(Instruction::Dec(TargetRegister::C)),
            0x0E => todo!(),
            0x0F => Some(Instruction::ShiftRight(
                TargetRegister::A,
                ShiftMode::Arithmetic,
                false,
            )),
            0x10 => Some(Instruction::Halt),
            0x11 => todo!(), // LD DE,d16 (3, 12)
            0x12 => todo!(), // LD (DE),A (1, 8)
            0x13 => todo!(), // INC DE (1 8)
            0x14 => Some(Instruction::Inc(TargetRegister::D)),
            0x15 => Some(Instruction::Dec(TargetRegister::D)),
            0x16 => Some(Instruction::LoadByte(LoadByteTarget::D, LoadByteSource::D8)),
            0x17 => Some(Instruction::ShiftLeft(
                TargetRegister::A,
                ShiftMode::Arithmetic,
                true,
            )),
            0x18 => todo!(), // JR r8 (2, 12)
            0x19 => todo!(), // ADD HL,DE (1, 8)
            0x1A => todo!(), // LD A,(DE) (1, 8)
            0x1B => todo!(), // DEC DE (1, 8)
            0x1C => Some(Instruction::Inc(TargetRegister::E)),
            0x1D => Some(Instruction::Dec(TargetRegister::E)),
            0x1E => Some(Instruction::LoadByte(LoadByteTarget::E, LoadByteSource::D8)),
            0x1F => Some(Instruction::ShiftRight(
                TargetRegister::A,
                ShiftMode::Arithmetic,
                true,
            )),
            0x20 => todo!(), // JR NZ, r8 (2, 12/8)
            0x21 => todo!(), // LD HL,d16 (3, 12)
            0x22 => todo!(), // LD (HL+),A (1, 8)
            0x23 => todo!(), // INC HL (1 8)
            0x24 => Some(Instruction::Inc(TargetRegister::H)),
            0x25 => Some(Instruction::Dec(TargetRegister::H)),
            0x26 => Some(Instruction::LoadByte(LoadByteTarget::H, LoadByteSource::D8)),
            0x27 => todo!(), // DAA (1 4)
            0x28 => todo!(), // JR Z r8 (2, 12)
            0x29 => todo!(), // ADD HL,HL (1, 8)
            0x2A => todo!(), // LD A,(HL+) (1, 8)
            0x2B => todo!(), // DEC HL (1, 8)
            0x2C => Some(Instruction::Inc(TargetRegister::L)),
            0x2D => Some(Instruction::Dec(TargetRegister::L)),
            0x2E => Some(Instruction::LoadByte(LoadByteTarget::L, LoadByteSource::D8)),
            0x2F => todo!(), // CPL (1, 4)
            0x30 => todo!(), // JR NC, r8 (2, 12/8)
            0x31 => todo!(), // LD SP,d16 (3, 12)
            0x32 => todo!(), // LD (HL-),A (1, 8)
            0x33 => todo!(), // INC SP (1 8)
            0x34 => todo!(), // INC (HL) (1, 12)
            0x35 => todo!(), // DEC (HL) (1, 12)
            0x36 => todo!(), // LD (HL),d8 (2, 12)
            0x37 => todo!(), // SCF (1 4)
            0x38 => todo!(), // JR C r8 (2, 12)
            0x39 => todo!(), // ADD HL,SP (1, 8)
            0x3A => todo!(), // LD A,(HL-) (1, 8)
            0x3B => todo!(), // DEC SP (1, 8)
            0x3C => Some(Instruction::Inc(TargetRegister::A)),
            0x3D => Some(Instruction::Dec(TargetRegister::A)),
            0x3E => Some(Instruction::LoadByte(LoadByteTarget::A, LoadByteSource::D8)),
            0x3F => todo!(), // CCF (1, 4)
            0x40 => Some(Instruction::LoadByte(LoadByteTarget::B, LoadByteSource::B)),
            0x41 => Some(Instruction::LoadByte(LoadByteTarget::B, LoadByteSource::C)),
            0x42 => Some(Instruction::LoadByte(LoadByteTarget::B, LoadByteSource::D)),
            0x43 => Some(Instruction::LoadByte(LoadByteTarget::B, LoadByteSource::E)),
            0x44 => Some(Instruction::LoadByte(LoadByteTarget::B, LoadByteSource::H)),
            0x45 => Some(Instruction::LoadByte(LoadByteTarget::B, LoadByteSource::L)),
            0x46 => todo!(), // LD B,(HL) (1, 8)
            0x47 => Some(Instruction::LoadByte(LoadByteTarget::B, LoadByteSource::A)),
            0x48 => Some(Instruction::LoadByte(LoadByteTarget::C, LoadByteSource::A)),
            0x49 => Some(Instruction::LoadByte(LoadByteTarget::C, LoadByteSource::C)),
            0x4A => Some(Instruction::LoadByte(LoadByteTarget::C, LoadByteSource::D)),
            0x4B => Some(Instruction::LoadByte(LoadByteTarget::C, LoadByteSource::E)),
            0x4C => Some(Instruction::LoadByte(LoadByteTarget::C, LoadByteSource::H)),
            0x4D => Some(Instruction::LoadByte(LoadByteTarget::C, LoadByteSource::L)),
            0x4E => todo!(), // LD C,(HL) (1, 8)
            0x4F => Some(Instruction::LoadByte(LoadByteTarget::C, LoadByteSource::A)),
            0x50 => Some(Instruction::LoadByte(LoadByteTarget::D, LoadByteSource::B)),
            0x51 => Some(Instruction::LoadByte(LoadByteTarget::D, LoadByteSource::C)),
            0x52 => Some(Instruction::LoadByte(LoadByteTarget::D, LoadByteSource::D)),
            0x53 => Some(Instruction::LoadByte(LoadByteTarget::D, LoadByteSource::E)),
            0x54 => Some(Instruction::LoadByte(LoadByteTarget::D, LoadByteSource::H)),
            0x55 => Some(Instruction::LoadByte(LoadByteTarget::D, LoadByteSource::L)),
            0x56 => todo!(), // LD D,(HL) (1, 8)
            0x57 => Some(Instruction::LoadByte(LoadByteTarget::D, LoadByteSource::A)),
            0x58 => Some(Instruction::LoadByte(LoadByteTarget::E, LoadByteSource::A)),
            0x59 => Some(Instruction::LoadByte(LoadByteTarget::E, LoadByteSource::C)),
            0x5A => Some(Instruction::LoadByte(LoadByteTarget::E, LoadByteSource::D)),
            0x5B => Some(Instruction::LoadByte(LoadByteTarget::E, LoadByteSource::E)),
            0x5C => Some(Instruction::LoadByte(LoadByteTarget::E, LoadByteSource::H)),
            0x5D => Some(Instruction::LoadByte(LoadByteTarget::E, LoadByteSource::L)),
            0x5E => todo!(), // LD E,(HL) (1, 8)
            0x5F => Some(Instruction::LoadByte(LoadByteTarget::E, LoadByteSource::A)),
            _ => todo!(),
        }
    }
}
