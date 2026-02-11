use crate::helpers::*;
use crate::instructions::*;
use crate::memory::*;
use crate::registers::*;

pub struct Cpu {
    registers: Registers,
    program_counter: u16,
    stack_pointer: u16,
    memory: Memory,
    halted: bool,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            registers: Registers::new(),
            program_counter: 0,
            stack_pointer: 0,
            memory: Memory::new(),
            halted: false,
        }
    }

    pub fn step(&mut self) {
        let mut instruction = self.memory.read_byte(self.program_counter);
        let prefixed = instruction == 0xCB;
        if prefixed {
            instruction = self.memory.read_byte(self.program_counter + 1);
            self.program_counter += 1;
        }
        let next_program_counter =
            if let Some(instruction) = Instruction::from_byte(instruction, prefixed) {
                self.execute(instruction)
            } else {
                panic!("Invalid instruction found for 0x{:x}", instruction);
            };

        self.program_counter = next_program_counter;
    }

    fn execute(&mut self, intstruction: Instruction) -> u16 {
        if self.halted {
            return 0;
        }
        match intstruction {
            Instruction::Add(target) => {
                let value: u8 = self.register_value(target);
                let new_v = self.add(value);
                self.registers.a = new_v;

                self.program_counter.wrapping_add(1)
            }
            Instruction::AddC(target) => {
                let value: u8 = self.register_value(target);
                let c = if self.registers.f.carry { 1 } else { 0 } as u8;

                self.add(c);
                let mid_carry = self.registers.f.carry;

                let new_v = self.add(value);

                self.registers.f.carry |= mid_carry;
                self.registers.a = new_v;

                self.program_counter.wrapping_add(1)
            }
            Instruction::Set(target, bit) => {
                let register: &mut u8 = self.register_reference(target);
                let value = set(*register, bit);
                *register = value;

                self.program_counter.wrapping_add(2)
            }
            Instruction::Reset(target, bit) => {
                let register: &mut u8 = self.register_reference(target);
                let value = reset(*register, bit);
                *register = value;

                self.program_counter.wrapping_add(2)
            }
            Instruction::Inc(target) => {
                let original: u8 = self.register_value(target);
                let (value, _) = original.overflowing_add(1);
                self.registers.f.zero = value == 0;
                self.registers.f.subtract = false;
                // Do not set carry
                self.registers.f.half_carry = (original & 0xF) + (value & 0xF) > 0xF;

                let register: &mut u8 = self.register_reference(target);
                *register = value;

                self.program_counter.wrapping_add(1)
            }
            Instruction::Dec(target) => {
                let original: u8 = self.register_value(target);
                let (value, _) = original.overflowing_sub(1);
                self.registers.f.zero = value == 0;
                self.registers.f.subtract = true;
                // Do not set carry
                self.registers.f.half_carry = (original & 0xF) + (value & 0xF) > 0xF;

                let register: &mut u8 = self.register_reference(target);
                *register = value;

                self.program_counter.wrapping_add(1)
            }
            Instruction::ShiftLeft(target, mode, with_carry) => {
                let original: u8 = self.register_value(target);
                let original_carry = if self.registers.f.carry { 1 } else { 0 };
                let (mut value, carry) = rot_left(original);

                match mode {
                    ShiftMode::Rolling => {
                        if with_carry {
                            value |= original_carry;
                        } else {
                            value |= carry;
                        }
                    }
                    _ => {}
                };

                self.registers.f.zero = value == 0;
                self.registers.f.subtract = false;
                self.registers.f.carry = carry == 1;
                self.registers.f.half_carry = false;

                let register: &mut u8 = self.register_reference(target);
                *register = value;

                self.program_counter.wrapping_add(2)
            }
            Instruction::ShiftRight(target, mode, with_carry) => {
                let original: u8 = self.register_value(target);
                let original_carry = if self.registers.f.carry { 1 } else { 0 };
                let (mut value, carry) = rot_right(original);

                match mode {
                    ShiftMode::Rolling => {
                        if with_carry {
                            value |= original_carry << 7;
                        } else {
                            value |= carry << 7;
                        }
                    }
                    ShiftMode::Arithmetic => {
                        value |= original & 0b10000000;
                    }
                    _ => {}
                };

                self.registers.f.zero = value == 0;
                self.registers.f.subtract = false;
                self.registers.f.carry = carry == 1;
                self.registers.f.half_carry = false;

                let register: &mut u8 = self.register_reference(target);
                *register = value;

                self.program_counter.wrapping_add(2)
            }
            Instruction::BinaryOp(target, op) => {
                let value: u8 = self.register_value(target);
                let a_reg: u8 = self.register_value(TargetRegister::A);
                let result: u8 = match op {
                    BinaryOp::AND => value & a_reg,
                    BinaryOp::OR => value | a_reg,
                    BinaryOp::XOR => value ^ a_reg,
                };

                self.registers.f.zero = result == 0;
                self.registers.f.subtract = false;
                self.registers.f.carry = false;
                self.registers.f.half_carry = match op {
                    BinaryOp::AND => true,
                    BinaryOp::OR => false,
                    BinaryOp::XOR => false,
                };

                self.program_counter.wrapping_add(1)
            }
            Instruction::Swap(target) => {
                let value: u8 = self.register_value(target);
                let new_value = ((value & 0x0F) << 4) | ((value & 0xF0) >> 4);

                self.registers.f.zero = new_value == 0;
                self.registers.f.subtract = false;
                self.registers.f.carry = false;
                self.registers.f.half_carry = false;

                let register: &mut u8 = self.register_reference(target);
                *register = new_value;

                self.program_counter.wrapping_add(2)
            }
            Instruction::Jump(test) => {
                let condition: bool = match test {
                    JumpTest::NotZero => !self.registers.f.zero,
                    JumpTest::Zero => self.registers.f.zero,
                    JumpTest::NotCarry => !self.registers.f.carry,
                    JumpTest::Carry => self.registers.f.carry,
                    JumpTest::Always => true,
                };

                self.jump(condition)
            }
            Instruction::LoadByte(target, source) => {
                let value: u16 = match source {
                    LoadByteSource::A => self.registers.a as u16,
                    LoadByteSource::B => self.registers.b as u16,
                    LoadByteSource::C => self.registers.c as u16,
                    LoadByteSource::D => self.registers.d as u16,
                    LoadByteSource::E => self.registers.e as u16,
                    LoadByteSource::H => self.registers.h as u16,
                    LoadByteSource::L => self.registers.l as u16,
                    LoadByteSource::D8 => self.memory.read_byte(self.program_counter + 1) as u16,
                    LoadByteSource::D16 => {
                        ((self.memory.read_byte(self.program_counter + 1) as u16) << 8)
                            + self.memory.read_byte(self.program_counter + 2) as u16
                    }
                    LoadByteSource::SP => self.stack_pointer,
                    LoadByteSource::HLI => self.memory.read_byte(self.registers.get_hl()) as u16,
                    LoadByteSource::BCI => self.memory.read_byte(self.registers.get_bc()) as u16,
                    LoadByteSource::DEI => self.memory.read_byte(self.registers.get_de()) as u16,
                    LoadByteSource::HLADD => {
                        let val = self.memory.read_byte(self.registers.get_hl()) as u16;
                        let hl_new = self.registers.get_hl().wrapping_add(1);
                        self.registers.h = ((hl_new & 0xF0) >> 8) as u8;
                        self.registers.l = (hl_new & 0x0F) as u8;
                        val
                    }
                    LoadByteSource::HLDEC => {
                        let val = self.memory.read_byte(self.registers.get_hl()) as u16;
                        let hl_new = self.registers.get_hl().wrapping_sub(1);
                        self.registers.h = ((hl_new & 0xF0) >> 8) as u8;
                        self.registers.l = (hl_new & 0x0F) as u8;
                        val
                    }
                    LoadByteSource::R8High => self
                        .memory
                        .read_byte(0xF0 & (self.memory.read_byte(self.program_counter + 1) as u16))
                        as u16,
                    LoadByteSource::CHigh => {
                        self.memory.read_byte(0xF0 & (self.registers.c as u16)) as u16
                    }
                };

                match target {
                    LoadByteTarget::A => self.registers.a = (value & 0x0F) as u8,
                    LoadByteTarget::B => self.registers.b = (value & 0x0F) as u8,
                    LoadByteTarget::C => self.registers.c = (value & 0x0F) as u8,
                    LoadByteTarget::D => self.registers.d = (value & 0x0F) as u8,
                    LoadByteTarget::E => self.registers.e = (value & 0x0F) as u8,
                    LoadByteTarget::H => self.registers.h = (value & 0x0F) as u8,
                    LoadByteTarget::L => self.registers.l = (value & 0x0F) as u8,
                    LoadByteTarget::BC => {
                        self.registers.b = ((value & 0xF0) >> 8) as u8;
                        self.registers.c = (value & 0x0F) as u8;
                    }
                    LoadByteTarget::DE => {
                        self.registers.d = ((value & 0xF0) >> 8) as u8;
                        self.registers.e = (value & 0x0F) as u8;
                    }
                    LoadByteTarget::HL => {
                        self.registers.h = ((value & 0xF0) >> 8) as u8;
                        self.registers.l = (value & 0x0F) as u8;
                    }
                    LoadByteTarget::SP => {
                        self.stack_pointer = value;
                    }
                    LoadByteTarget::A16 => {
                        self.memory
                            .write_byte(value, ((self.stack_pointer & 0xF0) >> 8) as u8);
                        self.memory
                            .write_byte(value + 1, (self.stack_pointer & 0x0F) as u8);
                    }
                    LoadByteTarget::HLI => self
                        .memory
                        .write_byte(self.registers.get_hl(), (value & 0x0F) as u8),
                    LoadByteTarget::BCI => self
                        .memory
                        .write_byte(self.registers.get_bc(), (value & 0x0F) as u8),
                    LoadByteTarget::DEI => self
                        .memory
                        .write_byte(self.registers.get_de(), (value & 0x0F) as u8),
                    LoadByteTarget::HLADD => {
                        self.memory
                            .write_byte(self.registers.get_hl(), (value & 0x0F) as u8);
                        let hl_new = self.registers.get_hl().wrapping_add(1);
                        self.registers.h = ((hl_new & 0xF0) >> 8) as u8;
                        self.registers.l = (hl_new & 0x0F) as u8;
                    }
                    LoadByteTarget::HLDEC => {
                        self.memory
                            .write_byte(self.registers.get_hl(), (value & 0x0F) as u8);
                        let hl_new = self.registers.get_hl().wrapping_sub(1);
                        self.registers.h = ((hl_new & 0xF0) >> 8) as u8;
                        self.registers.l = (hl_new & 0x0F) as u8;
                    }
                };

                let source_add = match source {
                    LoadByteSource::D8 => self.program_counter.wrapping_add(2),
                    LoadByteSource::D16 => self.program_counter.wrapping_add(3),
                    _ => self.program_counter.wrapping_add(1),
                };

                // Add extra cycles to some targets
                match target {
                    LoadByteTarget::A16 => source_add.wrapping_add(2), // Total 3
                    _ => source_add,
                }
            }
            Instruction::Push(target) => {
                let value = match target {
                    StackTarget::BC => self.registers.get_bc(),
                    StackTarget::DE => self.registers.get_de(),
                    StackTarget::HL => self.registers.get_hl(),
                    StackTarget::AF => self.registers.get_af(),
                };
                self.stack_push(value);
                self.program_counter.wrapping_add(1)
            }
            Instruction::Pop(target) => {
                let value: u16 = self.stack_pop();
                match target {
                    StackTarget::BC => self.registers.set_bc(value),
                    StackTarget::DE => self.registers.set_de(value),
                    StackTarget::HL => self.registers.set_hl(value),
                    StackTarget::AF => self.registers.set_af(value),
                }

                self.program_counter.wrapping_add(1)
            }
            Instruction::Call(test) => {
                let condition: bool = match test {
                    JumpTest::NotZero => !self.registers.f.zero,
                    JumpTest::Zero => self.registers.f.zero,
                    JumpTest::NotCarry => !self.registers.f.carry,
                    JumpTest::Carry => self.registers.f.carry,
                    JumpTest::Always => true,
                };
                self.fn_call(condition)
            }
            Instruction::RST(reset_address) => {
                let new_address: u16 = reset_address;
                self.stack_push(new_address.wrapping_add(1));
                new_address
            }
            Instruction::Return(test) => {
                let condition: bool = match test {
                    JumpTest::NotZero => !self.registers.f.zero,
                    JumpTest::Zero => self.registers.f.zero,
                    JumpTest::NotCarry => !self.registers.f.carry,
                    JumpTest::Carry => self.registers.f.carry,
                    JumpTest::Always => true,
                };
                self.fn_return(condition)
            }
            Instruction::Nop => self.program_counter.wrapping_add(1),
            Instruction::Halt => {
                self.halted = true;
                self.program_counter.wrapping_add(2)
            }
            Instruction::Compare(target) => {
                let value: u8 = self.register_value(target);
                self.subtract(value);

                self.program_counter.wrapping_add(1)
            }
            Instruction::Sub(target) => {
                let value: u8 = self.register_value(target);
                let new_v = self.subtract(value);
                self.registers.a = new_v;

                self.program_counter.wrapping_add(1)
            }
            Instruction::SubC(target) => {
                let value: u8 = self.register_value(target);
                let c = if self.registers.f.carry { 1 } else { 0 } as u8;

                self.subtract(c);
                let mid_carry = self.registers.f.carry;

                let new_v = self.subtract(value);

                self.registers.f.carry |= mid_carry;
                self.registers.a = new_v;

                self.program_counter.wrapping_add(1)
            }
            Instruction::Inc16(target) => {
                let mut register: u16 = match target {
                    TargetRegister16::BC => self.registers.get_bc(),
                    TargetRegister16::DE => self.registers.get_de(),
                    TargetRegister16::HL => self.registers.get_hl(),
                    TargetRegister16::SP => self.stack_pointer,
                };

                register = register.wrapping_add(1);

                match target {
                    TargetRegister16::BC => {
                        self.registers.b = ((register & 0xF0) >> 8) as u8;
                        self.registers.c = (register & 0x0F) as u8;
                    }
                    TargetRegister16::DE => {
                        self.registers.d = ((register & 0xF0) >> 8) as u8;
                        self.registers.e = (register & 0x0F) as u8;
                    }
                    TargetRegister16::HL => {
                        self.registers.h = ((register & 0xF0) >> 8) as u8;
                        self.registers.l = (register & 0x0F) as u8;
                    }
                    TargetRegister16::SP => self.stack_pointer = register,
                }

                self.program_counter.wrapping_add(1)
            }
            Instruction::Dec16(target) => {
                let mut register: u16 = match target {
                    TargetRegister16::BC => self.registers.get_bc(),
                    TargetRegister16::DE => self.registers.get_de(),
                    TargetRegister16::HL => self.registers.get_hl(),
                    TargetRegister16::SP => self.stack_pointer,
                };

                register = register.wrapping_sub(1);

                match target {
                    TargetRegister16::BC => {
                        self.registers.b = ((register & 0xF0) >> 8) as u8;
                        self.registers.c = (register & 0x0F) as u8;
                    }
                    TargetRegister16::DE => {
                        self.registers.d = ((register & 0xF0) >> 8) as u8;
                        self.registers.e = (register & 0x0F) as u8;
                    }
                    TargetRegister16::HL => {
                        self.registers.h = ((register & 0xF0) >> 8) as u8;
                        self.registers.l = (register & 0x0F) as u8;
                    }
                    TargetRegister16::SP => self.stack_pointer = register,
                }

                self.program_counter.wrapping_add(1)
            }
            Instruction::Stop => todo!(),
            Instruction::Add16(target_register16) => {
                let value = match target_register16 {
                    TargetRegister16::BC => self.registers.get_bc(),
                    TargetRegister16::DE => self.registers.get_de(),
                    TargetRegister16::HL => self.registers.get_hl(),
                    TargetRegister16::SP => self.stack_pointer,
                };

                let old = self.registers.get_hl();

                let (new_v, overflow) = old.overflowing_add(value);
                self.registers.f.carry = overflow;
                self.registers.f.half_carry = (old & 0x0F00) + (value & 0x0F00) > 0x0F00;
                self.registers.f.zero = false;
                self.registers.f.subtract = false;

                self.registers.h = ((new_v & 0xF0) >> 8) as u8;
                self.registers.l = (new_v & 0x0F) as u8;

                self.program_counter.wrapping_add(1)
            }
            Instruction::Bit(bit, target_register) => {
                self.registers.f.zero = (self.register_value(target_register) & (0b1 << bit)) == 0;
                self.program_counter.wrapping_add(1)
            }
            Instruction::JumpRelative(jump_test) => todo!(),
            Instruction::JumpToHL => {
                self.program_counter = self.registers.get_hl();
                self.program_counter
            }
            Instruction::ReturnEnableInterrupt => todo!(),
            Instruction::EnableInterrupt => todo!(),
            Instruction::DisableInterrupt => todo!(),
            Instruction::DecimalAdjustAccumulator => todo!(),
            Instruction::ComplimentAccumulator => todo!(),
            Instruction::SetCarry => {
                self.registers.f.carry = true;
                self.registers.f.half_carry = false;
                self.registers.f.subtract = false;
                self.program_counter.wrapping_add(1)
            }
            Instruction::ComplimentCarry => {
                self.registers.f.carry = !self.registers.f.carry;
                self.registers.f.half_carry = false;
                self.registers.f.subtract = false;
                self.program_counter.wrapping_add(1)
            }
            Instruction::AddStackPointerR8 => {
                let add: i8 = self.register_value(TargetRegister::D8) as i8;
                let old = (self.stack_pointer & 0x0F) as u8;

                if add >= 0 {
                    self.stack_pointer = self.stack_pointer.wrapping_add(add as u16);
                } else {
                    self.stack_pointer = self.stack_pointer.wrapping_sub(add.abs() as u16);
                }

                // TODO: What about sub? Can't that carry or cook things?

                let (new_v, overflow) = old.overflowing_add(add as u8);
                self.registers.f.carry = overflow;
                self.registers.f.half_carry = (old & 0xF) + (add as u8 & 0xF) > 0xF;
                self.registers.f.zero = false;
                self.registers.f.subtract = false;

                self.program_counter.wrapping_add(2)
            }
            Instruction::LoadHLStackPointerR8 => {
                let add: i8 = self.register_value(TargetRegister::D8) as i8;
                let old = (self.stack_pointer & 0x0F) as u8;

                let new;
                if add >= 0 {
                    new = self.stack_pointer.wrapping_add(add as u16);
                } else {
                    new = self.stack_pointer.wrapping_sub(add.abs() as u16);
                }

                self.registers.h = ((new & 0xF0) >> 8) as u8;
                self.registers.l = (new & 0x0F) as u8;

                // TODO: What about sub? Can't that carry or cook things?

                let (new_v, overflow) = old.overflowing_add(add as u8);
                self.registers.f.carry = overflow;
                self.registers.f.half_carry = (old & 0xF) + (add as u8 & 0xF) > 0xF;
                self.registers.f.zero = false;
                self.registers.f.subtract = false;

                self.program_counter.wrapping_add(2)
            }
            Instruction::LoadStackPointerHL => {
                self.stack_pointer = self.registers.get_hl();
                self.program_counter.wrapping_add(1)
            }
        }
    }

    fn register_value(&self, target: TargetRegister) -> u8 {
        match target {
            TargetRegister::A => self.registers.a,
            TargetRegister::B => self.registers.b,
            TargetRegister::C => self.registers.c,
            TargetRegister::D => self.registers.d,
            TargetRegister::E => self.registers.e,
            TargetRegister::H => self.registers.h,
            TargetRegister::L => self.registers.l,
            TargetRegister::D8 => self.memory.read_byte(self.program_counter.wrapping_add(1)),
            TargetRegister::HLI => self.memory.read_byte(self.registers.get_hl()),
        }
    }

    fn register_reference(&mut self, target: TargetRegister) -> &mut u8 {
        match target {
            TargetRegister::A => &mut self.registers.a,
            TargetRegister::B => &mut self.registers.b,
            TargetRegister::C => &mut self.registers.c,
            TargetRegister::D => &mut self.registers.d,
            TargetRegister::E => &mut self.registers.e,
            TargetRegister::H => &mut self.registers.h,
            TargetRegister::L => &mut self.registers.l,
            TargetRegister::D8 => panic!("Cannot Write to D8 as a register"),
            TargetRegister::HLI => panic!("Cannot Write to HLI as a register"),
        }
    }

    fn fn_call(&mut self, should_jump: bool) -> u16 {
        let next_program_counter = self.program_counter.wrapping_add(3);
        if should_jump {
            self.stack_push(next_program_counter);
            self.memory.read_word(self.program_counter + 1)
        } else {
            next_program_counter
        }
    }

    fn fn_return(&mut self, should_jump: bool) -> u16 {
        if should_jump {
            self.stack_pop()
        } else {
            self.program_counter.wrapping_add(1)
        }
    }

    fn stack_push(&mut self, value: u16) {
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
        self.memory
            .write_byte(self.stack_pointer, ((value & 0xFF00) >> 8) as u8);

        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
        self.memory
            .write_byte(self.stack_pointer, (value & 0xFF) as u8);
    }

    fn stack_pop(&mut self) -> u16 {
        let lsb = self.memory.read_byte(self.stack_pointer) as u16;
        self.stack_pointer = self.stack_pointer.wrapping_add(1);

        let msb = self.memory.read_byte(self.stack_pointer) as u16;
        self.stack_pointer = self.stack_pointer.wrapping_add(1);

        (msb << 8) | lsb
    }

    fn jump(&self, should_jump: bool) -> u16 {
        if should_jump {
            // Get the (Little Endian) address to jump to
            let lsb = self.memory.read_byte(self.program_counter + 1) as u16;
            let msb = self.memory.read_byte(self.program_counter + 2) as u16;
            (msb << 8) | lsb
        } else {
            // Jump over the two bytes specifying the jump location
            self.program_counter.wrapping_add(3)
        }
    }

    fn add(&mut self, value: u8) -> u8 {
        let (new_v, overflow) = self.registers.a.overflowing_add(value);
        self.registers.f.zero = new_v == 0;
        self.registers.f.subtract = false;
        self.registers.f.carry = overflow;
        self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;
        new_v
    }

    fn subtract(&mut self, value: u8) -> u8 {
        let (new_v, overflow) = self.registers.a.overflowing_sub(value);
        self.registers.f.zero = new_v == 0;
        self.registers.f.subtract = true;
        self.registers.f.carry = overflow;
        self.registers.f.half_carry = (self.registers.a & 0xF) + (value & 0xF) > 0xF;
        new_v
    }
}
