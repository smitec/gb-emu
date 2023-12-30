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

                self.program_counter + 1
            }
            Instruction::AddC(target) => {
                let value: u8 = self.register_value(target);
                let c = if self.registers.f.carry { 1 } else { 0 } as u8;

                self.add(c);
                let mid_carry = self.registers.f.carry;

                let new_v = self.add(value);

                self.registers.f.carry |= mid_carry;
                self.registers.a = new_v;

                self.program_counter + 1
            }
            Instruction::Set(target, bit) => {
                let register: &mut u8 = self.register_reference(target);
                let value = set(*register, bit);
                *register = value;

                self.program_counter + 2
            }
            Instruction::Reset(target, bit) => {
                let register: &mut u8 = self.register_reference(target);
                let value = reset(*register, bit);
                *register = value;

                self.program_counter + 2
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

                self.program_counter + 1
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

                self.program_counter + 1
            }
            Instruction::ShiftLeft(target, mode, with_carry) => {
                let original: u8 = self.register_value(target);
                let original_carry = if self.registers.f.carry { 1 } else { 0 };
                let (mut value, carry) = rot_left(original);

                match mode {
                    ShiftMode::Logical => {}
                    ShiftMode::Rolling => {
                        if with_carry {
                            value |= original_carry;
                        } else {
                            value |= carry;
                        }
                    }
                    ShiftMode::Arithmetic => {}
                };

                self.registers.f.zero = value == 0;
                self.registers.f.subtract = false;
                self.registers.f.carry = carry == 1;
                self.registers.f.half_carry = false;

                let register: &mut u8 = self.register_reference(target);
                *register = value;

                todo!() // PC increment
            }
            Instruction::ShiftRight(target, mode, with_carry) => {
                let original: u8 = self.register_value(target);
                let original_carry = if self.registers.f.carry { 1 } else { 0 };
                let (mut value, carry) = rot_right(original);

                match mode {
                    ShiftMode::Logical => {}
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
                };

                self.registers.f.zero = value == 0;
                self.registers.f.subtract = false;
                self.registers.f.carry = carry == 1;
                self.registers.f.half_carry = false;

                let register: &mut u8 = self.register_reference(target);
                *register = value;

                todo!() // PC increment
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

                self.program_counter + 1
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

                self.program_counter + 2
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
            // TODO: LoadWord, LoadAFromIndirect, LoadIndirectFromA, Load AFromByteAddress,
            // LoadByteAddressFromA
            Instruction::LoadByte(target, source) => {
                let value = match source {
                    LoadByteSource::A => self.registers.a,
                    LoadByteSource::B => self.registers.b,
                    LoadByteSource::C => self.registers.c,
                    LoadByteSource::D => self.registers.d,
                    LoadByteSource::E => self.registers.e,
                    LoadByteSource::H => self.registers.h,
                    LoadByteSource::L => self.registers.l,
                    LoadByteSource::D8 => self.memory.read_byte(self.program_counter + 1),
                    LoadByteSource::HLI => self.memory.read_byte(self.registers.get_hl()),
                };

                match target {
                    LoadByteTarget::A => self.registers.a = value,
                    LoadByteTarget::B => self.registers.b = value,
                    LoadByteTarget::C => self.registers.c = value,
                    LoadByteTarget::D => self.registers.d = value,
                    LoadByteTarget::E => self.registers.e = value,
                    LoadByteTarget::H => self.registers.h = value,
                    LoadByteTarget::L => self.registers.l = value,
                    LoadByteTarget::HLI => self.memory.write_byte(self.registers.get_hl(), value),
                };

                match source {
                    LoadByteSource::D8 => self.program_counter.wrapping_add(2),
                    _ => self.program_counter.wrapping_add(1),
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
}
