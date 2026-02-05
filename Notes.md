Full instruction set: https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html

Note that the ADD A,(HL) means get the value in memory at address HL and add it to A


Functions I need:

- 16 bit Load
- 16 bit INC
- 16 bit Add
- Relative Jump (Signed), with conditions
- 16 bit DEC

# Things

- Consider renaming the target and source ending in `I` to `A` to represent an address not immediate. I guess it could mean indirect but that seems less common
