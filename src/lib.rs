use chumsky::prelude::*;

pub fn gb_binary_literal() -> impl Parser<char, u16, Error = Simple<char>> {
  let underscores = just('_').ignored().repeated();
  just('%').ignore_then(
    filter(|c: &char| c.is_digit(2))
      .padded_by(underscores)
      .repeated()
      .collect::<String>()
      .try_map(|s: String, span| {
        u16::from_str_radix(&s, 2)
          .map_err(|why| Simple::custom(span, format!("{why:?}")))
      }),
  )
}
#[test]
fn test_gb_binary_literal() {
  let p = gb_binary_literal();
  for num in [0, 1, 76, 89, 255, 256, 492, 11489, u16::MAX] {
    assert_eq!(p.parse(format!("%{num:b}")), Ok(num));
    assert_eq!(p.parse(format!("%{num:016b}")), Ok(num));
  }
  assert_eq!(p.parse(format!("%1010_1111")), Ok(0b1010_1111));
  assert_eq!(p.parse(format!("%__1010_1111")), Ok(0b1010_1111));
  assert_eq!(p.parse(format!("%1010_1111____")), Ok(0b1010_1111));
}

pub fn gb_hex_literal() -> impl Parser<char, u16, Error = Simple<char>> {
  let underscores = just('_').ignored().repeated();
  just('$').ignore_then(
    filter(|c: &char| c.is_digit(16))
      .padded_by(underscores)
      .repeated()
      .collect::<String>()
      .try_map(|s: String, span| {
        u16::from_str_radix(&s, 16)
          .map_err(|why| Simple::custom(span, format!("{why:?}")))
      }),
  )
}
#[test]
fn test_gb_hex_literal() {
  let p = gb_hex_literal();
  for num in [0, 1, 76, 89, 255, 256, 492, 11489, u16::MAX] {
    assert_eq!(p.parse(format!("${num:x}")), Ok(num));
    assert_eq!(p.parse(format!("${num:08x}")), Ok(num));
    assert_eq!(p.parse(format!("${num:X}")), Ok(num));
    assert_eq!(p.parse(format!("${num:08X}")), Ok(num));
  }
  assert_eq!(p.parse(format!("$CF_AB")), Ok(0xCF_AB));
  assert_eq!(p.parse(format!("$__00_FF")), Ok(0x__00_FF));
  assert_eq!(p.parse(format!("$A___")), Ok(0xA___));
}

/*
TODO: Raw Instruction Forms

ADC A, [HL]
ADC A, n8
ADC A, r8
ADD A, [HL]
ADD A, n8
ADD A, r8
ADD HL, r16
ADD HL, SP
ADD SP, e8
AND A, [HL]
AND A, n8
AND A, r8
BIT u3, [HL]
BIT u3, r8
CALL cc, n16
CALL n16
CCF
CP A, [HL]
CP A, n8
CP A, r8
CPL
DAA
DEC [HL]
DEC r16
DEC r8
DEC SP
DI
EI
HALT
INC [HL]
INC r16
INC r8
INC SP
JP cc, n16
JP HL
JP n16
JR cc, e8
JR e8
LD [HL], n8
LD [HL], r8
LD [HLD], A
LD [HLI], A
LD [HL-], A
LD [HL+], A
LD [n16], A
LD [n16], SP
LD [r16], A
LD A, [HLD]
LD A, [HLI]
LD A, [HL-]
LD A, [HL+]
LD A, [n16]
LD A, [r16]
LD HL, SP+e8
LD HL, SP
LD r16, n16
LD r8, [HL]
LD r8, n8
LD r8, r8
LD SP, HL
LD SP, n16
LDH [C], A
LDH [n16], A
LDH A, [C]
LDH A, [n16]
NOP
OR A, [HL]
OR A, n8
OR A, r8
POP AF
POP r16
PUSH AF
PUSH r16
RES u3, [HL]
RES u3, r8
RET
RET cc
RETI
RL [HL]
RL r8
RLA
RLC [HL]
RLC r8
RLCA
RR [HL]
RR r8
RRA
RRC [HL]
RRC r8
RRCA
RST vec
SBC A, [HL]
SBC A, n8
SBC A, r8
SCF
SET u3, [HL]
SET u3, r8
SLA [HL]
SLA r8
SRA [HL]
SRA r8
SRL [HL]
SRL r8
STOP
SUB A, [HL]
SUB A, n8
SUB A, r8
SWAP [HL]
SWAP r8
XOR A, [HL]
XOR A, n8
XOR A, r8
*/
