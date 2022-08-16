// These are "program attributes". `mbc` is required, and the rest can take
// default values when not specified. Theoretically, they let the compiler know
// stuff about your game such as the MBC you're using, rom size, ram size, etc.
//
// In truth, the compiler only actually supports the rom_only MBC and other MBCs
// aren't really planned because this is all more like an experiment to goof
// around with the language part of things.
#![mbc(rom_only)]
#![title="demo title"]

// The code is based on the CC0 licensed hello-world, from the GB Asm Tutorial:
// https://eldred.fr/gb-asm-tutorial/assets/hello-world.asm

// Theoretically some sort of `use` and module system could be nice? but
// "everything in one file" is also fine enough since probably i'll be the only
// person ever to program with this goofy thing.

// Like in rust, a `const` is a value that you can use during compilaion
// anywhere that you'd need an integer literal. They don't take up any runtime
// space. Limited math is supported within const expressions, using `u16` as the
// datatype. If a const is used with an instruction that needs a smaller sized
// integer (either `u8` or `u3`) it's fine as long as the actual value of the
// const fits into that smaller integer type (127 fits into a `u8`, and 5 fits
// into a `u3`). If the value does not fit, a compile error is generated.

// The "directives" are styled to look like rust macro invocations using `[]`:
// * name![args here]
//
// Different directives might be appropriate in different positions within a
// program. Currently, all directives are used as expressions. Directives that
// are used in statement position might be introduced later.
// * `bit` evaluates to an int.
// * `size_of_val` evaluates to an int.
// * `include_bytes` evaluates to a byte slice.
// * `gfx` evaluates to a byte slice.

const NR52 = $FF26; // hex literals start with $ or 0x, interior _ is allowed
const LCDC = $FF40;
const LCDC_LCD_ON = bit![7];
const LCDC_WIN_TILEMAP1 = bit![6];
const LCDC_WIN_ON = bit![5];
const LCDC_CHARBLOCK_LOW = bit![4];
const LCDC_BG_TILEMAP1 = bit![3];
const LCDC_OBJ_IS_TALL = bit![2];
const LCDC_OBJ_ON = bit![1];
const LCDC_BGWIN_ON = bit![0];
const LY = $FF44;
const BGP = $FF47;
const VRAM_BLOCK2 = $9000;
const TILEMAP0 = $9800;

// Each function is its own output section and label.

// The rom's header is automatically generated for you by the compiler based on
// program attributes you declare at the top of the file. The header's entry
// point jump will automatically go to your `main` function. It's a compile
// error to not have a main function.

// Because `main` function wasn't really "called" by the entry point (using call
// to adjust the stack) it can't return (also that would be bad because the
// header isn't code anyway). We mark this with `-> !`, and then it's a compile
// error to use any return instruction, or to allow control flow to pass off the
// end of the function.
fn main -> ! {
  // shut down audio
  ld a, 0
  ld [NR52], a

  // Wait for VBlank to turn off LCD
  loop {
    ld a, [LY]
    cp 144 // decimal literals don't have a prefix, interior _ is allowed.
    // the end of every loop must explicitly state when to break or continue the
    // loop. You could also use break or continue at other points of the loop
    // body if you want to continue the loop early for example.
    if c, continue
  }

  /*
  Not needed/shown in this example, but i'd also like to support if and if-else
  at some point.
  */

  // Turn LCD off
  ld a, 0
  ld [LCDC], a

  // copy tile data
  ld de, Tiles
  ld hl, VRAM_BLOCK2
  ld bc, size_of_val![Tiles]
  call simple_copy

  // copy tile map data
  ld de, TileMap
  ld hl, TILEMAP0
  ld bc, size_of_val![TileMap]
  call simple_copy

  // Turn LCD on
  ld a, LCDC_LCD_ON | LCDC_BGWIN_ON
  ld [LCDC], a

  // set the four background palette indexes:
  // 2-bits each, low to high bits
  ld a, %11_10_01_00 // binary literals start with % or 0b, interior _ is allowed
  ld [BGP], a

  // just spin loop, our demo has nothing else to do.
  loop {}
}

// Each function

// Copies `bc` bytes from `de` to `hl`. Assumes that `bc` > 0, and so always
// copies at least one byte.
fn simple_copy {
  loop {
    ld a, [de]
    ld [hl+], a
    inc de
    dec bc
    ld a, b
    or a, c
    if nz, continue
  }
  // A `ret` instruction is automatically inserted at the end of this function
  // during compilation, you don't have to state it explicitly. You could if you
  // really want (and the compiler won't put two `ret` in a row in that case),
  // but you just don't have to.
}

// A `static` puts non-instruction data into the output rom. In this case, we're
// putting an unspecified number of bytes with `[u8]`. You can also specify
// `[u8; N]` and the compiler will issue an error if the number of bytes listed
// isn't exactly equal to `N`.

static Tiles: [u8] = [
  $00,$ff, $00,$ff, $00,$ff, $00,$ff, $00,$ff, $00,$ff, $00,$ff, $00,$ff,
	$00,$ff, $00,$80, $00,$80, $00,$80, $00,$80, $00,$80, $00,$80, $00,$80,
	$00,$ff, $00,$7e, $00,$7e, $00,$7e, $00,$7e, $00,$7e, $00,$7e, $00,$7e,
	$00,$ff, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01, $00,$01,
	$00,$ff, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00,
	$00,$ff, $00,$7f, $00,$7f, $00,$7f, $00,$7f, $00,$7f, $00,$7f, $00,$7f,
	$00,$ff, $03,$fc, $00,$f8, $00,$f0, $00,$e0, $20,$c0, $00,$c0, $40,$80,
	$00,$ff, $c0,$3f, $00,$1f, $00,$0f, $00,$07, $04,$03, $00,$03, $02,$01,
	$00,$80, $00,$80, $7f,$80, $00,$80, $00,$80, $7f,$80, $7f,$80, $00,$80,
	$00,$7e, $2a,$7e, $d5,$7e, $2a,$7e, $54,$7e, $ff,$00, $ff,$00, $00,$00,
	$00,$01, $00,$01, $ff,$01, $00,$01, $01,$01, $fe,$01, $ff,$01, $00,$01,
	$00,$80, $80,$80, $7f,$80, $80,$80, $00,$80, $ff,$80, $7f,$80, $80,$80,
	$00,$7f, $2a,$7f, $d5,$7f, $2a,$7f, $55,$7f, $ff,$00, $ff,$00, $00,$00,
	$00,$ff, $aa,$ff, $55,$ff, $aa,$ff, $55,$ff, $fa,$07, $fd,$07, $02,$07,
	$00,$7f, $2a,$7f, $d5,$7f, $2a,$7f, $55,$7f, $aa,$7f, $d5,$7f, $2a,$7f,
	$00,$ff, $80,$ff, $00,$ff, $80,$ff, $00,$ff, $80,$ff, $00,$ff, $80,$ff,
	$40,$80, $00,$80, $7f,$80, $00,$80, $00,$80, $7f,$80, $7f,$80, $00,$80,
	$00,$3c, $02,$7e, $85,$7e, $0a,$7e, $14,$7e, $ab,$7e, $95,$7e, $2a,$7e,
	$02,$01, $00,$01, $ff,$01, $00,$01, $01,$01, $fe,$01, $ff,$01, $00,$01,
	$00,$ff, $80,$ff, $50,$ff, $a8,$ff, $50,$ff, $a8,$ff, $54,$ff, $a8,$ff,
	$7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80,
	$ff,$00, $ff,$00, $ff,$00, $ab,$7e, $d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e,
	$ff,$01, $fe,$01, $ff,$01, $fe,$01, $ff,$01, $fe,$01, $ff,$01, $fe,$01,
	$7f,$80, $ff,$80, $7f,$80, $ff,$80, $7f,$80, $ff,$80, $7f,$80, $ff,$80,
	$ff,$00, $ff,$00, $ff,$00, $aa,$7f, $d5,$7f, $aa,$7f, $d5,$7f, $aa,$7f,
	$f8,$07, $f8,$07, $f8,$07, $80,$ff, $00,$ff, $aa,$ff, $55,$ff, $aa,$ff,
	$7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $ff,$80, $7f,$80, $ff,$80,
	$d5,$7f, $aa,$7f, $d5,$7f, $aa,$7f, $d5,$7f, $aa,$7f, $d5,$7f, $aa,$7f,
	$d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e, $d5,$7e, $eb,$3c,
	$54,$ff, $aa,$ff, $54,$ff, $aa,$ff, $54,$ff, $aa,$ff, $54,$ff, $aa,$ff,
	$7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $00,$ff,
	$d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e, $d5,$7e, $2a,$ff,
	$ff,$01, $fe,$01, $ff,$01, $fe,$01, $ff,$01, $fe,$01, $ff,$01, $80,$ff,
	$7f,$80, $ff,$80, $7f,$80, $ff,$80, $7f,$80, $ff,$80, $7f,$80, $aa,$ff,
	$ff,$00, $ff,$00, $ff,$00, $ff,$00, $ff,$00, $ff,$00, $ff,$00, $2a,$ff,
	$ff,$01, $fe,$01, $ff,$01, $fe,$01, $fe,$01, $fe,$01, $fe,$01, $80,$ff,
	$7f,$80, $ff,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $7f,$80, $00,$ff,
	$fe,$01, $fe,$01, $fe,$01, $fe,$01, $fe,$01, $fe,$01, $fe,$01, $80,$ff,
	$3f,$c0, $3f,$c0, $3f,$c0, $1f,$e0, $1f,$e0, $0f,$f0, $03,$fc, $00,$ff,
	$fd,$03, $fc,$03, $fd,$03, $f8,$07, $f9,$07, $f0,$0f, $c1,$3f, $82,$ff,
	$55,$ff, $2a,$7e, $54,$7e, $2a,$7e, $54,$7e, $2a,$7e, $54,$7e, $00,$7e,
	$01,$ff, $00,$01, $01,$01, $00,$01, $01,$01, $00,$01, $01,$01, $00,$01,
	$54,$ff, $ae,$f8, $50,$f0, $a0,$e0, $60,$c0, $80,$c0, $40,$80, $40,$80,
	$55,$ff, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00,
	$55,$ff, $6a,$1f, $05,$0f, $02,$07, $05,$07, $02,$03, $03,$01, $02,$01,
	$54,$ff, $80,$80, $00,$80, $80,$80, $00,$80, $80,$80, $00,$80, $00,$80,
	$55,$ff, $2a,$1f, $0d,$07, $06,$03, $01,$03, $02,$01, $01,$01, $00,$01,
	$55,$ff, $2a,$7f, $55,$7f, $2a,$7f, $55,$7f, $2a,$7f, $55,$7f, $00,$7f,
	$55,$ff, $aa,$ff, $55,$ff, $aa,$ff, $55,$ff, $aa,$ff, $55,$ff, $00,$ff,
	$15,$ff, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00, $00,$00,
	$55,$ff, $6a,$1f, $0d,$07, $06,$03, $01,$03, $02,$01, $03,$01, $00,$01,
	$54,$ff, $a8,$ff, $54,$ff, $a8,$ff, $50,$ff, $a0,$ff, $40,$ff, $00,$ff,
	$00,$7e, $2a,$7e, $d5,$7e, $2a,$7e, $54,$7e, $ab,$76, $dd,$66, $22,$66,
	$00,$7c, $2a,$7e, $d5,$7e, $2a,$7e, $54,$7c, $ff,$00, $ff,$00, $00,$00,
	$00,$01, $00,$01, $ff,$01, $02,$01, $07,$01, $fe,$03, $fd,$07, $0a,$0f,
	$00,$7c, $2a,$7e, $d5,$7e, $2a,$7e, $54,$7e, $ab,$7e, $d5,$7e, $2a,$7e,
	$00,$ff, $a0,$ff, $50,$ff, $a8,$ff, $54,$ff, $a8,$ff, $54,$ff, $aa,$ff,
	$dd,$62, $bf,$42, $fd,$42, $bf,$40, $ff,$00, $ff,$00, $f7,$08, $ef,$18,
	$ff,$00, $ff,$00, $ff,$00, $ab,$7c, $d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e,
	$f9,$07, $fc,$03, $fd,$03, $fe,$01, $ff,$01, $fe,$01, $ff,$01, $fe,$01,
	$d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e, $d5,$7e, $ab,$7e, $d5,$7e, $ab,$7c,
	$f7,$18, $eb,$1c, $d7,$3c, $eb,$3c, $d5,$3e, $ab,$7e, $d5,$7e, $2a,$ff,
	$ff,$01, $fe,$01, $ff,$01, $fe,$01, $ff,$01, $fe,$01, $ff,$01, $a2,$ff,
	$7f,$c0, $bf,$c0, $7f,$c0, $bf,$e0, $5f,$e0, $af,$f0, $57,$fc, $aa,$ff,
	$ff,$01, $fc,$03, $fd,$03, $fc,$03, $f9,$07, $f0,$0f, $c1,$3f, $82,$ff,
	$55,$ff, $2a,$ff, $55,$ff, $2a,$ff, $55,$ff, $2a,$ff, $55,$ff, $00,$ff,
	$45,$ff, $a2,$ff, $41,$ff, $82,$ff, $41,$ff, $80,$ff, $01,$ff, $00,$ff,
	$54,$ff, $aa,$ff, $54,$ff, $aa,$ff, $54,$ff, $aa,$ff, $54,$ff, $00,$ff,
	$15,$ff, $2a,$ff, $15,$ff, $0a,$ff, $15,$ff, $0a,$ff, $01,$ff, $00,$ff,
	$01,$ff, $80,$ff, $01,$ff, $80,$ff, $01,$ff, $80,$ff, $01,$ff, $00,$ff,
];

static TileMap: [u8] = [
  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $01, $02, $03, $01, $04, $03, $01, $05, $00, $01, $05, $00, $06, $04, $07, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $08, $09, $0a, $0b, $0c, $0d, $0b, $0e, $0f, $08, $0e, $0f, $10, $11, $12, $13, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $14, $15, $16, $17, $18, $19, $1a, $1b, $0f, $14, $1b, $0f, $14, $1c, $16, $1d, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $1e, $1f, $20, $21, $22, $23, $24, $22, $25, $1e, $22, $25, $26, $22, $27, $1d, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $01, $28, $29, $2a, $2b, $2c, $2d, $2b, $2e, $2d, $2f, $30, $2d, $31, $32, $33, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $08, $34, $0a, $0b, $11, $0a, $0b, $35, $36, $0b, $0e, $0f, $08, $37, $0a, $38, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $14, $39, $16, $17, $1c, $16, $17, $3a, $3b, $17, $1b, $0f, $14, $3c, $16, $1d, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $1e, $3d, $3e, $3f, $22, $27, $21, $1f, $20, $21, $22, $25, $1e, $22, $40, $1d, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $00, $41, $42, $43, $44, $30, $33, $41, $45, $43, $41, $30, $43, $41, $30, $33, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,  0,0,0,0,0,0,0,0,0,0,0,0,
];

// Here is the list of supported instruction mnemonics. Within an actual program
// they're case-insensitive, but for this example list the case matters.
// * The parts in caps indicate what the programmer would write directly.
// * The parts in lowercase indicate where the programmer would substitute a
//   value (`n8`, `n16`, `e8`, or `u3`), a register name (`r8`, r16`), or a
//   condition code (`cc`).
// * In any place a number is needed the programer can write a literal, const,
//   or label. Every function and static is labeled with its own name. When a
//   label is used the value is the final address of that label after placement
//   within the rom's layout.

/*
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
