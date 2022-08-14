//! Stuff that "lexes" source text into "lexemes".
//!
//! Use the `DmgToken` enum and the [`Logos`](logos::Logos) trait to lex a
//! string into its tokens.
//!
//! ```no_run
//! use dmgrs::lexer::DmgToken;
//! use logos::Logos; // trait, provides the `lexer` function
//!
//! const SRC: &str = "ld a, 0";
//! for (span, token) in DmgToken::lexer(SRC).spanned() {
//!   println!("{span:?} => {token:?}");
//! }
//! ```
//!
//! Most likely you'll want to *parse* the token stream that come out of lexing.
//! You can pass the `DmgToken::lexer(SRC).spanned()` iterator over to
//! [`chumsky::Stream::from_iter`](chumsky::Stream::from_iter). You'll also need
//! to tell chumsky what the "span" value will be when you've reached the
//! end-of-input, which will generally be the source string length as both sides
//! of a `..` range:
//!
//! ```no_run
//! use chumsky::stream::Stream;
//! use dmgrs::lexer::DmgToken;
//! use logos::Logos; // trait, provides the `lexer` function
//!
//! const SRC: &str = "ld a, 0";
//! let len = SRC.len();
//! let lex_iter = DmgToken::lexer(SRC).spanned();
//! let stream = Stream::from_iter(len..len, lex_iter);
//! ```

use logos::Logos;

// TODO: probably the register names should ALSO have empty variants.

/// The possible tokens within a Dmgrs program.
///
/// There's many no-data variants in this enum. Theoretically these are not
/// needed, they could be captured as `Ident` values. However, `Ident` stores
/// the identifier as a `Box<str>`, and the instructions and keywords are
/// presumed to be quite common, so we give them each their own empty variant to
/// avoid excess allocations.
#[derive(Clone, PartialEq, Eq, Logos)] // custom Debug at the end of the file
pub enum DmgToken {
  /// An error during lexing.
  #[error]
  Error,

  /// Spaces and tabs, but *not* line endings.
  #[regex(r"[ \t]+", logos::skip)]
  HorizontalWhitespace,

  /// Starts with `//` and eats characters until the end of line.
  #[regex(r"//[^\r\n]*", logos::skip)]
  EndOfLineComment,

  /// Multi-line comments start with `/*`
  #[token(r"/*", priority = 2)]
  StartMultiComment,

  /// Multi-line comments end with `*/`
  #[token(r"*/", priority = 2)]
  EndMultiComment,

  /// A line ends with `\r\n` (windows), `\n` (unix), or `\r` (pre-unix mac).
  #[regex(r"(\r\n|\n|\r)")]
  EndOfLine,

  /// A punctuation character (using the `[:punct:]` regex class) that does
  /// *not* match any other case.
  #[regex(r"[[:punct:]]", |lex| lex.slice().chars().next().unwrap())]
  Punctuation(char),

  /// A standard identifier in C-style langs: `[_a-zA-Z][_a-zA-Z0-9]*`
  ///
  /// The lone character `_` ends up matching as an Ident rather than a
  /// Punctuation.
  #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |lex| lex.slice().to_string().into_boxed_str(), priority=2)]
  Ident(Box<str>),

  /// The digits of a binary literal (no prefix).
  ///
  /// * Binary literals are a digit sequence (`0-1`) prefixed with `%` or `0b`
  /// * Use of `_` is allowed in the literal, but they don't change the value.
  #[regex(r"(%|0b)[01_]+", |lex| {let s = lex.slice(); try_bin_str_to_u16(if s.starts_with('%') { &s[1..] } else { &s[2..] }) })]
  BinaryLiteral(u16),

  /// The digits of a decimal literal.
  ///
  /// * Decimal literals are a digit sequence (`0-9`) with no prefix.
  /// * Use of `_` is allowed in the literal, but they don't change the value.
  #[regex(r"[0-9]+", |lex| try_dec_str_to_u16(lex.slice()))]
  DecimalLiteral(u16),

  /// The digits of a hex literal (no prefix).
  ///
  /// * Hex literals are a digit sequence (`0-9a-fA-F`) prefixed with `$` or
  ///   `0x`
  /// * Use of `_` is allowed in the literal, but they don't change the value.
  #[regex(r"(\$|0x)[0-9a-fA-F_]+", |lex| {let s = lex.slice(); try_hex_str_to_u16(if s.starts_with('$') { &s[1..] } else { &s[2..] }) })]
  HexLiteral(u16),

  /// Holds all the stuff *between* `"`.
  ///
  /// This allows `\"` and `\\` within the string literal that's collected, but
  /// doesn't actually handle escape sequence processing.
  ///
  /// Thanks to `Quirl`, who made this regex: "works by specifying all of the
  /// escape sequences you allow (here just `\"`, and `\\` for a `\` itself) up
  /// front and then requiring the rest of the literal to not be a quote or
  /// escape (that would start with `\`)"
  #[regex(r#""((\\"|\\\\)|[^\\"])*""#, |lex| {let s = lex.slice(); s[1..s.len()-1].to_string().into_boxed_str() })]
  StrLiteral(Box<str>),

  /// `const`
  #[regex(r"const")]
  KeywordConst,

  /// `static`
  #[regex(r"static")]
  KeywordStatic,

  /// `fn`
  #[regex(r"fn")]
  KeywordFn,

  /// `if`
  #[regex(r"if")]
  KeywordIf,

  /// `else`
  #[regex(r"else")]
  KeywordElse,

  /// `loop`
  #[regex(r"loop")]
  KeywordLoop,

  /// `continue`
  #[regex(r"continue")]
  KeywordContinue,

  /// `break`
  #[regex(r"break")]
  KeywordBreak,

  /// `return`
  #[regex(r"return")]
  KeywordReturn,

  /// `adc` or `ADC`
  #[regex(r"(adc|ADC)", priority = 3)]
  InstructionAdc,

  /// `add` or `ADD`
  #[regex(r"(add|ADD)", priority = 3)]
  InstructionAdd,

  /// `and` or `AND`
  #[regex(r"(and|AND)", priority = 3)]
  InstructionAnd,

  /// `bit` or `BIT`
  #[regex(r"(bit|BIT)", priority = 3)]
  InstructionBit,

  /// `call` or `CALL`
  #[regex(r"(call|CALL)", priority = 3)]
  InstructionCall,

  /// `ccf` or `CCF`
  #[regex(r"(ccf|CCF)", priority = 3)]
  InstructionCcf,

  /// `daa` or `DAA`
  #[regex(r"(daa|DAA)", priority = 3)]
  InstructionDaa,

  /// `dec` or `DEC`
  #[regex(r"(dec|DEC)", priority = 3)]
  InstructionDec,

  /// `di` or `DI`
  #[regex(r"(di|DI)", priority = 3)]
  InstructionDi,

  /// `ei` or `EI`
  #[regex(r"(ei|EI)", priority = 3)]
  InstructionEi,

  /// `halt` or `HALT`
  #[regex(r"(halt|HALT)", priority = 3)]
  InstructionHalt,

  /// `inc` or `INC`
  #[regex(r"(inc|INC)", priority = 3)]
  InstructionInc,

  /// `jp` or `JP`
  #[regex(r"(jp|JP)", priority = 3)]
  InstructionJp,

  /// `jr` or `JR`
  #[regex(r"(jr|JR)", priority = 3)]
  InstructionJr,

  /// `ld` or `LD`
  #[regex(r"(ld|LD)", priority = 3)]
  InstructionLd,

  /// `ldh` or `LDH`
  #[regex(r"(ldh|LDH)", priority = 3)]
  InstructionLdh,

  /// `nop` or `NOP`
  #[regex(r"(nop|NOP)", priority = 3)]
  InstructionNop,

  /// `or` or `OR`
  #[regex(r"(or|OR)", priority = 3)]
  InstructionOr,

  /// `pop` or `POP`
  #[regex(r"(pop|POP)", priority = 3)]
  InstructionPop,

  /// `push` or `PUSH`
  #[regex(r"(push|PUSH)", priority = 3)]
  InstructionPush,

  /// `res` or `RES`
  #[regex(r"(res|RES)", priority = 3)]
  InstructionRes,

  /// `ret` or `RET`
  #[regex(r"(ret|RET)", priority = 3)]
  InstructionRet,

  /// `reti` or `RETI`
  #[regex(r"(reti|RETI)", priority = 3)]
  InstructionReti,

  /// `rl` or `RL`
  #[regex(r"(rl|RL)", priority = 3)]
  InstructionRl,

  /// `rla` or `RLA`
  #[regex(r"(rla|RLA)", priority = 3)]
  InstructionRla,

  /// `rlc` or `RLC`
  #[regex(r"(rlc|RLC)", priority = 3)]
  InstructionRlc,

  /// `rlca` or `RLCA`
  #[regex(r"(rlca|RLCA)", priority = 3)]
  InstructionRlca,

  /// `rr` or `RR`
  #[regex(r"(rr|RR)", priority = 3)]
  InstructionRr,

  /// `rra` or `RRA`
  #[regex(r"(rra|RRA)", priority = 3)]
  InstructionRra,

  /// `rrc` or `RRC`
  #[regex(r"(rrc|RRC)", priority = 3)]
  InstructionRrc,

  /// `rrca` or `RRCA`
  #[regex(r"(rrca|RRCA)", priority = 3)]
  InstructionRrca,

  /// `rst` or `RST`
  #[regex(r"(rst|RST)", priority = 3)]
  InstructionRst,

  /// `sbc` or `SBC`
  #[regex(r"(sbc|SBC)", priority = 3)]
  InstructionSbc,

  /// `scf` or `SCF`
  #[regex(r"(scf|SCF)", priority = 3)]
  InstructionScf,

  /// `set` or `SET`
  #[regex(r"(set|SET)", priority = 3)]
  InstructionSet,

  /// `sla` or `SLA`
  #[regex(r"(sla|SLA)", priority = 3)]
  InstructionSla,

  /// `sra` or `SRA`
  #[regex(r"(sra|SRA)", priority = 3)]
  InstructionSra,

  /// `srl` or `SRL`
  #[regex(r"(srl|SRL)", priority = 3)]
  InstructionSrl,

  /// `stop` or `STOP`
  #[regex(r"(stop|STOP)", priority = 3)]
  InstructionStop,

  /// `sub` or `SUB`
  #[regex(r"(sub|SUB)", priority = 3)]
  InstructionSub,

  /// `swap` or `SWAP`
  #[regex(r"(swap|SWAP)", priority = 3)]
  InstructionSwap,

  /// `xor` or `XOR`
  #[regex(r"(xor|XOR)", priority = 3)]
  InstructionXor,
}
impl DmgToken {
  /// Gets the number out of the lexeme, if any.
  ///
  /// ```
  /// # use dmgrs::lexer::DmgToken;
  /// use DmgToken::*;
  /// assert_eq!(DecimalLiteral(123).get_number(), Some(123));
  /// assert_eq!(InstructionXor.get_number(), None);
  /// ```
  pub const fn get_number(&self) -> Option<u16> {
    match self {
      Self::BinaryLiteral(n) => Some(*n),
      Self::DecimalLiteral(n) => Some(*n),
      Self::HexLiteral(n) => Some(*n),
      _ => None,
    }
  }
}

/// Tries to parse a binary digit string into a `u16`.
pub fn try_bin_str_to_u16(s: &str) -> Option<u16> {
  s.chars().filter(|&c| c != '_').try_fold(0, |acc: u16, c| {
    let (new_u, overflowed) = acc.overflowing_mul(2);
    if overflowed {
      None
    } else {
      Some(
        new_u
          + match c {
            '0'..='1' => (c as u16) - ('0' as u16),
            _ => unimplemented!("Lexer allowed illegal char: {c:?}"),
          },
      )
    }
  })
}
#[test]
fn test_try_bin_str_to_u16() {
  assert_eq!(try_bin_str_to_u16("1010"), Some(0b1010));
}

/// Tries to parse a decimal digit string into a `u16`.
pub fn try_dec_str_to_u16(s: &str) -> Option<u16> {
  s.chars().filter(|&c| c != '_').try_fold(0, |acc: u16, c| {
    let (new_u, overflowed) = acc.overflowing_mul(10);
    if overflowed {
      None
    } else {
      Some(
        new_u
          + match c {
            '0'..='9' => (c as u16) - ('0' as u16),
            _ => unimplemented!("Lexer allowed illegal char: {c:?}"),
          },
      )
    }
  })
}
#[test]
fn test_try_dec_str_to_u16() {
  assert_eq!(try_dec_str_to_u16("1234"), Some(1234));
}

/// Tries to parse a hexadecimal digit string into a `u16`.
pub fn try_hex_str_to_u16(s: &str) -> Option<u16> {
  s.chars().filter(|&c| c != '_').try_fold(0, |acc: u16, c| {
    let (new_u, overflowed) = acc.overflowing_mul(16);
    if overflowed {
      None
    } else {
      Some(
        new_u
          + match c {
            '0'..='9' => (c as u16) - ('0' as u16),
            'a'..='f' => 10 + (c as u16) - ('a' as u16),
            'A'..='F' => 10 + (c as u16) - ('A' as u16),
            _ => unimplemented!("Lexer allowed illegal char: {c:?}"),
          },
      )
    }
  })
}
#[test]
fn test_try_hex_str_to_u16() {
  assert_eq!(try_hex_str_to_u16("12aF"), Some(0x12AF));
  assert_eq!(try_hex_str_to_u16("FF26"), Some(0xFF26));
}

impl core::fmt::Debug for DmgToken {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      // numbers print out using the correct radix (rust-style) for maximum
      // recognizability
      DmgToken::BinaryLiteral(n) => write!(f, "BinaryLiteral(0b{n:b})"),
      DmgToken::DecimalLiteral(n) => write!(f, "DecimalLiteral({n})"),
      DmgToken::HexLiteral(n) => write!(f, "HexLiteral(0x{n:X})"),

      // other payload variants print their payload
      DmgToken::Punctuation(p) => write!(f, "Punctuation({p:?})"),
      DmgToken::Ident(i) => write!(f, "Ident({i:?})"),
      DmgToken::StrLiteral(s) => write!(f, "Punctuation({s:?})"),

      // empty variants just print their name
      DmgToken::Error => write!(f, stringify!(Error)),
      DmgToken::HorizontalWhitespace => {
        write!(f, stringify!(HorizontalWhitespace))
      }
      DmgToken::EndOfLineComment => write!(f, stringify!(EndOfLineComment)),
      DmgToken::StartMultiComment => write!(f, stringify!(StartMultiComment)),
      DmgToken::EndMultiComment => write!(f, stringify!(EndMultiComment)),
      DmgToken::EndOfLine => write!(f, stringify!(EndOfLine)),
      DmgToken::KeywordConst => write!(f, stringify!(KeywordConst)),
      DmgToken::KeywordStatic => write!(f, stringify!(KeywordStatic)),
      DmgToken::KeywordFn => write!(f, stringify!(KeywordFn)),
      DmgToken::KeywordIf => write!(f, stringify!(KeywordIf)),
      DmgToken::KeywordElse => write!(f, stringify!(KeywordElse)),
      DmgToken::KeywordLoop => write!(f, stringify!(KeywordLoop)),
      DmgToken::KeywordContinue => write!(f, stringify!(KeywordContinue)),
      DmgToken::KeywordBreak => write!(f, stringify!(KeywordBreak)),
      DmgToken::KeywordReturn => write!(f, stringify!(KeywordReturn)),
      DmgToken::InstructionAdc => write!(f, stringify!(InstructionAdc)),
      DmgToken::InstructionAdd => write!(f, stringify!(InstructionAdd)),
      DmgToken::InstructionAnd => write!(f, stringify!(InstructionAnd)),
      DmgToken::InstructionBit => write!(f, stringify!(InstructionBit)),
      DmgToken::InstructionCall => write!(f, stringify!(InstructionCall)),
      DmgToken::InstructionCcf => write!(f, stringify!(InstructionCcf)),
      DmgToken::InstructionDaa => write!(f, stringify!(InstructionDaa)),
      DmgToken::InstructionDec => write!(f, stringify!(InstructionDec)),
      DmgToken::InstructionDi => write!(f, stringify!(InstructionDi)),
      DmgToken::InstructionEi => write!(f, stringify!(InstructionEi)),
      DmgToken::InstructionHalt => write!(f, stringify!(InstructionHalt)),
      DmgToken::InstructionInc => write!(f, stringify!(InstructionInc)),
      DmgToken::InstructionJp => write!(f, stringify!(InstructionJp)),
      DmgToken::InstructionJr => write!(f, stringify!(InstructionJr)),
      DmgToken::InstructionLd => write!(f, stringify!(InstructionLd)),
      DmgToken::InstructionLdh => write!(f, stringify!(InstructionLdh)),
      DmgToken::InstructionNop => write!(f, stringify!(InstructionNop)),
      DmgToken::InstructionOr => write!(f, stringify!(InstructionOr)),
      DmgToken::InstructionPop => write!(f, stringify!(InstructionPop)),
      DmgToken::InstructionPush => write!(f, stringify!(InstructionPush)),
      DmgToken::InstructionRes => write!(f, stringify!(InstructionRes)),
      DmgToken::InstructionRet => write!(f, stringify!(InstructionRet)),
      DmgToken::InstructionReti => write!(f, stringify!(InstructionReti)),
      DmgToken::InstructionRl => write!(f, stringify!(InstructionRl)),
      DmgToken::InstructionRla => write!(f, stringify!(InstructionRla)),
      DmgToken::InstructionRlc => write!(f, stringify!(InstructionRlc)),
      DmgToken::InstructionRlca => write!(f, stringify!(InstructionRlca)),
      DmgToken::InstructionRr => write!(f, stringify!(InstructionRr)),
      DmgToken::InstructionRra => write!(f, stringify!(InstructionRra)),
      DmgToken::InstructionRrc => write!(f, stringify!(InstructionRrc)),
      DmgToken::InstructionRrca => write!(f, stringify!(InstructionRrca)),
      DmgToken::InstructionRst => write!(f, stringify!(InstructionRst)),
      DmgToken::InstructionSbc => write!(f, stringify!(InstructionSbc)),
      DmgToken::InstructionScf => write!(f, stringify!(InstructionScf)),
      DmgToken::InstructionSet => write!(f, stringify!(InstructionSet)),
      DmgToken::InstructionSla => write!(f, stringify!(InstructionSla)),
      DmgToken::InstructionSra => write!(f, stringify!(InstructionSra)),
      DmgToken::InstructionSrl => write!(f, stringify!(InstructionSrl)),
      DmgToken::InstructionStop => write!(f, stringify!(InstructionStop)),
      DmgToken::InstructionSub => write!(f, stringify!(InstructionSub)),
      DmgToken::InstructionSwap => write!(f, stringify!(InstructionSwap)),
      DmgToken::InstructionXor => write!(f, stringify!(InstructionXor)),
    }
  }
}
