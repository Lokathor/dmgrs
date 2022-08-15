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

use crate::{intern_str_impl::intern_str, StaticStr};

// TODO: probably the register names should ALSO have empty variants.

/// The possible tokens within a Dmgrs program.
///
/// At this early stage of the processing all keywords, instructions, and
/// register names just count as an "ident". A simple string interning mechanism
/// is used, so it's not a super allocation fest to do this.
#[derive(Clone, Copy, PartialEq, Eq, Logos)] // Note: custom Debug at the end of the file
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
  #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |lex| intern_str(lex.slice()), priority=2)]
  Ident(StaticStr),

  /// Holds all the stuff *between* `"`.
  ///
  /// This allows `\"` and `\\` within the string literal that's collected, but
  /// doesn't actually handle escape sequence processing.
  ///
  /// Thanks to `Quirl`, who made this regex: "works by specifying all of the
  /// escape sequences you allow (here just `\"`, and `\\` for a `\` itself) up
  /// front and then requiring the rest of the literal to not be a quote or
  /// escape (that would start with `\`)"
  #[regex(r#""((\\"|\\\\)|[^\\"])*""#, |lex| {let s = lex.slice(); intern_str(&s[1..s.len()-1]) })]
  StrLiteral(StaticStr),

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
}
impl DmgToken {
  /// Gets the number out of the lexeme, if any.
  ///
  /// ```
  /// # use dmgrs::lexer::DmgToken;
  /// use DmgToken::*;
  /// assert_eq!(DecimalLiteral(123).get_number(), Some(123));
  /// assert_eq!(Punctuation('.').get_number(), None);
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
      DmgToken::StrLiteral(s) => write!(f, "StrLiteral({s:?})"),

      // empty variants just print their name
      DmgToken::Error => write!(f, stringify!(Error)),
      DmgToken::HorizontalWhitespace => {
        write!(f, stringify!(HorizontalWhitespace))
      }
      DmgToken::EndOfLineComment => write!(f, stringify!(EndOfLineComment)),
      DmgToken::StartMultiComment => write!(f, stringify!(StartMultiComment)),
      DmgToken::EndMultiComment => write!(f, stringify!(EndMultiComment)),
      DmgToken::EndOfLine => write!(f, stringify!(EndOfLine)),
    }
  }
}
