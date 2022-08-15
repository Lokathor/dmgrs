//! Stuff that "lexes" source text into "lexemes".
//!
//! Use the `DmgToken` enum and the [`Logos`](logos::Logos) trait to lex a
//! string into its tokens.
//!
//! ```no_run
//! use dmgrs::lexer::Lexeme;
//! use logos::Logos; // trait, provides the `lexer` function
//!
//! const SRC: &str = "ld a, 0";
//! for (span, lexeme) in Lexeme::lexer(SRC).spanned() {
//!   println!("{span:?} => {lexeme:?}");
//! }
//! ```
//!
//! Most likely you'll want to *parse* the token stream that come out of lexing.
//! You can pass the `Lexeme::lexer(SRC).spanned()` iterator over to
//! [`chumsky::Stream::from_iter`](chumsky::Stream::from_iter). You'll also need
//! to tell chumsky what the "span" value will be when you've reached the
//! end-of-input, which will generally be the source string length as both sides
//! of a `..` range:
//!
//! ```no_run
//! use chumsky::stream::Stream;
//! use dmgrs::lexer::Lexeme;
//! use logos::Logos; // trait, provides the `lexer` function
//!
//! const SRC: &str = "ld a, 0";
//! let len = SRC.len();
//! let lex_iter = Lexeme::lexer(SRC).spanned();
//! let stream = Stream::from_iter(len..len, lex_iter);
//! ```

use logos::Logos;

use crate::{str_cache_impl::cache_str, StaticStr};

/// The possible tokens within a Dmgrs program.
///
/// At this early stage of the processing all keywords, instructions, and
/// register names just count as an "ident". A simple string interning mechanism
/// is used, so it's not a super allocation fest to do this.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Logos)] // Note: custom Debug at the end of the file
pub enum Lexeme {
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
  Punct(char),

  /// A standard identifier in C-style langs: `[_a-zA-Z][_a-zA-Z0-9]*`
  ///
  /// The lone character `_` ends up matching as an Ident rather than a
  /// Punctuation.
  #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |lex| cache_str(lex.slice()), priority=2)]
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
  #[regex(r#""((\\"|\\\\)|[^\\"])*""#, |lex| {let s = lex.slice(); cache_str(&s[1..s.len()-1]) })]
  Str(StaticStr),

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
impl Lexeme {
  /// If this token is an `Ident`
  pub const fn is_ident(&self) -> bool {
    match self {
      Self::Ident(_) => true,
      _ => false,
    }
  }

  /// Unwraps the `Ident` content
  pub const fn unwrap_ident(self) -> StaticStr {
    match self {
      Self::Ident(i) => i,
      _ => panic!("unwrapped a non-ident"),
    }
  }

  /// If this token is any number token
  pub const fn is_number(&self) -> bool {
    match self {
      Self::BinaryLiteral(_) => true,
      Self::DecimalLiteral(_) => true,
      Self::HexLiteral(_) => true,
      _ => false,
    }
  }
  /// Unwraps the number inside.
  pub const fn unwrap_number(self) -> u16 {
    match self {
      Self::BinaryLiteral(n) => n,
      Self::DecimalLiteral(n) => n,
      Self::HexLiteral(n) => n,
      _ => panic!("unwrapped a non-number"),
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

impl core::fmt::Debug for Lexeme {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      // numbers print out using the correct radix (rust-style) for maximum
      // recognizability
      Lexeme::BinaryLiteral(n) => write!(f, "BinaryLiteral(0b{n:b})"),
      Lexeme::DecimalLiteral(n) => write!(f, "DecimalLiteral({n})"),
      Lexeme::HexLiteral(n) => write!(f, "HexLiteral(0x{n:X})"),

      // other payload variants print their payload
      Lexeme::Punct(p) => write!(f, "Punct({p:?})"),
      Lexeme::Ident(i) => write!(f, "Ident({i:?})"),
      Lexeme::Str(s) => write!(f, "Str({s:?})"),

      // empty variants just print their name
      Lexeme::Error => write!(f, stringify!(Error)),
      Lexeme::HorizontalWhitespace => {
        write!(f, stringify!(HorizontalWhitespace))
      }
      Lexeme::EndOfLineComment => write!(f, stringify!(EndOfLineComment)),
      Lexeme::StartMultiComment => write!(f, stringify!(StartMultiComment)),
      Lexeme::EndMultiComment => write!(f, stringify!(EndMultiComment)),
      Lexeme::EndOfLine => write!(f, stringify!(EndOfLine)),
    }
  }
}

impl core::fmt::Display for Lexeme {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      // numbers print out using the correct radix (rust-style) for maximum
      // recognizability
      Lexeme::BinaryLiteral(n) => write!(f, "%{n:b}"),
      Lexeme::DecimalLiteral(n) => write!(f, "{n}"),
      Lexeme::HexLiteral(n) => write!(f, "${n:X}"),

      // other payload variants print their payload
      Lexeme::Punct(p) => write!(f, "{p}"),
      Lexeme::Ident(i) => write!(f, "{i}"),
      Lexeme::Str(s) => write!(f, "{s:?}"),

      // empty variants just print their name
      Lexeme::Error => write!(f, stringify!(Error)),
      Lexeme::HorizontalWhitespace => {
        write!(f, " ")
      }
      Lexeme::EndOfLineComment => write!(f, "//..."),
      Lexeme::StartMultiComment => write!(f, "/*"),
      Lexeme::EndMultiComment => write!(f, "*/"),
      Lexeme::EndOfLine => write!(f, "\n"),
    }
  }
}
