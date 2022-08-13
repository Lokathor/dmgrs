#![warn(missing_docs)]
#![allow(dead_code)]

//! Stuff to work with Dmgrs assembly files.
//!
//! Right now, just a lot of [chumsky] parsers.

use std::ops::Range;

use chumsky::{
  prelude::*,
  text::{ident, newline},
};

#[allow(unused)]
macro_rules! box_str {
  ($x:expr) => {
    $x.to_string().into_boxed_str()
  };
}

/// Boxed str, but easier to type without the `<>`.
pub type BoxStr = Box<str>;

/// GB Binary literals are prefixed with `%`, then 0-1, with `_` allowed.
pub fn gb_binary_literal() -> impl Parser<char, u16, Error = Simple<char>> {
  let underscores = just('_').ignored().repeated();
  just('%').ignore_then(
    filter(|c: &char| c.is_digit(2))
      .padded_by(underscores)
      .repeated()
      .at_least(1)
      .collect::<String>()
      .try_map(|s: String, span| {
        u16::from_str_radix(&s, 2)
          .map_err(|why| Simple::custom(span, format!("{why:?}")))
      }),
  )
}
#[test]
fn test_gb_binary_literal() {
  let p = gb_binary_literal().then_ignore(end());
  for num in [0, 1, 76, 89, 255, 256, 492, 11489, u16::MAX] {
    assert_eq!(p.parse(format!("%{num:b}")), Ok(num));
    assert_eq!(p.parse(format!("%{num:016b}")), Ok(num));
  }
  assert_eq!(p.parse(format!("%1010_1111")), Ok(0b1010_1111));
  assert_eq!(p.parse(format!("%__1010_1111")), Ok(0b1010_1111));
  assert_eq!(p.parse(format!("%1010_1111____")), Ok(0b1010_1111));
}

/// GB Hex literals are prefixed with `$`, then 0-F, with `_` allowed.
pub fn gb_hex_literal() -> impl Parser<char, u16, Error = Simple<char>> {
  let underscores = just('_').ignored().repeated();
  just('$').ignore_then(
    filter(|c: &char| c.is_digit(16))
      .padded_by(underscores)
      .repeated()
      .at_least(1)
      .collect::<String>()
      .try_map(|s: String, span| {
        u16::from_str_radix(&s, 16)
          .map_err(|why| Simple::custom(span, format!("{why:?}")))
      }),
  )
}
#[test]
fn test_gb_hex_literal() {
  let p = gb_hex_literal().then_ignore(end());
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

/// Multi-line comments start with `/*` and end with `*/`
///
/// As the name implies, there can be newlines within the comment
pub fn multi_line_comment() -> impl Parser<char, (), Error = Simple<char>> {
  just("/*").then(take_until(just("*/"))).ignored()
}
#[test]
fn test_multi_line_comment() {
  let p = multi_line_comment().then_ignore(end());
  assert_eq!(p.parse("/* multi \n line \n comment */"), Ok(()));
}

/// Horizontal space doesn't advance to the next statement
///
/// * spaces
/// * tabs
/// * multi-line comments (which "count as" one space).
pub fn horizontal_space() -> impl Parser<char, (), Error = Simple<char>> {
  let space = just(' ').ignored();
  let tab = just('\t').ignored();
  let multi = multi_line_comment();
  choice((space, tab, multi)).repeated().ignored()
}
#[test]
fn test_horizontal_space() {
  let p = horizontal_space().then_ignore(end());
  assert_eq!(p.parse(""), Ok(()));
  assert_eq!(p.parse(" "), Ok(()));
  assert_eq!(p.parse("\t"), Ok(()));
  assert_eq!(p.parse("   \t   \t  "), Ok(()));
  assert_eq!(p.parse("  /* hello there \n general kenobi */ \t  "), Ok(()));
}

/// A [`newline`](chumsky::text::newline) implicitly ends all statements in
/// Dmgrs.
///
/// * [`horizontal_space`] can come before the end of the statement.
/// * By using `//`, text until the newline becomes a comment.
/// * For these purposes, the end of input counts as "a newline".
pub fn end_of_statement() -> impl Parser<char, (), Error = Simple<char>> {
  let instant_newline = newline();
  let eol_comment =
    just("//").ignored().then(take_until(newline().or(end()))).ignored();

  horizontal_space()
    .ignored()
    .then(choice((end(), instant_newline, eol_comment)))
    .ignored()
}
#[test]
fn test_end_of_statement() {
  let p = end_of_statement().then_ignore(end());
  assert_eq!(p.parse(""), Ok(()));
  assert_eq!(p.parse(" \t "), Ok(()));
  assert_eq!(p.parse("\n"), Ok(()));
  assert_eq!(p.parse(" // EOL comment before EOF"), Ok(()));
  assert_eq!(p.parse(" // EOL comment then newline\n"), Ok(()));
}

/// See [`program_attribute`]
#[derive(Debug, Clone)]
pub struct ProgramAttribute {
  span: Range<usize>,
  text: BoxStr,
}

/// Program attributes are like `#![words here]`.
///
/// The output is the text between the brackets.
pub fn program_attribute(
) -> impl Parser<char, ProgramAttribute, Error = Simple<char>> {
  just("#!")
    .ignore_then(
      filter(|c: &char| *c != ']')
        .repeated()
        .collect::<String>()
        .delimited_by(just('['), just(']')),
    )
    .map_with_span(|string, span| ProgramAttribute {
      text: string.into_boxed_str(),
      span,
    })
}
#[test]
fn test_program_attribute() {
  let p = program_attribute().then_ignore(end());
  assert_eq!(p.parse("#![]").unwrap().text, box_str!(""));
  assert_eq!(p.parse("#![foo bar]").unwrap().text, box_str!("foo bar"));
}

/// See [`macro_invocation`]
#[derive(Debug, Clone)]
pub struct MacroInvocation {
  span: Range<usize>,
  name: BoxStr,
  content: BoxStr,
}
/// Macros are invoked with `name!(words here)`
///
/// The output is the text between the brackets.
pub fn macro_invocation(
) -> impl Parser<char, MacroInvocation, Error = Simple<char>> {
  ident()
    .then_ignore(just('!'))
    .then(
      filter(|c: &char| *c != ')')
        .repeated()
        .collect::<String>()
        .delimited_by(just('('), just(')')),
    )
    .map_with_span(|(name, content), span| MacroInvocation {
      span,
      name: name.into_boxed_str(),
      content: content.into_boxed_str(),
    })
}
#[test]
fn test_macro_invocation() {
  let p = macro_invocation().then_ignore(end());
  //
  let MacroInvocation { name, content, span } = p.parse("bit!(7)").unwrap();
  assert_eq!(name, box_str!("bit"));
  assert_eq!(content, box_str!("7"));
  drop(span);
  //
  let MacroInvocation { name, content, span } =
    p.parse(r#"gfx!(".X", ..XX..X.)"#).unwrap();
  assert_eq!(name, box_str!("gfx"));
  assert_eq!(content, box_str!(r#"".X", ..XX..X."#));
  drop(span);
}
