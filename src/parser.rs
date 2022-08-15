//! Parses stuff, mostly with the [chumsky] crate.

use std::ops::Range;

#[allow(unused)]
use chumsky::{
  prelude::Simple,
  primitive::{end, filter, just},
  Parser,
};
use chumsky::{primitive::none_of, recursive::recursive};

use crate::{
  lexer::{DmgToken, DmgToken::*},
  StaticStr,
};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct ProgramAttr {
  span: Range<usize>,
  tokens: Vec<DmgToken>,
}
impl ProgramAttr {
  fn parser() -> impl Parser<DmgToken, Self, Error = Simple<DmgToken>> {
    just(Punct('#')).ignore_then(just(Punct('!'))).ignore_then(
      filter(|t: &DmgToken| *t != Punct(']'))
        .repeated()
        .collect::<Vec<_>>()
        .delimited_by(just(Punct('[')), just(Punct(']')))
        .map_with_span(|tokens, span| ProgramAttr { span, tokens }),
    )
  }
}
#[test]
fn test_ProgramAttr_parser() {
  let p = ProgramAttr::parser().then_ignore(end());
  assert_eq!(
    p.parse(vec![Punct('#'), Punct('!'), Punct('['), Punct(']'),])
      .unwrap()
      .tokens,
    vec![]
  );
  assert_eq!(
    p.parse(
      vec![Punct('#'), Punct('!'), Punct('['), Str("hello"), Punct(']'),]
    )
    .unwrap()
    .tokens,
    vec![Str("hello")]
  );
}

struct MultiLineComment;
impl MultiLineComment {
  fn parser() -> impl Parser<DmgToken, (), Error = Simple<DmgToken>> {
    recursive(|multiline_comment| {
      none_of([StartMultiComment, EndMultiComment])
        .ignored()
        .or(multiline_comment)
        .ignored()
        .repeated()
        .ignored()
        .delimited_by(just(StartMultiComment), just(EndMultiComment))
        .ignored()
    })
  }
}
#[test]
fn test_MultiLineComment_parser() {
  let p = MultiLineComment::parser().then_ignore(end());
  p.parse(vec![
    StartMultiComment,
    Punct('#'),
    Punct('!'),
    StartMultiComment,
    Punct('['),
    EndMultiComment,
    Punct(']'),
    EndMultiComment,
  ])
  .unwrap();
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct ConstDecl {
  span: Range<usize>,
  label: StaticStr,
  value: u16,
}
impl ConstDecl {
  // TODO: this should eventually handle at least simple constant integer
  // expressions, right now it can only take a single number.
  fn parser() -> impl Parser<DmgToken, Self, Error = Simple<DmgToken>> {
    just(Ident("const"))
      .ignore_then(filter(DmgToken::is_ident).map(DmgToken::unwrap_ident))
      .then_ignore(just(Punct('=')))
      .then(filter(DmgToken::is_number).map(DmgToken::unwrap_number))
      .then_ignore(just(Punct(';')))
      .map_with_span(|(label, value), span| ConstDecl { span, label, value })
  }
}
#[test]
fn test_ConstDecl_parser() {
  let p = ConstDecl::parser().then_ignore(end());
  let decl = p
    .parse(vec![
      Ident("const"),
      Ident("NR52"),
      Punct('='),
      HexLiteral(0xFF26),
      Punct(';'),
    ])
    .unwrap();
  assert_eq!(decl.label, "NR52");
  assert_eq!(decl.value, 0xFF26);
}

// TODO: directive!(patterns)
