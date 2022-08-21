#![allow(nonstandard_style)]

use dmgrs::{easy_lexer::EasyLexer, lexer::Lexeme};

#[test]
fn test_EasyLexer1() {
  let src = r#"fn main {
    loop0:
      jr loop0
  }"#;
  let expected = [
    Lexeme::KwFn,
    Lexeme::Ident("main"),
    Lexeme::Punct('{'),
    Lexeme::EndOfLine,
    Lexeme::Ident("loop0"),
    Lexeme::Punct(':'),
    Lexeme::EndOfLine,
    Lexeme::Ident("jr"),
    Lexeme::Ident("loop0"),
    Lexeme::EndOfLine,
    Lexeme::Punct('}'),
  ];
  let actual = EasyLexer::new(src).map(|(l, _r)| l).collect::<Vec<_>>();
  assert_eq!(expected.as_slice(), actual.as_slice());
}
