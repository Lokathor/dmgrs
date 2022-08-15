use super::*;

/// A program attribute is like `#![words here]`
///
/// The significant part is the potion *between* the square brackets.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct AstProgramAttr {
  pub span: Range<usize>,
  pub tokens: Vec<Lexeme>,
}
impl AstProgramAttr {
  pub fn parser() -> impl Parser<Lexeme, Self, Error = Simple<Lexeme>> {
    just(Punct('#'))
      .ignore_then(just(Punct('!')))
      .ignore_then(
        filter(|t: &Lexeme| *t != Punct(']'))
          .repeated()
          .collect::<Vec<_>>()
          .delimited_by(just(Punct('[')), just(Punct(']'))),
      )
      .map_with_span(|tokens, span| AstProgramAttr { span, tokens })
      .labelled("Program Attribute")
  }
}

#[test]
fn test_AstProgramAttr_parser() {
  let p = AstProgramAttr::parser().then_ignore(end());
  assert_eq!(
    p.parse(vec![Punct('#'), Punct('!'), Punct('['), Punct(']'),])
      .unwrap()
      .tokens,
    vec![]
  );
  assert_eq!(
    p.parse(vec![
      Punct('#'),
      Punct('!'),
      Punct('['),
      Str("mbc"),
      Punct('('),
      Str("rom_only"),
      Punct(')'),
      Punct(']'),
    ])
    .unwrap()
    .tokens,
    vec![Str("mbc"), Punct('('), Str("rom_only"), Punct(')'),]
  );
  assert_eq!(
    p.parse(vec![
      Punct('#'),
      Punct('!'),
      Punct('['),
      Ident("title"),
      Punct('='),
      Str("demo title"),
      Punct(']'),
    ])
    .unwrap()
    .tokens,
    vec![Ident("title"), Punct('='), Str("demo title")]
  );
}
