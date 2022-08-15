use super::*;

/// A static declaration is like `static NAME: TYPE = EXPR;`
///
/// A static puts data into the ROM.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstStaticDecl {
  pub span: Range<usize>,
  pub name: StaticStr,
  pub ty_expr: Vec<Lexeme>,
  pub content_expr: Vec<Lexeme>,
}
impl AstStaticDecl {
  pub fn parser() -> impl Parser<Lexeme, Self, Error = Simple<Lexeme>> {
    just(Ident("static"))
      .ignore_then(filter(Lexeme::is_ident).map(Lexeme::unwrap_ident))
      .then_ignore(just(Punct(':')))
      .then(take_until(just(Punct('='))).map(|(ty_expr, _end)| ty_expr))
      .then(
        take_until(just(Punct(';'))).map(|(content_expr, _end)| content_expr),
      )
      .map_with_span(|((name, ty_expr), content_expr), span| AstStaticDecl {
        span,
        name,
        ty_expr,
        content_expr,
      })
  }
}

#[test]
fn test_AstStaticDecl_parser() {
  let p = AstStaticDecl::parser().then_ignore(end());
  //
  let c = p
    .parse(vec![
      Ident("static"),
      Ident("Tiles"),
      Punct(':'),
      Punct('['),
      Ident("u8"),
      Punct(']'),
      Punct('='),
      Punct('['),
      EndOfLine,
      HexLiteral(0x0),
      Punct(','),
      HexLiteral(0xFF),
      Punct(','),
      HexLiteral(0x0),
      Punct(']'),
      Punct(';'),
    ])
    .unwrap();
  assert_eq!(c.name, "Tiles");
  assert_eq!(c.ty_expr, vec![Punct('['), Ident("u8"), Punct(']'),]);
  assert_eq!(
    c.content_expr,
    vec![
      Punct('['),
      EndOfLine,
      HexLiteral(0x0),
      Punct(','),
      HexLiteral(0xFF),
      Punct(','),
      HexLiteral(0x0),
      Punct(']'),
    ]
  );
}
