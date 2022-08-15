use super::*;

/// A const declaration is like `const NAME = EXPR;`
///
/// A const makes a named alias for a value during compilation, but does not
/// produce output in the ROM.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstConstDecl {
  pub span: Range<usize>,
  pub label: StaticStr,
  pub expr: Vec<Lexeme>,
}
impl AstConstDecl {
  pub fn parser() -> impl Parser<Lexeme, Self, Error = Simple<Lexeme>> {
    just(Ident("const"))
      .ignore_then(filter(Lexeme::is_ident).map(Lexeme::unwrap_ident))
      .then_ignore(just(Punct('=')))
      .then(take_until(just(Punct(';'))).map(|(lexemes, _end)| lexemes))
      .map_with_span(|(label, expr), span| AstConstDecl { span, label, expr })
  }
}
#[test]
fn test_ConstDecl_parser() {
  let p = AstConstDecl::parser().then_ignore(end());
  //
  let c = p
    .parse(vec![
      Ident("const"),
      Ident("LCDC"),
      Punct('='),
      HexLiteral(0xFF40),
      Punct(';'),
    ])
    .unwrap();
  assert_eq!(c.label, "LCDC");
  assert_eq!(c.expr, vec![HexLiteral(0xFF40)]);
  //
  let c = p
    .parse(vec![
      Ident("const"),
      Ident("LCDC_LCD_ON"),
      Punct('='),
      Ident("bit"),
      Punct('!'),
      Punct('('),
      DecimalLiteral(7),
      Punct(')'),
      Punct(';'),
    ])
    .unwrap();
  assert_eq!(c.label, "LCDC_LCD_ON");
  assert_eq!(
    c.expr,
    vec![Ident("bit"), Punct('!'), Punct('('), DecimalLiteral(7), Punct(')'),]
  );
}
