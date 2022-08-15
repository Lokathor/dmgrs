use super::*;

pub struct AstFnDecl {
  pub span: Range<usize>,
  pub name: StaticStr,
  pub interior: Vec<Lexeme>,
}

// TODO
