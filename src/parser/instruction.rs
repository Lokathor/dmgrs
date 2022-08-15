use chumsky::primitive::one_of;

use super::*;

pub struct AstInstruction {
  span: Range<usize>,
  tokens: Vec<Lexeme>,
}
impl AstInstruction {
  pub fn parser() -> impl Parser<Lexeme, Self, Error = Simple<Lexeme>> {
    todo()
  }
}
