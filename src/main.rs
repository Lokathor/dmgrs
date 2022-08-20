use chumsky::{Parser, Stream};
use dmgrs::{easy_lexer::EasyLexer, parser::Ast};

const SRC: &str = include_str!("../samples/minimum-program.s");

fn main() {
  println!("EasyLexer: {} lexemes found.", EasyLexer::new(SRC).count());
  for (lex, _r) in EasyLexer::new(SRC) {
    print!("{lex:?} ");
  }

  let lexer = EasyLexer::new(SRC);

  let stream = Stream::from_iter(SRC.len()..SRC.len(), lexer);

  println!("{:?}", Ast::parser().parse(stream).unwrap());
}
