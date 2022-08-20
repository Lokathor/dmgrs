use chumsky::{Parser, Stream};
use dmgrs::{easy_lexer::EasyLexer, parser::Ast};

const SRC: &str = include_str!("../samples/minimum-program.s");

fn main() {
  let lexer = EasyLexer::new(SRC);

  let stream = Stream::from_iter(SRC.len()..SRC.len(), lexer);

  let ast = Ast::parser().parse(stream).unwrap();

  for item in ast.items.iter() {
    println!("Item: {item:?}");
  }
}
