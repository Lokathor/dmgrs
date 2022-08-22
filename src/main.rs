use chumsky::{Parser, Stream};
use dmgrs::{easy_lexer::EasyLexer, header::Header, parser::Ast};

const SRC: &str = include_str!("../samples/minimum-program.s");

fn main() {
  let lexer = EasyLexer::new(SRC);

  let stream = Stream::from_iter(SRC.len()..SRC.len(), lexer);

  let ast = Ast::parser().parse(stream).unwrap();

  for item in ast.items.iter() {
    println!("Item: {item:?}");
  }

  println!("Default Header: {:#?}", Header::default());

  let mut v = vec![0_u8; 0x100];
  v.extend(Header::default().as_bytes());
  v[0] = 0x18;
  v[1] = (-2_i8) as u8;
  std::fs::write("target/demo.gb", &v).unwrap();
}
