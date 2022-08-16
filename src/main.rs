use dmgrs::{
  lexer::Lexeme,
  parser::{
    multi_line_comments::MultiLineCommentFilter,
    repeated_newline_filter::RepeatedNewlineFilter,
  },
  parser2::parser,
};
use logos::Logos;
use chumsky::{Stream, Parser as _};

const SRC: &str = include_str!("../samples/hello_world.s");

fn main() {
  // for (lex, _span) in RepeatedNewlineFilter::new(MultiLineCommentFilter::new(
  //   Lexeme::lexer(SRC).spanned(),
  // )) {
  //   //print!("{lex:?} ");
  //   print!("TOKEN {lex} {lex:?}");
  // }

  let lexer = RepeatedNewlineFilter::new(MultiLineCommentFilter::new(Lexeme::lexer(SRC).spanned()));

  let mut stream = Stream::from_iter(SRC.len()..SRC.len(), lexer);

  println!("Tokens = {:?}", stream.fetch_tokens().collect::<Vec<_>>());

  println!("{:#?}", parser().parse(stream));
}
