use dmgrs::{
  lexer::Lexeme,
  parser::{
    multi_line_comments::MultiLineCommentFilter,
    repeated_newline_filter::RepeatedNewlineFilter,
  },
};
use logos::Logos;

const SRC: &str = include_str!("../samples/hello_world.s");

fn main() {
  for (lex, _span) in RepeatedNewlineFilter::new(MultiLineCommentFilter::new(
    Lexeme::lexer(SRC).spanned(),
  )) {
    //print!("{lex:?} ");
    print!("{lex} ");
  }
}
