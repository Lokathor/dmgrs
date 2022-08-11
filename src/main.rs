use chumsky::{prelude::*, text::whitespace};

fn main() {
  println!("{:?}", parser().then_ignore(end()).parse(SRC));
}

const SRC: &str = "
#![mbc(none)]
#![ram(none)]
#![entry(EntryPoint)]

// Using the above attributes, the correct header would
// be generated automatically. We don't have to fiddle
// with adding the correct amount of space and that stuff.

const ZERO = 0; // a literal alias uses rust-like syntax
";

fn parser() -> impl Parser<char, Vec<String>, Error = Simple<char>> {
  file_attr_parser().padded_by(whitespace()).repeated().collect::<Vec<String>>()
}

/// Parses `#![mbc(none)]`, keeping the part inside the square brackets.
fn file_attr_parser() -> impl Parser<char, String, Error = Simple<char>> {
  just('#')
    .ignored()
    .then(just('!').ignored())
    .then(
      filter(|c: &char| *c != ']')
        .repeated()
        .collect::<String>()
        .delimited_by(just('['), just(']')),
    )
    .map(|(((), ()), string)| string)
}
