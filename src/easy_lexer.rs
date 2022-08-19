//! Provides an easy to use lexer

use std::ops::Range;

use logos::{Logos, SpannedIter};

use crate::{
  lexer::Lexeme, multi_line_comments::MultiLineCommentFilter,
  repeated_newline_filter::RepeatedNewlineFilter,
};

/// This wraps the basic "spanned" lexeme iterator provided by the `logos` crate
/// with adapters to cleanup the lexeme stream.
pub struct EasyLexer<'a> {
  iter: RepeatedNewlineFilter<MultiLineCommentFilter<SpannedIter<'a, Lexeme>>>,
}
impl<'a> EasyLexer<'a> {
  /// Makes a new iterator.
  pub fn new(str: &'a str) -> Self {
    Self {
      iter: RepeatedNewlineFilter::new(MultiLineCommentFilter::new(
        Lexeme::lexer(str).spanned(),
      )),
    }
  }
}
impl<'a> Iterator for EasyLexer<'a> {
  type Item = (Lexeme, Range<usize>);
  fn next(&mut self) -> Option<Self::Item> {
    self.iter.next()
  }
}
