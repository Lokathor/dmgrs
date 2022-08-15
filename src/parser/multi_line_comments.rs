//! A multi-line comment is any amount of stuff between `/*` and `*/`, nesting
//! allowed.
//!
//! Reminder: end-of-line comments (`//` to end of line) strip content during
//! the initial lexing phase, so `//` can prevent multi-line openings and
//! endings from showing up in the lexeme stream.

use super::*;

/// This iterator adapter will remove all multi-line comments from the lexeme
/// stream.
pub struct MultiLineCommentFilter<I: Iterator<Item = (Lexeme, Range<usize>)>> {
  i: I,
}
impl<I: Iterator<Item = (Lexeme, Range<usize>)>> MultiLineCommentFilter<I> {
  /// Makes a new iterator.
  pub const fn new(i: I) -> Self {
    Self { i }
  }
}
impl<I: Iterator<Item = (Lexeme, Range<usize>)>> core::iter::Iterator
  for MultiLineCommentFilter<I>
{
  type Item = (Lexeme, Range<usize>);
  fn next(&mut self) -> Option<Self::Item> {
    let mut multi_comment_level = 0;
    loop {
      match self.i.next()? {
        (StartMultiComment, _span) => {
          multi_comment_level += 1;
        }
        (EndMultiComment, _span) => {
          multi_comment_level -= 1;
        }
        (lex, span) if multi_comment_level == 0 => {
          return Some((lex, span));
        }
        (_lex, _span) => {}
      }
    }
  }
}

#[test]
fn test_MultiLineCommentFilter() {
  let mut iter = MultiLineCommentFilter::new(
    vec![
      Str("hello"),
      StartMultiComment,
      Punct('#'),
      Punct('!'),
      StartMultiComment,
      Punct('['),
      EndMultiComment,
      Punct(']'),
      EndMultiComment,
      Str("world"),
    ]
    .into_iter()
    .zip(core::iter::repeat(0..0)),
  );
  assert_eq!(iter.next(), Some((Str("hello"), 0..0)));
  assert_eq!(iter.next(), Some((Str("world"), 0..0)));
  assert_eq!(iter.next(), None);
}
