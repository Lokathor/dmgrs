//! Repeated newlines are not needed, it's better to handle them early.

use std::iter::Peekable;

use super::*;

/// This iterator adapter will collapse all repeated newlines into a single end
/// of line.
pub struct RepeatedNewlineFilter<I: Iterator<Item = (Lexeme, Range<usize>)>> {
  i: Peekable<I>,
}
impl<I: Iterator<Item = (Lexeme, Range<usize>)>> RepeatedNewlineFilter<I> {
  /// Makes a new iterator.
  pub fn new(i: I) -> Self {
    Self { i: i.peekable() }
  }
}
impl<I: Iterator<Item = (Lexeme, Range<usize>)>> core::iter::Iterator
  for RepeatedNewlineFilter<I>
{
  type Item = (Lexeme, Range<usize>);
  fn next(&mut self) -> Option<Self::Item> {
    match self.i.next()? {
      (EndOfLine, span) => {
        while let Some((EndOfLine, _span)) = self.i.peek() {
          self.i.next()?;
        }
        Some((EndOfLine, span))
      }
      other => Some(other),
    }
  }
}

#[test]
fn test_RepeatedNewlineFilter() {
  let mut iter = RepeatedNewlineFilter::new(
    vec![Str("hello"), EndOfLine, EndOfLine, EndOfLine, Str("world")]
      .into_iter()
      .zip(core::iter::repeat(0..0)),
  );
  assert_eq!(iter.next(), Some((Str("hello"), 0..0)));
  assert_eq!(iter.next(), Some((EndOfLine, 0..0)));
  assert_eq!(iter.next(), Some((Str("world"), 0..0)));
  assert_eq!(iter.next(), None);
}
