//#![warn(missing_docs)]
#![cfg_attr(test, allow(nonstandard_style))]
#![allow(unused_parens)]

//! Stuff to work with Dmgrs assembly files.

pub mod easy_lexer;
pub mod header;
pub mod lexer;
pub mod multi_line_comments;
pub mod parser;
pub mod repeated_newline_filter;
pub mod section;
pub mod str_cache_impl;

type StaticStr = &'static str;

/// Generates a `usize` id value, used for sticking on the end of made up
/// labels.
pub fn next_id() -> usize {
  use std::sync::atomic::AtomicUsize;
  static GLOBAL_COUNTER: AtomicUsize = AtomicUsize::new(0);
  GLOBAL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::AcqRel)
}
