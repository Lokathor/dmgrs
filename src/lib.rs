#![warn(missing_docs)]
#![cfg_attr(test, allow(nonstandard_style))]

//! Stuff to work with Dmgrs assembly files.

#[allow(unused)]
macro_rules! box_str {
  ($x:expr) => {
    $x.to_string().into_boxed_str()
  };
}

pub mod easy_lexer;
pub mod lexer;
pub mod multi_line_comments;
pub mod parser;
pub mod repeated_newline_filter;
pub mod str_cache_impl;

type StaticStr = &'static str;
