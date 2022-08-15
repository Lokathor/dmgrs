#![warn(missing_docs)]
#![allow(dead_code)]
#![cfg_attr(test, allow(nonstandard_style))]

//! Stuff to work with Dmgrs assembly files.

#[allow(unused)]
macro_rules! box_str {
  ($x:expr) => {
    $x.to_string().into_boxed_str()
  };
}

pub mod lexer;
pub mod parser;
pub mod str_cache_impl;

type StaticStr = &'static str;
