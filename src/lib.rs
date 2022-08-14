#![warn(missing_docs)]
#![allow(dead_code)]

//! Stuff to work with Dmgrs assembly files.

#[allow(unused)]
macro_rules! box_str {
  ($x:expr) => {
    $x.to_string().into_boxed_str()
  };
}

pub mod lexer;
pub mod parser;
