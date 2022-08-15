#![allow(unused_imports)]
#![allow(missing_docs)]

//! Parses stuff, mostly with the [chumsky] crate.

use std::ops::Range;

use chumsky::{
  prelude::Simple,
  primitive::{choice, end, filter, just, none_of, take_until, todo},
  recursive::recursive,
  Parser,
};

use crate::{
  lexer::{Lexeme, Lexeme::*},
  StaticStr,
};

use self::{
  const_decl::AstConstDecl, program_attr::AstProgramAttr,
  static_decl::AstStaticDecl,
};

pub mod const_decl;
pub mod fn_decl;
pub mod instruction;
pub mod multi_line_comments;
pub mod program_attr;
pub mod repeated_newline_filter;
pub mod static_decl;

pub enum AstTopLevelItem {
  ProgramAttr(AstProgramAttr),
  ConstDecl(AstConstDecl),
  StaticDecl(AstStaticDecl),
}
