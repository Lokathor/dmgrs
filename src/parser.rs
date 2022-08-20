//! The "parser" stage turns the stream of lexemes (from the lexer) into ab
//! "abstract syntax tree".
//!
//! There is *some* validation when going from lexeme stream to AST, but not all
//! of it.

use crate::{lexer::Lexeme, StaticStr};
use chumsky::prelude::*;
use std::ops::{Deref, DerefMut, Range};

/// Wraps a span around any other type.
///
/// The contained value is accessable with [`Deref`] and [`DerefMut`].
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Spanned<T> {
  t: T,
  range_start: usize,
  range_end: usize,
}
impl<T> Spanned<T> {
  /// Constructs the spanned value
  #[must_use]
  pub const fn new(t: T, range: Range<usize>) -> Self {
    Self { t, range_start: range.start, range_end: range.end }
  }
  /// Gets the span in the form of a [`Range<usize>`]
  #[must_use]
  pub const fn get_span_range(&self) -> Range<usize> {
    self.range_start..self.range_end
  }
}
impl<T> From<(T, Range<usize>)> for Spanned<T> {
  #[must_use]
  fn from((t, range): (T, Range<usize>)) -> Self {
    Self::new(t, range)
  }
}
impl<T> Deref for Spanned<T> {
  type Target = T;
  #[must_use]
  fn deref(&self) -> &Self::Target {
    &self.t
  }
}
impl<T> DerefMut for Spanned<T> {
  #[must_use]
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.t
  }
}

/// Attributes for the program or for a specific `fn` or `static`.
///
/// * Program attributes are written using `#![words here]`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Attribute {
  /// Call attributes are like `foo(bar)`, `foo()`, or even `foo`
  ///
  /// The current parser can't distingush between `foo()` (with parens) and
  /// `foo` (no parens). This is suspected to not ever be important.
  Call(Spanned<StaticStr>, Vec<Attribute>),
  /// Eq attributes are like `foo = "bar"`.
  Eq(Spanned<StaticStr>, Spanned<StaticStr>),
}
impl Attribute {
  /// Parses lexeme streams into Attributes.
  pub fn parser() -> impl Parser<Lexeme, Self, Error = Simple<Lexeme>> {
    // Parses the internals of an attribute (i.e: in `#![mbc(rom_only)]`, it
    // parses the `mbc(rom_only)`)
    let attr_inner = recursive(|attr_inner| {
      // Parses `foo = "bar"`
      let eq_attr = ident_parser()
        .then_ignore(just(Lexeme::Punct('=')))
        .then(string_parser())
        .map(|(lhs, rhs)| Attribute::Eq(lhs, rhs));

      // Parses `foo` and `foo(...)`
      let call_attr = ident_parser()
        .then(
          attr_inner
            .separated_by(just(Lexeme::Punct(',')))
            .allow_trailing()
            .delimited_by(just(Lexeme::Punct('(')), just(Lexeme::Punct(')')))
            .or_not(),
        )
        .map(|(name, args)| Attribute::Call(name, args.unwrap_or_default()));

      eq_attr.or(call_attr)
    });

    // Parses attributes like `#![...]`
    just(Lexeme::Punct('#')).then(just(Lexeme::Punct('!'))).ignore_then(
      attr_inner
        .delimited_by(just(Lexeme::Punct('[')), just(Lexeme::Punct(']'))),
    )
  }
}

/// An "expression" means an integer constant expression.
///
/// They can assigned to a `const`, or used inline in an instruction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
  /// An actual numeric value, without anything more to look up or evaluate.
  Num(Spanned<u16>),
  /// The name of a `const`
  Ident(Spanned<StaticStr>),
  /// Bitwise OR operation, left and right hand sides.
  BitOr(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
  /// `bit![x]`, an arguably more readable way to write `1 << x`
  DirectiveBit(Box<Spanned<Expr>>),
  /// `size_of_val![ident]` gives the size (in bytes) of a named `fn` or
  /// `static`
  DirectiveSizeOfVal(Spanned<StaticStr>),
}
impl Expr {
  /// Parses any expression, which means it parses a heck of a lot.
  pub fn parser() -> impl Parser<Lexeme, Expr, Error = Simple<Lexeme>> {
    recursive(|expr| {
      let literal = select! {
        Lexeme::HexLiteral(x) => x,
        Lexeme::DecimalLiteral(x) => x,
        Lexeme::BinaryLiteral(x) => x,
      }
      .map_with_span(|x, span| Expr::Num(Spanned::new(x, span)));

      let constant = select! { Lexeme::Ident(s) => s }
        .map_with_span(|s, span| Expr::Ident(Spanned::new(s, span)));

      let bit_macro = just(Lexeme::Ident("bit"))
        .then_ignore(just(Lexeme::Punct('!')))
        .ignore_then(
          expr
            .clone()
            .map_with_span(|expr, span| (expr, span))
            .delimited_by(just(Lexeme::Punct('[')), just(Lexeme::Punct(']'))),
        )
        .map(|(x, span)| Expr::DirectiveBit(Box::new(Spanned::new(x, span))));

      let size_of_val_macro = just(Lexeme::Ident("size_of_val"))
        .then_ignore(just(Lexeme::Punct('!')))
        .ignore_then(
          select! { Lexeme::Ident(s) => s }
            .delimited_by(just(Lexeme::Punct('[')), just(Lexeme::Punct(']'))),
        )
        .map_with_span(|s, span| {
          Expr::DirectiveSizeOfVal(Spanned::new(s, span))
        });

      let expr_macro = bit_macro.or(size_of_val_macro);

      let atom = literal
        .or(expr_macro)
        .or(constant)
        .map_with_span(|expr, span: Range<usize>| (expr, span));

      let bitor = atom
        .clone()
        .then(just(Lexeme::Punct('|')).ignore_then(atom).repeated())
        .foldl(|(lhs, lhs_span), (rhs, rhs_span)| {
          let span = lhs_span.start..rhs_span.end;
          (
            Expr::BitOr(
              Box::new(Spanned::new(lhs, lhs_span)),
              Box::new(Spanned::new(rhs, rhs_span)),
            ),
            span,
          )
        });

      bitor.map(|(expr, _span)| expr)
    })
  }
}

/// A constant declaration: `const FOO = EXPR;`
///
/// A `const` exists only at compile time, they do not appear at any fixed
/// location into the ROM.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Const {
  /// The name of the `const`, which other code can now refer to.
  pub name: Spanned<StaticStr>,
  /// The expression that this `const` is equivalent to.
  pub expr: Spanned<Expr>,
}
impl Const {
  /// Parses for a `const` declaration.
  pub fn parser() -> impl Parser<Lexeme, Const, Error = Simple<Lexeme>> {
    just(Lexeme::KwConst)
      .ignore_then(ident_parser())
      .then_ignore(just(Lexeme::Punct('=')))
      .then(Expr::parser().map_with_span(|expr, span| (expr, span)))
      .then_ignore(just(Lexeme::Punct(';')))
      .map(|(name, expr_span)| Const { name, expr: expr_span.into() })
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StaticType {
  ByteSlice,
}

/// A static declaration: `static FOO: [u8] = [EXPR, EXPR, EXPR, ...];`
///
/// A `static` is a series of bytes that end up in the ROM, which can be copied
/// into VRAM or similar. In assembler terms, every `static` is its own section
/// which can be placed during the linking phase.
///
/// **Note:** At the moment, only a literal slice of bytes is allowed, but in
/// the future it's planned that other static expressions will become possible.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Static {
  /// The name of this `static`
  pub name: Spanned<StaticStr>,
  /// The intended type of the static.
  pub type_: Spanned<StaticType>,
  /// The list of byte expressions in the static slice.
  pub items: Vec<Spanned<Expr>>,
}
impl Static {
  /// Parser for a `static` declaration.
  pub fn parser() -> impl Parser<Lexeme, Static, Error = Simple<Lexeme>> {
    let eols = just(Lexeme::EndOfLine).ignored().repeated();

    let byte_slice = just(Lexeme::Punct('['))
      .then(just(Lexeme::Ident("u8")))
      .then(just(Lexeme::Punct(']')))
      .to(StaticType::ByteSlice);

    let ty = byte_slice.map_with_span(|t, span| Spanned::new(t, span));

    // TODO: Allow more than just arrays of expressions
    let static_expr = Expr::parser()
      .map_with_span(|expr, span| Spanned::new(expr, span))
      .padded_by(eols)
      .separated_by(just(Lexeme::Punct(',')))
      .allow_trailing()
      .padded_by(eols)
      .delimited_by(just(Lexeme::Punct('[')), just(Lexeme::Punct(']')));

    just(Lexeme::KwStatic)
      .ignore_then(ident_parser())
      .then_ignore(just(Lexeme::Punct(':')))
      .then(ty)
      .then_ignore(just(Lexeme::Punct('=')))
      .then(static_expr)
      .then_ignore(just(Lexeme::Punct(';')))
      .map(|((name, type_), items)| Static { name, type_, items })
  }
}

/// Where to branch the loop to
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BranchTgt {
  /// Continue looping, branch to the start of the loop
  Continue,
  /// Break the loop, branch to the end of the loop
  Break,
}

/// An argument to an instruction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstrArg {
  /// A register name.
  Reg(StaticStr),
  /// A register dereference, with optional post-op offset.
  Deref(Expr, isize),
  /// An expression.
  Expr(Expr),
}

/// A single statement within a block.
///
/// **Note:** This doesn't currently have support for `if` or `if-else`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
  /// A single CPU instruction.
  Instr(Spanned<StaticStr>, Vec<Spanned<InstrArg>>),
  /// A conditional branch to the start or end of the loop.
  ConditionalBranch(Spanned<StaticStr>, Spanned<BranchTgt>),
  /// An unconditional branch to the start of end of the loop.
  AlwaysBranch(Spanned<BranchTgt>),
  /// A loop over 0 or more statements.
  Loop(Box<Spanned<Block>>),
}

/// A block of statements
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
  /// The list of statements in the block
  pub stmts: Vec<Stmt>,
}

/// A function in the program.
///
/// Because this is assembly programming, functions don't take or return args.
/// When you `call` a function the return address is pushed to the stack, and
/// when you return it's popped from the stack. That's about the extent of the
/// support that the compiler and CPU give you.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fn {
  /// The name of the function
  pub name: Spanned<StaticStr>,
  /// The block of statements for the function
  pub block: Spanned<Block>,
}
impl Fn {
  /// Parser for individual functions.
  pub fn parser() -> impl Parser<Lexeme, Fn, Error = Simple<Lexeme>> {
    let eols = just(Lexeme::EndOfLine).ignored().repeated();

    let block = recursive(|block| {
      let loop_parser = just(Lexeme::KwLoop)
        .ignore_then(block)
        .map(|block| Stmt::Loop(Box::new(block)));

      const REGS: &[&str] = &[
        "a", "b", "c", "d", "e", "h", "l", "sp", "pc", "af", "bc", "de", "hl",
      ];
      let instr_arg =
              // Register args: `a`
              select! { Lexeme::Ident(s) if REGS.contains(&s) => InstrArg::Reg(s) }
              // Expression args: `FOO + BAR`
              .or(Expr::parser().map(InstrArg::Expr))
              // Deref args: `[FOO]`, `[BAR-]`, `[BAZ++]`
              .or(Expr::parser()
                  .then(just(Lexeme::Punct('+')).to(1)
                      .or(just(Lexeme::Punct('-')).to(-1))
                      .repeated())
                  .map(|(ptr, shift)| InstrArg::Deref(ptr, shift.into_iter().sum()))
                  .delimited_by(just(Lexeme::Punct('[')), just(Lexeme::Punct(']'))))
              .map_with_span(|arg, span| Spanned::new(arg, span));

      let instr = ident_parser()
        .then(instr_arg.separated_by(just(Lexeme::Punct(','))))
        .map(|(name, args)| Stmt::Instr(name, args));

      let branch_if = just(Lexeme::KwIf)
        .ignore_then(ident_parser())
        .then_ignore(just(Lexeme::Punct(',')))
        .then(
          ((just(Lexeme::KwContinue).to(BranchTgt::Continue))
            .or(just(Lexeme::KwBreak).to(BranchTgt::Break)))
          .map_with_span(|tgt, span| (tgt, span)),
        )
        .map(|(condition, tgt)| Stmt::ConditionalBranch(condition, tgt.into()));

      let branch_always = just(Lexeme::KwBreak)
        .to(BranchTgt::Break)
        .or(just(Lexeme::KwContinue).to(BranchTgt::Continue))
        .map_with_span(|tgt, span| Stmt::AlwaysBranch((tgt, span).into()));

      let branch = branch_if.or(branch_always);

      let stmt = loop_parser.or(instr).or(branch);

      stmt
        .separated_by(eols.at_least(1))
        .allow_leading()
        .allow_trailing()
        .delimited_by(just(Lexeme::Punct('{')), just(Lexeme::Punct('}')))
        .map_with_span(|stmts, span| Spanned::new(Block { stmts }, span))
    });

    just(Lexeme::KwFn)
      .ignore_then(ident_parser())
      .then(block)
      .map(|(name, block)| Fn { name, block })
  }
}

/// An item is anything that can appear at the top level of a source file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
  /// Program attributes
  Attr(Attribute),
  /// Constants
  Const(Const),
  /// Functions
  Fn(Fn),
  /// Static data
  Static(Static),
}

/// The Abstract Syntax Tree of a Dmgrs program.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ast {
  /// The list of items in the program.
  pub items: Vec<Spanned<Item>>,
}
impl Ast {
  /// The parser for the AST.
  pub fn parser() -> impl Parser<Lexeme, Ast, Error = Simple<Lexeme>> {
    let eols = just(Lexeme::EndOfLine).ignored().repeated();

    let item = choice((
      Attribute::parser().map(Item::Attr),
      Const::parser().map(Item::Const),
      Fn::parser().map(Item::Fn),
      Static::parser().map(Item::Static),
    ));

    item
      .map_with_span(|item, span| Spanned::new(item, span))
      .padded_by(eols)
      .repeated()
      .map(|items| Ast { items })
      .then_ignore(end())
  }
}

fn ident_parser(
) -> impl Parser<Lexeme, Spanned<StaticStr>, Error = Simple<Lexeme>> {
  select! { Lexeme::Ident(s) => s }
    .map_with_span(|name, span| Spanned::new(name, span))
}

fn string_parser(
) -> impl Parser<Lexeme, Spanned<StaticStr>, Error = Simple<Lexeme>> {
  select! { Lexeme::Str(s) => s }
    .map_with_span(|name, span| Spanned::new(name, span))
}
