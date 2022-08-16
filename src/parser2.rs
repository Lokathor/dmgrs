use chumsky::prelude::*;
use crate::{lexer::Lexeme, StaticStr};
use std::ops::Range;

type Spanned<T> = (T, Range<usize>);

// TODO: Generalise this?
#[derive(Debug)]
pub enum Attr {
    Call(Spanned<StaticStr>, Vec<Attr>),
    Eq(Spanned<StaticStr>, Spanned<StaticStr>),
}

#[derive(Debug)]
pub enum Expr {
    Num(u16),
    Const(StaticStr),
    BitOr(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    BitMacro(Box<Spanned<Expr>>),
    SizeOfValMacro(Box<Spanned<Expr>>),
}

#[derive(Debug)]
pub struct Const {
    pub name: Spanned<StaticStr>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug)]
pub struct Static {
    pub name: Spanned<StaticStr>,
    pub items: Vec<Spanned<Expr>>,
}

#[derive(Clone, Debug)]
pub enum FnRet {
    Nothing,
    Never,
    Irq,
}

#[derive(Clone, Debug)]
pub enum BranchTgt {
    Continue,
}

#[derive(Debug)]
pub enum InstrArg {
    Reg(StaticStr),
    Deref(Expr, isize),
    Expr(Expr),
}

#[derive(Debug)]
pub enum Stmt {
    Loop(Box<Spanned<Block>>),
    Branch(Spanned<StaticStr>, Spanned<BranchTgt>),
    Instr(Spanned<StaticStr>, Vec<Spanned<InstrArg>>),
}

#[derive(Debug)]
pub struct Block {
    stmts: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Fn {
    pub name: Spanned<StaticStr>,
    pub ret: FnRet,
    pub block: Spanned<Block>,
}

#[derive(Debug)]
pub enum Item {
    Attr(Attr),
    Const(Const),
    Fn(Fn),
    Static(Static),
}

#[derive(Debug)]
pub struct Ast {
    pub items: Vec<Spanned<Item>>,
}

fn ident_parser() -> impl Parser<Lexeme, Spanned<StaticStr>, Error = Simple<Lexeme>> {
    select! { Lexeme::Ident(s) => s }
        .map_with_span(|name, span| (name, span))
}

fn string_parser() -> impl Parser<Lexeme, Spanned<StaticStr>, Error = Simple<Lexeme>> {
    select! { Lexeme::Str(s) => s }
        .map_with_span(|name, span| (name, span))
}

fn attr_parser() -> impl Parser<Lexeme, Attr, Error = Simple<Lexeme>> {
    // Parses the internals of an attribute (i.e: in `#![mbc(rom_only)]`, it parses the `mbc(rom_only)`)
    let attr_inner = recursive(|attr_inner| {
        // Parses `foo = "bar"`
        let eq_attr = ident_parser()
            .then_ignore(just(Lexeme::Punct('=')))
            .then(string_parser())
            .map(|(lhs, rhs)| Attr::Eq(lhs, rhs));

        // Parses `foo` and `foo(...)`
        let call_attr = ident_parser()
            .then(attr_inner
                .separated_by(just(Lexeme::Punct(',')))
                .allow_trailing()
                .delimited_by(just(Lexeme::Punct('(')), just(Lexeme::Punct(')')))
                .or_not())
            .map(|(name, args)| Attr::Call(
                name,
                args.unwrap_or_default(),
            ));

        eq_attr.or(call_attr)
    });

    // Parses attributes like `#![...]`
    // TODO: Generalise this to accept attributes on items too, not just global attributes
    just(Lexeme::Punct('#'))
        .then(just(Lexeme::Punct('!')))
        .ignore_then(attr_inner
            .delimited_by(just(Lexeme::Punct('[')), just(Lexeme::Punct(']'))))
}

fn expr_parser() -> impl Parser<Lexeme, Expr, Error = Simple<Lexeme>> {
    recursive(|expr| {
        let literal = select! { Lexeme::HexLiteral(x) => Expr::Num(x) }
            .or(select! { Lexeme::DecimalLiteral(x) => Expr::Num(x) })
            .or(select! { Lexeme::BinaryLiteral(x) => Expr::Num(x) });

        let constant = select! { Lexeme::Ident(s) => s }
            .map(Expr::Const);

        let bit_macro = just(Lexeme::Ident("bit"))
            .then_ignore(just(Lexeme::Punct('!')))
            .ignore_then(expr
                .clone()
                .map_with_span(|expr, span| (expr, span))
                .delimited_by(just(Lexeme::Punct('[')), just(Lexeme::Punct(']'))))
            .map(|expr| Expr::BitMacro(Box::new(expr)));

        let size_of_val_macro = just(Lexeme::Ident("size_of_val"))
            .then_ignore(just(Lexeme::Punct('!')))
            .ignore_then(expr
                .clone()
                .map_with_span(|expr, span| (expr, span))
                .delimited_by(just(Lexeme::Punct('[')), just(Lexeme::Punct(']'))))
            .map(|expr| Expr::SizeOfValMacro(Box::new(expr)));

        let expr_macro = bit_macro
            .or(size_of_val_macro);

        let atom = literal
            .or(expr_macro)
            .or(constant)
            .map_with_span(|expr, span: Range<usize>| (expr, span));

        let bitor = atom.clone()
            .then(just(Lexeme::Punct('|'))
                .ignore_then(atom)
                .repeated())
            .foldl(|lhs, rhs| {
                let span = lhs.1.start..rhs.1.end;
                (Expr::BitOr(Box::new(lhs), Box::new(rhs)), span)
            });

        bitor
            .map(|(expr, _span)| expr)
    })
}

fn const_parser() -> impl Parser<Lexeme, Const, Error = Simple<Lexeme>> {
    just(Lexeme::KwConst)
        .ignore_then(ident_parser())
        .then_ignore(just(Lexeme::Punct('=')))
        .then(expr_parser().map_with_span(|expr, span| (expr, span)))
        .then_ignore(just(Lexeme::Punct(';')))
        .map(|(name, expr)| Const { name, expr })
}

fn static_parser() -> impl Parser<Lexeme, Static, Error = Simple<Lexeme>> {
    let eols = just(Lexeme::EndOfLine).ignored().repeated();

    // TODO: Generalise this
    let ty = just(Lexeme::Ident("u8"))
        .delimited_by(just(Lexeme::Punct('[')), just(Lexeme::Punct(']')));

    // TODO: Allow more than just arrays of expressions
    let static_expr = expr_parser()
        .map_with_span(|expr, span| (expr, span))
        .padded_by(eols)
        .separated_by(just(Lexeme::Punct(',')))
        .allow_trailing()
        .padded_by(eols)
        .delimited_by(just(Lexeme::Punct('[')), just(Lexeme::Punct(']')));

    just(Lexeme::KwStatic)
        .ignore_then(ident_parser())
        .then_ignore(just(Lexeme::Punct(':')))
        .then_ignore(ty)
        .then_ignore(just(Lexeme::Punct('=')))
        .then(static_expr)
        .then_ignore(just(Lexeme::Punct(';')))
        .map(|(name, items)| Static { name, items })
}

fn fn_parser() -> impl Parser<Lexeme, Fn, Error = Simple<Lexeme>> {
    let eols = just(Lexeme::EndOfLine).ignored().repeated();

    // Parses the return of a function (`` or `-> !` or `-> Irq`)
    // TODO: `->` should be a single lexeme!
    let ret = just(Lexeme::Punct('-'))
        .then(just(Lexeme::Punct('>')))
        .ignore_then(just(Lexeme::Punct('!')).to(FnRet::Never)
            .or(just(Lexeme::Ident("Irq")).to(FnRet::Irq)))
        .or_not()
        .map(|ret| ret.unwrap_or(FnRet::Nothing));

    let block = recursive(|block| {
        let looop = just(Lexeme::KwLoop)
            .ignore_then(block)
            .map(|block| Stmt::Loop(Box::new(block)));

        const REGS: &[&str] = &["a", "b", "c", "d", "e", "h", "l", "sp", "pc", "af", "bc", "de", "hl", "f"];
        let instr_arg =
            // Register args: `a`
            select! { Lexeme::Ident(s) if REGS.contains(&s) => InstrArg::Reg(s) }
            // Expression args: `FOO + BAR`
            .or(expr_parser().map(InstrArg::Expr))
            // Deref args: `[FOO]`, `[BAR-]`, `[BAZ++]`
            .or(expr_parser()
                .then(just(Lexeme::Punct('+')).to(1)
                    .or(just(Lexeme::Punct('-')).to(-1))
                    .repeated())
                .map(|(ptr, shift)| InstrArg::Deref(ptr, shift.into_iter().sum()))
                .delimited_by(just(Lexeme::Punct('[')), just(Lexeme::Punct(']'))))
            .map_with_span(|arg, span| (arg, span));

        let instr = ident_parser()
            .then(instr_arg.separated_by(just(Lexeme::Punct(','))))
            .map(|(name, args)| Stmt::Instr(name, args));

        let branch = just(Lexeme::KwIf)
            .ignore_then(ident_parser())
            .then_ignore(just(Lexeme::Punct(',')))
            .then(just(Lexeme::KwContinue)
                .to(BranchTgt::Continue)
                .map_with_span(|tgt, span| (tgt, span)))
            .map(|(condition, tgt)| Stmt::Branch(condition, tgt));

        let stmt = looop
            .or(instr)
            .or(branch);

        stmt
            .separated_by(eols.at_least(1))
            .allow_leading()
            .allow_trailing()
            .delimited_by(just(Lexeme::Punct('{')), just(Lexeme::Punct('}')))
            .map_with_span(|stmts, span| (Block { stmts }, span))
    });

    just(Lexeme::KwFn)
        .ignore_then(ident_parser())
        .then(ret)
        .then(block)
        .map(|((name, ret), block)| Fn { name, ret, block })
}

pub fn parser() -> impl Parser<Lexeme, Ast, Error = Simple<Lexeme>> {
    let eols = just(Lexeme::EndOfLine).ignored().repeated();

    let item = attr_parser().map(Item::Attr)
        .or(const_parser().map(Item::Const))
        .or(fn_parser().map(Item::Fn))
        .or(static_parser().map(Item::Static));

    item
        .map_with_span(|item, span| (item, span))
        .padded_by(eols)
        .repeated()
        .map(|items| Ast { items })
        .then_ignore(end())
}
