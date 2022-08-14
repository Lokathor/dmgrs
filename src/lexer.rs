//! This "lexes" source files.
//!
//! Lexing converts the stream of `char` values in the file text into a stream
//! of *slightly* more abstract tokens. It simplifies the parsing mostly by
//! handling whitespace all up front.

use logos::Logos;
