//! A custom lexer which collaborates with the parser to know when an identifier
//! is a TYPE_IDENTIFIER or a regular IDENTIFIER. Without this distinction, the
//! P4 grammar is ambiguous:
//! https://p4.org/p4-spec/docs/P4-16-v1.2.1.html#sec-grammar

use logos::{Lexer, Logos, SpannedIter};
use std::ops::Range;

#[derive(Logos, Clone, Debug, PartialEq)]
pub enum Token<'input> {
    #[token("=")]
    Equals,
    #[token("_")]
    Underscore,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token("!")]
    Exclamation,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("control")]
    Control,
    #[token("apply")]
    Apply,
    #[token("action")]
    Action,
    #[token("const")]
    Const,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("in")]
    In,
    #[token("out")]
    Out,
    #[token("inout")]
    InOut,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier(&'input str),
    #[regex(r"[a-zA-Z0-9_.]+\.[a-zA-Z0-9_]+")]
    FilePath(&'input str),

    // Ignore comments
    #[regex(r"//[^\n]*", logos::skip)]
    // Taken from https://stackoverflow.com/a/46220379
    #[regex(r"/\*/*(?:(?:\**[^*/]+/*)*)\*+/", logos::skip)]
    // Ignore whitespace
    #[regex(r"\s+", logos::skip)]
    #[error]
    Error,
}

pub struct LalrpopLexerIter<'input> {
    inner: SpannedIter<'input, Token<'input>>,
}

impl<'input> LalrpopLexerIter<'input> {
    pub fn new(lexer: Lexer<'input, Token<'input>>) -> Self {
        Self {
            inner: lexer.spanned(),
        }
    }
}

impl<'input> Iterator for LalrpopLexerIter<'input> {
    type Item = Result<(usize, Token<'input>, usize), Range<usize>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            Some((token, span)) => Some(match token {
                Token::Error => Err(span),
                token => Ok((span.start, token, span.end)),
            }),
            None => None,
        }
    }
}
