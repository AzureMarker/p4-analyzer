//! A custom lexer which collaborates with the parser to know when an identifier
//! is a TYPE_IDENTIFIER or a regular IDENTIFIER. Without this distinction, the
//! P4 grammar is ambiguous:
//! https://p4.org/p4-spec/docs/P4-16-v1.2.1.html#sec-grammar

use logos::{Lexer, Logos};
use std::cell::RefCell;
use std::collections::HashSet;
use std::ops::Range;

/// The lexer state stores the names of previously declared types
pub type LexerState<'input> = &'input RefCell<HashSet<String>>;

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(extras = LexerState<'s>)]
pub enum Token<'input> {
    #[token("=")]
    Equals,
    #[token("_")]
    Underscore,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token(".")]
    Period,
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
    #[token("struct")]
    Struct,
    #[token("control")]
    Control,
    #[token("table")]
    Table,
    #[token("key")]
    Key,
    #[token("actions")]
    Actions,
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
    #[token("bool")]
    Bool,
    // The boolean specifies if the identifier is a type identifier
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", callback = is_type_ident)]
    Identifier((&'input str, bool)),
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

fn is_type_ident<'input>(lexer: &mut Lexer<'input, Token<'input>>) -> (&'input str, bool) {
    (lexer.slice(), lexer.extras.borrow().contains(lexer.slice()))
}

pub struct LalrpopLexerIter<'input> {
    pub lexer: Lexer<'input, Token<'input>>,
}

impl<'input> LalrpopLexerIter<'input> {
    pub fn new(lexer: Lexer<'input, Token<'input>>) -> Self {
        Self { lexer }
    }
}

impl<'input> Iterator for LalrpopLexerIter<'input> {
    type Item = Result<(usize, Token<'input>, usize), Range<usize>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.next() {
            Some(token) => {
                let span = self.lexer.span();
                Some(match token {
                    Token::Error => Err(span),
                    token => Ok((span.start, token, span.end)),
                })
            }
            None => None,
        }
    }
}
