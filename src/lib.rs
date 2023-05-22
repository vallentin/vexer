#![forbid(unsafe_code)]
#![forbid(elided_lifetimes_in_paths)]
#![deny(missing_debug_implementations)]
#![cfg_attr(debug_assertions, allow(dead_code, unreachable_code))]

pub use regex::Error as RegexError;

use std::error;
use std::fmt;
use std::iter::FusedIterator;
use std::ops::{ControlFlow, Range};

use regex::{Match, Regex, RegexBuilder};

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct TokenSpan<'text> {
    text: &'text str,
    start: usize,
    end: usize,
}

impl<'text> TokenSpan<'text> {
    pub fn new(text: &'text str, start: usize, end: usize) -> Self {
        Self { text, start, end }
    }

    #[inline]
    pub fn start(&self) -> usize {
        self.start
    }

    #[inline]
    pub fn end(&self) -> usize {
        self.end
    }

    /// Guaranteed to be at least 1.
    #[inline]
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    #[inline]
    pub fn range(&self) -> Range<usize> {
        self.start..self.end
    }

    #[inline]
    pub fn as_str(&self) -> &'text str {
        &self.text[self.range()]
    }
}

impl fmt::Debug for TokenSpan<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TokenSpan")
            .field("start", &self.start)
            .field("end", &self.end)
            .field("string", &self.as_str())
            .finish()
    }
}

impl fmt::Display for TokenSpan<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

#[derive(Clone, Debug)]
pub struct RegexLexer<Tok>
where
    Tok: Clone,
{
    rules: Vec<(Regex, Tok)>,
}

impl<Tok> RegexLexer<Tok>
where
    Tok: Clone,
{
    pub fn new<R, I>(rules: I) -> Result<Self, RegexLexerError>
    where
        I: IntoIterator<Item = (R, Tok)>,
        R: AsRef<str>,
    {
        let rules = rules
            .into_iter()
            .map(|(pat, tok)| {
                let re = compile_rule(pat.as_ref())?;
                Ok((re, tok))
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Self { rules })
    }

    pub fn tokens<'lexer, 'text>(&'lexer self, text: &'text str) -> Tokens<'lexer, 'text, Tok> {
        Tokens::new(self, text)
    }
}

#[derive(Clone, Debug)]
pub struct Tokens<'lexer, 'text, Tok>
where
    Tok: Clone,
{
    lexer: &'lexer RegexLexer<Tok>,
    text: &'text str,
    rest: &'text str,
    at: usize,
}

impl<'lexer, 'text, Tok> Tokens<'lexer, 'text, Tok>
where
    Tok: Clone,
{
    #[inline]
    fn new(lexer: &'lexer RegexLexer<Tok>, text: &'text str) -> Self {
        Self {
            lexer,
            text,
            rest: text,
            at: 0,
        }
    }
}

impl<'lexer, 'text, Tok> Iterator for Tokens<'lexer, 'text, Tok>
where
    Tok: Clone,
{
    type Item = (Option<Tok>, TokenSpan<'text>);

    fn next(&mut self) -> Option<Self::Item> {
        let tok_and_match = self
            .lexer
            .rules
            .iter()
            .filter_map(|(re, tok)| {
                let m = re.find(self.rest)?;
                Some((tok, m))
            })
            .try_fold::<Option<(&'lexer Tok, Match<'text>)>, _, _>(None, |earliest, (tok, m)| {
                if m.start() == 0 {
                    return ControlFlow::Break(Some((tok, m)));
                }

                if let Some((earliest_tok, earliest_match)) = earliest {
                    if m.start() < earliest_match.start() {
                        ControlFlow::Continue(Some((tok, m)))
                    } else {
                        ControlFlow::Continue(Some((earliest_tok, earliest_match)))
                    }
                } else {
                    ControlFlow::Continue(Some((tok, m)))
                }
            });
        let tok_and_match = match tok_and_match {
            ControlFlow::Continue(tok_and_match) => tok_and_match,
            ControlFlow::Break(tok_and_match) => tok_and_match,
        };

        match tok_and_match {
            Some((tok, m)) if m.start() == 0 => {
                let span_len = m.end();
                let span = TokenSpan::new(self.text, self.at, self.at + span_len);

                self.at += span_len;
                self.rest = &self.rest[span_len..];

                Some((Some(tok.to_owned()), span))
            }
            Some((_next_tok, next_m)) => {
                let span_len = next_m.start();
                let span = TokenSpan::new(self.text, self.at, self.at + span_len);

                self.at += span_len;
                self.rest = &self.rest[span_len..];

                Some((None, span))
            }
            None if self.rest.is_empty() => None,
            None => {
                let span_len = self.rest.len();
                let span = TokenSpan::new(self.text, self.at, self.at + span_len);

                self.at += span_len;
                self.rest = &self.rest[span_len..];

                Some((None, span))
            }
        }
    }
}

impl<'lexer, 'text, Tok> FusedIterator for Tokens<'lexer, 'text, Tok> where Tok: Clone {}

fn compile_rule(pattern: &str) -> Result<Regex, RegexLexerError> {
    RegexBuilder::new(pattern)
        .multi_line(true)
        .build()
        .map_err(RegexLexerError::InvalidRegex)
}

#[derive(Clone, Debug)]
pub enum RegexLexerError {
    InvalidRegex(RegexError),
}

impl error::Error for RegexLexerError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Self::InvalidRegex(err) => Some(err),
        }
    }
}

impl fmt::Display for RegexLexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidRegex(err) => write!(f, "invalid regex: {}", err),
        }
    }
}
