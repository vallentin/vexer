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

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub struct LineCol {
    /// The 1-indexed line in the text on which the token starts or ends (inclusive).
    pub line: usize,
    /// The 1-indexed column in the text (in UTF-8 characters) on which the token starts or ends (inclusive).
    pub col: usize,
}

impl LineCol {
    const START: Self = Self { line: 1, col: 1 };
}

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
            .enumerate()
            .map(|(i, (pat, tok))| {
                let re = compile_rule(pat.as_ref())?;
                if re.is_match("") {
                    return Err(RegexLexerError::EmptyMatch(i));
                }
                Ok((re, tok))
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Self { rules })
    }

    pub fn tokens<'lexer, 'text>(&'lexer self, text: &'text str) -> Tokens<'lexer, 'text, Tok> {
        Tokens::new(self, text)
    }

    pub fn positioned_tokens<'lexer, 'text>(
        &'lexer self,
        text: &'text str,
    ) -> PositionedTokens<'lexer, 'text, Tok> {
        Tokens::new(self, text).positioned()
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

    #[inline]
    pub fn positioned(self) -> PositionedTokens<'lexer, 'text, Tok> {
        PositionedTokens::new(self)
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

#[derive(Clone, Debug)]
pub struct PositionedTokens<'lexer, 'text, Tok>
where
    Tok: Clone,
{
    tokens: Tokens<'lexer, 'text, Tok>,
    loc: LineCol,
}

impl<'lexer, 'text, Tok> PositionedTokens<'lexer, 'text, Tok>
where
    Tok: Clone,
{
    fn new(tokens: Tokens<'lexer, 'text, Tok>) -> Self {
        Self {
            tokens,
            loc: LineCol::START,
        }
    }
}

impl<'lexer, 'text, Tok> Iterator for PositionedTokens<'lexer, 'text, Tok>
where
    Tok: Clone,
{
    type Item = (Option<Tok>, TokenSpan<'text>, (LineCol, LineCol));

    fn next(&mut self) -> Option<Self::Item> {
        let (tok, span) = self.tokens.next()?;

        let start = self.loc;
        for c in span.as_str().chars() {
            if c == '\n' {
                self.loc.line += 1;
                self.loc.col = LineCol::START.col;
            } else {
                self.loc.col += 1;
            }
        }

        Some((tok, span, (start, self.loc)))
    }
}

impl<'lexer, 'text, Tok> FusedIterator for PositionedTokens<'lexer, 'text, Tok> where Tok: Clone {}

fn compile_rule(pattern: &str) -> Result<Regex, RegexLexerError> {
    RegexBuilder::new(pattern)
        .multi_line(true)
        .build()
        .map_err(RegexLexerError::InvalidRegex)
}

#[derive(Clone, Debug)]
pub enum RegexLexerError {
    InvalidRegex(RegexError),
    EmptyMatch(usize),
}

impl error::Error for RegexLexerError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            Self::InvalidRegex(err) => Some(err),
            Self::EmptyMatch(_) => None,
        }
    }
}

impl fmt::Display for RegexLexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidRegex(err) => write!(f, "invalid regex: {}", err),
            Self::EmptyMatch(i) => write!(f, "empty match for rule {}", i),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_simple() {
        #[derive(PartialEq, Eq, Clone, Copy, Debug)]
        enum Tok {
            Whitespace,
            NonWhitespace,
        }

        #[rustfmt::skip]
        let rules = [
            (r"\s+", Tok::Whitespace),
            (r"\S+", Tok::NonWhitespace),
        ];
        let lexer = RegexLexer::new(rules).unwrap();

        let text = "foo bar   baz\tqux \r\n\tquux ";
        assert_tokens(
            lexer.tokens(text),
            [
                (Some(Tok::NonWhitespace), (0, 3, "foo")),
                (Some(Tok::Whitespace), (3, 4, " ")),
                (Some(Tok::NonWhitespace), (4, 7, "bar")),
                (Some(Tok::Whitespace), (7, 10, "   ")),
                (Some(Tok::NonWhitespace), (10, 13, "baz")),
                (Some(Tok::Whitespace), (13, 14, "\t")),
                (Some(Tok::NonWhitespace), (14, 17, "qux")),
                (Some(Tok::Whitespace), (17, 21, " \r\n\t")),
                (Some(Tok::NonWhitespace), (21, 25, "quux")),
                (Some(Tok::Whitespace), (25, 26, " ")),
            ],
        );
        assert_spans(text, &lexer);
    }

    #[test]
    fn test_lexer_lines() {
        #[derive(PartialEq, Eq, Clone, Copy, Debug)]
        enum Tok {
            Comment,
            Line,
        }

        #[rustfmt::skip]
        let rules = [
            (r"//.*$", Tok::Comment),
            (r".+$", Tok::Line),
        ];
        let lexer = RegexLexer::new(rules).unwrap();

        let text = "// This is comment
This is a test
// This is another comment\n\n";
        assert_tokens(
            lexer.tokens(text),
            [
                (Some(Tok::Comment), (0, 18, "// This is comment")),
                (None, (18, 19, "\n")),
                (Some(Tok::Line), (19, 33, "This is a test")),
                (None, (33, 34, "\n")),
                (Some(Tok::Comment), (34, 60, "// This is another comment")),
                (None, (60, 62, "\n\n")),
            ],
        );
        assert_spans(text, &lexer);
    }

    #[test]
    fn test_lexer_unknown() {
        #[derive(PartialEq, Eq, Clone, Copy, Debug)]
        enum Tok {
            Whitespace,
            Alphabetic,
        }

        let rules = [
            (r"\s+", Tok::Whitespace),
            (r"[[:alpha:]]+", Tok::Alphabetic),
        ];
        let lexer = RegexLexer::new(rules).unwrap();

        // Unknown at the start
        let text = "1234 \t56 foo 78 bar";
        assert_tokens(
            lexer.tokens(text),
            [
                (None, (0, 4, "1234")),
                (Some(Tok::Whitespace), (4, 6, " \t")),
                (None, (6, 8, "56")),
                (Some(Tok::Whitespace), (8, 9, " ")),
                (Some(Tok::Alphabetic), (9, 12, "foo")),
                (Some(Tok::Whitespace), (12, 13, " ")),
                (None, (13, 15, "78")),
                (Some(Tok::Whitespace), (15, 16, " ")),
                (Some(Tok::Alphabetic), (16, 19, "bar")),
            ],
        );
        assert_spans(text, &lexer);

        // Unknown in between
        let text = "foo 123 456 \tbar";
        assert_tokens(
            lexer.tokens(text),
            [
                (Some(Tok::Alphabetic), (0, 3, "foo")),
                (Some(Tok::Whitespace), (3, 4, " ")),
                (None, (4, 7, "123")),
                (Some(Tok::Whitespace), (7, 8, " ")),
                (None, (8, 11, "456")),
                (Some(Tok::Whitespace), (11, 13, " \t")),
                (Some(Tok::Alphabetic), (13, 16, "bar")),
            ],
        );
        assert_spans(text, &lexer);

        // Unknown at the end
        let text = "foo 12   bar 345";
        assert_tokens(
            lexer.tokens(text),
            [
                (Some(Tok::Alphabetic), (0, 3, "foo")),
                (Some(Tok::Whitespace), (3, 4, " ")),
                (None, (4, 6, "12")),
                (Some(Tok::Whitespace), (6, 9, "   ")),
                (Some(Tok::Alphabetic), (9, 12, "bar")),
                (Some(Tok::Whitespace), (12, 13, " ")),
                (None, (13, 16, "345")),
            ],
        );
        assert_spans(text, &lexer);
    }

    #[test]
    fn test_lexer_positioned_tokens() {
        #[derive(PartialEq, Eq, Clone, Copy, Debug)]
        enum Tok {
            Space,
            Let,
            Fn,
            Name,
            SimpleString,
        }

        let rules = [
            (r"\s+", Tok::Space),
            (r"let", Tok::Let),
            (r"fn", Tok::Fn),
            (r"\w+", Tok::Name),
            ("\".*?\"", Tok::SimpleString),
        ];
        let lexer = RegexLexer::new(rules).unwrap();

        let text = r#"fn main() {
    println!("Hello World");
}"#;
        let mut tokens = lexer.tokens(text).positioned();

        let (tok, span, (start, end)) = tokens.next().unwrap();
        assert_eq!(tok, Some(Tok::Fn));
        assert_eq!((span.start(), span.end(), span.as_str()), (0, 2, "fn"));
        assert_eq!((start.line, start.col), (1, 1));
        assert_eq!((end.line, end.col), (1, 3));

        let (tok, span, (start, end)) = tokens.next().unwrap();
        assert_eq!(tok, Some(Tok::Space));
        assert_eq!((span.start(), span.end(), span.as_str()), (2, 3, " "));
        assert_eq!((start.line, start.col), (1, 3));
        assert_eq!((end.line, end.col), (1, 4));

        let (tok, span, (start, end)) = tokens.next().unwrap();
        assert_eq!(tok, Some(Tok::Name));
        assert_eq!((span.start(), span.end(), span.as_str()), (3, 7, "main"));
        assert_eq!((start.line, start.col), (1, 4));
        assert_eq!((end.line, end.col), (1, 8));

        let (tok, span, (start, end)) = tokens.next().unwrap();
        assert_eq!(tok, None);
        assert_eq!((span.start(), span.end(), span.as_str()), (7, 9, "()"));
        assert_eq!((start.line, start.col), (1, 8));
        assert_eq!((end.line, end.col), (1, 10));

        let (tok, span, (start, end)) = tokens.next().unwrap();
        assert_eq!(tok, Some(Tok::Space));
        assert_eq!((span.start(), span.end(), span.as_str()), (9, 10, " "));
        assert_eq!((start.line, start.col), (1, 10));
        assert_eq!((end.line, end.col), (1, 11));

        let (tok, span, (start, end)) = tokens.next().unwrap();
        assert_eq!(tok, None);
        assert_eq!((span.start(), span.end(), span.as_str()), (10, 11, "{"));
        assert_eq!((start.line, start.col), (1, 11));
        assert_eq!((end.line, end.col), (1, 12));

        let (tok, span, (start, end)) = tokens.next().unwrap();
        assert_eq!(tok, Some(Tok::Space));
        assert_eq!(
            (span.start(), span.end(), span.as_str()),
            (11, 16, "\n    ")
        );
        assert_eq!((start.line, start.col), (1, 12));
        assert_eq!((end.line, end.col), (2, 5));

        let (tok, span, (start, end)) = tokens.next().unwrap();
        assert_eq!(tok, Some(Tok::Name));
        assert_eq!(
            (span.start(), span.end(), span.as_str()),
            (16, 23, "println")
        );
        assert_eq!((start.line, start.col), (2, 5));
        assert_eq!((end.line, end.col), (2, 12));

        let (tok, span, (start, end)) = tokens.next().unwrap();
        assert_eq!(tok, None);
        assert_eq!((span.start(), span.end(), span.as_str()), (23, 25, "!("));
        assert_eq!((start.line, start.col), (2, 12));
        assert_eq!((end.line, end.col), (2, 14));

        let (tok, span, (start, end)) = tokens.next().unwrap();
        assert_eq!(tok, Some(Tok::SimpleString));
        assert_eq!(
            (span.start(), span.end(), span.as_str()),
            (25, 38, "\"Hello World\"")
        );
        assert_eq!((start.line, start.col), (2, 14));
        assert_eq!((end.line, end.col), (2, 27));

        let (tok, span, (start, end)) = tokens.next().unwrap();
        assert_eq!(tok, None);
        assert_eq!((span.start(), span.end(), span.as_str()), (38, 40, ");"));
        assert_eq!((start.line, start.col), (2, 27));
        assert_eq!((end.line, end.col), (2, 29));

        let (tok, span, (start, end)) = tokens.next().unwrap();
        assert_eq!(tok, Some(Tok::Space));
        assert_eq!((span.start(), span.end(), span.as_str()), (40, 41, "\n"));
        assert_eq!((start.line, start.col), (2, 29));
        assert_eq!((end.line, end.col), (3, 1));

        let (tok, span, (start, end)) = tokens.next().unwrap();
        assert_eq!(tok, None);
        assert_eq!((span.start(), span.end(), span.as_str()), (41, 42, "}"));
        assert_eq!((start.line, start.col), (3, 1));
        assert_eq!((end.line, end.col), (3, 2));

        assert_eq!(tokens.next(), None);
    }

    #[test]
    fn test_lexer_no_rules() {
        #[derive(PartialEq, Eq, Clone, Copy, Debug)]
        enum Tok {
            Whitespace,
            NonWhitespace,
        }

        let lexer = RegexLexer::<Tok>::new::<&str, _>([]).unwrap();

        let text = "foo bar\n\tbaz";
        assert_tokens(lexer.tokens(text), [(None, (0, text.len(), text))]);
        assert_spans(text, &lexer);
    }

    #[test]
    fn test_lexer_error_empty_match() {
        #[derive(PartialEq, Eq, Clone, Copy, Debug)]
        enum Tok {
            NonEmpty,
            Empty,
        }

        let err = RegexLexer::new([(r"", Tok::Empty)]).unwrap_err();
        match err {
            RegexLexerError::EmptyMatch(0) => {}
            _ => panic!("{:?}", err),
        }

        let err = RegexLexer::new([(r".*", Tok::Empty)]).unwrap_err();
        match err {
            RegexLexerError::EmptyMatch(0) => {}
            _ => panic!("{:?}", err),
        }

        let err = RegexLexer::new([(r".+", Tok::NonEmpty), (r".*", Tok::Empty)]).unwrap_err();
        match err {
            RegexLexerError::EmptyMatch(1) => {}
            _ => panic!("{:?}", err),
        }
    }

    fn assert_tokens<'a, Tok>(
        actual_tokens: impl IntoIterator<Item = (Option<Tok>, TokenSpan<'a>)>,
        expected_tokens: impl IntoIterator<Item = (Option<Tok>, (usize, usize, &'a str))>,
    ) where
        Tok: PartialEq + fmt::Debug,
    {
        let mut actual_tokens = actual_tokens.into_iter();
        let expected_tokens = expected_tokens.into_iter();

        for (expected_tok, expected_span) in expected_tokens {
            let (tok, span) = actual_tokens.next().unwrap();

            assert_eq!(tok, expected_tok);
            assert_eq!(span.start(), expected_span.0);
            assert_eq!(span.end(), expected_span.1);
            assert_eq!(span.as_str(), expected_span.2);
        }

        assert_eq!(actual_tokens.next(), None);
    }

    fn assert_spans<'a, Tok>(text: &'a str, lexer: &RegexLexer<Tok>)
    where
        Tok: Clone,
    {
        let mut end = 0;

        for (_tok, span) in lexer.tokens(text) {
            assert_eq!(span.as_str(), &text[span.start()..span.end()]);
            end = span.end();
        }

        assert_eq!(end, text.len());
    }
}
