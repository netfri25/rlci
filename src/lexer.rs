use std::path::Path;
use std::collections::BTreeMap;

use crate::token::{Token, TokenKind, Loc};

#[derive(Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    loc: Loc,
    last_was_newline: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, path: &(impl AsRef<Path> + ?Sized)) -> Self {
        let loc = Loc::new(path);
        Self {
            input,
            loc,
            last_was_newline: false,
        }
    }

    /// returns `TokenKind::Invalid` when tokenizing has failed.
    /// if the lexer has eached the end of the input, it will return the Eof token
    pub fn next_token(&mut self) -> Token<'a> {
        if self.input.is_empty() {
            return self.new_token(0, TokenKind::Eof);
        }

        // also skips whitespace
        if !self.skip_comments() {
            return self.new_token(0, TokenKind::Invalid)
        }

        let methods = [
            Self::lex_newline,
            Self::lex_string_lit,
            Self::lex_keyword,
            Self::lex_ident,
            Self::lex_float_lit,
            Self::lex_int_lit,
        ];

        let Some(token) = methods.into_iter().find_map(|method| method(self)) else {
            return self.new_token(0, TokenKind::Invalid);
        };
        self.consume(token.text().len());

        let is_newline = token.kind() == TokenKind::NewLine;
        if self.last_was_newline && is_newline {
            self.next_token()
        } else {
            self.last_was_newline = is_newline;
            token
        }
    }

    fn consume(&mut self, len: usize) {
        let consumed;
        (consumed, self.input) = self.input.split_at(len);
        for c in consumed.as_bytes() {
            if *c == b'\n' {
                self.loc.next_row();
            } else {
                self.loc.next_col();
            }
        }
    }

    fn skip_whitespace(&mut self) {
        // not skipping newline since it's considered to be a token
        let len = self.count_while(|c| *c != '\n' && c.is_ascii_whitespace());
        self.consume(len)
    }

    // skips both whitespace and comments
    // returns false when there's an unclosed block comment
    fn skip_comments(&mut self) -> bool {
        loop {
            self.skip_whitespace();
            let len = if self.starts_with("BTW") {
                self.count_while(|c| *c != '\n')
            } else if self.starts_with("OBTW") {
                let finish = "TLDR";
                let Some(distance) = self.count_to(finish) else {
                    return false;
                };
                distance + finish.len()
            } else {
                break;
            };

            self.consume(len);
        }

        self.skip_whitespace();
        true
    }

    // newline contains a chunk of whitespaces with one or more newlines in between
    fn lex_newline(&self) -> Option<Token<'a>> {
        let mut has_newline = false;
        let len = self.count_while(|c| {
            has_newline |= *c == '\n';
            c.is_ascii_whitespace()
        });
        has_newline.then(|| self.new_token(len, TokenKind::NewLine))
    }

    fn lex_ident(&self) -> Option<Token<'a>> {
        if !self.peek().is_ascii_alphabetic() {
            return None;
        }

        let len = self.count_while(|&c| c.is_ascii_digit() || c.is_ascii_alphabetic() || c == '_');
        Some(self.new_token(len, TokenKind::Ident))
    }

    fn lex_keyword(&self) -> Option<Token<'a>> {
        use TokenKind::*;
        let keywords = BTreeMap::from([
            ("A", A),
            ("ALL OF", AllOf),
            ("AN", An),
            ("ANY OF", AnyOf),
            ("AN YR", AnYr),
            ("BIGGR OF", BiggrOf),
            ("BOTH OF", BothOf),
            ("BOTH SAEM", BothSaem),
            ("BUKKIT", Bukkit),
            ("CAN HAS", CanHas),
            (",", Comma),
            ("DIFF OF", DiffOf),
            ("DIFFRINT", Diffrint),
            ("EITHER OF", EitherOf),
            ("FAIL", Fail),
            ("FOUND YR", FoundYr),
            ("GIMMEH", Gimmeh),
            ("GTFO", Gtfo),
            ("HAI", Hai),
            ("HAS A", HasA),
            ("HOW IZ", HowIz),
            ("I DUZ", IDuz),
            ("IF U SAY SO", IfUSaySo),
            ("IM IN YR", ImInYr),
            ("IM LIEK", ImLiek),
            ("IM OUTTA YR", ImOuttaYr),
            ("INVISIBLE", Invisible),
            ("IS NOW A", IsNowA),
            ("IT", It),
            ("ITZ A", ItzA),
            ("ITZ", Itz),
            ("ITZ LIEK A", ItzLiekA),
            ("IZ", Iz),
            ("KTHXBYE", KThxBye),
            ("KTHX", KThx),
            ("MAEK", Maek),
            ("MEBBE", Mebbe),
            ("MKAY", Mkay),
            ("MOD OF", ModOf),
            ("NERFIN", Nerfin),
            ("NOOB", Noob),
            ("NOT", Not),
            ("NO WAI", NoWai),
            ("NUMBAR", Numbar),
            ("NUMBR", Numbr),
            ("O HAI IM", OHaiIm),
            ("OIC", Oic),
            ("OMG", Omg),
            ("OMG WTF", OmgWtf),
            ("O RLY?", ORly),
            ("PRODUKT OF", ProduktOf),
            ("QUOSHUNT OF", QuoshuntOf),
            ("R", R),
            ("SMALLR OF", SmallrOf),
            ("SMOOSH", Smoosh),
            ("SRS", Srs),
            ("SUM OF", SumOf),
            ("TIL", Til),
            ("TROOF", Troof),
            ("UPPIN", Uppin),
            ("VISIBLE", Visible),
            ("WILE", Wile),
            ("WIN", Win),
            ("WON OF", WonOf),
            ("WTF?", Wtf),
            ("YA RLY", YaRly),
            ("YARN", Yarn),
            ("YR", Yr),
            ("'Z", ApostZ),
        ]);

        keywords.iter().rev().find_map(|(keyword, kind)| {
            self.starts_with(keyword)
                .then(|| self.new_token(keyword.len(), *kind))
        })
    }

    fn lex_int_lit(&self) -> Option<Token<'a>> {
        let mut is_first = true;
        let len = self.count_while(|&c| {
            let b = (is_first && c == '-') || c.is_ascii_digit();
            is_first = false;
            b
        });

        (len > 0).then(|| self.new_token(len, TokenKind::IntLit))
    }

    fn lex_float_lit(&self) -> Option<Token<'a>> {
        let mut is_first = true;
        let mut has_dot = false;
        let len = self.count_while(|&c| {
            let b = (is_first && c == '-') || c.is_ascii_digit() || (c == '.' && !has_dot);
            has_dot |= c == '.';
            is_first = false;
            b
        });

        (len > 0 && has_dot).then(|| self.new_token(len, TokenKind::FloatLit))
    }

    fn lex_string_lit(&self) -> Option<Token<'a>> {
        if self.peek() != '"' {
            return None;
        }

        let mut escape = false;
        let mut quots_count = 0;
        let len = self.count_while(|&c| {
            if escape {
                escape = false;
                true
            } else {
                escape = c == ':';
                quots_count += (c == '"') as u8;
                quots_count < 2
            }
        });
        // include the closing quots
        let len = len + 1;

        if self.peek_at(len - 1) != Some('"') {
            // unterminated string literal
            Some(self.new_token(0, TokenKind::Invalid))
        } else {
            Some(self.new_token(len, TokenKind::StringLit))
        }
    }

    fn peek(&self) -> char {
        self.input.chars().next().unwrap_or_default()
    }

    fn peek_at(&self, at: usize) -> Option<char> {
        self.input.chars().nth(at)
    }

    fn count_while(&self, pred: impl FnMut(&char) -> bool) -> usize {
        self.input.chars().take_while(pred).count()
    }

    fn starts_with(&self, text: &str) -> bool {
        self.input.starts_with(text)
    }

    fn count_to(&self, text: &str) -> Option<usize> {
        self.input.find(text)
    }

    fn new_token(&self, len: usize, kind: TokenKind) -> Token<'a> {
        Token::new(kind, &self.input[..len], self.loc.clone())
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let tkn = self.next_token();
        if [TokenKind::Invalid, TokenKind::Eof].contains(&tkn.kind()) {
            None
        } else {
            Some(tkn)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use TokenKind::*;

    #[test]
    pub fn keywords() {
        let input = "HAI I HAS A KTHXBYE ITZ ITZ A A SRS EITHER OF BOTH OF NOT WON OF ANY OF ALL OF DIFFRINT";
        let tkns = lex(input);
        assert_eq!(
            tkns,
            vec![
                token(Hai, "HAI"),
                token(Ident, "I"),
                token(HasA, "HAS A"),
                token(KThxBye, "KTHXBYE"),
                token(Itz, "ITZ"),
                token(ItzA, "ITZ A"),
                token(A, "A"),
                token(Srs, "SRS"),
                token(EitherOf, "EITHER OF"),
                token(BothOf, "BOTH OF"),
                token(Not, "NOT"),
                token(WonOf, "WON OF"),
                token(AnyOf, "ANY OF"),
                token(AllOf, "ALL OF"),
                token(Diffrint, "DIFFRINT"),
            ]
        )
    }

    #[test]
    pub fn types() {
        let input = "NOOB TROOF NUMBR NUMBAR YARN BUKKIT";
        let tkns = lex(input);
        assert_eq!(
            tkns,
            vec![
                token(Noob, "NOOB"),
                token(Troof, "TROOF"),
                token(Numbr, "NUMBR"),
                token(Numbar, "NUMBAR"),
                token(Yarn, "YARN"),
                token(Bukkit, "BUKKIT"),
            ]
        )
    }

    #[test]
    fn literals() {
        let input = r#"WIN FAIL 209348 -2948 0.234 -1234234.3 "hello :"world:"""#;
        let tkns = lex(input);
        assert_eq!(
            tkns,
            vec![
                token(TokenKind::Win, "WIN"),
                token(TokenKind::Fail, "FAIL"),
                token(TokenKind::IntLit, "209348"),
                token(TokenKind::IntLit, "-2948"),
                token(TokenKind::FloatLit, "0.234"),
                token(TokenKind::FloatLit, "-1234234.3"),
                token(TokenKind::StringLit, "\"hello :\"world:\"\""),
            ]
        )
    }

    #[test]
    fn comments() {
        let input = "BTW WIN FAIL\nOBTW 209348 -2948 0.234 TLDR  \n-1234234.3";
        let tkns = lex(input);
        assert_eq!(
            tkns,
            vec![
                token(TokenKind::NewLine, "\n"),
                token(TokenKind::FloatLit, "-1234234.3"),
            ]
        )
    }

    #[test]
    fn unclosed_comment() {
        let input = "WIN FAIL OBTW WIN FAIL";
        let tkns = lex(input);
        assert_eq!(
            tkns,
            vec![
                token(TokenKind::Win, "WIN"),
                token(TokenKind::Fail, "FAIL"),
            ]
        )
    }

    #[test]
    fn operators() {
        let input = "AN SUM OF DIFF OF PRODUKT OF QUOSHUNT OF MOD OF BIGGR OF SMALLR OF";
        let tkns = lex(input);
        assert_eq!(
            tkns,
            vec![
                token(TokenKind::An, "AN"),
                token(TokenKind::SumOf, "SUM OF"),
                token(TokenKind::DiffOf, "DIFF OF"),
                token(TokenKind::ProduktOf, "PRODUKT OF"),
                token(TokenKind::QuoshuntOf, "QUOSHUNT OF"),
                token(TokenKind::ModOf, "MOD OF"),
                token(TokenKind::BiggrOf, "BIGGR OF"),
                token(TokenKind::SmallrOf, "SMALLR OF"),
            ]
        )
    }

    fn token(kind: TokenKind, text: &'static str) -> Token<'static> {
        Token::new(kind, text, Loc::default())
    }

    fn lex(input: &'static str) -> Vec<Token<'static>> {
        Lexer::new(input, None).collect()
    }
}
