use crate::token::{Token, TokenKind};

pub struct Lexer<'a> {
    input: &'a str,
    last_was_newline: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            last_was_newline: false,
        }
    }

    /// returns None when tokenizing has failed.
    /// if the lexer has eached the end of the input, it will return the Eof token
    pub fn next_token(&mut self) -> Token<'a> {
        if self.input.is_empty() {
            return self.new_token(0, TokenKind::Eof);
        }

        let methods = [
            Self::lex_newline,
            Self::lex_ident,
            Self::lex_type,
            Self::lex_keyword,
            Self::lex_bool_lit,
            Self::lex_float_lit,
            Self::lex_int_lit,
            Self::lex_string_lit,
        ];

        // also skips whitespace
        self.skip_comments();
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
        self.input = &self.input[len..]
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
        if !self.peek().is_ascii_lowercase() {
            return None;
        }

        let len = self.count_while(|&c| c.is_ascii_digit() || c.is_ascii_lowercase() || c == '_');
        Some(self.new_token(len, TokenKind::Ident))
    }

    fn lex_keyword(&self) -> Option<Token<'a>> {
        use TokenKind::*;
        const KEYWORDS: &[(&str, TokenKind)] = &[
            ("QUOSHUNT", Quoshunt),
            ("KTHXBYE", KThxBye),
            ("PRODUKT", Produkt),
            ("SMALLR", Smallr),
            ("EITHER", Either),
            ("BIGGR", Biggr),
            ("HAS A", HasA),
            ("DIFF", Diff),
            ("BOTH", Both),
            ("MOD", Mod),
            ("HAI", Hai),
            ("ITZ", Itz),
            ("SRS", Srs),
            ("SUM", Sum),
            ("NOT", Not),
            ("ALL", All),
            ("ANY", Any),
            ("WON", Won),
            ("AN", An),
            ("OF", Of),
            ("I", I),
            ("A", A),
            ("R", R),
        ];

        KEYWORDS.iter().find_map(|(keyword, kind)| {
            self.starts_with(keyword)
                .then(|| self.new_token(keyword.len(), *kind))
        })
    }

    fn lex_type(&self) -> Option<Token<'a>> {
        use TokenKind::*;
        const TYPES: &[(&str, TokenKind)] = &[
            ("NOOB", Noob),
            ("TROOF", Troof),
            ("NUMBR", Numbr),
            ("NUMBAR", Numbar),
            ("YARN", Yarn),
            ("BUKKIT", Bukkit),
        ];

        TYPES.iter().find_map(|(typ, kind)| {
            self.starts_with(typ)
                .then(|| self.new_token(typ.len(), *kind))
        })
    }

    fn lex_bool_lit(&self) -> Option<Token<'a>> {
        use TokenKind::*;
        const LITS: &[(&str, TokenKind)] = &[("WIN", Win), ("FAIL", Fail)];

        LITS.iter().find_map(|(lit, kind)| {
            self.starts_with(lit)
                .then(|| self.new_token(lit.len(), *kind))
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
        Token::new(kind, &self.input[..len])
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
        let input = "HAI I HAS A KTHXBYE ITZ A SRS EITHER BOTH NOT WON ANY ALL";
        let tkns = lex(input);
        assert_eq!(
            tkns,
            vec![
                Token::new(Hai, "HAI"),
                Token::new(I, "I"),
                Token::new(HasA, "HAS A"),
                Token::new(KThxBye, "KTHXBYE"),
                Token::new(Itz, "ITZ"),
                Token::new(A, "A"),
                Token::new(Srs, "SRS"),
                Token::new(Either, "EITHER"),
                Token::new(Both, "BOTH"),
                Token::new(Not, "NOT"),
                Token::new(Won, "WON"),
                Token::new(Any, "ANY"),
                Token::new(All, "ALL"),
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
                Token::new(Noob, "NOOB"),
                Token::new(Troof, "TROOF"),
                Token::new(Numbr, "NUMBR"),
                Token::new(Numbar, "NUMBAR"),
                Token::new(Yarn, "YARN"),
                Token::new(Bukkit, "BUKKIT"),
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
                Token::new(TokenKind::Win, "WIN"),
                Token::new(TokenKind::Fail, "FAIL"),
                Token::new(TokenKind::IntLit, "209348"),
                Token::new(TokenKind::IntLit, "-2948"),
                Token::new(TokenKind::FloatLit, "0.234"),
                Token::new(TokenKind::FloatLit, "-1234234.3"),
                Token::new(TokenKind::StringLit, "\"hello :\"world:\"\""),
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
                Token::new(TokenKind::NewLine, "\n"),
                Token::new(TokenKind::FloatLit, "-1234234.3"),
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
                Token::new(TokenKind::Win, "WIN"),
                Token::new(TokenKind::Fail, "FAIL"),
            ]
        )
    }

    #[test]
    fn operators() {
        let input = "AN OF SUM DIFF PRODUKT QUOSHUNT MOD BIGGR SMALLR";
        let tkns = lex(input);
        assert_eq!(
            tkns,
            vec![
                Token::new(TokenKind::An, "AN"),
                Token::new(TokenKind::Of, "OF"),
                Token::new(TokenKind::Sum, "SUM"),
                Token::new(TokenKind::Diff, "DIFF"),
                Token::new(TokenKind::Produkt, "PRODUKT"),
                Token::new(TokenKind::Quoshunt, "QUOSHUNT"),
                Token::new(TokenKind::Mod, "MOD"),
                Token::new(TokenKind::Biggr, "BIGGR"),
                Token::new(TokenKind::Smallr, "SMALLR"),
            ]
        )
    }

    fn lex(input: &'static str) -> Vec<Token<'static>> {
        Lexer::new(input).collect()
    }
}
