use crate::token::{Token, TokenKind};

pub struct Lexer<'a> {
    input: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    /// returns None when tokenizing has failed.
    /// if the lexer has eached the end of the input, it will return the Eof token
    pub fn next_token(&mut self) -> Option<Token<'a>> {
        if self.input.is_empty() {
            return Some(self.new_token(0, TokenKind::Eof));
        }

        let methods = [
            Self::lex_newline,
            Self::lex_ident,
            Self::lex_type,
            Self::lex_keyword,
            Self::lex_bool_lit,
            Self::lex_float_lit,
            Self::lex_int_lit,
        ];

        self.skip_whitespace();
        let token = methods.into_iter().find_map(|method| method(self))?;
        self.consume(token.text().len());
        Some(token)
    }

    fn consume(&mut self, len: usize) {
        self.input = &self.input[len..]
    }

    fn skip_whitespace(&mut self) {
        // not skipping newline since it's considered to be a token
        let len = self.count_while(|c| *c != '\n' && c.is_ascii_whitespace());
        self.consume(len)
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
            ("KTHXBYE", KThxBye),
            ("HAS A", HasA),
            ("HAI", Hai),
            ("ITZ", Itz),
            ("I", I),
            ("A", A),
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

    fn peek(&self) -> char {
        self.input.chars().next().unwrap_or_default()
    }

    fn count_while(&self, pred: impl FnMut(&char) -> bool) -> usize {
        self.input.chars().take_while(pred).count()
    }

    fn starts_with(&self, text: &str) -> bool {
        self.input.starts_with(text)
    }

    fn new_token(&self, len: usize, kind: TokenKind) -> Token<'a> {
        Token::new(kind, &self.input[..len])
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().filter(|tkn| tkn.kind() != TokenKind::Eof)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenKind::*;

    #[test]
    pub fn keywords() {
        let input = "HAI I HAS A KTHXBYE ITZ A";
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
        let input = "WIN FAIL 209348 -2948 0.234 -1234234.3";
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
            ]
        )
    }

    fn lex(input: &'static str) -> Vec<Token<'static>> {
        Lexer::new(input).collect()
    }
}
