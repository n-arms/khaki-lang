use core::fmt;

use logos::Logos;

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum TokenKind {
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("[")]
    LeftSquare,
    #[token("]")]
    RightSquare,
    #[token("=")]
    Equals,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token(".")]
    Dot,
    #[regex("[_a-z][A-Za-z0-9_]*")]
    Name,
    #[regex("[A-Z][A-Za-z0-9_]*")]
    UpperName,
    #[regex("[0-9]+")]
    Number,
    #[token("func")]
    Func,
    #[token("cor")]
    Cor,
    #[token("let")]
    Let,
    #[token("set")]
    Set,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("struct")]
    Struct,
    #[token("True")]
    True,
    #[token("False")]
    False,
    #[token("!")]
    Bang,
    #[token("yield")]
    Yield,
    #[token("any")]
    Any,
    #[token("open")]
    Open,
    #[token("&")]
    Ampersand,
    #[token("*")]
    Star,
    #[token("+")]
    Plus,
    #[token("-")]
    Dash,
    #[token("/")]
    Slash,
    #[token("<<")]
    ShiftLeft,
    #[token(">>")]
    ShiftRight,
    #[token("|")]
    BitOr,
    #[token("~")]
    BitNot,
    #[token("^")]
    BitXor,
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GreaterEqual,
    #[token("<")]
    LessThan,
    #[token("<=")]
    LessEqual,
    #[token("==")]
    DoubleEquals,
    #[token("!=")]
    NotEquals,
    #[token("&&")]
    LogicAnd,
    #[token("||")]
    LogicOr,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LeftBrace => write!(f, "{{"),
            TokenKind::RightBrace => write!(f, "}}"),
            TokenKind::LeftSquare => write!(f, "["),
            TokenKind::RightSquare => write!(f, "]"),
            TokenKind::Equals => write!(f, "="),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::DoubleColon => write!(f, "::"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Name => write!(f, "<name>"),
            TokenKind::UpperName => write!(f, "<TypeName>"),
            TokenKind::Number => write!(f, "<integer>"),
            TokenKind::Func => write!(f, "func"),
            TokenKind::Cor => write!(f, "cor"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Set => write!(f, "set"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Then => write!(f, "then"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::While => write!(f, "while"),
            TokenKind::Struct => write!(f, "struct"),
            TokenKind::True => write!(f, "True"),
            TokenKind::False => write!(f, "False"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Yield => write!(f, "yield"),
            TokenKind::Any => write!(f, "any"),
            TokenKind::Open => write!(f, "open"),
            TokenKind::Ampersand => write!(f, "&"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Dash => write!(f, "-"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::ShiftLeft => write!(f, "<<"),
            TokenKind::ShiftRight => write!(f, ">>"),
            TokenKind::BitOr => write!(f, "|"),
            TokenKind::BitNot => write!(f, "~"),
            TokenKind::BitXor => write!(f, "^"),
            TokenKind::GreaterThan => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::LessThan => write!(f, "<"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::DoubleEquals => write!(f, "=="),
            TokenKind::NotEquals => write!(f, "!="),
            TokenKind::LogicAnd => write!(f, "&&"),
            TokenKind::LogicOr => write!(f, "||"),
        }
    }
}
