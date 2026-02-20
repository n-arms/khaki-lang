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
    #[token("struct")]
    Struct,
    #[token("True")]
    True,
    #[token("False")]
    False,
    #[token("!")]
    Bang,
    #[token("&")]
    Ampersand,
    #[token("yield")]
    Yield,
}
