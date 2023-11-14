pub enum Token {
    ILLEGAL,
    EOF,

    IDENTIFIER(String),
    INTEGER(usize),

    ASSIGN,
    PLUS,

    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET
}