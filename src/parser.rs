use std::collections::VecDeque;

use crate::{token::Token, ast::{Program, Statement, Expression}};

#[allow(non_camel_case_types)]
pub enum ParseFault {
    ParseLet_NextNotIdent,
    ParseLet_AssignNotExist
}

impl ParseFault {
    pub fn msg(&self) -> String {
        match self {
            ParseFault::ParseLet_NextNotIdent => String::from("letの次が識別子ではありません"),
            ParseFault::ParseLet_AssignNotExist => String::from("let文に代入式(=)がありません")
        }
    }
}

pub struct Parser {
    tokens : VecDeque<Token>,
    pub faults : Vec<ParseFault>,
}

impl Parser {
    pub fn new(tokens:VecDeque<Token>) -> Self {
        Parser { tokens ,faults: Vec::new() }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();
        loop {
            let Some(cur_token) = self.tokens.pop_front() else { break; };
            if let Some(stmt) = self.parse_stmt(cur_token) {
                program.stmts.push(stmt);
            }
        }
        program
    }

    fn parse_stmt(&mut self,cur_token:Token) -> Option<Statement> {
        match cur_token {
            Token::LET => self.parse_let(),
            _ => None
        }
    }

    fn parse_let(&mut self) -> Option<Statement> {
        let Some(Token::IDENTIFIER(ident_str)) = self.tokens.pop_front() else {
            self.faults.push(ParseFault::ParseLet_NextNotIdent);
            return None;
        };
        if self.tokens.pop_front() != Some(Token::ASSIGN) {
            self.faults.push(ParseFault::ParseLet_AssignNotExist);
            return None;
        };
        // TODO
        while self.tokens.pop_front() != Some(Token::SEMICOLON) {};
        
        let ident = Expression::Ident{value:ident_str};
        let value = Expression::Decoy;
        Some(Statement::Let { ident, value })
    }
}


#[cfg(test)]
mod test_parser {

    use crate::{lexer::analyze_lexical, ast::{Statement, Expression}};

    use super::Parser;

    #[test]
    fn let_statement(){
        test_let_statement("let foo = 10;", "foo", "decoy");
        test_let_statement("let xxx = 10;", "xxx", "decoy");
        test_let_statement("let x = x;", "x", "decoy");
        
    }

    fn test_let_statement(input:&str,ident:&str,exp_literal:&str) {
        let tokens = analyze_lexical(String::from(input));
        let mut parser = Parser::new(tokens);
        let mut program = parser.parse_program();
        let Some(Statement::Let { ident: p_ident, value }) = program.stmts.pop() else {
            panic!("Statement::Letではない!! input:{}",input)
        };
        if let Expression::Ident { value } = p_ident {
            assert_eq!(value,ident,"input:\"{}\"",input);
        }else{
            panic!("identがExpression::Identではない!! input:\"{}\"",input)
        }
        assert_eq!(value.token_literal(),exp_literal,"input:\"{}\"",input);
    }
}