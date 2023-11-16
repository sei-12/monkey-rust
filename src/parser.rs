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
            Token::RETURN => self.parse_return(),
            _ => None
        }
    }

    fn parse_return(&mut self) -> Option<Statement> {
        // TODO
        while self.tokens.pop_front() != Some(Token::SEMICOLON) {};
        Some(Statement::Return { value: Expression::Decoy })
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
        let input = String::from("\
        let a = 10;\
        let b = c;\
        let aaa = 11;\
        ");

        let tokens = analyze_lexical(input);
        let mut parser = Parser::new(tokens);
        let mut program = parser.parse_program();
        assert_eq!(parser.faults.len(),0);
        test_let_statement(program.stmts.pop(), "aaa", "decoy");
        test_let_statement(program.stmts.pop(), "b", "decoy");
        test_let_statement(program.stmts.pop(), "a", "decoy");
        assert!(program.stmts.pop().is_none());
    }

    fn test_let_statement(stmt:Option<Statement>,ident:&str,exp_literal:&str) {
        let Some(Statement::Let { ident: p_ident, value }) = stmt else {
            panic!("Statement::Letではない!!")
        };
        if let Expression::Ident { value } = p_ident {
            assert_eq!(value,ident);
        }else{
            panic!("identがExpression::Identではない!!")
        }
        assert_eq!(value.string(),exp_literal);
    }

    #[test]
    fn return_statement(){
        let input = String::from("\
        return a;\
        return b;\
        return 10;\
        ");

        let tokens = analyze_lexical(input);
        let mut parser = Parser::new(tokens);
        let mut program = parser.parse_program();
        assert_eq!(parser.faults.len(),0);
        // Vecなので後ろから順にテストしていく
        test_return_statement(program.stmts.pop(), "decoy");
        test_return_statement(program.stmts.pop(), "decoy");
        test_return_statement(program.stmts.pop(), "decoy");
        assert!(program.stmts.pop().is_none());
    }

    fn test_return_statement(stmt:Option<Statement>,val_literal:&str){
        let Some(Statement::Return { value }) = stmt else {
            panic!("Statement::Returnではない");
        };
        assert_eq!(value.string(),val_literal);
    }
}