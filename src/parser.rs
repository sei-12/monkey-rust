use std::collections::VecDeque;

use crate::{token::Token, ast::{Program, Statement, Expression, PrefixOpe}};

#[allow(non_camel_case_types)]
pub enum ParseFault {
    ParseLet_NextNotIdent,
    ParseLet_AssignNotExist,
    NoPrefixParseFn(Token),
    ParsePrefix_NextNotExist,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl ParseFault {
    pub fn msg(&self) -> String {
        match self {
            ParseFault::ParseLet_NextNotIdent => String::from("letの次が識別子ではありません"),
            ParseFault::ParseLet_AssignNotExist => String::from("let文に代入式(=)がありません"),
            ParseFault::NoPrefixParseFn(tkn) => format!("no prefix parse function for {:?} found",tkn),
            ParseFault::ParsePrefix_NextNotExist => String::from("前置演算子の次にトークンがありません"),
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
            _ => self.parse_expression_statement(cur_token)
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

    fn parse_expression_statement(&mut self,cur_token:Token) -> Option<Statement> {
        let Some(exp) = self.parse_exp(Precedence::Lowest,cur_token) else {
            return None;
        };
        if self.tokens.front() == Some(&Token::SEMICOLON) {
            self.tokens.pop_front();
        };
        return Some(Statement::Expression { exp });
    }
    
    fn parse_exp(&mut self,_prec:Precedence,cur_token:Token) -> Option<Expression> {
        match cur_token {
            Token::IDENTIFIER(ident) => Some(self.parse_identifier(ident)),
            Token::INTEGER(value) => Some(self.parge_integer(value)),
            Token::BANG | Token::MINUS => self.parse_prefix_expression(cur_token),
            _ => {
                self.faults.push(ParseFault::NoPrefixParseFn(cur_token));
                None
            }
        }
    }

    fn parse_identifier(&self,ident:String) -> Expression {
        Expression::Ident { value: ident }
    }
    fn parge_integer(&self,value:usize) -> Expression {
        Expression::Integer { value }
    }

    fn parse_prefix_expression(&mut self,cur_token:Token) -> Option<Expression> {
        let Some(next_tkn) = self.tokens.pop_front() else {
            self.faults.push(ParseFault::ParsePrefix_NextNotExist);
            return None;
        };

        let Some(right) = self.parse_exp(Precedence::Prefix, next_tkn)else{
            return None;
        };

        let ope = match cur_token {
            Token::BANG => PrefixOpe::Bang,
            Token::MINUS => PrefixOpe::Minus,
            _ => panic!("修正必須のバグを発見!!")
        };

        return Some(Expression::Prefix { ope, right: Box::new(right) });
    }
}


#[cfg(test)]
mod test_parser {

    use crate::{lexer::analyze_lexical, ast::{Statement, Expression, PrefixOpe}};

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

    #[test]
    fn identifier_exp(){
        let input = String::from("foo;");

        let tokens = analyze_lexical(input);
        let mut parser = Parser::new(tokens);
        let mut program = parser.parse_program();
        assert_eq!(parser.faults.len(),0);
        // Vecなので後ろから順にテストしていく
        test_identifier_expression(program.stmts.pop(), "foo");
        assert!(program.stmts.pop().is_none());
    }

    fn test_identifier_expression(stmt:Option<Statement>,ident:&str){
        let Some(Statement::Expression { exp }) = stmt else {
            panic!("Statement::Expressionではない");
        };

        let Expression::Ident { value } = exp else {
            panic!("Expression::Identではない")
        };

        assert_eq!(value,ident);
    }

    #[test]
    fn integer_exp(){
        let input = String::from("\
        100;
        200;
        1;
        2
        4
        ");

        let tokens = analyze_lexical(input);
        let mut parser = Parser::new(tokens);
        let mut program = parser.parse_program();
        assert_eq!(parser.faults.len(),0);
        // Vecなので後ろから順にテストしていく
        test_integer_expression(program.stmts.pop(), 4);
        test_integer_expression(program.stmts.pop(), 2);
        test_integer_expression(program.stmts.pop(), 1);
        test_integer_expression(program.stmts.pop(), 200);
        test_integer_expression(program.stmts.pop(), 100);
        assert!(program.stmts.pop().is_none());
    }

    fn test_integer_expression(stmt:Option<Statement>,val:usize){
        let Some(Statement::Expression { exp }) = stmt else {
            panic!("Statement::Expressionではない");
        };

        let Expression::Integer { value } = exp else {
            panic!("Expression::Integerではない");
        };

        assert_eq!(value,val);
    }

    #[test]
    fn prefix_expressions(){
        let input = String::from("\
        -1;
        !1;
        -xx;
        !xx;
        -1
        !1
        -xx
        !xx
        !!x
        ");

        let tokens = analyze_lexical(input);
        let mut parser = Parser::new(tokens);
        let mut program = parser.parse_program();

        if parser.faults.len() > 0 {
            for f in &parser.faults {
                println!("{}",f.msg());
            }
            panic!();
        }
        // Vecなので後ろから順にテストしていく
        test_prefix_expression(program.stmts.pop(), PrefixOpe::Bang, "!x");
        test_prefix_expression(program.stmts.pop(), PrefixOpe::Bang, "xx");
        test_prefix_expression(program.stmts.pop(), PrefixOpe::Minus, "xx");
        test_prefix_expression(program.stmts.pop(), PrefixOpe::Bang, "1");
        test_prefix_expression(program.stmts.pop(), PrefixOpe::Minus, "1");
        test_prefix_expression(program.stmts.pop(), PrefixOpe::Bang, "xx");
        test_prefix_expression(program.stmts.pop(), PrefixOpe::Minus, "xx");
        test_prefix_expression(program.stmts.pop(), PrefixOpe::Bang, "1");
        test_prefix_expression(program.stmts.pop(), PrefixOpe::Minus, "1");
        assert!(program.stmts.pop().is_none());
    }

    fn test_prefix_expression(stmt:Option<Statement>,expect_ope:PrefixOpe,right_str:&str) {
        let Some(Statement::Expression { exp }) = stmt else {
            panic!("Statement::Expressionではない");
        };

        let Expression::Prefix { ope, right } = exp else {
            panic!("Expression::Prefixではない");
        };

        assert_eq!(ope,expect_ope);
        assert_eq!(right.string(),right_str);
    }

    
}