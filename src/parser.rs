use std::collections::VecDeque;

use crate::{token::Token, ast::{Program, Statement, Expression, PrefixOpe, InfixOpe}};

#[allow(non_camel_case_types)]
pub enum ParseFault {
    ParseLet_NextNotIdent,
    NoPrefixParseFn(Token),
    ExpectPopFront { expect: Token, got: Option<Token> },
    NextNotExist,
}

impl ParseFault {
    pub fn msg(&self) -> String {
        match self {
            ParseFault::ParseLet_NextNotIdent => String::from("letの次が識別子ではありません"),
            ParseFault::NoPrefixParseFn(tkn) => format!("{:?}に対応する関数は見当たりません",tkn),
            ParseFault::ExpectPopFront { expect, got } => {
                format!("期待されたのは\"{:?}\" ですが実際は\"{:?}\"でした",expect,got)
            },
            ParseFault::NextNotExist => {
                format!("トークンが足りません")
            }
        }
    }
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

impl Precedence {
    fn from_op(op:&InfixOpe) -> Self {
        match op {
            InfixOpe::Eq | InfixOpe::NotEq => Self::Equals,
            InfixOpe::LT | InfixOpe::GT    => Self::LessGreater,
            InfixOpe::Plus | InfixOpe::Minus => Self::Sum,
            InfixOpe::Asterisk | InfixOpe::Slash => Self::Product
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
            match self.parse_stmt(cur_token) {
                Ok(stmt) => program.stmts.push(stmt),
                Err(fault) => self.faults.push(fault),
            }
        }

        program
    }

    fn parse_stmt(&mut self,cur_token:Token) -> Result<Statement,ParseFault> {
        match cur_token {
            Token::LET => self.parse_let(),
            Token::RETURN => self.parse_return(),
            _ => self.parse_expression_statement(cur_token)
        }
    }

    fn parse_return(&mut self) -> Result<Statement,ParseFault> {
        // TODO
        while self.tokens.pop_front() != Some(Token::SEMICOLON) {};
        Ok(Statement::Return { value: Expression::Decoy })
    }

    fn parse_let(&mut self) -> Result<Statement,ParseFault> {

        let Token::IDENTIFIER(ident_str) = self.get_next_token()? else {
            return Err(ParseFault::ParseLet_NextNotIdent);
        };

        self.expect_pop_front(Token::ASSIGN)?;
        // TODO
        while self.tokens.pop_front() != Some(Token::SEMICOLON) {};
        
        let ident = Expression::Ident{value:ident_str};
        let value = Expression::Decoy;
        Ok(Statement::Let { ident, value })
    }

    fn parse_expression_statement(&mut self,cur_token:Token) -> Result<Statement,ParseFault> {
        let exp = self.parse_exp(Precedence::Lowest,cur_token)?;
        if self.tokens.front() == Some(&Token::SEMICOLON) {
            self.tokens.pop_front();
        };

        return Ok(Statement::Expression { exp });
    }
    
    fn parse_group_expression(&mut self) -> Result<Expression,ParseFault> {
        let cur = self.get_next_token()?;
        let exp = self.parse_exp(Precedence::Lowest, cur)?;
        self.expect_pop_front(Token::RPAREN)?;
        Ok(exp)
    }

    fn parse_exp(&mut self,prec:Precedence,cur_token:Token) -> Result<Expression,ParseFault> {
        let mut left_exp = match cur_token {
            Token::IDENTIFIER(value) => Ok(Expression::Ident { value }),
            Token::INTEGER(value)     => Ok(Expression::Integer { value }),
            Token::TRUE | Token::FALSE       => Ok(Expression::Boolean { value: cur_token == Token::TRUE }),
            Token::BANG | Token::MINUS       => self.parse_prefix_expression(cur_token),
            Token::LPAREN                    => self.parse_group_expression(),
            Token::IF                        => self.parse_if_expression(),
            _                                => Err(ParseFault::NoPrefixParseFn(cur_token))
        }?;

        loop {
            if self.tokens.front() == Some(&Token::SEMICOLON) { break; };
            let Some(front) = self.tokens.front() else { break; };
            let Some(in_op) = InfixOpe::from_tkn(front) else { break; };
            let front_prec = Precedence::from_op(&in_op);
            if prec >= front_prec { break; }
            // 次のトークンが中置演算子でかついまの演算子より優先度が高い

            self.tokens.pop_front();
            let new_left = self.parse_infix_expression(in_op, left_exp)?;
            left_exp = new_left;
        };

        Ok(left_exp)
    }

    fn expect_pop_front(&mut self,token:Token) -> Result<(),ParseFault> {
        let front = self.tokens.pop_front();
        if let Some(front) = front {
            if front == token {
                Ok(())
            }else{
                Err(ParseFault::ExpectPopFront { expect: token, got: Some(front) })
            }
        }else{
            Err(ParseFault::ExpectPopFront { expect: token, got: None })
        }
    }

    fn get_next_token(&mut self) -> Result<Token,ParseFault> {
        match self.tokens.pop_front() {
            Some(token) => Ok(token),
            None => Err(ParseFault::NextNotExist)
        }
    }

    fn parse_if_expression(&mut self) -> Result<Expression,ParseFault> {
        self.expect_pop_front(Token::LPAREN)?;
        let cur_token = self.get_next_token()?;
        let condition= self.parse_exp(Precedence::Lowest, cur_token)?;
        self.expect_pop_front(Token::RPAREN)?;
        self.expect_pop_front(Token::LBRACE)?;
        let cons = self.parse_block_statement();
        let mut alt = None;
        if self.tokens.front() == Some(&Token::ELSE) {
            self.tokens.pop_front();
            self.expect_pop_front(Token::LBRACE)?;
            alt = Some(Box::new(self.parse_block_statement()));
        };
        Ok(Expression::If { 
            condition:Box::new(condition), 
            consequence:Box::new(cons), 
            alternative: alt 
        })
    }

    fn parse_block_statement(&mut self) -> Statement {
        let mut stmts = Vec::new();
        let mut cur = self.tokens.pop_front();
        while cur != Some(Token::RBRACE) && cur != None {
            match self.parse_stmt(cur.expect("パニックになるわけない")) {
                Ok(stmt) => stmts.push(stmt),
                Err(falult) => self.faults.push(falult)
            };
            cur = self.tokens.pop_front();
        }
        Statement::Block { stmts }
    }

    fn parse_prefix_expression(&mut self,cur_token:Token) -> Result<Expression,ParseFault> {
        let next_tkn= self.get_next_token()?;
        let right= self.parse_exp(Precedence::Prefix, next_tkn)?;
        let ope = match cur_token {
            Token::BANG => PrefixOpe::Bang,
            Token::MINUS => PrefixOpe::Minus,
            _ => panic!("修正必須のバグを発見!!")
        };

        return Ok(Expression::Prefix { ope, right: Box::new(right) });
    }

    fn parse_infix_expression(&mut self,ope:InfixOpe,left:Expression) -> Result<Expression,ParseFault> {
        let next_tkn = self.get_next_token()?;
        let right= self.parse_exp(Precedence::from_op(&ope), next_tkn)?;
        Ok(Expression::Infix { 
            left: Box::new(left), 
            ope, 
            right: Box::new(right)
        })
    }
}


#[cfg(test)]
mod test_parser {

    use crate::{lexer::analyze_lexical, ast::{Statement, Expression, PrefixOpe, InfixOpe}};

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
        !1;
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
        test_prefix_expression(program.stmts.pop(), PrefixOpe::Bang, "(!x)");
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
            panic!("Expression::Prefixではない {:?}",exp);
        };

        assert_eq!(ope,expect_ope);
        assert_eq!(right.string(),right_str);
    }

    #[test]
    fn infix_expression(){
        let input = String::from("\
        5 + 5;
        5 - 5;
        5 * 5;
        5 / 5;
        5 < 5
        foo > foo
        foo == foo
        foo != foo
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
        test_infix_expression(program.stmts.pop(), "foo", InfixOpe::NotEq, "foo");
        test_infix_expression(program.stmts.pop(), "foo", InfixOpe::Eq, "foo");
        test_infix_expression(program.stmts.pop(), "foo", InfixOpe::GT, "foo");
        test_infix_expression(program.stmts.pop(), "5", InfixOpe::LT, "5");
        test_infix_expression(program.stmts.pop(), "5", InfixOpe::Slash, "5");
        test_infix_expression(program.stmts.pop(), "5", InfixOpe::Asterisk, "5");
        test_infix_expression(program.stmts.pop(), "5", InfixOpe::Minus, "5");
        test_infix_expression(program.stmts.pop(), "5", InfixOpe::Plus, "5");
        assert!(program.stmts.pop().is_none());
    }

    fn test_infix_expression(stmt:Option<Statement>,left_str:&str,expect_ope:InfixOpe,right_str:&str){
        let Some(Statement::Expression { exp }) = stmt else {
            panic!("Statement::Expressionではない");
        };

        let Expression::Infix { left, ope, right } = exp else {
            panic!("Expression::Infixではない");
        };

        assert_eq!(ope,expect_ope);
        assert_eq!(right.string(),right_str);
        assert_eq!(left.string(),left_str);
    }

    #[test]
    fn operator_precedence_pasing(){
        test_program("-a * b", "((-a) * b)");
        test_program("!-a", "(!(-a))");
        test_program("a + b + c", "((a + b) + c)");
        test_program("a + b * c", "(a + (b * c))");
        test_program("a * b * c", "((a * b) * c)");
        test_program("a / b * c", "((a / b) * c)");
        test_program("a - b / c", "(a - (b / c))");
        test_program("d * a + b * c", "((d * a) + (b * c))");
        test_program("d + a + b * c", "((d + a) + (b * c))");
        test_program("d > a == b < c", "((d > a) == (b < c))");
        test_program("d > a != b < c", "((d > a) != (b < c))");
        test_program("d > a ; b < c", "(d > a)(b < c)");
        test_program("d > -a != b < c", "((d > (-a)) != (b < c))");
        test_program("(1 + 1) * (2 + 2)", "((1 + 1) * (2 + 2))");
        test_program("-(5 + 5)", "(-(5 + 5))");        
    }

    #[test]
    fn bool(){
        test_program("true", "true");
        test_program("false", "false");
        test_program("3 > 5 == false", "((3 > 5) == false)");
    }

    fn test_program(input:&str,out_str:&str){
        let tokens = analyze_lexical(String::from(input));
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program();
        assert_eq!(program.string(),out_str);
    }

    #[test]
    fn if_expression(){
        let input = String::from("\
        if ( a == b ) { x }
        if ( (a + b) > c ) { aaa }
        if ( a == b ) { x } else { y }
        if ( (a + b) > c ) { aaa } else { bbb }
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
        test_if_expression(program.stmts.pop(), "((a + b) > c)", "aaa", Some("bbb"));
        test_if_expression(program.stmts.pop(), "(a == b)", "x", Some("y"));
        test_if_expression(program.stmts.pop(), "((a + b) > c)", "aaa", None);
        test_if_expression(program.stmts.pop(), "(a == b)", "x", None);
        
        assert!(program.stmts.pop().is_none());
    }

    fn test_if_expression(stmt:Option<Statement>,cond:&str,cons:&str,alt:Option<&str>){
        let Some(Statement::Expression { exp }) = stmt else {
            panic!("Statement::Expressionではない");
        };

        let Expression::If { condition, consequence, alternative } = exp else {
            panic!("Expression::Ifではない");
        };

        assert_eq!(condition.string(),cond);
        assert_eq!(consequence.string(),cons);

        let Some(alt) = alt else { return; };
        let s_alt = alternative.expect("alternative is none");
        assert_eq!(alt,s_alt.string());
    }

    
}