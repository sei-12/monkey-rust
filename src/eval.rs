use crate::{ast::{Program, Statement, Expression, PrefixOpe, InfixOpe}, obj::Object};

pub enum RuntimeError {
    UnknownPrefixOperator { ope: PrefixOpe, obj: Object },
    UnknownInfixOperator { ope: InfixOpe, left: Object, right: Object}
}

impl RuntimeError {
    pub fn msg(&self) -> String {
        todo!();
    }
}

pub fn eval_program(program:Program) -> Result<Object,RuntimeError> {
    let mut result = Ok(Object::Null);

    for stmt in program.stmts {
        result = eval_statement(stmt);
    }

    result
}

fn eval_statement(stmt:Statement) -> Result<Object,RuntimeError> {
    match stmt {
        Statement::Expression { exp } => eval_expression(exp),
        _ => panic!("未実装")
    }
}

fn eval_expression(exp:Expression) -> Result<Object,RuntimeError> {
    match exp {
        Expression::Integer { value } => Ok(Object::Integer { value: value as isize }),
        Expression::Boolean { value } => Ok(Object::Boolean { value }),
        Expression::Prefix { ope, right } => eval_prefix_exp(ope, right),
        Expression::Infix { left, ope, right } => eval_infix_exp(left, ope, right),
        _ => panic!("未実装")
    }
}

fn eval_prefix_exp(ope:PrefixOpe,right:Box<Expression>) -> Result<Object,RuntimeError> {
    let right_obj = eval_expression(*right)?;
    match ope {
        PrefixOpe::Bang => eval_prefix_bang(right_obj),
        PrefixOpe::Minus => eval_prefix_minus(right_obj)
    }
}

fn eval_prefix_minus(right:Object) -> Result<Object,RuntimeError> {
    let Object::Integer { value } = right else {
        return Err(RuntimeError::UnknownPrefixOperator { ope: PrefixOpe::Minus, obj: right });
    };

    Ok(Object::Integer { value: -value })
}

fn eval_prefix_bang(right:Object) -> Result<Object,RuntimeError> {
    let Object::Boolean { value } = right else {
        return Err(RuntimeError::UnknownPrefixOperator { ope: PrefixOpe::Bang, obj: right });
    };

    Ok(Object::Boolean { value: !value })
}

fn eval_infix_exp(left:Box<Expression>, ope: InfixOpe, right: Box<Expression> ) -> Result<Object,RuntimeError> {
    let left = eval_expression(*left)?;
    let right = eval_expression(*right)?;

    if left.is_int() && right.is_int() {
        eval_integer_infix_exp(left, ope, right)
    }else{
        Err(RuntimeError::UnknownInfixOperator { ope, left, right })
    }
}

fn eval_integer_infix_exp(left:Object,ope: InfixOpe,right:Object) -> Result<Object,RuntimeError> {
    let Object::Integer { value:left } = left  else {panic!("バグ")};
    let Object::Integer { value:right } = right else {panic!("バグ")};

    let ret_val = match ope {
        InfixOpe::Plus => left + right,
        InfixOpe::Minus => left - right,
        InfixOpe::Slash => left / right,
        InfixOpe::Asterisk => left * right,
        _ => { return Err(
            RuntimeError::UnknownInfixOperator { 
                ope, left: Object::Integer { value: left }, right: Object::Integer { value: right }
            }
        );}
    };

    Ok(Object::Integer { value: ret_val })
}

#[cfg(test)]
mod eval_tests {
    use crate::{lexer::analyze_lexical, parser::Parser};

    use super::eval_program;


    #[test]
    fn program(){
        test_program("5", "5");
        test_program("100;", "100");
        test_program("10", "10");
        test_program("true;", "true");
        test_program("true", "true");
        test_program("false;", "false");
        test_program("false", "false");
        test_program("!true", "false");
        test_program("!false", "true");
        test_program("-1", "-1");
        test_program("-100", "-100");

        test_program("1 + 1", "2");
        test_program("1 * 1", "1");
        test_program("1 + 1 + 2", "4");
        test_program("2 + 2", "4");
        test_program("2 + 2 * 3", "8");
        test_program("(2 + 2) * 3", "12");
        test_program("-2 + 2", "0");
        test_program("-2 + 2 * -1", "-4");
        test_program("-2 + 2 / 2", "-1");
        test_program("100 / 3 * 3", "99");
        test_program("40 / 2 * ( 2 + 2)", "80");

    }

    
    fn test_program(input:&str,expect:&str){
        let tokens = analyze_lexical(String::from(input));
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program();

        if parser.faults.len() > 0 {
            for f in &parser.faults {
                println!("{}",f.msg());
            }
            panic!();
        }

        match eval_program(program) {
            Ok(obj) => assert_eq!(obj.inspect(),expect),
            Err(err) => panic!("{}",err.msg())
        }

    }
}