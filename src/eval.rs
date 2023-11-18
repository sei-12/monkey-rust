use crate::{ast::{Program, Statement, Expression, PrefixOpe}, obj::Object};

pub enum RuntimeError {
    UnknownPrefixOperator { op: PrefixOpe, obj: Object }
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
        return Err(RuntimeError::UnknownPrefixOperator { op: PrefixOpe::Minus, obj: right });
    };

    Ok(Object::Integer { value: -value })
}

fn eval_prefix_bang(right:Object) -> Result<Object,RuntimeError> {
    let Object::Boolean { value } = right else {
        return Err(RuntimeError::UnknownPrefixOperator { op: PrefixOpe::Bang, obj: right });
    };

    Ok(Object::Boolean { value: !value })
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