use std::{rc::Rc, cell::RefCell};

use crate::{ast::{Program, Statement, Expression, PrefixOpe, InfixOpe}, obj::{Object, ObjectType}, env::Environment};

#[derive(Debug)]
pub enum RuntimeError {
    UnknownPrefixOperator { ope: PrefixOpe, obj: Object },
    UnknownInfixOperator { ope: InfixOpe, left: Object, right: Object},
    TodoRename1, // ifの()の中が真偽値ではない
    TodoRename2, // BlockStatementではない
    IdentNotFound,
    NotAFunction,
}

impl RuntimeError {
    pub fn msg(&self) -> String {
        format!("{:?}",self)
    }
}

type Env = Rc<RefCell<Environment>>;

pub fn eval_program(program:Program,env:Env) -> Result<Object,RuntimeError> {
    let mut result = Object::Null;
    for stmt in program.stmts {
        result = eval_statement(stmt,env.clone())?;
        if result.obj_type() == ObjectType::Return {
            let Object::Return { value } = result else { panic!("ありえない") };
            return Ok(*value);
        }
    }

    Ok(result)
}

fn eval_statement(stmt:Statement,env:Env) -> Result<Object,RuntimeError> {
    match stmt {
        Statement::Expression { exp } => eval_expression(exp,env),
        Statement::Return { value } => eval_return_stmt(value,env),
        Statement::Let { ident, value } => eval_let_stmt(ident, value, env),
        Statement::Block { stmts } => eval_block_stmts(stmts, env.clone())
    }
}

fn eval_let_stmt(ident:Expression,value:Expression,env:Env) -> Result<Object,RuntimeError>{
    let Expression::Ident { value:ident_str } = ident else {
        // TODO
        todo!("たしか構文解析の時点でチェック済みなのでありえない。いつかParseErrorからRuntimeErrorに変える")
    };
    let val = eval_expression(value,env.clone())?;
    env.borrow_mut().set(ident_str, val);
    Ok(Object::Null)
}

fn eval_identifier(ident:&String,env:Env) -> Result<Object,RuntimeError> {
    match env.borrow().get(ident) {
        Some(val) => Ok(val),
        None => Err(RuntimeError::IdentNotFound)
    }
}

fn eval_expression(exp:Expression,env:Env) -> Result<Object,RuntimeError> {
    match exp {
        Expression::Integer { value } => Ok(Object::Integer { value: value as isize }),
        Expression::Boolean { value } => Ok(Object::Boolean { value }),
        Expression::Prefix { ope, right } => eval_prefix_exp(ope, right,env),
        Expression::Infix { left, ope, right } => eval_infix_exp(left, ope, right,env),
        Expression::If { condition, consequence, alternative } => {
            eval_if_exp(condition, consequence, alternative,env)
        },
        Expression::Ident { value } => eval_identifier(&value,env),
        Expression::Function { params, body } => {
            Ok(Object::Function { params, body:*body , env: env.clone() })
        },
        Expression::Call { func, args } => {
            eval_call_expression(func, args,env.clone())
        },
    }
}

fn eval_call_expression(func:Box<Expression>,args:Vec<Expression>,env:Env) -> Result<Object,RuntimeError> {
    let func = eval_expression(*func, env.clone())?;
    let args = eval_expressions(args, env.clone())?;
    apply_funciton(func, args)
}

fn apply_funciton(func:Object,args:Vec<Object>) -> Result<Object,RuntimeError> {
    let Object::Function { params, body, env } = func else {
        return Err(RuntimeError::NotAFunction);
    };

    let extended_env = Rc::new(RefCell::new(extend_func_env(env, args, params)));
    let evaluated = eval_statement(body, extended_env)?;
    Ok(unwrap_return_obj(evaluated))
}

fn unwrap_return_obj(obj:Object) -> Object {
    match obj {
        Object::Return { value } => *value,
        _ => obj
    }
}

fn extend_func_env(env:Env,args:Vec<Object>,params:Vec<Expression>) -> Environment {
    let mut env = Environment::new_enclosed_env(env);
    for (param,arg) in params.iter().zip(args) {
        // TODO リファクタリング
        let Expression::Ident { value } = param else { panic!("ありえないはず")};
        env.set(value.clone(), arg);
    };
    env
}

fn eval_expressions(exps:Vec<Expression>,env:Env) -> Result<Vec<Object>,RuntimeError>{
    let mut result = Vec::new();
    for exp in exps {
        result.push(eval_expression(exp, env.clone())?);
    }
    Ok(result)
}
fn eval_return_stmt(val:Expression,env:Env) -> Result<Object,RuntimeError>{
    let value = eval_expression(val,env)?;
    Ok(Object::Return { value:Box::new(value) })
}

fn eval_if_exp(condition: Box<Expression>, consequence: Box<Statement>, alternative: Option<Box<Statement>>,env:Env) -> Result<Object,RuntimeError>{
    let Object::Boolean { value:cond } = eval_expression(*condition,env.clone())? else {
        return Err(RuntimeError::TodoRename1);
    };

    if cond {
        match *consequence {
            Statement::Block { stmts } => eval_block_stmts(stmts,env),
            _ => Err(RuntimeError::TodoRename2)
        }
    }else{
        let Some(alt) = alternative else { return Ok(Object::Null); };
        match *alt {
            Statement::Block { stmts } => eval_block_stmts(stmts,env),
            _ => Err(RuntimeError::TodoRename2)
        }
    }
}

fn eval_block_stmts(stmts:Vec<Statement>,env:Env) -> Result<Object,RuntimeError>{
    let mut result = Object::Null;

    for stmt in stmts {
        result = eval_statement(stmt,env.clone())?;
        if result.obj_type() == ObjectType::Return { break; };
    }

    Ok(result)
}

fn eval_prefix_exp(ope:PrefixOpe,right:Box<Expression>,env:Env) -> Result<Object,RuntimeError> {
    let right_obj = eval_expression(*right,env)?;
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

fn eval_infix_exp(left:Box<Expression>, ope: InfixOpe, right: Box<Expression> ,env:Env) -> Result<Object,RuntimeError> {
    let left = eval_expression(*left,env.clone())?;
    let right = eval_expression(*right,env.clone())?;

    if left.obj_type() == ObjectType::Integer && right.obj_type() == ObjectType::Integer {
        eval_integer_infix_exp(left, ope, right)
    }
    else if left.obj_type() == ObjectType::Boolean && right.obj_type() == ObjectType::Boolean {
        eval_boolean_infix_exp(left, ope, right)
    }
    else{
        Err(RuntimeError::UnknownInfixOperator { ope, left, right })
    }
}

fn eval_integer_infix_exp(left:Object,ope: InfixOpe,right:Object) -> Result<Object,RuntimeError> {
    let Object::Integer { value:left } = left  else {panic!("バグ")};
    let Object::Integer { value:right } = right else {panic!("バグ")};

    let ret_obj = match ope {
        InfixOpe::Plus     => Object::Integer { value: left +  right },
        InfixOpe::Minus    => Object::Integer { value: left -  right },
        InfixOpe::Slash    => Object::Integer { value: left /  right },
        InfixOpe::Asterisk => Object::Integer { value: left *  right },
        InfixOpe::Eq       => Object::Boolean { value: left == right },
        InfixOpe::NotEq    => Object::Boolean { value: left != right },
        InfixOpe::GT       => Object::Boolean { value: left >  right },
        InfixOpe::LT       => Object::Boolean { value: left <  right }
    };

    Ok(ret_obj)
}

fn eval_boolean_infix_exp(left:Object,ope:InfixOpe,right:Object) -> Result<Object,RuntimeError> {
    let Object::Boolean { value:left } = left  else {panic!("バグ")};
    let Object::Boolean { value:right } = right else {panic!("バグ")};

    let ret_val = match ope {
        InfixOpe::Eq => left == right,
        InfixOpe::NotEq => left != right,
        _ => { return Err(
            RuntimeError::UnknownInfixOperator { 
                ope, left: Object::Boolean { value: left }, right: Object::Boolean { value: right }
            }
        );}
    };

    Ok(Object::Boolean { value: ret_val })
}


#[cfg(test)]
mod eval_tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::{lexer::analyze_lexical, parser::Parser, env::Environment};

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
        test_program("1 < 2", "true");
        test_program("2 == 1 + 1", "true");
        test_program("1 > 2 * 2", "false");
        test_program("true != false", "true");
        test_program("(1 == 2) != (1 != 2)", "true");
        test_program("if ( true ) { 10 }", "10");
        test_program("if ( false ) { 10 }", "null");
        test_program("if ( false ) { 20 } else { 10 }", "10");
        test_program("if ( 1 == 1 ) { 10 }", "10");
        test_program("if ( 1 != 1 ) { 10 }", "null");
        test_program("if ( 1 == 1 ) { 10;true }", "true");
        test_program("10;return 11;12;", "11");
        test_program("\
        if ( true ){
            if ( true ){
                return 10;
            }
            return 11;
        }
        ", "10");
        test_program("let a = 5;a;", "5");
        test_program("let a = 5 * 5", "null");
        test_program("let a = 5 * 5;a", "25");
        test_program("let a = 5 * 5;let b = a + a;b", "50");
        test_program("fn (x,y) { return x; }", "fn (x,y) { return x; }");
        test_program("let a = fn (x,y) { return x; };a", "fn (x,y) { return x; }");
        test_program("\
        let sum = fn ( x,y ){
            x + y
        }
        sum(5,5)
        ", "10");
        test_program("\
        let sum = fn ( x,y ){
            x + y
        }
        sum(sum(10,10),sum(10,10))
        ", "40");
        test_program("fn (x,y){x + y}(20,20)", "40");

    }

    
    fn test_program(input:&str,expect:&str){
        let tokens = analyze_lexical(String::from(input));
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program();
        let env = Rc::new(RefCell::new(Environment::new()));

        if parser.faults.len() > 0 {
            for f in &parser.faults {
                println!("{}",f.msg());
            }
            panic!();
        }

        match eval_program(program,env) {
            Ok(obj) => assert_eq!(obj.inspect(),expect),
            Err(err) => panic!("{}",err.msg())
        }

    }
}