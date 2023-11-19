use std::{rc::Rc, cell::RefCell};

use crate::{ast::{Expression, Statement}, env::Environment};

#[derive(PartialEq,Debug,Clone)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
    Return,
    Function
}

#[derive(Clone,Debug)]
pub enum Object {
    Integer { value: isize },
    Boolean { value: bool },
    Null,
    Return { value: Box<Object> },
    Function { params: Vec<Expression>,body: Statement, env:Rc<RefCell<Environment>> }
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer { value } => format!("{}",value),
            Self::Boolean { value } => format!("{}",value),
            Self::Null => format!("null"),
            Self::Return { value } => value.inspect(),
            Self::Function { params, body, env:_ } => {
                let param_strs :Vec<String> = params.iter().map(|x|x.string()).collect();
                format!("fn ({}) {}",param_strs.join(","),body.string())
            }
        }
    }

    pub fn obj_type(&self) -> ObjectType {
        match self {
            Self::Integer { value:_ } => ObjectType::Integer,
            Self::Boolean { value:_ } => ObjectType::Boolean,
            Self::Null => ObjectType::Null,
            Self::Return { value :_ } => ObjectType::Return,
            Self::Function { params:_, body:_, env:_ } => ObjectType::Function
        }
    }
}