
pub enum Object {
    Integer { value: isize },
    Boolean { value: bool },
    Null,
    Return { value: Box<Object> }
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer { value } => format!("{}",value),
            Self::Boolean { value } => format!("{}",value),
            Self::Null => format!("null"),
            Self::Return { value } => value.inspect()
        }
    }

    pub fn is_int(&self) -> bool {
        matches!(self,Object::Integer { value:_ })
    }

    pub fn is_bool(&self) -> bool {
        matches!(self,Object::Boolean { value:_ })
    }

    pub fn is_return(&self) -> bool {
        matches!(self,Object::Return { value:_ })
    }
}