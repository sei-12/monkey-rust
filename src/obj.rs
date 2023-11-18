
pub enum Object {
    Integer { value: isize },
    Boolean { value: bool },
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer { value } => format!("{}",value),
            Self::Boolean { value } => format!("{}",value),
            Self::Null => format!("null"),
        }
    }
}