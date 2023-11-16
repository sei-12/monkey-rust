
#[derive(PartialEq,Debug)]
pub enum Statement {
    Let { ident: Expression, value: Expression },
    Return { value: Expression },
    Expression { exp: Expression },
}
impl Statement {
    pub fn string(&self) -> String {
        match self {
            Self::Let { ident, value } => {
                format!("let {} = {};",ident.string(),value.string())
            },
            Self::Return { value } => {
                format!("return {};",value.string())
            },
            Self::Expression { exp } => {
                exp.string()
            }
        }
    }
}

#[derive(PartialEq,Debug)]
pub enum Expression {
    Decoy, // いまだけ
    Ident { value: String },
    Integer { value: usize },
    Prefix { ope: PrefixOpe, right: Box<Expression> }
}
impl Expression {
    pub fn string(&self) -> String {
        match self {
            Self::Ident { value } => {
                value.clone()
            },
            Self::Decoy => {
                String::from("decoy")
            },
            Self::Integer { value } => {
                value.to_string()
            },
            Self::Prefix { ope, right } => {
                format!("{}{}",ope.string(),right.string())
            }
        }
    }
}

pub struct Program {
    pub stmts : Vec<Statement>
}
impl Program {
    pub fn string(&self) -> String {
        let mut ret = String::new();
        for stmt in &self.stmts {
            ret.push_str(&stmt.string());
        };
        ret
    }
    pub fn new() -> Self {
        Program { stmts: Vec::new() }
    }
}

#[derive(PartialEq,Debug)]
pub enum PrefixOpe {
    Bang,
    Minus
}
impl PrefixOpe {
    pub fn string(&self) -> String {
        match self {
            PrefixOpe::Bang => String::from("!"),
            PrefixOpe::Minus => String::from("-")
        }
    }
}