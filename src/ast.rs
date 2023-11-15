
#[derive(PartialEq,Debug)]
pub enum Statement {
    Let { ident: Expression, value: Expression },
}
impl Statement {
    pub fn token_literal(&self) -> String {
        match self {
            Self::Let { ident, value } => {
                format!("let {} = {};",ident.token_literal(),value.token_literal())
            }
        }
    }
}

#[derive(PartialEq,Debug)]
pub enum Expression {
    Decoy, // いまだけ
    Ident { value: String }
}
impl Expression {
    pub fn token_literal(&self) -> String {
        match self {
            Self::Ident { value } => {
                value.clone()
            },
            Self::Decoy => {
                String::from("decoy")
            }
        }
    }
}

pub struct Program {
    pub stmts : Vec<Statement>
}
impl Program {
    pub fn token_literal(&self) -> String {
        let mut ret = String::new();
        for stmt in &self.stmts {
            ret.push_str(&stmt.token_literal());
        };
        ret
    }
    pub fn new() -> Self {
        Program { stmts: Vec::new() }
    }
}