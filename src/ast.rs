use crate::token::Token;


#[derive(PartialEq,Debug,Clone)]
pub enum Statement {
    Let { ident: Expression, value: Expression },
    Return { value: Expression },
    Expression { exp: Expression },
    Block{ stmts: Vec<Statement> },
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
            },
            Self::Block { stmts } => {
                let mut strings = Vec::new();
                for stmt in stmts {
                    strings.push(stmt.string());
                };
                format!("{{ {} }}",strings.join(""))
            },
        }
    }
}

#[derive(PartialEq,Debug,Clone)]
pub enum Expression {
    Ident { value: String },
    Integer { value: usize },
    Boolean { value: bool },
    Prefix { ope: PrefixOpe, right: Box<Expression> },
    Infix { left: Box<Expression>, ope:InfixOpe, right: Box<Expression> },
    If { condition: Box<Expression>, consequence: Box<Statement>, alternative: Option<Box<Statement>>},
    Function { params: Vec<Expression>, body: Box<Statement> },
    Call { func: Box<Expression> ,args:Vec<Expression> }
}

impl Expression {
    pub fn string(&self) -> String {
        match self {
            Self::Ident { value } => {
                value.clone()
            },
            Self::Integer { value } => {
                value.to_string()
            },
            Self::Prefix { ope, right } => {
                format!("({}{})",ope.string(),right.string())
            },
            Self::Infix { left, ope, right } => {
                format!("({} {} {})",left.string(),ope.string(),right.string())
            },
            Self::Boolean { value } => {
                format!("{}",value)
            },
            Self::If { condition, consequence, alternative } => {
                match alternative {
                    Some(alt) => format!("if ( {} ) {} else {}",condition.string(),consequence.string(),alt.string()),
                    None => format!("if ( {} ) {}",condition.string(),consequence.string())
                }
            },
            Self::Function { params, body } => {
                let params_str : Vec<String> = params.iter().map(|x| x.string()).collect();
                format!("fn ( {} ) {}",params_str.join(","),body.string())
            },
            Self::Call { func, args } => {
                let args_strs : Vec<String> = args.iter().map(|x| x.string() ).collect();
                format!("{}({})",func.string(),args_strs.join(","))
            },
        }
    }
}

pub struct Program {
    pub stmts : Vec<Statement>
}
impl Program {
    #[cfg(test)]
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
#[derive(PartialEq,Debug,Clone)]
pub enum InfixOpe {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Eq,
    NotEq,
    LT,
    GT,
}
impl InfixOpe {
    pub fn string(&self) -> String {
        let s = match self {
            Self::Asterisk => "*",
            Self::Eq => "==",
            Self::GT => ">",
            Self::LT => "<",
            Self::Minus => "-",
            Self::NotEq => "!=",
            Self::Plus => "+",
            Self::Slash => "/"
        };

        String::from(s)
    }
    pub fn from_tkn(tkn:&Token) -> Option<InfixOpe> {
        match tkn {
            Token::ASTERISK => Some(Self::Asterisk),
            Token::EQ => Some(Self::Eq),
            Token::GT => Some(Self::GT),
            Token::LT => Some(Self::LT),
            Token::MINUS => Some(Self::Minus),
            Token::NOTEQ => Some(Self::NotEq),
            Token::PLUS => Some(Self::Plus),
            Token::SLASH => Some(Self::Slash),
            _ => None
        }
    }
}
#[derive(PartialEq,Debug,Clone)]
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

    pub fn from_tkn(tkn:&Token) -> Option<Self> {
        match tkn {
            Token::BANG => Some(Self::Bang),
            Token::MINUS => Some(Self::Minus),
            _ => None
        }
    }
}