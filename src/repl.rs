use std::{io::{self, Write}, rc::Rc, cell::RefCell};
use crate::{lexer::analyze_lexical, parser::Parser, eval::eval_program, env::Environment};

pub fn start(){
    let env = Rc::new(RefCell::new(Environment::new()));

    loop {
        let mut input = String::new();
        print!(">> ");

        // プロンプトを表示するために必要 https://stackoverflow.com/questions/54262976/how-do-i-print-stdout-and-get-stdin-on-the-same-line-in-rust
        let _ = io::stdout().flush(); 

        io::stdin().read_line(&mut input).ok();

        // 今だけの仕様
        if input == "exit\n" {
            break
        }

        let tokens = analyze_lexical(input);
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program();
        if parser.faults.len() > 0 {
            print_parse_fault(&parser);
        }
        match eval_program(program,env.clone()) {
            Ok(obj) => println!("{}",obj.inspect()),
            Err(err) => println!("{}",err.msg())
        }
        
    }
}

fn print_parse_fault(parser:&Parser){
    for f in &parser.faults {
        println!("{}",f.msg());
    }
}