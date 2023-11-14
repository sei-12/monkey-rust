use std::io::{self, Write};
use crate::lexer::analyze_lexical;

pub fn start(){

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
        println!("{:?}",tokens);
    }


}