use std::collections::VecDeque;

use crate::token::Token;
struct UnresolvedStr {
    val : String
}

impl UnresolvedStr {
    fn new() -> Self {
        UnresolvedStr { val: String::from("") }
    }

    fn reset(&mut self){
        self.val = String::from("");
    }

    fn to_token_type(&self) -> Option<Token>{
        if self.val.len() <= 0 {
            None
        }else{
            Some(string_to_token(self.val.clone()))
        }
    }

    fn push(&mut self,ch:char){
        self.val.push(ch);
    }
}

/**
 * 関数というよりはマクロ
 * けど、マクロにすると複雑になると思ったので関数にする
 */
fn push_unresolved_str(target:&mut VecDeque<Token>,unresolved_str:&mut UnresolvedStr){
    let token = unresolved_str.to_token_type();

    if token.is_none() {
        return;
    }

    target.push_back(token.unwrap());

    unresolved_str.reset();
}


pub fn analyze_lexical(input:String) -> VecDeque<Token> {
    let mut unresolved_str = UnresolvedStr::new();
    let mut result = VecDeque::new();
    let mut cur = ' ';
    let input = input + "  ";

    for next in input.chars() {

        if is_skip_char(&cur) {
            push_unresolved_str(&mut result, &mut unresolved_str);
            cur = next;
            continue;
        }

        if let Some(token) = two_letter_to_token(cur,next) {
            push_unresolved_str(&mut result, &mut unresolved_str);
            result.push_back(token);
            cur = ' ';
            continue;
        }

        if let Some(token) = char_to_token(cur) {
            push_unresolved_str(&mut result, &mut unresolved_str);
            result.push_back(token);
            cur = next;
            continue;
        }

        unresolved_str.push(cur);
        cur = next;
    }

    result
}

fn is_skip_char(char:&char) -> bool {
    if char == &' '{
        true
    }else if char == &'\n'{
        true
    }else if char == &'\t'{
        true
    }else if char == &'\r'{
        true
    }else{
        false
    }
}

fn char_to_token(ch:char) -> Option<Token> {
    match ch {
        '=' => Some(Token::ASSIGN),
        '+' => Some(Token::PLUS),
        ',' => Some(Token::COMMA),
        ';' => Some(Token::SEMICOLON),
        '(' => Some(Token::LPAREN),
        ')' => Some(Token::RPAREN),
        '{' => Some(Token::LBRACE),
        '}' => Some(Token::RBRACE),
        '-' => Some(Token::MINUS),
        '!' => Some(Token::BANG),
        '*' => Some(Token::ASTERISK),
        '/' => Some(Token::SLASH),
        '>' => Some(Token::GT),
        '<' => Some(Token::LT),
        _ => None
    }
}

fn string_to_token(string:String) -> Token {
    if string == "fn" {
        Token::FUNCTION
    }else if string == "let" {
        Token::LET
    }else if string == "true" {
        Token::TRUE
    }else if string == "false" {
        Token::FALSE
    }else if string == "if" {
        Token::IF
    }else if string == "else" {
        Token::ELSE
    }else if string == "return" {
        Token::RETURN
    }else if string.parse::<usize>().is_ok() {
        Token::INTEGER(string.parse().unwrap())
    }else{
        Token::IDENTIFIER(string)
    }
}

fn two_letter_to_token(prev:char,current:char) -> Option<Token>{
    let string = format!("{}{}",prev,current);
    if string == "==" {
        Some(Token::EQ)
    }else if string == "!="{
        Some(Token::NOTEQ)
    }else{
        None
    }
}

#[cfg(test)]
mod test_analyze_lexical {
    use crate::token::Token;

    use super::analyze_lexical;


    #[test]
    fn single_char(){
        let input = String::from("=+,;(){}-!*/<>");
        let mut tokens = analyze_lexical(input);
        assert_eq!(tokens.pop_front().unwrap(),Token::ASSIGN);
        assert_eq!(tokens.pop_front().unwrap(),Token::PLUS);
        assert_eq!(tokens.pop_front().unwrap(),Token::COMMA);
        assert_eq!(tokens.pop_front().unwrap(),Token::SEMICOLON);
        assert_eq!(tokens.pop_front().unwrap(),Token::LPAREN);
        assert_eq!(tokens.pop_front().unwrap(),Token::RPAREN);
        assert_eq!(tokens.pop_front().unwrap(),Token::LBRACE);
        assert_eq!(tokens.pop_front().unwrap(),Token::RBRACE);
        assert_eq!(tokens.pop_front().unwrap(),Token::MINUS);
        assert_eq!(tokens.pop_front().unwrap(),Token::BANG);
        assert_eq!(tokens.pop_front().unwrap(),Token::ASTERISK);
        assert_eq!(tokens.pop_front().unwrap(),Token::SLASH);
        assert_eq!(tokens.pop_front().unwrap(),Token::LT);
        assert_eq!(tokens.pop_front().unwrap(),Token::GT);
        assert!(tokens.pop_front().is_none());
    }

    #[test]
    fn string(){
        let input = String::from("let a = fn () { 123 }");
        let mut tokens = analyze_lexical(input);
        assert_eq!(tokens.pop_front().unwrap(),Token::LET);
        assert_eq!(tokens.pop_front().unwrap(),Token::IDENTIFIER(String::from("a")));
        assert_eq!(tokens.pop_front().unwrap(),Token::ASSIGN);
        assert_eq!(tokens.pop_front().unwrap(),Token::FUNCTION);
        assert_eq!(tokens.pop_front().unwrap(),Token::LPAREN);
        assert_eq!(tokens.pop_front().unwrap(),Token::RPAREN);
        assert_eq!(tokens.pop_front().unwrap(),Token::LBRACE);
        assert_eq!(tokens.pop_front().unwrap(),Token::INTEGER(123));
        assert_eq!(tokens.pop_front().unwrap(),Token::RBRACE);
        assert!(tokens.pop_front().is_none());


        let input = String::from("true false if else return");
        let mut tokens = analyze_lexical(input);
        assert_eq!(tokens.pop_front().unwrap(),Token::TRUE);
        assert_eq!(tokens.pop_front().unwrap(),Token::FALSE);
        assert_eq!(tokens.pop_front().unwrap(),Token::IF);
        assert_eq!(tokens.pop_front().unwrap(),Token::ELSE);
        assert_eq!(tokens.pop_front().unwrap(),Token::RETURN);
        assert!(tokens.pop_front().is_none());
    }

    #[test]
    fn skip_char(){
        let input = String::from(" ; ;   ;\n;\t;      ; a a");
        let mut tokens = analyze_lexical(input);
        assert_eq!(tokens.pop_front().unwrap(),Token::SEMICOLON);
        assert_eq!(tokens.pop_front().unwrap(),Token::SEMICOLON);
        assert_eq!(tokens.pop_front().unwrap(),Token::SEMICOLON);
        assert_eq!(tokens.pop_front().unwrap(),Token::SEMICOLON);
        assert_eq!(tokens.pop_front().unwrap(),Token::SEMICOLON);
        assert_eq!(tokens.pop_front().unwrap(),Token::SEMICOLON);
        assert_eq!(tokens.pop_front().unwrap(),Token::IDENTIFIER(String::from("a")));
        assert_eq!(tokens.pop_front().unwrap(),Token::IDENTIFIER(String::from("a")));
        assert!(tokens.pop_front().is_none());
    }

    #[test]
    fn number(){
        let input = String::from("100 -100 1.0 -1,0");
        let mut tokens = analyze_lexical(input);
        assert_eq!(tokens.pop_front().unwrap(),Token::INTEGER(100));
        assert_eq!(tokens.pop_front().unwrap(),Token::MINUS);
        assert_eq!(tokens.pop_front().unwrap(),Token::INTEGER(100));
        assert_eq!(tokens.pop_front().unwrap(),Token::IDENTIFIER(String::from("1.0")));
        assert_eq!(tokens.pop_front().unwrap(),Token::MINUS);
        assert_eq!(tokens.pop_front().unwrap(),Token::INTEGER(1));
        assert_eq!(tokens.pop_front().unwrap(),Token::COMMA);
        assert_eq!(tokens.pop_front().unwrap(),Token::INTEGER(0));

        assert!(tokens.pop_front().is_none());
    }

    #[test]
    fn two_char(){
        let input = String::from("== != =!= !!= !== = =");
        let mut tokens = analyze_lexical(input);
        assert_eq!(tokens.pop_front().unwrap(),Token::EQ);
        assert_eq!(tokens.pop_front().unwrap(),Token::NOTEQ);
        assert_eq!(tokens.pop_front().unwrap(),Token::ASSIGN);
        assert_eq!(tokens.pop_front().unwrap(),Token::NOTEQ);
        assert_eq!(tokens.pop_front().unwrap(),Token::BANG);
        assert_eq!(tokens.pop_front().unwrap(),Token::NOTEQ);
        assert_eq!(tokens.pop_front().unwrap(),Token::NOTEQ);
        assert_eq!(tokens.pop_front().unwrap(),Token::ASSIGN);
        assert_eq!(tokens.pop_front().unwrap(),Token::ASSIGN);
        assert_eq!(tokens.pop_front().unwrap(),Token::ASSIGN);
        assert!(tokens.pop_front().is_none());
    }
}