use regex::Regex;
use std::env;

#[derive(Debug, PartialEq)]
enum Token {
    Identifier(String),
    Constant(String),
    IntKeyword,
    VoidKeyword,
    ReturnKeyword,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
}

fn tokenize(mut input: &str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let patterns = create_regex_patterns();

    while !input.is_empty() {
        if input.starts_with(char::is_whitespace) {
            // Trim whitespace from start of input
            input = input.trim_start();
        } else {
            // Find longest match at start of input
            let mut matched = false;

            for (pattern, token_constructor) in &patterns {
                if let Some(captures) = pattern.captures(input) {
                    if let Some(m) = captures.get(0) {
                        if m.start() == 0 {
                            // Match at start of input
                            let matched_str = m.as_str();

                            // Create token using the provided constructor function
                            let token = token_constructor(matched_str);
                            tokens.push(token);

                            input = &input[matched_str.len()..];
                            matched = true;
                            break;
                        }
                    }
                }
            }

            if !matched {
                return Err(format!("No matching pattern found at: '{}'", input));
            }
        }
    }

    Ok(tokens)
}

fn create_regex_patterns() -> Vec<(Regex, Box<dyn Fn(&str) -> Token>)> {
    vec![
        (
            Regex::new(r"^[a-zA-Z_]\w*\b").unwrap(),
            Box::new(|s: &str| Token::Identifier(s.to_string())),
        ),
        (
            Regex::new(r"^[0-9]+\b").unwrap(),
            Box::new(|s: &str| Token::Constant(s.to_string())),
        ),
        (
            Regex::new(r"^int\b").unwrap(),
            Box::new(|_| Token::IntKeyword),
        ),
        (
            Regex::new(r"^void\b").unwrap(),
            Box::new(|_| Token::VoidKeyword),
        ),
        (
            Regex::new(r"^return\b").unwrap(),
            Box::new(|_| Token::ReturnKeyword),
        ),
        (Regex::new(r"^\(").unwrap(), Box::new(|_| Token::OpenParen)),
        (Regex::new(r"^\)").unwrap(), Box::new(|_| Token::CloseParen)),
        (Regex::new(r"^\{").unwrap(), Box::new(|_| Token::OpenBrace)),
        (Regex::new(r"^\}").unwrap(), Box::new(|_| Token::CloseBrace)),
        (Regex::new(r"^;").unwrap(), Box::new(|_| Token::Semicolon)),
    ]
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("usage: {} \"input string to tokenize\"", args[0]);
        return;
    }

    let input = "int main() { return 42; }";
    match tokenize(input) {
        Ok(tokens) => println!("Tokens: {:#?}", tokens),
        Err(e) => eprintln!("Error: {}", e),
    }
    let mut lex_only = false;
    let mut parse_only = false;
    let mut codegen_only = false;
    let mut assembly_only = false;
    let mut input_file = None;

    for arg in &args[1..] {
        match arg.as_str() {
            "--lex" => lex_only = true,
            "--parse" => parse_only = true,
            "--codegen" => codegen_only = true,
            "-S" => assembly_only = true,
            _ => {
                if arg.starts_with("-") {
                    eprintln!("Unknown option: {}", arg);
                    std::process::exit(1);
                } else {
                    input_file = Some(arg.clone());
                }
            }
        }
    }

    let input_file = match input_file {
        Some(file) => file,
        None => {
            eprintln!("No input file specified");
            std::process::exit(1);
        }
    };

    let input = match std::fs::read_to_string(&input_file) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file {}: {}", input_file, e);
            std::process::exit(1);
        }
    };
}
