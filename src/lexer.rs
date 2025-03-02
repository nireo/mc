use anyhow::{Result, anyhow};
use regex::Regex;

#[derive(Debug, PartialEq)]
pub enum Token {
    Identifier(String),
    Constant(i64),
    IntKeyword,
    VoidKeyword,
    ReturnKeyword,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Tilde,
    Minus,
}

pub fn tokenize(mut input: &str) -> Result<Vec<Token>> {
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
                            let matched_str = m.as_str();
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
                return Err(anyhow!("No matching pattern found at: '{}'", input));
            }
        }
    }
    Ok(tokens)
}

fn create_regex_patterns() -> Vec<(Regex, Box<dyn Fn(&str) -> Token>)> {
    vec![
        // Keywords first (to prevent them being matched as identifiers)
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
        // Constants before identifiers (to ensure numbers are matched as constants)
        (
            Regex::new(r"^[0-9]+\b").unwrap(),
            Box::new(|s: &str| Token::Constant(s.parse::<i64>().unwrap())),
        ),
        // Identifiers after keywords and constants
        (
            Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*\b").unwrap(),
            Box::new(|s: &str| Token::Identifier(s.to_string())),
        ),
        // Other tokens
        (Regex::new(r"^\(").unwrap(), Box::new(|_| Token::OpenParen)),
        (Regex::new(r"^\)").unwrap(), Box::new(|_| Token::CloseParen)),
        (Regex::new(r"^\{").unwrap(), Box::new(|_| Token::OpenBrace)),
        (Regex::new(r"^\}").unwrap(), Box::new(|_| Token::CloseBrace)),
        (Regex::new(r"^\-").unwrap(), Box::new(|_| Token::Minus)),
        (Regex::new(r"^\~").unwrap(), Box::new(|_| Token::Tilde)),
        (Regex::new(r"^;").unwrap(), Box::new(|_| Token::Semicolon)),
    ]
}
