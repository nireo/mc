mod codegen;
mod lexer;
mod parser;

use anyhow::Result;
use codegen::{CodeEmitter, CodeGenerator};
use std::env;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("usage: {} \"input string to tokenize\"", args[0]);
        return Ok(());
    }

    let input = "int main() { return 42; }";
    match lexer::tokenize(input) {
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

    let tokens = lexer::tokenize(&input)?;
    let mut parser = parser::Parser::new(tokens.into_iter());

    let program = parser.parse()?;
    println!("{:?}", program.fn_def);

    let asm_ast = CodeGenerator::new().generate_code(program)?;
    CodeEmitter::new().emit_code(asm_ast, input_file + ".out")?;

    Ok(())
}
