use crate::parser::Program;

#[derive(Debug)]
pub struct Codegen {
    program: Program,
}

#[derive(Debug)]
pub enum Operand {
    Imm(i64),
    Reg,
}

#[derive(Debug)]
pub enum Instruction {
    Ret,
    Mov { src: Operand, dest: Operand },
}

#[derive(Debug)]
pub enum GeneratedFunction {
    name: String,
    instructions: Vec<Instruction>,
}

impl Codegen {
    pub fn new(prog: Program) -> Self {
        Self { program: prog }
    }

    pub fn generate(&self) -> String {
        let mut result = Vec::new();

        result
    }

    pub fn gen_function_def(&self) -> Instruction {}

    pub fn gen_statement(&self) -> Vec<Instruction> {}
}
