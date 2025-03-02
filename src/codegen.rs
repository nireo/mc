use std::io::{self, Write};
use std::{
    fmt::{self, Display, Formatter},
    fs::File,
    path::Path,
};

use crate::parser::{self, Expression, Statement};
use anyhow::Result;

#[derive(Debug)]
pub enum Operand {
    Imm(i64),
    Reg,
}

#[derive(Debug)]
pub enum Instruction {
    Ret,
    Mov(Operand, Operand),
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub struct GeneratedProgram {
    pub function: Function,
}

impl Display for Operand {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Imm(value) => write!(f, "${}", value),
            Operand::Reg => write!(f, "%eax"),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Mov(src, dst) => write!(f, "\tmovl {}, {}", src, dst),
            Instruction::Ret => write!(f, "\tret"),
        }
    }
}

pub struct CodeGenerator {}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn generate_code(&self, program: parser::Program) -> Result<GeneratedProgram> {
        let fn_def = program.fn_def;
        let func_name = fn_def.identifier;

        let mut instructions = Vec::new();

        self.generate_statement(&fn_def.statement, &mut instructions)?;

        Ok(GeneratedProgram {
            function: Function {
                name: func_name,
                instructions,
            },
        })
    }

    fn generate_statement(
        &self,
        stmt: &Statement,
        instructions: &mut Vec<Instruction>,
    ) -> Result<()> {
        match stmt {
            Statement::Return(expr) => {
                let operand = self.generate_expr(expr)?;
                instructions.push(Instruction::Mov(operand, Operand::Reg));
                instructions.push(Instruction::Ret);
            }
            _ => unimplemented!("not implemeneted for statement"),
        }

        Ok(())
    }

    fn generate_expr(&self, expr: &Expression) -> Result<Operand> {
        match expr {
            Expression::Integer(val) => Ok(Operand::Imm(*val)),
        }
    }
}

pub struct CodeEmitter {}

impl CodeEmitter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn emit_code<P: AsRef<Path>>(
        &self,
        program: GeneratedProgram,
        output_path: P,
    ) -> Result<()> {
        let mut file = File::create(output_path)?;
        self.emit_program(&program, &mut file)?;
        Ok(())
    }

    fn emit_program<W: Write>(&self, program: &GeneratedProgram, writer: &mut W) -> Result<()> {
        self.emit_function(&program.function, writer)?;

        writeln!(writer, "\t.section .note.GNU-stack,\"\",@progbits")?;

        Ok(())
    }

    fn emit_function<W: Write>(&self, function: &Function, writer: &mut W) -> Result<()> {
        writeln!(writer, "\t.globl {}", function.name)?;
        writeln!(writer, "{}:", function.name)?;

        for instruction in &function.instructions {
            writeln!(writer, "{}", instruction)?;
        }

        Ok(())
    }
}
