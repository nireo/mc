package mc

import (
	"fmt"
	"io"
	"os"
)

type OperandKind int

const (
	OPERAND_IMM OperandKind = iota
	OPERAND_REG_AX
	OPERAND_REG_R10
	OPERAND_PSEUDO
	OPERAND_STACK
)

type Operand struct {
	kind  OperandKind
	imm   int64
	ident string
}

func (o Operand) String() string {
	switch o.kind {
	case OPERAND_IMM:
		return fmt.Sprintf("$%d", o.imm)
	case OPERAND_REG_AX:
		return "%eax"
	case OPERAND_REG_R10:
		return "%r10d"
	case OPERAND_STACK:
		return fmt.Sprintf("%d(%%rbp)", o.imm)
	}
	return ""
}

type InstructionKind int

const (
	INS_RET InstructionKind = iota
	INS_MOV
	INS_UNARY
	INS_ALLOC_STACK
)

type Instruction struct {
	kind    InstructionKind
	mov_a   *Operand
	mov_b   *Operand
	integer int64
}

type genFunction struct {
	name         string
	instructions []Instruction
}

type genProgram struct {
	fn *genFunction
}

func (i Instruction) String() string {
	switch i.kind {
	case INS_RET:
		return "\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret"
	case INS_MOV:
		return fmt.Sprintf("\tmovl %s %s", i.mov_a.String(), i.mov_b.String())
	case INS_ALLOC_STACK:
		return fmt.Sprintf("subq $%d, %%rsp", i.integer)
	}

	return ""
}

type CodeGenerator struct{}

func NewCodeGenerator() *CodeGenerator {
	return &CodeGenerator{}
}

func (cg *CodeGenerator) GenerateCode(program *Program) (*genProgram, error) {
	fnDef := program.mainFunction
	funcName := fnDef.identifier
	instructions := []Instruction{}

	err := cg.generateStatement(fnDef.statement, &instructions)
	if err != nil {
		return nil, err
	}

	return &genProgram{
		fn: &genFunction{
			name:         funcName,
			instructions: instructions,
		},
	}, nil
}

func (cg *CodeGenerator) generateStatement(stmt *Statement, instructions *[]Instruction) error {
	switch stmt.kind {
	case STMT_RETURN:
		operand, err := cg.generateExpr(stmt.ret.expr)
		if err != nil {
			return err
		}

		movInstr := Instruction{
			kind:  INS_MOV,
			mov_a: operand,
			mov_b: &Operand{
				kind: OPERAND_REG_AX,
			},
		}

		retInstr := Instruction{
			kind: INS_RET,
		}

		*instructions = append(*instructions, movInstr, retInstr)
	case STMT_IF:
		return fmt.Errorf("if statement not implemented")
	default:
		return fmt.Errorf("unsupported statement kind: %d", stmt.kind)
	}

	return nil
}

func (cg *CodeGenerator) generateExpr(expr *Expression) (*Operand, error) {
	switch expr.kind {
	case EXP_INTEGER:
		return &Operand{
			kind: OPERAND_IMM,
			imm:  expr.integer,
		}, nil
	case EXP_UNARY:
		return nil, fmt.Errorf("unary expression not implemented")
	default:
		return nil, fmt.Errorf("unsupported expression kind: %d", expr.kind)
	}
}

type CodeEmitter struct{}

func NewCodeEmitter() *CodeEmitter {
	return &CodeEmitter{}
}

func (ce *CodeEmitter) EmitCode(program *genProgram, outputPath string) error {
	file, err := os.Create(outputPath)
	if err != nil {
		return err
	}
	defer file.Close()

	return ce.EmitProgram(program, file)
}

func (ce *CodeEmitter) EmitProgram(program *genProgram, writer io.Writer) error {
	if err := ce.EmitFunction(program.fn, writer); err != nil {
		return err
	}

	if _, err := fmt.Fprintln(writer, "\t.section .note.GNU-stack,\"\",@progbits"); err != nil {
		return err
	}

	return nil
}

func (ce *CodeEmitter) EmitFunction(function *genFunction, writer io.Writer) error {
	if _, err := fmt.Fprintf(writer, "\t.globl %s\n", function.name); err != nil {
		return err
	}

	if _, err := fmt.Fprintf(writer, "%s:\n", function.name); err != nil {
		return err
	}

	if _, err := fmt.Fprintln(writer, "\tpushq %rbp"); err != nil {
		return err
	}

	if _, err := fmt.Fprintln(writer, "\tmovq %rsp, %rbp"); err != nil {
		return err
	}

	for _, instruction := range function.instructions {
		if _, err := fmt.Fprintln(writer, instruction.String()); err != nil {
			return err
		}
	}

	if _, err := fmt.Fprintln(writer, "\tmovq %rbp, %rsp"); err != nil {
		return err
	}

	if _, err := fmt.Fprintln(writer, "\tpopq %rbp"); err != nil {
		return err
	}

	return nil
}
