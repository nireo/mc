package mc

import (
	"fmt"
	"io"
)

type Operation int

const (
	A_O_NOT Operation = iota
	A_O_NEG
)

func (o Operation) String() string {
	switch o {
	case A_O_NOT:
		return "notl"
	case A_O_NEG:
		return "negl"
	}

	return ""
}

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

type UnaryInstruction struct {
	op      Operation
	operand *Operand
}

type Instruction struct {
	kind    InstructionKind
	mov_a   *Operand
	mov_b   *Operand
	integer int64
	unary   *UnaryInstruction
}

type GenFunction struct {
	name         string
	instructions []Instruction
}

type GenProgram struct {
	fn *GenFunction
}

func (i Instruction) String() string {
	switch i.kind {
	case INS_RET:
		return "\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret"
	case INS_MOV:
		return fmt.Sprintf("\tmovl %s, %s", i.mov_a.String(), i.mov_b.String())
	case INS_ALLOC_STACK:
		return fmt.Sprintf("\tsubq $%d, %%rsp", i.integer)
	case INS_UNARY:
		return fmt.Sprintf("\t%s %s", i.unary.op, i.unary.operand)
	}

	return ""
}

func convertIrValue(val *IrVal) *Operand {
	switch val.kind {
	case IR_VAL_CONSTANT:
		return &Operand{kind: OPERAND_IMM, imm: val.constant}
	case IR_VAL_VAR:
		return &Operand{kind: OPERAND_PSEUDO, ident: val.identifier}
	}

	panic("unrecognized ir value")
}

func convertIrOperator(op IrOperator) Operation {
	switch op {
	case IR_UNARY_COMPLEMENT:
		return A_O_NOT
	case IR_UNARY_NEGATE:
		return A_O_NEG
	}

	panic("unrecognized ir operator")
}

func convertIrInstruction(ins *IrInstruction, instructions *[]Instruction) {
	switch ins.kind {
	case IR_INSTRUCTION_RETURN:
		*instructions = append(*instructions, Instruction{
			kind:  INS_MOV,
			mov_a: convertIrValue(ins.ret.val),
			mov_b: &Operand{kind: OPERAND_REG_AX},
		}, Instruction{kind: INS_RET})
		return
	case IR_INSTRUCTION_UNARY:
		*instructions = append(*instructions, Instruction{
			kind:  INS_MOV,
			mov_a: convertIrValue(ins.unary.src),
			mov_b: convertIrValue(ins.unary.dst),
		}, Instruction{
			kind: INS_UNARY,
			unary: &UnaryInstruction{
				op:      convertIrOperator(ins.unary.operator),
				operand: convertIrValue(ins.unary.dst),
			},
		})
	}
}

func replacePseudoRegisters(insts *[]Instruction) int {
	pseudoToStack := make(map[string]int64)
	currentOffset := int64(-4)

	for i := range *insts {
		inst := &(*insts)[i]

		if inst.kind == INS_MOV && inst.mov_a != nil && inst.mov_a.kind == OPERAND_PSEUDO {
			if _, exists := pseudoToStack[inst.mov_a.ident]; !exists {
				pseudoToStack[inst.mov_a.ident] = currentOffset
				currentOffset -= 4
			}
		}

		if inst.kind == INS_MOV && inst.mov_b != nil && inst.mov_b.kind == OPERAND_PSEUDO {
			if _, exists := pseudoToStack[inst.mov_b.ident]; !exists {
				pseudoToStack[inst.mov_b.ident] = currentOffset
				currentOffset -= 4
			}
		}

		if inst.kind == INS_UNARY && inst.unary != nil && inst.unary.operand != nil &&
			inst.unary.operand.kind == OPERAND_PSEUDO {
			if _, exists := pseudoToStack[inst.unary.operand.ident]; !exists {
				pseudoToStack[inst.unary.operand.ident] = currentOffset
				currentOffset -= 4
			}
		}
	}

	for i := range *insts {
		inst := &(*insts)[i]

		if inst.kind == INS_MOV && inst.mov_a != nil && inst.mov_a.kind == OPERAND_PSEUDO {
			offset, exists := pseudoToStack[inst.mov_a.ident]
			if exists {
				inst.mov_a.kind = OPERAND_STACK
				inst.mov_a.imm = offset
			}
		}

		if inst.kind == INS_MOV && inst.mov_b != nil && inst.mov_b.kind == OPERAND_PSEUDO {
			offset, exists := pseudoToStack[inst.mov_b.ident]
			if exists {
				inst.mov_b.kind = OPERAND_STACK
				inst.mov_b.imm = offset
			}
		}

		if inst.kind == INS_UNARY && inst.unary != nil && inst.unary.operand != nil &&
			inst.unary.operand.kind == OPERAND_PSEUDO {
			offset, exists := pseudoToStack[inst.unary.operand.ident]
			if exists {
				inst.unary.operand.kind = OPERAND_STACK
				inst.unary.operand.imm = offset
			}
		}
	}

	return len(pseudoToStack) * 4
}

func fixInvalidMoves(insts *[]Instruction) {
	var fixedInstructions []Instruction

	for _, inst := range *insts {
		if inst.kind == INS_MOV &&
			inst.mov_a != nil && inst.mov_a.kind == OPERAND_STACK &&
			inst.mov_b != nil && inst.mov_b.kind == OPERAND_STACK {

			moveToR10 := Instruction{
				kind:  INS_MOV,
				mov_a: inst.mov_a,
				mov_b: &Operand{kind: OPERAND_REG_R10},
			}

			moveFromR10 := Instruction{
				kind:  INS_MOV,
				mov_a: &Operand{kind: OPERAND_REG_R10},
				mov_b: inst.mov_b,
			}

			fixedInstructions = append(fixedInstructions, moveToR10, moveFromR10)
		} else {
			fixedInstructions = append(fixedInstructions, inst)
		}
	}

	*insts = fixedInstructions
}

func GenerateIr(prog *IrProgram) *GenProgram {
	genFn := &GenFunction{
		name:         prog.function.identifier,
		instructions: make([]Instruction, 0),
	}

	for _, inst := range prog.function.body {
		convertIrInstruction(inst, &genFn.instructions)
	}

	stackSize := replacePseudoRegisters(&genFn.instructions)
	fixInvalidMoves(&genFn.instructions)

	if stackSize > 0 {
		allocInst := Instruction{
			kind:    INS_ALLOC_STACK,
			integer: int64(stackSize),
		}

		genFn.instructions = append([]Instruction{allocInst}, genFn.instructions...)
	}

	return &GenProgram{
		fn: genFn,
	}
}

type CodeEmitter struct {
}

func NewCodeEmitter() *CodeEmitter {
	return &CodeEmitter{}
}

func (ce *CodeEmitter) EmitProgram(program *GenProgram, writer io.Writer) error {
	if err := ce.EmitFunction(program.fn, writer); err != nil {
		return err
	}

	if _, err := fmt.Fprintln(writer, "\t.section .note.GNU-stack,\"\",@progbits"); err != nil {
		return err
	}

	return nil
}

func (ce *CodeEmitter) EmitFunction(function *GenFunction, writer io.Writer) error {
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

	return nil
}
