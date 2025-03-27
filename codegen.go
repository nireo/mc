package mc

import (
	"fmt"
	"io"
)

type Operation int

const (
	A_O_NOT Operation = iota
	A_O_NEG
	A_O_ADD
	A_O_SUB
	A_O_MUL
	A_O_DIV
	A_O_REMAINDER
)

func (o Operation) String() string {
	switch o {
	case A_O_NOT:
		return "notl"
	case A_O_NEG:
		return "negl"
	case A_O_ADD:
		return "addl"
	case A_O_SUB:
		return "subl"
	case A_O_MUL:
		return "imull"
	}

	return ""
}

type OperandKind int

const (
	OPERAND_IMM OperandKind = iota
	OPERAND_REG_AX
	OPERAND_REG_DX
	OPERAND_REG_R10
	OPERAND_REG_R11
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
	case OPERAND_REG_DX:
		return "%edx"
	case OPERAND_REG_R10:
		return "%r10d"
	case OPERAND_REG_R11:
		return "%r11d"
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
	INS_CDQ
	INS_IDIV
	INS_BINARY
)

type UnaryInstruction struct {
	op      Operation
	operand *Operand
}

type BinaryInstruction struct {
	op Operation
	a  *Operand
	b  *Operand
}

type Instruction struct {
	kind    InstructionKind
	mov_a   *Operand
	mov_b   *Operand
	integer int64
	unary   *UnaryInstruction
	idiv    *Operand
	binary  *BinaryInstruction
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
	case INS_BINARY:
		fmt.Println("got b binary:", i.binary.b.kind)
		return fmt.Sprintf("\t%s %s, %s", i.binary.op, i.binary.a, i.binary.b)
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
	case IR_BIN_ADD:
		return A_O_ADD
	case IR_BIN_REMAINDER:
		return A_O_ADD
	case IR_BIN_SUB:
		return A_O_SUB
	case IR_BIN_MUL:
		return A_O_MUL
	case IR_BIN_DIV:
		return A_O_DIV
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
	case IR_INSTRUCTION_BINARY:
		if ins.binary.operator == IR_BIN_DIV {
			*instructions = append(*instructions,
				Instruction{
					kind:  INS_MOV,
					mov_a: convertIrValue(ins.binary.src1),
					mov_b: &Operand{kind: OPERAND_REG_AX},
				},
				Instruction{kind: INS_CDQ},
				Instruction{
					kind: INS_IDIV, idiv: convertIrValue(ins.binary.src2),
				},
				Instruction{
					kind:  INS_MOV,
					mov_a: &Operand{kind: OPERAND_REG_AX},
					mov_b: convertIrValue(ins.binary.dst),
				},
			)
		} else if ins.binary.operator == IR_BIN_REMAINDER {
			*instructions = append(*instructions,
				Instruction{
					kind:  INS_MOV,
					mov_a: convertIrValue(ins.binary.src1),
					mov_b: &Operand{kind: OPERAND_REG_AX},
				},
				Instruction{kind: INS_CDQ},
				Instruction{
					kind: INS_IDIV, idiv: convertIrValue(ins.binary.src2),
				},
				Instruction{
					kind:  INS_MOV,
					mov_a: &Operand{kind: OPERAND_REG_DX},
					mov_b: convertIrValue(ins.binary.dst),
				},
			)
		} else {
			*instructions = append(*instructions, Instruction{
				kind:  INS_MOV,
				mov_a: convertIrValue(ins.binary.src1),
				mov_b: convertIrValue(ins.binary.dst),
			}, Instruction{
				kind: INS_BINARY,
				binary: &BinaryInstruction{
					op: convertIrOperator(ins.binary.operator),
					a:  convertIrValue(ins.binary.src2),
					b:  convertIrValue(ins.binary.dst),
				},
			})
		}
	}
}

func replacePseudoRegisters(insts *[]Instruction) int {
	pseudoToStack := make(map[string]int64)
	currentOffset := int64(-4)

	for i := range *insts {
		inst := &(*insts)[i]

		if inst.kind == INS_MOV && inst.mov_a.kind == OPERAND_PSEUDO {
			if _, exists := pseudoToStack[inst.mov_a.ident]; !exists {
				pseudoToStack[inst.mov_a.ident] = currentOffset
				currentOffset -= 4
			}
		}

		if inst.kind == INS_MOV && inst.mov_b.kind == OPERAND_PSEUDO {
			if _, exists := pseudoToStack[inst.mov_b.ident]; !exists {
				pseudoToStack[inst.mov_b.ident] = currentOffset
				currentOffset -= 4
			}
		}

		if inst.kind == INS_UNARY && inst.unary.operand.kind == OPERAND_PSEUDO {
			if _, exists := pseudoToStack[inst.unary.operand.ident]; !exists {
				pseudoToStack[inst.unary.operand.ident] = currentOffset
				currentOffset -= 4
			}
		}

		if inst.kind == INS_BINARY && inst.binary.a.kind == OPERAND_PSEUDO {
			if _, exists := pseudoToStack[inst.binary.a.ident]; !exists {
				pseudoToStack[inst.binary.a.ident] = currentOffset
				currentOffset -= 4
			}
		}

		if inst.kind == INS_BINARY && inst.binary.b.kind == OPERAND_PSEUDO {
			if _, exists := pseudoToStack[inst.binary.b.ident]; !exists {
				pseudoToStack[inst.binary.b.ident] = currentOffset
				currentOffset -= 4
			}
		}
	}

	for i := range *insts {
		inst := &(*insts)[i]

		if inst.kind == INS_MOV && inst.mov_a.kind == OPERAND_PSEUDO {
			offset, exists := pseudoToStack[inst.mov_a.ident]
			if exists {
				inst.mov_a.kind = OPERAND_STACK
				inst.mov_a.imm = offset
			}
		}

		if inst.kind == INS_MOV && inst.mov_b.kind == OPERAND_PSEUDO {
			offset, exists := pseudoToStack[inst.mov_b.ident]
			if exists {
				inst.mov_b.kind = OPERAND_STACK
				inst.mov_b.imm = offset
			}
		}

		if inst.kind == INS_UNARY && inst.unary.operand.kind == OPERAND_PSEUDO {
			offset, exists := pseudoToStack[inst.unary.operand.ident]
			if exists {
				inst.unary.operand.kind = OPERAND_STACK
				inst.unary.operand.imm = offset
			}
		}

		if inst.kind == INS_BINARY && inst.binary.a.kind == OPERAND_PSEUDO {
			offset, exists := pseudoToStack[inst.binary.a.ident]
			if exists {
				inst.binary.a.kind = OPERAND_STACK
				inst.binary.a.imm = offset
			}
		}

		if inst.kind == INS_BINARY && inst.binary.b.kind == OPERAND_PSEUDO {
			offset, exists := pseudoToStack[inst.binary.b.ident]
			if exists {
				inst.binary.b.kind = OPERAND_STACK
				inst.binary.b.imm = offset
			}
		}
	}

	return len(pseudoToStack) * 4
}

func fixInstructions(insts *[]Instruction) {
	var fixedInstructions []Instruction

	for _, inst := range *insts {
		if inst.kind == INS_MOV && inst.mov_a.kind == OPERAND_STACK && inst.mov_b.kind == OPERAND_STACK {
			// movl -4(%rbp), -8(%rbp)
			// ->
			// movl -4(%rbp), %r10d
			// movl %r10d, -8(%rbp)
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
		} else if inst.kind == INS_IDIV && inst.idiv.kind == OPERAND_IMM {
			// idivl $3
			// ->
			// movl $3, %r10d
			// idivl %r10d

			movToR10 := Instruction{
				kind:  INS_MOV,
				mov_a: inst.idiv,
				mov_b: &Operand{kind: OPERAND_REG_R10},
			}

			idivl := Instruction{
				kind: INS_IDIV,
				idiv: &Operand{kind: OPERAND_REG_R10},
			}
			fixedInstructions = append(fixedInstructions, movToR10, idivl)
		} else if inst.kind == INS_BINARY &&
			(inst.binary.op == A_O_ADD || inst.binary.op == A_O_SUB) &&
			inst.binary.a.kind == OPERAND_STACK &&
			inst.binary.b.kind == OPERAND_STACK {

			// addl -4(%rbp), -8(%rbp)
			// ->
			// movl -4(%rbp), %r10d
			// addl %r10d, -8(%rbp)

			moveToR10 := Instruction{
				kind:  INS_MOV,
				mov_a: inst.binary.a,
				mov_b: &Operand{kind: OPERAND_REG_R10},
			}

			addl := Instruction{
				kind: INS_BINARY,
				binary: &BinaryInstruction{
					op: inst.binary.op,
					a:  &Operand{kind: OPERAND_REG_R10},
					b:  inst.binary.b,
				},
			}
			fixedInstructions = append(fixedInstructions, moveToR10, addl)
		} else if inst.kind == INS_BINARY &&
			inst.binary.op == A_O_MUL &&
			inst.binary.b.kind == OPERAND_STACK {

			// imull $3, -4(%rbp)
			// ->
			// movl -4(%rbp), %r11d
			// imull $3, %r11d
			// movl %r11d, -4(%rbp)
			moveToR11 := Instruction{
				kind:  INS_MOV,
				mov_a: inst.binary.b,
				mov_b: &Operand{kind: OPERAND_REG_R11},
			}

			imull := Instruction{
				kind: INS_BINARY,
				binary: &BinaryInstruction{
					op: A_O_MUL,
					a:  inst.binary.a,
					b:  &Operand{kind: OPERAND_REG_R11},
				},
			}

			movToStack := Instruction{
				kind:  INS_MOV,
				mov_a: &Operand{kind: OPERAND_REG_R11},
				mov_b: inst.binary.b,
			}
			fixedInstructions = append(fixedInstructions, moveToR11, imull, movToStack)
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
	fixInstructions(&genFn.instructions)

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
