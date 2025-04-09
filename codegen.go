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

type ConditionCode string

const (
	COND_E  ConditionCode = "e"
	COND_NE ConditionCode = "ne"
	COND_G  ConditionCode = "g"
	COND_GE ConditionCode = "ge"
	COND_L  ConditionCode = "l"
	COND_LE ConditionCode = "le"
)

var relToCondCode = map[IrOperator]ConditionCode{
	IR_BIN_EQ:   COND_E,
	IR_BIN_NEQ:  COND_NE,
	IR_BIN_LT:   COND_L,
	IR_BIN_GT:   COND_G,
	IR_BIN_LTEQ: COND_LE,
	IR_BIN_GTEQ: COND_GE,
}

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

var oneByteRegisters = map[OperandKind]string{
	OPERAND_REG_AX:  "%al",
	OPERAND_REG_DX:  "%dl",
	OPERAND_REG_R10: "%r10b",
	OPERAND_REG_R11: "%r11b",
}

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

type Cdq struct{}
type Ret struct{}

type UnaryInstruction struct {
	op      Operation
	operand Operand
}

type BinaryInstruction struct {
	op Operation
	a  Operand
	b  Operand
}

type Cmp struct {
	a Operand
	b Operand
}

type Jmp struct {
	identifier string
}

type JmpCC struct {
	condCode   ConditionCode
	identifier string
}

type SetCC struct {
	condCode ConditionCode
	a        Operand
}

type Idivl struct {
	op Operand
}

type Mov struct {
	a Operand
	b Operand
}

type Instruction struct {
	data any
}

type GenFunction struct {
	name         string
	instructions []Instruction
}

type GenProgram struct {
	fn *GenFunction
}

func (i Instruction) String() string {
	switch in := i.data.(type) {
	case *Ret:
		return "\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret"
	case *Mov:
		return fmt.Sprintf("\tmovl %s, %s", in.a.String(), in.b.String())
	case int64:
		return fmt.Sprintf("\tsubq $%d, %%rsp", in)
	case *UnaryInstruction:
		return fmt.Sprintf("\t%s %s", in.op, in.operand)
	case *BinaryInstruction:
		return fmt.Sprintf("\t%s %s, %s", in.op, in.a, in.b)
	case *Idivl:
		return fmt.Sprintf("\tidivl %s", in.op)
	case *Cdq:
		return "\tcdq"
	case *Cmp:
		return fmt.Sprintf("\tcmpl %s, %s", in.a, in.b)
	case *Jmp:
		return fmt.Sprintf("\tjmp .L%s", in.identifier)
	case *JmpCC:
		return fmt.Sprintf("\tj%s .L%s", in.condCode, in.identifier)
	case *SetCC:
		if regName, shouldEmitSingleByte := oneByteRegisters[in.a.kind]; shouldEmitSingleByte {
			return fmt.Sprintf("\tset%s %s", in.condCode, regName)
		}
		// since the register wasn't ax, dx, r10 or r11. just emit the existing one
		return fmt.Sprintf("\tset%s %s", in.condCode, in.a)
	case string:
		return fmt.Sprintf(".L%s:", i.data.(string))
	}
	panic("unsupported isntructions")
}

func toOperand(val *IrVal) Operand {
	switch val.kind {
	case IR_VAL_CONSTANT:
		return Operand{kind: OPERAND_IMM, imm: val.constant}
	case IR_VAL_VAR:
		return Operand{kind: OPERAND_PSEUDO, ident: val.identifier}
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
		return A_O_REMAINDER
	case IR_BIN_SUB:
		return A_O_SUB
	case IR_BIN_MUL:
		return A_O_MUL
	case IR_BIN_DIV:
		return A_O_DIV
	}

	panic("unrecognized ir operator")
}

func newMov(src Operand, dst Operand) Instruction {
	return Instruction{
		data: &Mov{
			a: src,
			b: dst,
		},
	}
}

func convertIrInstruction(ins *IrInstruction, instructions *[]Instruction) {
	switch ins.kind {
	case IR_INSTRUCTION_RETURN:
		retIr := ins.data.(*IrReturnInstruction)
		*instructions = append(*instructions, newMov(toOperand(retIr.val), Operand{kind: OPERAND_REG_AX}),
			Instruction{data: &Ret{}})
		return
	case IR_INSTRUCTION_UNARY:
		unaryIr := ins.data.(*IrUnaryInstruction)
		if unaryIr.operator == IR_UNARY_NOT {
			*instructions = append(*instructions, Instruction{
				data: &Cmp{
					a: Operand{kind: OPERAND_IMM, imm: 0},
					b: toOperand(unaryIr.src),
				},
			}, newMov(Operand{kind: OPERAND_IMM, imm: 0}, toOperand(unaryIr.dst)),
				Instruction{
					data: &SetCC{
						condCode: COND_E,
						a:        toOperand(unaryIr.dst),
					},
				})
		} else {
			*instructions = append(*instructions,
				newMov(toOperand(unaryIr.src), toOperand(unaryIr.dst)), Instruction{
					data: &UnaryInstruction{
						op:      convertIrOperator(unaryIr.operator),
						operand: toOperand(unaryIr.dst),
					},
				})
		}
	case IR_INSTRUCTION_BINARY:
		binaryIr := ins.data.(*IrBinaryInstruction)
		if binaryIr.operator == IR_BIN_DIV {
			*instructions = append(*instructions,
				newMov(toOperand(binaryIr.src1), Operand{kind: OPERAND_REG_AX}),
				Instruction{data: &Cdq{}},
				Instruction{
					data: &Idivl{
						op: toOperand(binaryIr.src2),
					},
				},
				newMov(Operand{kind: OPERAND_REG_AX}, toOperand(binaryIr.dst)),
			)
		} else if binaryIr.operator == IR_BIN_REMAINDER {
			*instructions = append(*instructions,
				newMov(toOperand(binaryIr.src1), Operand{kind: OPERAND_REG_AX}),
				Instruction{data: &Cdq{}},
				Instruction{
					data: &Idivl{
						op: toOperand(binaryIr.src2),
					},
				},
				newMov(Operand{kind: OPERAND_REG_DX}, toOperand(binaryIr.dst)),
			)
		} else if condCode, ok := relToCondCode[binaryIr.operator]; ok {
			*instructions = append(*instructions, Instruction{
				data: &Cmp{
					a: toOperand(binaryIr.src2),
					b: toOperand(binaryIr.src1),
				},
			},
				newMov(Operand{kind: OPERAND_IMM, imm: 0}, toOperand(binaryIr.dst)),
				Instruction{
					data: &SetCC{
						condCode: condCode,
						a:        toOperand(binaryIr.dst),
					},
				})
		} else {
			*instructions = append(*instructions,
				newMov(toOperand(binaryIr.src1), toOperand(binaryIr.dst)),
				Instruction{
					data: &BinaryInstruction{
						op: convertIrOperator(binaryIr.operator),
						a:  toOperand(binaryIr.src2),
						b:  toOperand(binaryIr.dst),
					},
				})
		}
	case IR_INSTRUCTION_JUMP:
		jumpLabel := ins.data.(string)
		*instructions = append(*instructions, Instruction{
			data: &Jmp{
				identifier: jumpLabel,
			},
		})
	case IR_INSTRUCTION_JUMP_IF_ZERO:
		jumpIfZero := ins.data.(*IrJumpIfZero)
		*instructions = append(*instructions, Instruction{
			data: &Cmp{
				a: Operand{kind: OPERAND_IMM, imm: 0},
				b: toOperand(jumpIfZero.condition),
			},
		}, Instruction{
			data: &JmpCC{
				condCode:   COND_E,
				identifier: jumpIfZero.target,
			},
		})
	case IR_INSTRUCTION_JUMP_IF_NOT_ZERO:
		jmpifn0 := ins.data.(*IrJumpIfNotZero)
		*instructions = append(*instructions, Instruction{
			data: &Cmp{
				a: Operand{kind: OPERAND_IMM, imm: 0},
				b: toOperand(jmpifn0.condition),
			},
		}, Instruction{
			data: &JmpCC{
				condCode:   COND_NE,
				identifier: jmpifn0.target,
			},
		})
	case IR_INSTRUCTION_COPY:
		cpy := ins.data.(*IrCopyInstruction)
		*instructions = append(*instructions, newMov(toOperand(cpy.src), toOperand(cpy.dst)))
	case IR_INSTRUCTION_LABEL:
		label := ins.data.(string)
		*instructions = append(*instructions, Instruction{
			data: label,
		})
	}
}

func replacePseudoRegisters(insts *[]Instruction) int {
	pseudoToStack := make(map[string]int64)
	currentOffset := int64(-4)

	registerPseudo := func(op Operand) {
		if op.kind == OPERAND_PSEUDO {
			if _, exists := pseudoToStack[op.ident]; !exists {
				pseudoToStack[op.ident] = currentOffset
				currentOffset -= 4
			}
		}
	}
	for _, inst := range *insts {
		switch in := inst.data.(type) {
		case *Mov:
			registerPseudo(in.a)
			registerPseudo(in.b)
		case *Idivl:
			registerPseudo(in.op)
		case *UnaryInstruction:
			registerPseudo(in.operand)
		case *BinaryInstruction:
			registerPseudo(in.a)
			registerPseudo(in.b)
		case *Cmp:
			registerPseudo(in.a)
			registerPseudo(in.b)
		case *SetCC:
			registerPseudo(in.a)
		}
	}

	replacePseudo := func(op *Operand) {
		if op.kind == OPERAND_PSEUDO {
			if offset, exists := pseudoToStack[op.ident]; exists {
				op.kind = OPERAND_STACK
				op.imm = offset
			}
		}
	}

	for i := range *insts {
		inst := &(*insts)[i]
		switch in := inst.data.(type) {
		case *Mov:
			replacePseudo(&in.a)
			replacePseudo(&in.b)
		case *Idivl:
			replacePseudo(&in.op)
		case *UnaryInstruction:
			replacePseudo(&in.operand)
		case *BinaryInstruction:
			replacePseudo(&in.a)
			replacePseudo(&in.b)
		case *Cmp:
			replacePseudo(&in.a)
			replacePseudo(&in.b)
		case *SetCC:
			replacePseudo(&in.a)
		}
	}

	return len(pseudoToStack) * 4
}

func fixInstructions(insts *[]Instruction) {
	var fixedInstructions []Instruction

	for _, inst := range *insts {
		if mov, ok := inst.data.(*Mov); ok && mov.a.kind == OPERAND_STACK && mov.b.kind == OPERAND_STACK {
			// movl -4(%rbp), -8(%rbp)
			// ->
			// movl -4(%rbp), %r10d
			// movl %r10d, -8(%rbp)
			moveToR10 := newMov(mov.a, Operand{kind: OPERAND_REG_R10})
			moveFromR10 := newMov(Operand{kind: OPERAND_REG_R10}, mov.b)

			fixedInstructions = append(fixedInstructions, moveToR10, moveFromR10)
		} else if idivl, ok := inst.data.(*Idivl); ok && idivl.op.kind == OPERAND_IMM {
			// idivl $3
			// ->
			// movl $3, %r10d
			// idivl %r10d

			movToR10 := newMov(idivl.op, Operand{kind: OPERAND_REG_R10})
			idivl := Instruction{
				data: &Idivl{
					op: Operand{kind: OPERAND_REG_R10},
				},
			}
			fixedInstructions = append(fixedInstructions, movToR10, idivl)
		} else if binary, ok := inst.data.(*BinaryInstruction); ok &&
			(binary.op == A_O_ADD || binary.op == A_O_SUB) &&
			binary.a.kind == OPERAND_STACK &&
			binary.b.kind == OPERAND_STACK {

			// addl -4(%rbp), -8(%rbp)
			// ->
			// movl -4(%rbp), %r10d
			// addl %r10d, -8(%rbp)

			moveToR10 := newMov(binary.a, Operand{kind: OPERAND_REG_R10})
			addl := Instruction{
				data: &BinaryInstruction{
					op: binary.op,
					a:  Operand{kind: OPERAND_REG_R10},
					b:  binary.b,
				},
			}
			fixedInstructions = append(fixedInstructions, moveToR10, addl)
		} else if binary, ok := inst.data.(*BinaryInstruction); ok &&
			binary.op == A_O_MUL &&
			binary.b.kind == OPERAND_STACK {

			// imull $3, -4(%rbp)
			// ->
			// movl -4(%rbp), %r11d
			// imull $3, %r11d
			// movl %r11d, -4(%rbp)
			moveToR11 := newMov(binary.b, Operand{kind: OPERAND_REG_R11})
			imull := Instruction{
				data: &BinaryInstruction{
					op: A_O_MUL,
					a:  binary.a,
					b:  Operand{kind: OPERAND_REG_R11},
				},
			}

			movToStack := newMov(Operand{kind: OPERAND_REG_R11}, binary.b)
			fixedInstructions = append(fixedInstructions, moveToR11, imull, movToStack)
		} else if cmp, ok := inst.data.(*Cmp); ok &&
			cmp.a.kind == OPERAND_STACK && cmp.b.kind == OPERAND_STACK {
			// both operands cannot be stack addresses therefore we need to fix it
			// for example:
			// cmpl -4(%rbp), -8(%rbp)
			// ->
			// movl -4(%rbp), %r10d
			// cmpl %r10d, -8(%rbp)

			moveToR10 := newMov(cmp.a, Operand{kind: OPERAND_REG_R10})
			cmpl := Instruction{
				data: &Cmp{
					a: Operand{kind: OPERAND_REG_R10},
					b: cmp.b,
				},
			}

			fixedInstructions = append(fixedInstructions, moveToR10, cmpl)
		} else if cmp, ok := inst.data.(*Cmp); ok && cmp.b.kind == OPERAND_IMM {

			moveConstant := newMov(cmp.b, Operand{kind: OPERAND_REG_R11})
			cmpl := Instruction{
				data: &Cmp{
					a: cmp.a,
					b: Operand{kind: OPERAND_REG_R11},
				},
			}

			fixedInstructions = append(fixedInstructions, moveConstant, cmpl)
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
			data: int64(stackSize),
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
