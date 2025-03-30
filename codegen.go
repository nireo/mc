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

type InstructionKind int

const (
	INS_RET InstructionKind = iota
	INS_MOV
	INS_UNARY
	INS_ALLOC_STACK
	INS_CDQ
	INS_IDIV
	INS_BINARY
	INS_CMP
	INS_JMP
	INS_JMPCC
	INS_SETCC
	INS_LABEL
)

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
	kind InstructionKind
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
	switch i.kind {
	case INS_RET:
		return "\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret"
	case INS_MOV:
		mov := i.data.(*Mov)
		return fmt.Sprintf("\tmovl %s, %s", mov.a.String(), mov.b.String())
	case INS_ALLOC_STACK:
		size := i.data.(int64)
		return fmt.Sprintf("\tsubq $%d, %%rsp", size)
	case INS_UNARY:
		unary := i.data.(*UnaryInstruction)
		return fmt.Sprintf("\t%s %s", unary.op, unary.operand)
	case INS_BINARY:
		binary := i.data.(*BinaryInstruction)
		return fmt.Sprintf("\t%s %s, %s", binary.op, binary.a, binary.b)
	case INS_IDIV:
		idivl := i.data.(*Idivl)
		return fmt.Sprintf("\tidivl %s", idivl.op)
	case INS_CDQ:
		return "\tcdq"
	case INS_CMP:
		cmp := i.data.(*Cmp)
		return fmt.Sprintf("\tcmpl %s, %s", cmp.a, cmp.b)
	case INS_JMP:
		jmp := i.data.(*Jmp)
		return fmt.Sprintf("\tjmp .L%s", jmp.identifier)
	case INS_JMPCC:
		jmpcc := i.data.(*JmpCC)
		return fmt.Sprintf("\tj%s .L%s", jmpcc.condCode, jmpcc.identifier)
	case INS_SETCC:
		setcc := i.data.(*SetCC)
		if regName, shouldEmitSingleByte := oneByteRegisters[setcc.a.kind]; shouldEmitSingleByte {
			return fmt.Sprintf("\tset%s %s", setcc.condCode, regName)
		}
		// since the register wasn't ax, dx, r10 or r11. just emit the existing one

		return fmt.Sprintf("\tset%s %s", setcc.condCode, setcc.a)
	case INS_LABEL:
		return fmt.Sprintf(".L%s", i.data.(string))
	}

	return ""
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

func convertIrInstruction(ins *IrInstruction, instructions *[]Instruction) {
	switch ins.kind {
	case IR_INSTRUCTION_RETURN:
		retIr := ins.data.(*IrReturnInstruction)
		*instructions = append(*instructions, Instruction{
			kind: INS_MOV,
			data: &Mov{
				a: toOperand(retIr.val),
				b: Operand{kind: OPERAND_REG_AX},
			},
		}, Instruction{kind: INS_RET})
		return
	case IR_INSTRUCTION_UNARY:
		unaryIr := ins.data.(*IrUnaryInstruction)
		if unaryIr.operator == IR_UNARY_NOT {
			*instructions = append(*instructions, Instruction{
				kind: INS_CMP,
				data: &Cmp{
					a: Operand{kind: OPERAND_IMM, imm: 0},
					b: toOperand(unaryIr.src),
				},
			}, Instruction{
				kind: INS_MOV,
				data: &Mov{
					a: Operand{kind: OPERAND_IMM, imm: 0},
					b: toOperand(unaryIr.dst),
				},
			}, Instruction{
				kind: INS_SETCC,
				data: &SetCC{
					condCode: COND_E,
					a:        toOperand(unaryIr.dst),
				},
			})
		} else {
			*instructions = append(*instructions, Instruction{
				kind: INS_MOV,
				data: &Mov{
					a: toOperand(unaryIr.src),
					b: toOperand(unaryIr.dst),
				},
			}, Instruction{
				kind: INS_UNARY,
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
				Instruction{
					kind: INS_MOV,
					data: &Mov{
						a: toOperand(binaryIr.src1),
						b: Operand{kind: OPERAND_REG_AX},
					},
				},
				Instruction{kind: INS_CDQ},
				Instruction{
					kind: INS_IDIV,
					data: &Idivl{
						op: toOperand(binaryIr.src2),
					},
				},
				Instruction{
					kind: INS_MOV,
					data: &Mov{
						a: Operand{kind: OPERAND_REG_AX},
						b: toOperand(binaryIr.dst),
					},
				},
			)
		} else if binaryIr.operator == IR_BIN_REMAINDER {
			*instructions = append(*instructions,
				Instruction{
					kind: INS_MOV,
					data: &Mov{
						a: toOperand(binaryIr.src1),
						b: Operand{kind: OPERAND_REG_AX},
					},
				},
				Instruction{kind: INS_CDQ},
				Instruction{
					kind: INS_IDIV, data: &Idivl{
						op: toOperand(binaryIr.src2),
					},
				},

				Instruction{
					kind: INS_MOV,
					data: &Mov{
						a: Operand{kind: OPERAND_REG_DX},
						b: toOperand(binaryIr.dst),
					},
				},
			)
		} else if condCode, ok := relToCondCode[binaryIr.operator]; ok {
			*instructions = append(*instructions, Instruction{
				kind: INS_CMP,
				data: &Cmp{
					a: toOperand(binaryIr.src2),
					b: toOperand(binaryIr.src1),
				},
			}, Instruction{
				kind: INS_MOV,
				data: &Mov{
					a: Operand{kind: OPERAND_IMM, imm: 0},
					b: toOperand(binaryIr.dst),
				},
			}, Instruction{
				kind: INS_SETCC,
				data: &SetCC{
					condCode: condCode,
					a:        toOperand(binaryIr.dst),
				},
			})
		} else {
			*instructions = append(*instructions, Instruction{
				kind: INS_MOV,
				data: &Mov{
					a: toOperand(binaryIr.src1),
					b: toOperand(binaryIr.dst),
				},
			}, Instruction{
				kind: INS_BINARY,
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
			kind: INS_JMP,
			data: &Jmp{
				identifier: jumpLabel,
			},
		})
	case IR_INSTRUCTION_JUMP_IF_ZERO:
		jumpIfZero := ins.data.(*IrJumpIfZero)
		*instructions = append(*instructions, Instruction{
			kind: INS_CMP,
			data: &Cmp{
				a: Operand{kind: OPERAND_IMM, imm: 0},
				b: toOperand(jumpIfZero.condition),
			},
		}, Instruction{
			kind: INS_JMPCC,
			data: &JmpCC{
				condCode:   COND_E,
				identifier: jumpIfZero.target,
			},
		})
	case IR_INSTRUCTION_JUMP_IF_NOT_ZERO:
		jmpifn0 := ins.data.(*IrJumpIfNotZero)
		*instructions = append(*instructions, Instruction{
			kind: INS_CMP,
			data: &Cmp{
				a: Operand{kind: OPERAND_IMM, imm: 0},
				b: toOperand(jmpifn0.condition),
			},
		}, Instruction{
			kind: INS_JMPCC,
			data: &JmpCC{
				condCode:   COND_NE,
				identifier: jmpifn0.target,
			},
		})
	case IR_INSTRUCTION_COPY:
		cpy := ins.data.(*IrCopyInstruction)
		*instructions = append(*instructions, Instruction{
			kind: INS_MOV,
			data: &Mov{
				a: toOperand(cpy.src),
				b: toOperand(cpy.dst),
			},
		})
	case IR_INSTRUCTION_LABEL:
		label := ins.data.(string)
		*instructions = append(*instructions, Instruction{
			kind: INS_LABEL,
			data: label,
		})
	}
}

func replacePseudoRegisters(insts *[]Instruction) int {
	pseudoToStack := make(map[string]int64)
	currentOffset := int64(-4)

	for _, inst := range *insts {
		registerPseudo := func(op Operand) {
			if op.kind == OPERAND_PSEUDO {
				if _, exists := pseudoToStack[op.ident]; !exists {
					pseudoToStack[op.ident] = currentOffset
					currentOffset -= 4
				}
			}
		}

		switch inst.kind {
		case INS_MOV:
			mov := inst.data.(*Mov)
			registerPseudo(mov.a)
			registerPseudo(mov.b)
		case INS_IDIV:
			idivl := inst.data.(*Idivl)
			registerPseudo(idivl.op)
		case INS_UNARY:
			unary := inst.data.(*UnaryInstruction)
			registerPseudo(unary.operand)
		case INS_BINARY:
			bin := inst.data.(*BinaryInstruction)
			registerPseudo(bin.a)
			registerPseudo(bin.b)
		case INS_CMP:
			cmp := inst.data.(*Cmp)
			registerPseudo(cmp.a)
			registerPseudo(cmp.b)
		case INS_SETCC:
			setcc := inst.data.(*SetCC)
			registerPseudo(setcc.a)
		}
	}

	for i := range *insts {
		inst := &(*insts)[i]

		replacePseudo := func(op *Operand) {
			if op.kind == OPERAND_PSEUDO {
				if offset, exists := pseudoToStack[op.ident]; exists {
					op.kind = OPERAND_STACK
					op.imm = offset
				}
			}
		}

		switch inst.kind {
		case INS_MOV:
			mov := inst.data.(*Mov)
			replacePseudo(&mov.a)
			replacePseudo(&mov.b)
		case INS_IDIV:
			idivl := inst.data.(*Idivl)
			replacePseudo(&idivl.op)
		case INS_UNARY:
			unary := inst.data.(*UnaryInstruction)
			replacePseudo(&unary.operand)
		case INS_BINARY:
			bin := inst.data.(*BinaryInstruction)
			replacePseudo(&bin.a)
			replacePseudo(&bin.b)
		case INS_CMP:
			cmp := inst.data.(*Cmp)
			replacePseudo(&cmp.a)
			replacePseudo(&cmp.b)
		case INS_SETCC:
			setcc := inst.data.(*SetCC)
			replacePseudo(&setcc.a)
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
			moveToR10 := Instruction{
				kind: INS_MOV,
				data: &Mov{
					a: mov.a,
					b: Operand{kind: OPERAND_REG_R10},
				},
			}

			moveFromR10 := Instruction{
				kind: INS_MOV,
				data: &Mov{
					a: Operand{kind: OPERAND_REG_R10},
					b: mov.b,
				},
			}

			fixedInstructions = append(fixedInstructions, moveToR10, moveFromR10)
		} else if idivl, ok := inst.data.(*Idivl); ok && idivl.op.kind == OPERAND_IMM {
			// idivl $3
			// ->
			// movl $3, %r10d
			// idivl %r10d

			movToR10 := Instruction{
				kind: INS_MOV,
				data: &Mov{
					a: idivl.op,
					b: Operand{kind: OPERAND_REG_R10},
				},
			}

			idivl := Instruction{
				kind: INS_IDIV,
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

			moveToR10 := Instruction{
				kind: INS_MOV,
				data: &Mov{
					a: binary.a,
					b: Operand{kind: OPERAND_REG_R10},
				},
			}

			addl := Instruction{
				kind: INS_BINARY,
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
			moveToR11 := Instruction{
				kind: INS_MOV,
				data: &Mov{
					a: binary.b,
					b: Operand{kind: OPERAND_REG_R11},
				},
			}

			imull := Instruction{
				kind: INS_BINARY,
				data: &BinaryInstruction{
					op: A_O_MUL,
					a:  binary.a,
					b:  Operand{kind: OPERAND_REG_R11},
				},
			}

			movToStack := Instruction{
				kind: INS_MOV,
				data: &Mov{
					a: Operand{kind: OPERAND_REG_R11},
					b: binary.b,
				},
			}
			fixedInstructions = append(fixedInstructions, moveToR11, imull, movToStack)
		} else if cmp, ok := inst.data.(*Cmp); ok &&
			cmp.a.kind == OPERAND_STACK && cmp.b.kind == OPERAND_STACK {
			// both operands cannot be stack addresses therefore we need to fix it
			// for example:
			// cmpl -4(%rbp), -8(%rbp)
			// ->
			// movl -4(%rbp), %r10d
			// cmpl %r10d, -8(%rbp)

			moveToR10 := Instruction{
				kind: INS_MOV,
				data: &Mov{
					a: cmp.a,
					b: Operand{kind: OPERAND_REG_R10},
				},
			}

			cmpl := Instruction{
				kind: INS_CMP,
				data: &Cmp{
					a: Operand{kind: OPERAND_REG_R10},
					b: cmp.b,
				},
			}

			fixedInstructions = append(fixedInstructions, moveToR10, cmpl)
		} else if cmp, ok := inst.data.(*Cmp); ok && cmp.b.kind == OPERAND_IMM {
			moveConstant := Instruction{
				kind: INS_MOV,
				data: &Mov{
					a: cmp.b,
					b: Operand{kind: OPERAND_REG_R11},
				},
			}

			cmpl := Instruction{
				kind: INS_CMP,
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
			kind: INS_ALLOC_STACK,
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
