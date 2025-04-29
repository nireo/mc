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
	OPERAND_REG_CX
	OPERAND_REG_DI
	OPERAND_REG_SI
	OPERAND_REG_R8
	OPERAND_REG_R9
)

func (o Operand) isRegister() bool {
	return o.kind == OPERAND_REG_AX || o.kind == OPERAND_REG_DX || o.kind == OPERAND_REG_R10 || o.kind == OPERAND_REG_R11 ||
		o.kind == OPERAND_REG_CX || o.kind == OPERAND_REG_DI || o.kind == OPERAND_REG_SI || o.kind == OPERAND_REG_R8 || o.kind == OPERAND_REG_R9
}

var oneByteRegisters = map[OperandKind]string{
	OPERAND_REG_AX:  "%al",
	OPERAND_REG_DX:  "%dl",
	OPERAND_REG_R10: "%r10b",
	OPERAND_REG_R11: "%r11b",
}

func regWithN(reg Operand, bytes int) string {
	switch reg.kind {
	case OPERAND_REG_AX:
		switch bytes {
		case 8:
			return "%rax"
		case 4:
			return "%eax"
		case 1:
			return "%al"
		default:
			panic(fmt.Sprintf("Unsupported byte size %d for register AX", bytes))
		}
	case OPERAND_REG_DX:
		switch bytes {
		case 8:
			return "%rdx"
		case 4:
			return "%edx"
		case 1:
			return "%dl"
		default:
			panic(fmt.Sprintf("Unsupported byte size %d for register DX", bytes))
		}
	case OPERAND_REG_R10:
		switch bytes {
		case 8:
			return "%r10"
		case 4:
			return "%r10d"
		case 1:
			return "%r10b"
		default:
			panic(fmt.Sprintf("Unsupported byte size %d for register R10", bytes))
		}
	case OPERAND_REG_R11:
		switch bytes {
		case 8:
			return "%r11"
		case 4:
			return "%r11d"
		case 1:
			return "%r11b"
		default:
			panic(fmt.Sprintf("Unsupported byte size %d for register R11", bytes))
		}
	// --- Add cases for other registers if OperandKind is expanded ---
	case OPERAND_REG_CX:
		switch bytes {
		case 8:
			return "%rcx"
		case 4:
			return "%ecx"
		case 1:
			return "%cl"
		default:
			panic(fmt.Sprintf("unsupported byte size %d for register cx", bytes))
		}
	case OPERAND_REG_DI:
		switch bytes {
		case 8:
			return "%rdi"
		case 4:
			return "%edi"
		case 1:
			return "%dil"
		default:
			panic(fmt.Sprintf("unsupported byte size %d for register cx", bytes))
		}
	case OPERAND_REG_SI:
		switch bytes {
		case 8:
			return "%rsi"
		case 4:
			return "%esi"
		case 1:
			return "%sil"
		default:
			panic(fmt.Sprintf("unsupported byte size %d for register cx", bytes))
		}
	case OPERAND_REG_R8:
		switch bytes {
		case 8:
			return "%r8"
		case 4:
			return "%r8d"
		case 1:
			return "%r8b"
		default:
			panic(fmt.Sprintf("unsupported byte size %d for register cx", bytes))
		}
	case OPERAND_REG_R9:
		switch bytes {
		case 8:
			return "%r9"
		case 4:
			return "%r9d"
		case 1:
			return "%r9b"
		default:
			panic(fmt.Sprintf("unsupported byte size %d for register cx", bytes))
		}
	default:
		panic(fmt.Sprintf("Operand kind %v is not a register supported by regWithN", reg.kind))
	}
}

type Operand struct {
	kind  OperandKind
	imm   int64
	ident string
}

func (o Operand) String() string {
	if o.isRegister() {
		return regWithN(o, 4)
	}

	switch o.kind {
	case OPERAND_IMM:
		return fmt.Sprintf("$%d", o.imm)
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

type Push struct {
	op Operand
}

type Call struct {
	ident string
}

type DeallocateStack struct {
	bytesToRemove int64
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
	name string
	ib   InstructionBuilder
}

type GenProgram struct {
	functions []*GenFunction
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
	case *DeallocateStack:
		return fmt.Sprintf("\taddq $%d, %%rsp", in.bytesToRemove)
	case *Call:
		return fmt.Sprintf("\tcall %s@PLT", in.ident)
	case *Push:
		return fmt.Sprintf("\tpushq %s", in.op)
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

type InstructionBuilder struct {
	insts []Instruction
}

func (ib *InstructionBuilder) emit(instructions ...Instruction) {
	ib.insts = append(ib.insts, instructions...)
}

func (ib *InstructionBuilder) replacePseudoRegisters() int {
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
	for _, inst := range ib.insts {
		switch in := inst.data.(type) {
		case *Push:
			registerPseudo(in.op)
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

	for i := range ib.insts {
		inst := &ib.insts[i]
		switch in := inst.data.(type) {
		case *Mov:
			replacePseudo(&in.a)
			replacePseudo(&in.b)
		case *Idivl:
			replacePseudo(&in.op)
		case *Push:
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

func (ib *InstructionBuilder) fixInstructions() {
	// prealloc atleast the size of the instructions since we know the amount of instructions is equal
	// or more to the fixed instructions
	fixedInstructions := make([]Instruction, 0, len(ib.insts))

	for _, inst := range ib.insts {
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

	ib.insts = fixedInstructions
}

func (ib *InstructionBuilder) emitFunctionCall(irfn *IrFuncCall) {
	argRegisters := []Operand{
		{kind: OPERAND_REG_DI},
		{kind: OPERAND_REG_SI},
		{kind: OPERAND_REG_DX},
		{kind: OPERAND_REG_CX},
		{kind: OPERAND_REG_R8},
		{kind: OPERAND_REG_R9},
	}

	var registerArgs []*IrVal
	var stackArgs []*IrVal

	if len(irfn.args) < 6 {
		registerArgs = irfn.args
	} else {
		registerArgs = irfn.args[:6]
		stackArgs = irfn.args[6:]
	}

	stackPadding := 0
	if len(stackArgs)%2 == 1 {
		stackPadding = 8
	}

	if stackPadding != 0 {
		ib.emit(Instruction{
			data: int64(stackPadding),
		})
	}

	for rIdx, rArg := range registerArgs {
		assemblyArg := toOperand(rArg)
		ib.emit(Instruction{
			data: &Mov{
				a: assemblyArg,
				b: argRegisters[rIdx],
			},
		})
	}

	for idx := len(stackArgs) - 1; idx >= 0; idx-- {
		assemblyArg := toOperand(stackArgs[idx])
		if assemblyArg.isRegister() || assemblyArg.kind == OPERAND_IMM {
			ib.emit(Instruction{
				data: &Push{
					op: assemblyArg,
				},
			})
		} else {
			ib.emit(Instruction{
				data: &Mov{
					a: assemblyArg,
					b: Operand{kind: OPERAND_REG_AX},
				},
			}, Instruction{
				data: &Push{
					op: Operand{kind: OPERAND_REG_AX},
				},
			})
		}
	}

	ib.emit(Instruction{
		data: &Call{
			ident: irfn.identifier,
		},
	})

	bytesToRemove := int64(8*len(stackArgs) + stackPadding)
	if bytesToRemove != 0 {
		ib.emit(Instruction{
			data: &DeallocateStack{
				bytesToRemove: bytesToRemove,
			},
		})
	}

	assemblyDst := toOperand(irfn.dst)
	ib.emit(Instruction{
		data: &Mov{
			a: Operand{kind: OPERAND_REG_AX},
			b: assemblyDst,
		},
	})
}

func (ib *InstructionBuilder) emitIrInst(ins *IrInstruction) {
	switch in := ins.data.(type) {
	case *IrReturnInstruction:
		ib.emit(newMov(toOperand(in.val), Operand{kind: OPERAND_REG_AX}), Instruction{data: &Ret{}})
	case *IrFuncCall:
		ib.emitFunctionCall(in)
	case *IrUnaryInstruction:
		if in.operator == IR_UNARY_NOT {
			ib.emit(Instruction{
				data: &Cmp{
					a: Operand{kind: OPERAND_IMM, imm: 0},
					b: toOperand(in.src),
				},
			}, newMov(Operand{kind: OPERAND_IMM, imm: 0}, toOperand(in.dst)),
				Instruction{
					data: &SetCC{
						condCode: COND_E,
						a:        toOperand(in.dst),
					},
				})
		} else {
			ib.emit(newMov(toOperand(in.src), toOperand(in.dst)), Instruction{
				data: &UnaryInstruction{
					op:      convertIrOperator(in.operator),
					operand: toOperand(in.dst),
				},
			})
		}
	case *IrBinaryInstruction:
		if in.operator == IR_BIN_DIV {
			ib.emit(newMov(toOperand(in.src1), Operand{kind: OPERAND_REG_AX}),
				Instruction{data: &Cdq{}},
				Instruction{
					data: &Idivl{
						op: toOperand(in.src2),
					},
				},
				newMov(Operand{kind: OPERAND_REG_AX}, toOperand(in.dst)),
			)
		} else if in.operator == IR_BIN_REMAINDER {
			ib.emit(newMov(toOperand(in.src1), Operand{kind: OPERAND_REG_AX}),
				Instruction{data: &Cdq{}},
				Instruction{
					data: &Idivl{
						op: toOperand(in.src2),
					},
				},
				newMov(Operand{kind: OPERAND_REG_DX}, toOperand(in.dst)),
			)
		} else if condCode, ok := relToCondCode[in.operator]; ok {
			ib.emit(Instruction{
				data: &Cmp{
					a: toOperand(in.src2),
					b: toOperand(in.src1),
				},
			},
				newMov(Operand{kind: OPERAND_IMM, imm: 0}, toOperand(in.dst)),
				Instruction{
					data: &SetCC{
						condCode: condCode,
						a:        toOperand(in.dst),
					},
				})
		} else {
			ib.emit(newMov(toOperand(in.src1), toOperand(in.dst)),
				Instruction{
					data: &BinaryInstruction{
						op: convertIrOperator(in.operator),
						a:  toOperand(in.src2),
						b:  toOperand(in.dst),
					},
				})
		}
	case *IrJump:
		ib.emit(Instruction{data: &Jmp{identifier: in.target}})
	case *IrJumpIfZero:
		ib.emit(Instruction{
			data: &Cmp{
				a: Operand{kind: OPERAND_IMM, imm: 0},
				b: toOperand(in.condition),
			},
		}, Instruction{
			data: &JmpCC{
				condCode:   COND_E,
				identifier: in.target,
			},
		})
	case *IrJumpIfNotZero:
		ib.emit(Instruction{
			data: &Cmp{
				a: Operand{kind: OPERAND_IMM, imm: 0},
				b: toOperand(in.condition),
			},
		}, Instruction{
			data: &JmpCC{
				condCode:   COND_NE,
				identifier: in.target,
			},
		})
	case *IrCopyInstruction:
		ib.emit(newMov(toOperand(in.src), toOperand(in.dst)))
	case *IrLabel:
		ib.emit(Instruction{data: in.label})
	}
}

func GenerateIr(prog *IrProgram) *GenProgram {
	// TODO: maybe add workers here since at this point the functions can be
	// generated independently from each other.

	funcs := make([]*GenFunction, 0, len(prog.functions))
	for _, fn := range prog.functions {
		genFn := &GenFunction{
			name: fn.identifier,
			ib: InstructionBuilder{
				insts: make([]Instruction, 0),
			},
		}

		for _, inst := range fn.body {
			genFn.ib.emitIrInst(inst)
		}

		stackSize := genFn.ib.replacePseudoRegisters()
		genFn.ib.fixInstructions()

		if stackSize > 0 {
			allocInst := Instruction{
				data: int64(stackSize),
			}

			genFn.ib.insts = append([]Instruction{allocInst}, genFn.ib.insts...)
		}

		funcs = append(funcs, genFn)
	}

	return &GenProgram{
		functions: funcs,
	}
}

type CodeEmitter struct{}

func NewCodeEmitter() *CodeEmitter {
	return &CodeEmitter{}
}

func (ce *CodeEmitter) EmitProgram(program *GenProgram, writer io.Writer) error {
	for _, fn := range program.functions {
		if err := ce.EmitFunction(fn, writer); err != nil {
			return err
		}
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

	for _, instruction := range function.ib.insts {
		if _, err := fmt.Fprintln(writer, instruction.String()); err != nil {
			return err
		}
	}

	return nil
}
