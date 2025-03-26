package mc

import (
	"fmt"
)

type OperandKind int

const (
	OPERAND_IMM OperandKind = iota
	OPERAND_REG
)

type Operand struct {
	kind OperandKind
	imm  int64
}

func (o Operand) String() string {
	switch o.kind {
	case OPERAND_IMM:
		return fmt.Sprintf("$%d", o.imm)
	case OPERAND_REG:
		return "%eax"
	}
	return ""
}

type InstructionKind int

const (
	INS_RET InstructionKind = iota
	INS_MOV
)

type Instruction struct {
	kind InstructionKind
	mov  struct {
		a *Operand
		b *Operand
	}
}

func (i Instruction) String() string {
	switch i.kind {
	case INS_RET:
		return "ret"
	case INS_MOV:
		return fmt.Sprintf("movl %s %s", i.mov.a.String(), i.mov.b.String())
	}

	return ""
}
