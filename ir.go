package mc

import "fmt"

type IrOperator int

const (
	IR_UNARY_COMPLEMENT IrOperator = iota
	IR_UNARY_NEGATE
	IR_BIN_ADD
	IR_BIN_SUB
	IR_BIN_MUL
	IR_BIN_DIV
	IR_BIN_REMAINDER
)

func (op IrOperator) String() string {
	switch op {
	case IR_UNARY_COMPLEMENT:
		return "~"
	case IR_UNARY_NEGATE:
		return "-"
	default:
		panic("non supported unary op")
	}
}

type IrValueKind int

const (
	IR_VAL_CONSTANT IrValueKind = iota
	IR_VAL_VAR
)

type IrVal struct {
	kind       IrValueKind
	constant   int64  // if IR_VAL_CONSTANT
	identifier string // if IR_VAL_VAR
}

func NewIrConstant(value int64) *IrVal {
	return &IrVal{
		kind:     IR_VAL_CONSTANT,
		constant: value,
	}
}

func NewIrVar(id string) *IrVal {
	return &IrVal{
		kind:       IR_VAL_VAR,
		identifier: id,
	}
}

func (v *IrVal) String() string {
	switch v.kind {
	case IR_VAL_CONSTANT:
		return fmt.Sprintf("%d", v.constant)
	case IR_VAL_VAR:
		return v.identifier
	default:
		return "unknown"
	}
}

func (v *IrVal) IsVar() bool {
	return v.kind == IR_VAL_VAR
}

func (v *IrVal) IsConstant() bool {
	return v.kind == IR_VAL_CONSTANT
}

type IrInstructionKind int

const (
	IR_INSTRUCTION_RETURN IrInstructionKind = iota
	IR_INSTRUCTION_UNARY
	IR_INSTRUCTION_BINARY
)

type IrReturnInstruction struct {
	val *IrVal
}

type IrUnaryInstruction struct {
	operator IrOperator
	src      *IrVal
	dst      *IrVal
}

type IrBinaryInstruction struct {
	operator IrOperator
	src1     *IrVal
	src2     *IrVal
	dst      *IrVal
}

type IrInstruction struct {
	kind   IrInstructionKind
	ret    *IrReturnInstruction
	unary  *IrUnaryInstruction
	binary *IrBinaryInstruction
}

func NewIrReturnInstruction(val *IrVal) *IrInstruction {
	return &IrInstruction{
		kind: IR_INSTRUCTION_RETURN,
		ret: &IrReturnInstruction{
			val: val,
		},
	}
}

func NewIrUnaryInstruction(operator IrOperator, src, dst *IrVal) *IrInstruction {
	if !dst.IsVar() {
		panic("destination of unary operation must be a variable")
	}

	return &IrInstruction{
		kind: IR_INSTRUCTION_UNARY,
		unary: &IrUnaryInstruction{
			operator: operator,
			src:      src,
			dst:      dst,
		},
	}
}

func (i *IrInstruction) String() string {
	switch i.kind {
	case IR_INSTRUCTION_RETURN:
		return fmt.Sprintf("return %s", i.ret.val.String())
	case IR_INSTRUCTION_UNARY:
		return fmt.Sprintf("%s = %s %s",
			i.unary.dst.String(),
			i.unary.operator.String(),
			i.unary.src.String())
	default:
		return "unknown instruction"
	}
}

type IrFunction struct {
	identifier string
	body       []*IrInstruction
}

func NewIrFunction(identifier string, body []*IrInstruction) *IrFunction {
	return &IrFunction{
		identifier: identifier,
		body:       body,
	}
}

type IrProgram struct {
	function *IrFunction
}

func NewIrProgram(function *IrFunction) *IrProgram {
	return &IrProgram{
		function: function,
	}
}

type IrGenerator struct {
	tempVarCounter int
}

func NewIrGenerator() *IrGenerator {
	return &IrGenerator{
		tempVarCounter: 0,
	}
}

func (g *IrGenerator) Generate(program *Program) *IrProgram {
	fnDef := program.mainFunction
	irFunction := g.generateFunction(fnDef)

	return NewIrProgram(irFunction)
}

func (g *IrGenerator) generateFunction(fnDef *FunctionDef) *IrFunction {
	instructions := []*IrInstruction{}
	g.generateStatement(fnDef.statement, &instructions)

	return NewIrFunction(fnDef.identifier, instructions)
}

func (g *IrGenerator) generateStatement(stmt *Statement, instructions *[]*IrInstruction) {
	switch stmt.kind {
	case STMT_RETURN:
		val := g.generateExpression(stmt.ret.expr, instructions)
		*instructions = append(*instructions, NewIrReturnInstruction(val))
	case STMT_IF:
		panic("if statements not implemented in IR generator")
	}
}

func (g *IrGenerator) generateExpression(expr *Expression, instructions *[]*IrInstruction) *IrVal {
	switch expr.kind {
	case EXP_INTEGER:
		return NewIrConstant(expr.integer)

	case EXP_UNARY:
		innerVal := g.generateExpression(expr.unary.expr, instructions)

		resultVar := g.newTempVar()

		var irOp IrOperator
		switch expr.unary.operator {
		case MINUS:
			irOp = IR_UNARY_NEGATE
		case TILDE:
			irOp = IR_UNARY_COMPLEMENT
		default:
			panic("unsupported unary operator")
		}

		*instructions = append(*instructions,
			NewIrUnaryInstruction(irOp, innerVal, resultVar))

		return resultVar
	case EXP_BINARY:
		v1 := g.generateExpression(expr.binary.lhs, instructions)
		v2 := g.generateExpression(expr.binary.rhs, instructions)

		dst := g.newTempVar()
		var irOp IrOperator
		switch expr.binary.operator {
		case MINUS:
			irOp = IR_BIN_SUB
		case TOK_ASTERISK:
			irOp = IR_BIN_MUL
		case TOK_PLUS:
			irOp = IR_BIN_ADD
		case TOK_SLASH:
			irOp = IR_BIN_DIV
		case TOK_PERCENT:
			irOp = IR_BIN_REMAINDER
		default:
			panic("unsupported unary operator")
		}

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_BINARY,
			binary: &IrBinaryInstruction{
				operator: irOp,
				src1:     v1,
				src2:     v2,
				dst:      dst,
			},
		})

		return dst
	default:
		panic("unsupported expression kind in IR generator")
	}
}

func (g *IrGenerator) newTempVar() *IrVal {
	varName := fmt.Sprintf("t%d", g.tempVarCounter)
	g.tempVarCounter++
	return NewIrVar(varName)
}
