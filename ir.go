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
	IR_UNARY_NOT
	IR_BIN_GT
	IR_BIN_LT
	IR_BIN_GTEQ
	IR_BIN_LTEQ
	IR_BIN_NEQ
	IR_BIN_EQ
)

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
	IR_INSTRUCTION_COPY
	IR_INSTRUCTION_JUMP
	IR_INSTRUCTION_JUMP_IF_ZERO
	IR_INSTRUCTION_JUMP_IF_NOT_ZERO
	IR_INSTRUCTION_LABEL
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

type IrCopyInstruction struct {
	src *IrVal
	dst *IrVal
}

type IrJumpIfZero struct {
	condition *IrVal
	target    string // label
}

type IrJumpIfNotZero struct {
	condition *IrVal
	target    string
}

type IrInstruction struct {
	kind IrInstructionKind
	data any
}

func NewIrReturnInstruction(val *IrVal) *IrInstruction {
	return &IrInstruction{
		kind: IR_INSTRUCTION_RETURN,
		data: &IrReturnInstruction{
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
		data: &IrUnaryInstruction{
			operator: operator,
			src:      src,
			dst:      dst,
		},
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
	labelCount     int
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

func (g *IrGenerator) generateBlock(block *Block, instructions *[]*IrInstruction) {
	for _, block := range block.items {
		if block.kind == BLOCK_KIND_DECL {
			// we only need to generate something if the declaration contains a initializer
			decl := block.data.(*Declaration)
			if decl.init != nil {
				g.generateExpression(&Expression{
					kind: EXP_ASSIGN,
					data: &AssignExpr{
						lvalue: &Expression{
							kind: EXP_VAR,
							data: decl.identifier,
						},
						avalue: decl.init,
					},
				}, instructions)
			}

			continue
		}

		g.generateStatement(block.data.(*Statement), instructions)
	}
}

func (g *IrGenerator) generateFunction(fnDef *FunctionDef) *IrFunction {
	instructions := []*IrInstruction{}
	g.generateBlock(fnDef.body, &instructions)

	// ensure that the main function has a return statement
	if fnDef.identifier == "main" {
		addRet := false
		if len(fnDef.body.items) == 0 {
			addRet = true
		}

		if len(fnDef.body.items) > 0 && fnDef.body.items[len(fnDef.body.items)-1].kind != BLOCK_KIND_STMT {
			addRet = true
		}

		if len(fnDef.body.items) > 0 && fnDef.body.items[len(fnDef.body.items)-1].data.(*Statement).kind != STMT_RETURN {
			addRet = true
		}

		if addRet {
			g.generateStatement(&Statement{
				kind: STMT_RETURN,
				data: &ReturnStatement{
					expr: &Expression{
						kind: EXP_INTEGER,
						data: int64(0),
					},
				},
			}, &instructions)
		}
	}

	return NewIrFunction(fnDef.identifier, instructions)
}

func (g *IrGenerator) generateIfStatement(ifst *IfStatement, instructions *[]*IrInstruction) {
	c := g.generateExpression(ifst.cond, instructions)
	if ifst.otherwise == nil {
		endLabel := g.newLabel()
		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_JUMP_IF_ZERO,
			data: &IrJumpIfZero{
				condition: c,
				target:    endLabel,
			},
		})

		g.generateStatement(ifst.then, instructions)
		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_LABEL,
			data: endLabel,
		})
	} else {
		endLabel := g.newLabel()
		elseLabel := g.newLabel()

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_JUMP_IF_ZERO,
			data: &IrJumpIfZero{
				condition: c,
				target:    elseLabel,
			},
		})

		g.generateStatement(ifst.then, instructions)
		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_JUMP,
			data: endLabel,
		}, &IrInstruction{
			kind: IR_INSTRUCTION_LABEL,
			data: elseLabel,
		})

		g.generateStatement(ifst.otherwise, instructions)
		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_LABEL,
			data: endLabel,
		})
	}
}

func (g *IrGenerator) generateStatement(stmt *Statement, instructions *[]*IrInstruction) {
	switch stmt.kind {
	case STMT_RETURN:
		val := g.generateExpression(stmt.data.(*ReturnStatement).expr, instructions)
		*instructions = append(*instructions, NewIrReturnInstruction(val))
	case STMT_EXPR:
		g.generateExpression(stmt.data.(*Expression), instructions)
	case STMT_IF:
		g.generateIfStatement(stmt.data.(*IfStatement), instructions)
	case STMT_COMPOUND:
		g.generateBlock(stmt.data.(*Compound).block, instructions)
	}
}

func (g *IrGenerator) generateUnary(unaryExpr *UnaryExpr, instructions *[]*IrInstruction) *IrVal {
	innerVal := g.generateExpression(unaryExpr.expr, instructions)
	resultVar := g.newTempVar()

	var irOp IrOperator
	switch unaryExpr.operator {
	case MINUS:
		irOp = IR_UNARY_NEGATE
	case TILDE:
		irOp = IR_UNARY_COMPLEMENT
	case TOK_BANG:
		irOp = IR_UNARY_NOT
	default:
		panic("unsupported unary operator")
	}

	*instructions = append(*instructions, NewIrUnaryInstruction(irOp, innerVal, resultVar))
	return resultVar
}

func (g *IrGenerator) generateLogicalBinaryExpr(
	binExpr *BinaryExpr,
	instructions *[]*IrInstruction,
) *IrVal {
	falseLabel := g.newLabel()
	endLabel := g.newLabel()
	resultVar := g.newTempVar()

	if binExpr.operator == TOK_AND {
		v1 := g.generateExpression(binExpr.lhs, instructions)

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_JUMP_IF_ZERO,
			data: &IrJumpIfZero{
				condition: v1,
				target:    falseLabel,
			},
		})

		v2 := g.generateExpression(binExpr.rhs, instructions)

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_JUMP_IF_ZERO,
			data: &IrJumpIfZero{
				condition: v2,
				target:    falseLabel,
			},
		})

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_COPY,
			data: &IrCopyInstruction{
				src: NewIrConstant(1),
				dst: resultVar,
			},
		})

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_JUMP,
			data: endLabel,
		})

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_LABEL,
			data: falseLabel,
		})

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_COPY,
			data: &IrCopyInstruction{
				src: NewIrConstant(0),
				dst: resultVar,
			},
		})

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_LABEL,
			data: endLabel,
		})
	} else if binExpr.operator == TOK_OR {
		trueLabel := g.newLabel()
		v1 := g.generateExpression(binExpr.lhs, instructions)

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_JUMP_IF_NOT_ZERO,
			data: &IrJumpIfNotZero{
				condition: v1,
				target:    trueLabel,
			},
		})
		v2 := g.generateExpression(binExpr.rhs, instructions)

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_JUMP_IF_ZERO,
			data: &IrJumpIfZero{
				condition: v2,
				target:    falseLabel,
			},
		})

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_JUMP,
			data: trueLabel,
		})

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_LABEL,
			data: falseLabel,
		})

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_COPY,
			data: &IrCopyInstruction{
				src: NewIrConstant(0),
				dst: resultVar,
			},
		})

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_JUMP,
			data: endLabel,
		})

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_LABEL,
			data: trueLabel,
		})

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_COPY,
			data: &IrCopyInstruction{
				src: NewIrConstant(1),
				dst: resultVar,
			},
		})

		*instructions = append(*instructions, &IrInstruction{
			kind: IR_INSTRUCTION_LABEL,
			data: endLabel,
		})
	}

	return resultVar
}

func (g *IrGenerator) generateConditionalExpr(expr *CondExpr, instructions *[]*IrInstruction) *IrVal {
	cond := g.generateExpression(expr.left, instructions)
	e2Label := g.newLabel()
	endLabel := g.newLabel()

	*instructions = append(*instructions, &IrInstruction{
		kind: IR_INSTRUCTION_JUMP_IF_ZERO,
		data: &IrJumpIfZero{
			target:    e2Label,
			condition: cond,
		},
	})

	v1 := g.generateExpression(expr.middle, instructions)

	res := g.newTempVar()
	*instructions = append(*instructions, &IrInstruction{
		kind: IR_INSTRUCTION_COPY,
		data: &IrCopyInstruction{
			src: v1,
			dst: res,
		},
	}, &IrInstruction{
		kind: IR_INSTRUCTION_JUMP,
		data: endLabel,
	}, &IrInstruction{
		kind: IR_INSTRUCTION_LABEL,
		data: e2Label,
	})

	v2 := g.generateExpression(expr.right, instructions)
	*instructions = append(*instructions, &IrInstruction{
		kind: IR_INSTRUCTION_COPY,
		data: &IrCopyInstruction{
			src: v2,
			dst: res,
		},
	}, &IrInstruction{
		kind: IR_INSTRUCTION_LABEL,
		data: endLabel,
	})

	return res
}

func (g *IrGenerator) generateBinaryExpr(expr *BinaryExpr, instructions *[]*IrInstruction) *IrVal {
	if expr.operator == TOK_AND || expr.operator == TOK_OR {
		return g.generateLogicalBinaryExpr(expr, instructions)
	}

	v1 := g.generateExpression(expr.lhs, instructions)
	v2 := g.generateExpression(expr.rhs, instructions)

	dst := g.newTempVar()
	var irOp IrOperator
	switch expr.operator {
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
	case TOK_GT:
		irOp = IR_BIN_GT
	case TOK_LT:
		irOp = IR_BIN_LT
	case TOK_GTEQ:
		irOp = IR_BIN_GTEQ
	case TOK_LTEQ:
		irOp = IR_BIN_LTEQ
	case TOK_EQ:
		irOp = IR_BIN_EQ
	case TOK_NEQ:
		irOp = IR_BIN_NEQ
	default:
		panic("unsupported binary operator")
	}

	*instructions = append(*instructions, &IrInstruction{
		kind: IR_INSTRUCTION_BINARY,
		data: &IrBinaryInstruction{
			operator: irOp,
			src1:     v1,
			src2:     v2,
			dst:      dst,
		},
	})

	return dst
}

func (g *IrGenerator) generateAssign(assign *AssignExpr, instructions *[]*IrInstruction) *IrVal {
	if assign.lvalue.kind != EXP_VAR {
		panic("assignment operations left hand side needs to be a variable")
	}

	res := g.generateExpression(assign.avalue, instructions)

	irVar := NewIrVar(assign.lvalue.data.(string))
	*instructions = append(*instructions, &IrInstruction{
		kind: IR_INSTRUCTION_COPY,
		data: &IrCopyInstruction{
			src: res,
			dst: irVar,
		},
	})

	return irVar
}

func (g *IrGenerator) generateExpression(expr *Expression, instructions *[]*IrInstruction) *IrVal {
	fmt.Println("generating expression of kind", expr.kind)
	switch expr.kind {
	case EXP_INTEGER:
		return NewIrConstant(expr.data.(int64))
	case EXP_UNARY:
		return g.generateUnary(expr.data.(*UnaryExpr), instructions)
	case EXP_BINARY:
		return g.generateBinaryExpr(expr.data.(*BinaryExpr), instructions)
	case EXP_ASSIGN:
		return g.generateAssign(expr.data.(*AssignExpr), instructions)
	case EXP_VAR:
		return NewIrVar(expr.data.(string))
	case EXP_COND:
		return g.generateConditionalExpr(expr.data.(*CondExpr), instructions)
	default:
		panic("unsupported expression kind in IR generator")
	}
}

func (g *IrGenerator) newTempVar() *IrVal {
	varName := fmt.Sprintf("$t%d", g.tempVarCounter)
	g.tempVarCounter++
	return NewIrVar(varName)
}

func (g *IrGenerator) newLabel() string {
	labelName := fmt.Sprintf("%d", g.labelCount)
	g.labelCount++

	return labelName
}
