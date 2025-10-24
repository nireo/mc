package mc

import "fmt"

type IrOperator int

const (
	IrOpUnaryComplement IrOperator = iota
	IrOpUnaryNegate
	IrOpBinAdd
	IrOpBinSub
	IrOpBinMul
	IrOpBinDiv
	IrOpBinRemainder
	IrOpUnaryNot
	IrOpBinGT
	IrOpBinLT
	IrOpBinGTEQ
	IrOpBinLTEQ
	IrOpBinNeq
	IrOpBinEQ
)

type IrValueKind int

const (
	IrValConstant IrValueKind = iota
	IrValVar
)

type IrVal struct {
	kind       IrValueKind
	constant   int64  // if IrValConstant
	identifier string // if IrValVar
}

func NewIrConstant(value int64) *IrVal {
	return &IrVal{
		kind:     IrValConstant,
		constant: value,
	}
}

func NewIrVar(id string) *IrVal {
	return &IrVal{
		kind:       IrValVar,
		identifier: id,
	}
}

func (v *IrVal) String() string {
	switch v.kind {
	case IrValConstant:
		return fmt.Sprintf("%d", v.constant)
	case IrValVar:
		return v.identifier
	default:
		return "unknown"
	}
}

func (v *IrVal) IsVar() bool {
	return v.kind == IrValVar
}

func (v *IrVal) IsConstant() bool {
	return v.kind == IrValConstant
}

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

type IrLabel struct {
	label string
}

type IrJump struct {
	target string
}

type IrJumpIfZero struct {
	condition *IrVal
	target    string // label
}

type IrJumpIfNotZero struct {
	condition *IrVal
	target    string
}

type IrFuncCall struct {
	identifier string
	args       []*IrVal
	dst        *IrVal
}

type IrInstruction struct {
	data any
}

func NewIrReturnInstruction(val *IrVal) *IrInstruction {
	return &IrInstruction{
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
	params     []string
}

func NewIrFunction(identifier string, body []*IrInstruction, params []string) *IrFunction {
	return &IrFunction{
		identifier: identifier,
		body:       body,
		params:     params,
	}
}

type IrProgram struct {
	functions []*IrFunction
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
	funcs := make([]*IrFunction, 0, len(program.funcs))
	for _, fn := range program.funcs {
		if fn.body != nil {
			genFn := g.generateFunction(fn)
			funcs = append(funcs, genFn)
		}
	}

	return &IrProgram{functions: funcs}
}

func (g *IrGenerator) generateBlock(block *Block, instructions *[]*IrInstruction) {
	for _, block := range block.items {
		switch bt := block.data.(type) {
		case *Declaration:
			g.generateDecl(bt, instructions)
		case *Statement:
			g.generateStatement(bt, instructions)
		}
	}
}

func (g *IrGenerator) generateDecl(decl *Declaration, instructions *[]*IrInstruction) {
	// we only need to generate something if the declaration contains a initializer
	vd, ok := decl.data.(*VarDecl)
	if !ok {
		return // do nothing
	}

	if vd.init != nil {
		g.generateExpression(&Expression{
			data: &AssignExpr{
				lvalue: &Expression{
					data: vd.identifier,
				},
				avalue: vd.init,
			},
		}, instructions)
	}
}

func (g *IrGenerator) generateFunction(fnDef *FunctionDef) *IrFunction {
	instructions := []*IrInstruction{}
	g.generateBlock(fnDef.body, &instructions)

	g.generateStatement(&Statement{
		data: &ReturnStatement{
			expr: &Expression{
				data: int64(0),
			},
		},
	}, &instructions)

	return NewIrFunction(fnDef.identifier, instructions, fnDef.params)
}

func (g *IrGenerator) generateIfStatement(ifst *IfStatement, instructions *[]*IrInstruction) {
	c := g.generateExpression(ifst.cond, instructions)
	if ifst.otherwise == nil {
		endLabel := g.newLabel()
		g.jumpIfZero(endLabel, c, instructions)
		g.generateStatement(ifst.then, instructions)
		g.addLabel(endLabel, instructions)
	} else {
		endLabel := g.newLabel()
		elseLabel := g.newLabel()

		g.jumpIfZero(elseLabel, c, instructions)

		g.generateStatement(ifst.then, instructions)
		g.addJump(endLabel, instructions)

		g.addLabel(elseLabel, instructions)

		g.generateStatement(ifst.otherwise, instructions)
		g.addLabel(endLabel, instructions)
	}
}

func (g *IrGenerator) generateDoWhile(doWhile *DoWhileStatement, instructions *[]*IrInstruction) {
	startLabel := g.newLabel()
	g.addLabel(startLabel, instructions)

	breakLabel := "break" + doWhile.identifier
	continueLabel := "continue" + doWhile.identifier

	g.generateStatement(doWhile.body, instructions)
	g.addLabel(continueLabel, instructions)
	v1 := g.generateExpression(doWhile.cond, instructions)

	*instructions = append(*instructions, &IrInstruction{
		data: &IrJumpIfNotZero{
			target:    startLabel,
			condition: v1,
		},
	})
	g.addLabel(breakLabel, instructions)
}

func (g *IrGenerator) generateWhile(whileStmt *WhileStatement, instructions *[]*IrInstruction) {
	breakLabel := "break" + whileStmt.identifier
	continueLabel := "continue" + whileStmt.identifier
	g.addLabel(continueLabel, instructions)
	v1 := g.generateExpression(whileStmt.cond, instructions)

	g.jumpIfZero(breakLabel, v1, instructions)
	g.generateStatement(whileStmt.body, instructions)

	g.addJump(continueLabel, instructions)
	g.addLabel(breakLabel, instructions)
}

func (g *IrGenerator) generateFor(forStmt *ForStatement, instructions *[]*IrInstruction) {
	if forStmt.init != nil {
		switch v := forStmt.init.(type) {
		case *Expression:
			g.generateExpression(v, instructions)
		case *Declaration:
			g.generateDecl(v, instructions)
		}
	}
	labelStart := g.newLabel()
	breakLabel := "break" + forStmt.identifier
	continueLabel := "continue" + forStmt.identifier
	g.addLabel(labelStart, instructions)

	if forStmt.cond != nil {
		v1 := g.generateExpression(forStmt.cond, instructions)
		g.jumpIfZero(breakLabel, v1, instructions)
	} else {
		// c standard says that this expression is replaced by a nonzero constant
		g.jumpIfZero(breakLabel, NewIrConstant(1), instructions)
	}

	g.generateStatement(forStmt.body, instructions)
	g.addLabel(continueLabel, instructions)

	if forStmt.post != nil {
		g.generateExpression(forStmt.post, instructions)
	}

	g.addJump(labelStart, instructions)
	g.addLabel(breakLabel, instructions)
}

func (g *IrGenerator) generateStatement(stmt *Statement, instructions *[]*IrInstruction) {
	switch s := stmt.data.(type) {
	case *ReturnStatement:
		val := g.generateExpression(s.expr, instructions)
		*instructions = append(*instructions, NewIrReturnInstruction(val))
	case *Expression:
		g.generateExpression(s, instructions)
	case *IfStatement:
		g.generateIfStatement(s, instructions)
	case *Compound:
		g.generateBlock(s.block, instructions)
	case *Continue:
		g.addJump("continue"+s.identifier, instructions)
	case *Break:
		g.addJump("break"+s.identifier, instructions)
	case *DoWhileStatement:
		g.generateDoWhile(s, instructions)
	case *WhileStatement:
		g.generateWhile(s, instructions)
	case *ForStatement:
		g.generateFor(s, instructions)
	}
}

func (g *IrGenerator) generateUnary(unaryExpr *UnaryExpr, instructions *[]*IrInstruction) *IrVal {
	innerVal := g.generateExpression(unaryExpr.expr, instructions)
	resultVar := g.newTempVar()

	var irOp IrOperator
	switch unaryExpr.operator {
	case MINUS:
		irOp = IrOpUnaryNegate
	case TILDE:
		irOp = IrOpUnaryComplement
	case TOK_BANG:
		irOp = IrOpUnaryNot
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
		g.jumpIfZero(falseLabel, v1, instructions)

		v2 := g.generateExpression(binExpr.rhs, instructions)
		g.jumpIfZero(falseLabel, v2, instructions)

		g.addCopy(NewIrConstant(1), resultVar, instructions)
		g.addJump(endLabel, instructions)

		g.addLabel(falseLabel, instructions)
		g.addCopy(NewIrConstant(0), resultVar, instructions)
		g.addLabel(endLabel, instructions)
	} else if binExpr.operator == TOK_OR {
		trueLabel := g.newLabel()
		v1 := g.generateExpression(binExpr.lhs, instructions)

		*instructions = append(*instructions, &IrInstruction{
			data: &IrJumpIfNotZero{
				condition: v1,
				target:    trueLabel,
			},
		})
		v2 := g.generateExpression(binExpr.rhs, instructions)
		g.jumpIfZero(falseLabel, v2, instructions)

		g.addJump(trueLabel, instructions)
		g.addLabel(falseLabel, instructions)
		g.addCopy(NewIrConstant(0), resultVar, instructions)

		g.addJump(endLabel, instructions)
		g.addLabel(trueLabel, instructions)
		g.addCopy(NewIrConstant(1), resultVar, instructions)

		g.addLabel(endLabel, instructions)
	}

	return resultVar
}

func (g *IrGenerator) generateConditionalExpr(
	expr *CondExpr,
	instructions *[]*IrInstruction,
) *IrVal {
	cond := g.generateExpression(expr.left, instructions)
	e2Label := g.newLabel()
	endLabel := g.newLabel()

	g.jumpIfZero(e2Label, cond, instructions)
	v1 := g.generateExpression(expr.middle, instructions)

	res := g.newTempVar()
	g.addCopy(v1, res, instructions)
	g.addJump(endLabel, instructions)
	g.addLabel(e2Label, instructions)

	v2 := g.generateExpression(expr.right, instructions)
	g.addCopy(v2, res, instructions)
	g.addLabel(endLabel, instructions)

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
		irOp = IrOpBinSub
	case TOK_ASTERISK:
		irOp = IrOpBinMul
	case TOK_PLUS:
		irOp = IrOpBinAdd
	case TOK_SLASH:
		irOp = IrOpBinDiv
	case TOK_PERCENT:
		irOp = IrOpBinRemainder
	case TOK_GT:
		irOp = IrOpBinGT
	case TOK_LT:
		irOp = IrOpBinLT
	case TOK_GTEQ:
		irOp = IrOpBinGTEQ
	case TOK_LTEQ:
		irOp = IrOpBinLTEQ
	case TOK_EQ:
		irOp = IrOpBinEQ
	case TOK_NEQ:
		irOp = IrOpBinNeq
	default:
		panic("unsupported binary operator")
	}

	*instructions = append(*instructions, &IrInstruction{
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
	lval, ok := assign.lvalue.data.(string)
	if !ok {
		panic("assignment operations left hand side needs to be a variable")
	}
	res := g.generateExpression(assign.avalue, instructions)

	irVar := NewIrVar(lval)
	g.addCopy(res, irVar, instructions)

	return irVar
}

func (g *IrGenerator) generateFunCall(fncall *FunctionCall, instructions *[]*IrInstruction) *IrVal {
	args := make([]*IrVal, 0, len(fncall.args))
	for _, argExpr := range fncall.args {
		argVal := g.generateExpression(argExpr, instructions)
		args = append(args, argVal)
	}

	resVar := g.newTempVar()
	*instructions = append(*instructions, &IrInstruction{
		data: &IrFuncCall{
			identifier: fncall.ident,
			args:       args,
			dst:        resVar,
		},
	})

	return resVar
}

func (g *IrGenerator) generateExpression(expr *Expression, instructions *[]*IrInstruction) *IrVal {
	switch e := expr.data.(type) {
	case int64:
		return NewIrConstant(e)
	case *UnaryExpr:
		return g.generateUnary(e, instructions)
	case *BinaryExpr:
		return g.generateBinaryExpr(e, instructions)
	case *AssignExpr:
		return g.generateAssign(e, instructions)
	case string:
		return NewIrVar(e)
	case *CondExpr:
		return g.generateConditionalExpr(e, instructions)
	case *FunctionCall:
		return g.generateFunCall(e, instructions)
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

func (g *IrGenerator) addLabel(label string, instructions *[]*IrInstruction) {
	*instructions = append(*instructions, &IrInstruction{
		data: &IrLabel{
			label: label,
		},
	})
}

func (g *IrGenerator) addJump(target string, instructions *[]*IrInstruction) {
	*instructions = append(*instructions, &IrInstruction{
		data: &IrJump{
			target: target,
		},
	})
}

func (g *IrGenerator) addCopy(src *IrVal, dst *IrVal, instructions *[]*IrInstruction) {
	*instructions = append(*instructions, &IrInstruction{
		data: &IrCopyInstruction{
			src: src,
			dst: dst,
		},
	})
}

func (g *IrGenerator) jumpIfZero(target string, condition *IrVal, instructions *[]*IrInstruction) {
	*instructions = append(*instructions, &IrInstruction{
		data: &IrJumpIfZero{
			target:    target,
			condition: condition,
		},
	})
}
