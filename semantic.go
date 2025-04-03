package mc

import (
	"fmt"
)

// SemanticAnalyzer focuses on finding issues in the code that appear when code is syntastically
// correct but it doesn't work in practice.
//
// For example:
// 2 = a * 3
//
// int a = 3;
// int a;
type SemanticAnalyzer struct {
	uniqueVarCount int
}

func (s *SemanticAnalyzer) newUniqueName() string {
	unique := fmt.Sprintf("$u.%d", s.uniqueVarCount)
	s.uniqueVarCount += 1

	return unique
}

func (s *SemanticAnalyzer) resolveDecl(decl *Declaration, variables map[string]string) {
	if _, ok := variables[decl.identifier]; ok {
		panic("duplicate veriable declaration")
	}

	uniqueName := s.newUniqueName()
	variables[decl.identifier] = uniqueName

	if decl.init != nil {
		s.resolveExpr(decl.init, variables)
	}

	decl.identifier = uniqueName
}

func (s *SemanticAnalyzer) resolveExpr(expr *Expression, variables map[string]string) {
	switch expr.kind {
	case EXP_ASSIGN:
		assign := expr.data.(*AssignExpr)
		if assign.lvalue.kind != EXP_VAR {
			panic("invalid lvalue")
		}

		s.resolveExpr(assign.lvalue, variables)
		s.resolveExpr(assign.avalue, variables)
	case EXP_COND:
		cond := expr.data.(*CondExpr)
		s.resolveExpr(cond.left, variables)
		s.resolveExpr(cond.middle, variables)
		s.resolveExpr(cond.right, variables)
	case EXP_VAR:
		name := expr.data.(string)
		if uniqueName, ok := variables[name]; ok {
			expr.data = uniqueName
		} else {
			panic("undeclared variable")
		}
	case EXP_BINARY:
		bin := expr.data.(*BinaryExpr)
		s.resolveExpr(bin.lhs, variables)
		s.resolveExpr(bin.rhs, variables)
	case EXP_UNARY:
		unary := expr.data.(*UnaryExpr)
		s.resolveExpr(unary.expr, variables)
	}
}

func (s *SemanticAnalyzer) resolveStatement(stmt *Statement, variables map[string]string) {
	switch stmt.kind {
	case STMT_RETURN:
		ret := stmt.data.(*ReturnStatement)
		s.resolveExpr(ret.expr, variables)
	case STMT_EXPR:
		e := stmt.data.(*Expression)
		s.resolveExpr(e, variables)
	case STMT_IF:
		ifs := stmt.data.(*IfStatement)
		s.resolveExpr(ifs.cond, variables)
		s.resolveStatement(ifs.then, variables)

		if ifs.otherwise != nil {
			s.resolveStatement(ifs.otherwise, variables)
		}
	case STMT_COMPOUND:
		comp := stmt.data.(*Compound)
		s.resolveBlock(comp.block, variables)
	}
}

func (s *SemanticAnalyzer) resolveBlock(block *Block, variables map[string]string) {
	for _, item := range block.items {
		if item.kind == BLOCK_KIND_STMT {
			s.resolveStatement(item.data.(*Statement), variables)
		} else if item.kind == BLOCK_KIND_DECL {
			s.resolveDecl(item.data.(*Declaration), variables)
		}
	}
}

func (s *SemanticAnalyzer) resolveFunctionDef(fnDef *FunctionDef) {
	variables := make(map[string]string)
	s.resolveBlock(fnDef.body, variables)
}

func AnalyzeSemantic(prog *Program) {
	s := SemanticAnalyzer{}
	s.resolveFunctionDef(prog.mainFunction)
}
