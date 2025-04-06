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

type SymbolInfo struct {
	name             string
	fromCurrentScope bool
}

func (s *SemanticAnalyzer) newUniqueName() string {
	unique := fmt.Sprintf("U%d", s.uniqueVarCount)
	s.uniqueVarCount += 1

	return unique
}

func copyVariables(variables map[string]SymbolInfo) map[string]SymbolInfo {
	newVariables := make(map[string]SymbolInfo, len(variables))
	for name, info := range variables {
		info.fromCurrentScope = false
		newVariables[name] = info
	}
	return newVariables
}

func (s *SemanticAnalyzer) resolveDecl(decl *Declaration, variables map[string]SymbolInfo) {
	// prevent variables what have the same name and that are from the same scope
	if v, ok := variables[decl.identifier]; ok && v.fromCurrentScope {
		panic("duplicate veriable declaration")
	}

	uniqueName := s.newUniqueName()
	variables[decl.identifier] = SymbolInfo{
		name:             uniqueName,
		fromCurrentScope: true,
	}

	if decl.init != nil {
		s.resolveExpr(decl.init, variables)
	}

	decl.identifier = uniqueName
}

func (s *SemanticAnalyzer) resolveExpr(expr *Expression, variables map[string]SymbolInfo) {
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
		if si, ok := variables[name]; ok {
			expr.data = si.name
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

func (s *SemanticAnalyzer) resolveStatement(stmt *Statement, variables map[string]SymbolInfo) {
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
		newVariables := copyVariables(variables)
		s.resolveBlock(comp.block, newVariables)
	case STMT_WHILE:
		comp := stmt.data.(*WhileStatement)
		s.resolveExpr(comp.cond, variables)
		s.resolveStatement(comp.body, variables)
	case STMT_DOWHILE:
		comp := stmt.data.(*DoWhileStatement)
		s.resolveExpr(comp.cond, variables)
		s.resolveStatement(comp.body, variables)
	case STMT_FOR:
		newVariables := copyVariables(variables)
		forStmt := stmt.data.(*ForStatement)

		if forStmt.init != nil {
			switch v := forStmt.init.(type) {
			case *Expression:
				s.resolveExpr(v, newVariables)
			case *Declaration:
				s.resolveDecl(v, newVariables)
			}
		}

		if forStmt.cond != nil {
			s.resolveExpr(forStmt.cond, newVariables)
		}

		if forStmt.post != nil {
			s.resolveExpr(forStmt.post, newVariables)
		}

		s.resolveStatement(forStmt.body, newVariables)
	}
}

func (s *SemanticAnalyzer) resolveBlock(block *Block, variables map[string]SymbolInfo) {
	for _, item := range block.items {
		if item.kind == BLOCK_KIND_STMT {
			stmt := item.data.(*Statement)
			s.resolveStatement(stmt, variables)
			s.labelLoops(stmt, "")
		} else if item.kind == BLOCK_KIND_DECL {
			s.resolveDecl(item.data.(*Declaration), variables)
		}
	}
}

func (s *SemanticAnalyzer) resolveFunctionDef(fnDef *FunctionDef) {
	variables := make(map[string]SymbolInfo)
	s.resolveBlock(fnDef.body, variables)
}

func (s *SemanticAnalyzer) labelLoops(stmt *Statement, currLabel string) {
	switch stmt.kind {
	case STMT_BREAK:
		if currLabel == "" {
			panic("break statement outside of loop")
		}
		stmt.data.(*Break).identifier = currLabel
	case STMT_CONTINUE:
		if currLabel == "" {
			panic("continue statement outside of loop")
		}
		stmt.data.(*Continue).identifier = currLabel
	case STMT_WHILE:
		whileStmt := stmt.data.(*WhileStatement)
		newLabel := s.newUniqueName()
		s.labelLoops(whileStmt.body, newLabel)
		whileStmt.identifier = newLabel
	case STMT_DOWHILE:
		whileStmt := stmt.data.(*DoWhileStatement)
		newLabel := s.newUniqueName()
		s.labelLoops(whileStmt.body, newLabel)
		whileStmt.identifier = newLabel
	case STMT_FOR:
		forStmt := stmt.data.(*ForStatement)
		newLabel := s.newUniqueName()
		s.labelLoops(forStmt.body, newLabel)
		forStmt.identifier = newLabel
	}
}

func AnalyzeSemantic(prog *Program) {
	s := SemanticAnalyzer{}
	s.resolveFunctionDef(prog.mainFunction)
}
