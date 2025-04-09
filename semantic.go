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
	switch e := expr.data.(type) {
	case *AssignExpr:
		if _, ok := e.lvalue.data.(string); !ok {
			panic("invalid lvalue")
		}

		s.resolveExpr(e.lvalue, variables)
		s.resolveExpr(e.avalue, variables)
	case *CondExpr:
		cond := expr.data.(*CondExpr)
		s.resolveExpr(cond.left, variables)
		s.resolveExpr(cond.middle, variables)
		s.resolveExpr(cond.right, variables)
	case string:
		if si, ok := variables[e]; ok {
			expr.data = si.name
		} else {
			panic("undeclared variable")
		}
	case *BinaryExpr:
		s.resolveExpr(e.lhs, variables)
		s.resolveExpr(e.rhs, variables)
	case *UnaryExpr:
		s.resolveExpr(e.expr, variables)
	}
}

func (s *SemanticAnalyzer) resolveStatement(stmt *Statement, variables map[string]SymbolInfo) {

	switch st := stmt.data.(type) {
	case *ReturnStatement:
		s.resolveExpr(st.expr, variables)
	case *Expression:
		s.resolveExpr(st, variables)
	case *IfStatement:
		s.resolveExpr(st.cond, variables)
		s.resolveStatement(st.then, variables)

		if st.otherwise != nil {
			s.resolveStatement(st.otherwise, variables)
		}
	case *Compound:
		newVariables := copyVariables(variables)
		s.resolveBlock(st.block, newVariables)
	case *DoWhileStatement:
		s.resolveExpr(st.cond, variables)
		s.resolveStatement(st.body, variables)
	case *WhileStatement:
		s.resolveExpr(st.cond, variables)
		s.resolveStatement(st.body, variables)
	case *ForStatement:
		newVariables := copyVariables(variables)
		if st.init != nil {
			switch v := st.init.(type) {
			case *Expression:
				s.resolveExpr(v, newVariables)
			case *Declaration:
				s.resolveDecl(v, newVariables)
			}
		}

		if st.cond != nil {
			s.resolveExpr(st.cond, newVariables)
		}
		if st.post != nil {
			s.resolveExpr(st.post, newVariables)
		}

		s.resolveStatement(st.body, newVariables)
	}
}

func (s *SemanticAnalyzer) resolveBlock(block *Block, variables map[string]SymbolInfo) {
	for _, item := range block.items {
		switch bt := item.data.(type) {
		case *Statement:
			s.resolveStatement(bt, variables)
			s.labelLoops(bt, "")
		case *Declaration:
			s.resolveDecl(bt, variables)
		}
	}
}

func (s *SemanticAnalyzer) resolveFunctionDef(fnDef *FunctionDef) {
	variables := make(map[string]SymbolInfo)
	s.resolveBlock(fnDef.body, variables)
}

func (s *SemanticAnalyzer) labelLoops(stmt *Statement, currLabel string) {
	switch st := stmt.data.(type) {
	case *Continue:
		if currLabel == "" {
			panic("continue statement outside of loop")
		}
		st.identifier = currLabel
	case *Break:
		if currLabel == "" {
			panic("break statement outside of loop")
		}
		st.identifier = currLabel
	case *DoWhileStatement:
		newLabel := s.newUniqueName()
		s.labelLoops(st.body, newLabel)
		st.identifier = newLabel
	case *WhileStatement:
		newLabel := s.newUniqueName()
		s.labelLoops(st.body, newLabel)
		st.identifier = newLabel
	case *ForStatement:
		newLabel := s.newUniqueName()
		s.labelLoops(st.body, newLabel)
		st.identifier = newLabel
	}
}

func AnalyzeSemantic(prog *Program) {
	s := SemanticAnalyzer{}
	s.resolveFunctionDef(prog.mainFunction)
}
