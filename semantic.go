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
	hasLinkage       bool
}

type Identifiers map[string]SymbolInfo

func (s *SemanticAnalyzer) newUniqueName() string {
	unique := fmt.Sprintf("U%d", s.uniqueVarCount)
	s.uniqueVarCount += 1

	return unique
}

func copyidentifiers(identifiers Identifiers) Identifiers {
	newidentifiers := make(Identifiers, len(identifiers))
	for name, info := range identifiers {
		info.fromCurrentScope = false
		newidentifiers[name] = info
	}
	return newidentifiers
}

func (s *SemanticAnalyzer) resolveDecl(decl *Declaration, identifiers Identifiers) {
	switch d := decl.data.(type) {
	case *VarDecl:
		// prevent identifiers what have the same name and that are from the same scope
		if v, ok := identifiers[d.identifier]; ok && v.fromCurrentScope {
			panic("duplicate veriable declaration")
		}

		uniqueName := s.newUniqueName()
		identifiers[d.identifier] = SymbolInfo{
			name:             uniqueName,
			fromCurrentScope: true,
		}

		if d.init != nil {
			s.resolveExpr(d.init, identifiers)
		}
		d.identifier = uniqueName
	case *FunctionDef:
		s.resolveFuncDecl(d, identifiers)
	}
}

func (s *SemanticAnalyzer) resolveExpr(expr *Expression, identifiers Identifiers) {
	switch e := expr.data.(type) {
	case *AssignExpr:
		if _, ok := e.lvalue.data.(string); !ok {
			panic("invalid lvalue")
		}

		s.resolveExpr(e.lvalue, identifiers)
		s.resolveExpr(e.avalue, identifiers)
	case *CondExpr:
		s.resolveExpr(e.left, identifiers)
		s.resolveExpr(e.middle, identifiers)
		s.resolveExpr(e.right, identifiers)
	case string:
		if si, ok := identifiers[e]; ok {
			expr.data = si.name
		} else {
			panic("undeclared variable")
		}
	case *BinaryExpr:
		s.resolveExpr(e.lhs, identifiers)
		s.resolveExpr(e.rhs, identifiers)
	case *UnaryExpr:
		s.resolveExpr(e.expr, identifiers)
	}
}

func (s *SemanticAnalyzer) resolveStatement(stmt *Statement, identifiers Identifiers) {
	switch st := stmt.data.(type) {
	case *ReturnStatement:
		s.resolveExpr(st.expr, identifiers)
	case *Expression:
		s.resolveExpr(st, identifiers)
	case *IfStatement:
		s.resolveExpr(st.cond, identifiers)
		s.resolveStatement(st.then, identifiers)

		if st.otherwise != nil {
			s.resolveStatement(st.otherwise, identifiers)
		}
	case *FunctionCall:
		if fn, ok := identifiers[st.ident]; ok {
			st.ident = fn.name
			for _, arg := range st.args {
				s.resolveExpr(arg, identifiers)
			}
		} else {
			panic("call to undeclared function")
		}
	case *Compound:
		newidentifiers := copyidentifiers(identifiers)
		s.resolveBlock(st.block, newidentifiers)
	case *DoWhileStatement:
		s.resolveExpr(st.cond, identifiers)
		s.resolveStatement(st.body, identifiers)
	case *WhileStatement:
		s.resolveExpr(st.cond, identifiers)
		s.resolveStatement(st.body, identifiers)
	case *ForStatement:
		newidentifiers := copyidentifiers(identifiers)
		if st.init != nil {
			switch v := st.init.(type) {
			case *Expression:
				s.resolveExpr(v, newidentifiers)
			case *Declaration:
				s.resolveDecl(v, newidentifiers)
			}
		}

		if st.cond != nil {
			s.resolveExpr(st.cond, newidentifiers)
		}
		if st.post != nil {
			s.resolveExpr(st.post, newidentifiers)
		}

		s.resolveStatement(st.body, newidentifiers)
	}
}

func (s *SemanticAnalyzer) resolveBlock(block *Block, identifiers Identifiers) {
	for _, item := range block.items {
		switch bt := item.data.(type) {
		case *Statement:
			s.resolveStatement(bt, identifiers)
			s.labelLoops(bt, "")
		case *Declaration:
			s.resolveDecl(bt, identifiers)
		}
	}
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

func (s *SemanticAnalyzer) resolveParam(param string, identifiers Identifiers) string {
	if v, ok := identifiers[param]; ok && v.fromCurrentScope {
		panic("parameter already defined")
	}

	uniqueName := s.newUniqueName()
	identifiers[param] = SymbolInfo{
		name:             uniqueName,
		fromCurrentScope: true,
		hasLinkage:       false,
	}

	return uniqueName
}

func (s *SemanticAnalyzer) resolveFuncDecl(fndef *FunctionDef, identifiers Identifiers) {
	if info, ok := identifiers[fndef.identifier]; ok {
		if info.fromCurrentScope && !info.hasLinkage {
			panic("duplicate declaration")
		}
	}

	identifiers[fndef.identifier] = SymbolInfo{
		name:             fndef.identifier,
		fromCurrentScope: true,
		hasLinkage:       true,
	}

	innerMap := copyidentifiers(identifiers)
	if len(fndef.params) > 0 {
		newParams := make([]string, len(fndef.params))
		for i, p := range fndef.params {
			newParams[i] = s.resolveParam(p, innerMap)
		}

		fndef.params = newParams
	}

	if fndef.body != nil {
		s.resolveBlock(fndef.body, innerMap)
	}
}

func AnalyzeSemantic(prog *Program) {
	s := SemanticAnalyzer{}
	identifiers := make(map[string]SymbolInfo)
	for _, fn := range prog.funcs {
		s.resolveFuncDecl(fn, identifiers)
	}

	checkProgramTypes(prog)
}
