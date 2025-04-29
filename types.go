package mc

type TypeKind int

const (
	TY_INT TypeKind = iota
	TY_FUNC
)

type FuncType struct {
	paramCount int
}

type Type struct {
	kind  TypeKind
	extra any
}

type Symbol struct {
	ty        Type
	defined   bool
	stackSize int
}

func newType(kind TypeKind) Type {
	return Type{kind: kind}
}

func checkBlockTypes(block *Block, symbols map[string]Symbol) {
	for _, block := range block.items {
		switch bt := block.data.(type) {
		case *Declaration:
			checkDeclTypes(bt, symbols)
		case *Statement:
			checkStmtTypes(bt, symbols)
		}
	}
}

func checkFuncDeclTypes(fndef *FunctionDef, symbols map[string]Symbol) {
	fnType := FuncType{paramCount: len(fndef.params)}
	hasBody := fndef.body != nil

	alreadyDefined := false
	if old, ok := symbols[fndef.identifier]; ok {
		if old.ty.kind != TY_FUNC {
			panic("incompatible function declarations")
		}

		alreadyDefined = old.defined
		if alreadyDefined && hasBody {
			panic("function is defined more than once")
		}
	}

	symbols[fndef.identifier] = Symbol{
		ty: Type{
			kind:  TY_FUNC,
			extra: fnType,
		},
		defined:   alreadyDefined || hasBody,
		stackSize: len(fndef.params) * 4,
	}
	// NOTE: remember stack size

	if hasBody {
		for _, p := range fndef.params {
			symbols[p] = Symbol{
				ty: newType(TY_INT),
			}
		}
		checkBlockTypes(fndef.body, symbols)
	}
}

func checkStmtTypes(stmt *Statement, symbols map[string]Symbol) {
	switch s := stmt.data.(type) {
	case *ReturnStatement:
		checkExprTypes(s.expr, symbols)
	case *Expression:
		checkExprTypes(s, symbols)
	case *IfStatement:
		checkExprTypes(s.cond, symbols)
		checkStmtTypes(s.then, symbols)
		if s.otherwise != nil {
			checkStmtTypes(s.otherwise, symbols)
		}
	case *Compound:
		checkBlockTypes(s.block, symbols)
	case *DoWhileStatement:
		checkExprTypes(s.cond, symbols)
		checkStmtTypes(s.body, symbols)
	case *WhileStatement:
		checkExprTypes(s.cond, symbols)
		checkStmtTypes(s.body, symbols)
	case *ForStatement:
		if s.init != nil {
			switch v := s.init.(type) {
			case *Expression:
				checkExprTypes(v, symbols)
			case *Declaration:
				checkDeclTypes(v, symbols)
			}
		}

		if s.cond != nil {
			checkExprTypes(s.cond, symbols)
		}
		if s.post != nil {
			checkExprTypes(s.post, symbols)
		}

		checkStmtTypes(s.body, symbols)
	}
}

func checkExprTypes(expr *Expression, symbols map[string]Symbol) {
	switch e := expr.data.(type) {
	case *FunctionCall:
		ftype := symbols[e.ident].ty
		if ftype.kind == TY_INT {
			panic("variable used as function name")
		}

		ft := ftype.extra.(FuncType)
		if ft.paramCount != len(e.args) {
			panic("function called with the wrong number of arguments")
		}

		for _, arg := range e.args {
			checkExprTypes(arg, symbols)
		}
	case string:
		if s, ok := symbols[e]; ok && s.ty.kind != TY_INT {
			panic("function name used as a variable")
		}
	case *AssignExpr:
		checkExprTypes(e.lvalue, symbols)
		checkExprTypes(e.avalue, symbols)
	case *UnaryExpr:
		checkExprTypes(e.expr, symbols)
	case *CondExpr:
		checkExprTypes(e.left, symbols)
		checkExprTypes(e.middle, symbols)
		checkExprTypes(e.right, symbols)
	}
}

func checkDeclTypes(decl *Declaration, symbols map[string]Symbol) {
	switch st := decl.data.(type) {
	case *VarDecl:
		checkVariableDeclTypes(st, symbols)
	case *FunctionDef:
		checkFuncDeclTypes(st, symbols)
	}
}

func checkVariableDeclTypes(decl *VarDecl, symbols map[string]Symbol) {
	symbols[decl.identifier] = Symbol{
		ty: newType(TY_INT),
	}

	if decl.init != nil {
		checkExprTypes(decl.init, symbols)
	}
}

func checkProgramTypes(program *Program) map[string]Symbol {
	symbols := make(map[string]Symbol)

	for _, fn := range program.funcs {
		checkFuncDeclTypes(fn, symbols)
	}

	return symbols
}
