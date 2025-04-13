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
	ty      Type
	defined bool
}

func newType(kind TypeKind) Type {
	return Type{kind: kind}
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
		defined: alreadyDefined || hasBody,
	}

	if hasBody {
		for _, p := range fndef.params {
			symbols[p] = Symbol{
				ty: newType(TY_INT),
			}
		}
	}
}
