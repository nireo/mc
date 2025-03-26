package mc

import "fmt"

// ExpressionKind represents different types of expressions
type ExpressionKind int

const (
	EXP_UNARY ExpressionKind = iota
	EXP_INTEGER
)

type UnayExpr struct {
	operator Token
	expr     *Expression
}

// Expression represents a C expression
type Expression struct {
	kind    ExpressionKind
	integer int64
	unary   *UnayExpr
}

// StatementKind represents different types of statements
type StatementKind int

const (
	STMT_RETURN StatementKind = iota
	STMT_IF
)

type ReturnStatement struct {
	expr *Expression
}

type IfStatement struct {
	cond      *Expression
	then      *Statement
	otherwise *Statement
}

// Statement represents a C expression
type Statement struct {
	kind StatementKind
	ret  *ReturnStatement
	ifs  *IfStatement
}

type FunctionDef struct {
	identifier string
	statement  *Statement
}

type Program struct {
	mainFunction *FunctionDef
}

type Parser struct {
	idx    int
	tokens []TokenValue
}

func NewParser(tokens []TokenValue) *Parser {
	return &Parser{
		idx:    0,
		tokens: tokens,
	}
}

func (p *Parser) expect(expectedToken Token) {
	if p.tokens[p.idx].Kind != expectedToken {
		panic(fmt.Sprintf("expected another token %d but got wrong token %d", p.tokens[p.idx].Kind, expectedToken))
	}
	p.idx += 1
}

func (p *Parser) parseStatement() *Statement {
	p.expect(RETURN_KEYWORD)
	expr := p.parseExpr()
	p.expect(SEMICOLON)

	return &Statement{
		kind: STMT_RETURN,
		ret: &ReturnStatement{
			expr: expr,
		},
	}
}

func (p *Parser) parseExpr() *Expression {
	tok := p.tokens[p.idx]
	p.idx += 1

	switch tok.Kind {
	case CONSTANT:
		return &Expression{
			kind:    EXP_INTEGER,
			integer: tok.Value.(int64),
		}
	}

	panic("no implementation for expression")
}

func (p *Parser) parseFunctionDef() *FunctionDef {
	p.expect(INT_KEYWORD)

	identifier := p.tokens[p.idx].Value.(string)
	p.idx += 1

	p.expect(OPEN_PAREN)
	p.expect(VOID_KEYWORD)
	p.expect(CLOSE_PAREN)
	p.expect(OPEN_BRACE)

	stmt := p.parseStatement()

	return &FunctionDef{identifier: identifier, statement: stmt}
}

func (p *Parser) Parse() *Program {
	fnDef := p.parseFunctionDef()

	return &Program{
		mainFunction: fnDef,
	}
}
