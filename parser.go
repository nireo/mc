package mc

import (
	"fmt"
)

// ExpressionKind represents different types of expressions
type ExpressionKind int

const (
	EXP_UNARY ExpressionKind = iota
	EXP_INTEGER
	EXP_BINARY
)

type UnaryExpr struct {
	operator Token
	expr     *Expression
}

type BinaryExpr struct {
	operator Token
	lhs      *Expression
	rhs      *Expression
}

// Expression represents a C expression
type Expression struct {
	kind ExpressionKind
	data any
}

var precedences = map[Token]int{
	TOK_ASTERISK: 50,
	TOK_SLASH:    50,
	TOK_PERCENT:  50,
	TOK_PLUS:     45,
	MINUS:        45,
	TOK_GT:       35,
	TOK_LT:       35,
	TOK_GTEQ:     35,
	TOK_LTEQ:     35,
	TOK_EQ:       30,
	TOK_NEQ:      30,
	TOK_AND:      10,
	TOK_OR:       5,
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
	data any
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
	expr := p.parseExpr(0)
	p.expect(SEMICOLON)

	return &Statement{
		kind: STMT_RETURN,
		data: &ReturnStatement{
			expr: expr,
		},
	}
}

func (p *Parser) parseFactor() *Expression {
	tok := p.tokens[p.idx]
	p.idx += 1

	switch tok.Kind {
	case TOK_CONSTANT:
		return &Expression{
			kind: EXP_INTEGER,
			data: tok.Value.(int64),
		}
	case TILDE, MINUS, TOK_BANG:
		op := tok.Kind
		innerExpr := p.parseFactor()
		return &Expression{
			kind: EXP_UNARY,
			data: &UnaryExpr{
				operator: op,
				expr:     innerExpr,
			},
		}
	case OPEN_PAREN:
		innerExpr := p.parseExpr(0)
		p.expect(CLOSE_PAREN)

		return innerExpr
	}

	panic("no implementation for expression")

}

func (p *Parser) parseExpr(minPrec int) *Expression {
	left := p.parseFactor()

	for {
		if p.idx >= len(p.tokens)-1 {
			break
		}

		next := p.tokens[p.idx].Kind
		pred, ok := precedences[next]
		if !ok || pred < minPrec {
			break
		}

		p.idx++

		right := p.parseExpr(pred + 1)
		left = &Expression{
			kind: EXP_BINARY,
			data: &BinaryExpr{
				operator: next,
				lhs:      left,
				rhs:      right,
			},
		}
	}

	return left
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
