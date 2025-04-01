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
	EXP_ASSIGN
	EXP_VAR
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

type AssignExpr struct {
	lvalue *Expression
	avalue *Expression
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
	TOK_ASSIGN:   1,
}

// StatementKind represents different types of statements
type StatementKind int

const (
	STMT_RETURN StatementKind = iota
	STMT_IF
	STMT_EXPR
	STMT_NULL
)

type BlockKind int

const (
	BLOCK_KIND_STMT BlockKind = iota
	BLOCK_KIND_DECL
)

type ReturnStatement struct {
	expr *Expression
}

// Statement represents a C expression
type Statement struct {
	kind StatementKind
	data any
}

type Declaration struct {
	identifier string
	init       *Expression
}

type Block struct {
	kind BlockKind
	data any
}

type FunctionDef struct {
	identifier string
	body       []Block
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
	if p.tokens[p.idx].Kind == RETURN_KEYWORD {
		p.expect(RETURN_KEYWORD)
		expr := p.parseExpr(0)
		p.expect(SEMICOLON)

		return &Statement{
			kind: STMT_RETURN,
			data: &ReturnStatement{
				expr: expr,
			},
		}
	} else if p.tokens[p.idx].Kind == SEMICOLON {
		return &Statement{
			kind: STMT_NULL,
		}
	}

	expr := p.parseExpr(0)
	p.expect(SEMICOLON)
	return &Statement{
		kind: STMT_EXPR,
		data: expr,
	}
}

func (p *Parser) parseFactor() *Expression {
	tok := p.tokens[p.idx]
	p.idx += 1

	switch tok.Kind {
	case TOK_IDENT:
		return &Expression{
			kind: EXP_VAR,
			data: tok.Value.(string),
		}
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
		if p.idx >= len(p.tokens) {
			break
		}

		next := p.tokens[p.idx].Kind
		pred, ok := precedences[next]
		if !ok || pred < minPrec {
			break
		}

		if next == TOK_ASSIGN {
			p.idx++ // skip the assign symbol
			right := p.parseExpr(pred)
			left = &Expression{
				kind: EXP_ASSIGN,
				data: &AssignExpr{
					lvalue: left,
					avalue: right,
				},
			}
		} else {
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
	}

	return left
}

func (p *Parser) parseDecl() *Declaration {
	p.expect(INT_KEYWORD)
	ident := p.tokens[p.idx].Value.(string)
	p.idx += 1
	var expr *Expression

	if p.tokens[p.idx].Kind == TOK_ASSIGN {
		p.idx += 1
		expr = p.parseExpr(0)
		p.expect(SEMICOLON)
	} else {
		p.expect(SEMICOLON)
	}

	return &Declaration{
		identifier: ident,
		init:       expr,
	}
}

func (p *Parser) parseFunctionDef() *FunctionDef {
	p.expect(INT_KEYWORD)

	identifier := p.tokens[p.idx].Value.(string)
	p.idx += 1

	p.expect(OPEN_PAREN)
	p.expect(VOID_KEYWORD)
	p.expect(CLOSE_PAREN)
	p.expect(OPEN_BRACE)

	body := make([]Block, 0)
	for {
		currTok := p.tokens[p.idx].Kind
		if currTok == CLOSE_BRACE {
			break
		}

		if currTok == INT_KEYWORD {
			decl := p.parseDecl()
			body = append(body, Block{
				kind: BLOCK_KIND_DECL,
				data: decl,
			})
		} else {
			stmt := p.parseStatement()
			body = append(body, Block{
				kind: BLOCK_KIND_STMT,
				data: stmt,
			})
		}
	}
	p.expect(CLOSE_BRACE)

	return &FunctionDef{identifier: identifier, body: body}
}

func (p *Parser) Parse() *Program {
	fnDef := p.parseFunctionDef()

	return &Program{
		mainFunction: fnDef,
	}
}
