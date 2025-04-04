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
	EXP_COND
)

type CondExpr struct {
	left   *Expression
	middle *Expression
	right  *Expression
}

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
	TOK_QUESTION: 3,
	TOK_ASSIGN:   1,
}

// StatementKind represents different types of statements
type StatementKind int

const (
	STMT_RETURN StatementKind = iota
	STMT_IF
	STMT_EXPR
	STMT_NULL
	STMT_COMPOUND
	STMT_CONTINUE
	STMT_BREAK
	STMT_WHILE
	STMT_DOWHILE
	STMT_FOR
)

type BlockKind int

const (
	BLOCK_KIND_STMT BlockKind = iota
	BLOCK_KIND_DECL
)

type ReturnStatement struct {
	expr *Expression
}

type IfStatement struct {
	cond      *Expression
	then      *Statement
	otherwise *Statement
}

type DoWhileStatement struct {
	body       *Statement
	cond       *Expression
	identifier string
}

type WhileStatement struct {
	cond       *Expression
	body       *Statement
	identifier string
}

type ForStatement struct {
	init       any // declaration | expression or nothing
	cond       *Expression
	post       *Expression
	body       *Statement
	identifier string
}

// Statement represents a C expression
type Statement struct {
	kind StatementKind
	data any
}

type Break struct {
	identifier string
}

type Continue struct {
	identifier string
}

type Declaration struct {
	identifier string
	init       *Expression
}

type BlockItem struct {
	kind BlockKind
	data any
}

type Block struct {
	items []BlockItem
}

type FunctionDef struct {
	identifier string
	body       *Block
}

type Compound struct {
	block *Block
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

func (p *Parser) parseIfStatement() *Statement {
	p.expect(TOK_IF)
	p.expect(OPEN_PAREN)
	cond := p.parseExpr(0)
	p.expect(CLOSE_PAREN)
	then := p.parseStatement()

	var otherwise *Statement
	if p.tokens[p.idx].Kind == TOK_ELSE {
		p.idx += 1
		otherwise = p.parseStatement()
	}

	return &Statement{
		kind: STMT_IF,
		data: &IfStatement{
			cond:      cond,
			then:      then,
			otherwise: otherwise,
		},
	}
}

func (p *Parser) parseStatement() *Statement {
	switch p.tokens[p.idx].Kind {
	case RETURN_KEYWORD:
		p.expect(RETURN_KEYWORD)
		expr := p.parseExpr(0)
		p.expect(SEMICOLON)

		return &Statement{
			kind: STMT_RETURN,
			data: &ReturnStatement{
				expr: expr,
			},
		}
	case TOK_IF:
		return p.parseIfStatement()
	case SEMICOLON:
		p.idx += 1
		return &Statement{
			kind: STMT_NULL,
		}
	case OPEN_BRACE:
		p.idx += 1
		block := p.parseBlock()

		return &Statement{
			kind: STMT_COMPOUND,
			data: &Compound{
				block: block,
			},
		}
	case TOK_BREAK:
		p.idx += 1
		p.expect(SEMICOLON)
		return &Statement{
			kind: STMT_BREAK,
			data: &Break{},
		}
	case TOK_CONTINUE:
		p.idx += 1
		p.expect(SEMICOLON)
		return &Statement{
			kind: STMT_CONTINUE,
			data: &Continue{},
		}
	case TOK_DO:
		p.idx += 1
		stmt := p.parseStatement()
		p.expect(TOK_WHILE)
		p.expect(OPEN_PAREN)

		cond := p.parseExpr(0)
		p.expect(CLOSE_PAREN)
		p.expect(SEMICOLON)

		return &Statement{
			kind: STMT_DOWHILE,
			data: &DoWhileStatement{
				cond: cond,
				body: stmt,
			},
		}
	case TOK_WHILE:
		p.idx += 1
		p.expect(OPEN_PAREN)
		cond := p.parseExpr(0)
		p.expect(CLOSE_PAREN)

		body := p.parseStatement()
		return &Statement{
			kind: STMT_WHILE,
			data: &WhileStatement{
				cond: cond,
				body: body,
			},
		}
	case TOK_FOR:
		return p.parseForStatement()
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

	panic(fmt.Sprintf("no implementation for expression %d", tok.Kind))

}

func (p *Parser) parseConditionalMiddle() *Expression {
	p.expect(TOK_QUESTION)
	expr := p.parseExpr(0)
	p.expect(TOK_COLON)

	return expr
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
		} else if next == TOK_QUESTION {
			middle := p.parseConditionalMiddle()
			right := p.parseExpr(pred)
			left = &Expression{
				kind: EXP_COND,
				data: &CondExpr{
					left,
					middle,
					right,
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

func (p *Parser) parseForStatement() *Statement {
	p.expect(TOK_FOR)
	p.expect(OPEN_PAREN)

	var init any
	isDecl := false
	switch p.tokens[p.idx].Kind {
	case INT_KEYWORD:
		init = p.parseDecl()
		isDecl = true
	case SEMICOLON:
		init = nil
	default:
		init = p.parseExpr(0)
	}

	if !isDecl {
		p.expect(SEMICOLON)
	}

	var cond *Expression = nil
	if p.tokens[p.idx].Kind == SEMICOLON {
		p.idx += 1
	} else {
		cond = p.parseExpr(0)
		p.expect(SEMICOLON)
	}

	var post *Expression = nil
	if p.tokens[p.idx].Kind == CLOSE_PAREN {
		p.idx += 1
	} else {
		cond = p.parseExpr(0)
		p.expect(CLOSE_PAREN)
	}

	body := p.parseStatement()

	return &Statement{
		kind: STMT_FOR,
		data: &ForStatement{
			init: init,
			cond: cond,
			post: post,
			body: body,
		},
	}
}

func (p *Parser) parseBlock() *Block {
	body := make([]BlockItem, 0)
	for {
		currTok := p.tokens[p.idx].Kind
		if currTok == CLOSE_BRACE {
			break
		}

		if currTok == INT_KEYWORD {
			decl := p.parseDecl()
			body = append(body, BlockItem{
				kind: BLOCK_KIND_DECL,
				data: decl,
			})
		} else if currTok == OPEN_BRACE {
			p.idx += 1
			block := p.parseBlock()
			body = append(body, BlockItem{
				kind: BLOCK_KIND_STMT,
				data: &Statement{
					kind: STMT_COMPOUND,
					data: &Compound{
						block: block,
					},
				},
			})
		} else {
			stmt := p.parseStatement()
			body = append(body, BlockItem{
				kind: BLOCK_KIND_STMT,
				data: stmt,
			})
		}
	}
	p.expect(CLOSE_BRACE)

	return &Block{
		items: body,
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

	body := p.parseBlock()

	return &FunctionDef{identifier: identifier, body: body}
}

func (p *Parser) Parse() *Program {
	fnDef := p.parseFunctionDef()

	return &Program{
		mainFunction: fnDef,
	}
}
