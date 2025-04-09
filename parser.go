package mc

import (
	"fmt"
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

type FunctionCall struct {
	ident string
	args  []*Expression
}

// Expression represents a C expression
type Expression struct {
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
	data any
}

type Break struct {
	identifier string
}

type Continue struct {
	identifier string
}

type VarDecl struct {
	identifier string
	init       *Expression
}

type Declaration struct {
	data any
}

type BlockItem struct {
	data any
}

type Block struct {
	items []BlockItem
}

type FunctionDef struct {
	identifier string
	params     []string
	body       *Block
}

type Compound struct {
	block *Block
}

type Program struct {
	funcs []*FunctionDef
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
	if p.curr().Kind != expectedToken {
		panic(fmt.Sprintf("expected another token %d but got wrong token %d", expectedToken, p.curr().Kind))
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
			data: &ReturnStatement{
				expr: expr,
			},
		}
	case TOK_IF:
		return p.parseIfStatement()
	case SEMICOLON:
		p.idx += 1
		return &Statement{data: nil}
	case OPEN_BRACE:
		p.idx += 1
		block := p.parseBlock()

		return &Statement{
			data: &Compound{
				block: block,
			},
		}
	case TOK_BREAK:
		p.idx += 1
		p.expect(SEMICOLON)
		return &Statement{
			data: &Break{},
		}
	case TOK_CONTINUE:
		p.idx += 1
		p.expect(SEMICOLON)
		return &Statement{
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
		data: expr,
	}
}

func (p *Parser) parseExprList() []*Expression {
	exprs := make([]*Expression, 0)
	first := true

	for p.tokens[p.idx].Kind == TOK_COMMA || first {
		p.idx += 1
		if p.curr().Kind == CLOSE_PAREN {
			break
		}
		expr := p.parseExpr(0)
		exprs = append(exprs, expr)
		first = false
	}

	p.expect(CLOSE_PAREN)

	return exprs
}

func (p *Parser) parseFactor() *Expression {
	tok := p.tokens[p.idx]
	p.idx += 1

	switch tok.Kind {
	case TOK_IDENT:
		ident := tok.Value.(string)
		if p.curr().Kind == OPEN_PAREN {
			args := p.parseExprList()
			return &Expression{
				data: &FunctionCall{
					ident: ident,
					args:  args,
				},
			}
		}

		return &Expression{
			data: tok.Value.(string),
		}
	case TOK_CONSTANT:
		return &Expression{
			data: tok.Value.(int64),
		}
	case TILDE, MINUS, TOK_BANG:
		op := tok.Kind
		innerExpr := p.parseFactor()
		return &Expression{
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
				data: &AssignExpr{
					lvalue: left,
					avalue: right,
				},
			}
		} else if next == TOK_QUESTION {
			middle := p.parseConditionalMiddle()
			right := p.parseExpr(pred)
			left = &Expression{
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

func (p *Parser) parseDecl(parseVar bool) *Declaration {
	p.expect(INT_KEYWORD)
	ident := p.tokens[p.idx].Value.(string)
	p.idx += 1
	var expr *Expression

	c := p.curr()
	if c.Kind == TOK_ASSIGN {
		p.next()
		expr = p.parseExpr(0)
		p.expect(SEMICOLON)
	} else if c.Kind == OPEN_PAREN {
		p.next()
		args := p.parseParamList()
		c := p.curr()
		var body *Block
		if c.Kind == OPEN_BRACE {
			p.idx += 1

			if parseVar {
				panic("function inside another function")
			}
			body = p.parseBlock()

		} else if c.Kind == SEMICOLON {
			p.idx += 1
		} else {
			panic("unexpected token after function")
		}

		return &Declaration{
			data: &FunctionDef{
				identifier: ident,
				params:     args,
				body:       body,
			},
		}
	} else {
		p.expect(SEMICOLON)
	}

	if !parseVar {
		panic("not allowed vars in top level")
	}

	return &Declaration{
		data: &VarDecl{
			identifier: ident,
			init:       expr,
		},
	}
}

func (p *Parser) parseForStatement() *Statement {
	p.expect(TOK_FOR)
	p.expect(OPEN_PAREN)

	var init any
	isDecl := false
	switch p.tokens[p.idx].Kind {
	case INT_KEYWORD:
		init = p.parseDecl(true)
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
			decl := p.parseDecl(true)
			body = append(body, BlockItem{
				data: decl,
			})
		} else if currTok == OPEN_BRACE {
			p.idx += 1
			block := p.parseBlock()
			body = append(body, BlockItem{
				data: &Statement{
					data: &Compound{
						block: block,
					},
				},
			})
		} else {
			stmt := p.parseStatement()
			body = append(body, BlockItem{
				data: stmt,
			})
		}
	}
	p.expect(CLOSE_BRACE)

	return &Block{
		items: body,
	}
}

func (p *Parser) curr() *TokenValue {
	return &p.tokens[p.idx]
}

func (p *Parser) next() {
	p.idx += 1
}

func (p *Parser) parseParamList() []string {
	args := []string{}
	tok := p.tokens[p.idx]
	switch tok.Kind {
	case VOID_KEYWORD:
		p.idx += 1
	case INT_KEYWORD:
		p.idx += 1
		identifier := p.tokens[p.idx].Value.(string)
		p.idx += 1

		args = append(args, identifier)

		for p.tokens[p.idx].Kind == TOK_COMMA {
			p.idx += 1
			p.expect(INT_KEYWORD)
			i := p.tokens[p.idx].Value.(string)
			args = append(args, i)
			p.idx += 1
		}
	}

	p.expect(CLOSE_PAREN)

	return args
}

func (p *Parser) Parse() *Program {
	var funcs []*FunctionDef
	for p.idx < len(p.tokens) && p.curr().Kind == INT_KEYWORD {
		decl := p.parseDecl(false)
		if fnDef, ok := decl.data.(*FunctionDef); ok {
			funcs = append(funcs, fnDef)
		} else {
			panic("failed to parse function decl")
		}
	}

	return &Program{funcs: funcs}
}
