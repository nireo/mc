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
	TokAsterisk: 50,
	TokSlash:    50,
	TokPercent:  50,
	TokPlus:     45,
	TokMinus:    45,
	TokGT:       35,
	TokLT:       35,
	TokGTEQ:     35,
	TokLTEQ:     35,
	TokEq:       30,
	TokNeq:      30,
	TokAnd:      10,
	TokOr:       5,
	TokQuestion: 3,
	TokAssign:   1,
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
		panic(
			fmt.Sprintf(
				"expected another token %d but got wrong token %d",
				expectedToken,
				p.curr().Kind,
			),
		)
	}
	p.idx += 1
}

func (p *Parser) parseIfStatement() *Statement {
	p.expect(TokIf)
	p.expect(TokOpenParen)
	cond := p.parseExpr(0)
	p.expect(TokCloseParen)
	then := p.parseStatement()

	var otherwise *Statement
	if p.tokens[p.idx].Kind == TokElse {
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
	case TokReturn:
		p.expect(TokReturn)
		expr := p.parseExpr(0)
		p.expect(TokSemicolon)

		return &Statement{
			data: &ReturnStatement{
				expr: expr,
			},
		}
	case TokIf:
		return p.parseIfStatement()
	case TokSemicolon:
		p.idx += 1
		return &Statement{data: nil}
	case TokOpenBrace:
		p.idx += 1
		block := p.parseBlock()

		return &Statement{
			data: &Compound{
				block: block,
			},
		}
	case TokBreak:
		p.idx += 1
		p.expect(TokSemicolon)
		return &Statement{
			data: &Break{},
		}
	case TokContinue:
		p.idx += 1
		p.expect(TokSemicolon)
		return &Statement{
			data: &Continue{},
		}
	case TokDo:
		p.idx += 1
		stmt := p.parseStatement()
		p.expect(TokWhile)
		p.expect(TokOpenParen)

		cond := p.parseExpr(0)
		p.expect(TokCloseParen)
		p.expect(TokSemicolon)

		return &Statement{
			data: &DoWhileStatement{
				cond: cond,
				body: stmt,
			},
		}
	case TokWhile:
		p.idx += 1
		p.expect(TokOpenParen)
		cond := p.parseExpr(0)
		p.expect(TokCloseParen)

		body := p.parseStatement()
		return &Statement{
			data: &WhileStatement{
				cond: cond,
				body: body,
			},
		}
	case TokFor:
		return p.parseForStatement()
	}

	expr := p.parseExpr(0)
	p.expect(TokSemicolon)
	return &Statement{
		data: expr,
	}
}

func (p *Parser) parseExprList() []*Expression {
	exprs := make([]*Expression, 0)
	first := true

	for p.tokens[p.idx].Kind == TokComma || first {
		p.idx += 1
		if p.curr().Kind == TokCloseParen {
			break
		}
		expr := p.parseExpr(0)
		exprs = append(exprs, expr)
		first = false
	}

	p.expect(TokCloseParen)

	return exprs
}

func (p *Parser) parseFactor() *Expression {
	tok := p.tokens[p.idx]
	p.idx += 1

	switch tok.Kind {
	case TokIdent:
		ident := tok.Value.(string)
		if p.curr().Kind == TokOpenParen {
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
	case TokConstant:
		return &Expression{
			data: tok.Value.(int64),
		}
	case TokTilde, TokMinus, TokBang:
		op := tok.Kind
		innerExpr := p.parseFactor()
		return &Expression{
			data: &UnaryExpr{
				operator: op,
				expr:     innerExpr,
			},
		}
	case TokOpenParen:
		innerExpr := p.parseExpr(0)
		p.expect(TokCloseParen)

		return innerExpr
	}

	panic(fmt.Sprintf("no implementation for expression %d", tok.Kind))
}

func (p *Parser) parseConditionalMiddle() *Expression {
	p.expect(TokQuestion)
	expr := p.parseExpr(0)
	p.expect(TokColon)

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

		if next == TokAssign {
			p.idx++ // skip the assign symbol
			right := p.parseExpr(pred)
			left = &Expression{
				data: &AssignExpr{
					lvalue: left,
					avalue: right,
				},
			}
		} else if next == TokQuestion {
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
	p.expect(TokInt)
	ident := p.tokens[p.idx].Value.(string)
	p.idx += 1
	var expr *Expression

	c := p.curr()
	if c.Kind == TokAssign {
		p.next()
		expr = p.parseExpr(0)
		p.expect(TokSemicolon)
	} else if c.Kind == TokOpenParen {
		p.next()
		args := p.parseParamList()
		c := p.curr()
		var body *Block
		if c.Kind == TokOpenBrace {
			p.idx += 1

			if parseVar {
				panic("function inside another function")
			}
			body = p.parseBlock()

		} else if c.Kind == TokSemicolon {
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
		p.expect(TokSemicolon)
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
	p.expect(TokFor)
	p.expect(TokOpenParen)

	var init any
	isDecl := false
	switch p.tokens[p.idx].Kind {
	case TokInt:
		init = p.parseDecl(true)
		isDecl = true
	case TokSemicolon:
		init = nil
	default:
		init = p.parseExpr(0)
	}

	if !isDecl {
		p.expect(TokSemicolon)
	}

	var cond *Expression = nil
	if p.tokens[p.idx].Kind == TokSemicolon {
		p.idx += 1
	} else {
		cond = p.parseExpr(0)
		p.expect(TokSemicolon)
	}

	var post *Expression = nil
	if p.tokens[p.idx].Kind == TokCloseParen {
		p.idx += 1
	} else {
		cond = p.parseExpr(0)
		p.expect(TokCloseParen)
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
		if currTok == TokCloseBrace {
			break
		}

		if currTok == TokInt {
			decl := p.parseDecl(true)
			body = append(body, BlockItem{
				data: decl,
			})
		} else if currTok == TokOpenBrace {
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
	p.expect(TokCloseBrace)

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
	case TokVoid:
		p.idx += 1
	case TokInt:
		p.idx += 1
		identifier := p.tokens[p.idx].Value.(string)
		p.idx += 1

		args = append(args, identifier)

		for p.tokens[p.idx].Kind == TokComma {
			p.idx += 1
			p.expect(TokInt)
			i := p.tokens[p.idx].Value.(string)
			args = append(args, i)
			p.idx += 1
		}
	}

	p.expect(TokCloseParen)

	return args
}

func (p *Parser) Parse() *Program {
	var funcs []*FunctionDef
	for p.idx < len(p.tokens) && p.curr().Kind == TokInt {
		decl := p.parseDecl(false)
		if fnDef, ok := decl.data.(*FunctionDef); ok {
			funcs = append(funcs, fnDef)
		} else {
			panic("failed to parse function decl")
		}
	}

	return &Program{funcs: funcs}
}
