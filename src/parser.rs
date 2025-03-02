use crate::lexer::Token;
use anyhow::{Result, anyhow};
use std::iter::Peekable;

#[derive(Debug)]
pub enum Expression {
    Integer(i64),
    Unary {
        operator: Token,
        expr: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum Statement {
    Return(Box<Expression>),
    If {
        expr: Box<Expression>,
        then: Box<Statement>,
        otherwise: Option<Box<Statement>>,
    },
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub identifier: String,
    pub statement: Box<Statement>,
}

#[derive(Debug)]
pub struct Program {
    pub fn_def: FunctionDefinition,
}

#[derive(Debug)]
pub struct Parser<I>
where
    I: Iterator<Item = Token>,
{
    tokens: Peekable<I>,
}

impl<I> Parser<I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Program> {
        Ok(Program {
            fn_def: self.parse_function_def()?,
        })
    }

    fn expect(&mut self, expected_tok: Token) -> Result<Token> {
        if let Some(tok) = self.tokens.next() {
            if tok != expected_tok {
                Err(anyhow!("Expected {:?} token got {:?}", expected_tok, tok))
            } else {
                Ok(tok)
            }
        } else {
            Err(anyhow!("Expected {:?} token but got nothing", expected_tok))
        }
    }

    fn parse_statement(&mut self) -> Result<Box<Statement>> {
        self.expect(Token::ReturnKeyword)?;
        let return_value = self.parse_expression()?;

        self.expect(Token::Semicolon)?;

        Ok(Box::new(Statement::Return(return_value)))
    }

    fn get_ident(&mut self) -> Result<String> {
        if let Some(tok) = self.tokens.next() {
            match tok {
                Token::Identifier(val) => Ok(val),
                _ => Err(anyhow!("Expected identifier token got {:?}", tok)),
            }
        } else {
            Err(anyhow!("Expected identifier token but got nothing"))
        }
    }

    fn get_int(&mut self) -> Result<i64> {
        if let Some(tok) = self.tokens.next() {
            match tok {
                Token::Constant(val) => Ok(val),
                _ => Err(anyhow!("Expected identifier token got {:?}", tok)),
            }
        } else {
            Err(anyhow!("Expected identifier token but got nothing"))
        }
    }

    fn parse_expression(&mut self) -> Result<Box<Expression>> {
        let next_token = match self.tokens.next() {
            Some(tok) => tok,
            _ => return Err(anyhow!("Expected token but got nothing when parsing expr")),
        };

        match next_token {
            Token::Constant(val) => Ok(Box::new(Expression::Integer(val))),
            Token::Tilde | Token::Minus => {
                let op = next_token;
                let inner_expr = self.parse_expression()?;

                Ok(Box::new(Expression::Unary {
                    operator: op,
                    expr: inner_expr,
                }))
            }
            Token::OpenParen => {
                let inner = self.parse_expression()?;
                self.expect(Token::CloseParen)?;

                Ok(inner)
            }
            _ => return Err(anyhow!("Unsupported expression token {:?}", next_token)),
        }
    }

    fn parse_function_def(&mut self) -> Result<FunctionDefinition> {
        self.expect(Token::IntKeyword)?;
        let identifier = self.get_ident()?;

        self.expect(Token::OpenParen)?;
        self.expect(Token::VoidKeyword)?;
        self.expect(Token::CloseParen)?;
        self.expect(Token::OpenBrace)?;

        let statement = self.parse_statement()?;

        self.expect(Token::CloseBrace)?;

        Ok(FunctionDefinition {
            identifier,
            statement,
        })
    }
}
