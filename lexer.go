package mc

import (
	"errors"
	"fmt"
	"regexp"
	"strconv"
)

// Token represents a lexical token
type Token int

// TokenValue holds the actual value of a token
type TokenValue struct {
	Kind  Token
	Value any
}

const (
	TOK_IDENT Token = iota
	TOK_CONSTANT
	INT_KEYWORD
	VOID_KEYWORD
	RETURN_KEYWORD
	OPEN_PAREN
	CLOSE_PAREN
	OPEN_BRACE
	CLOSE_BRACE
	SEMICOLON
	TILDE
	MINUS
	DECREMENT
	TOK_PLUS
	TOK_ASTERISK
	TOK_SLASH
	TOK_PERCENT
	TOK_AND
	TOK_OR
	TOK_BANG
	TOK_EQ
	TOK_NEQ
	TOK_LT
	TOK_GT
	TOK_LTEQ
	TOK_GTEQ
	TOK_ASSIGN
	TOK_IF
	TOK_ELSE
	TOK_QUESTION
	TOK_COLON
)

// Pattern represents a regex pattern and its token constructor
type Pattern struct {
	regex     *regexp.Regexp
	tokenFunc func(string) TokenValue
}

func createRegexPatterns() []Pattern {
	return []Pattern{
		{
			regex:     regexp.MustCompile(`^int\b`),
			tokenFunc: func(s string) TokenValue { return TokenValue{INT_KEYWORD, nil} },
		},
		{
			regex:     regexp.MustCompile(`^void\b`),
			tokenFunc: func(s string) TokenValue { return TokenValue{VOID_KEYWORD, nil} },
		},
		{
			regex:     regexp.MustCompile(`^return\b`),
			tokenFunc: func(s string) TokenValue { return TokenValue{RETURN_KEYWORD, nil} },
		},
		{
			regex:     regexp.MustCompile(`^if\b`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_IF, nil} },
		},
		{
			regex:     regexp.MustCompile(`^else\b`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_ELSE, nil} },
		},
		{
			regex: regexp.MustCompile(`^[0-9]+\b`),
			tokenFunc: func(s string) TokenValue {
				num, _ := strconv.ParseInt(s, 10, 64)
				return TokenValue{TOK_CONSTANT, num}
			},
		},
		{
			regex: regexp.MustCompile(`^[a-zA-Z_][a-zA-Z0-9_]*\b`),
			tokenFunc: func(s string) TokenValue {
				return TokenValue{TOK_IDENT, s}
			},
		},
		{
			regex:     regexp.MustCompile(`^==`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_EQ, nil} },
		},
		{
			regex:     regexp.MustCompile(`^!=`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_NEQ, nil} },
		},
		{
			regex:     regexp.MustCompile(`^<=`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_LTEQ, nil} },
		},
		{
			regex:     regexp.MustCompile(`^>=`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_GTEQ, nil} },
		},
		{
			regex:     regexp.MustCompile(`^--`),
			tokenFunc: func(s string) TokenValue { return TokenValue{DECREMENT, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\(`),
			tokenFunc: func(s string) TokenValue { return TokenValue{OPEN_PAREN, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\)`),
			tokenFunc: func(s string) TokenValue { return TokenValue{CLOSE_PAREN, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\{`),
			tokenFunc: func(s string) TokenValue { return TokenValue{OPEN_BRACE, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\}`),
			tokenFunc: func(s string) TokenValue { return TokenValue{CLOSE_BRACE, nil} },
		},
		{
			regex:     regexp.MustCompile(`^<`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_LT, nil} },
		},
		{
			regex:     regexp.MustCompile(`^>`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_GT, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\?`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_QUESTION, nil} },
		},
		{
			regex:     regexp.MustCompile(`^:`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_COLON, nil} },
		},
		{
			regex:     regexp.MustCompile(`^&&`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_AND, nil} },
		},
		{
			regex:     regexp.MustCompile(`^=`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_ASSIGN, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\|\|`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_OR, nil} },
		},
		{
			regex:     regexp.MustCompile(`^!`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_BANG, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\-`),
			tokenFunc: func(s string) TokenValue { return TokenValue{MINUS, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\%`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_PERCENT, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\+`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_PLUS, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\/`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_SLASH, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\*`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TOK_ASTERISK, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\~`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TILDE, nil} },
		},
		{
			regex:     regexp.MustCompile(`^;`),
			tokenFunc: func(s string) TokenValue { return TokenValue{SEMICOLON, nil} },
		},
	}
}

func Tokenize(input string) ([]TokenValue, error) {
	tokens := []TokenValue{}
	patterns := createRegexPatterns()

	for len(input) > 0 {
		if regexp.MustCompile(`^\s`).MatchString(input) {
			input = regexp.MustCompile(`^\s+`).ReplaceAllString(input, "")
		} else {
			matched := false
			for _, pattern := range patterns {
				if matches := pattern.regex.FindStringSubmatch(input); len(matches) > 0 {
					matchedStr := matches[0]
					token := pattern.tokenFunc(matchedStr)
					tokens = append(tokens, token)
					input = input[len(matchedStr):]
					matched = true
					break
				}
			}
			if !matched {
				return nil, errors.New(fmt.Sprintf("No matching pattern found at: '%s'", input))
			}
		}
	}
	return tokens, nil
}
