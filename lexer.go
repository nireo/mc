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
	IDENTIFIER Token = iota
	CONSTANT
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
			regex: regexp.MustCompile(`^[0-9]+\b`),
			tokenFunc: func(s string) TokenValue {
				num, _ := strconv.ParseInt(s, 10, 64)
				return TokenValue{CONSTANT, num}
			},
		},
		{
			regex: regexp.MustCompile(`^[a-zA-Z_][a-zA-Z0-9_]*\b`),
			tokenFunc: func(s string) TokenValue {
				return TokenValue{IDENTIFIER, s}
			},
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
			regex:     regexp.MustCompile(`^\-`),
			tokenFunc: func(s string) TokenValue { return TokenValue{MINUS, nil} },
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
