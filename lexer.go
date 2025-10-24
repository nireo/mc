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
	TokIdent Token = iota
	TokConstant
	TokInt
	TokVoid
	TokReturn
	TokOpenParen // 5
	TokCloseParen
	TokOpenBrace
	TokCloseBrace
	TokSemicolon // 9
	TokTilde
	TokMinus
	TokDecrement
	TokPlus
	TokAsterisk
	TokSlash
	TokPercent
	TokAnd
	TokOr
	TokBang
	TokEq
	TokNeq
	TokLT
	TokGT
	TokLTEQ
	TokGTEQ
	TokAssign
	TokIf
	TokElse
	TokQuestion
	TokColon
	TokDo
	TokWhile
	TokFor
	TokBreak
	TokContinue
	TokComma
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
			tokenFunc: func(s string) TokenValue { return TokenValue{TokInt, nil} },
		},
		{
			regex:     regexp.MustCompile(`^void\b`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokVoid, nil} },
		},
		{
			regex:     regexp.MustCompile(`^do\b`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokDo, nil} },
		},
		{
			regex:     regexp.MustCompile(`^while\b`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokWhile, nil} },
		},
		{
			regex:     regexp.MustCompile(`^continue\b`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokContinue, nil} },
		},
		{
			regex:     regexp.MustCompile(`^break\b`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokBreak, nil} },
		},
		{
			regex:     regexp.MustCompile(`^for\b`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokFor, nil} },
		},
		{
			regex:     regexp.MustCompile(`^return\b`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokReturn, nil} },
		},
		{
			regex:     regexp.MustCompile(`^if\b`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokIf, nil} },
		},
		{
			regex:     regexp.MustCompile(`^else\b`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokElse, nil} },
		},
		{
			regex: regexp.MustCompile(`^[0-9]+\b`),
			tokenFunc: func(s string) TokenValue {
				num, _ := strconv.ParseInt(s, 10, 64)
				return TokenValue{TokConstant, num}
			},
		},
		{
			regex: regexp.MustCompile(`^[a-zA-Z_][a-zA-Z0-9_]*\b`),
			tokenFunc: func(s string) TokenValue {
				return TokenValue{TokIdent, s}
			},
		},
		{
			regex:     regexp.MustCompile(`^==`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokEq, nil} },
		},
		{
			regex:     regexp.MustCompile(`^!=`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokNeq, nil} },
		},
		{
			regex:     regexp.MustCompile(`^<=`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokLTEQ, nil} },
		},
		{
			regex:     regexp.MustCompile(`^>=`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokGTEQ, nil} },
		},
		{
			regex:     regexp.MustCompile(`^--`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokDecrement, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\(`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokOpenParen, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\)`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokCloseParen, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\{`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokOpenBrace, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\}`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokCloseBrace, nil} },
		},
		{
			regex:     regexp.MustCompile(`^<`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokLT, nil} },
		},
		{
			regex:     regexp.MustCompile(`^>`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokGT, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\?`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokQuestion, nil} },
		},
		{
			regex:     regexp.MustCompile(`^:`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokColon, nil} },
		},
		{
			regex:     regexp.MustCompile(`^&&`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokAnd, nil} },
		},
		{
			regex:     regexp.MustCompile(`^=`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokAssign, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\|\|`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokOr, nil} },
		},
		{
			regex:     regexp.MustCompile(`^!`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokBang, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\-`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokMinus, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\%`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokPercent, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\+`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokPlus, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\/`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokSlash, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\*`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokAsterisk, nil} },
		},
		{
			regex:     regexp.MustCompile(`^\~`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokTilde, nil} },
		},
		{
			regex:     regexp.MustCompile(`^;`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokSemicolon, nil} },
		},
		{
			regex:     regexp.MustCompile(`^,`),
			tokenFunc: func(s string) TokenValue { return TokenValue{TokComma, nil} },
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
