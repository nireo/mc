package main

import (
	"fmt"
	"log"
	"os"

	"github.com/nireo/mc"
)

func main() {
	tokenized, err := mc.Tokenize("int main(void) { return ~(-2); }")
	if err != nil {
		log.Fatalf("cannot tokenize")
	}

	p := mc.NewParser(tokenized)
	prog := p.Parse()

	ir := mc.NewIrGenerator()
	irProg := ir.Generate(prog)

	genProgram := mc.GenerateIr(irProg)

	codeEmitter := mc.NewCodeEmitter()
	err = codeEmitter.EmitProgram(genProgram, os.Stdout)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Code emission error: %v\n", err)
		os.Exit(1)
	}
}
