package main

import (
	"fmt"
	"log"
	"os"
	"path/filepath"

	"github.com/nireo/mc"
)

func main() {
	tokenized, err := mc.Tokenize("int main(void) { return 42; }")
	if err != nil {
		log.Fatalf("cannot tokenize")
	}

	p := mc.NewParser(tokenized)
	prog := p.Parse()
	fmt.Printf("%+v", prog)

	ir := mc.NewIrGenerator()
	irProg := ir.Generate(prog)

	genProgram := mc.GenerateIr(irProg)

	outputPath := filepath.Join(".", "output.s")
	codeEmitter := mc.NewCodeEmitter()
	err = codeEmitter.EmitCode(genProgram, outputPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Code emission error: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Successfully compiled to %s\n", outputPath)

}
