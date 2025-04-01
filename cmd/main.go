package main

import (
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/nireo/mc"
)

func main() {
	shouldLex := flag.Bool("lex", false, "Run the lexer, but stop before parsing")
	shouldParse := flag.Bool("parse", false, "Run the lexer and parser, but stop before assembly generation")
	shouldCodegen := flag.Bool("codegen", false, "Perform lexing, parsing, and assembly generation, but stop before code emission")
	shouldValidate := flag.Bool("validate", false, "Perform lexing, parsing but stop before IR generation")
	shouldEmitAssembly := flag.Bool("S", false, "Emit an assembly file, but don't assemble or link it")

	flag.Parse()

	if flag.NArg() != 1 {
		fmt.Fprintf(os.Stderr, "Usage: %s [options] input_file.c\n", os.Args[0])
		os.Exit(1)
	}

	inputFile := flag.Arg(0)

	if _, err := os.Stat(inputFile); os.IsNotExist(err) {
		fmt.Fprintf(os.Stderr, "Error: Input file '%s' does not exist\n", inputFile)
		os.Exit(1)
	}

	baseName := filepath.Base(inputFile)
	extension := filepath.Ext(baseName)
	nameWithoutExt := strings.TrimSuffix(baseName, extension)

	inputDir := filepath.Dir(inputFile)
	preprocessedFile := filepath.Join(inputDir, nameWithoutExt+".i")
	assemblyFile := filepath.Join(inputDir, nameWithoutExt+".s")
	executableFile := filepath.Join(inputDir, nameWithoutExt)
	fmt.Println(executableFile)

	if err := preprocess(inputFile, preprocessedFile); err != nil {
		fmt.Fprintf(os.Stderr, "Preprocessing error: %v\n", err)
		os.Exit(1)
	}

	if *shouldLex {
		if err := runLexer(preprocessedFile); err != nil {
			fmt.Fprintf(os.Stderr, "Lexer error: %v\n", err)
			os.Exit(1)
		}
		cleanup(preprocessedFile)
		os.Exit(0)
	}

	if *shouldParse {
		if err := runParser(preprocessedFile); err != nil {
			fmt.Fprintf(os.Stderr, "Parser error: %v\n", err)
			os.Exit(1)
		}
		cleanup(preprocessedFile)
		os.Exit(0)
	}

	if *shouldValidate {
		if err := runSemantic(preprocessedFile); err != nil {
			fmt.Fprintf(os.Stderr, "Parser error: %v\n", err)
			os.Exit(1)
		}
		cleanup(preprocessedFile)
		os.Exit(0)
	}

	if err := compile(preprocessedFile, assemblyFile); err != nil {
		fmt.Fprintf(os.Stderr, "Compilation error: %v\n", err)
		cleanup(preprocessedFile)
		os.Exit(1)
	}

	cleanup(preprocessedFile)

	if *shouldCodegen {
		cleanup(assemblyFile)
		os.Exit(0)
	}

	if *shouldEmitAssembly {
		os.Exit(0)
	}

	if err := assembleAndLink(assemblyFile, executableFile); err != nil {
		fmt.Fprintf(os.Stderr, "Assembly/Linking error: %v\n", err)
		cleanup(assemblyFile)
		os.Exit(1)
	}

	cleanup(assemblyFile)

	os.Exit(0)
}

func preprocess(inputFile, outputFile string) error {
	cmd := exec.Command("gcc", "-E", "-P", inputFile, "-o", outputFile)
	return cmd.Run()
}

func runLexer(inputFile string) error {
	content, err := os.ReadFile(inputFile)
	if err != nil {
		return fmt.Errorf("cannot read file: %v", err)
	}

	_, err = mc.Tokenize(string(content))
	return err
}

func runSemantic(inputFile string) error {
	content, err := os.ReadFile(inputFile)
	if err != nil {
		return fmt.Errorf("cannot read file: %v", err)
	}

	tokenized, err := mc.Tokenize(string(content))
	if err != nil {
		return err
	}

	p := mc.NewParser(tokenized)
	prog := p.Parse()
	mc.AnalyzeSemantic(prog)

	return nil
}

func runParser(inputFile string) error {
	content, err := os.ReadFile(inputFile)
	if err != nil {
		return fmt.Errorf("cannot read file: %v", err)
	}

	tokenized, err := mc.Tokenize(string(content))
	if err != nil {
		return err
	}

	p := mc.NewParser(tokenized)
	_ = p.Parse()

	return nil
}

func compile(inputFile, outputFile string) error {
	content, err := os.ReadFile(inputFile)
	if err != nil {
		return fmt.Errorf("cannot read file: %v", err)
	}

	tokenized, err := mc.Tokenize(string(content))
	if err != nil {
		return err
	}

	p := mc.NewParser(tokenized)
	prog := p.Parse()
	mc.AnalyzeSemantic(prog)

	ir := mc.NewIrGenerator()
	irProg := ir.Generate(prog)
	genProgram := mc.GenerateIr(irProg)

	assemblyFile, err := os.Create(outputFile)
	if err != nil {
		return fmt.Errorf("failed to create assembly file: %v", err)
	}
	defer assemblyFile.Close()

	codeEmitter := mc.NewCodeEmitter()
	return codeEmitter.EmitProgram(genProgram, assemblyFile)
}

func assembleAndLink(assemblyFile, executableFile string) error {
	cmd := exec.Command("gcc", assemblyFile, "-o", executableFile)
	return cmd.Run()
}

func cleanup(file string) {
	os.Remove(file)
}
