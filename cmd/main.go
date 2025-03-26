package main

import (
	"fmt"
	"log"

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
}
