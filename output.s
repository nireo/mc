	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	movl $42 %eax
	movq %rbp, %rsp
	popq %rbp
	ret
	.section .note.GNU-stack,"",@progbits
