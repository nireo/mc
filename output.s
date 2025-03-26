	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	movl $42 %eax
	ret
	movq %rbp, %rsp
	popq %rbp
	.section .note.GNU-stack,"",@progbits
