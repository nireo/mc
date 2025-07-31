	.globl x
x:
	pushq %rbp
	movq %rsp, %rbp
	subq $96, %rsp
	movl %edi, -4(%rbp)
	movl %esi, -8(%rbp)
	movl %edx, -12(%rbp)
	movl %ecx, -16(%rbp)
	movl %r8d, -20(%rbp)
	movl %r9d, -24(%rbp)
	cmpl $1, -4(%rbp)
	movl $0, -28(%rbp)
	sete -28(%rbp)
	cmpl $0, -28(%rbp)
	je .L8
	cmpl $2, -8(%rbp)
	movl $0, -32(%rbp)
	sete -32(%rbp)
	cmpl $0, -32(%rbp)
	je .L8
	movl $1, -36(%rbp)
	jmp .L9
.L8:
	movl $0, -36(%rbp)
.L9:
	cmpl $0, -36(%rbp)
	je .L6
	cmpl $3, -12(%rbp)
	movl $0, -40(%rbp)
	sete -40(%rbp)
	cmpl $0, -40(%rbp)
	je .L6
	movl $1, -44(%rbp)
	jmp .L7
.L6:
	movl $0, -44(%rbp)
.L7:
	cmpl $0, -44(%rbp)
	je .L4
	cmpl $4, -16(%rbp)
	movl $0, -48(%rbp)
	sete -48(%rbp)
	cmpl $0, -48(%rbp)
	je .L4
	movl $1, -52(%rbp)
	jmp .L5
.L4:
	movl $0, -52(%rbp)
.L5:
	cmpl $0, -52(%rbp)
	je .L2
	cmpl $5, -20(%rbp)
	movl $0, -56(%rbp)
	sete -56(%rbp)
	cmpl $0, -56(%rbp)
	je .L2
	movl $1, -60(%rbp)
	jmp .L3
.L2:
	movl $0, -60(%rbp)
.L3:
	cmpl $0, -60(%rbp)
	je .L0
	cmpl $6, -24(%rbp)
	movl $0, -64(%rbp)
	sete -64(%rbp)
	cmpl $0, -64(%rbp)
	je .L0
	movl $1, -68(%rbp)
	jmp .L1
.L0:
	movl $0, -68(%rbp)
.L1:
	movl -68(%rbp), %eax
	movq %rbp, %rsp
	popq %rbp
	ret
	movl $0, %eax
	movq %rbp, %rsp
	popq %rbp
	ret
	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movl $4, -4(%rbp)
	movl $24, %eax
	cdq
	idivl -8(%rbp)
	movl %eax, -12(%rbp)
	movl $1, %edi
	movl $2, %esi
	movl $3, %edx
	movl $4, %ecx
	movl $5, %r8d
	movl -12(%rbp), %r9d
	call x
	movl %eax, -16(%rbp)
	movl -16(%rbp), %eax
	movq %rbp, %rsp
	popq %rbp
	ret
	movl $0, %eax
	movq %rbp, %rsp
	popq %rbp
	ret
	.section .note.GNU-stack,"",@progbits
