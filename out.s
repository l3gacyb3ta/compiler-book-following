.globl main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$12, %rsp
	movl	$42, -4(%rbp)
	negl	-4(%rbp)
	movl	-4(%rbp), %r10d
	movl	%r10d, -8(%rbp)
	negl	-8(%rbp)
	movl	-8(%rbp), %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
.section .note.GNU-stack,"",@progbits