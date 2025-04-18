.globl main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	
	subq	$24, %rsp
	movl	$3, -4(%rbp)
	movl	$3, -8(%rbp)
	negl	-8(%rbp)
	movl	-8(%rbp), %r10d
	movl	%r10d, -12(%rbp)
	movl	-4(%rbp), %r10d
	addl	%r10d, -12(%rbp)
	movl	-12(%rbp), %r10d
	movl	%r10d, -16(%rbp)
	movl	-4(%rbp), %r10d
	movl	%r10d, -20(%rbp)
	movl	-16(%rbp), %r10d
	addl	%r10d, -20(%rbp)
	movl	-20(%rbp), %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
	movl	$0, %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
.section .note.GNU-stack,"",@progbits
