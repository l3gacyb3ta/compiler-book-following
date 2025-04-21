.globl main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	
	subq	$20, %rsp
	movl	$0, -4(%rbp)
	.Lloop.0.continue:
	cmpl	$10, -4(%rbp)
	movl	$0, -8(%rbp)
	setl	-8(%rbp)
	cmpl	$0, -8(%rbp)
	je	.Lloop.0.break
	movl	-4(%rbp), %r10d
	movl	%r10d, -12(%rbp)
	movl	$1, -16(%rbp)
	movl	-12(%rbp), %r10d
	addl	%r10d, -16(%rbp)
	movl	-16(%rbp), %r10d
	movl	%r10d, -4(%rbp)
	jmp	.Lloop.0.continue
	.Lloop.0.break:
	movl	-4(%rbp), %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
	movl	$0, %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
.section .note.GNU-stack,"",@progbits
