.globl main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	
	subq	$20, %rsp
	movl	$3, -4(%rbp)
	cmpl	$3, -4(%rbp)
	movl	$0, -8(%rbp)
	sete	-8(%rbp)
	movl	-8(%rbp), %r10d
	movl	%r10d, -12(%rbp)
	cmpl	$0, -12(%rbp)
	je	.Lend_label.1
	movl	$10, -16(%rbp)
	jmp	.Lend_label.1
	.Le2_label.0:
	movl	$30, -16(%rbp)
	.Lend_label.1:
	movl	-16(%rbp), %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
	movl	$0, %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
.section .note.GNU-stack,"",@progbits
