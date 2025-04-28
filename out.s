.globl main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	
	subq	$32, %rsp
	movq	$20, %r10
	movq	$20, -4(%rbp)
	movl	$3, %r10d
	movslq	%r10d,%r11
	movq	%r11, -8(%rbp)
	movq	-4(%rbp), %r10
	movq	-4(%rbp), %r10
	movq	%r10, -16(%rbp)
	movq	-8(%rbp), %r10
	addq	%r10, -16(%rbp)
	movl	-16(%rbp), %r10d
	movl	%r10d, -24(%rbp)
	movl	-24(%rbp), %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
	movl	$0, %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
.section .note.GNU-stack,"",@progbits
