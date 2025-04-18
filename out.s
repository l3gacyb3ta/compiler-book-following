.globl main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	
	subq	$20, %rsp
	movl	$3, -4(%rbp)
	movl	-4(%rbp), %r10d
	movl	%r10d, -8(%rbp)
	movl	$3, -12(%rbp)
	movl	-12(%rbp), %r11d
	imull	-8(%rbp), %r11d
	movl	%r11d, -12(%rbp)
	movl	-12(%rbp), %r10d
	movl	%r10d, -4(%rbp)
	movl	-4(%rbp), %r10d
	movl	%r10d, -16(%rbp)
	addl	$4, -16(%rbp)
	movl	-16(%rbp), %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
	movl	$0, %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
.section .note.GNU-stack,"",@progbits
