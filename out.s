.globl main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	
	subq	$12, %rsp
	movl	$3, -4(%rbp)
	addl	$5, -4(%rbp)
	movl	-4(%rbp), %r10d
	movl	%r10d, -8(%rbp)
	movl	-8(%rbp), %r11d
	imull	$5, %r11d
	movl	%r11d, -8(%rbp)
	movl	-8(%rbp), %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
.section .note.GNU-stack,"",@progbits
