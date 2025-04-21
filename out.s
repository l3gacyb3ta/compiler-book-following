.globl main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	
	subq	$12, %rsp
	movl	$1, -4(%rbp)
	movl	$3, -8(%rbp)
	movl	$4, -8(%rbp)
	movl	-4(%rbp), %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
	movl	$0, %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
.section .note.GNU-stack,"",@progbits
