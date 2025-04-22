	.globl counter
	.data
	.align 4
counter:
	.zero 4

.globl increment_counter
increment_counter:
	pushq	%rbp
	movq	%rsp, %rbp
	
	subq	$12, %rsp
	movl	counter(%rip), %r10d
	movl	%r10d, -4(%rbp)
	movl	$1, -8(%rbp)
	movl	-4(%rbp), %r10d
	addl	%r10d, -8(%rbp)
	movl	-8(%rbp), %r10d
	movl	%r10d, counter(%rip)
	movl	$0, %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret

.globl main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	
	subq	$12, %rsp
	call increment_counter
	movl	%eax, -4(%rbp)
	call increment_counter
	movl	%eax, -8(%rbp)
	call increment_counter
	movl	%eax, -12(%rbp)
	movl	counter(%rip), %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
	movl	$0, %eax
	movq	%rbp, %rsp
	popq	%rbp
	ret
.section .note.GNU-stack,"",@progbits
