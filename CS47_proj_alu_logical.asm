.include "./cs47_proj_macro.asm"
.text
.globl au_logical
# TBD: Complete your project procedures
# Needed skeleton is given
#####################################################################
# Implement au_logical
# Argument:
# 	$a0: First number
#	$a1: Second number
#	$a2: operation code ('+':add, '-':sub, '*':mul, '/':div)
# Return:
#	$v0: ($a0+$a1) | ($a0-$a1) | ($a0*$a1):LO | ($a0 / $a1)
# 	$v1: ($a0 * $a1):HI | ($a0 % $a1)
# Notes:
#####################################################################
au_logical:
	# Build frame
	addi	$sp, $sp, -24
	sw	$fp, 24($sp)
	sw	$ra, 20($sp)
	sw	$a0, 16($sp)
	sw	$a1, 12($sp)
	sw	$a2, 8($sp)
	addi	$fp, $sp, 24
	
	# Check for operation
	beq $a2, '+', add_logical
	beq $a2, '-', sub_logical
	beq $a2, '*', mul_logical
	beq $a2, '/', div_logical
	
add_logical:
	li $a2, 0x00000000
	j add_sub_logical
	
sub_logical:
	li $a2, 0xFFFFFFFF
	j add_sub_logical
	
mul_logical:
	jal mul_signed
	j end_alu

div_logical:
	jal div_signed 
	j end_alu
	
end_alu:
	# Restore frame
	lw	$fp, 24($sp)
	lw	$ra, 20($sp)
	lw	$a0, 16($sp)
	lw	$a1, 12($sp)
	lw	$a2, 8($sp)
	addi	$sp, $sp, 24
	# return
	jr 	$ra
	
add_sub_logical: 
	li $t0, 0 # t0 = index (i)
	li $t1, 0  # t1 = result (S)
	extract_nth_bit($t2, $a2, $zero) # t2 = carryout/in (C)
	bne $t2, 1, loop_add_sub # check if subtraction operation
	not $a1, $a1
loop_add_sub:
	extract_nth_bit($t3, $a0, $t0) # t3 = a0(i) = A
	extract_nth_bit($t4, $a1, $t0) # t4 = a1(i) = B
	xor $t5, $t3, $t4 # t5 = XOR A,B
	xor $t6, $t2, $t5 # t6 = Y = XOR C, t5 ***
	and $t7, $t2, $t5 # t7 = AND C, t5
	and $t8, $t3, $t4 # t8 = AND A,B
	or $t2, $t7, $t8 # C = OR t7,t8 ***
	insert_to_nth_bit($t1, $t0, $t6, $t7) # insert into result (S)
	addi $t0, $t0, 1 # i++
	bne $t0, 32, loop_add_sub
	la $v0, ($t1) # v0 = S(result)
	la $v1, ($t2) # v1 = C(carryout)
	# return
	jr $ra

twos_complement:
	# Build frame
	addi	$sp, $sp -20
	sw	$fp, 20($sp)
	sw	$ra, 16($sp)
	sw	$a0, 12($sp)
	sw	$a1, 8($sp)
	addi	$fp, $sp, 20
	not $a0, $a0 # invert a0
	li $a1, 1
	jal add_logical # ~a0 + 1
	# Restore frame
	lw	$fp, 20($sp)
	lw	$ra, 16($sp)
	lw	$a0, 12($sp)
	lw	$a1, 8($sp)
	addi	$sp, $sp, 20
	# return 
	jr $ra

twos_complement_if_neg:
	# Build frame
	addi	$sp, $sp, -16
	sw	$fp, 16($sp)
	sw	$ra, 12($sp)
	sw	$a0, 8($sp)
	addi	$fp, $sp, 16
	blt $a0, $zero, neg_a0 # check if a0 is negative
	j pos_a0
neg_a0:
	jal twos_complement # twos comp a0
	la $a0, ($v0) # put result of twos comp into a0
pos_a0:
	la $v0, ($a0) # load a0 as the return value (v0)
	# Restore frame
	lw	$fp, 16($sp)
	lw	$ra, 12($sp)
	lw	$a0, 8($sp)
	addi	$sp, $sp, 16
	# return
	jr $ra	
	
twos_complement_64bit:
	# Build frame
	addi	$sp, $sp, -28
	sw	$fp, 28($sp)
	sw	$ra, 24($sp)
	sw	$a0, 20($sp)
	sw	$a1, 16($sp)
	sw	$s0, 12($sp)
	sw	$s1, 8($sp)
	addi	$fp, $sp, 28
	not $a0, $a0 # invert a0
	not $a1, $a1 # invert a1
	la $s0, ($a1) # save ~a1
	li $a1, 1
	jal add_logical # ~a0 + 1
	la $s1, ($v0) # save result into s1 **this is Lo
	la $a0, ($v1) # load carryout into a0
	la $a1, ($s0) # restore ~a1
	jal add_logical # ~a1 + carryout
	la $v1, ($v0) # v1 = Hi
	la $v0, ($s1) # v0 = Lo
	# Retore frame
	lw	$fp, 28($sp)
	lw	$ra, 24($sp)
	lw	$a0, 20($sp)
	lw	$a1, 16($sp)
	lw	$s0, 12($sp)
	lw	$s1, 8($sp)
	addi	$sp, $sp, 28
	# return
	jr $ra

bit_replicator:
	# Build frame
	addi	$sp, $sp, -16
	sw	$fp, 16($sp)
	sw	$ra, 12($sp)
	sw	$a0, 8($sp)
	addi	$fp, $sp, 16
	beq $a0, 0x1, rep_neg # check if a0 is negative
	li $v0, 0x00000000 # if a0 = positive, loads 0x00000000 into v0
	j rep_end
rep_neg:
	li $v0, 0xFFFFFFFF # if a0 = negative, loads 0xFFFFFFFF into v0
rep_end:
	# Restore frame
	lw	$fp, 16($sp)
	lw	$ra, 12($sp)
	lw	$a0, 8($sp)
	addi	$sp, $sp, 16
	# return
	jr $ra
	
mul_unsigned:
	# Build frame
	addi	$sp, $sp, -48
	sw	$fp, 48($sp)
	sw 	$ra, 44($sp)
	sw	$a0, 40($sp)
	sw	$a1, 36($sp)
	sw	$a2, 32($sp)
	sw	$s0, 28($sp)
	sw	$s1, 24($sp)
	sw	$s2, 20($sp)
	sw	$s3, 16($sp)
	sw	$s4, 12($sp)
	sw	$s5, 8($sp)
	addi	$fp, $sp, 48
	li $s0, 0 # s0 = index i
	li $s1, 0 # s1 = high register H (Hi)
	la $s2, ($a0) # MCND M
	la $s3, ($a1) # MPLR L (Lo)
loop_mul_unsigned:
	extract_nth_bit($t0, $s3, $zero) # t0 = L[0]
	la $a0, ($t0)
	jal bit_replicator # replicate L[0]
	la $s4, ($v0) # R = 32-bit replicated L[0]
	and $s5, $s2, $s4 # X = AND M,R
	la $a0, ($s1) 
	la $a1, ($s5)
	jal add_logical # add H(a0) with X(a1)
	la $s1, ($v0) # H = H + X
	srl $s3, $s3, 1 # L >>1
	extract_nth_bit($t0, $s1, $zero) # t0 = H[0]
	li $t1, 31
	insert_to_nth_bit($s3, $t1, $t0, $t2) # L[31] = H[0]
	srl $s1, $s1, 1 # H >>1
	addi $s0, $s0, 1 # i++
	bne $s0, 32, loop_mul_unsigned
	la $v0, ($s3) # v0 = Lo
	la $v1, ($s1) # v1 = Hi
	# Restore frame
	lw	$fp, 48($sp)
	lw 	$ra, 44($sp)
	lw	$a0, 40($sp)
	lw	$a1, 36($sp)
	lw	$a2, 32($sp)
	lw	$s0, 28($sp)
	lw	$s1, 24($sp)
	lw	$s2, 20($sp)
	lw	$s3, 16($sp)
	lw	$s4, 12($sp)
	lw	$s5, 8($sp)
	addi	$sp, $sp, 48
	# return
	jr $ra
	
mul_signed:
	# Build frame 
	addi	$sp, $sp, -36
	sw	$fp, 36($sp)
	sw	$ra, 32($sp)
	sw	$a0, 28($sp)
	sw	$a1, 24($sp)
	sw	$s0, 20($sp)
	sw	$s1, 16($sp)
	sw	$s2, 12($sp)
	sw	$s3, 8($sp)
	addi	$fp, $sp, 36
	la $s0, ($a0) # N1 = s0 = a0
	la $s1, ($a1) # N2 = s1 = a1
	la $s2, ($a0) # s2 = original value of a0
	la $s3, ($a1) # s3 = original value of a1
	jal twos_complement_if_neg # twos comp N1 if its neg
	la $s0, ($v0) # load result into N1
	la $a0, ($s1) # a0 = N2
	jal twos_complement_if_neg # twos comp N2 if its neg
	la $s1, ($v0) # load result into N2
	la $a0, ($s0) 
	la $a1, ($s1) 
	jal mul_unsigned # unsigned mul with N1(a0) and N2(a1)
	la $a0, ($v0) # a0 = Rlo
	la $a1, ($v1) # a1 = Rhi
	li $t0, 31
	extract_nth_bit($t1, $s2, $t0) # t1 = original a0[31]
	extract_nth_bit($t2, $s3, $t0) # t2 = original a1[31]
	xor $t3, $t1, $t2 # t3 = S = XOR t1,t2
	bne $t3, 1, end_mul_signed
	jal twos_complement_64bit
end_mul_signed:
	# Restore frame
	lw	$fp, 36($sp)
	lw	$ra, 32($sp)
	lw	$a0, 28($sp)
	lw	$a1, 24($sp)
	lw	$s0, 20($sp)
	lw	$s1, 16($sp)
	lw	$s2, 12($sp)
	lw	$s3, 8($sp)
	addi	$sp, $sp, 36
	# return
	jr $ra

div_unsigned:
	# Build frame
	addi	$sp, $sp -40
	sw	$fp, 40($sp)
	sw	$ra, 36($sp)
	sw	$a0, 32($sp)
	sw	$a1, 28($sp)
	sw	$s0, 24($sp)
	sw	$s1, 20($sp)
	sw	$s2, 16($sp)
	sw	$s3, 12($sp)
	sw	$s4, 8($sp)
	addi	$fp, $sp 40
	li $s0, 0 # s0 = index i
	la $s1, ($a0) # s1 = Q(DVND)
	la $s2, ($a1) # s2 = D(DVSR)
	li $s3, 0 # s3 = remainder R
loop_div_unsigned:
	sll $s3, $s3, 1 # R <<1
	li $t0, 31
	extract_nth_bit($t1, $s1, $t0) # t1 = Q[31]
	insert_to_nth_bit($s3, $zero, $t1, $t2) # R[0] = Q[31]
	sll $s1, $s1, 1 # Q <<1
	la $a0, ($s3)
	la $a1, ($s2)
	jal sub_logical # R(a0) - D(a1)
	la $s4, ($v0) # s4 = S = R - D
	blt $s4, $zero, s_neg # check if S is neg
	la $s3, ($s4) # R = S
	li $t0, 1 
	insert_to_nth_bit($s1, $zero, $t0, $t1) # Q[0] = 1
s_neg:
	addi $s0, $s0, 1 # i++
	bne $s0, 32, loop_div_unsigned
	la $v0, ($s1) # v0 = quotient
	la $v1, ($s3) # v1 = remainder
	# Restore frame
	lw	$fp, 40($sp)
	lw	$ra, 36($sp)
	lw	$a0, 32($sp)
	lw	$a1, 28($sp)
	lw	$s0, 24($sp)
	lw	$s1, 20($sp)
	lw	$s2, 16($sp)
	lw	$s3, 12($sp)
	lw	$s4, 8($sp)
	addi	$sp, $sp 40
	# return
	jr $ra
	
div_signed:
	# Build frame
	addi	$sp, $sp, -44
	sw	$fp, 44($sp)
	sw	$ra, 40($sp)
	sw	$a0, 36($sp)
	sw	$a1, 32($sp)
	sw	$s0, 28($sp)
	sw	$s1, 24($sp)
	sw	$s2, 20($sp)
	sw	$s3, 16($sp)
	sw	$s4, 12($sp)
	sw	$s5, 8($sp)
	addi	$fp, $sp, 44
	la $s0, ($a0) # N1 = s0 = a0
	la $s1, ($a1) # N2 = s1 = a1
	la $s2, ($a0) # s2 = original value of a0
	la $s3, ($a1) # s3 = original value of a1
	jal twos_complement_if_neg # twos comp N1 if its neg
	la $s0, ($v0) # load result into N1
	la $a0, ($s1) # a0 = N2
	jal twos_complement_if_neg # twos comp N2 if its neg
	la $s1, ($v0) # load result into N2
	la $a0, ($s0) 
	la $a1, ($s1) 
	jal div_unsigned # unsigned div with N1(a0) and N2(a1)
	la $s4, ($v0) # s4 = Q
	la $s5, ($v1) # s5 = R
	li $t0, 31
	extract_nth_bit($t1, $s2, $t0) # t1 = a0[31]
	extract_nth_bit($t2, $s3, $t0) # t2 = a1[31]
	xor $t3, $t1, $t2 # t3 = S = XOR t1,t2
	bne $t3, 1, s_pos # check if S is neg
	la $a0, ($s4)
	jal twos_complement # S is neg, twos comp Q
	la $s4, ($v0) # Q = twos comp Q
s_pos:
	li $t0, 31
	extract_nth_bit($t1, $s2, $t0) # t1 = S2 = a0[31]
	bne $t1, 1, s2_pos # check if S2 is neg
	la $a0, ($s5)
	jal twos_complement # S2 is neg, twos comp R
	la $s5, ($v0) # R = twos comp R
s2_pos:
	la $v0, ($s4) # v0 = Q
	la $v1, ($s5) # v1 = R
	# Restore frame
	lw	$fp, 44($sp)
	lw	$ra, 40($sp)
	lw	$a0, 36($sp)
	lw	$a1, 32($sp)
	lw	$s0, 28($sp)
	lw	$s1, 24($sp)
	lw	$s2, 20($sp)
	lw	$s3, 16($sp)
	lw	$s4, 12($sp)
	lw	$s5, 8($sp)
	addi	$sp, $sp, 44
	# return
	jr $ra