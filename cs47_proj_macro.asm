# Add you macro definition here - do not touch cs47_common_macro.asm"
#<------------------ MACRO DEFINITIONS ---------------------->#

	# D: target S: source T: n(position)
	.macro extract_nth_bit($regD, $regS, $regT)
	li $regD, 0x1
	sllv $regD, $regD, $regT
	and $regD, $regS, $regD
	srlv $regD, $regD, $regT
	.end_macro 
	
	# D: target S: n(position) T: 0x1 
	.macro insert_to_nth_bit($regD, $regS, $regT, $maskReg)
	sllv $maskReg, $regT, $regS # maskReg is temp mask
	not $maskReg, $maskReg # invert mask
	and $regD, $regD, $maskReg # bitwise and the target with inverted mask
	sllv $regT, $regT, $regS # set up another mask with 1 at n position
	or $regD, $regD, $regT # bit wise or the target with that mask
	.end_macro 